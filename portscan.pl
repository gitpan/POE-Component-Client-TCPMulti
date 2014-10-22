#!/usr/bin/perl -l
# Proxy Scanner Proof of concept.
# tag@cpan.org

use strict;
use warnings FATAL => qw( all );
use Socket qw(inet_aton);

$|++;

use Getopt::Std;
use POE qw(Component::Client::TCPMulti Filter::Block);
#$POE::Component::Client::TCPMulti::DEBUG++;
use constant CL => "\cM\cJ\cM\cJ";

my %Getopts = ( t => 100,
                s => "127.0.0.1",
                p => "1-65535",
                x => 30,
                d => 0,
                v => 0,
                S => 0,
                b => undef );
my %Opt;
my @Ports;
my @Open;
my %Service;
my $Address;
my $EOF;

getopts "dvp:s:v:x:t:b:", \%Getopts;

# Legibility ROCKS!
@Opt{qw( Range Address Verbose Timeout Services 
        Threads BindIP Debug )} = delete @Getopts{qw( p s v x S t b d )};

$POE::Component::Client::TCPMulti::DEBUG++ if $Opt{Debug};

my ($R_Start, $R_End) = split /-/, $Opt{Range};

@Ports = ($R_Start..$R_End);

unless ($Opt{Services}) {
    open SERVICES, "<", "/etc/services";
    while (<SERVICES>) {
        $Service{$2} = $1 if m!^(\w+)\s+(\d+)/tcp!;
    }
    close SERVICES;
}

# Event States------------------------------------------------------------------

sub SuccessHandle {
    $_[CHEAP]->{HandleSocks} = 0;

    push @Open, $_[CHEAP]->PORT;
    $_[KERNEL]->yield(shutdown => $_[CHEAP]->ID);
}

sub ErrorHandle {
    my $Port;
    unless ($Port = shift @Ports) {
        return $_[HEAP]->{Running} = 0;
    }

    $_[KERNEL]->yield( connect => $Opt{Address}, $Port, $Opt{BindIP} );
}

sub InputHandle {
    printf "<<< %d Sent: %s\n", $_[CHEAP]->ID, $_[ARG0] if $Opt{Verbose};
}

# Sessions----------------------------------------------------------------------
POE::Component::Client::TCPMulti->new
( Timeout       => $Opt{Timeout},

  InputEvent    => \&InputHandle,

  ErrorEvent    => \&ErrorHandle,
  TimeoutEvent  => \&ErrorHandle,
  FailureEvent  => \&ErrorHandle,
  Disconnected  => \&ErrorHandle,

  SuccessEvent  => \&SuccessHandle,
  Alias         => "Main",
  inline_states => {
    _start => sub {
        if ($Opt{Verbose}) {
            printf "Starting Scan: %s\n", $Opt{Address};
            printf "Scanning Port range: %s\n", $Opt{Range};
        }

        $_[HEAP]->{Running}++;
        $_[KERNEL]->call(Main => "verbose") if $Opt{Verbose};

        ErrorHandle @_ for 1..$Opt{Threads};
    },
    verbose => sub {
        my $pos;
        
        if ($_[HEAP]->{Running}) {
            printf " [ %s%s%s ] %0.2f%%\r", 
            "=" x ($pos = (($R_End - @Ports) / $R_End) * 60),
            ">",
            " " x (60 - int $pos), (($R_End - @Ports) / $R_End) * 100;

            $_[KERNEL]->delay(verbose => 0.25);
        }
    },
    _stop => sub {
        printf "\nSummary for: %s\n", $Opt{Address};
        for my $Port (@Open) {
            unless ($Opt{Services}) {
                $Service{$Port} ||= "Unknown";

                printf "Open Port:  %6d (%s)\n", $Port, $Service{$Port};
            }
            else {
                printf "Open Port:   %6d\n", $Port;
            } 
        }
        $_[HEAP]->{Running} = 0;
    },
  }
);

# Being Program-----------------------------------------------------------------
run POE::Kernel;

__DATA__

=head1 NAME

    portscan.pl - POE::Component::Client::TCPMulti proof of concept.

=head1 SYNPOSIS

 ./portscan.pl [ -vdS ] [ -t <sockets> ] [ -p <port-range> ] [ -s <Address> ]
               [ -x <timeout> ]  

=cut               
