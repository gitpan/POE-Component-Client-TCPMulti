#!/usr/bin/perl
# Proxy Scanner Proof of concept.
# tag@cpan.org

use strict;
use warnings FATAL => qw( all );
use Socket qw(inet_aton);
use Net::DNS;

use Getopt::Std;
use POE qw(Component::Client::TCPMulti Filter::Block);
$POE::Component::Client::TCPMulti::DEBUG++;
use constant CL => "\cM\cJ\cM\cJ";

my %Getopts = ( t => 100,
                s => "66.230.130.254",
                p => "proxies.txt",
                v => "verified.txt",
                x => 30,
                b => undef );
my %Opt;
my @Queue;
my @Ports;
my $Server;
my $EOF;

getopts "Vv:p:s:x:t:b:", \%Getopts;

# Legibility ROCKS!
@Opt{qw( Verified Proxies Server Verbose
         Timeout  Threads BindIP )} = delete @Getopts{qw( v p s V x t b )};

# Event States------------------------------------------------------------------
sub nextProxy () { 
    unless (@Queue) {
        my $Next = <PROXIES>;
        return unless defined $Next;

        $Next =~ s/\.[^.]+$//;
        
        for my $x (2..254) {
            push @Queue, [ sprintf("%s.%d", $Next, $x), $_ ] for @Ports;
        }
    }

    shift @Queue;
}


sub SocksHandle {
    $_[CHEAP]->{SockPoint}++;
    $_[CHEAP]->{Input} .= $_[ARG0];

    printf "<<< %d Socks Sent: %d\n", $_[CHEAP]->ID, unpack "C", $_[ARG0]
        if $Opt{Verbose};

    if ($_[CHEAP]->{SockPoint} == 2) {
        my ($version, $method) = unpack "CC", $_[CHEAP]->{Input};
        $_[CHEAP]->{Version} = 0;

        if (($version == 2) or ($version == 4)) {
            $_[KERNEL]->yield(shutdown => $_[ARG1]) if $method > 90;
            $_[CHEAP]->{Version} = 4;
        }

        else { $_[KERNEL]->yield(shutdown => $_[ARG1]) }

        delete $_[CHEAP]->{Input};
    }

    elsif ($_[CHEAP]->{SockPoint} == 8) {
        if (($_[CHEAP]->{Version} == 4) and (unpack "C", $_[ARG0] ne 0)) {

            delete @{ $_[CHEAP] }{qw( SockPoint Version )};

            $_[CHEAP]->filter("POE::Filter::Line");
            $_[CHEAP]->{HandleSocks} = 0;

            $_[KERNEL]->yield
                ( send => $_[CHEAP]->ID, 
                  sprintf "HELO isprime.com%s", CL );
        }

        else { $_[KERNEL]->yield(shutdown => $_[ARG1]) }

        delete $_[CHEAP]->{Input};
    }
}

sub SuccessHandle {
    $_[CHEAP]->{HandleSocks} = 0;

    printf "!!! %d Connection Successful\n", $_[CHEAP]->ID if $Opt{Verbose};

    if ($_[CHEAP]->PORT == 1080) {
        $_[CHEAP]->{HandleSocks}++;
        $_[CHEAP]->{SockPoint} = 0;
        $_[CHEAP]->filter("POE::Filter::Block", BlockSize => 1 );

        $_[KERNEL]->yield(send => $_[CHEAP]->ID, pack "CCn", 4, 1, 25);
        $_[KERNEL]->yield(send => $_[CHEAP]->ID, inet_aton $Opt{Server});
        $_[KERNEL]->yield(send => $_[CHEAP]->ID, pack "x");
    }

    else {
        $_[KERNEL]->yield
            ( send => $_[CHEAP]->ID,
              sprintf "CONNECT %s:25 HTTP/1.0%s%s", $Opt{Server}, CL, CL );
    }
}

sub ErrorHandle {
    my $Next;
    return unless $Next = nextProxy;

    $_[KERNEL]->yield( connect => $Next->[0], $Next->[1], $Opt{BindIP} );
}

sub InputHandle {
    return SocksHandle @_ if $_[CHEAP]->{HandleSocks};

    printf "<<< %d Sent: %s\n", $_[CHEAP]->ID, $_[ARG0] if $Opt{Verbose};

    if (($_[ARG0] =~ m/200/) && ($_[ARG0] =~ m/^HTTP/)) {
        $_[KERNEL]->yield
            ( send => $_[CHEAP]->ID, 
              sprintf "HELO isprime.com%s", CL );
    }
    if ($_[ARG0] =~ m/^250/) {
        $_[KERNEL]->yield(send => $_[CHEAP]->ID, sprintf "QUIT%s", CL);
        printf VERIFIED "%s:%d\n", $_[CHEAP]->ADDR, $_[CHEAP]->PORT;
    }
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
        print " Hi...I need to get ready.\r";

        open PROXIES, "<", $Opt{Proxies} or die "$Opt{Proxies}: $!";
        open VERIFIED, ">", $Opt{Verified};

        my %Portlist;

        while (<PROXIES>) {
            chomp;
            $Portlist{ ( split /:/ )[1] } = 1;
        }

        @Ports = keys %Portlist;

        $EOF = tell PROXIES;
        seek PROXIES, 0, 0;

        ErrorHandle @_ for 1..$Opt{Threads};

        select VERIFIED; $|++;
        select STDOUT;   $|++;

        print " Word!  Lets go\r";

        $_[HEAP]->{Running}++;
        $_[KERNEL]->call(Main => "verbose") unless $Opt{Verbose};
    },
    verbose => sub {
        my $pos;
        
        printf " [ %s%s%s ] %0.2f\r", 
                "=" x ($pos = ((tell(PROXIES) / $EOF) * 60)),
                ">",
                " " x (60 - int $pos), $pos;
        
        $_[KERNEL]->delay(verbose => 0.5) if $_[HEAP]->{Running};
    },
    _end => sub {
        close PROXIES;
        close VERIFIED;

        $_[HEAP]->{Running} = 0;
        $_[KERNEL]->call(Main => "verbose");
    },
  }
);

# Being Program-----------------------------------------------------------------
run POE::Kernel;
