# vim600: set ts=4 sw=4 tw=80 expandtab nowrap noai cin foldmethod=marker:
# A Multiplex TCP Component designed for performance.
# -----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 43) borrowed from FreeBSD's jail.c:
# <tag@cpan.org> wrote this file.  As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return.   Scott S. McCoy
# -----------------------------------------------------------------------------
# See TCPMulti.otl (TVO format) or TCPMulti.pod (POD format) for documentation
package POE::Component::Client::TCPMulti;

# Settings and Initialization {{{

use strict;
use warnings FATAL => qw( all );
use constant CHEAP => -1;

# POE::Component::Server::TCPMulti can export cheap also
# We're not going to require order from the user.
sub import {
    no strict "refs";
    my $caller = caller;

    unless (defined *{"${caller}::CHEAP"}) {
        *{"${caller}::CHEAP"} = \&CHEAP;
    }
}


use UNIVERSAL qw( isa );
use POE qw( Kernel
            Session
            Driver::SysRW
            Filter::Line 
            Wheel::ReadWrite
            Wheel::SocketFactory );

use Carp qw( carp croak );

*VERSION = \0.050;

our $VERSION;
BEGIN { 
    unless (defined &DEBUG) {
        use constant DEBUG => 0;
    }   
}

# Heap is now package global.  This is fine, each wheel throughout the POE
# Kernel has its own unique identification.  So multiple component sessions
# can utilize the same hash for Connection Heaps.

# Note: Explicit lexical was not accessable by the inline states (This seems to
# be a bug in perl >= 5.8.1, although its marked as simply changed behavior in
# the changelog.  Its only with strange combinations of lexicals anonymous
# subroutines and anonymous hashrefs (As commonly used in POE
# programming...bastards :P)
our %Heap;

# }}}
# new (Depriciated) {{{

sub new { goto &create }
    
# }}}
# Constructor {{{

sub create {
    # Initialization {{{

    shift if $_[0] eq __PACKAGE__;
    my ($Code, %UserCode);

    %UserCode = @_;

    $UserCode{$_} ||= sub {} for qw( ErrorEvent
                                     InputEvent
                                     Initialize
                                     Disconnected
                                     SuccessEvent
                                     FlushedEvent
                                     FailureEvent
                                     TimeoutEvent );

    $UserCode{Timeout}        ||= 30;
    $UserCode{ConnectTimeout} ||= $UserCode{Timeout};
    $UserCode{InputTimeout}   ||= 300;
    $UserCode{Filter}         ||= "POE::Filter::Line";
    $UserCode{FilterArgs}     ||= undef;
    $UserCode{options}        ||= {};
    $UserCode{package_states} ||= [];
    $UserCode{object_states}  ||= [];

    if (ref $UserCode{Filter} eq "ARRAY") {
        my @FilterData = @{ delete $UserCode{Filter} };
        $UserCode{Filter} = shift @FilterData;
        $UserCode{FilterArgs} = \@FilterData;
    }

    @{ $UserCode{UserStates} }{ qw( _start _stop _child ) } =
        delete @{ $UserCode{inline_states} }{ qw( _start _stop _child ) };

    # }}}
    # Internal States {{{
    $Code = {
        # Session Events {{{
        #   _start:     Session Start {{{

        _start      => sub {
            $_[KERNEL]->alias_set( delete $UserCode{Alias} ) 
                if defined $UserCode{Alias};
    
            $UserCode{UserStates}->{_start}->(@_)
                if ref $UserCode{UserStates}->{_start} eq "CODE";
        },
    
        #   }}}
        #   _child:     Session Child {{{

        _child      => sub {
            $UserCode{states}->{_child}->(@_)
                if ref $UserCode{UserStates}->{_child} eq "CODE";
        },
    
        #   }}}
        #   _stop:      Session End {{{

        _stop       => sub {
            $UserCode{UserStates}->{_stop}->(@_)
                if ref $UserCode{UserStates}->{_stop} eq "CODE";
        },
    
        #   }}}
        # }}}
        # Connection States {{{
        #   -success:       Connection was successful (Internal) {{{

        -success       => sub {
            my ($handle, $old_id) = @_[ARG0, ARG3];
            my $filter;
    
            # We need 1 filter per Wheel...yeah
            if (ref $UserCode{Filter} && 
                    UNIVERSAL::isa($UserCode{Filter}, "UNIVERSAL")) {
                $filter = $UserCode{Filter} = ref $UserCode{Filter};
            }

            $filter = $UserCode{Filter}->new( @{ $UserCode{FilterArgs} } );

            $Heap{$old_id}{-SERVER} = POE::Wheel::ReadWrite->new
                ( Handle        => $handle,
                  Driver        => POE::Driver::SysRW->new(BlockSize => 4096),
                  Filter        => $filter,
                  InputEvent    => '-incoming',
                  ErrorEvent    => '-error',
                  FlushedEvent  => '-flushed' );

            # Transfer entire heap (including wheel), reinstate -ID
            my $new_id = $Heap{$old_id}{-SERVER}->ID;
            $Heap{$new_id} = delete $Heap{$old_id};

            bless $Heap{$new_id}, "POE::Component::Client::TCPMulti::CHEAP";

            # ARG4 differs from Wheel definition...its our new id.
            push @_, $new_id, $Heap{$new_id};

            $_[CHEAP]{-ID} = $new_id;
            $_[CHEAP]{-TIMEOUT} = $UserCode{InputTimeout};

            if ($UserCode{InputTimeout}) {
                $_[KERNEL]->delay_adjust
                    ( $_[CHEAP]{-ALARM}, $_[CHEAP]{-TIMEOUT} );
            }
            else {
                $_[KERNEL]->alarm_remove( delete $_[CHEAP]{-ALARM} );
            }

            $UserCode{SuccessEvent}->(@_);
    
            printf '%d: Successfull Connection %s:%d\n', $new_id,
                @{ $Heap{$new_id} }{qw( -ADDR -PORT )} if DEBUG;
        },
    
        #   }}}
        #   connect:        Open new connection {{{

        # Connect to the next available proxy
        connect         => sub {	
            my ($address, $port, $bindaddress, $bindport) = @_[ARG0..ARG3];

            unless (defined $address) {
                return printf STDERR   
                    "connect called without address or port, %s: line %d\n",
                    @_[CALLER_FILE, CALLER_LINE];
            }
            
            push @_, POE::Component::Client::TCPMulti->connect
                ( RemoteAddress => $address,
                  RemotePort    => $port,
                  BindAddress   => $bindaddress,
                  BindPort      => $bindport,
                  Timeout       => $UserCode{ConnectTimeout},
                );

            $UserCode{Initialize}->(@_);
        }, 
    
        #   }}}
        # }}}
        # IO States {{{
        #   -incoming:      Handling recieved data (Internal) {{{
    
        -incoming  => sub {
            push @_, $Heap{$_[ARG1]};
            return unless $_[CHEAP]{-RUNNING};

            if ($_[CHEAP]{-TIMEOUT}) {
                $_[KERNEL]->delay_adjust
                    ( $_[CHEAP]{-ALARM}, $_[CHEAP]{-TIMEOUT} );
            }

            $UserCode{InputEvent}(@_);
        },

        #   }}}
        #   send:           Send Data {{{

        send        => sub {
            unless (defined $_[ARG1]) {
                return printf STDERR  
                    "send called without socket or data %s: line %d\n",
                    @_[CALLER_FILE, CALLER_LINE];
            }
            elsif (defined $Heap{$_[ARG0]}{-SERVER}) {
                $Heap{$_[ARG0]}{-SERVER}->put( @_[ARG1 .. $#_] );
            } 
        },

        #   }}}
        # }}}
        # Error States {{{
        #   -failure:       Handle Connection Failure (Internal) {{{
    
        -failure   => sub {
            printf "%d: Disconnected - Failed\n", $_[ARG3] if DEBUG;

            push @_, $Heap{$_[ARG3]};
            $UserCode{FailureEvent}->(@_);

            delete $_[CHEAP];
            delete $Heap{$_[ARG3]}{-SERVER};

            $_[ARG0] = $_[ARG3];
            $Code->{shutdown}->(@_);
        },
    
        #   }}}
        #   -error:         Handle Connection Error (Internal) {{{

        -error     => sub { 
            printf "%d: Disconnected - Error\n", $_[ARG3] if DEBUG;
    
            $#_++;
            $_[CHEAP] = $Heap{$_[ARG3]};
            $UserCode{ErrorEvent}->(@_);
    
            delete $_[CHEAP];
            delete $Heap{$_[ARG3]}{-SERVER};
    
            $_[ARG0] = $_[ARG3];
            $Code->{shutdown}->(@_);
        }, 
    
        #   }}}
        #   -timeout:       Handle Connection Timeout (Internal) {{{
        # Occsaionally -timeout is being called after the connection errors,
        # thats what the extra check on -RUNNING is for, as well as in the
        # other error states, just to ensure there is no problem.  This doesn't
        # really happen anymore but I'm not comfortable with it yet.

        -timeout   => sub { 
            if ($Heap{$_[ARG0]}{-RUNNING}) {
                printf "%d: Disconnected - Timeout\n", $_[ARG0] if DEBUG;
    
                $#_++;
                $_[CHEAP] = $Heap{$_[ARG0]};
                $UserCode{TimeoutEvent}->(@_);
    
                delete $_[CHEAP];
                delete $Heap{$_[ARG0]}->{-SERVER};
    
                $Code->{shutdown}->(@_);
            }
        },
    
        #   }}}
        # }}}
        # Closing States {{{
        #   -flushed:       Empty Socket (Internal) {{{

        # flush - our socket is empty - Direct call is faster and fits reqs.
        -flushed   => sub {
            unless ($Heap{$_[ARG0]}{-RUNNING}) {
                $Code->{shutdown}->(@_);
            } 
        },
    
        #   }}}
        #   shutdown:       Handle Socket Shutdown {{{

        # Shutdown... push onto queue if not sent, delete driver.
        shutdown	=> sub {
            unless (defined $_[ARG0]) {
                return printf STDERR  
                    "shutdown called without CHEAP id %s: line %d\n",
                    @_[CALLER_FILE, CALLER_LINE];
            }

            if ($Heap{$_[ARG0]}{-RUNNING}) {
                # Remove Alarm
                $_[KERNEL]->alarm_remove ( $Heap{$_[ARG0]}{-ALARM} );
    
                $Heap{$_[ARG0]}->{-RUNNING} = 0;
                delete $Heap{$_[ARG0]}{-ALARM};
            }

            if (defined $Heap{$_[ARG0]}{-SERVER}) {
                if ($Heap{$_[ARG0]}{-SERVER}->can("get_driver_out_octets")) {
                    unless ($Heap{$_[ARG0]}{-SERVER}->get_driver_out_octets) {
                        printf "%d: Disconnected - Closed\n", $_[ARG0] 
                            if DEBUG;
    
                        push @_, $Heap{$_[ARG0]};
                        $UserCode{Disconnected}->(@_);
                        
                        # Blow shit up
                        delete $_[CHEAP];
                        delete $Heap{$_[ARG0]};
                    }
    
                    # Its either gone and we're out of synch (shouldn't happen),
                    # or we want to wait for a clean shutdown.
                    return;
                } 
            }
    
            delete $Heap{$_[ARG0]};
        },
    
        #   }}}
        #   die:            Gracefully close all sockets {{{
        # Shutdown quick, clean and gracefull. 

        die         => sub {
            $_[KERNEL]->call(shutdown => $_) for keys %Heap;
            $_[KERNEL]->alias_remove($_) for $_[KERNEL]->alias_list;
            $_[KERNEL]->alarm_remove_all;
        },

        #   }}}
        # }}}
    }; 
    # }}}
    # Session Constructor {{{

    POE::Session->create
        ( inline_states => { %{ delete $UserCode{inline_states} }, %$Code },
          object_states     => delete $UserCode{object_states},
          package_states    => delete $UserCode{package_states},
          options           => delete $UserCode{options},
          args              => delete $UserCode{args},
        );

    # }}}
}

# }}}
# Connect Method {{{

sub connect {
    my %Options = @_[1..$#_];
    $Options{Heap} ||= {};

    my $server = POE::Wheel::SocketFactory->new
        ( RemoteAddress => $Options{RemoteAddress},
          RemotePort    => $Options{RemotePort},
          BindAddress   => $Options{BindAddress},
          BindPort      => $Options{BindPort},
          SuccessEvent  => '-success',
          FailureEvent  => '-failure',
          Reuse         => 'yes',
        );
    
    my $id = $server->ID; 

    $Heap{$id} = bless {
        %{ $Options{Heap} },
        -ID         => $server->ID,
        -ADDR       => $Options{RemoteAddress},
        -PORT       => $Options{RemotePort},
        -BINDA      => $Options{BindAddress},
        -BINDP      => $Options{BindPort},
        -RUNNING    => 1,
        -TIMEOUT    => $Options{Timeout},
        -SERVER     => $server,
        -STAMP      => time,
    }, __PACKAGE__ . "::CHEAP";
    
    if ($Heap{$id}{-TIMEOUT}) {
        $Heap{$id}{-ALARM}  = $poe_kernel->delay_add
            ( -timeout => $Heap{$id}{-TIMEOUT}, $id);
    }
    else {
        $Heap{$id}{-ALARM} = 0;
    }

    printf "%d: Connecting %s:%d \n", $id, @{ $Heap{$id} }{qw( -ADDR -PORT )}
        if DEBUG;

    return $Heap{$id};
}

# }}}
# CHEAP Package {{{

package POE::Component::Client::TCPMulti::CHEAP;
use POE::Kernel;

#   Attribute Accessors {{{
sub ID {
    shift->{-ID}
}
sub ADDR {
    shift->{-ADDR}
}
sub PORT {
    shift->{-PORT}
}
#   }}}
#   Filter Settings {{{

sub filter {
    shift->{-SERVER}->set_filter( shift->new(@_) );
}

sub input_filter {
    shift->{-SERVER}->set_input_filter( shift->new(@_) );
}

sub output_filter {
    shift->{-SERVER}->set_output_filter( shift->new(@_) );
}

# }}}
#   Timeout Setting {{{

sub timeout {
    my ($cheap, $timeout) = @_;

    $poe_kernel->alarm_remove($cheap->{-ALARM}) if $cheap->{-ALARM};

    unless (defined $timeout) {
        return $cheap->{-TIMEOUT};
    }
    if ($timeout) {
        $cheap->{-TIMEOUT} = $timeout;
        $cheap->{-STAMP} = time;
        $cheap->{-ALARM} = $poe_kernel->delay_set
            ( -timeout => $cheap->{-TIMEOUT}, $cheap->{-ID});
    }
    else {
        $cheap->{-TIMEOUT} = 0;
        $cheap->{-ALARM}   = 0;
        $cheap->{-STAMP}   = 0;
    }
}

#   }}}
# }}}

return "POE Rules";
