# A Highconnectivity replacement for PoCo::Client::TCP
# -----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 43) borrowed from FreeBSD's jail.c:
# <tag@cpan.org> wrote this file.  As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return.   Scott S. McCoy
# -----------------------------------------------------------------------------
# See END | Notes for API POD, tho you should be able to figure it out.
package POE::Component::Client::TCPMulti;

use strict;
use warnings FATAL => qw( all );
use constant CHEAP => -1;
use base qw( Exporter );

our @EXPORT    = qw( CHEAP );
our @EXPORT_OK = qw( fetchCHEAP );

use POE qw( Kernel
            Session
            Driver::SysRW
            Filter::Line 
            Wheel::ReadWrite
            Wheel::SocketFactory );

use Carp qw( carp croak );


*VERSION = \0.008_004;

our $VERSION;  # To please strict with my constant. 
our $DEBUG = 0;

my ($Code, $UserCode, %Heap);

#-Init--------------------------------------------------------------------------
$Code = {
    _start      => sub {
        $_[KERNEL]->alias_set( $UserCode->{Alias} ) 
            if defined $UserCode->{Alias};

        $UserCode->{UserStates}->{_start}->(@_)
            if ref $UserCode->{UserStates}->{_start} eq "CODE";
    },

    _child      => sub {
        $UserCode->{states}->{_child}->(@_)
            if ref $UserCode->{UserStates}->{_child} eq "CODE";
    },

    _stop       => sub {
        $UserCode->{UserStates}->{_stop}->(@_)
            if ref $UserCode->{UserStates}->{_stop} eq "CODE";
    },

    # Reset the timeout...its gross I know
    TMzero_alarm    => sub {
        $_ = time - $Heap{$_[0]}->{TM_STAMP};
        $_[KERNEL]->alarm_adjust($Heap{$_[0]}->{TM_ALARM}, $_);
        $Heap{$_[0]}->{TM_STAMP} = time;
    },

    #-Connection States---------------------------------------------------------
    # Connect to the next available proxy
    connect         => sub {	

        my $server = POE::Wheel::SocketFactory->new
            ( RemoteAddress => $_[ARG0],
              RemotePort    => $_[ARG1],
              BindAddress   => $_[ARG2],
              BindPort      => $_[ARG3],
              SuccessEvent  => 'TMsuccess',
              FailureEvent  => 'TMfailure',
              Reuse         => 'yes',
            );

        # Store Heap Data.
        my $id = $server->ID; 
        $Heap{$id}->{TM_ID}     = $id;
        $Heap{$id}->{TM_ADDR}   = $_[ARG0];
        $Heap{$id}->{TM_PORT}   = $_[ARG1];
        $Heap{$id}->{TM_BINDA}  = $_[ARG2];
        $Heap{$id}->{TM_BINDP}  = $_[ARG3];
        $Heap{$id}->{TM_RUNNING}++;

        # Wheel Reference
        $Heap{$id}->{TM_SERVER} = $server;

        # Create Alarm
        $Heap{$id}->{TM_STAMP}  = time;
        $Heap{$id}->{TM_ALARM}  = $_[KERNEL]->alarm_set
            ( TMtimeout => time + $UserCode->{Timeout}, $id);
        
        bless $Heap{$id}, "POE::Component::Client::TCPMulti::CHEAP";

        $#_++;
        $_[CHEAP] = $Heap{$id};
        $UserCode->{Initialize}->(@_);

        printf "%d: Connecting %s:%d \n", $id, $_[ARG0], $_[ARG1] if $DEBUG;
    }, 

    # Got success, force destruction of SocketFactory and create Readwrite Wheel
    TMsuccess       => sub {
        $Heap{$_[ARG3]}->{TM_SERVER} = POE::Wheel::ReadWrite->new
            ( Handle        => $_[ARG0],
              Driver        => POE::Driver::SysRW->new(BlockSize => 4096),
              Filter        => $UserCode->{Filter}->new
                                ( @{ $UserCode->{FilterArgs} } ),
              InputEvent    => 'TMincoming',
              ErrorEvent    => 'TMerror',
              FlushedEvent  => 'TMflushed' );

        # Transfer entire heap (including wheel), reinstate TM_ID
        my $id = $Heap{$_[ARG3]}->{TM_SERVER}->ID;
        $Heap{$id} = delete $Heap{$_[ARG3]};
        $Heap{$id}->{TM_ID} = $id;

        # Delete the alarm and create a new one with the right ID 
        $_[KERNEL]->alarm_remove( delete $Heap{$id}->{TM_ALARM} );
        $Heap{$id}->{TM_ALARM}  = $_[KERNEL]->alarm_set
            ( TMtimeout => time + $UserCode->{Timeout}, $id);

        $_[ARG4]  = $id;
        $#_++;
        $_[CHEAP] = $Heap{$id};
        $UserCode->{SuccessEvent}->(@_);

        printf "%d: Connection Successful ID %d\n", $_[ARG3], $id if $DEBUG; 
    },

    #-IO States-----------------------------------------------------------------
    # This is just so we can queue up our sockwrites while we're parsing data
    send        => sub {
        if (defined $Heap{$_[ARG0]}->{TM_SERVER}) {
            $Heap{$_[ARG0]}->{TM_SERVER}->put($_[ARG1]);
        } 
        else {
            carp sprintf "Attempted to send to invalid socket %d\n", $_[ARG0];
        }
    },

    # Mail Socket Input Handler
    TMincoming  => sub {
        $#_++;
        $_[CHEAP] = $Heap{$_[ARG1]};
        return unless $_[CHEAP]->{TM_RUNNING};

        $UserCode->{InputEvent}->(@_);
    },

    #-Error States--------------------------------------------------------------
    # I could have called all of these by one name...just separating thoughts
    # Also, I needed to use call for error states so the alarm would be removed
    # before it could go off.  Duplicate removal calls happend during debugging.

    TMfailure   => sub {
        if ($Heap{$_[ARG3]}->{TM_RUNNING}) {
            printf "%d: Disconnected - Failed\n", $_[ARG3] if $DEBUG;

            $#_++;
            $_[CHEAP] = $Heap{$_[ARG3]};
            $UserCode->{FailureEvent}->(@_);

            delete $_[CHEAP];
            delete $Heap{$_[ARG3]}->{TM_SERVER};

            $_[ARG0] = $_[ARG3];
            $Code->{shutdown}->(@_);
        }
    },

    TMerror     => sub { 
        if ($Heap{$_[ARG3]}->{TM_RUNNING}) {
            printf "%d: Disconnected - Error\n", $_[ARG3] if $DEBUG;

            $#_++;
            $_[CHEAP] = $Heap{$_[ARG3]};
            $UserCode->{ErrorEvent}->(@_);

            delete $_[CHEAP];
            delete $Heap{$_[ARG3]}->{TM_SERVER};

            $_[ARG0] = $_[ARG3];
            $Code->{shutdown}->(@_);
        }
    }, 

    # Occsaionally TMtimeout is being called after the connection errors,
    # thats what the extra check on TM_RUNNING is for, as well as in the
    # other error states, just to ensure there is no problem
    TMtimeout   => sub { 
        if ($Heap{$_[ARG0]}->{TM_RUNNING}) {
            printf "%d: Disconnected - Timeout\n", $_[ARG0] if $DEBUG;

            $#_++;
            $_[CHEAP] = $Heap{$_[ARG0]};
            $UserCode->{TimeoutEvent}->(@_);

            delete $_[CHEAP];
            delete $Heap{$_[ARG0]}->{TM_SERVER};

            $Code->{shutdown}->(@_);
        }
    },

    # Shutdown... push onto queue if not sent, delete driver (or wait for flush)
    shutdown	=> sub {
        if ($Heap{$_[ARG0]}->{TM_RUNNING}) {
            # Remove Alarm
            $_[KERNEL]->alarm_remove ( $Heap{$_[ARG0]}->{TM_ALARM} );	

            $Heap{$_[ARG0]}->{TM_RUNNING} = 0;
            delete $Heap{$_[ARG0]}->{TM_ALARM};
        }

        if (defined $Heap{$_[ARG0]}->{TM_SERVER}) {
            if ($Heap{$_[ARG0]}->{TM_SERVER}->can("get_driver_out_octets")) {
                unless ($Heap{$_[ARG0]}->{TM_SERVER}->get_driver_out_octets) {
                    printf "%d: Disconnected - Closed\n", $_[ARG0] if $DEBUG;

                    $#_++;
                    $_[CHEAP] = $Heap{$_[ARG0]};
                    $UserCode->{Disconnected}->(@_);
                    
                    # Blow shit up
                    delete $_[CHEAP];
                    delete $Heap{$_[ARG0]};

                }

                # Its either gone, or we want to wait for a clean shutdown.
                return;
            } 
        }

        delete $Heap{$_[ARG0]};
    },

    # Flush - our socket is empty - Direct call is faster and fits reqs.
    TMflushed   => sub {
        unless ($Heap{$_[ARG0]}->{TM_RUNNING}) {
            delete $Heap{$_[ARG0]};
        } 
        else {
#            $Code->{TMzero_alarm}->(@_);
        }
    },

    # Shutdown quick, clean and gracefull. 
    die         => sub {
        $_[KERNEL]->yield(shutdown => $_) for keys %Heap;
    },
}; 

sub new {
    shift;
    $UserCode = { @_ };

    $UserCode->{$_} ||= sub {} for qw( ErrorEvent
                                       InputEvent
                                       Initialize
                                       Disconnected
                                       SuccessEvent
                                       FlushedEvent
                                       FailureEvent
                                       TimeoutEvent );

    $UserCode->{Timeout} ||= 30;
    $UserCode->{Filter}  ||= "POE::Filter::Line";
    $UserCode->{FilterArgs} = undef;
    $UserCode->{options} ||= {};
    $UserCode->{package_states} ||= [];
    $UserCode->{object_states}  ||= [];

    if (ref $UserCode->{Filter} eq "ARRAY") {
        my @FilterData = @{ delete $UserCode->{Filter} };
        $UserCode->{Filter} = shift @FilterData;
        $UserCode->{FilterArgs} = [ @FilterData ];
    }

    @{ $UserCode->{UserStates} }{ qw( _start _end _child ) } =
        delete @{ $UserCode->{inline_states} }{ qw( _start _end _child ) };

    POE::Session->create
        ( inline_states => { %{ delete $UserCode->{inline_states} }, %$Code },
          object_states     => delete $UserCode->{object_states},
          package_states    => delete $UserCode->{package_states},
          options           => delete $UserCode->{options},
          args              => delete $UserCode->{args},
        );
}

sub fetchCHEAP ($) {
    no warnings;
    return $POE::Component::Client::TCPMulti::Heap{$_[0]};
}

package POE::Component::Client::TCPMulti::CHEAP;
sub ID {
    shift->{TM_ID}
}
sub ADDR {
    shift->{TM_ADDR}
}
sub PORT {
    shift->{TM_PORT}
}

sub filter {
    shift->{TM_SERVER}->set_input_filter( shift->new(@_) );
}


1;

#-User POD----------------------------------------------------------------------

=head1 NAME

 POE::Component::Client::TCPMulti

=head1 SYNOPSIS

 POE::Component::Client::TCPMulti->new
 ( InputEvent   => sub {
       $_[KERNEL]->yield(send => $_[CHEAP]->ID, "Some Stuff");
   },

   Initialize   => \&InitWierdness,
   ErrorEvent   => \&ErrorHandle,   
   Disconnected => \&ErrorHandle,   
   TimeoutEvent => \&TimeoutHandle,
   FailureEvent => \&FailureHandle,
   SuccessEvent => \&SuccessHandle,

   Domain       => AF_INET,     # Optional

   Alias        => "MySession", # Optional
   Timeout      => 30,          # Seconds, Optional
   Filter       => "POE::Filter::Something", # Optional

   inline_states => {
       _start => sub {
           $_[KERNEL]->yield(connect => q(127.0.0.1), 25);
       },
   },
   
   args => $Session_Args                # Optional
   object_states => $Object_States      # Optional
   package_states => $Package_States    # Optional

 );


=head1 DESCRIPTION

POE::Component::Client::TCPMulti is a very lightweight, highly optimized
component designed for large numbers of simultanious outgoing connections.
The major advantage to this module over POE::Component::Client::TCP is that
it runs in a single session, reguardless of the number of outgoing
simultanious connections.  I have found this in fact to use considerable
less overhead than POE::Component::Client::TCP in high traffic.  The
disadvantage lies mearly in the API complexity over 
POE::Component::Client::TCP.  

It is in fact due to this added API complexity that I decided to create a
seperate module, rather than altering POE::Component::Client::TCP [ or
coaxing Rocco to let me ].  POE::Component::Client::TCP is a great module
and this is not designed to completely replace it.  It is however designed
as a solution for extremely high traffic situations when the overhead of an
individual session for each outgoing connection is not appropriate for the
added simplicity in the API.  Especially considering that this API is not
really *that* much more complex.

Over all I tried as hard as possible to make crossing your application over
from POE::Component::Client::TCP as simple as possible.

=head1 CONSTRUCTOR PARAMETERS 

=over 2

=item SuccessEvent

SuccessEvent, takes a CODE reference as a parameter, and is the event
which will be called after a connection attempt has decidedly been successful.
(See L<POE::Wheel::SocketFactory>)

 "ARG0" will hold the new socket handle, which you
          should never actually need.
 "ARG1" will hold the sockets remote address, which is packed.
          You will need to use inet_ntoa() (See L<Socket>)
          if a human readable version is neccesary.
 "ARG2" will hold the sockets remote port.
 "ARG3" holds the OLD id for the connection.
 "ARG4" holds the NEW id for the connection, 
          synonymous with $_[CHEAP]->ID

=item FailureEvent

FailureEvent, takes a CODE references as a parameter.  FailureEvent will
be called when a socket error occurs while attempting to create the
connection. (See L<POE::Wheel::SocketFactory>)

 "ARG0" contains the name of the operation that failed.
 "ARG1" and "ARG2" hold numeric and string 
          values describing the error.
 "ARG3" contains the Wheel's unique ID,
          synonymous with $_[CHEAP]->ID 
          
=item ErrorEvent

ErrorEvent, takes a CODE reference as a parameter.  It is the event that
will be called after a connection has been successfull, but has closed
unexpectedly.  (See L<POE::Wheel::ReadWrite>)

 "ARG0" contains the name of the operation that failed.  
          This is not a function name, but an action 
          (usually 'read').
 "ARG1" and "ARG2" hold numeric and string 
          values describing the error.
 "ARG3" contains the connections unique ID,
          again synonymous with $_[CHEAP]->ID

=item Disconnected

Disconnected takes a CODE reference, and is the event taht will be called
after a shutdown event was succesfull.  This only affects connection
closures requested by your program.

=item TimeoutEvent

TimeoutEvent takes a CODE reference, and is the event that will be called
when a connection has been idle for longer than the specified value of
Timeout.  (See Timeout below)  When this event occurs, Disconnected will not
be called.

=item Initialize

Initialization is wierdness.  Its designed to replace '_start' in multisession
applications.  Its simply called, is only here for integration convience, and
will eventually be depriciated.  ( It is after all, something of a hack ).
Basically in ::Client::TCP applications, many people use _start at the begining
of thier session to do various initialization for thier connection.  This is
what you would use until you finished making a ::Client::TCPMulti application
out of your ::Client::TCP application.  I dont suggest using it anything more
than temporarily, it is very likely to not exist in future versions.

=item inline_states

inline_states will actually create inline states with 3 exceptions, _start,
_child and _end inline states, and any inline state named "connect", "shutdown",
"send", or "die" will be overwritten.  However, _start, _child, or _end inline
states will be called during _start, _child, and _end appropriately, only
prior to ::Client::TCPMulti completing its own internal tasks for these times.
I cant really see any reason for using _child within the session this component
creates, but you never know :)  If you're trying to figure out why your _start
only gets called once, see Initialization is wierdness, above.

=item object_states

object_states will be passed on to the created POE::Session, and is expected
to be an ARRAY ref.  (See L<POE::Session>)

=item package_states

package_states will be passed on to the created POE::Session, and is expected
to be an ARRAY ref.  (See L<POE::Session>)

=item options

options describes the options to be set for the created POE::Session, and is
expected to be a hashref.  (See L<POE::Session>).  Useful options
are commonly trace and assert, which turn on trace and assertion debug output
for the session:

options => { trace => 1, assert => 1 },

=item args

args will be passed on to the created POE::Session, and is expected to be an
ARRAY ref.  The value of args will be passed on to the _start state of your
code.  (See L<POE::Session>)

=item Timeout

Timeout takes a integer as a parameter, and is the value in seckonds in which 
each connection's timeout alarm will be executed if there is no incoming data
from the connection.  The default is 30 seconds.

=item Alias

Alias takes a string as a parameter, which will be the alias for the session
this component creates.  (See L<POE::Session>) 

=back

=head1 INLINE STATES

This component defines a number of inline states which cannot be overridden.
They are used as part of the API, for performing tasks that were handled in
the constructor of ::Client::TCP, as well as a few which are predefined for
convenience.

=over 2

=item connect

The "connect" state creates a new connection to the specified remote address
and port, using the optionally specified local address and port.  It can be
posted to, yielded, or called just as a normal inline state would be.  The
syntax for such would be something like:

 $_[KERNEL]->yield(connect => $ip, $port, $BindIP, $BindPort);
 where $BindIP and $BindPort are optional.

=item send

The "send" state appends data to a connections queue for sending.  It is
almost exactly the same as the "send" state used in the POE Cookbook.  It
takes a connection id, and data as an arguement.  The suggested syntax for
it would be like:

 $_[KERNEL]->yield(send => $_[CHEAP]->ID, "some data");

=item shutdown

The "shutdown" state attempts to close a connection gracefully.  It is the
same as the "shutdown" state for ::Client::TCP.  It takes a
connection id as an arguement, the suggest syntax is something like:

 $_[KERNEL]->yield(shutdown => $_[CHEAP]->ID);

=item die

The "die" state attempts to close all open connections gracefully, ending the
session.  It takes no arguements:

 $_[KERNEL]->yield("die");

=back

=head1 OPTIMIZATIONS

This module has a number of optimizations, as it is in fact designed for
extremely high traffic situations, and easy migration from ::Client::TCP.

=head3 Event Routing

All component event routing is done independantly of POE::Kernel.  While it
is true that POE::Kernel is extremely fast, and very light weight, it is
already issuing the events to this Components inline states.  So while it is
a common practice to use POE::Kernel for Component Event routing, it has been
opted against.  Its just extra overhead, and each of this module's inline
states are extremely low in overhead, so all event routing is done completely
aside from any event queue.  The event queue is used to issue the intial event.

Alot of testing has proven this to actually create a faster runtime without
reducing responsive time of POE.  In fact, in most instances it was greatly
increased since less work was put in the event queue uneccesarily.

=head3 Localized Heap

The $_[CHEAP] your event states will recieve is slightly magical.  This was
done namely as a migration route for applications designed using ::Client::TCP.
Each *connection* has its own internal heap, which can be accessed via $_[CHEAP].
This was provided as a solution to each connection not having its own session,
and in turn, its own $_[HEAP].  Events that do not go through the component will
have a $_[CHEAP] which is undefined.  So if you want it, you will have to fetch
it (Or just store a $_[CHEAP] reference for each connection in your $_[HEAP]).
See fetchCHEAP for fetching the $_[CHEAP].

Again, yes, you can use $_[CHEAP] as a "normal" heap.  It will contain
elements with special meanings, but they are all prefixed with ``TM_''.

=head3 CHEAP magic

$_[CHEAP] has other magic aside from being a localized heap, it is a blessed
reference with several methods.  Some of these are replacement for Session-Wise
methods you would usually use to identify your current connection in the
::Client::TCP component.  Others, are for directly modifying your ReadWrite
wheel to do things that aren't possible in ::Client::TCP [ but I needed, so you
get the features :D ].

=head3 CHEAP Methods

=over 4

=item $_[CHEAP]->ID

The ``ID'' method returns the current ID of the current connection.  I would
not rely on this being static, but if you need to use it for some kind of
internal indexing, just pay mind to the fact that your ID will change after
the connection was successful.  The reason for this is that the Wheel ID is
used for connection indexing by ::Client::TCPMulti.

=item $_[CHEAP]->ADDR

Unlike ::Client::TCP, ::Client::TCPMulti keeps track of information about your
connection for you.  So yes, you can go through and delete a bunch of that
code.  The ``ADDR'' method returns the address you are connecting or connected
to, in Local Short IP notation (255.255.255.255).

=item $_[CHEAP]->PORT

The ``PORT'' method returns the PORT you are connected to, as an integer.

=head4 $_[CHEAP]->filter("POE::Filter::Something", Option => "value");

``filter'' is a very special method for CHEAP, it lets you change the filter
the B<current> connection is currently using, in real time.  You must 'use'
or 'require' the Filter module somewhere in your code if you want to use it
(unless its POE::Filter::Line).

=item $_[CHEAP]->event() - Removed

This method has been removed.  If you need to change an event, I suggest
you make the first statement in your event handler reroute the request
based a $_[CHEAP] element.

=back

=head1 BUGS

Probably tons, let me know if you find any.  Currently, an occasional
unnecessary timeout will be called on an errored connection.  These 
timeouts are ignored by error checking code since the connection will
no longer exist.  This should be fixed very shortly.

Multiple Sessions will probably not work correctly.  This is because
the code references provided for each session is globalized throughout
the component's package.  So subsequent sessions would override previous
ones.  This will eventually be fixed when I think of a good plan for how. :)

=head1 AUTHOR

=over 2

=item Scott S. McCoy

Scott S. McCoy is (I<tag@cpan.org>)
 
Thanks to Rocco Caputo, author of POE.

This software is released under the BEERWARE license, a free software license. 

=back

The End

=cut
