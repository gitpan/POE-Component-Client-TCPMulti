#!/usr/bin/perl
# $Id: Makefile.PL,v 1.6 2002/09/10 03:30:51 rcaputo Exp $

use ExtUtils::MakeMaker;

# Touch CHANGES so it exists.
open(CHANGES, ">>CHANGES") and close CHANGES;

WriteMakefile
  ( NAME         => 'POE::Component::Client::TCPMulti',
    AUTHOR       => 'Scott S. McCoy (tag@cpan.org)',
    ABSTRACT     => 'POE component for High Connectivity TCP Clients',
    VERSION_FROM => 'TCPMulti.pm',

    PM           => { 'TCPMulti.pm' => '$(INST_LIBDIR)/TCPMulti.pm' },
    PREREQ_PM    => { POE      => 0.11 },
    dist         =>
    { COMPRESS   => 'gzip -9f',
      SUFFIX     => 'gz',
      PREOP      => ( 'svn log | ' .
                      'tee ./$(DISTNAME)-$(VERSION)/CHANGES > ./CHANGES'
                    ),
    },
  );
