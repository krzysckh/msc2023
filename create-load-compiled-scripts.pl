#!/usr/bin/perl

use strict;
use warnings;

@ARGV = sort @ARGV;

print <<_
#include "optyka.h"
extern scheme scm;
_
;

print <<__
void load_compiled_scripts(void) {
__
;

for (@ARGV) {
  my $orig = $_;
  $_ =~ s/[\._\-\/]/_/g;
  print <<_
  extern char $_\[\];
  scheme_load_string(&scm, $_);
  TraceLog(LOG_INFO, "loaded builtin $orig");
_
;
}

print <<_
}
_

