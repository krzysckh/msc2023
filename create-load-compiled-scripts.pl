#!/usr/bin/perl

use v5.10;

use strict;
use warnings;

my @l = (
  "tinyscheme/r5rs.scm",
  "scm/util.scm",
  "scm/interop-helpers.scm",
  "scm/system-hooks.scm"
);

for (@ARGV) {
  my $cur = $_;
  push @l, $cur if (not grep { $_ eq $cur } @l);
}

print <<_
#include "optyka.h"
extern scheme scm;
_
;

print <<__
void load_compiled_scripts(void) {
__
;

for (@l) {
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

