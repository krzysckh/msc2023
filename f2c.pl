#!/usr/bin/perl

use strict;
use warnings;

use feature 'say';

my $nam = $ARGV[0];
$nam =~ s/[.\/\\\-]/_/g;
open my $f, '<', $ARGV[0];

print "char $nam\[\] = {";

for (<$f>) {
    print map { ord($_) . "," } split //, $_;
}

print "0};";
