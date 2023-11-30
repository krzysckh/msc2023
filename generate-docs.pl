#!/usr/bin/perl
# ad-hoc generator dokumentacji dla tego projektu
# kiedyś napiszę coś bardziej zgeneralizowanego tego rodzaju
# ale jest jak jest
#
# ~ krzych
#
# używanie
# (define (funkcja-w-scheme a b c)
#   "tu dokumentacja
#
#   @{a,linia o argumencie a}
#   @{b,linia o argumencie b}
#   @{c,linia o argumencie c}
#
#   @[(funkcja-w-scheme x y z),"wartość zwracana"]
#   "
#   ...)
#
# nie podoba mi się ten markup ale no cusz
#
# TODO: funkcje z src/scheme-interop.c
# TODO: funkcje z . czymś nie działają

use strict;
use warnings;

use Data::SExpression;
use File::Slurp qw(read_file);
use Class::Struct;

struct(
  func => {
    nam => '$',
    doc => '$',
    args => '@',
    ex => '@'
  }
);

struct(
  arg => {
    name => '$',
    doc => '$'
  }
);

struct(
  example => {
    expr => '$',
    doc => '$'
  }
);

struct(
  file => {
    name => '$',
    data => '@',
  }
);

# okropieństwo
sub parse_doc($$) {
  my ($doc, $definition) = @_;
  my $ret = func->new();

  $doc = "" if not defined $doc;

  $doc =~ s/\n//gm;
  $doc =~ s/\s+/ /gm;

  $ret->nam($definition->[0]->name);
  shift(@{$definition});

  my @argdocs_a = ($doc =~ /\@\{.*?\}/g);
  my %argdocs;

  for (@argdocs_a) {
    /\@\{(.*?)\,(.*?)\}/;
    $argdocs{$1} = $2;
  }

  my @argnames = map {eval { $_->name }} @{$definition};
  my @args;


  push @args, arg->new(name => $_, doc => $argdocs{$_}) for @argnames;

  $ret->args([@args]);

  my @examples;
  my @exampledocs = ($doc =~ /\@\[.*?\]/g);
  for (@exampledocs) {
    /\@\[(.*?)\,(.*?)\]/;
    push @examples, example->new(expr => $1, doc => $2);
  }

  $ret->ex([@examples]);
  $doc =~ s/(\@\{.*?\})|(\@\[.*?\])//g;
  $doc =~ s/(?:\s|^)($_)(?:\s|$|,|\.)/ `$1` /g for @argnames;

  $ret->doc($doc);

  return $ret
}

sub render {
  my (@files) = @_;

  for (@files) {
    my $f = $_;
    print "## [" . $f->name . "](https://git.krzysckh.org/kpm/science-cup-2023/src/branch/master/" . $f->name . ")\n\n";
    for (@{$f->data}) {
      print "### `(" . $_->nam . " " . join(" ", map({$_->name} @{$_->args})) . ")`\n";
      print($_->doc . "\n\n") if ref($_->doc) eq "";
      print "\n*argumenty*\n" if scalar @{$_->args};
      for (@{$_->args}) {
        print "- `" . $_->name . "`";
        print(" - ". $_->doc) if defined $_->doc;
        print "\n";
      }
    }
  }
  print "\n\n";
}

my $sr = Data::SExpression->new({fold_lists => 1, fold_alists => 1,
                                 use_symbol_class => 1});
my @sources = glob("scm/*.scm");
my @files;

for (@sources) {
  my $fname = $_;
  my $data = read_file($fname);
  my @defs;
  my @funcs;

  $data =~ s/;.*$//gm;
  $data =~ s/\#((?:t|f|(?:true)|(?:false)))/'$1/gm; # hack

  while (1) {
    my $sexp;
    eval { ($sexp, $data) = $sr->read($data); };

    last if $@;

    my ($define, $definition, $doc, @rest) = @{$sexp};

    if ($define eq "define" and ((ref($definition) eq "ARRAY") or
        (ref($definition) eq "Data::SExpression::Cons"))) {
      if (ref($definition) eq "Data::SExpression::Cons") {
        $definition = [$definition->car, $definition->cdr]
      } else {
        $definition = [$definition] if (ref($definition) ne "ARRAY")
      }

      push @defs, [$definition, $doc];
    }
  }

  for (@defs) {
    push @funcs, parse_doc($_->[1], $_->[0]);
  }
  push @files, file->new(name => $fname, data => [@funcs]);
  print STDERR "OK $fname\n"
}

render @files
