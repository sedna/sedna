#! /usr/bin/perl -w


$f = `cat "$ARGV[0]"` or die;
system("mv \"$ARGV[0]\" \"$ARGV[0]\"~");

$f =~ s{//(.*)$}{/\*$1\*/}gm;

open(O, ">$ARGV[0]");
print O $f;
close O;
