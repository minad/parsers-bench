#!/usr/bin/perl -w
use strict;
use File::Slurper qw(read_text);
my $p = read_text "bench-paripari.txt";
my $m = read_text "bench-megaparsec.txt";

my %pmap = ();
while ($p =~ /benchmarking (.*?)\n.*?time +([\d\.]+) ([^ ]+)/gs) {
    my $name = $1;
    my $time = 0.0 + $2;
    my $unit = $3;
    $time *= $unit eq "ns" ? 1e-3 : 1;
    $pmap{$name} = $time;
}
my %mmap = ();
while ($m =~ /benchmarking (.*?)\n.*?time +([\d\.]+) ([^ ]+)/gs) {
    my $name = $1;
    my $time = 0.0 + $2;
    my $unit = $3;
    $time *= $unit eq "ns" ? 1e-3 : 1;
    if (exists($pmap{$name})) {
        if ($pmap{$name} > $time) {
            my $speedup = int($pmap{$name} / $time * 100) / 100.0;
            print "megaparsec faster $name $time $pmap{$name} ${speedup}x\n";
        } else {
            my $speedup = int($time / $pmap{$name} * 100) / 100.0;
            print "paripari faster $name $pmap{$name} $time ${speedup}x\n";
        }
    } else {
        #print "$name does not exist\n";
    }
}
