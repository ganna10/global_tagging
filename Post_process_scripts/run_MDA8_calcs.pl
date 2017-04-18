#! /usr/bin/env perl
# Find all daily O3 nc files, calculate MDA8 and output to file
# Version 0: Jane Coates 4/11/2016

use strict;
use diagnostics;
use Cwd qw( cwd );

my $cwd = cwd();
my @months = (1..12);

foreach my $month (@months) {
    my $leading_month = sprintf("%02d", $month);
    opendir my $dir, $cwd or die "Can't open $cwd : $!";;
    my @files = grep { $_ =~ /\.2010-${leading_month}(.*?)\.O3\.nc$/ and $_ !~ /MDA8/ } readdir $dir;
    closedir $dir;

    foreach my $file (sort @files) {
        print "Calculating MDA8 for $file\n";
        system "perl calculate_MDA8.pl $file";
    }
}
