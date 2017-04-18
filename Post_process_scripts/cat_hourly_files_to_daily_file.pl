#! /usr/bin/env perl
# concatentate hourly history files to a single daily file. Argument is h-number of history files
# Version 0: Jane Coates 1/11/2016

use strict;
use diagnostics;
use Cwd qw( cwd );

die "List history file h-number" if (@ARGV == 0);

my $h_file = $ARGV[0];
my $hist_dir = cwd();
(my $case = $hist_dir) =~ s/^(.*?)archive\/(.*?)\/atm(.*?)$/$2/;
my $file_base = $case . ".cam." . $h_file . ".2010-";

my @months = (1..12);
my @days = (1..31);

foreach my $month (@months) {
    foreach my $day (@days) {
        print $day, "\n";
        my $file = $file_base . sprintf("%02d", $month) . "-" . sprintf("%02d", $day);
        my $out_file = $file . ".O3.nc";

        opendir my $dh, $hist_dir or die "Can't read $hist_dir : $!";
        my @daily_files = grep { $_ =~ $file } readdir $dh;
        print "No history files for day $day in month $month\n" if (scalar @daily_files == 0);
        my $in_files = join ' ', sort @daily_files;
        system "ncrcat -O $in_files $out_file";
        closedir $dh;
    }
}

print "done\n";
