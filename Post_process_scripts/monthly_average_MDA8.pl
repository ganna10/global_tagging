#! /usr/bin/env perl
# get monthly averages of O3 MDA2
# Version 0: Jane Coates 9/11/2016

use strict;
use diagnostics;
use Cwd qw( cwd );

my $pwd = cwd();
print "Calculating monthly average O3 MDA8 in $pwd\n";
(my $case = $pwd) =~ s/^(.*?)archive\/(.*?)\/atm(.*?)$/$2/;
my @months = (1..12);
#my @months = (9);

opendir my $dir, $pwd or die "Can't open $pwd : $!";;
my @files = grep { $_ =~ /MDA8/ } readdir $dir;
closedir $dir;

foreach my $month (@months) {
    my $leading_month = sprintf("%02d", $month);
    my @monthly_files = grep { $_ =~ /2010-$leading_month-\d\d\.MDA8\.O3\.nc/ } @files;
    my $file_list = join ' ', @monthly_files;
    my $out_file = $case . ".2010-$leading_month.meanMDA8.nc";
    my $timavg_out = $case . ".2010-$leading_month.timavg.meanMDA8.nc";
    system "ncea -O $file_list $out_file";
    system "cdo timavg $out_file $timavg_out";
}
