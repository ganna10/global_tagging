#! /usr/bin/env perl
# get monthly averages of O3 column
# Version 0: Jane Coates 6/1/2017

use strict;
use diagnostics;
use Cwd qw( cwd );

my $pwd = cwd();
print "Calculating monthly average column O3 in $pwd\n";
#my @months = (1..12);
my @months = (1);

opendir my $dir, $pwd or die "Can't open $pwd : $!";;
my @files = grep { $_ =~ /ColumnO3/ } readdir $dir;
closedir $dir;

foreach my $month (@months) {
    my $leading_month = sprintf("%02d", $month);
    my @monthly_files = grep { $_ =~ /2010-$leading_month-\d\d\.ColumnO3\.nc/ } @files;
    my $file_list = join ' ', @monthly_files;
    my $out_file = "HTAP_tagged_base.2010-$leading_month.avColumnO3.nc";
    my $timeavg_out_file = "HTAP_tagged_base.2010-$leading_month.timeavColumnO3.nc";
    system "ncea -O $file_list $out_file";
    system "cdo timavg $out_file $timeavg_out_file";
}
