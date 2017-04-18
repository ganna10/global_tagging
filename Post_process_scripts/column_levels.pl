#! /usr/bin/env perl
# Calculate column levels of species. ARGV is file of one day with hourly data 
# Version 0: Jane Coates 5/1/2017

use strict;
use diagnostics;
use PDL;
use PDL::NetCDF;
use PDL::NiceSlice;
use Fcntl;

#my $in_file = "HTAP_NOx_Tagging.20161117.cam.h2.2010-12-30.O3.nc";
my $in_file = $ARGV[0];
die "No input file specified : $!" if (@ARGV == 0); # check for argument
die "Incorrect file\n" unless ($in_file =~ /\.O3\.nc$/); # check for correct argument

# open nc file
my $nc = PDL::NetCDF->new($in_file, {MODE => O_RDONLY});
my $ntime = $nc->dimsize('time');
my $nlat = $nc->dimsize('lat');
my $nlon = $nc->dimsize('lon');
my $nlev = $nc->dimsize('lev');

my $lat = $nc->get('lat', [0, 0], [$nlat, 1]);
my $lon = $nc->get('lon', [0, 0], [$nlon, 1]);
my $lev = $nc->get('lev', [0, 0], [$nlev, 1]);
my $time = $nc->get('time', [0, 0], [$ntime, 1]);

my $vars = $nc->getvariablenames();
my @o3_spcs;
foreach my $var (@$vars) {
    push @o3_spcs, $var if ($var =~ /^O3/);
}

my $T0 = 273.15; # standard temperature in K
my $R = 287.3; # specific gas constant for air in J/kg/K
my $g0 = 9.80665; # average gravity at mean sea level in m/s2
my $p0 = 1.01325e5; # standard pressure in Pa

my %dobson_units;
foreach my $spc (@o3_spcs) { 
    my $data = $nc->get($spc);
    #my $data = $nc->get($spc, [0, 0, 0, 0], [$ntime, $nlev, 1, 1]); 
    my @dims = $data->dims; # lon, lat, lev, time (144, 96, 56, 24) 
    my @level_summations;
    foreach my $level (0..$nlev-1) {
        last if ($level + 1 >= $nlev);
        #print "Level: $level\n";
        #print $lev($level), "\n";
        #print $lev($level+1), "\n";
        #print $data($level, :)->squeeze, "\n";
        #print $data($level+1, :)->squeeze, "\n";
        my $vmr_sum = $data(:, :, $level, :)->squeeze + $data(:, :, $level + 1, :)->squeeze;
        my $pressure_diff = $lev($level + 1)->squeeze - $lev($level)->squeeze;
        my $summation = 0.5 * $vmr_sum * 1e6 * $pressure_diff; #vmr in ppm
        push @level_summations, $summation;

        #testing at 1 long and 1 latitude
        #my $test_sum = $vmr_sum(100, 45, :)->squeeze;
        #print $test_sum, "\n";
        #my @test_dims = $test_sum->dims;
        #print "@test_dims\n";
        #print $pressure_diff, "\n";
        #my $test_summation = $summation(100, 45, :)->squeeze;
        #print $test_summation, "\n";
    }
    my $summation = 0;
    $summation += $_ foreach (@level_summations);
    my $DU = (10 * $R * $T0 * $summation) / ($g0 * $p0);
    $dobson_units{$spc} = $DU;
    #my @du_dims = $DU->dims;
    #print "@dims\n";
    #print  "@du_dims\n";
}
$nc->close();

# output to nc file
(my $out_file = $in_file) =~ s/O3\.nc$/ColumnO3.nc/;
my $nc_out = PDL::NetCDF->new($out_file, {MODE => O_CREAT | O_RDWR});

$nc_out->put('lat', ['lat'], double $lat);
$nc_out->putatt('degrees_north', 'units', 'lat');
$nc_out->putatt('latitude', 'long_name', 'lat');

$nc_out->put('lon', ['lon'], double $lon);
$nc_out->putatt('degrees_east', 'units', 'lon');
$nc_out->putatt('longitude', 'long_name', 'lon');

$nc_out->put('time', ['time'], double $time);
$nc_out->putatt('days since 2010-01-01 00:00:00', 'units', 'time');
$nc_out->putatt('time', 'long_name', 'time');
$nc_out->putatt('gregorian', 'calendar', 'time');
$nc_out->putatt('time_bnds', 'bounds', 'time');

foreach my $spc (sort keys %dobson_units) { 
    $nc_out->put($spc, ['time', 'lat', 'lon'], float $dobson_units{$spc});
    $nc_out->putatt('Dobson_units', 'units', $spc);
    $nc_out->putatt("Column levels in Dobson Units for $spc", 'long_name', $spc);
}

$nc_out->close();
