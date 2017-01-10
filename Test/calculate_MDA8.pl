#! /usr/bin/env perl
# Calculate maximum daily 8hr mean at surface level. ARGV is file of one day with hourly data
# Version 0: Jane Coates 3/11/16

use strict;
use diagnostics;
use PDL;
use PDL::NetCDF;
use PDL::NiceSlice;
use Fcntl; # nc read/write

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
# my $lev = $nc->get('lev', [0, 0], [$nlev, 1]);
my $time = $nc->get('time', [0, 0], [$ntime, 1]);

my $vars = $nc->getvariablenames();
my @o3_spcs;
foreach my $var (@$vars) {
    push @o3_spcs, $var if ($var =~ /^O3/);
}

my %mda8;
foreach my $spc (@o3_spcs) { # get surface data
    my $data = $nc->get($spc, [0, 54, 0, 0], [$ntime, 1, $nlat, $nlon]);

    # calculate MDA8 at each grid point
    my @dims = $data->dims; # lon, lat, time (144, 96, 24)
    my $rolling_mean = zeroes($nlon, $nlat, $ntime);
    foreach my $hr (7..23) {
        last if ($hr + 7 >= 24);
        my $slice_8hr = $data(:, :, $hr:$hr+7);
        my @slice_dims = $slice_8hr->dims; # 144 96 8

        $rolling_mean(:, :, $hr) .= average($slice_8hr->reorder(2, 0, 1));
        
        # test what's going in at a single lat lon
        #my $slice_test = $slice_8hr(10, 9, :);
        #print $slice_test, "\n";
        #my $rolling_mean_test = $rolling_mean(10, 9, $hr);
        #print $rolling_mean_test, "\n";
    }

    my $mda8 = maximum($rolling_mean->reorder(2, 0, 1));
    $mda8 *= 1e9; # convert to ppbv
    my @mda8_dims = $mda8->dims; #144 96

    $mda8{$spc} = $mda8;

}
$nc->close();

# output to nc file
(my $out_file = $in_file) =~ s/O3\.nc$/MDA8.O3.nc/;
my $nc_out = PDL::NetCDF->new($out_file, {MODE => O_RDWR});

$nc_out->put('lat', ['lat'], float $lat);
$nc_out->putatt('degrees_north', 'units', 'lat');
$nc_out->putatt('latitude', 'long_name', 'lat');

$nc_out->put('lon', ['lon'], float $lon);
$nc_out->putatt('degrees_east', 'units', 'lon');
$nc_out->putatt('longitude', 'long_name', 'lon');

foreach my $spc (sort keys %mda8) { 
    $nc_out->put($spc, ['lat', 'lon'], float $mda8{$spc});
    $nc_out->putatt('ppbv', 'units', $spc);
    $nc_out->putatt("Maximum daily 8hr average $spc mixing ratio", 'long_name', $spc);
}

$nc_out->close();
