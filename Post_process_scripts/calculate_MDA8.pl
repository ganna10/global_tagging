#! /usr/bin/env perl
# Calculate maximum daily 8hr mean at surface level. ARGV is file of one day with hourly data
# Version 0: Jane Coates 3/11/16
# Version 1: Jane Coates 9/1/17 getting MDA8 of tagged species at same time period as non-tagged O3
# Version 2: Jane Coates 10/3/17 adding time, date, datesec to output

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
#my $nlev = $nc->dimsize('lev');

my $lat = $nc->get('lat', [0, 0], [$nlat, 1]);
my $lon = $nc->get('lon', [0, 0], [$nlon, 1]);
# my $lev = $nc->get('lev', [0, 0], [$nlev, 1]);
# variables needed in output
my $time = $nc->get('time');
my $date = $nc->get('date');
my $datesec = $nc->get('datesec');

my $vars = $nc->getvariablenames();
my @o3_spcs;
foreach my $var (@$vars) {
    push @o3_spcs, $var if ($var =~ /^O3/);
}

my %mda8;
my $non_tagged_spc = "O3";
my $non_tagged_data = $nc->get($non_tagged_spc, [0, 54, 0, 0], [$ntime, 1, $nlat, $nlon]);
my $non_tagged_mda8 = zeroes($nlon, $nlat);
my $mda8_hr_start = zeroes($nlon, $nlat);

for (my $j = 0; $j < $nlon; $j++) { # loop over longitude
    for (my $i = 0; $i < $nlat; $i++) { # loop over latitude
        foreach my $hr (7..23) {
            last if ($hr + 7 >= 24);
            my $slice_8hr = $non_tagged_data($j, $i, $hr:$hr+7)->squeeze;
            my $slice_av = 0;
            $slice_av = $slice_8hr->average;
            if ($slice_av > $non_tagged_mda8($j, $i)) {
                $non_tagged_mda8($j, $i) .= $slice_av;
                $mda8_hr_start($j, $i) .= $hr;
            }
        }
    }
}
$mda8{$non_tagged_spc} = $non_tagged_mda8 * 1e9;

foreach my $spc (@o3_spcs) { # get surface data
    next if ($spc eq "O3");
    my $tagged_mda8 = zeroes($nlon, $nlat);
    my $data = $nc->get($spc, [0, 54, 0, 0], [$ntime, 1, $nlat, $nlon]); 
    for (my $j = 0; $j < $nlon; $j++) { # loop over longitude
        for (my $i = 0; $i < $nlat; $i++) { # loop over latitude
            my $hr_start = $mda8_hr_start($j, $i)->squeeze;
            my $hr_end = $hr_start + 7;
            my $slice_8hr = $data($j, $i, $hr_start:$hr_end)->squeeze;
            my $slice_av = 0;
            $slice_av = $slice_8hr->average;
            $tagged_mda8($j, $i) .= $slice_av;
        }
    }
    $mda8{$spc} = $tagged_mda8 * 1e9; 
}
$nc->close();

# output to nc file
(my $out_file = $in_file) =~ s/O3\.nc$/MDA8.O3.nc/;
my $nc_out = PDL::NetCDF->new($out_file, {MODE => O_CREAT | O_RDWR});


$nc_out->put('lat', ['lat'], float $lat);
$nc_out->putatt('degrees_north', 'units', 'lat');
$nc_out->putatt('latitude', 'long_name', 'lat');

$nc_out->put('lon', ['lon'], float $lon);
$nc_out->putatt('degrees_east', 'units', 'lon');
$nc_out->putatt('longitude', 'long_name', 'lon');

$nc_out->put('time', ['time'], double $time);
$nc_out->putatt('days since 2010-01-01 00:00:00', 'units', 'time');
$nc_out->putatt('time', 'long_name', 'time');
$nc_out->putatt('gregorian', 'calendar', 'time');
$nc_out->putatt('time_bnds', 'bounds', 'time');

$nc_out->put('date', ['date'], double $date);
$nc_out->putatt('current date (YYYYMMDD)', 'long_name', 'date');

$nc_out->put('datesec', ['datesec'], double $datesec);
$nc_out->putatt('current seconds of current date', 'long_name', 'datesec');

foreach my $spc (sort keys %mda8) { 
    $nc_out->put($spc, ['lat', 'lon'], float $mda8{$spc});
    $nc_out->putatt('ppbv', 'units', $spc);
    $nc_out->putatt("Maximum daily 8hr average $spc mixing ratio", 'long_name', $spc);
}

$nc_out->close();

system("ncks -O --mk_rec_dmn time $out_file $out_file");
