#! perl
#
use PDL;
use PDL::NetCDF;
use PDL::NiceSlice;
use warnings;
use strict;

my %ignore_models = (
	'HCMAQ_BASE' => 1,      # wierd lat/lons
	'HadGEM2-ES_BASE' => 1, # lons start at zero
	'MOZART-4_BASE' => 1,   # lons start at zero
);

my $tier2file = "M:/users/tbu/HTAP2/HTAP_Phase2_tier2NC01x01.nc";
my $nc = PDL::NetCDF->new($tier2file);
my $tier2lat = $nc->get('lat');
my $tier2lon = $nc->get('long');
my $region_code = $nc->get('region_code');
my $max = $region_code->max;
my @region_codes;
for (0..$max) {
	my $i = which($region_code == $_);
	if (defined $i and $i->nelem > 0) {
		push @region_codes, $_;
	}
}
#my %region_code = map { $_ => 1 } list $region_code;
#my @region_codes = sort keys %region_code;
#my @region_codes = qw( 20 21 22 23 24 25 26 27 28 31 32 33 34 35 36 41 42 43 44 51 52 53 54 61 62 63 64 65 66 71 72 81 82 83 91 92 93 101 102 103 111 112 113 121 122 123 124 131 132 133 134 141 142 143 150 151 160 161 171);
my @months = qw(JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC);

my $modeldir = "M:/users/jco/HTAP2_Model_Output";
opendir DIR, $modeldir or die $!;
my @models = grep /_BASE/, readdir DIR;
closedir DIR;

print "Model, region_code, ", join ',', @months, "\n";

foreach my $model (@models) {
	next if defined $ignore_models{$model};
	warn "\t$model\n";
	my $model_region_code;
	if (-f "$model.regioncode.nc") {
		my $nc = PDL::NetCDF->new("$model.regioncode.nc");
		$model_region_code = $nc->get('region_code');
	} else {
		my $modelfile = "$modeldir/$model/htap2_${model}_vmro3_ModelLevel_2010-JAN.nc";
		die "$modelfile: $!" unless -f $modelfile;
		my $nc = PDL::NetCDF->new($modelfile);
		my $lon = $nc->get('lon');
		if ($lon->at(0) >= 0) {
			die "\t\tbad lons\n";
		}
		my $lat = $nc->get('lat');
		$model_region_code = zeroes(short, $lon->nelem, $lat->nelem);
		my @lat_indices = ();
		foreach my $t (0 .. $tier2lat->nelem-1) {
			my $t2lat = $tier2lat->at($t);
			my $tmp = $lat - $t2lat;
			my $i = which($tmp->abs == $tmp->abs->min)->at(0);
			push @{ $lat_indices[$i] }, $t;
			#my $model_lat = $lat->at($i->at(0));
			#print "lat $t2lat is closest to $model_lat\n";
		}
		#print "Found neighbors for $#lat_indices points\n";
		#foreach my $i (0 .. $#lat_indices) { my $model_lat = $lat->at($i); my $tmp = $lat_indices[$i]; print "Model lat $model_lat is closest to lats @$tmp\n"; }
		my @lon_indices = ();
		foreach my $t (0 .. $tier2lon->nelem-1) {
			my $t2lon = $tier2lon->at($t);
			my $tmp = $lon - $t2lon;
			my $i = which($tmp->abs == $tmp->abs->min)->at(0);
			push @{ $lon_indices[$i] }, $t;
			#my $model_lon = $lon->at($i->at(0));
			#print "lon $t2lon is closest to $model_lon\n";
		}
		#print "Found neighbors for $#lon_indices points\n";
		#foreach my $i (0 .. $#lon_indices) { my $model_lon = $lon->at($i); my $tmp = $lon_indices[$i]; print "Model lon $model_lon is closest to lons @$tmp\n"; }
		foreach my $model_j (0 .. $#lat_indices) {
			foreach my $model_i (0 .. $#lon_indices) {
				my %code_frequencies;
				foreach my $tier2_j (@{ $lat_indices[$model_j] }) {
					foreach my $tier2_i (@{ $lon_indices[$model_i] }) {
						my $code_value = $region_code->at($tier2_i, $tier2_j);
						$code_frequencies{$code_value}++;
					}
				}
				#my @codes = sort { $code_frequencies{$b} <=> $code_frequencies{$a} } keys %code_frequencies;
				#$model_region_code($model_i, $model_j) .= $codes[0];
				$model_region_code($model_i, $model_j) .= [ sort { $code_frequencies{$b} <=> $code_frequencies{$a} } keys %code_frequencies ]->[0];
			}
		}
		my $nc_out = PDL::NetCDF->new(">$model.regioncode.nc");
		$nc_out->put('lat', ['lat'], $lat);
		$nc_out->put('lon', ['lon'], $lon);
		$nc_out->put('region_code', ['lat','lon'], $model_region_code);
	}
	my @code_indices;
	foreach my $code (@region_codes) {
		$code_indices[$code] = which($model_region_code == $code);
		warn "\t\tNo points found for region $code" unless defined $code_indices[$code] and $code_indices[$code]->nelem > 0;
	}
	my %results;
	foreach my $month (@months) {
		my $modelfile = "$modeldir/$model/htap2_${model}_vmro3_ModelLevel_2010-$month.nc";
		die "$modelfile: $!" unless -f $modelfile;
		my $nc = PDL::NetCDF->new($modelfile);
		my %variablenames = map { $_ => 1 } @{ $nc->getvariablenames };
		my $model_bottom;
		if (defined $variablenames{'lev'}) {
			my $lev = $nc->get('lev');
			if ($lev(1) - $lev(0) > 0) {
				$model_bottom = -1;
			} else {
				$model_bottom = 0;
			}
		} else {
			$model_bottom = 0;
		}
		my $vmro3 = $nc->get('vmro3')->(:,:,($model_bottom))->flat;
		foreach my $code (@region_codes) {
			my $avg_vmro3 = $vmro3->index($code_indices[$code])->avg;
			push @{ $results{$code} }, $avg_vmro3;
		}
	}
	foreach my $code (@region_codes) {
		print "$model, $code, ", join ',', @{ $results{$code} }, "\n";
	}
}
