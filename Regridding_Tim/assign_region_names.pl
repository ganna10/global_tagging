#! /usr/bin/env perl
#
use strict;
use diagnostics;

my $file = "HTAP2_models_Tier2.csv";
open my $in, "<:encoding(utf-8)", $file or die $!;
my @lines = (<$in>);
close $in;

my $out_text = "Model,Region,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec\n";

foreach my $line (@lines) {
    next if ($line =~ /^Model/);
    chomp $line;
    my ($model_tag, $region, $rest) = split ', ', $line;
    my ($model, $base) = split '_', $model_tag;
    my $region_text = assign_region($region);
    $out_text .= $model . "," . $region_text . "," . $rest . "\n";
}
my $out_file = "HTAP_ensemble_assigned_Tier2_regions.csv";
open my $out, ">:encoding(utf-8)", $out_file;
print $out $out_text;
close $out;
print "perform :%s/,\\r//g on $out_file\n";

sub assign_region {
    my ($code) = @_;
    
    my $region;
    if ($code == 20) {
        $region = "Baltic Sea";
    } elsif ($code == 21) {
        $region = "North Atlantic";
    } elsif ($code == 22) {
        $region = "South Atlantic";
    } elsif ($code == 23) {
        $region = "North Pacific";
    } elsif ($code == 24) {
        $region = "South Pacific";
    } elsif ($code == 25) {
        $region = "Indian Ocean";
    } elsif ($code == 26) {
        $region = "Hudson Bay";
    } elsif ($code == 27) {
        $region = "Mediterranean Sea";
    } elsif ($code == 28) {
        $region = "Black and Caspian Sea";
    } elsif ($code == 31) {
        $region = "NE US";
    } elsif ($code == 32) {
        $region = "SE US";
    } elsif ($code == 33) {
        $region = "NW US";
    } elsif ($code == 34) {
        $region = "SW US";
    } elsif ($code == 35) {
        $region = "E Canada";
    } elsif ($code == 36) {
        $region = "W Canada and Alaska";
    } elsif ($code == 41) {
        $region = "NW Europe";
    } elsif ($code == 42) {
        $region = "SW Europe";
    } elsif ($code == 43) {
        $region = "E Europe";
    } elsif ($code == 44) {
        $region = "Greece; Turkey; Cyprus";
    } elsif ($code == 51) {
        $region = "N India; Nepal; Bangladesh; Afghanistan; Pakistan";
    } elsif ($code == 52) {
        $region = "S India; Sri Lanka";
    } elsif ($code == 53) {
        $region = "Indian Himalaya";
    } elsif ($code == 61) {
        $region = "NE China";
    } elsif ($code == 62) {
        $region = "SE China";
    } elsif ($code == 63) {
        $region = "W China; Mongolia";
    } elsif ($code == 64) {
        $region = "N Korea; S Korea";
    } elsif ($code == 65) {
        $region = "Japan";
    } elsif ($code == 66) {
        $region = "China; Tibet Himalaya";
    } elsif ($code == 111) {
        $region = "Lebanon; Israel; Jordan; Syria";
    } elsif ($code == 112) {
        $region = "Saudi Arabia; Yemen; Oman; UAE; Qatar; Bahrain";
    } elsif ($code == 113) {
        $region = "Iran; Iraq";
    } elsif ($code == 141) {
        $region = "W Russia";
    } elsif ($code == 142) {
        $region = "E Russia";
    } elsif ($code == 143) {
        $region = "Belarus; Ukraine";
    } else {
        $region = "Rest";
    }
    return $region;
}
