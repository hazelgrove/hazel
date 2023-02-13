#!/usr/bin/env perl

use v5.34; # gradescope currently uses ubuntu 22.04
use utf8;
use strict;
use warnings FATAL => 'all';
use open qw(:utf8) ;
BEGIN{$diagnostics::PRETTY = 1}
use diagnostics -verbose;

use Carp;
use Carp::Assert;
use Pod::Usage;
use Text::CSV;
use JSON;

use Cwd qw(abs_path);
use File::Basename qw(dirname);
use File::Spec;
# use local modules
    use lib (
        File::Spec->catdir(dirname(abs_path($0)), 'lib'),
        ); # https://stackoverflow.com/a/46550384
use Gradescope::Translate;

BEGIN{
    if($^V lt v5.36){
        require Scalar::Util;
        Scalar::Util->import(qw(reftype));
    }
    else{
        require builtin;
        warnings->import('-experimental::builtin');
        builtin->import(qw(reftype));
    }
}

pod2usage(-exitval => 0, -verbose => 2) if @ARGV;

my $score;
my @tests;
for my $test (@tests){ # stub
    my %test_output;
    $test_output{name} = $test->{name};
    $test_output{score} = $test->{score};
    $test_output{max_score} = $test->{max_score};
    $score = [@$score, \%test_output];
}
$score = 100 if !defined($score);
my %output; # gradescope expects JSON test output
if(!defined reftype $score){
    say '[debug] Using top level (total) score grading';
    $output{score} = $score;
}
elsif(reftype $score eq 'ARRAY'){
    say '[debug] Using individual tests grading';
    $output{tests} = $score;
}
else{
    confess "[error] `perldoc $0` to see how 'map submission' is supposed to be used";
}
$output{'stdout_visibility'} = 'visible'; # we shouldn't need to hide any output from this script
#$output{'output'} = '';
# test output
open(my $output_fh,
    '>:utf8',
    '/autograder/results/results.json') or confess '[error] writing output failed!';
print $output_fh JSON::to_json(\%output);

=pod

=encoding utf8

=head1 NAME

gradescope autograder base

=cut
