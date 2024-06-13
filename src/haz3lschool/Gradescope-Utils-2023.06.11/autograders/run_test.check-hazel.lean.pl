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
use File::Slurp;
# use local modules
    use lib (
        File::Spec->catdir(dirname(abs_path($0)), 'lib'),
        ); # https://stackoverflow.com/a/46550384

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

my @files = glob('/autograder/submission/*.json');
should(@files, 1);
my $submission_path = $files[0];
my $submission  = JSON::from_json File::Slurp::read_file($submission_path);
should(reftype $submission, 'HASH');
assert(defined $submission->{school});
my $score = 100;
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
    confess "[error] Hazel code upload sanity check FAILED";
}
$output{'stdout_visibility'} = 'visible'; # we shouldn't need to hide any output from this script
$output{'output'} = <<~"EOF"
    Upload appears to be a Hazel json export
    *No other checks performed*
    Hazel code upload sanity check PASSED
EOF
;
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
