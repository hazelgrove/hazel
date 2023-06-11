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
my $submission = JSON::from_json File::Slurp::read_file($submission_path);
# upload is a json hash of question -> answer
should(reftype $submission, 'HASH');
my $score = [{
        name => 'q1',
        score => ($submission->{0}->{answer} eq '0') ? 5.83333 : 0,
        max_score => 5.83333,
        output => $submission->{0}->{free_response},
    }, {
        name => 'q2',
        score => ($submission->{1}->{answer} eq '3') ? 5.83333 : 0,
        max_score => 5.83333,
        output => $submission->{1}->{free_response},
    }, {
        name => 'q3',
        score => ($submission->{2}->{answer} eq '1') ? 5.83333 : 0,
        max_score => 5.83333,
        output => $submission->{2}->{free_response},
    }, {
        name => 'q4',
        score => ($submission->{3}->{answer} eq '2') ? 5.83333 : 0,
        max_score => 5.83333,
        output => $submission->{3}->{free_response},
    }, {
        name => 'q5',
        score => ($submission->{4}->{answer} eq '3') ? 5.83333 : 0,
        max_score => 5.83333,
        output => $submission->{4}->{free_response},
    }, {
        name => 'q6',
        score => ($submission->{5}->{answer} eq '0') ? 5.83333 : 0,
        max_score => 5.83333,
        output => $submission->{5}->{free_response},
    }, {
        name => 'feedback',
        score => 5,
        max_score => 5,
    }];
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
    confess "[error] rustviz quiz autograder FAILED";
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
