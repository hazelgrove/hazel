#!/usr/bin/env perl

use v5.36;
use utf8;
use strictures 2; # nice `use strict`, `use warnings` defaults
use open qw(:utf8); # try to use Perl's internal Unicode encoding for everything
BEGIN{$diagnostics::PRETTY = 1} # a bit noisy, but somewhat informative
use diagnostics -verbose;

# Carp
    use Carp;
    use Carp::Assert;
# filepath functions
    use Cwd qw(abs_path);
    use File::Basename qw(basename dirname);
    use File::Spec;
# misc file utilities
    use File::Temp;
    use File::Slurp;
    use Text::CSV;
    use JSON;
    use YAML::XS;
# misc scripting IO utilities
    use IO::Prompter;
    # `capture_stdout` for backticks w/o shell
    use Capture::Tiny qw(:all);
# option/arg handling
    use Getopt::Long qw(:config gnu_getopt auto_version); # auto_help not the greatest
    use Pod::Usage;
# use local modules
    use lib (
        dirname(abs_path($0)),
        abs_path(File::Spec->rel2abs('../lib/', dirname(abs_path($0)))),
        ); # https://stackoverflow.com/a/46550384
 
# turn on features
    use builtin;
    no warnings 'experimental::builtin';
    use feature 'try';
    no warnings 'experimental::try';

    our $VERSION = version->declare('v2022.11.13');
# end prelude
use Gradescope::Translate;
use Gradescope::Curl qw(:config baseurl https://www.gradescope.com);

my %options;
GetOptions(\%options,
    'help|h|?',
    'filetype|f=s',
) or pod2usage(-exitval => 1, -verbose => 2);
pod2usage(-exitval => 0, -verbose => 2) if $options{help} || @ARGV < 2;

$options{filetype} //= 'csv';

$options{filetype} = ".$options{filetype}";

# from original python script:
#   You can get course and assignment IDs from the URL, e.g.:
#     https://www.gradescope.com/courses/1234/assignments/5678
#     course_id = 1234, assignment_id = 5678
my ($class_id, $assignment_id) = @ARGV;
my ($token2uniqname, $submissions) = do {
    local $/ = undef;
    @{JSON::from_json <STDIN>}
};
my %token2uniqname = %$token2uniqname;
my %submissions = %$submissions; # token ↦ submission

my $auth_token = Gradescope::Curl::login();
my $tmpdir = File::Temp->newdir();
carp "[debug] $tmpdir";
File::Slurp::write_file(File::Spec->catfile($tmpdir, "$_$options{filetype}"), JSON::to_json $submissions{$_}, {pretty => 1, canonical => 1}) for keys %submissions;
for my $t (keys %token2uniqname){
    my $f = File::Spec->catfile($tmpdir, "$t$options{filetype}");
    #say `curl -s -H 'access-token: $auth_token' -F 'owner_email=$token2uniqname{$t}\@umich.edu' -F 'files[]=\@$f' $Gradescope::Curl::baseurl/api/v1/courses/$class_id/assignments/$assignment_id/submissions`;
    system('curl', '-s', '-H', "access-token: $auth_token", '-F', "owner_email=$token2uniqname{$t}\@umich.edu", '-F', "files[]=\@$f", "$Gradescope::Curl::baseurl/api/v1/courses/$class_id/assignments/$assignment_id/submissions");
    say STDERR "";
    carp "[warning] curl return code on $t: ${\($? >> 8)}" if $? >> 8;
    carp "[warning] does $f actually exist?" if $? >> 8;
}

# PODNAME: upload.pl
# ABSTRACT: Gradescope submission script component

__END__

=pod

=encoding UTF-8

=head1 NAME

upload.pl - Gradescope submission script component

=head1 VERSION

version 2023.02.13

=head1 SYNOPSIS

upload.pl : [token2uniqname, submissions] (json array) -> ()

upload.pl class_id assignment_id

upload.pl 1234 5678

=head1 DESCRIPTION

I<submissions> := path to directory of submissions for upload created by ./split.pl

I<class_id> := gradescope class id

I<assignment_id> := gradescope assignment id

=head1 OPTIONS

=head2 filetype|f

eg C<-f csv>

=head1 AUTHOR

hejohns <hejohns@umich.edu>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2023 by University of Michigan.

This is free software, licensed under:

  The (three-clause) BSD License

=cut
