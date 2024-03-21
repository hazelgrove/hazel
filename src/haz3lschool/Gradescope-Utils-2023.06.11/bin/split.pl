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
    # `capture_stdout` for backticks w/o shell (escaping issues)
    use Capture::Tiny qw(:all);
    # for more complicated stuff
    # eg timeout, redirection
    use IPC::Run qw(run);
    use IPC::Cmd qw(can_run);
# option/arg handling
    use Getopt::Long qw(:config gnu_getopt auto_version); # auto_help not the greatest
    use Pod::Usage;
# use local modules
    use lib (
        dirname(abs_path($0)),
        abs_path(File::Spec->rel2abs('../lib/', dirname(abs_path($0)))),
        ); # https://stackoverflow.com/a/46550384

# turn on features
    use builtin qw(true false is_bool reftype);
    no warnings 'experimental::builtin';
    use feature 'try';
    no warnings 'experimental::try';

    our $VERSION = version->declare('v2023.04.30');
# end prelude
use Data::Printer;

use Gradescope::Translate;
use Gradescope::Color qw(color_print);

my %options;
GetOptions(\%options,
    'help|h|?',
    'delimiter|d=s',
    'keyheader|k=s@',
    'valueheader|v=s@',
    'tokenfilter|t|f|p=s@',
    'debug',
    ) or pod2usage(-exitval => 1, -verbose => 2);
pod2usage(-exitval => 0, -verbose => 2) if $options{help} || @ARGV < 1;

$options{delimiter} //= ':';
if(!defined $options{keyheader} || !defined $options{valueheader} || !defined $options{tokenfilter}){
    carp '[error] `-k`, `-v` required-- see `--help`';
    pod2usage(-exitval => 0, -verbose => 2);
}
$options{tokenfilter} //= ['true'];
$options{keyheader} = [$options{delimiter}, @{$options{keyheader}}];

my ($submissions) = @ARGV;
my $token2uniqname = do {
    local $/ = undef;
    JSON::from_json <STDIN>;
};
my %token2uniqname = %{$token2uniqname};
my %submissions;
for my $token (keys %token2uniqname){
    my %filtered = Gradescope::Translate::read_csv($submissions,
        $options{keyheader}, $options{valueheader},
        sub { # see Text::CSV for details
            my $pred = false;
            my @row = @{$_[1]}; # capture_stdout takes a code ref, so $_[1] inside is shadowed
            say STDERR "[debug] running tokenfilter cmd: @{$options{tokenfilter}} ${\(JSON::to_json $token)} ${\(JSON::to_json \@row)}" if $options{debug};
            capture_stdout {
                $pred = run [@{$options{tokenfilter}}, JSON::to_json $token], '<', \(JSON::to_json \@row);
            };
            say STDERR "[debug] returned: ${\($pred ? 'true' : 'false')}" if $options{debug};
            return $pred;
        },
    );
    delete @filtered{grep {!defined $filtered{$_}} keys %filtered};
    next if keys %filtered == 0; # some students may not have submissions
    $submissions{$token} = \%filtered;
}

color_print(JSON::to_json(\%submissions, {pretty => 1, canonical => 1}), 'JSON');

# PODNAME: split.pl
# ABSTRACT: Gradescope submission script component

__END__

=pod

=encoding UTF-8

=head1 NAME

split.pl - Gradescope submission script component

=head1 VERSION

version 2023.06.11

=head1 SYNOPSIS

split.pl : B<token2uniqname → csv → submissions>

split.pl [options] I<submissions_csv>

split.pl [-t ./field-n-eq?] [-k problem_id -v score] submissions.csv < token2uniqname.json

=head1 DESCRIPTION

splits up I<submissions_csv>
into a B<submissions> json hash

=head1 OPTIONS

=head2 help|h|?

=head2 delimiter|d

=head2 keyheader|k

=head2 valueheader|v

I<keyheader> and I<valueheader> will be passed to perl's C<Text::CSV>
to convert I<submissions_csv> to a key-value,
for each B<token2uniqname> token

this may require joining multiple csv columns for the key,
so a delimiter may be specified with C<-d> for joining multiple C<-k> headers

note that since I<tokenfilter> filters the csv per student,
there is no need to include the student's token as part of a multi-I<keyheader>

=head2 tokenfilter|t

command will be fed
a csv row as json through stdin,
and passed a student's token as a last argument

command is a predicate that should C<exit 0> iff the csv row should be used for the student with that token

=head2 debug

to see what C<-f> should be

=head1 AUTHOR

hejohns <hejohns@umich.edu>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2023 by University of Michigan.

This is free software, licensed under:

  The (three-clause) BSD License

=cut
