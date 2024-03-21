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
    use IPC::Run;
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

    our $VERSION = version->declare('v2023.02.14');
# end prelude
use Gradescope::Color qw(color_print);

my %options;
GetOptions(\%options,
    'help|h|?',
    ) or pod2usage(-exitval => 1, -verbose => 2);
pod2usage(-exitval => 0, -verbose => 2) if $options{help} || @ARGV < 0;

my ($regex_string) = @ARGV;
my $regex = qr/$regex_string/;
my $in = do {
    local $/ = undef;
    JSON::from_json <STDIN>;
};
my %in = %{$in};
my @good_keys = grep {m/$regex/} keys %in;
my %out = %in{@good_keys};
color_print(JSON::to_json(\%out, {pretty => 1, canonical => 1}), 'JSON');

# PODNAME: grep.pl
# ABSTRACT: Gradescope submission script lambda

__END__

=pod

=encoding UTF-8

=head1 NAME

grep.pl - Gradescope submission script lambda

=head1 VERSION

version 2023.06.11

=head1 SYNOPSIS

grep.pl : json hash -> json hash

grep.pl regex

=head1 DESCRIPTION

(on the keys)

=head1 AUTHOR

hejohns <hejohns@umich.edu>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2023 by University of Michigan.

This is free software, licensed under:

  The (three-clause) BSD License

=cut
