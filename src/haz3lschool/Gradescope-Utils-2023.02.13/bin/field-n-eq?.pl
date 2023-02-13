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
        ); # https://stackoverflow.com/a/46550384

# turn on features
    use builtin qw(true false is_bool reftype);
    no warnings 'experimental::builtin';
    use feature 'try';
    no warnings 'experimental::try';

    our $VERSION = version->declare('v2022.12.27');
# end prelude

my ($column_index, $token) = @ARGV;
assert(defined($column_index));
assert(defined($token));
my $row = do {
    local $/ = undef;
    JSON::from_json <STDIN>;
};
my @row = @$row;
if($row[$column_index] eq $token){
    exit 0;
}
else{
    exit 1;
}

# PODNAME: field-n-eq?.pl
# ABSTRACT: Gradescope submission script F<split.pl> lambda

__END__

=pod

=encoding UTF-8

=head1 NAME

field-n-eq?.pl - Gradescope submission script F<split.pl> lambda

=head1 VERSION

version 2023.02.13

=head1 SYNOPSIS

field-n-eq?.pl I<column_index> I<token>

field-n-eq?.pl 0 token < csv_row.json

=head1 DESCRIPTION

=head1 AUTHOR

hejohns <hejohns@umich.edu>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2023 by University of Michigan.

This is free software, licensed under:

  The (three-clause) BSD License

=cut
