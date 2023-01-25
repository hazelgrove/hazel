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

    our $VERSION = version->declare('v2022.12.30');
# end prelude
use Data::Printer;

use Gradescope::Color qw(color_print);

my %options;
GetOptions(\%options,
    'help|h|?',
    'fun|lambda|f|λ=s@',
    'timeout=s', # uses `timeout(1)` values
    'debug',
    ) or pod2usage(-exitval => 1, -verbose => 2);
pod2usage(-exitval => 0, -verbose => 2) if $options{help} || @ARGV < 0;

$options{fun} //= ['tee'];
$options{timeout} //= '30s';

my %submissions = do { # token ↦ submission
    local $/ = undef;
    %{JSON::from_json <STDIN>};
};
my %mapped;
for my $token (keys %submissions){
    carp "[debug] token = $token" if $options{debug};
    my $json_obj;
    try{
        my ($json_str) = capture_stdout { # use `timeout(1)` for portability
            IPC::Run::run [('timeout',
                    '--kill-after',
                    $options{timeout} =~ s/(\d+)/$1 + 5/er, # TODO: idk how long to wait
                    $options{timeout}),
                (@{$options{fun}},
                    $token)
            ], '<', \(JSON::to_json $submissions{$token});
            $? >> 8 && die;
        };
        $json_obj = JSON::from_json $json_str;
    }
    catch($e){
        carp "[warning] problem with $token: $e; skipping…";
    }
    $mapped{$token} = $json_obj;
}

color_print(JSON::to_json(\%mapped, {pretty => 1, canonical => 1}), 'JSON');

# PODNAME: map.pl
# ABSTRACT: Gradescope submission script component

__END__

=pod

=encoding UTF-8

=head1 NAME

map.pl - Gradescope submission script component

=head1 VERSION

version 2023.01.23

=head1 SYNOPSIS

map.pl [options]

map.pl [-f id.pl] --debug < submissions.json

=head1 DESCRIPTION

=head1 OPTIONS

=head2 help|h|?

=head2 debug

doesn't do much right now

=head2 fun|lambda|f|λ

command will be fed
a json object through stdin,
and passed the student's token (mostly for debugging purposes)

command should return another json object

command will be called with C<timeout>,
so the command should not timeout itself

=head3 bundled lambdas

=over 4

=item F<id.pl>

identity map

=item F<./hazel.pl>

TODO: the dune exec stuff that Yuchen wrote

=back

=head1 AUTHOR

hejohns <hejohns@umich.edu>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2023 by University of Michigan.

This is free software, licensed under:

  The (three-clause) BSD License

=cut
