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
        ); # https://stackoverflow.com/a/46550384
 
# turn on features
    use builtin qw(true false is_bool reftype);
    no warnings 'experimental::builtin';
    use feature 'try';
    no warnings 'experimental::try';

    our $VERSION = version->declare('v2023.05.01');
# end prelude

my @builds = glob 'Gradescope-Utils-*';
@builds = grep {m/^Gradescope-Utils-([\d\.]+)(\.tar\.gz)?$/} @builds;
say STDERR '[debug] found versions:';
say for @builds;
my @versions = map {m/^Gradescope-Utils-([\d\.]+)(\.tar\.gz)?$/; version->parse($1)} @builds;
@versions = sort @versions;
my $latest_version = pop @versions;
should($latest_version->stringify, "$latest_version"); # I'm a bit nervous about how version objects are handled
say STDERR "[debug] using version: $latest_version";
# NOTE: we could use the perl equivalents for portability, but I'm assuming so much *nix anyways, there's no point
# (ie sorry windows users)
my $local_share = File::Spec->catdir($ENV{HOME}, '.local', 'share', 'gradescope-utils');
my $confirm_overwrite_install = sub{
    if(-e $local_share){
        my $confirm = IO::Prompter::prompt(
            "Confirm: update existing install at '$local_share'? (y/N)? ",
            -in => *STDIN
        );
        croak '[error] user cancelled' if $confirm ne 'y';
    }
    else{
        run ['mkdir', '-p', $local_share] or croak '[error] `mkdir` failed';
    }
};
@builds = grep {m/^Gradescope-Utils-$latest_version(\.tar\.gz)?$/} @builds;
# prefer the regular build dir over the tar 'd one, if it exists
if(grep {m/^Gradescope-Utils-$latest_version$/} @builds){
    say STDERR "[debug] using build at 'Gradescope-Utils-$latest_version/'";
    &$confirm_overwrite_install;
    run ['cp', '-rT', "Gradescope-Utils-$latest_version", $local_share] or croak '[error] `cp` failed';
}
elsif(grep {m/\.tar\.gz$/} @builds){
    say STDERR "[debug] using build at 'Gradescope-Utils-$latest_version.tar.gz'";
    &$confirm_overwrite_install;
    my $tmpdir = File::Temp->newdir();
    run ['tar', '-xf', "Gradescope-Utils-$latest_version.tar.gz", '-C', $tmpdir] or croak '[error] `tar` failed';
    run ['cp', '-rT', File::Spec->catdir($tmpdir, "Gradescope-Utils-$latest_version"), $local_share] or croak '[error] `cp` failed';
}
else{
    croak '[error] no suitable builds found';
}

my $local_bin = File::Spec->catdir($ENV{HOME}, '.local', 'bin');
if(!-e $local_bin){
    run ['mkdir', '-p', File::Spec->catdir($ENV{HOME}, '.local', 'bin')] or croak '[error] `mkdir` failed';
}
run ['ln', '-fs', File::Spec->catfile($local_share, 'bin', 'gradescope-utils.pl'), File::Spec->catfile($ENV{HOME}, '.local', 'bin', 'gu')] or croak '[error] `ln` failed';

=pod

=encoding utf8

=head1 NAME

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut
