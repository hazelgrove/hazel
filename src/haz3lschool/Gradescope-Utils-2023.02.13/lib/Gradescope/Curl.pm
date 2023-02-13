package Gradescope::Curl v2022.11.13 {
    use v5.36;
    use utf8;
    use strictures 2; # nice `use strict`, `use warnings` defaults
    use open qw(:utf8); # try to use Perl's internal Unicode encoding for everything
    BEGIN{$diagnostics::PRETTY = 1} # a bit noisy, but somewhat informative
    use diagnostics -verbose;

    # turn on features
        use builtin qw(true false is_bool reftype);
        no warnings 'experimental::builtin';
        use feature 'try';
        no warnings 'experimental::try';
    # end prelude
    use Carp;
    use Carp::Assert;
    use IO::Prompter;
    use JSON;

    use parent qw(Exporter);

    # default exports
    our @EXPORT = qw();
    # optional exports
    our @EXPORT_OK = qw(
        login
    );

    our $baseurl;

    sub import {
        # in the style of Getopt::Long
        # (I figured people would be familiar w/ this import style since
        # Getopt::Long well known)
        shift; # package
        my @syms;
        my @config;
        my $dest = \@syms;
        for (@_){
            if($_ eq ':config'){
                $dest = \@config;
            } else{
                @$dest = (@$dest, $_);
            }
        }
        Gradescope::Curl->export_to_level(1, @syms);
        my %config = @config;
        assert(!defined($baseurl));
        $baseurl = $config{baseurl};
        assert(defined($baseurl));
    }

    sub login {
        # hacked together from the python script and a lot of netcat (thanks 489)
        # aka the curl snippets took a lot of trial and error
        open my $tty, '<', '/dev/tty' or confess 'This needs to be run in an interactive shell!';
        my $email = IO::Prompter::prompt('Enter your email: ', -in => $tty);
        my $password = IO::Prompter::prompt('Enter your password: ', -in => $tty, -echo => '');
        my %response = %{JSON::from_json(`curl -s --data 'email=$email&password=$password' $baseurl/api/v1/user_session`)};
        carp '[warning] curl returned error code on gradescope auth' if $? >> 8;
        $response{token} // confess "[error] Your gradescope login credentials are probably wrong";
        carp "[debug] token_expiration_time: $response{token_expiration_time}";
        return $response{token};
    }

    true;
}

# ABSTRACT: Gradescope submission script component

__END__

=pod

=encoding UTF-8

=head1 NAME

Gradescope::Curl - Gradescope submission script component

=head1 VERSION

version 2023.02.13

=head1 DESCRIPTION

=head1 AUTHOR

hejohns <hejohns@umich.edu>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2023 by University of Michigan.

This is free software, licensed under:

  The (three-clause) BSD License

=cut
