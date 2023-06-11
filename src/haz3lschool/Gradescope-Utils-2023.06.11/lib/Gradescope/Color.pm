package Gradescope::Color v2023.02.14 {
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
    use IPC::Cmd qw(can_run);
    use IPC::Run qw(run);

    use parent qw(Exporter);

    # default exports
    our @EXPORT = qw();
    # optional exports
    our @EXPORT_OK = qw(
        color_print
    );

    our $has_colorizer = defined(can_run('bat'));
    carp '[suggestion] get `bat` for colorized output' if !$has_colorizer;

    sub color_print {
        my ($str, $language) = @_;
        if($has_colorizer && !defined($ENV{GU_NO_PAGER})){
            run ['bat', '-l', $language], '<', \$str;
        }
        else{
            print $str;
        }
    }

    true;
}

# ABSTRACT: Gradescope submission script component

__END__

=pod

=encoding UTF-8

=head1 NAME

Gradescope::Color - Gradescope submission script component

=head1 VERSION

version 2023.06.11

=head1 DESCRIPTION

=head1 AUTHOR

hejohns <hejohns@umich.edu>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2023 by University of Michigan.

This is free software, licensed under:

  The (three-clause) BSD License

=cut
