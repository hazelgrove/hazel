# Gradescope-Utils

## NAME
EECS 490 Gradescope Utilities

## DESCRIPTION
Collection of scripts for gradescope stuff

Each file should contain its own documentation, which can be displayed by running `--help`

Originally a port of
<https://github.com/eecs490/Assignment-8-Gradescope>
during W22

## GETTING STARTED

### preface
First, to address a common concern:
Yes, most of the scripts are written in Perl.
But Gradescope-Utils (hereafter GU) does not presuppose Perl knowledge
any more than most scripts out there presuppose knowledge of their implementation details.
The extensional behavior of each script should be clear enough that--
barring bonafide bugs--
you should never *need* to read the source.
(You wouldn't notice if `cat` or `grep` were replaced with a Perl or Haskell implementation.)

Second, I realize this all seems rather complicated for what seems like a simple task.
But GU arose from a need; That is, GU *is* designed for a simple task.
Most scripts are half a terminal--
the job is just split over so many files so each step is well defined,
and so you can see how the data looks at each step.
Moreover, the modularity allows you to plug in your own scripts as the need arises.
### installing
You don't *need* to install these scripts (just `git clone` and relative path everything),
but there is a convenience wrapper.

The two commands are
`make install`
or `make install-lite`.
This installs a wrapper `gu` to `~/.local/bin`.
See OVERVIEW.

`make install` builds everything, which requires **a lot** of dependencies,
so you probably want to `make install-lite` which uses the included tarball.

Although you may still need to install some cpan modules, that many of the perl scripts use:
- `cpan Want`
- `cpan strictures`
- `cpan Carp::Assert`
- `cpan File::Slurp`
- `cpan IO::Prompter`
- `cpan Capture::Prompter`
- `cpan IPC::Run`
- or whenever you get the "you may need to install the FOO::BAR module" message

`gu` itself doesn't strictly need these,
but so many of the other scripts do,
you'll have to install them.

## OVERVIEW
### `gu` wrapper
- `gu --help` brings up this README
- `gu upload.pl ARGS` calls `bin/upload.pl ARGS`
- etc
(`~/.local/bin` should already be in `$PATH`, but you may need to manually add it)

#### examples
- `gu map.pl -f gu -f singletonkv2scalar.pl < submissions.json`

### bin
"types":

- **token2uniqname** is a json hash from (eg learn ocaml) tokens to uniqnames
- **submissions** is a json hash of student data, keyed by token

#### the main scripts, in approximate pipeline order:
##### join.pl : **zip** → [**token2uniqname**, **submissions**]
- stdin: n/a
- stdout: json pair of (**token2uniqname**, **submissions**)
- args: a Gradescope submissions export **zip**, and hooks to configure what ends up in **submissions**

##### split.pl : **token2uniqname** → **csv** → **submissions**
- stdin: **token2uniqname**
- stdout: **submissions**
- args: filepath to a **csv** (eg a sqlite dump),
and hooks to configure what ends up in **submissions**

comes with a wrapper to run each student in parallel, [`parallel.rb`](#parallelrb)

##### map.pl : **json hash** → **json hash**
- stdin: **json hash**
- stdout: **json hash** (with same keys)
- args: λ to run on each value

##### upload.pl : [**token2uniqname**, **submissions**] → ()
- stdin: json pair of (**token2uniqname**, **submissions**)
- stdout: debug messages
- args: Gradescope class and assignment ids

#### helper utilities/λs
see `gu --list`

### lib

## BUGS
Please raise issues on the [github](https://github.com/eecs490/gradescope-utils)
or email [hejohns@umich.edu](mailto:hejohns@umich.edu)
## SEE ALSO
