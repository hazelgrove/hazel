# esy

`package.json` workflow for native development with Reason/OCaml

[![Build Status](https://dev.azure.com/esy-dev/esy/_apis/build/status/build)](https://dev.azure.com/esy-dev/esy/_build/latest?definitionId=1)

This README serves as a development documentation for esy. For user
documentation refer to [esy.sh][] documentation site.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Repository structure](#repository-structure)
- [Workflow](#workflow)
  - [Testing Locally](#testing-locally)
  - [Running Tests](#running-tests)
  - [Branches](#branches)
  - [Issues](#issues)
  - [Publishing Releases](#publishing-releases)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Repository structure

The following snippet lists esy repository structured (omitting irrelevant or
obvious items) with further explanations:

    ├── CHANGELOG.md
    ├── LICENSE
    ├── README.md
    │
    ├── Makefile
    │   Common tasks and workflows for esy development.
    │
    ├── bin/esy
    │   symlink (wrapper on Windows) for esy command, used for running tests
    │
    ├── bin/esyInstallRelease.js
    │   postinstall step for npm releases produced with `esy npm-release`
    │   command. This is a built JS file which is developed in a separate flow
    │   inside `esy-install-npm-release/` subdirectory (see below).
    │
    ├── docs
    │   esy end user documentation in markdown format.
    │
    ├── dune
    ├── dune-project
    │
    ├── esy
    │   This dune library implements sandbox builder - a routine which builds
    │   the enture dependency graph and provides other introspection APIs.
    │
    ├── esy/bin
    │   This dune executable implements "esy" command.
    │
    ├── esy-solve
    │   This dune library implements solver.
    │
    ├── esy-install
    │   This dune library implements installer.
    │
    ├── esy-build-package
    │   This dune library implements package builder. esy library uses this to
    │   build each package.
    │
    ├── esy-build-package/bin
    │   This dune executable implements "esy-build-package" command.
    │
    ├── esy-installer
    │   Implementation of installation procedure defined with *.install files.
    │   This re-implements opam-installer.
    │
    ├── esy-install-npm-release
    │   Sources for `bin/esyInstallRelease.js`.
    │
    ├── esy-command-expression
    │   Parser for #{...} syntax used in esy manifests.
    │
    ├── esy-shell-expansion
    │   A simple shell expansion.
    │
    ├── esy-yarn-lockfile
    │   Parser for a subset of yarn lockfile format.
    │
    ├── esy-lib
    │   A collection of utility modules shared between other libraries.
    │
    ├── site
    │   Sources for https://esy.sh
    │
    ├── esy.lock
    ├── package.json
    │
    ├── scripts
    │
    ├── test
    │   Unit tests.
    │
    ├── test-e2e-slow
    │   End-to-end test suite which takes a significiant amount of time.
    │   We execute it on CI by placing `@slowtest` token in commit messages.
    │
    └── test-e2e
        End-to-end test suite.

## Workflow

To make changes to `esy` and test them locally:

```
% git clone git://github.com/esy/esy.git
% cd esy
% make bootstrap
```

And then run newly built `esy` executable from anywhere like `PATH_TO_REPO/bin/esy`.

### Updating `bin/esyInstallRelease.js`

`bin/esyInstallRelease.js` is developed separately within the `esy-install-npm-release/` directory.

Run:

```
% make bin/esyInstallRelease.js
```

to update the `bin/esyInstallRelease.js` file with the latest changed, don't
forget to commit it.

### Running Tests

Run:

```
% make test
```

#### Windows

In cases e2e tests fail with `Host key verification failed.`, you might have to create ssh keys
in the cygwin shall and add them to your github profile.

1. Enter cygwin installed by esy (not the global one)

```sh
.\node_modules\esy-bash\re\_build\default\bin\EsyBash.exe bash
```

2. Generate ssh keys

```sh
ssh-keygen
```

3. Add the public key to you Github profile

4. Add the following to the bash rc of the cygwin instance

```sh
eval $(ssh-agent -s)
ssh-add ~/.ssh/id_rsa
```


### Branches

There are two branches:

- `master` — the active development, we cut new versions out of there regularly.
- `0.0.x` — maintainance branch for 0.0.x releases.
- `0.2.x` — maintainance branch for 0.2.x releases.
- `0.3.x` — maintainance branch for 0.3.x releases.

## Workflow for esy.sh

To make changes to [esy.sh][]:

1. Bootstrap site's dev environment:

  ```
  % make site-bootstrap
  ```

2. Run site locally:

  ```
  % make site-start
  ```

3. When you are happy with the changes:

  ```
  % make site-publish
  ```

## Issues

Issues are tracked at [esy/esy][].

## Publishing Releases

esy is released on npm.

Because esy is written in OCaml/Reason and compiled into a native executable we
need to acquire a set of prebuilt binaries for each supported platform (Windows,
macOS and Linux). We employ CI servers (thanks Azure) to build platform specific
releases.

The release workflow is the following:

1.  Ensure you are on `master` branch and assuming you want to release the
    version currently defined in `package.json` (see step 6.), run

    ```
    % make release-tag
    % git push && git push --tags
    ```

2.  Wait till CI finishes its task and release `@esy-nightly/esy` package.

    You can test it manually.

3.  Run

    ```
    % make release-prepare
    ```

    which downloads the nightly corresponding to the current commit working
    directory is at and "promotes" it to a release. It will create
    `_release/package` directory.

4.  Ensure release inside `_release/package` directory is ok.

    You can `cd _release/package && npm pack && npm install -g ./esy-*.tgz` to test how
    release installs and feels.

5.  Run

    ```
    % make release-publish
    ```

    to upload the release on npm.

    Use

    ```
    % make NPM_RELEASE_TAG=next release-publish
    ```

    to publish the release under `next` tag (so users won't get it automatically but
    only explicitly requested).

6.  Bump version in `package.json` to the next patch version.

    We expect the next version to be mostly a patch version. In case you
    want to release new minor or major version you need to bump it before the
    release.

[hello-ocaml]: https://github.com/esy-ocaml/hello-ocaml
[hello-reason]: https://github.com/esy-ocaml/hello-reason
[esy/esy]: https://github.com/esy/esy
[esy-ocaml/esy-opam]: https://github.com/esy-ocaml/esy-opam
[opam]: https://opam.ocaml.org
[npm]: https://npmjs.org
[reason]: https://reasonml.github.io
[ocaml]: https://ocaml.org
[dune]: http://dune.readthedocs.io
[ocamlbuild]: https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc
[pjc]: https://github.com/jordwalke/PackageJsonForCompilers
[esy.sh]: http://esy.sh
