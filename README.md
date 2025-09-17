# Hazel ![Build Status](https://github.com/hazelgrove/hazel/actions/workflows/deploy_branches.yml/badge.svg) [![codecov](https://codecov.io/gh/hazelgrove/hazel/graph/badge.svg?token=lOGAH9Shqe)](https://codecov.io/gh/hazelgrove/hazel)

Hazel is a live functional programming environment rooted in the principles of
type theory. You can find the relevant papers and more motivation at [the Hazel
website](https://hazel.org/).

You can try Hazel online: the 
[dev](https://hazel.org/build/dev) branch is the main branch at the
moment. Every other branch that has been pushed to GitHub and successfully builds
can also be accessed at:

  `https://hazel.org/build/<branch_name>`

<!-- TODO: include some screenshots / animated GIFs once the UI stabilizes -->

## Building Hazel

### Short version

If you already have `ocaml` version 5.2.0, at least version 2.0 of `opam`, and a recent version of `npm` installed, and you have your GitHub SSH keys set up, you can build Hazel by running the following commands.

- `git clone git@github.com:hazelgrove/hazel.git`
- `cd hazel`
- `make deps`
- `make dev`

### Long Version

If the above pre-requisites don't apply, please follow the step-by-step installation instructions contained in [INSTALL.md](INSTALL.md).

### Build Warnings
You may see the following harmless warnings when building.

#### Ezjs_idb 
```
Warning 58 [no-cmx-file]: no cmx file was found in path for module Ezjs_idb, and its interface was not compiled with -opaque
```

This is due to an [upstream library issue](https://github.com/hazelgrove/hazel/issues/1104) and can be ignored.

#### joo_global_object
```
'joo_global_object' is being deprecated, please use `globalThis` instead
```

This is due various [upstream library issues](https://github.com/hazelgrove/hazel/issues/1796) and can be ignored. 

## Troubleshooting: dependency updates and build failures

- If Hazel fails to compile after pulling changes, first run:
  
  ```sh
  make deps
  ```

- If that doesn't work, your opam switch may be out of sync. Remove and recreate it, then reinstall deps:
  
  ```sh
  # Replace 5.2.0 with the name of your switch
  opam switch remove 5.2.0
  ```
  
  Recreate the switch following the instructions in [INSTALL.md](INSTALL.md) (see "Install OCaml" and "Install Library Dependencies"), then run `make deps` and rebuild.

## Running Hazel

### Locally
To run Hazel, you have to serve it on localhost or at some other non-file URL (you can't run it from a `file:///` URL due to browser restrictions e.g. on web workers.) 

If you have `python3` on your path, you can use the built in Python server via 
`make serve`, then navigate to `http://0.0.0.0:8000/` in your browser.

Alternatively, if you would live hot reloading, you can use `make hot` instead of `make serve`.

Otherwise, run `make echo-html-dir` which will echo the directory that needs 
to be served using some other server of your choice.

### Build Server
Every branch that has been pushed to GitHub and successfully builds
can also be accessed at the following URL (once the GitHub action is finished building and deploying it):

  `https://hazel.org/build/<branch_name>`

### Hazel CLI
Although Hazel is primarily designed as an integrated development environment, we do have a Hazel CLI executable called `hazel` available for basic tasks at the terminal. Further information is available in the [CLI README](src/CLI/README.md).

## Running Tests
You can run Hazel's unit tests with `make test`. Further details on testing are in the [test README](test/README.md).

## Contributing to Hazel
We welcome open source contributions to Hazel! If you are planning to contribute, please review the information in [CONTRIBUTING.md](CONTRIBUTING.md), which details how to run tests. You may also want to contact Cyrus Omar (comar@umich.edu) about your plans. We have a team Slack that might be helpful for you to join if you are planning to make non-trivial contributions and are happy to invite (and assist) external contributors!