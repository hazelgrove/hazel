# Hazel [![Build status: dev](https://img.shields.io/travis/hazelgrove/hazel/dev?label=build:%20dev)](https://travis-ci.org/hazelgrove/hazel) [![Build status: master](https://img.shields.io/travis/hazelgrove/hazel/master?label=build:%20master)](https://travis-ci.org/hazelgrove/hazel)

[![Hazel Mascot](src/hazelweb/www/imgs/hazel-logo.png)](https://hazel.org)

Hazel is a live functional-programming environment rooted in the principles of
type theory. You can find the relevant papers and more motivation at [the Hazel
website](http://hazel.org/).

You can try Hazel online with either the
[master](https://hazel.org/build/master/index.html) or
[dev](https://hazel.org/build/dev/index.html) version. Note that the master
branch is updated infrequently and is currently almost two years behind!

<!-- TODO: include some screenshots / animated GIFs once the UI stabilizes -->

## Building and Running Hazel

### Short version

If you already have `ocaml` version 4.08.1 and least version 2.0 of `opam`
installed, you can build Hazel by running the following commands.

- `git clone git@github.com:hazelgrove/hazel.git`
- `cd hazel`
- `make deps`
- `make dev`

To run Hazel, run the command `make echo-html`, which will print a filename.
Then use your preferred browser to open that file. For convenience, the 
following make targets open the corresponding browser or invoke the
corresponding command immediately (see INSTALL.md):

 - `make firefox`
 - `make chrome`
 - `make chrome-browser`
 - `make chromium`
 - `make chromium-browser`
 - `make win-chrome`
 - `make win-firefox`
 - `make xdg-open`
 - `make open`

### Long Version

If you are unfamiliar with `ocaml` or `opam`, do not have them installed, or
just get stuck, we recommend you follow the step-by-step installation
instructions contained in [INSTALL.md](INSTALL.md).

## Contributing

### From OCaml to ReasonML

This link lets you type OCaml and see what the corresponding ReasonML syntax is:
<https://reasonml.github.io/en/try>.

This is useful if you are trying to figure out the ReasonML syntax for something
that you know the OCaml syntax for.

<<<<<<< HEAD
### Suggested Extensions for VS Code
=======
### Results
You can now open `_build/default/src/www/index.html` in a browser to see Hazel in action.
>>>>>>> origin/master

Most of our team uses VisualStudio Code to write code.  If you use VS Code, here
are a few extensions that might be helpful.

- These extensions provide support for editing ReasonML and Dune source code:

  - [reason-vscode](https://marketplace.visualstudio.com/items?itemName=jaredly.reason-vscode)
  - [Dune](https://marketplace.visualstudio.com/items?itemName=maelvalais.dune)

- Due to Reason's poor parse errors, unbalanced parentheses can be difficult
  to find.  The following extensions help with that.

  - [Bracket Pair Colorizer 2](https://marketplace.visualstudio.com/items?itemName=coenraads.bracket-pair-colorizer-2)
  - [Indenticator](https://marketplace.visualstudio.com/items?itemName=sirtori.indenticator)
  - [indent-rainbow](https://marketplace.visualstudio.com/items?itemName=oderwat.indent-rainbow)

### Build System Details

Hazel is implemented in Reason (a dialect of OCaml) and is compiled to
Javascript for the web browser via the `js_of_ocaml` compiler.

Though `make` targets are provided as a convenience, they mostly translate to
`dune` commands.

Invoking `make` by itself is equivalent to invoking `make dev`. With these
commands we pass additional flags to `js_of_ocaml` that cause the insertion of
comments that map locations in the generated JS to locations in the source
files. This is useful for debugging purposes.

`make dev` also auto-formats Reason source files using `refmt` (this is what the
`@src/fmt` alias is for). This ensures code from all contributors follows the
same style.

The `make dev` and `make release` commands do three things:

1. Generate some internal parsers using `menhir`.
2. Compile the Reason code to OCaml bytecode using the OCaml compiler.
3. Compile the OCaml bytecode to JavaScript
   (`_build/default/src/hazelweb/www/hazel.js`) using `js_of_ocaml`.
   
### Debugging

You can print to the browser console using the standard `print_endline` function. This is probably the easiest method right now.

`js_of_ocaml` does support source maps and has some other flags that might be useful. If you experiment with those and get them to work, please update this README with some notes.
