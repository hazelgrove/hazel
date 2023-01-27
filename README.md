# Hazel ![Build Status](https://github.com/hazelgrove/hazel/actions/workflows/deploy_branches.yml/badge.svg)

[![Hazel Mascot](src/hazelweb/www/imgs/hazel-logo.png)](https://hazel.org)

Hazel is a live functional-programming environment rooted in the principles of
type theory. You can find the relevant papers and more motivation at [the Hazel
website](https://hazel.org/).

You can try Hazel online with either the
[trunk](https://hazel.org/build/trunk/index.html) or
[dev](https://hazel.org/build/dev/index.html) version. Note that the trunk
branch is updated infrequently and is currently almost two years behind!

<!-- TODO: include some screenshots / animated GIFs once the UI stabilizes -->

## Building and Running Hazel

### Short version

If you already have `ocaml` version 4.14.0 and least version 2.0 of `opam`
installed, you can build Hazel by running the following commands.

- `git clone git@github.com:hazelgrove/hazel.git`
- `cd hazel`
- `make deps`
- `make dev`

To view Hazel, you have to serve it, on localhost for development (you can't
run it from a `file:///` URL due to browser restrictions on web workers.) 

If you have `python3` on your path, you can use the Python server via 
`make serve`, then navigate to `http://0.0.0.0:8000/` in your browser.

Otherwise, run `make echo-html-dir` for the directory that needs to be served
using the server of your choice.

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

### Suggested Extensions for VS Code

Most of our team uses VisualStudio Code to write code.  If you use VS Code, here
are a few extensions that might be helpful.

- This extension provides full support for editing ReasonML source code and
  relevant tools:

  - [ocaml-platform](https://github.com/ocamllabs/vscode-ocaml-platform)

- Due to Reason's poor parse errors, unbalanced parentheses can be difficult
  to find.  The following extensions help with that.

  - [Bracket Pair Colorizer 2](https://marketplace.visualstudio.com/items?itemName=coenraads.bracket-pair-colorizer-2)
  - [Indenticator](https://marketplace.visualstudio.com/items?itemName=sirtori.indenticator)
  - [indent-rainbow](https://marketplace.visualstudio.com/items?itemName=oderwat.indent-rainbow)

In addition to these extensions, enabling the breadcrumbs bar can make
navigating a large code base easier. There are multiple ways to make the
breadcrumbs bar visible:

- Click **View** / **Show Breadcrumbs** from the menu bar.
- Press `Ctrl+Shift+P` (macOS: `Cmd+Shift+P`), start typing `breadcrumbs`, and
  select `View: Toggle Breadcrumbs` from the dropdown menu to toggle breadcrumbs
  on and off.
- Press `Ctrl+Shift+.` to start breadcrumbs navigation.

### Suggested Setup for NeoVim

If you enjoy your Vim binding and Vim setup, the following may help you set up your Reason IDE in NeoVim.

If you use vim, I recommend you to switch to NeoVim since it has a better support for multi-thread,
and thus less likely to block you when you are programming.

To set up the LSP(Language Server Protocal), you need to set up your Language Client for Neovim and Language Server for ocaml.
- [ocaml-language-server](https://www.npmjs.com/package/ocaml-language-server)
- [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)

After installing the previous two, you may want to copy the following to your neovim config file. 
(assuming `npm` have ocaml-language-server installed under `/usr/bin`)
```
let g:LanguageClient_serverCommands = {
    \ 'ocaml': ['/usr/bin/ocaml-language-server', '--stdio'],
    \ 'reason': ['/usr/bin/ocaml-language-server', '--stdio']
    \ }
" LanguageClient-neovim
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient#textDocument_references()<CR>
nnoremap <silent> gf :call LanguageClient#textDocument_formatting()<cr>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
```

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

For a smoother dev experience, use `make watch` to automatically watch 
for file changes. This will require installing `fswatch` (see INSTALL.md).
You can also run `make watch-release` to continuously build the release
build (takes longer per build).
   
### Debugging

#### Printing
You can print to the browser console using the standard `print_endline` function. This is probably the easiest method right now.

#### Source Maps
`js_of_ocaml` does support source maps and has some other flags that might be useful. If you experiment with those and get them to work, please update this README with some notes.

#### Debug Mode
If Hazel is hanging on load or when you perform certain actions, you can load into Debug Mode by appending `#debug` to the URL and reloading. From there, you have some buttons that will change settings or reset local storage. Refresh without the `#debug` flag and hopefully you can resolve the situation from there.

### Testing

You can run all of the unit tests located in `src/hazelcore/test` by running `make test`.

Unit tests are written using [ppx_expect](https://github.com/janestreet/ppx_expect/tree/master/example) and [ppx_inline_tests](https://github.com/janestreet/ppx_inline_test/tree/master/example). If you would like to adjust your expect tests to assert for the output that was last printed, run `make fix-test-answers`.

If the inline test runner causes problems for you, you can likely resolve the issue by running `opam update` then `opam upgrade`.

### Continuous Integration

When you push your branch to the main `hazelgrove/hazel` repository, we 
have a GitHub Action setup (see `.github/workflows/deploy_branches.yml`) 
that will build that branch (in `release` mode) and deploy it to the URL 
`https://hazel.org/build/<branch name>`, assuming the build succeeds.

It usually takes about 2 minutes if the build environment cache hits, or 
20+ minutes if not. You can view the status of the build in the [Actions 
tab on Github](https://github.com/hazelgrove/hazel/actions).
