# Contributing to Hazel

## ReasonML / OCaml

Hazel is written in [ReasonML](https://reasonml.github.io), which is a syntactic sugar atop OCaml.

This link lets you type OCaml and see what the corresponding ReasonML syntax is:
<https://reasonml.github.io/en/try>. You can also convert between OCaml and ReasonML syntax at the terminal using
`refmt` at the terminal. See `refmt --help` for the details.

## Editor Setup
### Visual Studio Code

Most of our team uses Visual Studio Code (VS Code) to write code. 
If you use VS Code, here are a few extensions that might be helpful.

- This extension provides full support for editing OCaml and ReasonML source code and relevant tools:

  - [ocaml-platform](https://github.com/ocamllabs/vscode-ocaml-platform)

- Due to Reason's poor parse errors, unbalanced parentheses can be difficult
  to find. The following extensions help with that.

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

### NeoVim

If you enjoy your Vim setup, the following may help you set up your Reason IDE in NeoVim. If you use vi/vim/gvim, I recommend you to switch to NeoVim since it has a better support for multi-threading, and thus is less likely to block you when you are programming.

To set up the LSP (Language Server Protocol), you need to set up your Language Client for Neovim and Language Server for ocaml.
- [ocaml-language-server](https://www.npmjs.com/package/ocaml-language-server)
- [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)

After installing the previous two, you may want to copy the following to your neovim config file. 
(assuming `npm` has `ocaml-language-server` installed under `/usr/bin`)
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

### Other Editors
If you use another editor (Emacs??) and want to add your setup suggestions here, please submit a PR!

## Build System Details

Hazel is compiled to Javascript for the web browser via the `js_of_ocaml` compiler.

Though `make` targets are provided as a convenience, they mostly translate to `dune` commands.

Invoking `make` by itself is equivalent to invoking `make dev`. 

The `make dev` and `make release` commands do three things:

1. Auto-format Reason source files using `refmt`.
2. Generate some internal parsers using `menhir`.
3. Compile the Reason code + parsers to OCaml bytecode using the OCaml compiler.
4. Compile the OCaml bytecode to JavaScript
   (`_build/default/src/hazelweb/www/hazel.js`) using `js_of_ocaml`.


## Debugging

### Printing
You can print to the browser console using the standard `print_endline` function. This is probably the easiest method to debug right now.

Most datatypes in the codebase have something like `[@deriving (show({with_path: false}), sexp, yojson)]` on them. This generates
helper functions for printing and serializing this data (as strings (show), S-expressions, and JSON, respectively). 

For a type named `t`, the `show` function will be named `show`. Otherwise,
for a type named something else like `q`, it will be `show_q`.

If you are working with an anonymous type, e.g. a tuple or list type, you can locally derive a show function, e.g. in the following example we derive a show function for lists of expressions:

```reason
[%derive.show: [Exp.t]]([e1, e2, e3])
```

### Source Maps
Source maps for `js_of_ocaml` are generated when making locally with the dev profile (`make`). This is configured using the env stanzas present in the `dune` files for each top-level directory.

Since source maps are generated, browser developer tools should show reason code in the debugger and source tree. Stack traces should also include reason line numbers. This isn't always perfect.

### Debug Mode
If Hazel is hanging on load or when you perform certain actions, you can load into Debug Mode by appending `#debug` to the URL and reloading. From there, you have some buttons that will change settings or reset local storage. Refresh without the `#debug` flag and hopefully you can resolve the situation from there.

### Manual Reset
If you want to do a fully clean reset and debug mode isn't working, you need to manually clear local storage by going into your browser development tools. In Chrome devtools, you do so via Application > Storage > Local Storage. You may also want to clear the IndexedDB, which logs edit actions.

## Testing
You can run all of the unit tests located in `test` by running `make test`.

Unit tests are written using the [Alcotest framework](https://github.com/mirage/alcotest).

See more documentation in the [test README](test/README.md)

### Test Coverage
Code coverage is provided by [bisect_ppx](https://github.com/aantron/bisect_ppx). To collect coverage statistics from tests run `make coverage`. After coverage statistics are generated, running `make generate-coverage-html` will generate a local webpage at `_coverage/index.html` that can be viewed to see line coverage per module.

## Continuous Integration

When you push your branch to the main `hazelgrove/hazel` repository, we 
have a GitHub Action setup (see `.github/workflows/deploy_branches.yml`) 
that will build that branch (in `release` mode) and deploy it to the URL 
`https://hazel.org/build/<branch name>`, assuming the build succeeds.

It usually takes about 2 minutes if the build environment cache hits, or 
20+ minutes if not. You can view the status of the build in the [Actions 
tab on Github](https://github.com/hazelgrove/hazel/actions).

Builds prior to July 2024 are archived at `https://hazel.org/build-pre-july2024/<branch name>`.

Note: If another archive needs to be performed, make sure to redeploy the following
branches manually since we refer to them in various public material (websites and
published papers):

  dev
  livelits
