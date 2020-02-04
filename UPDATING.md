# Instructions for Updating Various Components

## How to update Hazel to use a new version of ocaml

There are new releases of OCaml 2-4 times per year.

To update do the following:

- `opam update`

- `opam switch list-available`

- Choose the most recent version that does not contain a `+` character (e.g.,
  `4.09.0`)

- `opam switch create VERSION`, where `VERSION` is the most recent OCaml version
  that does not contain a `+` character (e.g., `4.09.0`).

- `make deps`

- `make repl`

- TODO: How to test REPL.

- `make release`

- Test in Firefox, Chrome, and Chromium using:

  - `chrome $(make echo-html)`,

  - `chromium $(make echo-html)`, and

  - `firefox $(make echo-html)`.

  TODO: How to test in browser.

- Test that the `reason-vscode` extension works with the OCaml version.

  To test this, ensure the `reason-vscode` extension is installed and then open
  a `*.re` file in a fresh instance of Visual Studio Code.  If the plugin does
  not support the OCaml version, after a few seconds a notification will appear
  in the lower right corner that says:

  ```Text
  Unsupported OCaml version: <version number>.
  Source: reason-vscode (Extension)
  ```

  If this notification does not appear, then the extension supports the OCaml
  version.

- Update the version number in `.travis.yml`

  - Check that travis builds correctly by going to
    <https://travis-ci.org/hazelgrove/hazel>.

  - Test that the travis build works by going to
    <http://hazel.org/build/dev/index.html>

- Update the version number in `.travis.yml` on the `master` branch.

  - Check that travis builds correctly by going to
    <https://travis-ci.org/hazelgrove/hazel>.

  - Test that the travis build works by going to
    <http://hazel.org/build/master/index.html>

- Update the version numbers in `README.md`

- Update the "Current Version" section in `UPDATING.md`

- Announce the reversion change on the `#hazel-dev` channel of the `hazelgrove`
  Slack by sending the following message.

      @channel
      The `reason-vscode` extension for VS code was just updated to support OCaml 4.08.1.  You can update to 4.08.1 by doing the following two things.

      - First, update your VS code extensions by pressing Ctrl+Shift+P to open
        the command pallet then typing or selecting the command
        `Extensions: Check for Extension Updates`.  If there is an update for
        the `reason-vscode` extension, click the `Update` button.

      - Second, update your OCaml installation by running the following:

        ```
        opam update
        opam switch create 4.08.1
        make deps
        ```

## Current version

The most recent version that we use is Ocaml 4.08.1.

- The `reason-vscode` plugin for VS Code does not (yet) support OCaml 4.09.
  See <https://github.com/jaredly/reason-language-server/pull/351>
  and <https://github.com/jaredly/reason-language-server/issues/309>.
