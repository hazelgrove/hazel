# Hazel TUI

A terminal UI for the Hazel structured editor, sharing the core editing
pipeline with the web app: the same `Action.t` / `Perform` / `Measured`
machinery from `haz3lcore`, and the same key→action map
(`Web.Keyboard.handle_key_event`).

## Running

```sh
./hazel-tui file.haz     # edit a file (created on first save)
./hazel-tui              # scratch buffer
make tui                 # just build
```

Like the Hazel CLI, the TUI is a js_of_ocaml executable run under node
(`src/CLI/polyfill.js` stubs the browser globals the `web` library touches
at init). It is not a native binary: the core libraries currently require
a JS runtime (`Unicode.re` uses `Intl.Segmenter`, `StringUtil.re` uses JS
regexp, `util` links `bonsai.web`). If the core is ever de-webbed, the
`Frame.t` renderer interface is the seam where a native notty backend
would slot in.

## Key bindings

Editor keys go through the same keymap as the web app (PC keymap):

| Keys | Action |
| --- | --- |
| printable chars | insert |
| arrows / Home / End | move (Ctrl+arrow: by token) |
| Shift+arrows / Shift+Home/End | select |
| Ctrl+A | select all |
| Ctrl+D | select current term |
| Backspace / Delete | destruct |
| Enter | linebreak |
| Tab | accept completion / put down backpack / next hole |
| Shift+Tab | previous hole |
| Escape | unselect |
| paste (bracketed) | `Paste` as a single action |

TUI-level bindings (handled before the editor keymap):

| Keys | Action |
| --- | --- |
| Ctrl+S | save (web's Ctrl+S PrettyPrint moved to Alt+P) |
| Ctrl+Z / Ctrl+Y | undo / redo (terminals can't see Ctrl+Shift+Z) |
| Ctrl+R | toggle result pane |
| PgUp / PgDn | page caret up/down |
| Ctrl+Q / Ctrl+C | quit (asks to confirm if unsaved) |
| Alt+P | pretty-print |

## Layout

- Editor with line-number gutter; viewport follows the caret.
- Result pane: live evaluation of the buffer (200 ms debounce,
  100k-step limit since evaluation is synchronous here).
- Status bar: file + dirty flag, caret position, type at cursor
  (mini cursor inspector), error/warning counts.

## Architecture

```
stdin bytes ─ AnsiInput ─ Util.Key.t ─ Web.Keyboard / Keymap ─ App.apply
                                                                  │
   Editor.Update.update / CachedStatics.init / Editor.Update.calculate
                                                                  │
EditorView (port of web Code.re walk) ─ Frame (styled rows) ─ ANSI ─ stdout
```

- `NodeTerm.re` — node stdin/stdout bindings, raw mode, alt screen,
  crash-safe terminal restore.
- `AnsiInput.re` — pure escape-sequence parser (tested in
  `test/Test_TuiInput.re`).
- `Keymap.re` — TUI bindings layered over `Web.Keyboard`.
- `App.re` — model/update/render; mirrors `CodeWithStatics.Update.calculate`.
- `EditorView.re` — `CachedSyntax` → styled rows; same token
  classification as web `Code.re`, colors in `Theme.re`.
- `Replay.re` — headless scripted driver
  (`./hazel-tui --replay 'let x = 1 in x\r'`), powers
  `test/Test_TuiGolden.re`. `--keys-debug` echoes parsed key events.

## Known limitations

- Projectors render as blank space of their measured shape (folds show
  `⋱`); there is no projector UI.
- TyDi assist is off (`settings.assist = false`); no completion buffer.
- Emoji widths: Hazel's column accounting may disagree with some
  terminals' wcwidth for exotic graphemes; ASCII is exact.
- Single scratch editor only — no slides/exercises/settings yet.
