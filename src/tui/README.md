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
| Ctrl+T | toggle cursor inspector pane |
| PgUp / PgDn | page caret up/down |
| Ctrl+Q / Ctrl+C | quit (asks to confirm if unsaved) |
| Alt+P | pretty-print |

macOS note: Alt chords (Alt+F fold, Alt+P pretty-print) work either
way — with "Option as Meta" enabled in your terminal (Terminal.app:
Profile → Keyboard → Use Option as Meta key; iTerm2: Profile → Keys →
Option key sends Esc+), or without it (the TUI also recognizes the
composed characters ƒ/π/† that plain Option typing produces, like the
web keymap does).

Mouse (SGR tracking): click places the caret; double/triple-click
selects token/term (same cycle as the web); shift+click and drag
select; wheel scrolls the viewport without moving the caret (the view
re-attaches to the caret on the next keyboard action).

## Layout

- Editor with line-number gutter; viewport follows the caret.
- Errors/warnings undercurled in place; backpack (picked-up shards
  awaiting Tab put-down) floats next to the caret as a `⇧` chip.
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

Projectors with a terminal view (`TermProjector.re` registry) render
live content inside their measured cell region and respond to clicks:
Fold (`⋱`, click to unfold), Checkbox (`✓`/`✗`, click to toggle),
Slider (`[====----]`, click to set), Statics (type shown offside after
the line). Other kinds fall back to blank space. The path to sharing
projector logic/views with the web properly is planned in
`docs/projector-backend-split.md`.

## Known limitations

- Projectors without a terminal view render as blank space of their
  measured shape; Block-shaped (multi-row) terminal views are not yet
  supported (inline + offside only).
- TyDi assist is off (`settings.assist = false`); no completion buffer.
- Emoji widths: Hazel's column accounting may disagree with some
  terminals' wcwidth for exotic graphemes; ASCII is exact.
- Single scratch editor only — no slides/exercises/settings yet.
