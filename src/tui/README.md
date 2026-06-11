# Hazel TUI

A terminal UI for the Hazel structured editor, sharing the core editing
pipeline with the web app: the same `Action.t` / `Perform` / `Measured`
machinery from `haz3lcore`, and the same key→action map
(`Keyboard.handle_key_event`, now in haz3lcore).

## Running

```sh
./hazel-tui file.haz     # edit a file (created on first save)
./hazel-tui              # scratch buffer
make tui                 # just build
```

The TUI is a NATIVE executable (`src/tui/tui.exe`) built on
[notty](https://github.com/pqwy/notty) — no node or JS runtime
involved. The core stack (util/language/haz3lcore) was de-webbed on
this branch, so the editor pipeline (parsing, statics, evaluation,
probes) runs as ordinary OCaml. Notty owns the terminal lifecycle,
input parsing, and rendering; the app's views stay backend-agnostic
behind `Frame.t` (styled rows + cursor), which `NottyIO` interprets as
notty images.

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
- Result pane: live evaluation of the buffer (200 ms debounce),
  asynchronously in a forked worker process — long evaluations never
  block typing, and an edit kills the in-flight worker. The 100M-step
  limit only bounds background CPU on doomed programs.
- Status bar: file + dirty flag, caret position, type at cursor
  (mini cursor inspector), error/warning counts.

## Architecture

```
stdin bytes ─ AnsiInput ─ Util.Key.t ─ Keyboard (haz3lcore) / Keymap ─ App.apply
                                                                  │
   Editor.Update.update / CachedStatics.init / Editor.Update.calculate
                                                                  │
EditorView (port of web Code.re walk) ─ Frame (styled rows) ─ ANSI ─ stdout
```

- `NottyIO.re` / `NottyEvents.re` — the notty backend: frames render
  as notty images; notty's parsed key/mouse/paste events translate to
  the same `AnsiInput.event` type the rest of the app consumes.
- `AnsiInput.re` — pure escape-sequence parser; drives `--replay` key
  scripts and the golden tests (tested in `test/Test_TuiInput.re`).
- `Keymap.re` — TUI bindings layered over the shared `Keyboard` keymap
  (now in haz3lcore, used by both frontends).
- `App.re` — model/update/render; mirrors `CodeWithStatics.Update.calculate`.
- `EditorView.re` — `CachedSyntax` → styled rows; same token
  classification as web `Code.re`, colors in `Theme.re`.
- `Replay.re` — headless scripted driver
  (`./hazel-tui --replay 'let x = 1 in x\r'`), powers
  `test/Test_TuiGolden.re`. `--keys-debug` echoes parsed key events.

Probes work: `Ctrl+E` toggles a probe on the indicated term (saved as
`^^probe(...)` trigger syntax, interoperable with the web); evaluation
collects samples for it and up to 5 render offside after the line as
`≡ v1 ⫽ v2 ⫽ ...` (`∅` if never reached). Sample focus, step-into,
and pinning — the web's full probe UI — are not ported.

Projectors render live content inside their measured cell region via
the `TermProjector.re` registry and respond to clicks: Fold (`⋱`,
click to unfold), Checkbox (`✓`/`✗`, click to toggle), Slider and
SliderF (`[====----]`, click to set), Statics (type shown offside),
TextArea (string content in its block region; read-only until the
focus protocol exists), Table (box-drawing table of the parsed rows),
Probe (sample values offside), Card (mini card faces: `A♡`, red/black
pips on a white face). Kinds without a bespoke view (Livelit, Csv)
render their underlying syntax as a dim chip. The path to sharing
projector logic/views with the web properly is planned in
`docs/projector-backend-split.md`.

## Known limitations

- TextArea/Table render read-only; in-place projector editing needs
  the focus protocol (docs/projector-backend-split.md step 4).
- TyDi assist is off (`settings.assist = false`); no completion buffer.
- Emoji widths: Hazel's column accounting may disagree with some
  terminals' wcwidth for exotic graphemes; ASCII is exact.
- Error/warning marks render as colored underlines (notty's attribute
  model has no undercurl or dim; dim styling is emulated with gray).
- Single scratch editor only — no slides/exercises/settings yet.
