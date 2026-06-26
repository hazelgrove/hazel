open Haz3ltui;

/* Translates notty's parsed terminal events into the TUI's input
   events (AnsiInput.event), so Keymap/App stay backend-agnostic.
   Stateful only for bracketed paste, which notty delivers as
   `Paste(`Start) .. key events .. `Paste(`End). */

type state = {paste: option(Stdlib.Buffer.t)};

let init: state = {paste: None};

let utf8_of_uchar = (u: Uchar.t): string => {
  let b = Stdlib.Buffer.create(4);
  Stdlib.Buffer.add_utf_8_uchar(b, u);
  Stdlib.Buffer.contents(b);
};

let has = (m, mods) => List.mem(m, mods);

let mk = (~mods: Notty.Unescape.mods=[], name: string): AnsiInput.event =>
  AnsiInput.Editor(
    AnsiInput.mk(
      ~shift=has(`Shift, mods),
      ~ctrl=has(`Ctrl, mods),
      ~alt=has(`Meta, mods),
      name,
    ),
  );

/* Ctrl chords reserved by the TUI (same set AnsiInput.ctrl_event
   intercepts when parsing raw bytes) */
let reserved_ctrl = (c: char): option(AnsiInput.event) =>
  switch (Char.lowercase_ascii(c)) {
  | 'c'
  | 'q' => Some(Tui(Quit))
  | 's' => Some(Tui(Save))
  | 'z' => Some(Tui(Undo))
  | 'y' => Some(Tui(Redo))
  | 'r' => Some(Tui(ToggleResultPane))
  | 't' => Some(Tui(ToggleInspector))
  | 'p' => Some(Tui(ProjectorPanel))
  | _ => None
  };

let key_events = ((k, mods): Notty.Unescape.key): list(AnsiInput.event) =>
  switch (k) {
  | `ASCII(c) when has(`Ctrl, mods) =>
    switch (reserved_ctrl(c)) {
    | Some(ev) => [ev]
    | None => [mk(~mods, String.make(1, Char.lowercase_ascii(c)))]
    }
  | `ASCII(c) => [mk(~mods, String.make(1, c))]
  | `Uchar(u) => [mk(~mods, utf8_of_uchar(u))]
  | `Enter => [mk("Enter")]
  | `Tab => has(`Shift, mods) ? [Tui(ShiftTab)] : [Tui(TabKey)]
  | `Backspace => [mk("Backspace")]
  | `Delete => [mk(~mods, "Delete")]
  | `Escape => [mk("Escape")]
  | `Home => [mk(~mods, "Home")]
  | `End => [mk(~mods, "End")]
  | `Arrow(`Up) => [mk(~mods, "ArrowUp")]
  | `Arrow(`Down) => [mk(~mods, "ArrowDown")]
  | `Arrow(`Left) => [mk(~mods, "ArrowLeft")]
  | `Arrow(`Right) => [mk(~mods, "ArrowRight")]
  | `Page(`Up) => [Tui(PageUp)]
  | `Page(`Down) => [Tui(PageDown)]
  | `Insert
  | `Function(_) => []
  };

let mouse_events =
    ((m, (x, y), mods): Notty.Unescape.mouse): list(AnsiInput.event) => {
  let pt: Util.Point.t = {
    row: y,
    col: x,
  };
  switch (m) {
  | `Press(`Left) => [Mouse(Press(pt, has(`Shift, mods)))]
  | `Press(`Scroll(`Up)) => [Mouse(Wheel(-3))]
  | `Press(`Scroll(`Down)) => [Mouse(Wheel(3))]
  | `Press(`Middle | `Right) => []
  | `Drag => [Mouse(Drag(pt))]
  | `Release => [Mouse(Release)]
  };
};

/* Text content of a key event while inside a bracketed paste */
let paste_text = ((k, _): Notty.Unescape.key): string =>
  switch (k) {
  | `ASCII(c) => String.make(1, c)
  | `Uchar(u) => utf8_of_uchar(u)
  | `Enter => "\n"
  | `Tab => "\t"
  | _ => ""
  };

let translate =
    (st: state, ev: Notty.Unescape.event): (state, list(AnsiInput.event)) =>
  switch (st.paste, ev) {
  | (None, `Key(k)) => (st, key_events(k))
  | (None, `Mouse(m)) => (st, mouse_events(m))
  | (None, `Paste(`Start)) => (
      {paste: Some(Stdlib.Buffer.create(64))},
      [],
    )
  | (None, `Paste(`End)) => (st, [])
  | (Some(buf), `Key(k)) =>
    Stdlib.Buffer.add_string(buf, paste_text(k));
    (st, []);
  | (Some(buf), `Paste(`End)) => (
      init,
      [PasteText(Stdlib.Buffer.contents(buf))],
    )
  | (Some(_), `Mouse(_))
  | (Some(_), `Paste(`Start)) => (st, [])
  };
