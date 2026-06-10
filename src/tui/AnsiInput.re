open Util;

/* Pure parser from raw terminal input bytes to key events. Stateful only
   in that an escape sequence may be split across stdin chunks; the
   unconsumed suffix is carried in [state.pending].

   Editor-relevant keys become [Util.Key.t] records (the same shape the
   web app builds from DOM events) so they can be fed directly to
   Web.Keyboard.handle_key_event. Keys that the web handles outside the
   core keymap (save, undo, quit, tab, ...) become [Tui] events. */

[@deriving show({with_path: false})]
type tui_key =
  | Quit /* Ctrl+C / Ctrl+Q */
  | Save /* Ctrl+S */
  | Undo /* Ctrl+Z */
  | Redo /* Ctrl+Y (Ctrl+Shift+Z is byte-identical to Ctrl+Z) */
  | TabKey
  | ShiftTab
  | PageUp
  | PageDown
  | ToggleResultPane; /* Ctrl+R */

/* SGR mouse events; points are 0-based screen coordinates */
[@deriving show({with_path: false})]
type mouse =
  | Press(Point.t, bool) /* left button; bool = shift held */
  | Drag(Point.t) /* motion with left button held */
  | Release
  | Wheel(int); /* rows to scroll; negative = up */

[@deriving show({with_path: false})]
type event =
  | Editor(Key.t)
  | Tui(tui_key)
  | Mouse(mouse)
  | PasteText(string); /* bracketed paste */

type state = {pending: string};

let init: state = {pending: ""};

let held = (b: bool): Key.held => b ? Down : Up;

let mk =
    (~shift=false, ~ctrl=false, ~alt=false, ~meta=false, name: string): Key.t => {
  key: D(name),
  code: name,
  sys: PC,
  shift: held(shift),
  meta: held(meta),
  ctrl: held(ctrl),
  alt: held(alt),
};

/* Byte length of the UTF-8 sequence starting with lead byte [c] */
let utf8_len = (c: char): int => {
  let code = Char.code(c);
  if (code < 0x80) {
    1;
  } else if (code >= 0xf0) {
    4;
  } else if (code >= 0xe0) {
    3;
  } else if (code >= 0xc0) {
    2;
  } else {
    1; /* stray continuation byte; consume to avoid getting stuck */
  };
};

/* Control bytes 0x00-0x1f outside of tab/enter/escape. TUI-reserved
   chords are intercepted here; the rest pass through as Ctrl+letter so
   the PC keymap in Web.Keyboard works (Ctrl+A select-all, etc.). */
let ctrl_event = (code: int): list(event) =>
  switch (code) {
  | 0x03 /* Ctrl+C */
  | 0x11 /* Ctrl+Q */ => [Tui(Quit)]
  | 0x13 /* Ctrl+S */ => [Tui(Save)]
  | 0x1a /* Ctrl+Z */ => [Tui(Undo)]
  | 0x19 /* Ctrl+Y */ => [Tui(Redo)]
  | 0x12 /* Ctrl+R */ => [Tui(ToggleResultPane)]
  | 0x08 /* Ctrl+H / BS */ => [Editor(mk("Backspace"))]
  | _ when code >= 0x01 && code <= 0x1a => [
      Editor(mk(~ctrl=true, String.make(1, Char.chr(code + 0x60)))),
    ]
  | _ => []
  };

type modifiers = {
  m_shift: bool,
  m_alt: bool,
  m_ctrl: bool,
  m_meta: bool,
};

let no_mods = {
  m_shift: false,
  m_alt: false,
  m_ctrl: false,
  m_meta: false,
};

/* xterm encodes modifiers as param value (bitmask + 1):
   1=Shift, 2=Alt, 4=Ctrl, 8=Meta */
let mods_of_param = (m: int): modifiers => {
  let bits = m - 1;
  {
    m_shift: bits land 1 != 0,
    m_alt: bits land 2 != 0,
    m_ctrl: bits land 4 != 0,
    m_meta: bits land 8 != 0,
  };
};

let key_event = (name: string, mods: modifiers): event =>
  Editor(
    mk(
      ~shift=mods.m_shift,
      ~ctrl=mods.m_ctrl,
      ~alt=mods.m_alt,
      ~meta=mods.m_meta,
      name,
    ),
  );

let arrow_name = (final: char): option(string) =>
  switch (final) {
  | 'A' => Some("ArrowUp")
  | 'B' => Some("ArrowDown")
  | 'C' => Some("ArrowRight")
  | 'D' => Some("ArrowLeft")
  | 'H' => Some("Home")
  | 'F' => Some("End")
  | _ => None
  };

/* CSI <num> ~ "vt-style" keys */
let tilde_key = (num: int, mods: modifiers): list(event) =>
  switch (num) {
  | 1
  | 7 => [key_event("Home", mods)]
  | 4
  | 8 => [key_event("End", mods)]
  | 3 => [key_event("Delete", mods)]
  | 5 => [Tui(PageUp)]
  | 6 => [Tui(PageDown)]
  | _ => []
  };

let paste_terminator = "\x1b[201~";

/* Find the index of [needle] in [hay] starting at [from] */
let find_sub = (hay: string, needle: string, from: int): option(int) => {
  let nlen = String.length(needle);
  let limit = String.length(hay) - nlen;
  let rec go = i =>
    if (i > limit) {
      None;
    } else if (String.sub(hay, i, nlen) == needle) {
      Some(i);
    } else {
      go(i + 1);
    };
  go(from);
};

/* SGR mouse report: CSI < Cb ; Cx ; Cy (M = press/drag, m = release).
   Cb bits: 0-1 button, +4 shift, +32 motion, +64 wheel. */
let mouse_event = (params: list(int), final: char): list(event) =>
  switch (params) {
  | [cb, cx, cy] =>
    let pt =
      Point.{
        row: cy - 1,
        col: cx - 1,
      };
    if (final == 'm') {
      [Mouse(Release)];
    } else if (cb land 64 != 0) {
      [Mouse(Wheel(cb land 1 == 0 ? (-3) : 3))];
    } else if (cb land 32 != 0) {
      cb land 3 == 0 ? [Mouse(Drag(pt))] : [];
    } else {
      switch (cb land 3) {
      | 0 => [Mouse(Press(pt, cb land 4 != 0))]
      | _ => [] /* middle/right buttons unused */
      };
    };
  | _ => []
  };

/* Parse a CSI sequence whose parameter bytes start at [i] (i.e. just
   after "\x1b["). Returns None if the sequence is incomplete and we
   should wait for more input; otherwise the events and the index just
   past the sequence. */
let parse_csi = (s: string, i: int): option((list(event), int)) => {
  let len = String.length(s);
  let rec find_final = j =>
    if (j >= len) {
      None;
    } else {
      let c = Char.code(s.[j]);
      c >= 0x40 && c <= 0x7e ? Some(j) : find_final(j + 1);
    };
  /* SGR mouse sequences are prefixed with '<' */
  let is_mouse = i < len && s.[i] == '<';
  let i = is_mouse ? i + 1 : i;
  switch (find_final(i)) {
  | None => None
  | Some(j) =>
    let final = s.[j];
    let params =
      String.sub(s, i, j - i)
      |> String.split_on_char(';')
      |> List.filter_map(int_of_string_opt);
    if (is_mouse) {
      switch (final) {
      | 'M'
      | 'm' => Some((mouse_event(params, final), j + 1))
      | _ => Some(([], j + 1))
      };
    } else {
      let mods =
        switch (params) {
        | [_, m, ..._] => mods_of_param(m)
        | _ => no_mods
        };
      switch (final) {
      | 'A'
      | 'B'
      | 'C'
      | 'D'
      | 'H'
      | 'F' =>
        let name = Option.get(arrow_name(final));
        Some(([key_event(name, mods)], j + 1));
      | 'Z' => Some(([Tui(ShiftTab)], j + 1))
      | '~' =>
        switch (params) {
        | [200, ..._] =>
          /* Bracketed paste: collect everything up to ESC[201~. If the
             terminator hasn't arrived yet, wait for more input. */
          switch (find_sub(s, paste_terminator, j + 1)) {
          | None => None
          | Some(t) =>
            let text = String.sub(s, j + 1, t - (j + 1));
            Some(([PasteText(text)], t + String.length(paste_terminator)));
          }
        | [num, ..._] => Some((tilde_key(num, mods), j + 1))
        | [] => Some(([], j + 1))
        }
      | _ => Some(([], j + 1)) /* unhandled CSI (focus, ...) */
      };
    };
  };
};

/* SS3 sequences (ESC O x): application-mode cursor keys */
let ss3 = (c: char): list(event) =>
  switch (arrow_name(c)) {
  | Some(name) => [key_event(name, no_mods)]
  | None => [] /* F1-F4 etc. */
  };

let rec go = (s: string, i: int, acc: list(event)): (list(event), string) => {
  let len = String.length(s);
  let keep_pending = () => (List.rev(acc), String.sub(s, i, len - i));
  if (i >= len) {
    (List.rev(acc), "");
  } else {
    let code = Char.code(s.[i]);
    if (code == 0x1b) {
      if (i + 1 >= len) {
        keep_pending(); /* lone ESC: bare Escape key or a split sequence */
      } else {
        switch (s.[i + 1]) {
        | '[' =>
          switch (parse_csi(s, i + 2)) {
          | None => keep_pending()
          | Some((evs, next)) => go(s, next, List.rev_append(evs, acc))
          }
        | 'O' =>
          if (i + 2 >= len) {
            keep_pending();
          } else {
            go(s, i + 3, List.rev_append(ss3(s.[i + 2]), acc));
          }
        | c =>
          /* ESC + char: Alt+char (terminal "meta sends escape") */
          let n = utf8_len(c);
          if (i + 1 + n > len) {
            keep_pending();
          } else {
            let ch = String.sub(s, i + 1, n);
            go(s, i + 1 + n, [Editor(mk(~alt=true, ch)), ...acc]);
          };
        };
      };
    } else if (code == 0x7f) {
      go(s, i + 1, [Editor(mk("Backspace")), ...acc]);
    } else if (code == 0x0d || code == 0x0a) {
      go(s, i + 1, [Editor(mk("Enter")), ...acc]);
    } else if (code == 0x09) {
      go(s, i + 1, [Tui(TabKey), ...acc]);
    } else if (code < 0x20) {
      go(s, i + 1, List.rev_append(ctrl_event(code), acc));
    } else {
      let n = utf8_len(s.[i]);
      if (i + n > len) {
        keep_pending(); /* split multi-byte character */
      } else {
        go(s, i + n, [Editor(mk(String.sub(s, i, n))), ...acc]);
      };
    };
  };
};

let parse = (st: state, chunk: string): (state, list(event)) => {
  let (events, pending) = go(st.pending ++ chunk, 0, []);
  ({pending: pending}, events);
};

/* Called on an idle timer when input stops while bytes are pending.
   A human pressing the bare Escape key produces exactly one ESC byte;
   anything longer is a split sequence still in flight, so keep waiting. */
let flush = (st: state): (state, list(event)) =>
  st.pending == "\x1b"
    ? ({pending: ""}, [Editor(mk("Escape"))]) : (st, []);
