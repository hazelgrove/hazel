open Util;

/* A declarative frame: styled text rows plus an optional cursor position,
   rendered to a single ANSI string. Inspired by notty's image model; the
   renderer is just data -> string, so a native backend could interpret
   the same frames.

   Strategy is full redraw per frame: the visible grid is small (a few KB
   of bytes), node writes it in well under a millisecond, and home +
   erase-to-end-of-line (rather than clear-screen) avoids flicker on the
   alternate screen. */

[@deriving show({with_path: false})]
type span = (Style.t, string); /* text must not contain newlines */

[@deriving show({with_path: false})]
type row = list(span);

[@deriving show({with_path: false})]
type t = {
  rows: list(row),
  /* screen coordinates, 0-based; None hides the terminal cursor */
  cursor: option(Point.t),
};

let cursor_to = (p: Point.t): string =>
  Printf.sprintf("\x1b[%d;%dH", p.row + 1, p.col + 1);

let render_row = (row: row): string =>
  row
  |> List.map(((style, text)) => Style.sgr(style) ++ text)
  |> String.concat("");

let render = (~size: (int, int), f: t): string => {
  let (rows, _cols) = size;
  let buf = Buffer.create(4096);
  /* synchronized output (ignored by terminals that lack it) + hide cursor
     while painting */
  Buffer.add_string(buf, "\x1b[?2026h\x1b[?25l\x1b[H");
  let visible = ListUtil.take(rows, f.rows);
  List.iteri(
    (i, row) => {
      if (i > 0) {
        Buffer.add_string(buf, "\r\n");
      };
      Buffer.add_string(buf, render_row(row));
      Buffer.add_string(buf, "\x1b[0m\x1b[K");
    }, /* clear rest of line */
    visible,
  );
  Buffer.add_string(buf, "\x1b[0m\x1b[J"); /* clear below content */
  switch (f.cursor) {
  | Some(p) when p.row >= 0 && p.col >= 0 =>
    Buffer.add_string(buf, cursor_to(p) ++ "\x1b[?25h")
  | _ => () /* leave cursor hidden */
  };
  Buffer.add_string(buf, "\x1b[?2026l");
  Buffer.contents(buf);
};

/* Plain-text version of a frame (for --dump / golden tests) */
let to_plain_text = (f: t): string =>
  f.rows
  |> List.map(row =>
       row |> List.map(((_, text)) => text) |> String.concat("")
     )
  |> String.concat("\n");
