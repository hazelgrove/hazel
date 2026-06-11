open Haz3ltui;
open Notty;

/* Renders the TUI's backend-agnostic frames (Frame.t: styled rows +
   cursor) as notty images. Style.t carries 256-color indexes; notty
   exposes the same palette as a 6x6x6 cube + grayscale ramp. */

let color_of_index = (n: int): A.color =>
  if (n >= 232 && n <= 255) {
    A.gray(n - 232);
  } else if (n >= 16 && n <= 231) {
    let n = n - 16;
    A.rgb(~r=n / 36, ~g=n / 6 mod 6, ~b=n mod 6);
  } else {
    /* basic 16: the theme doesn't use these; pick something visible */
    A.white;
  };

let attr_of_style = (s: Style.t): A.t => {
  /* notty has no undercurl or per-underline color: render undercurled
     cells as underlined text in the curl's color (error red /
     warning yellow), which keeps the two distinguishable */
  let fg =
    switch (s.undercurl, s.fg) {
    | (Some(n), _) => Some(color_of_index(n))
    | (None, Ansi256(n)) => Some(color_of_index(n))
    | (None, Default) => None
    };
  /* notty has no dim/faint style: emulate by graying default-colored
     text (dim is used for grout, gutters, pane titles) */
  let fg =
    switch (s.dim, fg) {
    | (true, None) => Some(A.gray(10))
    | _ => fg
    };
  let bg =
    switch (s.bg) {
    | Ansi256(n) => Some(color_of_index(n))
    | Default => None
    };
  let cat = NottyAttr.cat;
  [
    switch (fg) {
    | Some(c) => A.fg(c)
    | None => A.empty
    },
    switch (bg) {
    | Some(c) => A.bg(c)
    | None => A.empty
    },
    s.bold ? A.st(A.bold) : A.empty,
    s.reverse ? A.st(A.reverse) : A.empty,
    s.undercurl != None ? A.st(A.underline) : A.empty,
  ]
  |> List.fold_left(cat, A.empty);
};

let image_of_row = (row: Frame.row): I.t =>
  switch (row) {
  | [] => I.void(0, 1) /* blank line must still occupy a row */
  | spans =>
    spans
    |> List.map(((style, text)) => I.string(attr_of_style(style), text))
    |> I.hcat
  };

let image_of_frame = (f: Frame.t): I.t =>
  f.rows |> List.map(image_of_row) |> I.vcat;

let render = (term: Notty_unix.Term.t, f: Frame.t): unit => {
  Notty_unix.Term.image(term, image_of_frame(f));
  Notty_unix.Term.cursor(
    term,
    Option.map((p: Util.Point.t) => (p.col, p.row), f.cursor),
  );
};
