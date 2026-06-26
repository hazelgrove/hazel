open Util;

/* A declarative frame: styled text rows plus an optional cursor
   position — the backend-agnostic boundary between the app's views and
   the terminal. NottyIO interprets frames as notty images for the
   interactive UI; to_plain_text serves --replay and golden tests. */

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

/* Plain-text version of a frame (for --dump / golden tests) */
let to_plain_text = (f: t): string =>
  f.rows
  |> List.map(row =>
       row |> List.map(((_, text)) => text) |> String.concat("")
     )
  |> String.concat("\n");

/* === column-wise row surgery (overlays, clipping, splicing) === */

let cluster_cols = Unicode.Width.columns_of_cluster;

/* Split a span's text at a display-column boundary */
let split_text_at_col = (text: string, col: int): (string, string) => {
  let clusters = Unicode.to_list(text);
  let rec go = (taken, remaining, cs) =>
    switch (cs) {
    | [] => (List.rev(taken), [])
    | [c, ...rest] =>
      let w = cluster_cols(c);
      w <= remaining
        ? go([c, ...taken], remaining - w, rest) : (List.rev(taken), cs);
    };
  let (pre, post) = go([], col, clusters);
  (String.concat("", pre), String.concat("", post));
};

let span_cols = ((_, text): span): int =>
  Unicode.Width.columns_of_string(text);

let row_cols = (row: row): int =>
  row |> List.map(span_cols) |> List.fold_left((+), 0);

/* Apply [f] to the styles of all cells in [first, last) columns of a row */
let map_col_range =
    (row: row, ~first: int, ~last: int, f: Style.t => Style.t): row => {
  let rec go = (col, spans) =>
    switch (spans) {
    | [] => []
    | [(style, text) as span, ...rest] =>
      let w = span_cols(span);
      let (s_first, s_last) = (col, col + w);
      if (s_last <= first || s_first >= last) {
        [span, ...go(s_last, rest)];
      } else {
        let (pre, mid_post) =
          split_text_at_col(text, max(0, first - s_first));
        let (mid, post) =
          split_text_at_col(
            mid_post,
            min(w, last - s_first) - max(0, first - s_first),
          );
        List.filter(
          ((_, t)) => t != "",
          [(style, pre), (f(style), mid), (style, post)],
        )
        @ go(s_last, rest);
      };
    };
  go(0, row);
};

/* Pad a row with spaces so overlays can extend past its content */
let pad_row_to = (row: row, cols: int): row => {
  let w = row_cols(row);
  w >= cols ? row : row @ [(Style.default, String.make(cols - w, ' '))];
};

/* Clip a row horizontally to [col_off, col_off + width) */
let clip_row = (row: row, ~col_off: int, ~width: int): row => {
  let rec drop = (col, spans) =>
    switch (spans) {
    | [] => []
    | [(style, text) as span, ...rest] =>
      let w = span_cols(span);
      if (col + w <= col_off) {
        drop(col + w, rest);
      } else if (col >= col_off) {
        spans;
      } else {
        let (_, post) = split_text_at_col(text, col_off - col);
        [(style, post), ...rest];
      };
    };
  let visible = drop(0, row);
  let rec take = (cols, spans) =>
    switch (spans) {
    | [] => []
    | [(style, text) as span, ...rest] =>
      let w = span_cols(span);
      if (w <= cols) {
        [span, ...take(cols - w, rest)];
      } else {
        let (pre, _) = split_text_at_col(text, cols);
        [(style, pre)];
      };
    };
  take(width, visible);
};

/* Splice [spans] over [row] starting at display column [col] (the
   covered cells are replaced; content before/after is preserved) */
let overlay_at = (row: row, ~col: int, spans: row): row => {
  let w = row_cols(spans);
  let prefix = pad_row_to(clip_row(row, ~col_off=0, ~width=col), col)
  and suffix = clip_row(row, ~col_off=col + w, ~width=100000);
  prefix @ spans @ suffix;
};
