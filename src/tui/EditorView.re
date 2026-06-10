open Haz3lcore;
open Util;

/* Renders an editor's syntax to styled frame rows. This is a port of the
   walk in src/web/app/editors/code/Code.re, emitting (style, text) spans
   instead of Vdom nodes; the token classification predicates are shared
   with the web via Haz3lcore. Every emitted cell must line up with the
   columns in syntax.measured, or caret/selection positions drift.

   TODO(tui): extract the pure token classifier from Code.of_delim' into
   haz3lcore so web and TUI share it by construction. */

/* Mutable row builder used during the segment walk */
module Builder = {
  type t = {
    mutable rows: list(Frame.row), /* reversed */
    mutable current: list(Frame.span) /* reversed */
  };

  let create = (): t => {
    rows: [],
    current: [],
  };

  let emit = (b: t, style: Style.t, text: string): unit =>
    if (text != "") {
      b.current = [(style, text), ...b.current];
    };

  /* End the current row, insert (count - 1) blank rows, and start the
     next row at the given indentation — the row-wise equivalent of the
     web's "\n"*count ++ " "*indent text node. */
  let newline = (b: t, ~count: int, ~indent: int): unit => {
    b.rows = [List.rev(b.current), ...b.rows];
    for (_ in 2 to count) {
      b.rows = [[], ...b.rows];
    };
    b.current =
      indent > 0 ? [(Style.default, String.make(indent, ' '))] : [];
  };

  let finish = (b: t): list(Frame.row) =>
    List.rev([List.rev(b.current), ...b.rows]);
};

let is_ref = (token: string, sort: Sort.t) =>
  sort != Pat
  && sort != TPat
  && !Token.is_keyword(token)
  && !Token.is_base_typ(token)
  && Token.is_typ_var(token);

/* Mirrors the class computation in Code.of_delim' */
let style_of_token =
    (
      ~token: string,
      ~sort: Sort.t,
      ~is_consistent: bool,
      ~is_in_buffer: bool,
      ~is_complete: bool,
      ~is_infix_var: bool,
    )
    : Style.t => {
  let base_cls =
    switch (token) {
    | _ when !is_consistent => "sort-inconsistent"
    | _ when !is_complete => "incomplete"
    | _ when Token.is_llm_hole(token) => "llm-waiting"
    | _ when Token.is_explicit_hole(token) => "explicit-hole"
    | _ when Token.is_string(token) => "string-lit"
    | _ when is_infix_var => "Any"
    | _ => Sort.class_of(sort)
    };
  let style = Theme.of_base_cls(base_cls);
  let style = Token.is_keyword(token) ? Style.bold(style) : style;
  let style = is_in_buffer ? Style.dim(style) : style;
  ignore(is_ref); /* web's "ref" class is a subtle hue shift; skipped here */
  style;
};

let rows = (editor: Editor.Model.t): list(Frame.row) => {
  module DeferredLinebreaks = Measured.MkDeferredLinebreaks();

  let z = editor.state.zipper;
  let syntax = editor.syntax;
  let measured = syntax.measured;
  let term_data = syntax.term_data;
  let shape_map = syntax.shape_map;
  let buffer_ids =
    Selection.is_buffer(z.selection) ? syntax.selection_ids : [];

  let b = Builder.create();

  let sort = (t: Tile.t): Sort.t => t.mold.out;

  let is_consistent = (sort: Sort.t, t: Tile.t) =>
    switch (Id.Map.find_opt(t.id, term_data)) {
    | None => true
    | Some(data) =>
      switch (sort, data.sort) {
      | (Any, _)
      | (_, Any) => true
      | (Rul, Exp) => true
      | (Exp, Rul) => true
      | (Drv(_), _) => true
      | _ => sort == data.sort
      }
    };

  let of_delim = (t: Piece.tile, i: int): unit => {
    let token = List.nth(t.label, i);
    let sort = sort(t);
    let style =
      style_of_token(
        ~token,
        ~sort,
        ~is_consistent=is_consistent(sort, t),
        ~is_in_buffer=List.mem(t.id, buffer_ids),
        ~is_complete=Tile.is_complete(t),
        ~is_infix_var=
          Mold.is_infix_op(t.mold)
          && Form.is_infix_delimiter_op_prefix(token),
      );
    Builder.emit(b, style, token);
  };

  let measure_of = p => Measured.find_p(~msg="EditorView", p, measured);

  let of_grout = (g: Grout.t): unit =>
    switch (g.shape) {
    | Convex => Builder.emit(b, Theme.grout, "_")
    | Concave => Builder.emit(b, Theme.grout, "\xc2\xb7") /* · */
    };

  let of_secondary = (secondary: Secondary.t): unit =>
    switch (secondary.content) {
    | Whitespace(str) when str == Token.linebreak =>
      let indent = measure_of(Secondary(secondary)).last.col;
      Builder.newline(b, ~count=DeferredLinebreaks.of_secondary(), ~indent);
    | Whitespace(str) when str == Token.space =>
      Builder.emit(b, Style.default, " ")
    | Whitespace(_) => failwith("EditorView: Unrecognized Secondary")
    | Comment(str) when List.mem(secondary.id, buffer_ids) =>
      Builder.emit(b, Style.dim(Theme.comment), str)
    | Comment(str) => Builder.emit(b, Theme.comment, str)
    };

  let of_projector = (pr: Base.projector): unit => {
    let indent = measure_of(Projector(pr)).last.col;
    let size = DeferredLinebreaks.of_projector(pr, shape_map);
    let cols = size.row == 0 ? size.col : indent;
    /* The web draws projector content in a separate decoration layer the
       TUI doesn't have, so projectors render as blanks of their measured
       shape; folds at least show their glyph. */
    switch (pr.kind) {
    | ProjectorCore.Kind.Fold when size.row == 0 && size.col >= 1 =>
      Builder.emit(b, Theme.grout, "\xe2\x8b\xb1"); /* ⋱ */
      Builder.emit(b, Style.default, String.make(size.col - 1, ' '));
    | _ =>
      if (size.row > 0) {
        Builder.newline(b, ~count=size.row, ~indent=0);
      };
      Builder.emit(b, Style.default, String.make(cols, ' '));
    };
  };

  /* In-order traversal of a tile's shards and children. (The web uses
     Aba.join here, but that evaluates right-to-left, which is fine for
     building pure Vdom nodes and wrong for our in-order emit.) */
  let rec of_segment = (seg: Segment.t): unit =>
    List.iter(
      fun
      | Piece.Tile(t) => of_tile(t)
      | Piece.Grout(g) => of_grout(g)
      | Piece.Secondary(s) => of_secondary(s)
      | Piece.Projector(pr) => of_projector(pr),
      seg,
    )
  and of_tile = (t: Piece.tile): unit => {
    let rec go = (shards: list(int), children: list(Segment.t)) =>
      switch (shards, children) {
      | ([i], []) => of_delim(t, i)
      | ([i, ...is], [c, ...cs]) =>
        of_delim(t, i);
        of_segment(c);
        go(is, cs);
      | _ => () /* malformed tile; render what we can */
      };
    go(t.shards, t.children);
  };

  of_segment(syntax.segment);
  Builder.finish(b);
};

/* Caret position in buffer (row, col) coordinates */
let caret_point = (editor: Editor.Model.t): Point.t =>
  Zipper.Caret.point(editor.syntax.measured, editor.state.zipper);

/* Selection range in buffer coordinates, ordered, if non-empty. The
   selection's content is spatially contiguous, so the linear range
   between anchor and caret covers exactly the selected cells. */
let selection_range = (editor: Editor.Model.t): option((Point.t, Point.t)) => {
  let z = editor.state.zipper;
  if (Selection.is_empty(z.selection)) {
    None;
  } else {
    switch (Zipper.selection_anchor_point(editor.syntax.measured, z)) {
    | None => None
    | Some(anchor) =>
      let caret = caret_point(editor);
      Point.compare(anchor, caret) <= 0
        ? Some((anchor, caret)) : Some((caret, anchor));
    };
  };
};

/* === column-wise span surgery (selection overlay, clipping) === */

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

let span_cols = ((_, text): Frame.span): int =>
  Unicode.Width.columns_of_string(text);

/* Apply [f] to the styles of all cells in [first, last) columns of a row */
let map_col_range =
    (row: Frame.row, ~first: int, ~last: int, f: Style.t => Style.t)
    : Frame.row => {
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
let pad_row_to = (row: Frame.row, cols: int): Frame.row => {
  let w = row |> List.map(span_cols) |> List.fold_left((+), 0);
  w >= cols ? row : row @ [(Style.default, String.make(cols - w, ' '))];
};

/* Reverse-video the cells between two buffer points (selection overlay) */
let apply_selection =
    (rows: list(Frame.row), ~from: Point.t, ~to_: Point.t): list(Frame.row) =>
  List.mapi(
    (r, row) =>
      if (r < from.row || r > to_.row || Point.equals(from, to_)) {
        row;
      } else {
        let first = r == from.row ? from.col : 0;
        let last = r == to_.row ? to_.col : 100000;
        let row =
          r > from.row && r == to_.row ? pad_row_to(row, to_.col) : row;
        map_col_range(row, ~first, ~last, Style.reverse);
      },
    rows,
  );

/* Clip a row horizontally to [col_off, col_off + width) */
let clip_row = (row: Frame.row, ~col_off: int, ~width: int): Frame.row => {
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
