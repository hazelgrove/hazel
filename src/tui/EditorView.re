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

/* Build the buffer's styled rows. When [statics] is provided,
   projectors with a registered terminal view (TermProjector) render
   live content instead of blank space, and their offside views are
   returned as (row index, spans) for the caller to append after line
   ends. */
let rows_with_offside =
    (~statics: option(CachedStatics.t)=None, editor: Editor.Model.t)
    : (list(Frame.row), list((int, Frame.row))) => {
  module DeferredLinebreaks = Measured.MkDeferredLinebreaks();

  let z = editor.state.zipper;
  let syntax = editor.syntax;
  let measured = syntax.measured;
  let term_data = syntax.term_data;
  let shape_map = syntax.shape_map;
  let buffer_ids =
    Selection.is_buffer(z.selection) ? syntax.selection_ids : [];

  let b = Builder.create();
  let offsides: ref(list((int, Frame.row))) = ref([]);

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

  /* Fallback when a projector has no terminal view: blank space of
     its measured shape (folds at least show their glyph) */
  let of_projector_blank = (pr: Base.projector, size: Point.t): unit => {
    let indent = measure_of(Projector(pr)).last.col;
    let cols = size.row == 0 ? size.col : indent;
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

  /* Splice a Block-shaped terminal view: the first row continues the
     current line; the rest are whole rows; the final row must occupy
     exactly [last_col] cells so following content lines up. */
  let emit_block = (lines: list(Frame.row), ~rows: int, ~last_col: int) => {
    let lines = {
      let n = List.length(lines);
      n >= rows + 1
        ? Util.ListUtil.take(rows + 1, lines)
        : lines @ List.init(rows + 1 - n, _ => []);
    };
    List.iteri(
      (i, line) => {
        if (i > 0) {
          Builder.newline(b, ~count=1, ~indent=0);
        };
        let line =
          i == rows
            ? Frame.clip_row(
                Frame.pad_row_to(line, last_col),
                ~col_off=0,
                ~width=last_col,
              )
            : line;
        List.iter(((st, tx)) => Builder.emit(b, st, tx), line);
      },
      lines,
    );
  };

  let of_projector = (pr: Base.projector): unit => {
    let size = DeferredLinebreaks.of_projector(pr, shape_map);
    let indent = measure_of(Projector(pr)).last.col;
    let view =
      switch (statics, TermProjector.lookup(pr.kind)) {
      | (Some(st), tp) =>
        /* view calls must not take down the frame on unexpected syntax */
        switch (TermProjector.mk_info(~statics=st, pr)) {
        | info =>
          switch (tp) {
          | Some(tp) =>
            switch (tp.offside_view(~model=pr.model, ~info)) {
            | Some(spans) =>
              let row = List.length(b.rows);
              offsides := [(row, spans), ...offsides^];
            | None => ()
            | exception _ => ()
            }
          | None => ()
          };
          if (size.row == 0 && size.col >= 1) {
            let inline =
              switch (tp) {
              | Some(tp) =>
                tp.inline_view(~model=pr.model, ~info, ~width=size.col)
              | None => None
              };
            let inline =
              switch (inline) {
              | Some(_) => inline
              /* kinds without a terminal view get the syntax chip */
              | None => TermProjector.syntax_chip(~info, ~width=size.col)
              };
            Option.map(spans => `Inline(spans), inline);
          } else if (size.row > 0) {
            switch (tp) {
            | Some(tp) =>
              tp.block_view(
                ~model=pr.model,
                ~info,
                ~width=size.col,
                ~rows=size.row,
                ~last_col=indent,
              )
              |> Option.map(lines => `Block(lines))
            | None => None
            };
          } else {
            None;
          };
        | exception _ => None
        }
      | _ => None
      };
    switch (view) {
    | Some(`Inline(spans)) =>
      List.iter(((st, tx)) => Builder.emit(b, st, tx), spans)
    | Some(`Block(lines)) =>
      emit_block(lines, ~rows=size.row, ~last_col=indent)
    | None => of_projector_blank(pr, size)
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
  (Builder.finish(b), offsides^);
};

let rows = (editor: Editor.Model.t): list(Frame.row) =>
  fst(rows_with_offside(editor));

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

/* A (row, first_col, last_col-exclusive) range to be styled */
type col_range = {
  range_row: int,
  first: int,
  last: int,
};

/* Per-row ranges covered by a measurement (multi-row spans clamp
   intermediate rows to their measured max_col) */
let ranges_of_measurement =
    (m: Measured.measurement, rows_shape: Measured.Rows.t): list(col_range) =>
  if (m.origin.row == m.last.row) {
    [
      {
        range_row: m.origin.row,
        first: m.origin.col,
        last: m.last.col,
      },
    ];
  } else {
    let row_max = r =>
      switch (Measured.Rows.find_opt(r, rows_shape)) {
      | Some(shape) => shape.max_col
      | None => 0
      };
    List.init(
      m.last.row - m.origin.row + 1,
      i => {
        let r = m.origin.row + i;
        {
          range_row: r,
          first: r == m.origin.row ? m.origin.col : 0,
          last: r == m.last.row ? m.last.col : row_max(r),
        };
      },
    );
  };

/* Where the web draws error/warning "arms" under a term, the TUI
   underlines the term's shards: resolve each statics id to its root
   tile (as Arms.Errors.of_id does) and take the shard measurements. */
let id_ranges = (ids: list(Id.t), editor: Editor.Model.t): list(col_range) => {
  let syntax = editor.syntax;
  let measured = syntax.measured;
  ids
  |> List.concat_map(id =>
       switch (TermData.root_tile(id, syntax.term_data)) {
       | Some(t) =>
         switch (Id.Map.find_opt(t.id, measured.tiles)) {
         | Some(shards) =>
           shards
           |> List.concat_map(((_, m)) =>
                ranges_of_measurement(m, measured.rows)
              )
         | None => []
         }
       | None =>
         /* projectors are not in the tile map (cf. Arms.Errors.of_id) */
         switch (Id.Map.find_opt(id, measured.projectors)) {
         | Some(m) => ranges_of_measurement(m, measured.rows)
         | None => []
         }
       }
     );
};

let error_ranges = (statics: CachedStatics.t, editor: Editor.Model.t) =>
  id_ranges(statics.error_ids, editor);

let warning_ranges = (statics: CachedStatics.t, editor: Editor.Model.t) =>
  id_ranges(statics.warning_ids, editor);

/* === overlays in buffer coordinates (row surgery lives in Frame) === */

/* Apply a style transform over a set of (row, col-range) extents */
let apply_ranges =
    (rows: list(Frame.row), ranges: list(col_range), f: Style.t => Style.t)
    : list(Frame.row) =>
  List.mapi(
    (r, row) =>
      List.fold_left(
        (row, {range_row, first, last}) =>
          range_row == r && last > first
            ? Frame.map_col_range(row, ~first, ~last, f) : row,
        row,
        ranges,
      ),
    rows,
  );

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
          r > from.row && r == to_.row ? Frame.pad_row_to(row, to_.col) : row;
        Frame.map_col_range(row, ~first, ~last, Style.reverse);
      },
    rows,
  );
