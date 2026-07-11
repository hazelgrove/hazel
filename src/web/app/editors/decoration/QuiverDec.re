/* QuiverDec: GUI decoration for canonical completion visualization.
 *
 * Shows "arrows" (delimiters) ready to be "fired" (inserted) to complete
 * incomplete syntax. Displays:
 *   - Small triangles at insertion points (below text baseline)
 *   - Offside boxes showing what delimiters will be inserted
 *
 * The quiver holds completion arrows.
 */

open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;

/* An insertion with its resolved position; shape = the caret shape
   at the pin (the pole is a ghost caret). */
type positioned_insertion = {
  row: int,
  col: int,
  shape: option(Util.Direction.t),
  delimiters: list(CanonicalCompletion.delimiter_info),
};

/* Does this chip hold the shard tab would put down right now? */
/* Chip text scale relative to the code font */
let chip_font_scale = 0.72;

let matches_droppable =
    (
      droppable: option((Id.t, int)),
      delimiters: list(CanonicalCompletion.delimiter_info),
    )
    : bool =>
  switch (droppable) {
  | None => false
  | Some((tid, k)) =>
    delimiters
    |> List.exists((d: CanonicalCompletion.delimiter_info) =>
         switch (d.of_shard) {
         | Some((tid', k')) => Id.equal(tid, tid') && k == k'
         | None => false
         }
       )
  };

/* Find a piece by id along with its containing segment and index */
let rec find_piece_ctx =
        (sg: Segment.t, id: Id.t): option((Segment.t, int, Piece.t)) => {
  let rec go = (i, ps): option((Segment.t, int, Piece.t)) =>
    switch (ps) {
    | [] => None
    | [p, ...rest] =>
      if (Id.equal(Piece.id(p), id)) {
        Some((sg, i, p));
      } else {
        let deeper =
          switch ((p: Piece.t)) {
          | Tile(t) =>
            List.fold_left(
              (acc, ch) =>
                switch (acc) {
                | Some(_) => acc
                | None => find_piece_ctx(ch, id)
                },
              None,
              t.children,
            )
          | _ => None
          };
        switch (deeper) {
        | Some(r) => Some(r)
        | None => go(i + 1, rest)
        };
      }
    };
  go(0, sg);
};

let find_piece_deep = (sg: Segment.t, id: Id.t): option(Piece.t) =>
  find_piece_ctx(sg, id) |> Option.map(((_, _, p)) => p);

/* Coincidence-first placement: a pin's position within its
   inter-content whitespace region (linebreaks included) is
   semantically free, so it FOLLOWS the caret inside that zone and
   RESTS at the engine's spot otherwise. */
let resolve_position =
    (
      ~seg: Segment.t,
      ~caret_pos: option((int, int)),
      measured: Measured.t,
      ins: CanonicalCompletion.insertion,
    )
    : option(positioned_insertion) =>
  switch (Measured.find_by_id(ins.adjacent_id, measured)) {
  | None => None
  | Some(m) =>
    let (row, col) =
      switch (ins.side) {
      | Right => (m.last.row, m.last.col)
      | Left => (m.origin.row, m.origin.col)
      };
    let is_free = (p: Piece.t) =>
      switch (p) {
      | Grout(_)
      | Secondary(_) => true
      | _ => false
      };
    let leq = ((r1, c1), (r2, c2)) => r1 < r2 || r1 == r2 && c1 <= c2;
    switch (find_piece_ctx(seg, ins.adjacent_id)) {
    | None =>
      Some({
        row,
        col,
        shape: None,
        delimiters: ins.delimiters,
      })
    | Some((sg, i, p)) =>
      let rec prev_content = (j: int): option(Piece.t) =>
        j <= 0
          ? None
          : (
            switch (List.nth(sg, j - 1)) {
            | q when is_free(q) => prev_content(j - 1)
            | q => Some(q)
            }
          );
      let n = List.length(sg);
      let rec next_content = (j: int): option(Piece.t) =>
        j >= n
          ? None
          : (
            switch (List.nth(sg, j)) {
            | q when is_free(q) => next_content(j + 1)
            | q => Some(q)
            }
          );
      let measure_last = (q: Piece.t) =>
        Measured.find_by_id(Piece.id(q), measured)
        |> Option.map((qm: Measured.measurement) =>
             (qm.last.row, qm.last.col)
           );
      let measure_origin = (q: Piece.t) =>
        Measured.find_by_id(Piece.id(q), measured)
        |> Option.map((qm: Measured.measurement) =>
             (qm.origin.row, qm.origin.col)
           );
      /* zone bounds: previous/next content around the anchor's
         whitespace run (whole-document edges when absent) */
      let left_bound =
        switch (is_free(p) ? prev_content(i) : Some(p)) {
        | Some(q) => measure_last(q)
        | None => Some((0, 0))
        };
      let right_bound =
        switch (next_content(is_free(p) ? i : i + 1)) {
        | Some(q) => measure_origin(q)
        | None => None /* unbounded to the segment end */
        };
      /* resting spot: the left content edge when it shares the pin's
         line (the round-6 snap); the raw anchor position otherwise */
      let rest =
        switch (left_bound) {
        | Some((lr, lc)) when lr == row => (row, min(lc, col))
        | _ => (row, col)
        };
      let (row, col) =
        switch (caret_pos, left_bound) {
        | (Some((r, c)), Some(left))
            when
              leq(left, (r, c))
              && (
                switch (right_bound) {
                | Some(right) => leq((r, c), right)
                | None => true
                }
              ) => (
            r,
            c,
          )
        | _ => rest
        };
      /* ghost-caret shape at the pin: the shared-nib facing between
         the pieces around the insertion point, mirroring
         Siblings.direction_between (right neighborhood first) */
      let shape = {
        let (before, after) = Util.ListUtil.split_n(i + 1, sg);
        switch (Segment.edge_direction_of(Left, after)) {
        | None => Segment.edge_direction_of(Right, before)
        | d => d
        };
      };
      Some({
        row,
        col,
        shape,
        delimiters: ins.delimiters,
      });
    };
  };

/* Chip segments: the remainder is the payload (full contrast); the
   typed prefix and later coalesced segments fade. */
let delimiter_nodes =
    (
      ~font_metrics: FontMetrics.t,
      ~on_apply: option(Id.t => Ui_effect.t(unit)),
      delimiters: list(CanonicalCompletion.delimiter_info),
    )
    : list(Node.t) =>
  delimiters
  |> List.mapi((k, d: CanonicalCompletion.delimiter_info) => {
       let sep = k > 0 ? [Node.text(" ")] : [];
       let seg_cls = k > 0 ? ["chip-seg", "chip-seg-later"] : ["chip-seg"];
       /* modifier-click completes this delimiter's tile; unmodified
          pointer events fall through to the editor */
       let apply_attrs =
         switch (on_apply, d.of_shard) {
         | (Some(f), Some((tid, _))) => [
             Attr.on_pointerdown(evt =>
               Js_of_ocaml.Js.to_bool(evt##.metaKey)
               || Js_of_ocaml.Js.to_bool(evt##.ctrlKey)
                 ? Effect.Many([
                     Effect.Stop_propagation,
                     Effect.Prevent_default,
                     f(tid),
                   ])
                 : Effect.Ignore
             ),
           ]
         | _ => []
         };
       let body =
         switch (d.typed_len) {
         | Some(n) when n > 0 && n < String.length(d.text) => [
             Node.span(
               ~attrs=[Attr.classes(["chip-frac-typed"])],
               [Node.text(String.sub(d.text, 0, n))],
             ),
             Node.span(
               ~attrs=[Attr.classes(["chip-frac-rest"])],
               [
                 Node.text(String.sub(d.text, n, String.length(d.text) - n)),
               ],
             ),
           ]
         | _ => [Node.text(d.text)]
         };
       let suffix =
         d.needs_hole
           ? [
             Node.text(" "),
             EmptyHoleDec.view(
               FontMetrics.{
                 col_width: font_metrics.col_width *. chip_font_scale,
                 row_height: font_metrics.row_height *. chip_font_scale,
               },
               Grout.Convex,
             ),
           ]
           : [];
       sep
       @ [Node.span(~attrs=[Attr.classes(seg_cls)] @ apply_attrs, body)]
       @ suffix;
     })
  |> List.concat;

/* One interline chip: bubble centered on the line boundary above
   the insertion point, pole below. */
let chip_view =
    (
      ~font_metrics: FontMetrics.t,
      ~row: int,
      ~col: int,
      ~shape: option(Direction.t),
      ~caret_form: option((Direction.t, option(Direction.t))),
      ~live: bool,
      ~at_caret: bool,
      body: list(Node.t),
    )
    : Node.t => {
  let x = float_of_int(col) *. font_metrics.col_width;
  let y = float_of_int(row) *. font_metrics.row_height;
  /* the pole is a ghost caret: the path the real caret would draw
     here; hidden at coincidence */
  let pole =
    DecUtil.code_svg(
      ~font_metrics,
      ~origin={
        row,
        col,
      },
      ~base_cls=["quiver-chip-pole"],
      ~path_cls=["quiver-chip-pole-path"],
      ~scale=1.0,
      ~height_fudge=ShardDec.shadow_dy *. font_metrics.row_height,
      CaretDec.caret_base_path(Direction.Right, shape),
    );
  /* flag left edge = top-left corner of whichever caret stands at
     its foot: x = -(shape_adjust + caret_width/2) */
  let (dock_side, dock_shape) =
    switch (at_caret, caret_form) {
    | (true, Some((cs, csh))) => (cs, csh)
    | _ => (Direction.Right, shape)
    };
  let body_left =
    -. (
      ShardDec.shape_adjust(dock_side, dock_shape)
      +. 0.5
      *. CaretDec.caret_width
    )
    *. font_metrics.col_width;
  div(
    ~attrs=[
      Attr.classes(
        ["quiver-chip"]
        @ (
          switch (dock_shape) {
          | Some(Direction.Left) => ["chip-bend-left"]
          | Some(Right) => ["chip-bend-right"]
          | None => ["chip-straight"]
          }
        )
        @ (live ? ["chip-live"] : [])
        @ (at_caret ? ["chip-at-caret"] : []),
      ),
    ],
    [
      pole,
      div(
        ~attrs=[
          Attr.classes(["quiver-chip-anchor"]),
          Attr.create(
            "style",
            Printf.sprintf("left: %fpx; top: %fpx;", x, y),
          ),
        ],
        [
          div(
            ~attrs=[
              Attr.classes(["quiver-chip-body"]),
              Attr.create("style", Printf.sprintf("left: %fpx;", body_left)),
            ],
            body,
          ),
        ],
      ),
    ],
  );
};

/* Plain-text length of a chip's delimiters (for overlap coalescing) */
let delimiters_len =
    (delimiters: list(CanonicalCompletion.delimiter_info)): int =>
  delimiters
  |> List.map((d: CanonicalCompletion.delimiter_info) =>
       String.length(d.text) + (d.needs_hole ? 2 : 0)
     )
  |> List.fold_left((+), 0)
  |> (n => n + max(0, List.length(delimiters) - 1));

/* Overlapping same-row chips coalesce into the earlier one — only
   the first position survives its own application anyway. */
let coalesce_overlaps =
    (~font_metrics: FontMetrics.t, chips: list(positioned_insertion))
    : list(positioned_insertion) => {
  let chip_w = (c: positioned_insertion) =>
    float_of_int(delimiters_len(c.delimiters) + 2)
    *. font_metrics.col_width
    *. chip_font_scale;
  let rec go = (acc, rest) =>
    switch (acc, rest) {
    | (_, []) => List.rev(acc)
    | ([], [c, ...tl]) => go([c], tl)
    | ([prev, ...acc_tl], [c, ...tl]) =>
      let prev_right =
        float_of_int(prev.col) *. font_metrics.col_width +. chip_w(prev);
      let c_left = float_of_int(c.col) *. font_metrics.col_width;
      prev.row == c.row && c_left < prev_right +. 4.
        ? go(
            [
              {
                ...prev,
                delimiters: prev.delimiters @ c.delimiters,
              },
              ...acc_tl,
            ],
            tl,
          )
        : go([c, ...acc], tl);
    };
  go([], chips);
};

/* Main view function: renders quiver decorations for a segment */
let view =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~droppable: option((Id.t, int))=None,
      ~caret_pos: option((int, int))=None,
      ~caret_form: option((Direction.t, option(Direction.t)))=None,
      ~on_apply: option(Id.t => Ui_effect.t(unit))=None,
      /* the engine must see the user's REAL program: the display
         segment (CachedSyntax) still contains the suggestion-buffer
         ghost, which perturbs placement (an in anchoring at line
         start while a ghost completes Bo -> Bool). Anchor pieces
         exist in both segments, so engine insertions resolve fine
         against the display's measured map. */
      ~engine_seg: Segment.t,
      seg: Segment.t,
    )
    : Node.t => {
  ignore(seg);
  let seg = engine_seg;
  /* Get completion result with insertions */
  let result = CanonicalCompletion.for_editor(seg);
  let insertions = result.insertions;

  /* reset even when nothing draws: a vanished quiver must not leave
     stale row claims displacing probe offsides */
  RowOffsets.reset();

  if (List.length(insertions) == 0) {
    /* No completions needed */
    div([]);
  } else {
    let positioned =
      List.filter_map(
        resolve_position(~seg, ~caret_pos, measured),
        insertions,
      );
    let sorted =
      List.sort(
        (a, b) => {
          let row_cmp = Int.compare(a.row, b.row);
          row_cmp != 0 ? row_cmp : Int.compare(a.col, b.col);
        },
        positioned,
      );
    let chips =
      coalesce_overlaps(~font_metrics, sorted)
      |> List.map((ins: positioned_insertion) =>
           chip_view(
             ~font_metrics,
             ~row=ins.row,
             ~col=ins.col,
             ~shape=ins.shape,
             ~caret_form,
             ~live=matches_droppable(droppable, ins.delimiters),
             ~at_caret=caret_pos == Some((ins.row, ins.col)),
             delimiter_nodes(~font_metrics, ~on_apply, ins.delimiters),
           )
         );
    div(~attrs=[Attr.classes(["quiver-decorations"])], chips);
  };
};
