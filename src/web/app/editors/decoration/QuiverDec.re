/* QuiverDec: GUI decoration for canonical completion visualization.
 *
 * Shows "arrows" (delimiters) ready to be "fired" (inserted) to complete
 * incomplete syntax. Displays:
 *   - Small triangles at insertion points (below text baseline)
 *   - Offside boxes showing what delimiters will be inserted
 *
 * Named to complement "Backpack" - the quiver holds completion arrows.
 */

open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;

/* An insertion with its resolved position. side/shape drive the
   caret-shaped bar: the bar IS the caret — same path, same metrics —
   so when the real caret sits at the insertion point they coincide
   exactly. */
type positioned_insertion = {
  row: int,
  col: int,
  side: Direction.t,
  shape: option(Direction.t),
  delimiters: list(CanonicalCompletion.delimiter_info),
};

/* Two-state tab emphasis (backpack-parity): the entry whose shard is
   what tab would put down RIGHT NOW — the head of the local missing
   shards, caret Outer — renders at full emphasis; everything else is
   slightly dimmed (the same signal the backpack display carried via
   its graying). Matching is by (tile id, shard index) provenance. */
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

let rec find_piece_deep = (sg: Segment.t, id: Id.t): option(Piece.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        if (Id.equal(Piece.id(p), id)) {
          Some(p);
        } else {
          switch (p) {
          | Tile(t) =>
            List.fold_left(
              (acc, ch) =>
                switch (acc) {
                | Some(_) => acc
                | None => find_piece_deep(ch, id)
                },
              None,
              t.children,
            )
          | _ => None
          };
        }
      },
    None,
    sg,
  );

/* Resolve an insertion's position by looking up adjacent_id in
   Measured. A right-side anchor whose delimiter would be
   space-separated from the anchor token (SpaceNormalize inserts one
   at materialization) shifts one column to the space side, so the
   arrow sits where the delimiter actually lands instead of flush
   against the existing token. Witness arrows (typed_len set) never
   shift: the completion continues the typed prefix directly. */
let resolve_position =
    (
      ~seg: Segment.t,
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
    let col =
      switch (ins.side, ins.delimiters) {
      | (Right, [{typed_len: None, text, _}, ..._]) =>
        let sep =
          switch (find_piece_deep(seg, ins.adjacent_id)) {
          | Some(p) =>
            switch (SpaceNormalize.last_token(p)) {
            | Some(a) => SpaceNormalize.needs_space(a, text)
            | None => false
            }
          | None => false
          };
        col + (sep ? 1 : 0);
      | _ => col
      };
    let shape =
      switch (find_piece_deep(seg, ins.adjacent_id)) {
      | Some(p) => Segment.edge_direction_of(ins.side, [p])
      | None => None
      };
    Some({
      row,
      col,
      side: ins.side,
      shape,
      delimiters: ins.delimiters,
    });
  };

/* Chip segment rendering. EMPHASIS INVERSION vs the old offside
   boxes (andrew): the chip sits AT the token, so its payload is what
   REMAINS — the yet-to-type remainder renders at full contrast and
   the already-typed prefix fades toward the chip color (it is
   visible in the code an em away). Later segments of a coalesced
   chip fade the same way: only the first is where tab acts, and
   only its position survives its own application. */
let delimiter_nodes =
    (
      ~font_metrics: FontMetrics.t,
      delimiters: list(CanonicalCompletion.delimiter_info),
    )
    : list(Node.t) =>
  delimiters
  |> List.mapi((k, d: CanonicalCompletion.delimiter_info) => {
       let sep = k > 0 ? [Node.text(" ")] : [];
       let seg_cls = k > 0 ? ["chip-seg", "chip-seg-later"] : ["chip-seg"];
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
       sep @ [Node.span(~attrs=[Attr.classes(seg_cls)], body)] @ suffix;
     })
  |> List.concat;

/* One interline chip: a solid speech-bubble centered on the line
   boundary above the insertion point, its first character centered
   over the insertion column, overlapping the top of a caret-shaped
   bar — the SAME path the real caret draws at that position, so
   caret and bar coincide exactly when the caret is there. */
let chip_view =
    (
      ~font_metrics: FontMetrics.t,
      ~row: int,
      ~col: int,
      ~side: Direction.t,
      ~shape: option(Direction.t),
      ~live: bool,
      body: list(Node.t),
    )
    : Node.t => {
  let x = float_of_int(col) *. font_metrics.col_width;
  let y = float_of_int(row) *. font_metrics.row_height;
  let bar =
    DecUtil.code_svg(
      ~font_metrics,
      ~origin={
        row,
        col,
      },
      ~base_cls=["quiver-chip-bar"],
      ~path_cls=["quiver-chip-bar-path"],
      ~scale=1.0,
      ~height_fudge=ShardDec.shadow_dy *. font_metrics.row_height,
      CaretDec.caret_base_path(side, shape),
    );
  /* first character of the chip text sits centered over the
     insertion column: padding (4px) + half a chip-scaled char */
  let body_left = -. (4.0 +. 0.5 *. chip_font_scale *. font_metrics.col_width);
  div(
    ~attrs=[Attr.classes(["quiver-chip", live ? "chip-live" : "chip-dim"])],
    [
      bar,
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

/* Coalesce chips that would overlap on the same interline: the later
   one's delimiters join the earlier chip (in column order). Only the
   first chip keeps its bar — after the first insertion is applied
   the flow changes, so later positions are not truthful anyway. */
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
      seg: Segment.t,
    )
    : Node.t => {
  /* Get completion result with insertions */
  let result = CanonicalCompletion.for_editor(seg);
  let insertions = result.insertions;

  /* claims from the previous render must not accumulate — reset even
     when there is nothing to draw, else a vanished quiver leaves its
     stale claims and probe offsides stay displaced until some other
     quiver render happens to reset. (Chips are interline overlays and
     claim no line-end space themselves, so reset is all that is
     needed: probes sit at the standard offset again.) */
  RowOffsets.reset();

  if (List.length(insertions) == 0) {
    /* No completions needed */
    div([]);
  } else {
    let positioned =
      List.filter_map(resolve_position(~seg, measured), insertions);
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
             ~side=ins.side,
             ~shape=ins.shape,
             ~live=matches_droppable(droppable, ins.delimiters),
             delimiter_nodes(~font_metrics, ins.delimiters),
           )
         );
    div(~attrs=[Attr.classes(["quiver-decorations"])], chips);
  };
};
