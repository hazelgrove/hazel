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

/* An insertion with its resolved position */
type positioned_insertion = {
  row: int,
  col: int,
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

/* Resolve an insertion's position. COINCIDENCE-FIRST placement: a
   pin's exact position within a run of grout/whitespace is
   semantically free — materialization doesn't care — so spend the
   freedom on caret alignment. The pin's free zone spans the
   same-line grout/whitespace run around its anchor (bounded by the
   surrounding content pieces); when the caret is inside the zone the
   pin FOLLOWS it exactly (the takeover then shows one caret-like
   object); otherwise it rests at the zone's left content edge. */
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
      | Grout(_) => true
      | Secondary(sec) => !Secondary.is_linebreak(sec)
      | _ => false
      };
    let col =
      switch (find_piece_ctx(seg, ins.adjacent_id)) {
      | None => col
      | Some((sg, i, p)) =>
        /* zone left bound: right edge of the previous content piece */
        let rec prev_content = (j: int): option(Piece.t) =>
          j <= 0
            ? None
            : (
              switch (List.nth(sg, j - 1)) {
              | q when is_free(q) => prev_content(j - 1)
              | q => Some(q)
              }
            );
        let left_edge =
          switch (is_free(p) ? prev_content(i) : Some(p)) {
          | Some(q) =>
            switch (Measured.find_by_id(Piece.id(q), measured)) {
            | Some(qm) when qm.last.row == row => Some(qm.last.col)
            | _ => None
            }
          | None => Some(0)
          };
        /* zone right bound: left edge of the next content piece on
           the same line; an unbounded line end follows any column */
        let n = List.length(sg);
        let rec next_content = (j: int): option(Piece.t) =>
          j >= n
            ? None
            : (
              switch (List.nth(sg, j)) {
              | q when is_free(q) => next_content(j + 1)
              | Piece.Secondary(_) => None /* linebreak ends the zone */
              | q => Some(q)
              }
            );
        let right_edge =
          switch (next_content(is_free(p) ? i : i + 1)) {
          | Some(q) =>
            switch (Measured.find_by_id(Piece.id(q), measured)) {
            | Some(qm) when qm.origin.row == row => Some(qm.origin.col)
            | _ => Some(col)
            }
          | None => None /* unbounded to the line end */
          };
        switch (left_edge) {
        | None => col
        | Some(left) =>
          switch (caret_pos) {
          | Some((r, c))
              when
                r == row
                && c >= left
                && (
                  switch (right_edge) {
                  | Some(right) => c <= right
                  | None => true
                  }
                ) => c
          | _ => min(left, col)
          }
        };
      };
    Some({
      row,
      col,
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
   boundary above the insertion point, flagpole-aligned with a
   STRAIGHT caret bar — the same path/metrics the real caret draws,
   shape deliberately not shown (a remnant token's nib shapes are
   incidental: le's convex right stops existing once the t arrives).
   When the real caret sits exactly on the pin, the chip takes the
   caret's color and the caret glyph hides (CSS :has) — the signpost
   IS the caret there. */
let chip_view =
    (
      ~font_metrics: FontMetrics.t,
      ~row: int,
      ~col: int,
      ~live: bool,
      ~at_caret: bool,
      body: list(Node.t),
    )
    : Node.t => {
  let x = float_of_int(col) *. font_metrics.col_width;
  let y = float_of_int(row) *. font_metrics.row_height;
  /* flagpole: pole and bubble are sibling divs in the same anchor,
     sharing the SAME left offset float — one layout-rounding path,
     so their left edges align exactly (the SVG bar rounded on a
     different path and jittered a fraction of a pixel either way).
     Pole dimensions replicate the straight caret: caret_width wide
     centered on the column boundary, row height + shadow reach. */
  let body_left = -. (0.5 *. CaretDec.caret_width *. font_metrics.col_width);
  let pole =
    div(
      ~attrs=[
        Attr.classes(["quiver-chip-pole"]),
        Attr.create(
          "style",
          Printf.sprintf(
            "left: %fpx; top: 0px; width: %fpx; height: %fpx;",
            body_left,
            CaretDec.caret_width *. font_metrics.col_width,
            font_metrics.row_height *. (1.0 +. ShardDec.shadow_dy),
          ),
        ),
      ],
      [],
    );
  div(
    ~attrs=[
      Attr.classes(
        ["quiver-chip", live ? "chip-live" : "chip-dim"]
        @ (at_caret ? ["chip-at-caret"] : []),
      ),
    ],
    [
      div(
        ~attrs=[
          Attr.classes(["quiver-chip-anchor"]),
          Attr.create(
            "style",
            Printf.sprintf("left: %fpx; top: %fpx;", x, y),
          ),
        ],
        [
          pole,
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
      ~caret_pos: option((int, int))=None,
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
             ~live=matches_droppable(droppable, ins.delimiters),
             ~at_caret=caret_pos == Some((ins.row, ins.col)),
             delimiter_nodes(~font_metrics, ins.delimiters),
           )
         );
    div(~attrs=[Attr.classes(["quiver-decorations"])], chips);
  };
};
