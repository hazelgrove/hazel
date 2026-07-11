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
/* The anchor's sibling list, its index there, and — when that list
   is a tile's child — the IMMEDIATE enclosing (tile, child index):
   the zone bounds below need the parent's shards as walls. */
type piece_ctx = {
  sg: Segment.t,
  i: int,
  p: Piece.t,
  parent: option((Tile.t, int)),
};

let rec find_piece_ctx = (sg: Segment.t, id: Id.t): option(piece_ctx) => {
  let rec go = (i, ps): option(piece_ctx) =>
    switch (ps) {
    | [] => None
    | [p, ...rest] =>
      if (Id.equal(Piece.id(p), id)) {
        Some({
          sg,
          i,
          p,
          parent: None,
        });
      } else {
        let deeper =
          switch ((p: Piece.t)) {
          | Tile(t) =>
            List.fold_left(
              (acc, (ci, ch)) =>
                switch (acc) {
                | Some(_) => acc
                | None =>
                  find_piece_ctx(ch, id)
                  |> Option.map(ctx =>
                       ctx.parent == None
                         ? {
                           ...ctx,
                           parent: Some((t, ci)),
                         }
                         : ctx
                     )
                },
              None,
              List.mapi((ci, ch) => (ci, ch), t.children),
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
  find_piece_ctx(sg, id) |> Option.map(ctx => ctx.p);

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
    | Some({sg, i, p, parent}) =>
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
      /* Zone = the positions where this insertion lands
         identically: the whitespace run around the anchor, bounded
         by sibling content — or, in a tile's child, by the parent's
         SHARDS (a comma owed inside parens must never follow the
         caret past the `)`). Only at the top level does a missing
         bound mean open frontier. Dispatch (obligation_at_caret /
         TypeObligations.at_caret) matches this definition
         structurally by walking the caret's own siblings. */
      let parent_shard_wall = (which: Direction.t): option((int, int)) =>
        switch (parent) {
        | None => None
        | Some((t, ci)) =>
          let shard_idx =
            switch (which) {
            | Left => List.nth_opt(t.shards, ci)
            | Right => List.nth_opt(t.shards, ci + 1)
            };
          switch (shard_idx) {
          | None => None
          | Some(si) =>
            switch (Measured.find_shards(t, measured) |> List.assoc_opt(si)) {
            | Some(sm: Measured.measurement) =>
              switch (which) {
              | Left => Some((sm.last.row, sm.last.col))
              | Right => Some((sm.origin.row, sm.origin.col))
              }
            | None => None
            }
          };
        };
      let left_bound =
        switch (is_free(p) ? prev_content(i) : Some(p)) {
        | Some(q) => measure_last(q)
        | None =>
          switch (parent_shard_wall(Left)) {
          | Some(_) as wall => wall
          | None => Some((0, 0))
          }
        };
      let right_bound =
        switch (next_content(is_free(p) ? i : i + 1)) {
        | Some(q) => measure_origin(q)
        | None => parent_shard_wall(Right) /* None only at top level */
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
         the pieces around the insertion point. A side-Right insertion
         reads the right neighborhood first (mirroring
         Siblings.direction_between); a side-Left one (junction: the
         material lands against the content on its LEFT) reads the
         left neighborhood first — the chevron faces the content the
         pin docks to. */
      let shape = {
        /* the insertion point is right of the anchor for side-Right,
           left of it for side-Left — split so the anchor sits on the
           material's side, then read the facing neighborhood */
        let (before, after) =
          Util.ListUtil.split_n(
            switch (ins.side) {
            | Right => i + 1
            | Left => i
            },
            sg,
          );
        switch (ins.side) {
        | Right =>
          switch (Segment.edge_direction_of(Left, after)) {
          | None => Segment.edge_direction_of(Right, before)
          | d => d
          }
        | Left =>
          switch (Segment.edge_direction_of(Left, after)) {
          | None => Segment.edge_direction_of(Right, before)
          | d => d
          }
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
      ~body_shift: float=0.0,
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
    *. font_metrics.col_width
    +. body_shift;
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

/* Chips at the SAME point stack into one bubble — they insert at
   the same place, in order. Nearby-but-distinct chips stay separate
   (a comma inside the parens and an `in` outside must never read as
   one drop): the later bubble slides right just enough to clear its
   neighbor while its pole stays on the true insertion column. */
let layout_overlaps =
    (~font_metrics: FontMetrics.t, chips: list(positioned_insertion))
    : list((positioned_insertion, float)) => {
  /* rendered body width: scaled text + 4px padding each side */
  let chip_w = (c: positioned_insertion) =>
    float_of_int(delimiters_len(c.delimiters))
    *. font_metrics.col_width
    *. chip_font_scale
    +. 8.;
  let rec merge_same = (acc, rest) =>
    switch (acc, rest) {
    | (_, []) => List.rev(acc)
    | ([], [c, ...tl]) => merge_same([c], tl)
    | ([prev, ...acc_tl], [c, ...tl]) =>
      prev.row == c.row && prev.col == c.col
        ? merge_same(
            [
              {
                ...prev,
                delimiters: prev.delimiters @ c.delimiters,
              },
              ...acc_tl,
            ],
            tl,
          )
        : merge_same([c, ...acc], tl)
    };
  let rec shift = (prev: option((int, float)), cs) =>
    switch (cs) {
    | [] => []
    | [c, ...tl] =>
      let natural_left = float_of_int(c.col) *. font_metrics.col_width;
      let dx =
        switch (prev) {
        | Some((row, right)) when row == c.row && natural_left < right +. 2. =>
          right +. 2. -. natural_left
        | _ => 0.
        };
      [
        (c, dx),
        ...shift(Some((c.row, natural_left +. dx +. chip_w(c))), tl),
      ];
    };
  shift(None, merge_same([], chips));
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
      ~obligations: list(TypeObligations.t)=[],
      seg: Segment.t,
    )
    : Node.t => {
  /* Get completion result with insertions */
  let result = CanonicalCompletion.for_editor(seg);
  let insertions = result.insertions;
  /* T1 tuple-shape obligations join the same chip stream: merged
     into the site's closer chip when it exists, else fresh. They
     arrive from CachedStatics (pass-1 derivation) — with
     reification on, the live info_map no longer shows the deficit */
  let insertions =
    TypeObligations.as_insertions(~seg, ~existing=insertions, obligations);

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
      layout_overlaps(~font_metrics, sorted)
      |> List.map(((ins: positioned_insertion, body_shift)) =>
           chip_view(
             ~font_metrics,
             ~row=ins.row,
             ~col=ins.col,
             ~shape=ins.shape,
             ~caret_form,
             ~live=matches_droppable(droppable, ins.delimiters),
             ~at_caret=caret_pos == Some((ins.row, ins.col)),
             ~body_shift,
             delimiter_nodes(~font_metrics, ~on_apply, ins.delimiters),
           )
         );
    div(~attrs=[Attr.classes(["quiver-decorations"])], chips);
  };
};
