open Util;

/* QuiverLayout: the PURE placement layer for quiver chips — anchor
   resolution, coincidence-first caret following, zone bounds, and
   bubble de-collision. Lives in core (no vdom) so placement is
   headlessly testable against real editing trajectories: engine
   anchors are covered by Test_CanonicalCompletion; THIS layer is
   covered by Test_QuiverLayout. */

let chip_font_scale = 0.72;

/* An insertion with its resolved position; shape = the caret shape
   at the pin (the pole is a ghost caret). */
type positioned_insertion = {
  row: int,
  col: int,
  shape: option(Util.Direction.t),
  delimiters: list(CanonicalCompletion.delimiter_info),
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
    let point_of = (pm: Measured.measurement, side: Direction.t) =>
      switch (side) {
      | Right => (pm.last.row, pm.last.col)
      | Left => (pm.origin.row, pm.origin.col)
      };
    /* rest at the run's TRUE position (the splice ref — grout and
       whitespace included), not the content anchor: a chip for
       material landing after `..., ?` parks after the hole, not
       after the comma. Content anchor is the fallback (witnesses,
       unmeasured refs; shard refs use the tile's extent). */
    let (row, col) =
      switch (
        ins.splice
        |> Util.OptUtil.and_then(((id, _, sside)) =>
             Measured.find_by_id(id, measured)
             |> Option.map((sm: Measured.measurement) => point_of(sm, sside))
           )
      ) {
      | Some(p) => p
      | None => point_of(m, ins.side)
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
    (~col_width: float, chips: list(positioned_insertion))
    : list((positioned_insertion, float)) => {
  /* rendered body width: scaled text + 4px padding each side */
  let chip_w = (c: positioned_insertion) =>
    float_of_int(delimiters_len(c.delimiters))
    *. col_width
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
                /* same-tile delimiters stack in shard order (= before
                   in), whatever order their records arrived in */
                delimiters:
                  List.stable_sort(
                    (
                      a: CanonicalCompletion.delimiter_info,
                      b: CanonicalCompletion.delimiter_info,
                    ) =>
                      switch (a.of_shard, b.of_shard) {
                      | (Some((t1, i1)), Some((t2, i2)))
                          when Id.equal(t1, t2) =>
                        compare(i1, i2)
                      | _ => 0
                      },
                    prev.delimiters @ c.delimiters,
                  ),
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
      let natural_left = float_of_int(c.col) *. col_width;
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
