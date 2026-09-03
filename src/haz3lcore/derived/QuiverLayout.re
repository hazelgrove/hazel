open Util;

/* QuiverLayout: the PURE placement layer for quiver chips — anchor
   resolution and bubble de-collision. Lives in core (no vdom) so
   placement is headlessly testable against real editing trajectories:
   engine anchors are covered by Test_CanonicalCompletion; THIS layer
   by Test_QuiverLayout and Test_TabDispatch.

   OWNERSHIP IS NOT DECIDED HERE. Which records the caret owns (the
   bubble Tab acts on) comes in as `owned`, computed once by
   CompletionQuery.chips_among; this layer draws that list as ONE
   bubble at the caret and RESTS every other record at its anchor.
   Re-deriving ownership from measured coordinates is how the display
   and Tab drifted apart. */

let chip_font_scale = 0.72;

/* An insertion with its resolved position; shape = the caret shape
   at the pin (the pole is a ghost caret). owned = the caret's bubble
   (the only chip Tab acts on). */
type positioned_insertion = {
  row: int,
  col: int,
  shape: option(Util.Direction.t),
  owned: bool,
  delimiters: list(CanonicalCompletion.delimiter_info),
};

/* Find a piece by id along with its containing segment and index */
let rec find_piece_ctx =
        (sg: Segment.t, id: Id.t): option((Segment.t, int, Piece.t)) => {
  let rec go = (i, ps) =>
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

/* A record the caret does NOT own rests at the run's TRUE position
   (the splice ref — grout and whitespace included), not the content
   anchor: a chip for material landing after `..., ?` parks after the
   hole, not after the comma. Content anchor is the fallback
   (witnesses, unmeasured refs; shard refs use the tile's extent).
   Snap: the left content edge when it shares the pin's line. */
let rest_position =
    (
      ~seg: Segment.t,
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
    switch (find_piece_ctx(seg, ins.adjacent_id)) {
    | None =>
      Some({
        row,
        col,
        shape: None,
        owned: false,
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
      let left_edge =
        is_free(p)
          ? prev_content(i)
            |> Option.map(q => Measured.find_by_id(Piece.id(q), measured))
            |> Option.join
            |> Option.map((qm: Measured.measurement) =>
                 (qm.last.row, qm.last.col)
               )
          : None;
      let (row, col) =
        switch (left_edge) {
        | Some((lr, lc)) when lr == row => (row, min(lc, col))
        | _ => (row, col)
        };
      /* ghost-caret shape at the pin: the shared-nib facing between
         the pieces around the insertion point. A side-Right insertion
         reads the right neighborhood first (mirroring
         Siblings.direction_between); a side-Left one (junction: the
         material lands against the content on its LEFT) reads the
         left neighborhood first — the chevron faces the content the
         pin docks to. */
      let shape = {
        let (before, after) =
          Util.ListUtil.split_n(
            switch (ins.side) {
            | Right => i + 1
            | Left => i
            },
            sg,
          );
        switch (Segment.edge_direction_of(Left, after)) {
        | None => Segment.edge_direction_of(Right, before)
        | d => d
        };
      };
      Some({
        row,
        col,
        shape,
        owned: false,
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
   the same place, in order; the caret's OWNED bubble leads such a
   merge (Tab acts on its first delimiter). Nearby-but-distinct chips
   stay separate (a comma inside the parens and an `in` outside must
   never read as one drop): the later bubble slides right just enough
   to clear its neighbor while its pole stays on the true insertion
   column. */
let layout_overlaps =
    (~col_width: float, chips: list(positioned_insertion))
    : list((positioned_insertion, float)) => {
  /* rendered body width: scaled text + 4px padding each side */
  let chip_w = (c: positioned_insertion) =>
    float_of_int(delimiters_len(c.delimiters))
    *. col_width
    *. chip_font_scale
    +. 8.;
  let stack = (a: positioned_insertion, b: positioned_insertion) => {
    let (first, second) = b.owned && !a.owned ? (b, a) : (a, b);
    {
      ...first,
      owned: a.owned || b.owned,
      /* same-tile delimiters stack in shard order (= before in),
         whatever order their records arrived in */
      delimiters:
        List.stable_sort(
          (
            x: CanonicalCompletion.delimiter_info,
            y: CanonicalCompletion.delimiter_info,
          ) =>
            switch (x.of_shard, y.of_shard) {
            | (Some((t1, i1)), Some((t2, i2))) when Id.equal(t1, t2) =>
              compare(i1, i2)
            | _ => 0
            },
          first.delimiters @ second.delimiters,
        ),
    };
  };
  let rec merge_same = (acc, rest) =>
    switch (acc, rest) {
    | (_, []) => List.rev(acc)
    | ([], [c, ...tl]) => merge_same([c], tl)
    | ([prev, ...acc_tl], [c, ...tl]) =>
      prev.row == c.row && prev.col == c.col
        ? merge_same([stack(prev, c), ...acc_tl], tl)
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

/* The bubble list the view draws: the owned records as one bubble at
   the caret, every other record resting at its anchor, sorted by
   position, same-point stacks merged, neighbors slid apart. Shared
   with the tests so what they pin is what renders. */
let layout =
    (
      ~measured: Measured.t,
      ~col_width: float,
      ~caret_pos: option((int, int)),
      ~owned: list(CanonicalCompletion.insertion),
      ~seg: Segment.t,
      insertions: list(CanonicalCompletion.insertion),
    )
    : list((positioned_insertion, float)) => {
  /* the owned list is drawn from this same stream (physical identity
     first; (anchor, side) as the structural fallback) */
  let is_owned = (ins: CanonicalCompletion.insertion) =>
    List.exists(
      (o: CanonicalCompletion.insertion) =>
        o === ins
        || Id.equal(o.adjacent_id, ins.adjacent_id)
        && o.side == ins.side,
      owned,
    );
  let resting =
    insertions
    |> List.filter(ins => !is_owned(ins))
    |> List.filter_map(rest_position(~seg, measured));
  let caret_bubble =
    switch (caret_pos, owned) {
    | (Some((row, col)), [_, ..._]) => [
        {
          row,
          col,
          shape: None,
          owned: true,
          delimiters:
            List.concat_map(
              (ins: CanonicalCompletion.insertion) => ins.delimiters,
              owned,
            ),
        },
      ]
    | _ => []
    };
  let sorted =
    List.stable_sort(
      (a: positioned_insertion, b: positioned_insertion) =>
        compare((a.row, a.col), (b.row, b.col)),
      caret_bubble @ resting,
    );
  layout_overlaps(~col_width, sorted);
};
