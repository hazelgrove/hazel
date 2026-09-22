open Util;

/* PosMap — THE edit↔display position-translation home (the
   "translation layer" of the obligation-display design; see
   plans/obligation-display-design.md).

   FORWARD: an edit-side location (a boundary of a user piece) maps
   to a display Point. User pieces map through their measurements;
   the one nontrivial rule is CONSUMED-SPACE EDGE IDENTIFICATION:
   a space whose cell was borrowed by its hole measures zero wide,
   and BOTH its boundaries are identified with the hole's edges —
   the hole visually stands in for the space. Symmetric: the AFTER
   side of a space consumed by a FOLLOWING hole collapses onto the
   hole's left edge and maps to its right edge; the BEFORE side of
   a space consumed by a PRECEDING hole collapses onto the hole's
   right edge and maps to its left edge (fuzzer-found: `?>?` from
   a leading space — the walk start must draw left of the hole). In
   the aligned cases the raw measurement already coincides and the
   redirect is the identity.

   Ghost spans contribute NO edit positions — the caret only
   occupies real positions. Movement across a persisted span is
   therefore atomic by construction, and remote span churn cannot
   move the caret.

   INVERSE: display point -> nearest real edit position. This is
   implemented by the movement engine (Move.do_towards_point walks
   edit positions comparing forward points), so inverse consistency
   follows from forward correctness; a click inside a ghost span
   resolves to the nearest real position (the approved
   place-at-anchor default). The fuzzer arms the ties:
   - forward is strictly monotone along the caret walk
     (no two edit positions share a display point — the dead-stop
     class as a property);
   - inverse(forward(caret)) == caret (round trip). */

/* the consumed-space redirect: a consumed space's measurement with
   both boundaries identified with its consuming hole's edges */
let consumed_space_edges =
    (~measured: Measured.t, w: Secondary.t, m: Measured.measurement)
    : Measured.measurement =>
  switch (GroutCells.consumer_of(measured.grout_cells, w.id)) {
  | Some(gid) =>
    switch (Id.Map.find_opt(gid, measured.grout)) {
    | Some(gm) =>
      let last =
        gm.origin.row == m.last.row && gm.last.col > m.last.col
          ? gm.last : m.last;
      let origin =
        gm.origin.row == m.origin.row && gm.origin.col < m.origin.col
          ? gm.origin : m.origin;
      {
        origin,
        last,
      };
    | None => m
    }
  | None => m
  };

/* TRAILING-HOLE EDGE (P10 fill-position affinity): the line-end
   classes are the ONLY ones that add columns — the others borrow an
   existing whitespace cell, so they cannot shift what follows. Their
   column therefore pushes the right neighbour's origin (a linebreak,
   say) one cell past the caret's true position, and mapping a caret
   through that neighbour draws it on the hole's RIGHT — where nothing
   exists. The hole marks where content will land, so the caret sits
   BEFORE it: redirect to the hole's ORIGIN — which for LineEndPadded
   is the blank pad cell, i.e. immediately after the anchor token, so
   the caret lands before the pad (`then¦ ?`) and a typed char appears
   exactly where the caret was drawn. Geometric (the hole's `last`
   meets the neighbour's `origin` on one row), so it needs no
   structural traversal and applies to every consumer of this map. */
let trailing_hole_origin = (~measured: Measured.t, origin: Point.t): Point.t =>
  Id.Map.fold(
    (gid, gm: Measured.measurement, acc: Point.t) =>
      switch (GroutCells.cls_of(measured.grout_cells, gid)) {
      | Some(c)
          when
            GroutCells.is_line_end(c)
            && gm.last.row == acc.row
            && gm.last.col == acc.col =>
        gm.origin
      | _ => acc
      },
    measured.grout,
    origin,
  );

/* measurement lookup that tolerates display-side replacement (a
   promise-render witness swaps a partial token for its completed
   shard, so the edit id can be absent from `measured`) */
let safe_measurement =
    (~measured: Measured.t, p: Piece.t): option(Measured.measurement) =>
  switch (Measured.find_p(~msg="PosMap", p, measured)) {
  | m => Some(m)
  | exception _ => Measured.find_by_id(Piece.id(p), measured)
  };

/* the display point of a user piece's boundary: ~side=Left is the
   point AFTER the piece (its last), ~side=Right the point BEFORE it
   (its origin); consumed-space edges identified per the rule above */
let point_of_side =
    (~measured: Measured.t, ~side: Direction.t, p: Piece.t): option(Point.t) => {
  let redirect = (m: Measured.measurement): Measured.measurement =>
    switch (p) {
    | Piece.Secondary(w) when Secondary.is_space(w) =>
      consumed_space_edges(~measured, w, m)
    | _ => m
    };
  safe_measurement(~measured, p)
  |> Option.map(m =>
       switch (side) {
       | Direction.Left => redirect(m).last
       | Direction.Right =>
         trailing_hole_origin(~measured, redirect(m).origin)
       }
     );
};
