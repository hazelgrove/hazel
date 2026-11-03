open Language;

/* THE display caret. The caret is computed against the DISPLAYED
   segment, not the raw zipper, because the display projects the
   reified artifact: a witness replaces the user's partial token with
   a full completed shard (`i` -> the `in` shard, styled sub-token),
   so the zipper's representative piece (the partial `i`) is not in
   the display's measured map. This is the ONE named home for that
   translation:

   - if the caret's representative piece is a REPLACED WITNESS token,
     the caret lands at the reified shard's origin PLUS typed_len (the
     boundary between the typed prefix and the ghost remainder) — the
     same column the partial token's right edge occupied;
   - otherwise the caret is id-anchored exactly as Zipper.Caret.point
     computes it, since every other display piece keeps its id.

   INVARIANT (asserted here): a display ghost never appears strictly
   BEFORE the caret. Under it, raw-coords == display-coords for the
   caret — there is no bidirectional position algebra; a click or
   selection into a ghost keeps the dismiss-then-resolve path
   unchanged. */

/* the id of the piece the caret is anchored on (left neighbor, else
   right) — the same choice Zipper.base_point makes */
let representative_id = (z: Zipper.t): option(Id.t) =>
  Zipper.representative_piece(z) |> Option.map(((p, _)) => Piece.id(p));

let point =
    (
      ~caret_witnesses: list((Id.t, (Id.t, int, int))),
      measured: Measured.t,
      z: Zipper.t,
    )
    : Util.Point.t => {
  let witness_point = () =>
    switch (representative_id(z)) {
    | None => None
    | Some(pid) =>
      switch (List.assoc_opt(pid, caret_witnesses)) {
      | None => None
      | Some((tid, i, typed_len)) =>
        /* the reified shard stands where the partial token sat: land
           at its origin + typed_len (the typed/ghost boundary) */
        switch (Id.Map.find_opt(tid, measured.tiles)) {
        | Some(shards) =>
          switch (List.assoc_opt(i, shards)) {
          | Some(m: Measured.measurement) =>
            Some(
              Util.Point.{
                row: m.origin.row,
                col: m.origin.col + typed_len,
              },
            )
          | None => None
          }
        | None => None
        }
      }
    };
  switch (witness_point()) {
  | Some(p) => p
  | None => Zipper.Caret.point(measured, z)
  };
};

/* ASSERT the no-pre-caret invariant: no ghost-marked atom's reading
   position precedes the caret. Callers in a checked build can run
   this over the display; it returns true when the invariant holds.
   Cheap structural check kept out of the hot render path. */
let no_ghost_before_caret =
    (
      ~caret_witnesses: list((Id.t, (Id.t, int, int))),
      ~ghost_marks: list((Id.t, option(int))),
      measured: Measured.t,
      seg: Segment.t,
      z: Zipper.t,
    )
    : bool => {
  let caret = point(~caret_witnesses, measured, z);
  let marked = (id, sh) =>
    List.exists(
      ((mid, msh): (Id.t, option(int))) => Id.equal(mid, id) && msh == sh,
      ghost_marks,
    );
  let before = (m: Measured.measurement) =>
    Util.Point.compare(m.last, caret) <= 0
    && Util.Point.compare(m.origin, caret) < 0;
  let rec ok = (sg: Segment.t): bool =>
    List.for_all(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          let shards = Measured.find_shards(~msg="DisplayCaret", t, measured);
          List.for_all(
            i => !marked(t.id, Some(i)) || !before(List.assoc(i, shards)),
            t.shards,
          )
          && List.for_all(ok, t.children);
        | Grout(g) =>
          !marked(g.id, None) || !before(Measured.find_g(g, measured))
        | Secondary(w) =>
          !marked(w.id, None) || !before(Measured.find_w(w, measured))
        | Projector(_) => true
        },
      sg,
    );
  ok(seg);
};
