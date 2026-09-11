open Util;

/* Commit the virtual completion to the buffer: rebuild the zipper
 * around the materialized segment, caret re-anchored by neighbor id. */

let rezip = (z: Zipper.t, seg: Segment.t): Zipper.t => {
  let anchor = Zipper.representative_piece(z);
  let z0: Zipper.t = {
    selection: Selection.mk([]),
    relatives: {
      siblings: ([], seg),
      ancestors: [],
    },
    caret: Outer,
    refractors: z.refractors,
  };
  switch (anchor) {
  | Some((p, d)) =>
    switch (Move.jump_to_side_of_id(Direction.toggle(d), z0, Piece.id(p))) {
    | Some(z) => z
    | None => z0
    }
  | None => z0
  };
};

let prepared = (z: Zipper.t): Segment.t =>
  z
  |> Zipper.clear_unparsed_buffer
  |> Zipper.unselect_and_zip(~erase_buffer=true);

/* Materialized shards splice in without lexing, so a synthesized
   delimiter can land glued to its neighbor (end|in re-lexes as one
   token). SpaceNormalize inserts the space the lexer would have
   forced; it only fires at glom junctions, which parsed user
   material cannot contain. */
let all = (z: Zipper.t, ~root: Sort.t): Zipper.t =>
  rezip(
    z,
    CanonicalCompletion.materialize_all(~sort=root, prepared(z))
    |> SpaceNormalize.go,
  );

let one = (z: Zipper.t, ~root: Sort.t, id: Id.t): option(Zipper.t) =>
  CanonicalCompletion.materialize_one(~sort=root, prepared(z), id)
  |> Option.map(SpaceNormalize.go)
  |> Option.map(rezip(z));
