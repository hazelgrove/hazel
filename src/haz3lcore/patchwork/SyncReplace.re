open Util;
open Zipper;
open Language;

let rec move_to_start = (z: t): t => {
  switch (Move.local(ByToken, Left, z)) {
  | Some(z) => move_to_start(z)
  | None => z
  };
};

let move_to_id_anc = (z: t, (id, shard)): option(t) => {
  // this doesn't really work
  let z = z |> move_to_start;
  let rec go = (z: t): option(t) => {
    let guy =
      List.find_opt(
        (a: Ancestors.generation) =>
          fst(a).id == id && fst(a).shards |> fst |> ListUtil.hd_opt == shard,
        z.relatives.ancestors,
      )
      != None;
    guy
      ? Some(z)
      : (
        switch (Move.local(ByToken, Right, z)) {
        | Some(z) => go(z)
        | None => None
        }
      );
  };
  go(z);
};
let move_to_id =
    (d_init: Direction.t, caret_init: Caret.t, z: t, id: Id.t): option(t) => {
  let z = z |> move_to_start;
  let rec go = (z: t): option(t) => {
    let (guy, flag) =
      switch (z.relatives.siblings) {
      | (_, [p, ..._]) when d_init == Right => (Piece.id(p) == id, false)
      | ([_, ..._] as l, _) when d_init == Left => (
          Piece.id(ListUtil.last(l)) == id,
          false,
        )
      | (_, [p, ..._]) when caret_init == Outer => (
          Piece.id(p) == id,
          caret_init == Outer,
        )
      //TODO(andrew): edge case when on outer right of thing that BECOMES last in seg
      // | ([_, ..._] as l, _) when caret_init == Outer => (
      //     Piece.id(ListUtil.last(l)) == id,
      //     false,
      //   )
      | _ => (false, false)
      };
    guy
      ? flag ? Move.local(ByToken, Left, z) : Some(z)
      : (
        switch (Move.local(ByToken, Right, z)) {
        | Some(z) => go(z)
        | None => None
        }
      );
  };
  go(z);
};

let sync_replace = (z: Zipper.t, segment: Segment.t): option(Zipper.t) => {
  let overall_log = PerfLog.start("sync_replace_total");

  let num_pieces = PerfLog.Count.pieces_in_segment_deep(segment);
  let context = string_of_int(num_pieces) ++ " pieces";

  let (id_init, d_init: Direction.t) =
    switch (z.relatives.siblings) {
    | (_, [p, ..._]) => (Piece.id(p), Right)
    | ([_, ..._] as l, []) => (Piece.id(ListUtil.last(l)), Left)
    | _ => (Id.invalid, Left)
    };
  let caret_init = z.caret;
  let refractors = z.refractors;
  let ancestors = z.relatives.ancestors;
  let ancestor_ids =
    List.map(fst, ancestors)
    |> List.map((anc: Ancestor.t) =>
         (anc.id, anc.shards |> fst |> ListUtil.hd_opt)
       );

  let z =
    PerfLog.measure_with_context("unzip_segment", context, () =>
      Zipper.unzip(segment)
    );

  // Restore refractors
  let z = {
    ...z,
    refractors,
  };

  let cursor_log = PerfLog.start("cursor_repositioning");
  let z =
    switch (id_init) {
    | id =>
      switch (move_to_id(d_init, caret_init, z, id)) {
      | Some(z) => {
          ...z,
          caret: caret_init,
        }
      | None =>
        let rec go = (ancestor_ids, z): option(t) => {
          switch (ancestor_ids) {
          | [] => None
          | [ancestor_id, ...ancestor_ids] =>
            let z = z |> move_to_start;
            switch (move_to_id_anc(z, ancestor_id)) {
            | Some(z) => Some(z)
            | None => go(ancestor_ids, z)
            };
          };
        };
        switch (go(ancestor_ids, z)) {
        | Some(z) => z
        | None => z
        };
      }
    };
  PerfLog.end_(cursor_log);

  PerfLog.end_(overall_log);
  Some(z);
};

let should_send_state = (a: Action.t): bool =>
  //TODO(andrew): review actions esp project
  switch (a) {
  | SyncReplace(_)
  | UpdateRemoteCarets
  | Buffer(Clear | Accept)
  | Copy
  | Select(_)
  | Unselect(_)
  | Move(_) => false
  | Project(
      SetIndicated(_) | RemoveIndicated | SetModel(_) | Focus(_) | Escape(_) |
      SampleCursor(_),
    )
  | Probe(_)
  | Project(SetSyntax(_))
  | Reparse
  | Destruct(_)
  | Insert(_)
  | Put_down
  | Introduce
  | Paste(_)
  | Buffer(Set(_))
  | Cut
  | Dump => true
  };

let send_state = (a: Action.t, z: Zipper.t): unit =>
  if (should_send_state(a)) {
    let overall_log = PerfLog.start("send_state_total");

    let seg = z |> Zipper.zip;
    let num_pieces = PerfLog.Count.pieces_in_segment_deep(seg);
    let context = string_of_int(num_pieces) ++ " pieces";

    let flat_doc =
      PerfLog.measure_with_context("seg_to_doc", context, () =>
        FlatConvert.seg_to_doc(seg)
      );

    let doc_size = PerfLog.Count.pieces_in_doc(flat_doc);
    let send_context = string_of_int(doc_size) ++ " doc entries";

    PerfLog.measure_with_context("send_to_parent", send_context, () =>
      PatchworkComm.send_state(flat_doc)
    );

    PerfLog.end_(overall_log);
  };

/* Determines whether to send caret position update.
   Send for most actions except those that come from remote sources
   or that don't change caret position. */
let should_send_caret = (a: Action.t): bool =>
  switch (a) {
  | SyncReplace(_)
  | UpdateRemoteCarets => false /* Don't echo back remote updates */
  | _ => true /* Send for all other actions that might move caret */
  };

/* Get caret position from zipper for sending to parent.
   Returns (piece_id, shard_index, caret_offset, shape, side) where:
   - piece_id: ID of the piece we're "on" (from Indicated.for_index, skips whitespace)
   - shard_index: Which shard of the piece (from tile's shards field for fragments)
   - caret_offset: 0 for Outer, n+1 for Inner(n)
   - shape: caret shape at piece boundaries (None when inside a piece)
   - side: which edge of the piece the caret is on (Left = left edge, Right = right edge, None = inside)

   Uses Indicated.for_index to properly identify the piece, which handles:
   - Skipping Secondary pieces (whitespace) to get the actual code piece */
let get_caret_position =
    (z: Zipper.t)
    : option(
        (Id.t, option(int), int, option(Direction.t), option(Direction.t)),
      ) => {
  /* Use Indicated.for_index to properly identify the piece we're "on".
     This handles skipping Secondary pieces (whitespace) to get the actual code piece. */
  switch (Indicated.for_index(z)) {
  | None => None
  | Some((piece, direction, relation)) =>
    let piece_id = Piece.id(piece);
    /* Determine shard index based on the relation:
       - Parent: Use Indicated.shard_index which computes which shard we're adjacent to
         based on our position in the parent's children
       - Sibling: Extract from the piece's own t.shards field for fragmented tiles */
    let shard_index =
      switch (relation) {
      | Indicated.Parent => Indicated.shard_index(z)
      | Indicated.Sibling =>
        switch (piece) {
        | Tile(t) =>
          switch (t.shards) {
          | [i] => Some(i) /* Single shard fragment - return its index */
          | _ => None /* Complete tile - shard_index not needed */
          }
        | _ => None /* Grout, Secondary, Projector - no shards */
        }
      };
    let caret_offset =
      switch (z.caret) {
      | Outer => 0
      | Inner(n) => n + 1
      };
    /* Shape is only relevant at Outer positions (piece boundaries) */
    let shape =
      switch (z.caret) {
      | Inner(_) => None
      | Outer => Zipper.Caret.direction(z)
      };
    /* Side indicates which edge of the piece the caret is on.
       The `direction` from Indicated tells us which side of the caret the PIECE is on:
       - direction = Right means piece is to the right → caret is at LEFT edge
       - direction = Left means piece is to the left → caret is at RIGHT edge
       For Inner positions, side is None (inside the piece) */
    let side =
      switch (z.caret) {
      | Inner(_) => None
      | Outer =>
        switch (direction) {
        | Right => Some(Direction.Left) /* Piece to right → caret at left edge */
        | Left => Some(Direction.Right) /* Piece to left → caret at right edge */
        }
      };
    Some((piece_id, shard_index, caret_offset, shape, side));
  };
};

let send_caret = (a: Action.t, z: Zipper.t): unit =>
  if (should_send_caret(a)) {
    switch (get_caret_position(z)) {
    | Some((piece_id, shard_index, caret_offset, shape, side)) =>
      PatchworkComm.send_caret(
        piece_id,
        shard_index,
        caret_offset,
        shape,
        side,
      )
    | None => ()
    };
  };
