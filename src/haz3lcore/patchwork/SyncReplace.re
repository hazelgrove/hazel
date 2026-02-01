open Util;
open Zipper;
open Language;

let rec move_to_start = (z: t): t => {
  switch (Move.local(ByToken, Left, z)) {
  | Some(z) => move_to_start(z)
  | None => z
  };
};

/* Collect IDs of pieces that are "same-segment predecessors" of the cursor.
   These are pieces in fst(siblings) at current level, plus pieces in
   fst(generation.siblings) for each ancestor level. NOT pieces from
   ancestor.children (those are in different children, not same segment). */
let collect_predecessor_ids = (z: t): Id.Map.t(unit) => {
  /* Pieces before cursor at current level */
  let current_preds = z.relatives.siblings |> fst |> List.map(Piece.id);
  /* Pieces before each ancestor at their respective levels */
  let ancestor_preds =
    z.relatives.ancestors
    |> List.concat_map(((_, sibs): Ancestors.generation) =>
         sibs |> fst |> List.map(Piece.id)
       );
  let all_preds = current_preds @ ancestor_preds;
  all_preds |> List.to_seq |> Seq.map(id => (id, ())) |> Id.Map.of_seq;
};

/* Find position based on same-segment predecessors. Scans left-to-right,
   tracking the rightmost position where the piece to our left is a predecessor.
   Returns that position if found, None otherwise. */
let move_to_predecessor = (z: t, predecessor_ids: Id.Map.t(unit)): option(t) => {
  let z = z |> move_to_start;
  let rec go = (z: t, best: option(t)): option(t) => {
    /* Check if piece to our left is a predecessor */
    let is_pred =
      switch (z.relatives.siblings |> fst |> ListUtil.last_opt) {
      | Some(p) => Id.Map.mem(Piece.id(p), predecessor_ids)
      | None => false
      };
    /* Rightward bias: always update best when we see a predecessor,
       so we end up with the rightmost one */
    let best = is_pred ? Some(z) : best;
    switch (Move.local(ByToken, Right, z)) {
    | Some(z) => go(z, best)
    | None => best
    };
  };
  go(z, None);
};

let move_to_id_anc = (z: t, (id, shard, child_idx)): option(t) => {
  /* Find a position where the target ancestor is in the ancestor stack,
     preferring the correct child index. Falls back to the highest child
     index <= target if exact match not found (e.g., if child was deleted). */
  let z = z |> move_to_start;
  let rec go = (z: t, best: option(t)): option(t) => {
    let match_opt =
      List.find_opt(
        (a: Ancestors.generation) =>
          fst(a).id == id && fst(a).shards |> fst |> ListUtil.hd_opt == shard,
        z.relatives.ancestors,
      );
    let (best, found_exact) =
      switch (match_opt) {
      | Some((anc, _)) =>
        let current_child = List.length(fst(anc.children));
        if (current_child == child_idx) {
          (
            /* Exact match - we're done */
            Some(z),
            true,
          );
        } else if (current_child < child_idx) {
          /* We're in a lower child - remember this as fallback if it's
             the best we've seen (highest child index not exceeding target) */
          let dominated =
            switch (best) {
            | None => true
            | Some(best_z) =>
              switch (
                List.find_opt(
                  (a: Ancestors.generation) => fst(a).id == id,
                  best_z.relatives.ancestors,
                )
              ) {
              | Some((best_anc, _)) =>
                current_child > List.length(fst(best_anc.children))
              | None => true
              }
            };
          (dominated ? Some(z) : best, false);
        } else {
          (
            /* We're past the target child - keep the best we found */
            best,
            false,
          );
        };
      | None => (best, false)
      };
    found_exact
      ? best
      : (
        switch (Move.local(ByToken, Right, z)) {
        | Some(z) => go(z, best)
        | None => best
        }
      );
  };
  go(z, None);
};
let move_to_id =
    (d_init: Direction.t, caret_init: Caret.t, z: t, id: Id.t): option(t) => {
  //let z = z |> move_to_start;
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

/* Reposition cursor after sync, using layered fallback strategy:
   1. Try to find the exact piece the cursor was on (by ID)
   2. If deleted, find the rightmost "same-segment predecessor" that survives.
      These are pieces that were in the same segment as the cursor (siblings)
      at each level of the zipper, giving us the closest lexical position.
   3. If no predecessor survives (e.g., cursor was at start of a child),
      fall back to ancestor-based positioning which preserves structural
      position (which child of which ancestor we were in). */
let reposition_cursor =
    (
      z: t,
      ~predecessor_ids: Id.Map.t(unit),
      ~ancestor_ids: list((Id.t, option(int), int)),
      ~id_init: Id.t,
      ~d_init: Direction.t,
      ~caret_init: Caret.t,
    )
    : Zipper.t =>
  switch (id_init) {
  | id =>
    switch (move_to_id(d_init, caret_init, z, id)) {
    | Some(z) => {
        ...z,
        caret: caret_init,
      }
    | None =>
      let z = {
        ...z,
        caret: Outer,
      };
      switch (move_to_predecessor(z, predecessor_ids)) {
      | Some(z) => z
      | None =>
        let rec go = (ancestor_ids, z): option(t) => {
          switch (ancestor_ids) {
          | [] => None
          | [ancestor_id, ...ancestor_ids] =>
            switch (move_to_id_anc(z, ancestor_id)) {
            | Some(z) => Some(z)
            | None => go(ancestor_ids, z)
            }
          };
        };
        switch (go(ancestor_ids, z)) {
        | Some(z) => z
        | None => z
        };
      };
    }
  };

let sync_replace = (z: Zipper.t, delta_doc: FlatConvert.Doc.t): Zipper.t => {
  let overall_log = PerfLog.start("sync_replace_total");

  // Save cursor position info
  let (id_init, d_init: Direction.t) =
    switch (z.relatives.siblings) {
    | (_, [p, ..._]) => (Piece.id(p), Right)
    | ([_, ..._] as l, []) => (Piece.id(ListUtil.last(l)), Left)
    | _ => (Id.invalid, Left)
    };
  let caret_init = z.caret;
  let refractors = z.refractors;
  let predecessor_ids = collect_predecessor_ids(z);
  let ancestors = z.relatives.ancestors;
  let ancestor_ids =
    ancestors
    |> List.map(((anc: Ancestor.t, _sibs)) =>
         (
           anc.id,
           anc.shards |> fst |> ListUtil.hd_opt,
           List.length(fst(anc.children)),
         )
       );

  // Flatten current state to doc (unselect first to reassemble any fragments)
  let current_seg =
    PerfLog.measure("zip_current", () => Zipper.unselect_and_zip(z));

  let current_doc =
    PerfLog.measure("seg_to_doc_current", () =>
      FlatConvert.seg_to_doc(current_seg)
    );

  // Merge delta with current state (delta overrides current)
  let merged_doc =
    PerfLog.measure("merge_docs", () =>
      FlatConvert.Doc.union(
        (_, _, delta_piece) => Some(delta_piece),
        current_doc,
        delta_doc,
      )
    );

  // let num_merged = FlatConvert.Doc.cardinal(merged_doc);
  // Js_of_ocaml.Firebug.console##log(
  //   Js_of_ocaml.Js.string(
  //     "[SYNC] Merged doc has " ++ string_of_int(num_merged) ++ " pieces",
  //   ),
  // );

  // Unflatten merged doc to segment
  let new_seg =
    PerfLog.measure("doc_to_seg_merged", () =>
      FlatConvert.doc_to_seg(merged_doc)
    );

  //let num_pieces = PerfLog.Count.pieces_in_segment_deep(new_seg);
  let num_pieces = (-1);
  let context = string_of_int(num_pieces) ++ " pieces";

  let z =
    PerfLog.measure_with_context("unzip_segment", context, () =>
      Zipper.unzip(~direction=Left, new_seg)
    );

  // Restore refractors
  let z = {
    ...z,
    refractors,
  };

  let cursor_log = PerfLog.start("cursor_repositioning");
  let z =
    reposition_cursor(
      z,
      ~predecessor_ids,
      ~ancestor_ids,
      ~id_init,
      ~d_init,
      ~caret_init,
    );
  PerfLog.end_(cursor_log);
  PerfLog.end_(overall_log);
  z;
};

let should_send_state = (a: Action.t): bool =>
  //TODO(andrew): Doing this here misses undo/redo actions...
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

/* Core state-sending logic without action check.
   Computes delta between old and new zipper and sends to parent. */
let send_state_delta = (old_z: Zipper.t, new_z: Zipper.t): unit => {
  let overall_log = PerfLog.start("send_state_total");

  // Convert old state to flat doc (unselect first to reassemble any fragments)
  let old_seg = Zipper.unselect_and_zip(old_z);
  let old_flat_doc =
    PerfLog.measure("old_seg_to_doc", () => FlatConvert.seg_to_doc(old_seg));

  // Convert new state to flat doc (unselect first to reassemble any fragments)
  let new_seg = Zipper.unselect_and_zip(new_z);
  //let num_pieces = PerfLog.Count.pieces_in_segment_deep(new_seg);
  let num_pieces = (-1);
  let context = string_of_int(num_pieces) ++ " pieces";

  let new_flat_doc =
    PerfLog.measure_with_context("seg_to_doc", context, () =>
      FlatConvert.seg_to_doc(new_seg)
    );

  // Send both docs to compute delta and send
  PerfLog.measure("send_to_parent", () =>
    PatchworkComm.send_state(old_flat_doc, new_flat_doc)
  );

  PerfLog.end_(overall_log);
};

let send_state = (a: Action.t, old_z: Zipper.t, new_z: Zipper.t): unit =>
  if (should_send_state(a)) {
    send_state_delta(old_z, new_z);
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
  switch (Indicated.piece'(~no_ws=false, ~ign=_ => false, z)) {
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

/* Core caret-sending logic without action check.
   Extracts caret position from zipper and sends to parent. */
let send_caret_position = (z: Zipper.t): unit =>
  switch (get_caret_position(z)) {
  | Some((piece_id, shard_index, caret_offset, shape, side)) =>
    PatchworkComm.send_caret(piece_id, shard_index, caret_offset, shape, side)
  | None => ()
  };

let send_caret = (a: Action.t, z: Zipper.t): unit =>
  if (should_send_caret(a)) {
    send_caret_position(z);
  };

/* Sync state and caret to Patchwork parent after an edit.
   Only call this when running inside Patchwork iframe. */
let sync_to_parent =
    (~action: Action.t, ~old_zipper: Zipper.t, ~new_zipper: Zipper.t): unit => {
  send_state(action, old_zipper, new_zipper);
  send_caret(action, new_zipper);
};

/* Sync state and caret after undo/redo.
   Unlike sync_to_parent, this doesn't check action type since undo/redo
   are handled at a higher level (History) and don't go through Action.t. */
let sync_for_undo = (~old_zipper: Zipper.t, ~new_zipper: Zipper.t): unit => {
  send_state_delta(old_zipper, new_zipper);
  send_caret_position(new_zipper);
};
