open Util;
open Zipper;
open Language;

/* Information needed to restore selection after transform.
   We track the anchor (far endpoint from caret) by its piece ID and shards. */
type selection_anchor_info = {
  focus: Direction.t,
  anchor_id: Id.t,
  anchor_shards: option(list(int)) /* Some for Tile, None for others */,
};

/* Selection anchor for restoration after transform. None for empty or
   Buffer selections. id_init captures the piece just right of the
   selection, so transform + reposition normally lands the cursor at
   the RIGHT edge of the original selection — we track the LEFTMOST
   selected piece and grow leftward to find it. When the selection
   abuts buffer end there is no right piece: reposition falls back to
   the LEFT edge, leftward growth can't reach the anchor, and
   restore_selection safe-fails to unselect. */
let get_selection_anchor_info = (z: Zipper.t): option(selection_anchor_info) =>
  switch (z.selection) {
  | {content: [], _} => None
  | {mode: Buffer(_), _} => None
  | {focus, content: [first_piece, ..._], mode: Normal, _} =>
    let anchor_id = Piece.id(first_piece);
    let anchor_shards =
      switch (first_piece) {
      | Tile(t) => Some(t.shards)
      | Grout(_)
      | Secondary(_)
      | Projector(_) => None
      };
    Some({
      focus,
      anchor_id,
      anchor_shards,
    });
  };

/* Tiles must match both ID and shards: multi-delimiter forms may be
   fragmented into several pieces sharing one ID. */
let piece_matches_anchor =
    (p: Piece.t, anchor_id: Id.t, anchor_shards: option(list(int))): bool =>
  Piece.id(p) == anchor_id
  && (
    switch (p, anchor_shards) {
    | (Tile(t), Some(shards)) => t.shards == shards
    | (_, None) => true
    | (_, Some(_)) => false
    }
  );

/* Get the piece at the grow-direction end of the selection. */
let get_selection_edge_piece =
    (grow_direction: Direction.t, z: Zipper.t): option(Piece.t) =>
  switch (grow_direction, z.selection.content) {
  | (_, []) => None
  | (Left, [p, ..._]) => Some(p)
  | (Right, content) => ListUtil.last_opt(content)
  };

/* Grow leftward from the cursor (at the selection's former right edge;
   see get_selection_anchor_info) until the anchor piece is reached. */
let restore_selection =
    (z: Zipper.t, anchor_info: selection_anchor_info): Zipper.t => {
  let {focus: focus_init, anchor_id, anchor_shards} = anchor_info;
  let grow_direction = Direction.Left;
  let z = Zipper.set_focus(z, grow_direction);

  let rec grow_to_anchor = (z: Zipper.t): Zipper.t =>
    switch (Zipper.select(grow_direction, z)) {
    | None => Zipper.unselect(z)
    | Some(z) =>
      switch (get_selection_edge_piece(grow_direction, z)) {
      | Some(p) when piece_matches_anchor(p, anchor_id, anchor_shards) =>
        Zipper.set_focus(z, focus_init)
      | _ => grow_to_anchor(z)
      }
    };

  grow_to_anchor(z);
};

let rec move_to_start = (z: t): t =>
  switch (Move.local(ByToken, Left, z)) {
  | Some(z) => move_to_start(z)
  | None => z
  };

/* IDs of the cursor's "same-segment predecessors": left siblings at the
   current level and at each ancestor level. */
let collect_predecessor_ids = (z: t): Id.Map.t(unit) => {
  let current_preds = z.relatives.siblings |> fst |> List.map(Piece.id);
  let ancestor_preds =
    z.relatives.ancestors
    |> List.concat_map(((_, sibs): Ancestors.generation) =>
         sibs |> fst |> List.map(Piece.id)
       );
  current_preds
  @ ancestor_preds
  |> List.to_seq
  |> Seq.map(id => (id, ()))
  |> Id.Map.of_seq;
};

/* Find position based on same-segment predecessors. Scans left-to-right,
   tracking the rightmost position where the piece to our left is a predecessor. */
let move_to_predecessor = (z: t, predecessor_ids: Id.Map.t(unit)): option(t) => {
  let z = move_to_start(z);
  let rec go = (z: t, best: option(t)): option(t) => {
    let is_pred =
      switch (z.relatives.siblings |> fst |> ListUtil.last_opt) {
      | Some(p) => Id.Map.mem(Piece.id(p), predecessor_ids)
      | None => false
      };
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
     index <= target if exact match not found. */
  let z = move_to_start(z);
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
          (Some(z), true);
        } else if (current_child < child_idx) {
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
          (best, false);
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
  let rec go = (z: t): option(t) => {
    let (found, needs_adjust) =
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
      | _ => (false, false)
      };
    found
      ? needs_adjust ? Move.local(ByToken, Left, z) : Some(z)
      : (
        switch (Move.local(ByToken, Right, z)) {
        | Some(z) => go(z)
        | None => None
        }
      );
  };
  go(z);
};

/* Reposition cursor after a transform, using layered fallback strategy:
   1. Try to find the exact piece the cursor was on (by ID)
   2. If deleted, find the rightmost "same-segment predecessor" that survives
   3. Fall back to ancestor-based positioning

   Returns (zipper, found_exact) where found_exact is true only if case 1 succeeded. */
let reposition_cursor =
    (
      z: t,
      ~predecessor_ids: Id.Map.t(unit),
      ~ancestor_ids: list((Id.t, option(int), int)),
      ~id_init: Id.t,
      ~d_init: Direction.t,
      ~caret_init: Caret.t,
    )
    : (Zipper.t, bool) =>
  switch (move_to_id(d_init, caret_init, z, id_init)) {
  | Some(z) => (
      {
        ...z,
        caret: caret_init,
      },
      true,
    )
  | None =>
    let z = {
      ...z,
      caret: Outer,
    };
    let z =
      switch (move_to_predecessor(z, predecessor_ids)) {
      | Some(z) => z
      | None =>
        let rec go = (ancestor_ids, z): option(t) =>
          switch (ancestor_ids) {
          | [] => None
          | [ancestor_id, ...ancestor_ids] =>
            switch (move_to_id_anc(z, ancestor_id)) {
            | Some(z) => Some(z)
            | None => go(ancestor_ids, z)
            }
          };
        switch (go(ancestor_ids, z)) {
        | Some(z) => z
        | None => z
        };
      };
    (z, false);
  };

/* Apply a segment transformation while preserving caret position.
   Saves cursor state, unzips to segment, applies the transform,
   re-zips, and repositions the cursor using layered fallback. */
let transform = (z: Zipper.t, f: Segment.t => Segment.t): Zipper.t => {
  let (id_init, d_init: Direction.t) =
    switch (z.relatives.siblings, z.selection.content) {
    | ((_, [p, ..._]), _) => (Piece.id(p), Right)
    | (([_, ..._] as l, []), _) => (Piece.id(ListUtil.last(l)), Left)
    | (([], []), [_, ..._] as sel) =>
      switch (z.selection.focus) {
      | Right => (Piece.id(ListUtil.last(sel)), Left)
      | Left => (Piece.id(List.hd(sel)), Right)
      }
    | (([], []), []) => (Id.invalid, Left)
    };
  let caret_init = z.caret;
  let refractors = z.refractors;
  let predecessor_ids = collect_predecessor_ids(z);
  let ancestor_ids =
    z.relatives.ancestors
    |> List.map(((anc: Ancestor.t, _sibs)) =>
         (
           anc.id,
           anc.shards |> fst |> ListUtil.hd_opt,
           List.length(fst(anc.children)),
         )
       );
  let selection_anchor = get_selection_anchor_info(z);

  let current_seg = Zipper.unselect_and_zip(z);
  let new_seg = f(current_seg);
  let z = Zipper.unzip(~direction=Left, new_seg);

  let z = {
    ...z,
    refractors,
  };

  let (z, found_exact) =
    reposition_cursor(
      z,
      ~predecessor_ids,
      ~ancestor_ids,
      ~id_init,
      ~d_init,
      ~caret_init,
    );

  switch (selection_anchor) {
  | Some(anchor_info) when found_exact => restore_selection(z, anchor_info)
  | _ => z
  };
};
