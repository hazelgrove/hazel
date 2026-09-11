open Util;
open Zipper;
open Language;

/* Selection restoration data: both endpoints by piece id (+ shards for
   tiles), and which EDGE of the selection reposition will park the
   cursor at — the side whose neighbor id_init tracks. Restoration
   grows from that edge toward the far endpoint. */
type endpoint = {
  id: Id.t,
  shards: option(list(int)) /* Some for Tile, None for others */,
};
type selection_anchor_info = {
  focus: Direction.t,
  first: endpoint,
  last: endpoint,
  cursor_edge: Direction.t,
};

let endpoint_of = (p: Piece.t): endpoint => {
  id: Piece.id(p),
  shards:
    switch (p) {
    | Tile(t) => Some(t.shards)
    | Grout(_)
    | Secondary(_)
    | Projector(_) => None
    },
};

/* None for empty or Buffer selections. */
let get_selection_anchor_info =
    (~cursor_edge: Direction.t, z: Zipper.t): option(selection_anchor_info) =>
  switch (z.selection) {
  | {content: [], _} => None
  | {mode: Buffer(_), _} => None
  | {focus, content: [first_piece, ..._] as content, mode: Normal, _} =>
    Some({
      focus,
      first: endpoint_of(first_piece),
      last: endpoint_of(ListUtil.last(content)),
      cursor_edge,
    })
  };

/* Tiles must match both ID and shards: multi-delimiter forms may be
   fragmented into several pieces sharing one ID. */
let piece_matches = (p: Piece.t, e: endpoint): bool =>
  Piece.id(p) == e.id
  && (
    switch (p, e.shards) {
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

/* Grow from the cursor's edge toward the far endpoint: cursor at the
   RIGHT edge grows leftward to the first piece; at the LEFT edge (a
   selection abutting buffer end has no right neighbor to track)
   grows rightward to the last piece. */
let restore_selection =
    (z: Zipper.t, anchor_info: selection_anchor_info): Zipper.t => {
  let {focus: focus_init, first, last, cursor_edge} = anchor_info;
  let (grow_direction, target) =
    switch (cursor_edge) {
    | Right => (Direction.Left, first)
    | Left => (Direction.Right, last)
    };
  let z = Zipper.set_focus(z, grow_direction);
  let rec grow_to_target = (z: Zipper.t): Zipper.t =>
    switch (Zipper.select(grow_direction, z)) {
    | None => Zipper.unselect(z)
    | Some(z) =>
      switch (get_selection_edge_piece(grow_direction, z)) {
      | Some(p) when piece_matches(p, target) =>
        Zipper.set_focus(z, focus_init)
      | _ => grow_to_target(z)
      }
    };
  grow_to_target(z);
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
  /* id_init: the piece the cursor is tracked against; d_init: which
     side of it the cursor sits; cursor_edge: for a selection, which
     edge of the selection that puts the cursor at after reposition */
  let (id_init, d_init: Direction.t, cursor_edge: Direction.t) =
    switch (z.relatives.siblings, z.selection.content) {
    | ((_, [p, ..._]), _) => (Piece.id(p), Right, Right)
    | (([_, ..._] as l, []), _) => (
        Piece.id(ListUtil.last(l)),
        Left,
        Left,
      )
    | (([], []), [_, ..._] as sel) =>
      switch (z.selection.focus) {
      | Right => (Piece.id(ListUtil.last(sel)), Left, Right)
      | Left => (Piece.id(List.hd(sel)), Right, Left)
      }
    | (([], []), []) => (Id.invalid, Left, Left)
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
  let selection_anchor = get_selection_anchor_info(~cursor_edge, z);

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
