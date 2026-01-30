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
  let (id_init, d_init: Direction.t) =
    switch (z.relatives.siblings) {
    | (_, [p, ..._]) => (Piece.id(p), Right)
    | ([_, ..._] as l, []) => (Piece.id(ListUtil.last(l)), Left)
    | _ => (Id.invalid, Left)
    };
  let caret_init = z.caret;
  let ancestors = z.relatives.ancestors;
  let ancestor_ids =
    List.map(fst, ancestors)
    |> List.map((anc: Ancestor.t) =>
         (anc.id, anc.shards |> fst |> ListUtil.hd_opt)
       );
  // print_endline(
  //   "ancestor_ids: "
  //   ++ String.concat(", ", List.map(x=>x|>fst|>Id.to_string, ancestor_ids)),
  // );
  let z = Zipper.unzip(segment);

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
            // print_endline(
            //   "tying to move to ancestor_id: " ++ Id.to_string(ancestor_id),
            // );
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
    let flat_doc = FlatConvert.seg_to_doc(z |> Zipper.zip);
    PatchworkComm.send_state(flat_doc);
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
   Returns (piece_id, caret_offset, shape) where:
   - piece_id: ID of the piece you're "on" (first of right siblings, or last of left at end)
   - caret_offset: 0 for Outer, n+1 for Inner(n)
   - shape: caret shape at piece boundaries (None when inside a piece) */
let get_caret_position =
    (z: Zipper.t): option((Id.t, int, option(Direction.t))) => {
  /* Get the piece we're "on" - first of right siblings, or last of left at segment end */
  let piece_opt =
    switch (z.relatives.siblings) {
    | (_, [piece, ..._]) => Some(piece)
    | ([_, ..._] as left, []) => Some(ListUtil.last(left))
    | _ => None
    };
  switch (piece_opt) {
  | Some(piece) =>
    let piece_id = Piece.id(piece);
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
    Some((piece_id, caret_offset, shape));
  | None => None
  };
};

let send_caret = (a: Action.t, z: Zipper.t): unit =>
  if (should_send_caret(a)) {
    switch (get_caret_position(z)) {
    | Some((piece_id, caret_offset, shape)) =>
      PatchworkComm.send_caret(piece_id, caret_offset, shape)
    | None => ()
    };
  };
