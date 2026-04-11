open Util;
open Zipper;
open Language;

let sync_replace = (z: Zipper.t, delta_doc: FlatConvert.Doc.t): Zipper.t => {
  let overall_log = PerfLog.start("sync_replace_total");

  let z =
    CaretPreserving.transform(
      z,
      current_seg => {
        let current_doc =
          PerfLog.measure("seg_to_doc_current", () =>
            FlatConvert.seg_to_doc(current_seg)
          );

        let merged_doc =
          PerfLog.measure("merge_docs", () =>
            FlatConvert.Doc.union(
              (_, _, delta_piece) => Some(delta_piece),
              current_doc,
              delta_doc,
            )
          );

        let new_seg =
          PerfLog.measure("doc_to_seg_merged", () =>
            FlatConvert.doc_to_seg(merged_doc)
          );

        SegmentValidator.validate_all(new_seg);
        new_seg;
      },
    );

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
      EscapeToLineEnd(_) |
      SampleFocus(_),
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
  | Structural(_)
  | ToggleLineComment
  | Dump => true
  };

/* Core state-sending logic without action check.
   Computes delta between old and new zipper and sends to parent. */
let send_state_delta = (old_z: Zipper.t, new_z: Zipper.t): unit => {
  let overall_log = PerfLog.start("send_state_total");

  let old_seg = Zipper.unselect_and_zip(old_z);
  let old_flat_doc =
    PerfLog.measure("old_seg_to_doc", () => FlatConvert.seg_to_doc(old_seg));

  let new_seg = Zipper.unselect_and_zip(new_z);
  let num_pieces = (-1);
  let context = string_of_int(num_pieces) ++ " pieces";

  let new_flat_doc =
    PerfLog.measure_with_context("seg_to_doc", context, () =>
      FlatConvert.seg_to_doc(new_seg)
    );

  PerfLog.measure("send_to_parent", () =>
    PatchworkComm.send_state(old_flat_doc, new_flat_doc)
  );

  PerfLog.end_(overall_log);
};

let send_state = (a: Action.t, old_z: Zipper.t, new_z: Zipper.t): unit =>
  if (should_send_state(a)) {
    send_state_delta(old_z, new_z);
  };

/* Determines whether to send caret position update. */
let should_send_caret = (a: Action.t): bool =>
  switch (a) {
  | SyncReplace(_)
  | UpdateRemoteCarets => false
  | _ => true
  };

/* Get caret position from zipper for sending to parent.
   Returns (piece_id, shard_index, caret_offset, shape, side). */
let get_caret_position =
    (z: Zipper.t)
    : option(
        (Id.t, option(int), int, option(Direction.t), option(Direction.t)),
      ) =>
  switch (Indicated.indicated(~no_ws=false, ~ign=_ => false, z)) {
  | None => None
  | Some({piece, side: direction, relation}) =>
    let piece_id = Piece.id(piece);
    let shard_index =
      switch (relation) {
      | Indicated.Parent => Indicated.shard_index(z)
      | Indicated.Sibling =>
        switch (piece) {
        | Tile(t) =>
          switch (t.shards) {
          | [i] => Some(i)
          | _ => None
          }
        | _ => None
        }
      };
    let caret_offset =
      switch (z.caret) {
      | Outer => 0
      | Inner(n) => n + 1
      };
    let shape =
      switch (z.caret) {
      | Inner(_) => None
      | Outer => Zipper.Caret.direction(z)
      };
    let side =
      switch (z.caret) {
      | Inner(_) => None
      | Outer =>
        switch (direction) {
        | Right => Some(Direction.Left)
        | Left => Some(Direction.Right)
        }
      };
    Some((piece_id, shard_index, caret_offset, shape, side));
  };

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

/* Sync state and caret to Patchwork parent after an edit. */
let sync_to_parent =
    (~action: Action.t, ~old_zipper: Zipper.t, ~new_zipper: Zipper.t): unit => {
  send_state(action, old_zipper, new_zipper);
  send_caret(action, new_zipper);
};

/* Sync state and caret after undo/redo. */
let sync_for_undo = (~old_zipper: Zipper.t, ~new_zipper: Zipper.t): unit => {
  send_state_delta(old_zipper, new_zipper);
  send_caret_position(new_zipper);
};
