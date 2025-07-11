open Util;
include ZipperBase;

[@deriving (show({with_path: false}), sexp, yojson, eq, ord)]
type id = {
  uuid: Id.t,
  index: int,
};

module IdMap =
  MapUtil.Make({
    [@deriving (show({with_path: false}), sexp, yojson, eq, ord)]
    type t = id;
    let compare = compare;
  });

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary(Secondary.t)
and tile = {
  // invariants:
  // - length(mold.in_) + 1 == length(label)
  // - length(shards) <= length(label)
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  [@equal (_, _) => true]
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: list(int),
  children: list(id),
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = IdMap.t(segment);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type change =
  | Insert(id, segment)
  | Delete(id);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type diff = list(change);

// Helper functions for converting between piece types
let piece_to_auto_piece = (piece: Base.piece): piece =>
  switch (piece) {
  | Tile(tile) =>
    let auto_children =
      List.mapi(
        (i, _child_seg) => {
          {
            /* TODO: This assumes that indexes don't change, which they
               do if you put down a non-trailing delimiter */
            uuid: tile.id,
            index: i,
          }
        },
        tile.children,
      );
    Tile({
      id: tile.id,
      label: tile.label,
      mold: tile.mold,
      shards: tile.shards,
      children: auto_children,
    });
  | Base.Grout(grout) => Grout(grout)
  | Base.Secondary(secondary) => Secondary(secondary)
  | Base.Projector(_) =>
    // Projectors are not supported in AutoSeg, so we skip them
    // This could be handled differently depending on requirements
    Secondary({
      id: Id.mk(),
      content: Secondary.Comment("WHOOPS"),
    })
  };

let mk_id = (index: int, uuid: Id.t): id => {
  uuid,
  index,
};

let root = {
  uuid: Id.invalid,
  index: 0,
};

let seg_to_auto_seg = (~id=root, seg: Segment.t): IdMap.t(segment) => {
  let rec go =
          (id: id, acc: IdMap.t(segment), seg: Segment.t): IdMap.t(segment) => {
    let auto_seg = List.map(piece_to_auto_piece, seg);
    let acc = IdMap.add(id, auto_seg, acc);

    // Recursively process child segments from tiles
    List.fold_left(
      (acc, (index, piece)) => {
        switch (piece) {
        | Base.Tile(tile) =>
          List.fold_left(go(mk_id(index, tile.id)), acc, tile.children)
        | _ => acc
        }
      },
      acc,
      List.mapi((i, p) => (i, p), seg),
    );
  };

  go(id, IdMap.empty, seg);
};

let rec auto_piece_to_piece = (auto_seg: t, piece: piece): Base.piece => {
  switch (piece) {
  | Tile(tile) =>
    let children =
      List.map(
        id => {
          // Recursively convert child segments
          let child_seg =
            switch (IdMap.find_opt(id, auto_seg)) {
            | Some(seg) => seg
            | None => failwith("Child not found: " ++ Id.show(id.uuid))
            };
          List.map(auto_piece_to_piece(auto_seg), child_seg);
        },
        tile.children,
      );
    Base.Tile({
      id: tile.id,
      label: tile.label,
      mold: tile.mold,
      shards: tile.shards,
      children,
    });
  | Grout(grout) => Base.Grout(grout)
  | Secondary(secondary) => Base.Secondary(secondary)
  };
}
and auto_seg_to_seg = (seg: segment, auto_seg: t): Segment.t => {
  List.map(auto_piece_to_piece(auto_seg), seg);
};

let auto_seg_to_seg = (auto_seg: t): Segment.t => {
  // Find the root segment (the one with index 0)
  let root: segment =
    switch (IdMap.find_opt(root, auto_seg)) {
    | Some(seg) => seg
    | None => failwith("AutoSeg: Root not found")
    };

  auto_seg_to_seg(root, auto_seg);
};

let mk_diff = (auto_seg1: t, auto_seg2: t): diff => {
  let deletions =
    IdMap.fold(
      (id, _, acc) =>
        IdMap.mem(id, auto_seg2) ? acc : [Delete(id), ...acc],
      auto_seg1,
      [],
    );

  let insertions_and_updates =
    IdMap.fold(
      (id, segment2, acc) => {
        switch (IdMap.find_opt(id, auto_seg1)) {
        | None => [Insert(id, segment2), ...acc]
        | Some(segment1) =>
          segment1 == segment2
            ? acc : [Delete(id), Insert(id, segment2), ...acc]
        }
      },
      auto_seg2,
      [],
    );

  deletions @ insertions_and_updates;
};

/**
 * Converts an AutoSeg.diff to Delta.EditScript.t
 * Maps:
 * - Insert(id, segment) to Delta.EditOp.t with `U_s3_Insert variant
 * - Delete(id) to Delta.EditOp.t with `U_s1_Delete variant
 */
let diff_to_ts = (diff: diff): Delta.EditScript.t => {
  // Convert a single tile to Delta.Tile.t
  let convert_tile = (tile: tile): Delta.Tile.t => {
    // Create Delta Nibs for the Mold
    let create_nib = () => {
      Delta.Nib.create(~shape=`L_s0_Convex, ~sort=`L_s2_Exp, ());
    };

    // Convert Mold
    let convert_mold = (mold: Mold.t): Delta.Mold.t => {
      Delta.Mold.create(
        ~out=`L_s2_Exp,
        ~in_=List.map(_ => `L_s2_Exp, mold.in_),
        ~nibs=(create_nib(), create_nib()),
        (),
      );
    };

    // Create Delta Tile
    Delta.Tile.create(
      ~t=`L_s4_Tile,
      ~id=Id.to_string(tile.id),
      ~label=tile.label,
      ~mold=convert_mold(tile.mold),
      ~shards=List.map(float_of_int, tile.shards),
      ~children=[], // Simplifying by not handling children recursively
      (),
    );
  };

  // Convert a segment to a list of Delta Tiles
  let convert_segment = (segment: segment): list(Delta.Tile.t) => {
    List.fold_left(
      (acc, piece) => {
        switch (piece) {
        | Tile(tile) => [convert_tile(tile), ...acc]
        | _ => acc // Skip Grout and Secondary pieces
        }
      },
      [],
      segment,
    )
    |> List.rev;
  };

  // Map each change to a Delta EditOp
  List.map(
    (change: change) => {
      switch (change) {
      | Insert(id, segment) =>
        `U_s3_Insert(
          Delta.InsertOp.create(
            ~t=`L_s3_Insert,
            ~uuid=Id.to_string(id.uuid),
            ~index=float_of_int(id.index),
            ~tiles=convert_segment(segment),
            (),
          ),
        )
      | Delete(id) =>
        `U_s1_Delete(
          Delta.DeleteOp.create(
            ~t=`L_s1_Delete,
            ~uuid=Id.to_string(id.uuid),
            ~index=float_of_int(id.index),
            (),
          ),
        )
      }
    },
    diff,
  );
};
