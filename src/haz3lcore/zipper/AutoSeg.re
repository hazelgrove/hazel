open Util;
include ZipperBase;

[@deriving (show({with_path: false}), sexp, yojson, eq, ord)]
type id = {
  uuid: Id.t,
  index: int,
};

module IdMap =
  Map.Make({
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
          List.fold_left(go(mk_id(index, id.uuid)), acc, tile.children)
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
