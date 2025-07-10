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

let seg_to_auto_seg = (seg: Segment.t): IdMap.t(segment) =>
  failwith("TODO");
let auto_seg_to_seg = (auto_seg: t): Segment.t => failwith("TODO");

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
