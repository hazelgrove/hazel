open Util;
include ZipperBase;

module Flat = {
  type tile = {
    id: Id.t,
    label: Label.t,
    mold: Mold.t,
    shards: list(int),
    children: list(list(Id.t)),
  };
  type piece =
    | Tile(tile)
    | Grout(Grout.t)
    | Secondary(Secondary.t);
};
module Doc = {
  include Id.Map;
  type nonrec t = t(Flat.piece);
  let union_all = (docs: list(t)): t => {
    List.fold_left(union((_, _, a) => Some(a)), empty, docs);
  };
};

let seg_to_doc = (seg: Segment.t): Doc.t => {
  let root_form = Form.get(ParensExp);

  let rec go_seg = (seg: Segment.t): Doc.t => {
    seg |> List.map(go_piece) |> Doc.union_all;
  }
  and go_piece = (piece: Piece.t): Doc.t => {
    switch (piece) {
    | Projector(_) => Doc.empty
    | Secondary(secondary) =>
      Doc.singleton(secondary.id, Flat.Secondary(secondary))
    | Grout(grout) => Doc.singleton(grout.id, Flat.Grout(grout))
    | Tile({id, label, mold, shards, children}) =>
      let flat_tile =
        Flat.{
          id,
          label,
          mold,
          shards,
          children: children |> List.map(List.map(Piece.id)),
        };
      children
      |> List.map(go_seg)
      |> Doc.union_all
      |> Doc.add(id, Flat.Tile(flat_tile));
    };
  };

  go_seg(seg)
  |> Doc.add(
       Id.invalid,
       Flat.Tile({
         id: Id.invalid,
         label: root_form.label,
         mold: root_form.mold,
         shards: [0, 1],
         children: [List.map(Piece.id, seg)],
       }),
     );
};

let doc_to_seg = (doc: Doc.t): Segment.t => {
  let root_seg_ids =
    switch (Doc.find_opt(Id.invalid, doc)) {
    | Some(Tile({children: [children], _})) => children
    | _ => failwith("Root not found")
    };
  let rec go_seg = (seg_ids: list(Id.t)): Segment.t => {
    List.map(go_piece, seg_ids);
  }
  and go_piece = (piece_id: Id.t): Base.piece => {
    switch (Doc.find_opt(piece_id, doc)) {
    | Some(Tile({id, label, mold, shards, children})) =>
      Tile({
        id,
        label,
        mold,
        shards,
        children: List.map(go_seg, children),
      })
    | Some(Grout(grout)) => Grout(grout)
    | Some(Secondary(secondary)) => Secondary(secondary)
    | None => failwith("Piece not found: " ++ Id.show(piece_id))
    };
  };

  go_seg(root_seg_ids);
};

module StringMap = Map.Make(String);

let doc_to_string_map = (doc: Doc.t): StringMap.t(Flat.piece) => {
  Doc.fold(
    (id: Id.t, piece: Flat.piece, acc: StringMap.t(Flat.piece)) => {
      StringMap.add(Id.show(id), piece, acc)
    },
    doc,
    StringMap.empty,
  );
};

// [@deriving (show({with_path: false}), sexp, yojson, eq, ord)]
// type id = {
//   uuid: Id.t,
//   index: int,
// };

// module IdMap =
//   MapUtil.Make({
//     [@deriving (show({with_path: false}), sexp, yojson, eq, ord)]
//     type t = id;
//     let compare = compare;
//   });

// [@deriving (show({with_path: false}), sexp, yojson, eq)]
// type segment = list(piece)
// and piece =
//   | Tile(tile)
//   | Grout(Grout.t)
//   | Secondary(Secondary.t)
// and tile = {
//   // invariants:
//   // - length(mold.in_) + 1 == length(label)
//   // - length(shards) <= length(label)
//   // - length(shards) == length(children) + 1
//   // - sort(shards) == shards
//   [@equal (_, _) => true]
//   id: Id.t,
//   label: Label.t,
//   mold: Mold.t,
//   shards: list(int),
//   children: list(id),
// };

// [@deriving (show({with_path: false}), sexp, yojson, eq)]
// type t = IdMap.t(segment);

// [@deriving (show({with_path: false}), sexp, yojson, eq)]
// type change =
//   | Insert(id, segment)
//   | Delete(id);

// [@deriving (show({with_path: false}), sexp, yojson, eq)]
// type diff = list(change);

// // Helper functions for converting between piece types
// let piece_to_auto_piece = (piece: Base.piece): piece =>
//   switch (piece) {
//   | Tile(tile) =>
//     let auto_children =
//       List.mapi(
//         (i, _child_seg) => {
//           {
//             /* TODO: This assumes that indexes don't change, which they
//                do if you put down a non-trailing delimiter */
//             uuid: tile.id,
//             index: i,
//           }
//         },
//         tile.children,
//       );
//     Tile({
//       id: tile.id,
//       label: tile.label,
//       mold: tile.mold,
//       shards: tile.shards,
//       children: auto_children,
//     });
//   | Base.Grout(grout) => Grout(grout)
//   | Base.Secondary(secondary) => Secondary(secondary)
//   | Base.Projector(_) =>
//     // Projectors are not supported in AutoSeg, so we skip them
//     // This could be handled differently depending on requirements
//     Secondary({
//       id: Id.mk(),
//       content: Secondary.Comment("WHOOPS"),
//     })
//   };

// let mk_id = (index: int, uuid: Id.t): id => {
//   uuid,
//   index,
// };

// let root = {
//   uuid: Id.invalid,
//   index: 0,
// };

// let rec auto_piece_to_piece = (auto_seg: t, piece: piece): Base.piece => {
//   switch (piece) {
//   | Tile(tile) =>
//     let children =
//       List.map(
//         id => {
//           // Recursively convert child segments
//           let child_seg =
//             switch (IdMap.find_opt(id, auto_seg)) {
//             | Some(seg) => seg
//             | None => failwith("Child not found: " ++ Id.show(id.uuid))
//             };
//           List.map(auto_piece_to_piece(auto_seg), child_seg);
//         },
//         tile.children,
//       );
//     Base.Tile({
//       id: tile.id,
//       label: tile.label,
//       mold: tile.mold,
//       shards: tile.shards,
//       children,
//     });
//   | Grout(grout) => Base.Grout(grout)
//   | Secondary(secondary) => Base.Secondary(secondary)
//   };
// }
// and auto_seg_to_seg = (seg: segment, auto_seg: t): Segment.t => {
//   List.map(auto_piece_to_piece(auto_seg), seg);
// };

// let auto_seg_to_seg = (auto_seg: t): Segment.t => {
//   // Find the root segment (the one with index 0)
//   let root: segment =
//     switch (IdMap.find_opt(root, auto_seg)) {
//     | Some(seg) => seg
//     | None => failwith("AutoSeg: Root not found")
//     };

//   auto_seg_to_seg(root, auto_seg);
// };

// let mk_diff = (auto_seg1: t, auto_seg2: t): diff => {
//   let deletions =
//     IdMap.fold(
//       (id, _, acc) =>
//         IdMap.mem(id, auto_seg2) ? acc : [Delete(id), ...acc],
//       auto_seg1,
//       [],
//     );

//   let insertions_and_updates =
//     IdMap.fold(
//       (id, segment2, acc) => {
//         switch (IdMap.find_opt(id, auto_seg1)) {
//         | None => [Insert(id, segment2), ...acc]
//         | Some(segment1) =>
//           segment1 == segment2
//             ? acc : [Delete(id), Insert(id, segment2), ...acc]
//         }
//       },
//       auto_seg2,
//       [],
//     );

//   deletions @ insertions_and_updates;
// };

// /**
//  * Converts an AutoSeg.diff to Delta.EditScript.t
//  * Maps:
//  * - Insert(id, segment) to Delta.EditOp.t with `U_s3_Insert variant
//  * - Delete(id) to Delta.EditOp.t with `U_s1_Delete variant
//  */
// let diff_to_ts = (diff: diff): Delta.EditScript.t => {
//   // Convert a single tile to Delta.Tile.t
//   let convert_tile = (tile: tile): Delta.Tile.t => {
//     // Create Delta Nibs for the Mold
//     let create_nib = () => {
//       Delta.Nib.create(~shape=`L_s0_Convex, ~sort=`L_s2_Exp, ());
//     };

//     // Convert Mold
//     let convert_mold = (mold: Mold.t): Delta.Mold.t => {
//       Delta.Mold.create(
//         ~out=`L_s2_Exp,
//         ~in_=List.map(_ => `L_s2_Exp, mold.in_),
//         ~nibs=(create_nib(), create_nib()),
//         (),
//       );
//     };

//     // Create Delta Tile
//     Delta.Tile.create(
//       ~t=`L_s4_Tile,
//       ~id=Id.to_string(tile.id),
//       ~label=tile.label,
//       ~mold=convert_mold(tile.mold),
//       ~shards=List.map(float_of_int, tile.shards),
//       ~children=[], // Simplifying by not handling children recursively
//       (),
//     );
//   };

//   // Convert a segment to a list of Delta Tiles
//   let convert_segment = (segment: segment): list(Delta.Tile.t) => {
//     List.fold_left(
//       (acc, piece) => {
//         switch (piece) {
//         | Tile(tile) => [convert_tile(tile), ...acc]
//         | _ => acc // Skip Grout and Secondary pieces
//         }
//       },
//       [],
//       segment,
//     )
//     |> List.rev;
//   };

//   // Map each change to a Delta EditOp
//   List.map(
//     (change: change) => {
//       switch (change) {
//       | Insert(id, segment) =>
//         `U_s3_Insert(
//           Delta.InsertOp.create(
//             ~t=`L_s3_Insert,
//             ~uuid=Id.to_string(id.uuid),
//             ~index=float_of_int(id.index),
//             ~tiles=convert_segment(segment),
//             (),
//           ),
//         )
//       | Delete(id) =>
//         `U_s1_Delete(
//           Delta.DeleteOp.create(
//             ~t=`L_s1_Delete,
//             ~uuid=Id.to_string(id.uuid),
//             ~index=float_of_int(id.index),
//             (),
//           ),
//         )
//       }
//     },
//     diff,
//   );
// };
