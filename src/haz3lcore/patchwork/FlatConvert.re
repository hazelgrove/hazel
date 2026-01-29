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
      children
      |> List.map(go_seg)
      |> Doc.union_all
      |> Doc.add(
           id,
           Flat.Tile({
             id,
             label,
             mold,
             shards,
             children: children |> List.map(List.map(Piece.id)),
           }),
         )
    };
  };
  Doc.add(
    Id.invalid,
    Flat.Tile({
      id: Id.invalid,
      label: root_form.label,
      mold: root_form.mold,
      shards: [0, 1],
      children: [List.map(Piece.id, seg)],
    }),
    go_seg(seg),
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
