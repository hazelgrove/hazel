open Util;
include ZipperBase;

/* Re-export types from FlatTypes to maintain API compatibility */
include FlatTypes;

let seg_to_doc = (seg: Segment.t): Doc.t => {
  let piece_count = ref(0);
  let root_form = Form.get(ParensExp);
  let rec go_seg = (seg: Segment.t): Doc.t => {
    seg |> List.map(go_piece) |> Doc.union_all;
  }
  and go_piece = (piece: Piece.t): Doc.t => {
    piece_count := piece_count^ + 1;
    switch (piece) {
    | Projector({id, kind, syntax, model}) =>
      /* Flatten the wrapped syntax piece and add it to the doc,
         then add the projector entry referencing that piece by ID */
      go_piece(syntax)
      |> Doc.add(
           id,
           Flat.Projector({
             id,
             kind: ProjectorCore.Kind.name(kind),
             syntax: Piece.id(syntax),
             model,
           }),
         )
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
  let result =
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
  // Log piece traversal count
  PerfLog.log(
    "seg_to_doc traversed " ++ string_of_int(piece_count^) ++ " pieces",
  );
  result;
};

let doc_to_seg = (doc: Doc.t): Segment.t => {
  let piece_count = ref(0);
  let root_seg_ids =
    switch (Doc.find_opt(Id.invalid, doc)) {
    | Some(Tile({children: [children], _})) => children
    | _ => failwith("Root not found")
    };
  let rec go_seg = (seg_ids: list(Id.t)): Segment.t => {
    List.map(go_piece, seg_ids);
  }
  and go_piece = (piece_id: Id.t): Base.piece => {
    piece_count := piece_count^ + 1;
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
    | Some(Projector({id, kind, syntax, model})) =>
      Projector({
        id,
        kind: ProjectorCore.Kind.of_name(kind),
        syntax: go_piece(syntax),
        model,
      })
    | None => failwith("Piece not found: " ++ Id.show(piece_id))
    };
  };
  let result = go_seg(root_seg_ids);
  // Log piece reconstruction count
  PerfLog.log(
    "doc_to_seg reconstructed " ++ string_of_int(piece_count^) ++ " pieces",
  );
  result;
};
