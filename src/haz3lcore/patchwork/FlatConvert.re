open Util;
include ZipperBase;

/* Re-export types from FlatTypes to maintain API compatibility */
include FlatTypes;

/* Validate tile structure: for a complete tile, len(children) = len(shards) - 1.
   Logs warnings for debugging sync issues with multi-delimiter forms. */
let validate_tile =
    (
      ~context: string,
      ~id: Id.t,
      ~label: Label.t,
      ~shards: list(int),
      ~children: list('a),
    )
    : unit => {
  let num_shards = List.length(shards);
  let num_children = List.length(children);
  let expected_children = max(0, num_shards - 1);
  if (num_children != expected_children && num_shards > 0) {
    Js_of_ocaml.Firebug.console##warn(
      Js_of_ocaml.Js.string(
        "[FLAT DEBUG] "
        ++ context
        ++ " tile mismatch: id="
        ++ Id.to_string(id)
        ++ " label="
        ++ String.concat(",", label)
        ++ " shards="
        ++ string_of_int(num_shards)
        ++ " (indices: "
        ++ String.concat(",", List.map(string_of_int, shards))
        ++ ")"
        ++ " children="
        ++ string_of_int(num_children)
        ++ " (expected "
        ++ string_of_int(expected_children)
        ++ ")",
      ),
    );
  };
};

/* Since #2389 a tile's identity is (form, sort); the flat wire format
   still carries (label, mold), so recover the form by exact mold match
   among the label's base candidates — the same information the old
   record held. Fallback molds (pending-remold tiles stored at Any)
   match no candidate and get re-classified.
   TODO: the wire tile (FlatTypes.Flat.tile, embed/src/types/flatdoc.d.ts,
   FlatDoc.mli via `pnpm type:flatdoc`, PatchworkComm.JsConvert) should
   carry (form, sort) instead of (label, mold); until it does, a
   pending-remold tile stored at Any can come back as a different family
   (e.g. Ap -> Parens) after a sync_replace round trip. */
let form_of_label_mold = (label: Label.t, mold: Mold.t): (Form.t, Sort.t) =>
  switch (
    List.find_opt(
      ((_, m): (Form.t, Mold.t)) => m == mold,
      Form.base_candidates(label),
    )
  ) {
  | Some((f, _)) => (f, mold.out)
  | None => Form.classify_label(mold.out, label)
  };

let seg_to_doc = (seg: Segment.t): Doc.t => {
  let piece_count = ref(0);
  let root_form = Form.Compound(Parens);
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
    | Tile({id, shards, children, _} as t) =>
      let label = Tile.label(t);
      validate_tile(~context="seg_to_doc", ~id, ~label, ~shards, ~children);
      children
      |> List.map(go_seg)
      |> Doc.union_all
      |> Doc.add(
           id,
           Flat.Tile({
             id,
             label,
             mold: Tile.mold(t),
             shards,
             children: children |> List.map(List.map(Piece.id)),
           }),
         );
    };
  };
  let result =
    Doc.add(
      Id.invalid,
      Flat.Tile({
        id: Id.invalid,
        label: Form.label_of(root_form),
        mold: Form.mold_of(root_form, Exp),
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
      validate_tile(~context="doc_to_seg", ~id, ~label, ~shards, ~children);
      let (form, sort) = form_of_label_mold(label, mold);
      Tile({
        id,
        form,
        sort,
        shards,
        children: List.map(go_seg, children),
      });
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
