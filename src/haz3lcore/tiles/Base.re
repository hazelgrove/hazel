open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector)
and tile = {
  // invariants (arity = length(Form.label_of(form))):
  // - length(shards) <= arity
  // - length(shards) == length(children) + 1
  // - sort(shards) == shards
  [@equal (_, _) => true]
  id: Id.t,
  form: Form.t,
  // the local sort guess cached at insertion classification / remold
  // (the only writers); mold = Form.mold_of(form, sort)
  [@sexp.default Sort.Exp] [@sexp_drop_default.sexp]
  sort: Sort.t,
  // the sexp defaults spell the complete arity-1 tile, the common case
  [@sexp.default [0]] [@sexp_drop_default.sexp]
  shards: list(int),
  [@sexp.default []] [@sexp_drop_default.sexp]
  children: list(segment),
}
and projector = ProjectorCore.t(piece);

let rec map_piece = (~f_piece, x: piece) => {
  let rec_call = (piece: piece) => {
    switch (piece) {
    | Tile(t) =>
      Tile({
        ...t,
        children: t.children |> List.map(List.map(map_piece(~f_piece))),
      })
    | Grout(_)
    | Secondary(_)
    | Projector(_) => piece
    };
  };
  x |> f_piece(rec_call);
};
/* If the piece is parentheses, return the child. Otherwise,
 * return a singleton segment consisting of the piece.
 * The Parens family is op-shaped by construction; the concave-left
 * Ap family shares the ["(",")"] label but is a distinct family,
 * so it does not match here. */
let unparenthesize = (piece: piece): segment =>
  switch (piece) {
  | Tile({form: Form.Compound(Parens), children: [seg], _}) => seg
  | _ => [piece]
  };

let rec segment_to_string =
        (
          ~holes=" ",
          ~concave_holes=" ",
          ~refractors: list((Id.t, _))=[],
          ~refractor_seg_to_seg:
             (list((Id.t, _)), segment) => (list((Id.t, _)), segment),
          ~projector_to_segment,
          seg: segment,
        )
        : string => {
  let (refractors, seg) = refractor_seg_to_seg(refractors, seg);
  seg
  |> List.map(
       piece_to_string(
         ~holes,
         ~concave_holes,
         ~refractors,
         ~refractor_seg_to_seg,
         ~projector_to_segment,
       ),
     )
  |> String.concat("");
}
and piece_to_string =
    (
      ~holes: string,
      ~concave_holes: string,
      ~refractors: list((Id.t, _)),
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      p: piece,
    )
    : string =>
  switch (p) {
  | Tile(t) =>
    tile_to_string(
      ~holes,
      ~concave_holes,
      ~refractors,
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      t,
    )
  | Grout({shape: Concave, _}) => concave_holes
  | Grout({shape: Convex, _}) => holes
  | Secondary(w) => Secondary.get_string(w.content)
  | Projector(p) =>
    segment_to_string(
      ~holes,
      ~concave_holes,
      ~refractors,
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      projector_to_segment(p),
    )
  }
and tile_to_string =
    (
      ~holes: string,
      ~concave_holes: string,
      ~refractors: list((Id.t, _)),
      ~refractor_seg_to_seg,
      ~projector_to_segment,
      t: tile,
    )
    : string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(
       List.nth(Form.label_of(t.form)),
       segment_to_string(
         ~holes,
         ~concave_holes,
         ~refractors,
         ~refractor_seg_to_seg,
         ~projector_to_segment,
       ),
     )
  |> String.concat("");
