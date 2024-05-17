open Util;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type infer = {expected_ty: option(Typ.t)};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Fold
  | Infer(infer);

let to_string: t => string =
  fun
  | Fold => "F"
  | Infer(_) => "I";

let placeholder_length: t => int =
  fun
  | Fold => 2
  | Infer({expected_ty: None, _}) => "-" |> String.length
  | Infer({expected_ty: Some(expected_ty), _}) =>
    expected_ty |> Typ.pretty_print |> String.length;

let placeholder = (pr: t, id: Id.t): Piece.t =>
  Piece.Tile({
    id,
    label: [String.make(placeholder_length(pr), ' ')],
    mold: Mold.mk_op(Any, []),
    shards: [0],
    children: [],
  });

[@deriving (show({with_path: false}), sexp, yojson)]
module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type p = t;
  open Id.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(p);
  let empty = empty;
  let add = add;
  let remove = remove;
  let find = find_opt;
  let mem = mem;
  let fold = fold;
  let mapi = mapi;
  let cardinal = cardinal;
  let update = update;
};

let placehold = (ps: Map.t, p: Piece.t) =>
  switch (Map.find(Piece.id(p), ps)) {
  | None => p
  | Some(pr) => placeholder(pr, Piece.id(p))
  };

let rec of_segment = (projectors, seg: Segment.t): Segment.t => {
  seg |> List.map(placehold(projectors)) |> List.map(of_piece(projectors));
}
and of_piece = (projectors, p: Piece.t): Piece.t => {
  switch (p) {
  | Tile(t) => Tile(of_tile(projectors, t))
  | Grout(_) => p
  | Secondary(_) => p
  };
}
and of_tile = (projectors, t: Tile.t): Tile.t => {
  {...t, children: List.map(of_segment(projectors), t.children)};
};

let piece_is = (projectors: Map.t, p: option(Piece.t)) =>
  switch (p) {
  | Some(p) when Map.mem(Piece.id(p), projectors) =>
    Map.mem(Piece.id(p), projectors) ? Some(Piece.id(p)) : None
  | _ => None
  };

let neighbor_is = (projectors, s: Siblings.t): (option(Id.t), option(Id.t)) => (
  piece_is(projectors, Siblings.left_neighbor(s)),
  piece_is(projectors, Siblings.right_neighbor(s)),
);

let selection_sides_is =
    (projectors, s: Selection.t): (option(Id.t), option(Id.t)) => (
  piece_is(projectors, ListUtil.hd_opt(s.content)),
  piece_is(projectors, ListUtil.last_opt(s.content)),
);
