open Util;
open Sexplib.Std;

module type P = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
  let placeholder_length: t => int; //Projector
  let to_string: t => string; //Projector //TODO: rename to ci_string or something
  let can_project: (t, Piece.t) => bool; //ProjectorAction
  let update: (option(Info.t), t) => t; // ProjectorsUpdate
  //let toggle = (id: Id.t, z: Zipper.t, piece, d, rel) //ProjectorAction
};

module Fold: P = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = unit;
  let to_string: t => string = () => "F";
  let can_project = ((): t, p: Piece.t): bool => Piece.is_convex(p);
  let placeholder_length: t => int = () => 2;
  let update = (_: option(Info.t), (): t): t => ();
};

module Infer: P = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {expected_ty: option(Typ.t)};

  let to_string: t => string = _ => "I";
  let can_project = (_: t, p: Piece.t): bool =>
    Piece.is_convex(p)
    && (
      switch (p) {
      | Tile(t) => t.mold.out == Exp || t.mold.out == Pat
      | _ => false
      }
    );
  let placeholder_length: t => int =
    fun
    | {expected_ty: None, _} => "-" |> String.length
    | {expected_ty: Some(expected_ty), _} => {
        /* NOTE: This assumes pretty_print handles whitespace the same as view */
        //TODO(andrew): cleanup
        print_endline("placeholder_ty: " ++ Typ.pretty_print(expected_ty));
        print_endline(
          "placeholder_length "
          ++ (
            expected_ty |> Typ.pretty_print |> String.length |> string_of_int
          ),
        );
        expected_ty |> Typ.pretty_print |> String.length;
      };
  let update = (ci: option(Info.t), _: t): t => {
    print_endline("updating infer projector");
    let expected_ty =
      switch (ci) {
      | Some(InfoExp({mode, _}) | InfoPat({mode, _})) => Mode.ty_of(mode)
      | _ => Typ.Float
      };
    {expected_ty: Some(expected_ty)};
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Fold(Fold.t)
  | Infer(Infer.t);

// let placeholder = (pr: P.t, id: Id.t): Piece.t => {
//   Piece.Tile({
//     id,
//     label: [String.make(P.placeholder_length(pr), ' ')],
//     mold: Mold.mk_op(Any, []),
//     shards: [0],
//     children: [],
//   });
// };
// let placeholder = (type p, module X: P with type t = p, pr: p, id: Id.t) => {
//   Piece.Tile({
//     id,
//     label: [String.make(X.placeholder_length(pr), ' ')],
//     mold: Mold.mk_op(Any, []),
//     shards: [0],
//     children: [],
//   });
// };

module MkProjectorM = (P: P) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  module Map = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type p = P.t;
    open Id.Map;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Id.Map.t(p);
    let empty = empty;
    let find = find_opt;
    let mem = mem;
    let mapi = mapi;
    let update = update;
  };
  let placeholder = (pr: P.t, id: Id.t) => {
    Piece.Tile({
      id,
      label: [String.make(P.placeholder_length(pr), ' ')],
      mold: Mold.mk_op(Any, []),
      shards: [0],
      children: [],
    });
  };
  let placehold = (ps: Map.t, p: Piece.t) =>
    switch (Map.find(Piece.id(p), ps)) {
    | None => p
    | Some(pr) => placeholder(pr, Piece.id(p))
    };

  let rec of_segment = (projectors, seg: Segment.t): Segment.t => {
    seg
    |> List.map(placehold(projectors))
    |> List.map(of_piece(projectors));
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

  let neighbor_is =
      (projectors, s: Siblings.t): (option(Id.t), option(Id.t)) => (
    piece_is(projectors, Siblings.left_neighbor(s)),
    piece_is(projectors, Siblings.right_neighbor(s)),
  );

  let selection_sides_is =
      (projectors, s: Selection.t): (option(Id.t), option(Id.t)) => (
    piece_is(projectors, ListUtil.hd_opt(s.content)),
    piece_is(projectors, ListUtil.last_opt(s.content)),
  );
};

module FoldPlaceholder = MkProjectorM(Fold);
module InferPlaceholder = MkProjectorM(Infer);

type tt =
  | A(FoldPlaceholder.t);
