open Sexplib.Std;

/* Projector model types */

[@deriving (show({with_path: false}), sexp, yojson)]
type infer = {expected_ty: option(Typ.t)};

[@deriving (show({with_path: false}), sexp, yojson)]
type fold = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type proj_type =
  | Fold(fold)
  | Infer(infer);

[@deriving (show({with_path: false}), sexp, yojson)]
module ProjectorMap = {
  open Id.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(proj_type);
  let empty = empty;
  let find = find_opt;
  let mem = mem;
  let mapi = mapi;
  let update = update;
};

module type P = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
  let proj_type: proj_type;
  let data: t;
  let placeholder_length: unit => int;
  let can_project: Piece.t => bool;
  let update: option(Info.t) => proj_type;
};

type projector_module = (module P);

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Outer
    | Inner(int, int);

  let decrement: t => t =
    fun
    | Outer
    | Inner(_, 0) => Outer
    | Inner(d, c) => Inner(d, c - 1);

  let offset: t => int =
    fun
    | Outer => 0
    | Inner(_, c) => c + 1;
};

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret: Caret.t,
  [@opaque]
  projectors: ProjectorMap.t,
};

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let put_siblings = (siblings, z: t): t => update_siblings(_ => siblings, z);

let put_selection_content = (content: Segment.t, z): t => {
  ...z,
  selection: {
    ...z.selection,
    content,
  },
};
