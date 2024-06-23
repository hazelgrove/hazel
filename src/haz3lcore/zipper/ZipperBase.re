open Sexplib.Std;

/* Projector model types */

[@deriving (show({with_path: false}), sexp, yojson)]
type fold = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type infer = {expected_ty: option(Typ.t)};
[@deriving (show({with_path: false}), sexp, yojson)]
type checkbox = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type slider = {value: int};
[@deriving (show({with_path: false}), sexp, yojson)]
type textarea = {value: string};

/* Projector action types */

[@deriving (show({with_path: false}), sexp, yojson)]
type fold_action = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type infer_action = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type checkbox_action = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type slider_action =
  | Set(int);
[@deriving (show({with_path: false}), sexp, yojson)]
type textarea_action = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Inline(int)
  | Block(int);

[@deriving (show({with_path: false}), sexp, yojson)]
type projector =
  | Fold(fold)
  | Infer(infer)
  | Checkbox(checkbox)
  | Slider(slider)
  | TextArea(textarea);

[@deriving (show({with_path: false}), sexp, yojson)]
module ProjectorMap = {
  open Id.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(projector);
  let empty = empty;
  let find = find_opt;
  let mem = mem;
  let mapi = mapi;
  let update = update;
};

/* Externally calculated info to be fed to projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type projector_info = {info: option(Info.t)};

module type ProjectorCore = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  let projector: projector;
  let model: model;
  let placeholder: unit => shape;
  let can_project: Piece.t => bool;
  let auto_update: projector_info => projector;
  let update: string => projector;
};

type projector_core = (module ProjectorCore);

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
