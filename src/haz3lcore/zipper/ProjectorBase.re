open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Virtual_dom.Vdom;

[@deriving (show({with_path: false}), sexp, yojson)]
type fold = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type infer = {expected_ty: option(Typ.t)};
[@deriving (show({with_path: false}), sexp, yojson)]
type checkbox = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type slider = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type textarea = {inside: bool}; //TODO(andrew): rm

[@deriving (show({with_path: false}), sexp, yojson)]
type projector =
  | Fold(fold)
  | Infer(infer)
  | Checkbox(checkbox)
  | Slider(slider)
  | TextArea(textarea);

[@deriving (show({with_path: false}), sexp, yojson)]
module Map = {
  open Id.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(projector);
  let empty = empty;
  let find = find_opt;
  let mem = mem;
  let mapi = mapi;
  let update = update;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Inline(int)
  | Block(Measured.Point.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type status =
  | Indicated(Util.Direction.t)
  | Selected;

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Piece.t;

type inner_action = ..;
//TODO(andrew): decide how to rm
type inner_action +=
  | SetInside(bool);

type action =
  | Remove /* Remove projector */
  | FocusInternal(string) /* DOM Focus on projector */
  | Default /* Defer input to focal DOM element */
  | Escape(string, Util.Direction.t) /* Pass key control to parent editor */
  | UpdateSyntax(syntax => syntax)
  | UpdateModel(inner_action);

/* Externally calculated info to be fed to projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = {
  syntax,
  status: option(status),
  ci: option(Info.t),
};

let cls = (indicated: option(status)) =>
  switch (indicated) {
  | Some(Indicated(Left)) => ["indicated", "left"]
  | Some(Indicated(Right)) => ["indicated", "right"]
  | Some(Selected) => ["selected"]
  | None => []
  };

module type Core = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;

  let can_project: Piece.t => bool;

  let model: model;
  // let projector: projector;

  let view: (~info: info, ~inject: action => Ui_effect.t(unit)) => Node.t;
  let placeholder: info => shape;

  //[@deriving (show({with_path: false}), sexp, yojson)]
  //type action;
  let update: inner_action => projector;
  // let auto_update: info => projector;
  let keymap: (Util.Direction.t, Key.t) => option(action);
};

type core = (module Core);
