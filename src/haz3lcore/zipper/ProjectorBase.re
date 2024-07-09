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

/* Externally calculated info to be fed to projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = {info: option(Info.t)};

[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Inline(int)
  | Block(Measured.Point.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type accent =
  | Indicated(Util.Direction.t)
  | Selected;

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Piece.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Remove /* Remove projector */
  | FocusInternal(string) /* DOM Focus on projector */
  | Default /* Defer input to focal DOM element */
  | Escape(string, Util.Direction.t) /* Pass key control to parent editor */
  | UpdateSyntax(syntax => syntax)
  | UpdateModel(string);

let cls = (indicated: option(accent)) =>
  switch (indicated) {
  | Some(Indicated(Left)) => ["indicated", "left"]
  | Some(Indicated(Right)) => ["indicated", "right"]
  | Some(Selected) => ["selected"]
  | None => []
  };

module type Core = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;

  let model: model;
  let view: (~inject: action => Ui_effect.t(unit), option(accent)) => Node.t;
  let update: string => projector;
  let keymap: (Util.Direction.t, Key.t) => option(action);
  let placeholder: unit => shape;
  let can_project: Piece.t => bool;
  let auto_update: info => projector;
  let projector: projector;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
};

type core = (module Core);
