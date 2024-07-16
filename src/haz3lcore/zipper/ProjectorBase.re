open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
open Virtual_dom.Vdom;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Fold
  | Info
  | Checkbox
  | Slider
  | SliderF
  | TextArea;

[@deriving (show({with_path: false}), sexp)]
type sexp2 = Sexplib.Sexp.t;
//TODO(andrew): proper serialization
let sexp2_of_yojson = _ =>
  Sexplib.Sexp.Atom("TODO(andrew): proper serialization");
let yojson_of_sexp2 = _ => Yojson.Safe.from_string("TODOTODOTODO");
[@deriving (show({with_path: false}), sexp, yojson)]
type model = sexp2;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  kind,
  model,
};

[@deriving (show({with_path: false}), sexp, yojson)]
module Map = {
  open Id.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(entry);
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
  | Indicated(Util.Direction.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Piece.t;

type inner_action = ..;
//TODO(andrew): decide how to rm
// [@deriving (show({with_path: false}), sexp, yojson)]
type inner_action +=
  | SetInside(bool);

type action =
  | Remove /* Remove projector */
  | FocusInternal(string) /* DOM Focus on projector */
  | Default /* Defer input to focal DOM element */
  | Escape(string, Util.Direction.t) /* Pass key control to parent editor */
  | SetSyntax(syntax)
  | UpdateModel(inner_action);

/* Externally calculated info to be fed to projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = {
  syntax,
  status: option(status),
  ci: option(Info.t),
};

module type CoreInner = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;

  let init: model;
  let can_project: Piece.t => bool;
  let view:
    (model, ~info: info, ~inject: action => Ui_effect.t(unit)) => Node.t;
  let placeholder: (model, info) => shape;
  let update: (model, inner_action) => model;
  let keymap: (model, Util.Direction.t, Key.t) => option(action);
};

module type CoreOuter = {
  let init: model;
  let can_project: Piece.t => bool;
  let view:
    (model, ~info: info, ~inject: action => Ui_effect.t(unit)) => Node.t;
  let placeholder: (model, info) => shape;
  let update: (model, inner_action) => model;
  let keymap: (model, Util.Direction.t, Key.t) => option(action);
};

module CoreOuterMk = (C: CoreInner) : CoreOuter => {
  let init = C.sexp_of_model(C.init);
  let can_project = C.can_project;
  let view = m => m |> C.model_of_sexp |> C.view;
  let placeholder = m => m |> C.model_of_sexp |> C.placeholder;
  let update = (m, a) => C.update(C.model_of_sexp(m), a) |> C.sexp_of_model;
  let keymap = (m: Sexplib.Sexp.t) => m |> C.model_of_sexp |> C.keymap;
};

type core = (module CoreOuter);
