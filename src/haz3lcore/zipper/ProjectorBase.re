open Util;
open Virtual_dom.Vdom;

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | Fold
  | Info
  | Checkbox
  | Slider
  | SliderF
  | TextArea;

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  kind,
  model: string,
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

type action =
  | Remove /* Remove projector entirely */
  | Focus(option(Util.Direction.t)) /* Pass focus to projector */
  | Escape(Util.Direction.t) /* Pass focus to parent editor */
  | SetSyntax(syntax) /* Set underlying syntax */
  | SetModel(string); /* Set projector model */

type p_action = action;

/* Externally calculated info to be fed to projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = {
  id: Id.t,
  syntax,
  status: option(status),
  ci: option(Info.t),
};

let info_init = (p: syntax) => {
  id: Piece.id(p),
  syntax: p,
  status: None,
  ci: None,
};

module type Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  let init: model;
  let can_project: Piece.t => bool;
  let can_focus: bool;
  let view:
    (
      model,
      ~info: info,
      ~go: action => Ui_effect.t(unit),
      ~inject: p_action => Ui_effect.t(unit)
    ) =>
    Node.t;
  let placeholder: (model, info) => shape;
  let update: (model, action) => model;
  let focus: ((Id.t, Direction.t)) => unit;
};

type serialized_model = string;
type serialized_action = string;

module type Cooked = {
  let init: serialized_model;
  let can_project: Piece.t => bool;
  let can_focus: bool;
  let view:
    (
      serialized_model,
      ~info: info,
      ~go: serialized_action => Ui_effect.t(unit),
      ~inject: p_action => Ui_effect.t(unit)
    ) =>
    Node.t;
  let placeholder: (serialized_model, info) => shape;
  let update: (serialized_model, serialized_action) => serialized_model;
  let focus: ((Id.t, Direction.t)) => unit;
};

module Cook = (C: Projector) : Cooked => {
  let serialize_m = m => m |> C.sexp_of_model |> Sexplib.Sexp.to_string;
  let deserialize_m = s => s |> Sexplib.Sexp.of_string |> C.model_of_sexp;
  let serialize_a = a => a |> C.sexp_of_action |> Sexplib.Sexp.to_string;
  let deserialize_a = s => s |> Sexplib.Sexp.of_string |> C.action_of_sexp;
  let init = C.init |> serialize_m;
  let can_project = C.can_project;
  let can_focus = C.can_focus;
  let view = (m, ~info, ~go, ~inject) =>
    C.view(deserialize_m(m), ~info, ~go=a => go(serialize_a(a)), ~inject);
  let placeholder = m =>
    m |> Sexplib.Sexp.of_string |> C.model_of_sexp |> C.placeholder;
  let update = (m, a) =>
    C.update(m |> deserialize_m, a |> deserialize_a) |> serialize_m;
  let focus = C.focus;
};
