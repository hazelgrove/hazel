open Util;
open Virtual_dom.Vdom;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Base.kind;

/* Projectors currently have two options for placeholder
 * shapes: A inline display of a given length, or a block
 * display with given length & height. Both of these can
 * depend on the projector model and info package */
[@deriving (show({with_path: false}), sexp, yojson)]
type shape =
  | Inline(int)
  | Block(Point.t);

/* The type of syntax which a projector can replace.
 * Right now projectors can replace a single piece */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Base.piece;

/* Global actions available to handlers in all projectors */
type external_action =
  | Remove /* Remove projector entirely */
  | Escape(Util.Direction.t) /* Pass focus to parent editor */
  | SetSyntax(syntax); /* Set underlying syntax */

/* External info fed to all projectors. Eventually
 * dynamic information will be added here. Projector
 * position and dimensions in base editor could be
 * added here if needed */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = {
  id: Id.t,
  syntax,
  ci: option(Info.t),
};

/* To add a new projector, implement this module signature */
module type Projector = {
  /* The internal model type of the projector which will
   * be serialized and persisted. Use `unit` if you don't
   * need other state beyond the underlying syntax */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  /* An internal action type to be used in actions which
   * update the model. Use `unit` if the basic projector
   * actions (type `action`) above suffice */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  /* Initial state of the model */
  let init: model;
  /* A predicate determining if the given underlying
   * syntax (currently limited to convex pieces) is
   * supported by this projector. This is used to gate
   * adding the projector */
  let can_project: Base.piece => bool;
  /* Does this projector have internal position states,
   * overriding the editor caret & keyboard handlers?
   * If yes, the focus method will be called when this
   * projector is either clicked on or if left/right
   * is pressed when the caret is to the immediate
   * right/left of the projector */
  let can_focus: bool;
  /* Renders a DOM view for the projector, given the
   * model, an info packet (see info type for details),
   * and has two callbacks: ~parent for parent editor
   * actions(see external_action type above), and ~local
   * for this projector's local update function. */
  let view:
    (
      model,
      ~info: info,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit)
    ) =>
    Node.t;
  /* How much space should be left in the code view for
   * this projector? This determines how the base code
   * view is laid out, including how movement around the
   * projector works. In principle this could be derived
   * from the view, but this is awkward to do so for now
   * projector writers are responsible for keeping these
   * in sync with each other. */
  let placeholder: (model, info) => shape;
  /* Update the local projector model given an action */
  let update: (model, action) => model;
  /* Does whatever needs to be done to give a projector
   * keyboard focus. Right now this is only for side
   * effects but could be extended in the future to
   * take/return the model if the projector needs to
   * maintain a complex internal position state */
  let focus: ((Id.t, option(Direction.t))) => unit;
};

/* Projector model and action are serialized so that
 * they may be used by the Editor without it having
 * specialized knowledge of projector internals */
type serialized_model = string;
type serialized_action = string;

/* A cooked projector is the same as the base module
 * signature except model & action are serialized */
module type Cooked = {
  let init: serialized_model;
  let can_project: Base.piece => bool;
  let can_focus: bool;
  let view:
    (
      serialized_model,
      ~info: info,
      ~local: serialized_action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit)
    ) =>
    Node.t;
  let placeholder: (serialized_model, info) => shape;
  let update: (serialized_model, serialized_action) => serialized_model;
  let focus: ((Id.t, option(Direction.t))) => unit;
};

module Cook = (C: Projector) : Cooked => {
  let serialize_m = m => m |> C.sexp_of_model |> Sexplib.Sexp.to_string;
  let deserialize_m = s => s |> Sexplib.Sexp.of_string |> C.model_of_sexp;
  let serialize_a = a => a |> C.sexp_of_action |> Sexplib.Sexp.to_string;
  let deserialize_a = s => s |> Sexplib.Sexp.of_string |> C.action_of_sexp;
  let init = C.init |> serialize_m;
  let can_project = C.can_project;
  let can_focus = C.can_focus;
  let view = (m, ~info, ~local, ~parent) =>
    C.view(
      deserialize_m(m),
      ~info,
      ~local=a => local(serialize_a(a)),
      ~parent,
    );
  let placeholder = m =>
    m |> Sexplib.Sexp.of_string |> C.model_of_sexp |> C.placeholder;
  let update = (m, a) =>
    C.update(m |> deserialize_m, a |> deserialize_a) |> serialize_m;
  let focus = C.focus;
};

/* Projectors currently are all convex */
let shapes = (_: Base.projector) => Nib.Shape.(Convex, Convex);

/* Projectors currently have a unique molding */
let mold_of = (p, sort: Sort.t): Mold.t => {
  let (l, r) = shapes(p);
  {
    nibs: {
      ({shape: l, sort}, {shape: r, sort});
    },
    out: sort,
    in_: [],
  };
};
