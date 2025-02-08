open Util;
open Virtual_dom.Vdom;

/* This descibes the API for projectors: GUIs which
 * can replace part of the program syntax and perform
 * actions on that underlying syntax, as well as
 * mainting their own custom state. The comments below
 * detail the procedure of defining a new projector.
 *
 * See zipper/projectors/ for examples
 * of currently available projectors */

/* The type of syntax which a projector can replace.
 * Right now projectors can replace a single piece */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Base.piece;

//TODO(andrew)
[@deriving (show({with_path: false}), sexp, yojson)]
type trad = {
  id: Id.t,
  kind: ProjectorCore.Kind.t,
  syntax: Base.piece,
  model: string,
};

/* Global actions available to handlers in all projectors */
type external_action =
  | Remove /* Remove projector entirely */
  | Escape(Util.Direction.t) /* Pass focus to parent editor */
  | SetSyntax(Base.segment); /* Set underlying syntax */

/* Syntax utility functions/values for projector use,
 * provided here to resolve cyclic dependency issues */
[@deriving (show({with_path: false}), sexp, yojson)]
type utility = {
  /* Convert a segment to a term */
  seg_to_term: Base.segment => option(Term.Any.t),
  /* Convert a term to a segment */
  term_to_seg: Any.t => Base.segment,
  /* Lifts term->term functions to syntax->syntax. This will
   * proactively attempt to parenthesize resulting non-single
   * piece terms. As such, sorts that do not have parentheses
   * (currently all degenerate cases) will throw an error */
  lift_syntax: (Any.t => Any.t, Base.segment) => option(Base.segment),
};

/* External info proivded to all projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = {
  /* The id of the projector, equal to the id of the root
   * term of the syntax, provided directly here for convenience.
   * This is mostly intended to be used as a persistent unique
   * identifier to allow individual projectors to distiguish
   * their DOM nodes. */
  id: Id.t,
  /* The syntax underlying the projector. Currently this
   * is a single piece representing a complete term, but
   * this may be relaxed in the future. */
  syntax: Base.segment,
  /* Static information about the syntax including type
   * information. Statics may be disabled by the user;
   * this case (None) must be handled by projector authors */
  statics: option(Statics.Info.t),
  /* Dynamic information about the syntax including
   * live values of the syntax. Dynamics may be
   * disabled by the user; this case (None) must be
   * handled by projector authors */
  dynamics: option(Dynamics.Info.t),
  /* Syntax utility functions/values for projector use,
   * provided here to resolve cyclic dependency issues */
  utility,
};

module View = {
  type t =
    | Tylr(Node.t)
    | Pro({
        underlay: option(Node.t),
        inline: option(Node.t),
        overlay: option(Node.t),
        offside: option(Node.t),
      });

  let get_tylr = (v: t) =>
    switch (v) {
    | Tylr(v) => v
    | Pro(_) => failwith("Tylrlmao2")
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type seg = (~background: bool=?, Sort.t, list(syntax)) => Node.t;

  let mk = (~underlay=None, ~overlay=None, ~offside=None, inline) =>
    Pro({inline: Some(inline), underlay, overlay, offside});
};

/* To add a new projector:
 * 1. Create a new module implementing Projector (e.g. FoldProj)
 * 2. Add an entry for it in ProjectorCore.Kind.t
 * 3. Register the module in ProjectorInit.to_module
 * 4. If you want to expose the projector via a keyboard
 *    shortcut, add a Project(...) entry in Keyboard.re
 * 5. If you want to expose the projector in the projector
 *    panel bottom bar UI, update ProjectorView.name,
 *    ProjectorView.of_name, and ProjectorCore.projectors
 * 6. If you want to manually manage the projector as part of
 *    the update cycle, use the implementations of the SetIndicated
 *    and RemoveIndicated actions in ProjectorPerform as a guide
 *    for how to add/remove projectors from an editor */
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
  let can_project: Term.Any.t => bool;
  /* Does this projector have internal position states,
   * overriding the editor caret & keyboard handlers?
   * If yes, the focus method will be called when this
   * projector is either clicked on or if left/right
   * is pressed when the caret is to the immediate
   * right/left of the projector */
  let can_focus: bool;
  /* If dynamics is true, this projector will be
   * instrumented with a probe to collect dynamic
   * information during evaluation */
  let dynamics: bool;
  /* Renders the main DOM view for the projector,
   * which will replace the syntax in-line. */
  let view:
    (
      model,
      info,
      /* A callback for the projector's own actions */
      ~local: action => Ui_effect.t(unit),
      /* A callback for parent editor actions */
      ~parent: external_action => Ui_effect.t(unit),
      /* Creates a non-interactive embedded syntax view,
       * provided here to address a dependency cycle */
      ~view_seg: View.seg
    ) =>
    View.t;
  let placeholder: (model, info) => ProjectorCore.Shape.t;
  /* Update the local projector model given an action */
  let update: (model, info, action) => model;
  /* Does whatever needs to be done to give a projector
   * keyboard focus. Right now this is only for side
   * effects but could be extended in the future to
   * take/return the model if the projector needs to
   * maintain a complex internal position state */
  let focus: ((Id.t, option(Direction.t))) => unit;
};

/* A cooked projector is the same as the base module
 * signature except model & action are serialized so
 * they may be used by the Editor without it having
 * specialized knowledge of projector internals */
module type Cooked =
  Projector with type model = string and type action = string;

module Cook = (C: Projector) : Cooked => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = string;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = string;
  let serialize_m = m => m |> C.sexp_of_model |> Sexplib.Sexp.to_string;
  let deserialize_m = s => s |> Sexplib.Sexp.of_string |> C.model_of_sexp;
  let serialize_a = a => a |> C.sexp_of_action |> Sexplib.Sexp.to_string;
  let deserialize_a = s => s |> Sexplib.Sexp.of_string |> C.action_of_sexp;
  let init = C.init |> serialize_m;
  let can_project = C.can_project;
  let can_focus = C.can_focus;
  let dynamics = C.dynamics;
  let view = (m, info, ~local, ~parent, ~view_seg) =>
    C.view(
      deserialize_m(m),
      info,
      ~local=a => local(serialize_a(a)),
      ~parent,
      ~view_seg,
    );
  let placeholder = m =>
    m |> Sexplib.Sexp.of_string |> C.model_of_sexp |> C.placeholder;
  let update = (m, i, a) =>
    C.update(m |> deserialize_m, i, a |> deserialize_a) |> serialize_m;
  let focus = C.focus;
};

/* Projectors currently are all convex */
let shapes = _: Nibs.shapes => Nib.Shape.(Convex, Convex);
