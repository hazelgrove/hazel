open Util;
open Virtual_dom.Vdom;

/* This determines the API for projectors, GUIs which
 * can replace part of the program syntax, and perform
 * actions which changes that underlying syntax, as well
 * as mainting their own custom state. The comments below
 * detail the procedure of defining a new projector.
 *
 * See the zipper/projectors/ folder for the implementations
 * of currently available projectors */

/* The type of syntax which a projector can replace.
 * Right now projectors can replace a single piece */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Base.piece;

/* Global actions available to handlers in all projectors */
type external_action =
  | Remove /* Remove projector entirely */
  | Escape(Util.Direction.t) /* Pass focus to parent editor */
  | SetSyntax(syntax); /* Set underlying syntax */

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
  syntax,
  /* Static information about the syntax including type
   * information. Statics may be disabled by the user;
   * this case (None) must be handled by projector authors */
  statics: option(Statics.Info.t),
  /* Dynamic information about the syntax including
   * live values of the syntax. Dynamics may be
   * disabled by the user; this case (None) must be
   * handled by projector authors */
  dynamics: option(Dynamics.Info.t),
};

/* Utility functions/values for to projector views.
 * These should be considered unstable/experimental */
[@deriving (show({with_path: false}), sexp, yojson)]
type utility = {
  /* The current font metrics for the editor, usable
   * to coordinate with the parent coordinate grid */
  font_metrics: FontMetrics.t,
  /* Non-interactive view for segments, included here
   * because of cyclic dependency issues*/
  view_seg: (Sort.t, Base.segment) => Node.t,
  /* Convert an expression to a segment, included here
   * because of cyclic dependency issues*/
  exp_to_seg: Exp.t => Base.segment,
  /* Convert a segment to an expression, included here
   * because of cyclic dependency issues*/
  seg_to_exp: Base.segment => Exp.t,
};

/* To add a new projector:
 * 1. Create a new module implementing Projector (e.g. FoldCore)
 * 2. Add an entry for it in Base.projector_kind
 * 3. Register the module in Projector.to_module
 * 4. If you want to expose the projector via a keyboard
 *    shortcut, see the existing entry for Fold in Keyboard
 * 5. If you want to expose the projector in the projector
 *    panel bottom bar UI, update ProjectorView.name,
 *    ProjectorView.of_name, and ProjectorView.applicable_projectors
 * 6. If you want to manually manage the projector as part of
 *    the update cycle, see the implementations of the SetIndicated
 *    and Remove actions in ProjectorPerform for how to manually
 *    add/remove projectors from an editor */
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
  /* If dynamics is true, this projector will be
   * instrumented with a probe to collect dynamic
   * information during evaluation */
  let dynamics: bool;
  /* Renders a DOM view for the projector, given the
   * model, an info packet (see info type for details),
   * and has two callbacks: ~parent for parent editor
   * actions(see external_action type above), and ~local
   * for this projector's local update function. */
  let view:
    (
      model,
      info,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~utility: utility
    ) =>
    Node.t;
  /* An optional additional view to be rendered at the
   * end of the row which includes the projector */
  let offside_view:
    option(
      (
        model,
        info,
        ~local: action => Ui_effect.t(unit),
        ~parent: external_action => Ui_effect.t(unit),
        ~utility: utility
      ) =>
      Node.t,
    );
  /* An optional view to be rendered above
   * the code / regular projector layer */
  let overlay_view:
    option(
      (
        model,
        info,
        ~local: action => Ui_effect.t(unit),
        ~parent: external_action => Ui_effect.t(unit),
        ~utility: utility
      ) =>
      Node.t,
    );
  /* An optional view to be rendered below the code and
   * regular projector layer. If this is provided,
   * regular underlays like indication and selection
   * decorations will not be drawn; projector clients
   * should use the classes placed on the wrapping
   * element to trigger their own custom indication and
   * selection decorations. Pointer handlers should not
   * be placed on this layer. */
  let underlay_view: option((model, info, ~utility: utility) => Node.t);
  /* How much space should be left in the code view for
   * this projector? This determines how the base code
   * view is laid out, including how movement around the
   * projector works. In principle this could be derived
   * from the view, but this is awkward to do so for now
   * projector writers are responsible for keeping these
   * in sync with each other. */
  let placeholder: (model, info) => ProjectorShape.t;
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
  let view = (m, info, ~local, ~parent, ~utility) =>
    C.view(
      deserialize_m(m),
      info,
      ~local=a => local(serialize_a(a)),
      ~parent,
      ~utility,
    );
  let offside_view =
    Option.map(
      (f, m, info, ~local, ~parent, ~utility) =>
        f(
          deserialize_m(m),
          info,
          ~local=a => local(serialize_a(a)),
          ~parent,
          ~utility,
        ),
      C.offside_view,
    );
  let overlay_view =
    Option.map(
      (f, m, info, ~local, ~parent, ~utility) =>
        f(
          deserialize_m(m),
          info,
          ~local=a => local(serialize_a(a)),
          ~parent,
          ~utility,
        ),
      C.overlay_view,
    );
  let underlay_view =
    Option.map(
      (f, m, info, ~utility) => f(deserialize_m(m), info, ~utility),
      C.underlay_view,
    );
  let placeholder = m =>
    m |> Sexplib.Sexp.of_string |> C.model_of_sexp |> C.placeholder;
  let update = (m, i, a) =>
    C.update(m |> deserialize_m, i, a |> deserialize_a) |> serialize_m;
  let focus = C.focus;
};

/* Projectors currently are all convex */
let shapes = (_: Base.projector) => Nib.Shape.(Convex, Convex);

/* Projectors currently have a fixed molding */
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
