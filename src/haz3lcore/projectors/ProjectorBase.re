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
type syntax('p) = Base.piece('p);

/* Global actions available to handlers in all projectors */
type external_action('p) =
  | Remove /* Remove projector entirely */
  | Escape(Util.Direction.t) /* Pass focus to parent editor */
  | SetSyntax(Base.segment('p)); /* Set underlying syntax */

/* Syntax utility functions/values for projector use,
 * provided here to resolve cyclic dependency issues */
[@deriving (show({with_path: false}), sexp, yojson)]
type utility('p) = {
  /* Convert a segment to a term */
  seg_to_term: Base.segment('p) => option(Term.Any.t),
  /* Convert a term to a segment */
  term_to_seg: Any.t => Base.segment('p),
  /* Lifts term->term functions to syntax->syntax. This will
   * proactively attempt to parenthesize resulting non-single
   * piece terms. As such, sorts that do not have parentheses
   * (currently all degenerate cases) will throw an error */
  lift_syntax:
    (Any.t => Any.t, Base.segment('p)) => option(Base.segment('p)),
};

module Focusable = {
  /* Can the projector take focus, in the sense of handling
   * keyboard input? If so, how can it take focus? */

  /* Callbacks for projectors to react to getting focus */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus_keyboard = (Id.t, Direction.t) => unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus_pointer = Id.t => unit;

  /* If keyboard is not None, the projector can get focus
   * from keyboard arrow movement into it. If pointer is
   * not None, it can get focus from pointer interaction */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    pointer: option(focus_pointer),
    keyboard: option(focus_keyboard),
  };

  /* Default: A projector that cannot take focus */
  let non: t = {
    pointer: None,
    keyboard: None,
  };
};

/* External info proivded to all projectors */
[@deriving (show({with_path: false}), sexp, yojson)]
type info('p) = {
  /* The id of the projector, equal to the id of the root
   * term of the syntax, provided directly here for convenience.
   * This is mostly intended to be used as a persistent unique
   * identifier to allow individual projectors to distiguish
   * their DOM nodes. */
  id: Id.t,
  /* The syntax underlying the projector. Currently this
   * is a single piece representing a complete term, but
   * this may be relaxed in the future. */
  syntax: Base.segment('p),
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
  utility: utility('p),
};

module View = {
  /* A projector has an inline view, which replaces the underlying
   * syntax. Optionally, it may have an overlay view, which is shown
   * in the same place, but above most base editor decorations
   * including the inline views of all other projectors, and/or
   * an offside view, which is rendered at the end of the base
   * editor line containing the projector */
  type t = {
    inline: Node.t,
    overlay: option(Node.t),
    offside: option(Node.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type seg('p) = (~background: bool=?, Sort.t, list(syntax('p))) => Node.t;

  let mk = (~overlay=None, ~offside=None, inline) => {
    inline,
    overlay,
    offside,
  };
};

/* To add a new projector:
 * 1. Create a new module implementing Projector (e.g. FoldProj)
 * 2. Add an entry for it in ProjectorCore.Kind.t
 * 3. Register the module in ProjectorInit.to_module
 * 4. If you want to expose the projector via a keyboard
 *    shortcut, add a Project(...) entry in Keyboard.re
 * 5. If you want to expose the projector in the projector
 *    panel bottom bar UI, update ProjectorCore.Kind.name,
 *    ProjectorCore.Kind.of_name, and ProjectorCore.projectors
 * 6. If you want to manually manage the projector as part of
 *    the update cycle, use the implementation of the
 *    SetIndicated action in ProjectorPerform as a guide
 *    for how to add/remove projectors in an editor */
module type Projector = {
  /* The internal model type of the projector which will
   * be serialized and persisted. Use `unit` if you don't
   * need other state beyond the underlying syntax */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  let kind: ProjectorCore.Kind.gadt(model);
  /* An internal action type to be used in actions which
   * update the model. Use `unit` if the basic projector
   * actions (type `action`) above suffice */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  /* Init should return None if the projector doesn't want
   * to handle the provided term. Otherwise, it should
   * return the desired initial state of the model. */
  let init: Term.Any.t => option(model);
  /* Does this projector have some notion of internal
   * positions, whose handling should override the editor
   * caret & keyboard handlers? If so, provide handlers
   * here (see Focusable for more information) */
  let focusable: Focusable.t;
  /* If dynamics is true, this projector will be
   * instrumented with a probe to collect dynamic
   * information during evaluation */
  let dynamics: bool;
  /* Renders the DOM views for the projector */
  let view:
    (
      model,
      info('p),
      /* A callback for the projector's own actions */
      ~local: action => Ui_effect.t(unit),
      /* A callback for parent editor actions */
      ~parent: external_action('p) => Ui_effect.t(unit),
      /* Creates a non-interactive embedded syntax view,
       * provided here to address a dependency cycle */
      ~view_seg: View.seg('p)
    ) =>
    View.t;
  /* The space left for the projector in the base editor */
  let placeholder: (model, info('p)) => ProjectorShape.t;
  /* Update the local projector model given an action */
  let update: (model, info('p), action) => model;
  let mk_term: (~id: Id.t, ~from_segment: 's => Any.t, ~segment: 's) => Any.t;
};

let mk_term_default = (~id as _, ~from_segment, ~segment) => {
  from_segment(segment);
};

/* Projectors currently are all convex */
let shapes = (_: Base.projector('p)): Nibs.shapes =>
  Nib.Shape.(Convex, Convex);
