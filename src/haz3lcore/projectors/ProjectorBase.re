open Util;
open Language;

/* This describes the API for projector LOGIC: the backend-independent
 * half of projectors (state, syntax transformations, layout shape).
 * Views live in the frontends: the web's Vdom views and view registry
 * are in src/web/projectors (ProjectorViewBase), the terminal views in
 * src/tui/TermProjector. See docs/projector-backend-split.md.
 *
 * See projectors/implementations/ for examples of currently available
 * projector logic modules. */

/* The type of syntax which a projector can replace.
 * Right now projectors can replace a single piece */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Base.piece;

/* Global actions available to handlers in all projectors */
type external_action =
  | SampleFocus(Action.sample_focus)
  | Probe(Action.probe) /* Probe actions like StepInto */
  | Remove /* Remove projector entirely */
  | Escape(Util.Direction.t) /* Pass focus to parent editor */
  | EscapeToLineEnd(ProjectorCore.Kind.t) /* Pass focus to parent editor, move to end of line */
  | SetSyntax(Base.segment) /* Set underlying syntax */
  | FocusById(Util.Id.t); /* Focus a projector by its term id */

/* Syntax utility functions/values for projector use,
 * provided here to resolve cyclic dependency issues */
[@deriving (show({with_path: false}), sexp, yojson)]
type utility = {
  /* Convert a segment to a term */
  seg_to_term: Base.segment => option(Any.t),
  /* Convert a term to a segment */
  term_to_seg: (~inline: bool, Any.t) => Base.segment,
  seg_to_string: Base.segment => string,
  /* Lifts term->term functions to syntax->syntax. This will
   * proactively attempt to parenthesize resulting non-single
   * piece terms. As such, sorts that do not have parentheses
   * (currently all degenerate cases) will throw an error */
  lift_syntax:
    (~inline: bool, Any.t => Any.t, Base.segment) => option(Base.segment),
};

module Focusable = {
  /* Can the projector take focus, in the sense of handling
   * keyboard input? If so, how can it take focus?
   *
   * The callbacks are frontend concerns (the web's move DOM focus),
   * so they are not part of the logic modules: view backends install
   * a Kind-indexed mapping in [focusables] below at startup. */

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

/* Frontend-installed focus behavior per projector kind (see
 * Focusable above). Consulted by ProjectorPerform's Focus action and
 * by the web's keyboard hand-off. Default: nothing is focusable. */
let focusables: ref(ProjectorCore.Kind.t => Focusable.t) =
  ref(_ => Focusable.non);

let focusable = (kind: ProjectorCore.Kind.t): Focusable.t =>
  focusables^(kind);

/* External info provided to all projectors */
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
  statics: option(Language.Statics.Info.t),
  /* Dynamic information about the syntax including
   * live values of the syntax. Dynamics may be
   * disabled by the user; this case (None) must be
   * handled by projector authors */
  dynamics: option(Language.Dynamics.Info.t),
  /* The elaborated sub-expression at this projector's ID.
   * Available when statics/elaboration is enabled. The elaborated
   * form has labels inserted/rearranged by the elaborator. */
  elaborated: option(Language.Exp.t),
  /* The node's own (solo) reachability path condition, computed (only for the
   * Reach refractor) in ProjectorInfo.mk_info. None for every other projector
   * kind. Group merges are assembled in the Reach sidebar. */
  reach: option(Reach.t),
  /* All merge groups in use across every Reach refractor (empty elsewhere);
   * lets the offside offer the whole set to toggle membership. */
  reach_groups: list(int),
  /* Syntax utility functions/values for projector use,
   * provided here to resolve cyclic dependency issues */
  utility,
};

/* A projector-reported error, e.g. "can't render as table" */
type error = {message: string};

/* To add a new projector:
 * 1. Create a new module implementing Projector (e.g. FoldProj)
 * 2. Add an entry for it in ProjectorCore.Kind.t
 * 3. Register the module in ProjectorInit.to_module
 * 4. Add view modules in the frontends (src/web/projectors,
 *    optionally src/tui/TermProjector) and register them there
 * 5. If you want to expose the projector via a keyboard
 *    shortcut, add a Project(...) entry in Keyboard.re
 * 6. If you want to expose the projector in the projector
 *    panel bottom bar UI, update ProjectorCore.Kind.name,
 *    ProjectorCore.Kind.of_name, and ProjectorCore.projectors
 * 7. If you want to manually manage the projector as part of
 *    the update cycle, use the implementation of the
 *    SetIndicated action in ProjectorPerform as a guide
 *    for how to add/remove projectors in an editor */
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
  /* Init should return None if the projector doesn't want
   * to handle the provided term. Otherwise, it should
   * return the desired initial state of the model. */
  let init: Any.t => option(model);
  /* If dynamics is true, this projector will be
   * instrumented with a probe to collect dynamic
   * information during evaluation */
  let dynamics: bool;
  /* Whether this projector needs type-elaborated syntax.
   *
   * Some projectors (e.g. TableProj) require syntactic features
   * that only exist after elaboration — for example, auto-labels
   * inserted by `LabeledTuple.rearrange` during type-directed
   * elaboration. Without elaboration, a list of unlabeled tuples
   * like `[("Alice", 12)]` has no `TupLabel` nodes, so the
   * projector can't determine column headers.
   *
   * When this flag is true:
   *
   * 1. At init time (`ProjectorPerform.init`), the elaborated
   *    sub-expression is validated via `init` but the original
   *    syntax is stored unchanged. The projector's syntax is
   *    never replaced with the elaborated form.
   *
   * 2. At render time, `info.elaborated` is populated with the
   *    elaborated sub-expression (looked up by the inner
   *    expression's ID). The projector uses this for rendering.
   *    If the elaborated form becomes incompatible (e.g. the
   *    surrounding type changes), the projector shows an error.
   *
   * 3. When the projector is removed, `pr.syntax` (the original
   *    user syntax) is restored directly.
   *
   * 4. The context menu also checks the elaborated form when
   *    deciding applicability, so the menu item appears even
   *    when the raw syntax wouldn't pass `init`.
   *
   * If elaboration is unavailable (e.g. during trigger-based
   * invocation before statics have run), the elaborated path
   * is skipped and the projector falls back to the raw syntax. */
  let elaborate_syntax: bool;
  /* The space left for the projector in the base editor */
  let placeholder: (model, info) => ProjectorCore.Shape.t;
  /* Update the local projector model given an action */
  let update: (model, info, action) => model;
  /* Report an error if the projector can't render properly */
  let error: (model, info) => option(error);
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
  let init = any => C.init(any) |> Option.map(serialize_m);
  let dynamics = C.dynamics;
  let elaborate_syntax = C.elaborate_syntax;
  let placeholder = m =>
    m |> Sexplib.Sexp.of_string |> C.model_of_sexp |> C.placeholder;
  let update = (m, i, a) =>
    C.update(
      m |> deserialize_m,
      i,
      a |> Sexplib.Sexp.of_string |> C.action_of_sexp,
    )
    |> serialize_m;
  let error = (m, i) => C.error(m |> deserialize_m, i);
};
