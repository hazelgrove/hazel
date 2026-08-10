open Util;
open Virtual_dom.Vdom;
open Language;

/* This descibes the API for projectors: GUIs which
 * can replace part of the program syntax and perform
 * actions on that underlying syntax, as well as
 * mainting their own custom state. The comments below
 * detail the procedure of defining a new projector.
 *
 * See zipper/projectors/ for examples
 * of currently available projectors */

/* The type of syntax which a projector can replace.
 * A projector's syntax is a segment (list of pieces) which may
 * contain Splice pieces whose contents are separately-editable
 * sub-regions. */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = Base.segment;

/* Global actions available to handlers in all projectors */
type external_action =
  | SampleFocus(Action.sample_focus)
  | Probe(Action.probe) /* Probe actions like StepInto */
  | Remove /* Remove projector entirely */
  | Escape(Util.Direction.t) /* Pass focus to parent editor */
  | EscapeToLineEnd(ProjectorCore.Kind.t) /* Pass focus to parent editor, move to end of line */
  | SetSyntax(Base.segment) /* Set underlying syntax */
  | SetTerm(Any.t, bool) /* Set underlying term, optionally preserving original splices */
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
  /* Lifts term->term functions over the projector's syntax term. */
  lift_term: (Any.t => Any.t, Base.segment) => option(Any.t),
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
  /* Syntax utility functions/values for projector use,
   * provided here to resolve cyclic dependency issues */
  utility,
};

/* A projector-reported error, e.g. "can't render as table" */
type error = {message: string};

/* A context-menu item a projector contributes when the editor's
 * context menu is opened with the caret inside one of the projector's
 * own splices (e.g. table row/column operations). Only the innermost
 * projector owning the splice is consulted, so for a table nested in a
 * table cell the menu shows the inner table's actions. */
type context_action = {
  label: string,
  action: external_action,
};

/* A replacement for the projector's underlying syntax, returned by
 * [init]. Projectors that transform their syntax at projection time
 * should prefer [Term]: provide the desired term (e.g. list items
 * wrapped in splices) and the framework prints it to a segment,
 * exactly as SetTerm does for later edits. [Syntax] is the escape
 * hatch for projectors that need piece-level control. */
[@deriving (show({with_path: false}), sexp, yojson)]
type init_override =
  | Term(Any.t)
  | Syntax(Base.segment);

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
    /* If true, the projector div gets the "error" class,
     * triggering the dashed red SVG border from proj-base.css */
    error: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type status = {
    kind: ProjectorCore.Kind.t,
    sort: Sort.t, /* What sort does the parent editor attribute to the projector? */
    indication: option(Direction.t), /* Is the parent editor caret adjacent? */
    selected: bool, /* Is the projector contained within a selection? */
    error: bool, /* Is there an error mark on the projector? */
    warning: bool /* Is there a warning mark on the projector? */
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type seg =
    (
      ~single_line: bool=?,
      ~background: bool=?,
      ~text_only: bool=?,
      Sort.t,
      Base.segment
    ) =>
    Node.t;

  /* Thunk returning the view for a splice by its id. Thunked so that
   * expensive splice rendering only happens for splices the projector
   * actually places in its view. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type splice_view = Id.t => Node.t;

  /* Intrinsic bounding box size of a splice's content, in the splice's
   * own coordinate frame. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type splice_size = Id.t => Util.Point.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type args('model, 'action) = {
    model: 'model,
    info,
    /* A callback for the projector's own actions */
    local: 'action => Ui_effect.t(unit),
    /* A callback for parent editor actions */
    parent: external_action => Ui_effect.t(unit),
    /* Creates a non-interactive embedded syntax view,
     * provided here to address a dependency cycle */
    view_seg: seg,
    /* Thunked splice views; call to render an inner editor for a splice
     * contained in this projector's syntax */
    splice_view,
    /* Intrinsic size of a splice's content */
    splice_size,
    /* All splice children of this projector in document order */
    splices: list(Base.splice),
    /* Parent editor context on the projector */
    status,
    /* Core settings for feature flags */
    core_settings: Language.CoreSettings.t,
  };

  let mk = (~overlay=None, ~offside=None, ~error=false, inline) => {
    inline,
    overlay,
    offside,
    error,
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
  /* An internal action type to be used in actions which
   * update the model. Use `unit` if the basic projector
   * actions (type `action`) above suffice */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  /* Init should return None if the projector doesn't want
   * to handle the provided term. Otherwise, it should
   * return the desired initial state of the model, along with
   * an optional replacement for the underlying syntax. [init]
   * receives the original [Base.segment] the user selected (in
   * addition to the parsed term). When the second component is
   * [Some(Term(t))], the framework prints [t] and stores the result
   * as the projector's syntax (the same path SetTerm uses); this is
   * the preferred way to transform the underlying syntax at init
   * time (e.g. to wrap list items in splices). [Some(Syntax(seg))]
   * installs [seg] directly for projectors that need piece-level
   * control. Return [None] to keep the selected syntax unchanged. */
  let init: (Any.t, Base.segment) => option((model, option(init_override)));
  /* Does this projector have some notion of internal
   * positions, whose handling should override the editor
   * caret & keyboard handlers? If so, provide handlers
   * here (see Focusable for more information) */
  let focusable: Focusable.t;
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
  /* Renders the DOM views for the projector */
  let view: View.args(model, action) => View.t;
  /* The space left for the projector in the base editor. The projector
   * receives [splice_size] so that it may size itself around its splice
   * children; splice sizes are intrinsic (independent of the layout of
   * the parent projector) and may be looked up by splice id. */
  let placeholder: (model, info, View.splice_size) => ProjectorCore.Shape.t;
  /* Update the local projector model given an action */
  let update: (model, info, action) => model;
  /* Report an error if the projector can't render properly */
  let error: (model, info) => option(error);
  /* Context-menu items contributed when the menu is opened with the
   * caret inside the projector's splice [splice] (see context_action).
   * Return [] if the projector has no splice-local actions. */
  let context_actions: (model, info, ~splice: Id.t) => list(context_action);
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
  let init = (any, seg) =>
    C.init(any, seg) |> Option.map(((m, seg)) => (serialize_m(m), seg));
  let focusable = C.focusable;
  let dynamics = C.dynamics;
  let elaborate_syntax = C.elaborate_syntax;
  let view = (args: View.args(model, action)) =>
    C.view({
      model: deserialize_m(args.model),
      info: args.info,
      local: a => args.local(serialize_a(a)),
      parent: args.parent,
      view_seg: args.view_seg,
      splice_view: args.splice_view,
      splice_size: args.splice_size,
      splices: args.splices,
      status: args.status,
      core_settings: args.core_settings,
    });
  let placeholder = (m, info, splice_size) =>
    C.placeholder(
      m |> Sexplib.Sexp.of_string |> C.model_of_sexp,
      info,
      splice_size,
    );
  let update = (m, i, a) =>
    C.update(m |> deserialize_m, i, a |> deserialize_a) |> serialize_m;
  let error = (m, i) => C.error(m |> deserialize_m, i);
  let context_actions = (m, i, ~splice) =>
    C.context_actions(m |> deserialize_m, i, ~splice);
};
