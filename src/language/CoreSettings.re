open Util;

module Evaluation = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show_case_clauses: bool,
    show_fn_bodies: bool,
    show_fixpoints: bool,
    show_ascription_steps: bool,
    show_ascriptions: bool,
    show_case_steps: bool,
    show_lookup_steps: bool,
    show_stepper_filters: bool,
    // TODO[Matt]: Move this to somewhere where it is a per-scratch setting
    stepper_history: bool,
    show_settings: bool,
    show_hidden_steps: bool,
    enable_proof: bool,
    project_tables: bool,
  };

  let init = {
    show_case_clauses: true,
    show_fn_bodies: false,
    show_fixpoints: false,
    show_ascription_steps: false,
    show_ascriptions: false,
    show_case_steps: false,
    show_lookup_steps: false,
    show_stepper_filters: false,
    stepper_history: false,
    show_settings: false,
    show_hidden_steps: false,
    enable_proof: false,
    project_tables: true,
  };
};

module FormatShortcut = {
  /* What cmd/ctrl+S does. Cumulative ladder: each level includes the
   * previous. Cmd+Shift+S is always Breaks regardless. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Nothing /* no formatting */
    | Indent /* re-indent only */
    | Spaces /* re-indent + canonicalize within-line spacing */
    | Breaks; /* full pretty print (may change linebreaks) */
};

module DragChain = {
  /* drag ladders: each drag track runs through its refactor repeated
     (D2's list reorder — every position a state of one drag), landing
     on the nearest rung at release. Off = one rung. (Everything is
     reserved for mixing kinds in one pull; it acts as SameKind.) */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Off
    | SameKind /* only more of the first commit's kind */
    | Everything;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  assist: bool,
  dynamics: bool,
  probe_all: bool,
  /* Completion-triggered local re-indentation (experimental) */
  auto_reindent: bool,
  format_shortcut: FormatShortcut.t,
  /* Indentation-transparent editing: arrow movement skips leading
     whitespace and backspace at first-content joins lines (deletes
     indentation + linebreak). Off keeps char-exact caret behavior
     (test harnesses position carets by counted moves). */
  indentation_ux: bool,
  flip_animations: bool,
  /* Experimental: FLIP-ghost every edit's code movement, not just
   * refactor invocations (see GhostFlip.re) */
  animate_all_edits: bool,
  /* Modal drag-to-refactor: plain drag on a token pulls it along
   * candidate transform tracks (CodeDrag.re) */
  drag_refactor: bool,
  [@sexp.default DragChain.SameKind]
  drag_chain: DragChain.t,
  /* a blocked drag move pushes through: carries its dependencies or
     lifts to a helper (the keyboard's second press) */
  [@sexp.default true]
  drag_push: bool,
  /* a drag runs through every move that changes where a value lives
     (extract, hoist, sink, merge, lift, feed), and a grabbed value
     moves its own line */
  [@sexp.default true]
  drag_composites: bool,
  /* drag overlay: the refactoring names, and the tracks + target dots
     (both off = no decoration beyond the moving code itself) */
  [@sexp.default true]
  drag_labels: bool,
  [@sexp.default true]
  drag_tracks: bool,
  display_warnings: bool,
  /* "Character-level mouse". When false (default), a mouse drag does
   * smart-rounded selection (char inside the starting token, whole-token
   * beyond) and the modifier (Alt/Ctrl) does pure char; when true, that
   * pairing is swapped. Only affects the mouse — keyboard Shift+Arrow is
   * always char-level (modifier → smart). */
  selection_chunkiness: bool,
  evaluation: Evaluation.t,
};

let off: t = {
  statics: false,
  elaborate: false,
  assist: false,
  dynamics: false,
  probe_all: false,
  auto_reindent: false,
  format_shortcut: FormatShortcut.Spaces,
  indentation_ux: false,
  flip_animations: false,
  animate_all_edits: false,
  drag_refactor: false,
  drag_chain: DragChain.SameKind,
  drag_push: true,
  drag_composites: true,
  drag_labels: true,
  drag_tracks: true,
  display_warnings: false,
  selection_chunkiness: false,
  evaluation: Evaluation.init,
};

let on: t = {
  statics: true,
  elaborate: true,
  assist: true,
  dynamics: true,
  probe_all: false, /* Off by default even in "on" config - opt-in feature */
  auto_reindent: true,
  format_shortcut: FormatShortcut.Spaces,
  indentation_ux: true,
  flip_animations: true,
  animate_all_edits: false,
  drag_refactor: false,
  drag_chain: DragChain.SameKind,
  drag_push: true,
  drag_composites: true,
  drag_labels: true,
  drag_tracks: true,
  display_warnings: true,
  selection_chunkiness: false,
  evaluation: Evaluation.init,
};

let eq_ignoring_stepper_modals = (a: t, b: t) =>
  {
    ...a,
    evaluation: {
      ...a.evaluation,
      stepper_history: false,
      show_settings: false,
    },
  }
  == {
       ...b,
       evaluation: {
         ...b.evaluation,
         stepper_history: false,
         show_settings: false,
       },
     };
