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

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  assist: bool,
  dynamics: bool,
  probe_all: bool,
  deep_reassociate: bool,
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
  deep_reassociate: false,
  auto_reindent: false,
  format_shortcut: FormatShortcut.Spaces,
  indentation_ux: false,
  flip_animations: false,
  animate_all_edits: false,
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
  deep_reassociate: true, /* matches the product (web Settings.re) */
  auto_reindent: true,
  format_shortcut: FormatShortcut.Spaces,
  indentation_ux: true,
  flip_animations: true,
  animate_all_edits: false,
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
