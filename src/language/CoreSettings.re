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
    project_html: bool,
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
    project_html: true,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  assist: bool,
  dynamics: bool,
  probe_all: bool,
  deep_reassociate: bool,
  flip_animations: bool,
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
  flip_animations: false,
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
  deep_reassociate: false,
  flip_animations: true,
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
