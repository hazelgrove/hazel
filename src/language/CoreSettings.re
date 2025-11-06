open Util;

module Evaluation = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show_case_clauses: bool,
    show_fn_bodies: bool,
    show_fixpoints: bool,
    show_ascription_steps: bool,
    show_lookup_steps: bool,
    show_stepper_filters: bool,
    project_tables: bool,
    // TODO[Matt]: Move this to somewhere where it is a per-scratch setting
    stepper_history: bool,
    show_settings: bool,
    show_hidden_steps: bool,
    enable_proof: bool,
  };

  let init = {
    show_case_clauses: true,
    show_fn_bodies: false,
    show_fixpoints: false,
    show_ascription_steps: false,
    show_lookup_steps: false,
    show_stepper_filters: false,
    project_tables: false,
    stepper_history: false,
    show_settings: false,
    show_hidden_steps: false,
    enable_proof: false,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  assist: bool,
  dynamics: bool,
  dynamic_feedback: bool,
  flip_animations: bool,
  evaluation: Evaluation.t,
};

let off: t = {
  statics: false,
  elaborate: false,
  assist: false,
  dynamics: false,
  dynamic_feedback: false,
  flip_animations: false,
  evaluation: Evaluation.init,
};

let on: t = {
  statics: true,
  elaborate: true,
  assist: true,
  dynamics: true,
  dynamic_feedback: true,
  flip_animations: true,
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
