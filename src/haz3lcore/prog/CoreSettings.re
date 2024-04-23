open Sexplib.Std;

module Evaluation = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show_case_clauses: bool,
    show_fn_bodies: bool,
    show_fixpoints: bool,
    show_casts: bool,
    show_lookup_steps: bool,
    show_stepper_filters: bool,
    // TODO[Matt]: Move this to somewhere where it is a per-scratch setting
    stepper_history: bool,
    show_settings: bool,
    show_hidden_steps: bool,
  };

  let init = {
    show_case_clauses: true,
    show_fn_bodies: false,
    show_fixpoints: false,
    show_casts: false,
    show_lookup_steps: false,
    show_stepper_filters: false,
    stepper_history: false,
    show_settings: false,
    show_hidden_steps: false,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  statics: bool,
  elaborate: bool,
  assist: bool,
  dynamics: bool,
  evaluation: Evaluation.t,
};

let off: t = {
  statics: false,
  elaborate: false,
  assist: false,
  dynamics: false,
  evaluation: Evaluation.init,
};

let on: t = {
  statics: true,
  elaborate: true,
  assist: true,
  dynamics: true,
  evaluation: Evaluation.init,
};
