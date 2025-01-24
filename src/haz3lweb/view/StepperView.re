/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.Model.stepper;

  let init = StepperBase.Model.init_stepper;

  let get_state = StepperBase.Model.get_state_stepper;

  let get_elaboration = StepperBase.Model.get_elaboration_stepper;
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.Update.stepper;

  let update = StepperBase.Update.update_stepper;

  let calculate = StepperBase.Update.calculate_stepper;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = StepperBase.Selection.stepper;

  let get_cursor_info = StepperBase.Selection.get_cursor_info_stepper;

  let handle_key_event = StepperBase.Selection.handle_key_event_stepper;
};

module View = {
  let view = StepperBase.View.view_stepper;
};
