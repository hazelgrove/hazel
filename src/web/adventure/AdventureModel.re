/* Adventure Mode: Runtime State
 *
 * Manages the active state of an adventure, including current position
 * in the script, checkpoints for reset, and action counting for
 * stuck-user detection.
 */

open Util;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  active: bool,
  script: Adventure.script,
  current_step: int,
  checkpoint: option(Zipper.t), /* Last checkpoint for reset */
  actions_since_gate: int, /* Actions since entering current gate */
  show_reset_suggestion: bool, /* Prompt user to reset */
  original_slide_index: option(int) /* Slide to restore on exit */
};

/* Initial inactive state */
let inactive: t = {
  active: false,
  script: {
    id: "",
    title: "",
    steps: [],
  },
  current_step: 0,
  checkpoint: None,
  actions_since_gate: 0,
  show_reset_suggestion: false,
  original_slide_index: None,
};

/* Start an adventure with a given script.
 * original_slide_index should be set by the caller when creating a fresh slide. */
let start = (~original_slide_index=None, script: Adventure.script): t => {
  active: true,
  script,
  current_step: 0,
  checkpoint: None,
  actions_since_gate: 0,
  show_reset_suggestion: false,
  original_slide_index,
};

/* Get current step, if any */
let current_step = (model: t): option(Adventure.step) =>
  if (model.current_step >= 0
      && model.current_step < List.length(model.script.steps)) {
    Some(List.nth(model.script.steps, model.current_step));
  } else {
    None;
  };

/* Check if we're at the end */
let is_complete = (model: t): bool =>
  model.current_step >= List.length(model.script.steps);

/* Check if current step is a user gate */
let is_at_gate = (model: t): bool =>
  switch (current_step(model)) {
  | Some(UserGate(_)) => true
  | _ => false
  };

/* Check if current step allows manual advancement (Next button) */
let can_advance = (model: t): bool =>
  switch (current_step(model)) {
  | Some(Message({can_advance: true, _})) => true
  | Some(AgentAction(_)) => true /* Agent actions auto-advance but also allow manual */
  | _ => false
  };

/* Check if reset is available (has checkpoint and user has acted) */
let can_reset = (model: t): bool =>
  Option.is_some(model.checkpoint) && model.actions_since_gate > 0;
