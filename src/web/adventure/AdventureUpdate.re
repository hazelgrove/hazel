/* Adventure Mode: Update Logic
 *
 * Handles adventure state transitions: advancing through steps,
 * resetting to checkpoints, and responding to user actions.
 */

open Util;
open Haz3lcore;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Start(Adventure.script) /* Begin an adventure */
  | StartWithSlide(Adventure.script, int) /* Begin with original slide index */
  | Stop /* End adventure mode */
  | Advance /* Move to next step (user clicked Next) */
  | Reset /* Reset to last checkpoint */
  | DismissResetSuggestion /* User dismissed the reset prompt */
  | UserActed /* User performed an editor action (for gate checking) */;

/* Result of an update: new state plus optional editor actions to perform */
type update_result = {
  model: AdventureModel.t,
  editor_actions: list(Action.t), /* Actions to apply to editor */
  set_checkpoint: bool, /* Should capture current editor state */
  reset_to_checkpoint: bool /* Should reset editor to checkpoint */
};

let no_side_effects = (model): update_result => {
  model,
  editor_actions: [],
  set_checkpoint: false,
  reset_to_checkpoint: false,
};

/* Process the current step and determine if we should auto-advance.
 * Returns (model, should_continue) where should_continue means
 * we should process the next step immediately. */
let process_step =
    (model: AdventureModel.t, step: Adventure.step): (update_result, bool) => {
  switch (step) {
  | Message(_) =>
    /* Messages wait for user to click Next */
    (no_side_effects(model), false)

  | AgentAction({actions, _}) =>
    /* Execute actions and auto-advance */
    (
      {
        model: {
          ...model,
          current_step: model.current_step + 1,
        },
        editor_actions: actions,
        set_checkpoint: false,
        reset_to_checkpoint: false,
      },
      true /* Continue to next step */
    )

  | Checkpoint =>
    /* Set checkpoint and auto-advance */
    (
      {
        model: {
          ...model,
          current_step: model.current_step + 1,
          actions_since_gate: 0,
          show_reset_suggestion: false,
        },
        editor_actions: [],
        set_checkpoint: true,
        reset_to_checkpoint: false,
      },
      true,
    )

  | LoadEditor(zipper) =>
    /* Load editor state and auto-advance */
    (
      {
        model: {
          ...model,
          current_step: model.current_step + 1,
        },
        editor_actions: [
          Paste(Segment(Siblings.zip(zipper.relatives.siblings))),
        ],
        set_checkpoint: false,
        reset_to_checkpoint: false,
      },
      true,
    )

  | UserGate(_) =>
    /* Gates wait for predicate satisfaction */
    (no_side_effects(model), false)
  };
};

/* Advance through steps, processing auto-advancing ones */
let rec advance_steps = (model: AdventureModel.t): update_result => {
  switch (AdventureModel.current_step(model)) {
  | None =>
    /* End of script */
    no_side_effects(model)
  | Some(step) =>
    let (result, should_continue) = process_step(model, step);
    if (should_continue && !AdventureModel.is_complete(result.model)) {
      /* Process next step, accumulating actions */
      let next_result = advance_steps(result.model);
      {
        ...next_result,
        editor_actions: result.editor_actions @ next_result.editor_actions,
        set_checkpoint: result.set_checkpoint || next_result.set_checkpoint,
      };
    } else {
      result;
    };
  };
};

let update = (action: t, model: AdventureModel.t): update_result => {
  switch (action) {
  | Start(script) =>
    let started = AdventureModel.start(script);
    advance_steps(started);

  | StartWithSlide(script, original_index) =>
    let started =
      AdventureModel.start(~original_slide_index=Some(original_index), script);
    advance_steps(started);

  | Stop => no_side_effects(AdventureModel.inactive)

  | Advance =>
    if (AdventureModel.can_advance(model)) {
      let advanced = {
        ...model,
        current_step: model.current_step + 1,
        actions_since_gate: 0,
        show_reset_suggestion: false,
      };
      advance_steps(advanced);
    } else {
      no_side_effects(model);
    }

  | Reset =>
    if (AdventureModel.can_reset(model)) {
      {
        model: {
          ...model,
          actions_since_gate: 0,
          show_reset_suggestion: false,
        },
        editor_actions: [],
        set_checkpoint: false,
        reset_to_checkpoint: true,
      };
    } else {
      no_side_effects(model);
    }

  | DismissResetSuggestion =>
    no_side_effects({
      ...model,
      show_reset_suggestion: false,
    })

  | UserActed =>
    /* Increment action count and maybe show reset suggestion */
    switch (AdventureModel.current_step(model)) {
    | Some(UserGate({action_threshold, _})) =>
      let new_count = model.actions_since_gate + 1;
      let show_suggestion = new_count >= action_threshold;
      no_side_effects({
        ...model,
        actions_since_gate: new_count,
        show_reset_suggestion:
          show_suggestion && Option.is_some(model.checkpoint),
      });
    | _ => no_side_effects(model)
    }
  };
};

/* Check if a gate predicate is satisfied.
 * Called after user actions to determine if we should auto-advance. */
let check_gate =
    (~zipper: Zipper.t, ~info_map: Statics.Map.t, model: AdventureModel.t)
    : update_result => {
  switch (AdventureModel.current_step(model)) {
  | Some(UserGate({predicate, _})) =>
    let satisfied = AdventureGate.check(~zipper, ~info_map, predicate);
    if (satisfied) {
      /* Gate passed! Advance to next step */
      let advanced = {
        ...model,
        current_step: model.current_step + 1,
        actions_since_gate: 0,
        show_reset_suggestion: false,
      };
      advance_steps(advanced);
    } else {
      no_side_effects(model);
    };
  | _ => no_side_effects(model)
  };
};
