let update_program =
    (
      model: Model.t,
      action: ModelAction.t,
      state: State.t,
      new_program: Program.t,
      ~schedule_action: _,
    )
    : Model.t => {
  /* let old_program = model |> get_program; */
  let update_selected_instances = _si => {
    /* FIXME: Fix this. */
    let si =
      /* Program.get_result(old_program) == Program.get_result(new_program) ? si : */
      UserSelectedInstances.init;

    switch (
      model.settings.evaluation.evaluate,
      new_program |> Program.cursor_on_exp_hole,
    ) {
    | (false, _)
    | (_, None) => si
    | (true, Some(u)) =>
      switch (si |> UserSelectedInstances.find_opt(u)) {
      | None => si |> UserSelectedInstances.add(u, 0)
      | Some(_) => si
      }
    };
  };

  let model =
    model
    |> Model.put_program(new_program)
    |> Model.map_selected_instances(update_selected_instances)
    |> Model.put_undo_history(
         {
           let history = model |> Model.get_undo_history;
           let prev_cardstacks = model |> Model.get_cardstacks;
           let new_cardstacks =
             model |> Model.put_program(new_program) |> Model.get_cardstacks;
           UndoHistory.push_edit_state(
             history,
             prev_cardstacks,
             new_cardstacks,
             action,
           );
         },
       );

  /* Send evaluation request. */
  State.ProgramEvaluator.next(state.evaluator, new_program);

  /* Set evaluation to pending. */
  schedule_action(ModelAction.UpdateResult(ResultPending));

  model;
};

let perform_edit_action =
    (model: Model.t, state: State.t, a: Action.t, ~schedule_action) => {
  TimeUtil.measure_time(
    "Model.perform_edit_action",
    model.settings.performance.measure
    && model.settings.performance.model_perform_edit_action,
    () => {
      let new_program =
        model |> Model.get_program |> Program.perform_edit_action(a);
      let model_action = ModelAction.EditAction(a);
      update_program(
        model,
        model_action,
        state,
        new_program,
        ~schedule_action,
      );
    },
  );
};

let move_via_key =
    (model: Model.t, state: State.t, move_key, ~schedule_action) => {
  let (new_program, action) =
    model
    |> Model.get_program
    |> Program.move_via_key(~settings=model.settings, move_key);
  let model_action = ModelAction.EditAction(action);
  update_program(model, model_action, state, new_program, ~schedule_action);
};

let move_via_click =
    (model: Model.t, state: State.t, row_col, ~schedule_action) => {
  let (new_program, action) =
    model
    |> Model.get_program
    |> Program.move_via_click(~settings=model.settings, row_col);
  let model_action = ModelAction.EditAction(action);
  update_program(model, model_action, state, new_program, ~schedule_action);
};

let select_case_branch =
    (
      model: Model.t,
      state: State.t,
      path_to_case: CursorPath.steps,
      branch_index: int,
      ~schedule_action,
    ) => {
  let program = model |> Model.get_program;
  let action = Program.move_to_case_branch(path_to_case, branch_index);
  let new_program = Program.perform_edit_action(action, program);
  let model_action = ModelAction.EditAction(action);

  let model = model |> Model.put_program(new_program);
  let model =
    update_program(model, model_action, state, new_program, ~schedule_action)
    |> Model.focus_cell;
  model |> Model.focus_cell;
};
