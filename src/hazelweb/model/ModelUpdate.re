open Lwt.Infix;

open Model;

type deferred_action = Lwt.t(option(ModelAction.t));

let update_program = (a: ModelAction.t, new_program, model) => {
  /* let old_program = model |> get_program; */
  let update_selected_instances = _si => {
    /* FIXME: Restore this. */
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
    |> put_program(new_program)
    |> map_selected_instances(update_selected_instances)
    |> put_undo_history(
         {
           let history = model |> get_undo_history;
           let prev_cardstacks = model |> get_cardstacks;
           let new_cardstacks =
             model |> put_program(new_program) |> get_cardstacks;
           UndoHistory.push_edit_state(
             history,
             prev_cardstacks,
             new_cardstacks,
             a,
           );
         },
       );

  /* Run evaluation asynchronously, returning deferred update action. */
  let (deferred_result, evaluator) =
    model |> get_program |> ProgramEvaluator.get_result(model.evaluator);
  let model = model |> put_evaluator(evaluator);
  let deferred_action =
    deferred_result
    >|= Option.map(result => ModelAction.UpdateLastResult(result));

  (model, deferred_action);
};

let perform_edit_action = (a: Action.t, model: t) => {
  TimeUtil.measure_time(
    "Model.perform_edit_action",
    model.settings.performance.measure
    && model.settings.performance.model_perform_edit_action,
    () => {
      let new_program =
        model |> get_program |> Program.perform_edit_action(a);
      let ma = ModelAction.EditAction(a);
      model |> update_program(ma, new_program);
    },
  );
};

let move_via_key = (move_key, model) => {
  let (new_program, action) =
    model
    |> get_program
    |> Program.move_via_key(~settings=model.settings, move_key);
  let model_action = ModelAction.EditAction(action);
  model |> update_program(model_action, new_program);
};

let move_via_click = (row_col, model) => {
  let (new_program, action) =
    model
    |> get_program
    |> Program.move_via_click(~settings=model.settings, row_col);
  let model_action = ModelAction.EditAction(action);
  model |> update_program(model_action, new_program);
};

let select_case_branch =
    (path_to_case: CursorPath.steps, branch_index: int, model: t) => {
  let program = model |> get_program;
  let action = Program.move_to_case_branch(path_to_case, branch_index);
  let new_program = Program.perform_edit_action(action, program);
  let model_action = ModelAction.EditAction(action);

  let (model, deferred_action) =
    model
    |> put_program(new_program)
    |> update_program(model_action, new_program);
  (model |> focus_cell, deferred_action);
};
