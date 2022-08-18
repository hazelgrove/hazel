type t = {
  res: ModelResult.t,
  cardstacks: ZCardstacks.t,
  cell_width: int,
  selected_instances: UserSelectedInstances.t,
  undo_history: UndoHistory.t,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  font_metrics: FontMetrics.t,
  mouse_position: ref(MousePosition.t),
  settings: Settings.t,
  cursor_inspector: CursorInspectorModel.t,
};

let cutoff = (m1, m2) => m1 === m2;

let cardstack_info = [
  Examples.cardstack,
  Examples.teststack,
  // TutorialCards.cardstack,
  // RCStudyCards.cardstack,
];

let init = (): t => {
  let cell_width = 80;
  let cardstacks = ZCardstacks.mk(~width=cell_width, cardstack_info);
  let undo_history: UndoHistory.t = {
    let cursor_term_info =
      UndoHistory.get_cursor_term_info(
        ~new_cardstacks_after=cardstacks,
        ~new_cardstacks_before=cardstacks,
      );
    let timestamp = Unix.time();
    let undo_history_entry: UndoHistory.undo_history_entry = {
      cardstacks_after_action: cardstacks,
      cardstacks_after_move: cardstacks,
      cursor_term_info,
      previous_action: EditAction(Init),
      action_group: Init,
      timestamp,
    };
    let undo_history_group: UndoHistory.undo_history_group = {
      group_entries: ([], undo_history_entry, []),
      is_expanded: false,
    };
    {
      groups: ([], undo_history_group, []),
      all_hidden_history_expand: false,
      disable_auto_scrolling: false,
      preview_on_hover: true,
      hover_recover_group_id: 0,
      hover_recover_elt_id: 0,
      cur_group_id: 0,
      cur_elt_id: 0,
    };
  };
  let settings = Settings.init;
  let cursor_inspector = CursorInspectorModel.init;
  let selected_instances = {
    let si = UserSelectedInstances.init;
    switch (
      settings.evaluation.evaluate,
      cardstacks |> ZCardstacks.get_program |> Program.cursor_on_exp_hole,
    ) {
    | (false, _)
    | (_, None) => si
    | (true, Some(u)) => UserSelectedInstances.add(u, 0, si)
    };
  };
  {
    res: ModelResult.init,
    cardstacks,
    cell_width,
    selected_instances,
    undo_history,
    left_sidebar_open: false,
    right_sidebar_open: true,
    font_metrics:
      FontMetrics.{
        // to be set on display
        row_height: 1.0,
        col_width: 1.0,
      },
    mouse_position: ref(MousePosition.{x: 0, y: 0}),
    settings,
    cursor_inspector,
  };
};

let get_result = (model: t): ModelResult.t => model.res;
let put_result = (res: ModelResult.t, model: t): t => {...model, res};
let update_result = (current: ModelResult.current, model: t): t => {
  let res = model |> get_result |> ModelResult.update_current(current);
  model |> put_result(res);
};

let get_program = (model: t): Program.t =>
  model.cardstacks |> ZCardstacks.get_program;

let get_edit_state = (model: t): Statics.edit_state => {
  let program = get_program(model);
  program.edit_state;
};

let get_cursor_info = (model: t): CursorInfo.t =>
  model |> get_program |> Program.get_cursor_info;

let put_program = (program: Program.t, model: t): t => {
  ...model,
  cardstacks: model.cardstacks |> ZCardstacks.put_program(program),
};
let map_program = (f: Program.t => Program.t, model: t): t => {
  let new_program = f(model |> get_program);
  model |> put_program(new_program);
};

let get_undo_history = (model: t): UndoHistory.t => model.undo_history;
let put_undo_history = (history: UndoHistory.t, model: t): t => {
  ...model,
  undo_history: history,
};

let get_cardstacks = model => model.cardstacks;
let put_cardstacks = (cardstacks, model) => {...model, cardstacks};
let map_cardstacks = (f: ZCardstacks.t => ZCardstacks.t, model: t): t => {
  let new_cardstacks = f(model |> get_cardstacks);
  model |> put_cardstacks(new_cardstacks);
};

let get_cardstack = model => model |> get_cardstacks |> ZCardstacks.get_z;
let get_card = model => model |> get_cardstack |> Cardstack.get_z;

let get_cards_info = (model: t): list(CardInfo.t) =>
  switch (
    model.cardstacks |> ZList.prefix_length |> List.nth_opt(cardstack_info)
  ) {
  | None => []
  | Some(cardinfo) => cardinfo.cards
  };

let map_selected_instances =
    (f: UserSelectedInstances.t => UserSelectedInstances.t, model) => {
  ...model,
  selected_instances: f(model.selected_instances),
};

let focus_cell = map_program(Program.focus);
let blur_cell = map_program(Program.blur);

let is_cell_focused = model => {
  let program = get_program(model);
  program.is_focused;
};

let get_selected_hole_instance = model =>
  switch (model |> get_program |> Program.cursor_on_exp_hole) {
  | None => None
  | Some(u) =>
    let i =
      model.selected_instances
      |> UserSelectedInstances.find_opt(u)
      |> Option.get;
    Some((u, i));
  };

let select_hole_instance = ((u, i): HoleInstance.t, model: t): t =>
  model
  |> map_program(program => {
       let action = Program.move_to_hole(u, program);
       Program.perform_edit_action(action, program);
     })
  |> map_selected_instances(UserSelectedInstances.add(u, i))
  |> focus_cell;

let update_program = (a: ModelAction.t, new_program, model) => {
  /* TODO: Need to fix this since web worker changes, but leaving for haz3l. */
  /* let old_program = model |> get_program; */
  let update_selected_instances = _si => {
    let si =
      /* Program.get_result(old_program) == Program.get_result(new_program) ? si :  */
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
};

let prev_card = model => {
  model
  |> map_cardstacks(ZCardstacks.map_z(Cardstack.prev_card))
  |> focus_cell;
};
let next_card = model => {
  model
  |> map_cardstacks(ZCardstacks.map_z(Cardstack.next_card))
  |> focus_cell;
};

let nth_card = (n, model) => {
  model
  |> map_cardstacks(ZCardstacks.map_z(Cardstack.nth_card(n)))
  |> focus_cell;
};

let perform_edit_action = (a: Action.t, model: t): t => {
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
    (path_to_case: CursorPath.steps, branch_index: int, model: t): t => {
  let program = model |> get_program;
  let action = Program.move_to_case_branch(path_to_case, branch_index);
  let new_program = Program.perform_edit_action(action, program);
  let model_action = ModelAction.EditAction(action);
  model
  |> put_program(new_program)
  |> update_program(model_action, new_program)
  |> focus_cell;
};

let toggle_left_sidebar = (model: t): t => {
  ...model,
  left_sidebar_open: !model.left_sidebar_open,
};
let toggle_right_sidebar = (model: t): t => {
  ...model,
  right_sidebar_open: !model.right_sidebar_open,
};

let load_cardstack = (model, idx) => {
  model |> map_cardstacks(ZCardstacks.load_cardstack(idx)) |> focus_cell;
};

let load_undo_history =
    (model: t, undo_history: UndoHistory.t, ~is_after_move: bool): t => {
  let new_cardstacks =
    UndoHistory.get_cardstacks(undo_history, ~is_after_move);
  let new_program = ZCardstacks.get_program(new_cardstacks);
  let update_selected_instances = _ => {
    let si = UserSelectedInstances.init;
    switch (Program.cursor_on_exp_hole(new_program)) {
    | None => si
    | Some(u) => si |> UserSelectedInstances.add(u, 0)
    };
  };
  model
  |> put_undo_history(undo_history)
  |> put_cardstacks(new_cardstacks)
  |> map_selected_instances(update_selected_instances);
};
