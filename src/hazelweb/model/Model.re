open OptUtil.Syntax;

type t = {
  cardstacks: ZCardstacks.t,
  cell_width: int,
  selected_instances: UserSelectedInstances.t,
  undo_history: UndoHistory.t,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  font_metrics: FontMetrics.t,
  is_mac: bool,
  mouse_position: ref(MousePosition.t),
  settings: Settings.t,
};

let cutoff = (m1, m2) => m1 === m2;

let cardstack_info = [
  TutorialCards.cardstack,
  // RCStudyCards.cardstack,
];

let init = (): t => {
  let cell_width = 100;
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
      previous_action: Init,
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
  let selected_instances = {
    let si = UserSelectedInstances.init;
    switch (
      settings.evaluation.evaluate,
      cardstacks |> ZCardstacks.get_program |> Program.cursor_on_inst,
    ) {
    | (false, _)
    | (_, None) => si
    | (true, Some((kind, u))) =>
      UserSelectedInstances.add((kind, (u, 0)), si)
    };
  };
  {
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
    is_mac: true,
    mouse_position: ref(MousePosition.{x: 0, y: 0}),
    settings,
  };
};

let get_program = (model: t): Program.t =>
  model.cardstacks |> ZCardstacks.get_program;

let get_cursor_info = (model: t): option(CursorInfo.t) =>
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

let map_selected_instances =
    (f: UserSelectedInstances.t => UserSelectedInstances.t, model) => {
  ...model,
  selected_instances: f(model.selected_instances),
};

let get_selected_instance = model =>
  switch (model |> get_program |> Program.cursor_on_inst) {
  | None => None
  | Some((kind, u)) =>
    model.selected_instances
    |> UserSelectedInstances.find_opt(kind, u)
    |> Option.map(i => (kind, (u, i)))
  };

let select_instance =
    ((kind, (u, _)) as tni: TaggedNodeInstance.t, model: t): t =>
  model
  |> map_program(program => {
       let action = Program.move_to_node(kind, u, program);
       Program.perform_action(~settings=model.settings, action, program);
     })
  |> map_selected_instances(UserSelectedInstances.add(tni));

let prev_card = model => {
  model
  |> map_cardstacks(ZCardstacks.map_z(Cardstack.prev_card))
  |> map_program(Program.focus);
};
let next_card = model => {
  model
  |> map_cardstacks(ZCardstacks.map_z(Cardstack.next_card))
  |> map_program(Program.focus);
};

let perform_action =
    (~move_via: option(MoveInput.t)=?, a: Action.t, model: t): t => {
  let settings = model.settings;
  TimeUtil.measure_time(
    "Model.perform_action",
    settings.performance.measure
    && settings.performance.model_perform_edit_action,
    () => {
      let old_program = get_program(model);
      let new_program =
        Program.perform_action(~settings, ~move_via?, a, old_program);
      let update_selected_instances = si => {
        let si =
          Program.get_result(old_program) == Program.get_result(new_program)
            ? si : UserSelectedInstances.init;
        switch (
          model.settings.evaluation.evaluate,
          new_program |> Program.cursor_on_inst,
        ) {
        | (false, _)
        | (_, None) => si
        | (true, Some((kind, u))) =>
          switch (si |> UserSelectedInstances.find_opt(kind, u)) {
          | None => si |> UserSelectedInstances.add((kind, (u, 0)))
          | Some(_) => si
          }
        };
      };
      model
      |> put_program(new_program)
      |> map_selected_instances(update_selected_instances)
      |> put_undo_history(
           {
             let history = get_undo_history(model);
             let new_cardstacks =
               model |> put_program(new_program) |> get_cardstacks;
             switch (ZCardstacks.get_program(new_cardstacks).edit_state.term) {
             | Unfocused(_) => failwith("history entry without cursor")
             | Focused(_) => ()
             };
             UndoHistory.push_edit_state(history, new_cardstacks, a);
           },
         );
    },
  );
};

let move_via_key = (move_key, model) => {
  let+ target =
    model
    |> get_program
    |> Program.target_path_of_key_input(~settings=model.settings, move_key);
  perform_action(~move_via=Key(move_key), MoveTo(target), model);
};

let move_via_click = (opt_splice, row_col, model) => {
  let target =
    model
    |> get_program
    |> Program.target_path_of_click_input(
         ~settings=model.settings,
         opt_splice,
         row_col,
       );
  perform_action(
    ~move_via=Click(opt_splice, row_col),
    MoveTo(target),
    model,
  );
};

let select_case_branch =
    (path_to_case: CursorPath.steps, branch_index: int, model: t): t => {
  let a = Program.move_to_case_branch(path_to_case, branch_index);
  perform_action(a, model);
};

let toggle_left_sidebar = (model: t): t => {
  ...model,
  left_sidebar_open: !model.left_sidebar_open,
};
let toggle_right_sidebar = (model: t): t => {
  ...model,
  right_sidebar_open: !model.right_sidebar_open,
};

let load_example = (model: t, e: UHExp.t): t => {
  let (ze, ty, u_gen) =
    Statics_Exp.fix_and_renumber_holes_z(
      (BuiltinFunctions.ctx, Livelits.initial_livelit_ctx),
      ZExp.place_before(e),
    );
  let edit_state: Program.EditState.t = {term: Focused(ze), ty, u_gen};
  model |> put_program(Program.mk(~width=model.cell_width, edit_state));
};

let load_cardstack = (model, idx) => {
  model
  |> map_cardstacks(ZCardstacks.load_cardstack(idx))
  |> map_program(Program.focus);
};

let load_undo_history =
    (model: t, undo_history: UndoHistory.t, ~is_after_move: bool): t => {
  let new_cardstacks =
    UndoHistory.get_cardstacks(undo_history, ~is_after_move);
  let new_program = ZCardstacks.get_program(new_cardstacks);
  let update_selected_instances = _ => {
    let si = UserSelectedInstances.init;
    switch (Program.cursor_on_inst(new_program)) {
    | None => si
    | Some((kind, u)) => UserSelectedInstances.add((kind, (u, 0)), si)
    };
  };
  model
  |> put_undo_history(undo_history)
  |> put_cardstacks(new_cardstacks)
  |> map_selected_instances(update_selected_instances);
};
