[@deriving sexp]
type editor =
  | MainProgram
  | AssistantTypeEditor
  | NoFocus;

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
  focal_editor: editor,
  assistant: AssistantModel.t,
};

let editor_id = (editor: editor): string =>
  switch (editor) {
  | MainProgram => "code-root"
  | AssistantTypeEditor => "typefilter"
  | NoFocus => ""
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
      cardstacks
      |> ZCardstacks.get_program
      |> Editor.get_edit_state
      |> Editor.EditState_Exp.cursor_on_exp_hole,
    ) {
    | (false, _)
    | (_, None) => si
    | (true, Some(u)) => UserSelectedInstances.add(u, 0, si)
    };
  };
  {
    cardstacks,
    cell_width,
    selected_instances,
    undo_history,
    left_sidebar_open: false,
    right_sidebar_open: false, // TODO(andrew): restore default when merging
    font_metrics:
      FontMetrics.{
        // to be set on display
        row_height: 1.0,
        col_width: 1.0,
      },
    is_mac: true,
    mouse_position: ref(MousePosition.{x: 0, y: 0}),
    settings,
    focal_editor: MainProgram,
    assistant: AssistantModel.init,
  };
};

let get_program = (model: t): Editor.exp =>
  model.cardstacks |> ZCardstacks.get_program;
let put_assistant_model = (model, assistant) => {...model, assistant};

let get_edit_state = (model: t): Editor.EditState_Exp.t => {
  let program = get_program(model);
  program.edit_state;
};

let get_cursor_info = (model: t): CursorInfo.t =>
  model |> get_program |> Editor.Exp.get_cursor_info;

let put_program = (program: Editor.exp, model: t): t => {
  ...model,
  cardstacks: model.cardstacks |> ZCardstacks.put_program(program),
};
let map_program = (f: Editor.exp => Editor.exp, model: t): t => {
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

let get_focal_editor = model => model.focal_editor;
let put_focal_editor = (id, model) => {...model, focal_editor: id};

let focus_main_editor = put_focal_editor(MainProgram);
let blur_cell = model => model |> put_focal_editor(NoFocus);

let get_selected_hole_instance = model =>
  switch (
    model
    |> get_program
    |> Editor.get_edit_state
    |> Editor.EditState_Exp.cursor_on_exp_hole
  ) {
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
       let action = Editor.Exp.move_to_hole(u, program);
       let edit_state =
         Editor.EditState_Exp.perform_edit_action(action, program.edit_state);
       {...program, edit_state};
     })
  |> map_selected_instances(UserSelectedInstances.add(u, i))
  |> focus_main_editor;

let update_program = (a: Action.t, new_program, model) => {
  let edit_state = Editor.get_edit_state(new_program);
  let old_program = model |> get_program;
  let update_selected_instances = si => {
    let si =
      Editor.EditState_Exp.get_result(old_program.edit_state)
      == Editor.EditState_Exp.get_result(edit_state)
        ? si : UserSelectedInstances.init;
    switch (
      model.settings.evaluation.evaluate,
      edit_state |> Editor.EditState_Exp.cursor_on_exp_hole,
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
  |> focus_main_editor;
};
let next_card = model => {
  model
  |> map_cardstacks(ZCardstacks.map_z(Cardstack.next_card))
  |> focus_main_editor;
};

let update_filter_editor = (a: Action.t, new_editor, model: t): t => {
  AssistantModel.update_filter_editor(a, new_editor, model.assistant)
  |> put_assistant_model(model);
};

/*
 let set_assistant_hover_index = (hover_index, model: t) => {
   AssistantModel.set_hover_index(hover_index, model.assistant)
   |> put_assistant_model(model);
 };*/

let nth_card = (n, model) => {
  model
  |> map_cardstacks(ZCardstacks.map_z(Cardstack.nth_card(n)))
  |> focus_main_editor;
};

let perform_edit_action = (a: Action.t, model: t): t => {
  TimeUtil.measure_time(
    "Model.perform_edit_action",
    model.settings.performance.measure
    && model.settings.performance.model_perform_edit_action,
    () => {
    switch (get_focal_editor(model)) {
    | AssistantTypeEditor =>
      update_filter_editor(a, model.assistant.filter_editor, model)
    | MainProgram
    | NoFocus =>
      let edit_state =
        model
        |> get_program
        |> Editor.get_edit_state
        |> Editor.EditState_Exp.perform_edit_action(a);
      model |> update_program(a, {...get_program(model), edit_state});
    }
  });
};

let move_via_key = (move_key, model) => {
  switch (get_focal_editor(model)) {
  | AssistantTypeEditor =>
    let (new_editor, action) =
      model.assistant.filter_editor
      |> Editor.Typ.move_via_key(~settings=model.settings, move_key);
    update_filter_editor(action, new_editor, model);
  | MainProgram
  | NoFocus =>
    let (new_program, action) =
      model
      |> get_program
      |> Editor.Exp.move_via_key(~settings=model.settings, move_key);
    model |> update_program(action, new_program);
  };
};

let move_via_click = (row_col, model) => {
  switch (get_focal_editor(model)) {
  | AssistantTypeEditor =>
    let (new_editor, action) =
      model.assistant.filter_editor
      |> Editor.Typ.move_via_click(~settings=model.settings, row_col);
    update_filter_editor(action, new_editor, model);
  | MainProgram
  | NoFocus =>
    let (new_program, action) =
      model
      |> get_program
      |> Editor.Exp.move_via_click(~settings=model.settings, row_col);
    model |> update_program(action, new_program);
  };
};

let select_case_branch =
    (path_to_case: CursorPath.steps, branch_index: int, model: t): t => {
  let program = model |> get_program;
  let edit_state = Editor.get_edit_state(program);
  let action =
    Editor.EditState_Exp.move_to_case_branch(path_to_case, branch_index);
  let new_edit_state =
    Editor.EditState_Exp.perform_edit_action(action, edit_state);
  let new_program = {...program, edit_state: new_edit_state};
  model
  |> put_program(new_program)
  |> update_program(action, new_program)
  |> focus_main_editor;
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
  model
  |> map_cardstacks(ZCardstacks.load_cardstack(idx))
  |> focus_main_editor;
};

let load_undo_history =
    (model: t, undo_history: UndoHistory.t, ~is_after_move: bool): t => {
  let new_cardstacks =
    UndoHistory.get_cardstacks(undo_history, ~is_after_move);
  let new_program = ZCardstacks.get_program(new_cardstacks);
  let edit_state = Editor.get_edit_state(new_program);
  let update_selected_instances = _ => {
    let si = UserSelectedInstances.init;
    switch (Editor.EditState_Exp.cursor_on_exp_hole(edit_state)) {
    | None => si
    | Some(u) => si |> UserSelectedInstances.add(u, 0)
    };
  };
  model
  |> put_undo_history(undo_history)
  |> put_cardstacks(new_cardstacks)
  |> map_selected_instances(update_selected_instances);
};
