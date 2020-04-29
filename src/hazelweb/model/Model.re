type t = {
  cardstacks: Cardstacks.t,
  cell_width: int,
  selected_instances: UserSelectedInstances.t,
  undo_history: UndoHistory.t,
  compute_results: bool,
  show_case_clauses: bool,
  show_fn_bodies: bool,
  show_casts: bool,
  show_unevaluated_expansion: bool,
  selected_example: option(UHExp.t),
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  show_contenteditable: bool,
  show_presentation: bool,
  font_metrics: FontMetrics.t,
};

let cutoff = (m1, m2) => m1 === m2;

let cardstack_info = [
  TutorialCards.cardstack,
  // RCStudyCards.cardstack,
];

let init = (): t => {
  let cell_width = 80;
  let cardstacks = Cardstacks.mk(~width=cell_width, cardstack_info);
  let undo_history: UndoHistory.t = {
    let cursor_term_info =
      UndoHistory.get_cursor_info(
        ~cardstacks_after=cardstacks,
        ~cardstacks_before=cardstacks,
      );
    let undo_history_entry: UndoHistory.undo_history_entry = {
      cardstacks,
      cursor_term_info,
      previous_action: Construct(SOp(SSpace)),
      edit_action: Ignore,
    };
    let timestamp = Unix.time();
    let undo_history_group: UndoHistory.undo_history_group = {
      group_entries: ([], undo_history_entry, []),
      is_expanded: false,
      timestamp,
      display_timestamp: false,
    };
    {
      groups: ([], undo_history_group, []),
      latest_timestamp: timestamp,
      all_hidden_history_expand: false,
    };
  };
  let compute_results = true;
  let selected_instances = {
    let si = UserSelectedInstances.init;
    switch (
      compute_results,
      cardstacks |> Cardstacks.get_program |> Program.cursor_on_exp_hole,
    ) {
    | (false, _)
    | (_, None) => si
    | (true, Some(u)) => si |> UserSelectedInstances.insert_or_update((u, 0))
    };
  };
  {
    cardstacks,
    cell_width,
    selected_instances,
    undo_history,
    compute_results,
    show_case_clauses: false,
    show_fn_bodies: false,
    show_casts: false,
    show_unevaluated_expansion: false,
    selected_example: None,
    left_sidebar_open: false,
    right_sidebar_open: true,
    show_contenteditable: false,
    show_presentation: false,
    font_metrics:
      FontMetrics.{
        // to be set on display
        row_height: 1.0,
        col_width: 1.0,
      },
  };
};

let get_program = (model: t): Program.t =>
  model.cardstacks |> Cardstacks.get_program;
let put_program = (program: Program.t, model: t): t => {
  ...model,
  cardstacks: model.cardstacks |> Cardstacks.put_program(program),
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
let map_cardstacks = (f: Cardstacks.t => Cardstacks.t, model: t): t => {
  let new_cardstacks = f(model |> get_cardstacks);
  model |> put_cardstacks(new_cardstacks);
};

let get_cardstack = model => model |> get_cardstacks |> Cardstacks.get_z;
let get_card = model => model |> get_cardstack |> Cardstack.get_z;

let map_selected_instances =
    (f: UserSelectedInstances.t => UserSelectedInstances.t, model) => {
  ...model,
  selected_instances: f(model.selected_instances),
};

let focus_cell = map_program(Program.focus);
let blur_cell = map_program(Program.blur);

let is_cell_focused = model => model |> get_program |> Program.is_focused;

let get_selected_hole_instance = model =>
  switch (model |> get_program |> Program.cursor_on_exp_hole) {
  | None => None
  | Some(u) =>
    let i =
      model.selected_instances
      |> UserSelectedInstances.lookup(u)
      |> Option.get;
    Some((u, i));
  };

let select_hole_instance = ((u, _) as inst: HoleInstance.t, model: t): t =>
  model
  |> map_program(Program.move_to_hole(u))
  |> map_selected_instances(UserSelectedInstances.insert_or_update(inst))
  |> focus_cell;

let update_program = (~undoable, new_program, model) => {
  let old_program = model |> get_program;
  let update_selected_instances = si => {
    let si =
      Program.get_result(old_program) == Program.get_result(new_program)
        ? si : UserSelectedInstances.init;
    switch (model.compute_results, new_program |> Program.cursor_on_exp_hole) {
    | (false, _)
    | (_, None) => si
    | (true, Some(u)) =>
      switch (si |> UserSelectedInstances.lookup(u)) {
      | None => si |> UserSelectedInstances.insert_or_update((u, 0))
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
  |> map_cardstacks(Cardstacks.map_z(Cardstack.prev_card))
  |> focus_cell;
};
let next_card = model => {
  model
  |> map_cardstacks(Cardstacks.map_z(Cardstack.next_card))
  |> focus_cell;
};

let perform_edit_action = (a: Action.t, model: t): t => {
  let new_program = model |> get_program |> Program.perform_edit_action(a);
  model
  |> update_program(~undoable=UndoHistory.undoable_action(a), new_program);
};

let move_via_key = (move_key, model) => {
  let new_program = model |> get_program |> Program.move_via_key(move_key);
  model |> update_program(~undoable=false, new_program);
};

let move_via_click = (row_col, model) => {
  let new_program = model |> get_program |> Program.move_via_click(row_col);
  model |> update_program(~undoable=false, new_program);
};

let toggle_left_sidebar = (model: t): t => {
  ...model,
  left_sidebar_open: !model.left_sidebar_open,
};
let toggle_right_sidebar = (model: t): t => {
  ...model,
  right_sidebar_open: !model.right_sidebar_open,
};

let load_example = (model: t, e: UHExp.t): t =>
  model
  |> put_program(
       Program.mk(
         ~width=model.cell_width,
         Statics.Exp.fix_and_renumber_holes_z(
           Contexts.empty,
           ZExp.place_before(e),
         ),
       ),
     );

let load_cardstack = (model, idx) => {
  model |> map_cardstacks(Cardstacks.load_cardstack(idx)) |> focus_cell;
};
