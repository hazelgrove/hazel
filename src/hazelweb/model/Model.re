type compute_results = {
  compute_results: bool,
  show_case_clauses: bool,
  show_fn_bodies: bool,
  show_casts: bool,
  show_unevaluated_expansion: bool,
};

type measurements = {
  measurements: bool,
  model_perform_edit_action: bool,
  program_get_doc: bool,
  layoutOfDoc_layout_of_doc: bool,
  uhcode_view: bool,
  cell_view: bool,
  page_view: bool,
  hazel_create: bool,
  update_apply_action: bool,
};

type cursor_inspector = {
  visible: bool,
  show_expanded: bool,
  term_novice_message_mode: bool,
  type_novice_message_mode: bool,
};

type t = {
  cardstacks: ZCardstacks.t,
  cell_width: int,
  selected_instances: UserSelectedInstances.t,
  undo_history: UndoHistory.t,
  compute_results,
  measurements,
  memoize_doc: bool,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  font_metrics: FontMetrics.t,
  is_mac: bool,
  mouse_position: ref(MousePosition.t),
  cursor_inspector,
  synthesizing: option((MetaVar.t, int, list(UHExp.t) /* + constraints */)),
};

let cutoff = (m1, m2) => m1 === m2;

let cardstack_info = [
  TutorialCards.cardstack,
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
  let compute_results = true;
  let selected_instances = {
    let si = UserSelectedInstances.init;
    switch (
      compute_results,
      cardstacks |> ZCardstacks.get_program |> Program.cursor_on_exp_hole,
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
    compute_results: {
      compute_results,
      show_case_clauses: false,
      show_fn_bodies: false,
      show_casts: false,
      show_unevaluated_expansion: false,
    },
    measurements: {
      measurements: false,
      model_perform_edit_action: true,
      program_get_doc: true,
      layoutOfDoc_layout_of_doc: true,
      uhcode_view: true,
      cell_view: true,
      page_view: true,
      hazel_create: true,
      update_apply_action: true,
    },
    memoize_doc: true,
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
    cursor_inspector: {
      visible: true,
      show_expanded: false,
      term_novice_message_mode: false,
      type_novice_message_mode: false,
    },
    synthesizing: None,
  };
};

let get_novice_mode = (model: t): bool =>
  model.cursor_inspector.term_novice_message_mode
  && model.cursor_inspector.type_novice_message_mode;

let toggle_novice_mode = (model: t): t => {
  let toggle = get_novice_mode(model);
  {
    ...model,
    cursor_inspector: {
      ...model.cursor_inspector,
      term_novice_message_mode: !toggle,
      type_novice_message_mode: !toggle,
    },
  };
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

let update_program = (a: Action.t, new_program, model) => {
  let old_program = model |> get_program;
  let update_selected_instances = si => {
    let si =
      fst(Program.get_result(old_program))
      == fst(Program.get_result(new_program))
        ? si : UserSelectedInstances.init;
    switch (
      model.compute_results.compute_results,
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

let perform_edit_action = (a: Action.t, model: t): t => {
  TimeUtil.measure_time(
    "Model.perform_edit_action",
    model.measurements.measurements
    && model.measurements.model_perform_edit_action,
    () => {
      let new_program =
        model |> get_program |> Program.perform_edit_action(a);
      model |> update_program(a, new_program);
    },
  );
};

[@warning "-27"]
let synthesize = (u: MetaVar.t, model: t): t => {
  /*
     Shmyth.fill
     - will require a little more deconstruction of the current program
       to get relevant contexts etc centered around the hole
     - call Shmyth.fill
     - populate model.synthesizing with results
   */
  switch (Shmyth.solve(Program.get_uhexp(get_program(model)))) {
  | None =>
    print_endline("synth error");
    model;
  | Some([]) =>
    print_endline("no synth results");
    model;
  | Some([synthesized, ..._]) =>
    synthesized
    |> List.fold_left(
         (model, (u, synthesized)) =>
           perform_edit_action(FillExpHole(u, synthesized), model),
         model,
       )
  };
};

[@warning "-32"]
let accept_filling = (_: t): t => {
  /* just perform FillExpHole with selected filling
     clear model.synthesizing */
  failwith(
    "todo",
  );
};

let move_via_key = (move_key, model) => {
  let (new_program, action) =
    model
    |> get_program
    |> Program.move_via_key(
         ~measure_program_get_doc=
           model.measurements.measurements
           && model.measurements.program_get_doc,
         ~measure_layoutOfDoc_layout_of_doc=
           model.measurements.measurements
           && model.measurements.layoutOfDoc_layout_of_doc,
         ~memoize_doc=model.memoize_doc,
         move_key,
       );
  model |> update_program(action, new_program);
};

let move_via_click = (row_col, model) => {
  let (new_program, action) =
    model
    |> get_program
    |> Program.move_via_click(
         ~measure_program_get_doc=
           model.measurements.measurements
           && model.measurements.program_get_doc,
         ~measure_layoutOfDoc_layout_of_doc=
           model.measurements.measurements
           && model.measurements.layoutOfDoc_layout_of_doc,
         ~memoize_doc=model.memoize_doc,
         row_col,
       );
  model |> update_program(action, new_program);
};

let select_case_branch =
    (path_to_case: CursorPath.steps, branch_index: int, model: t): t => {
  let program = model |> get_program;
  let action = Program.move_to_case_branch(path_to_case, branch_index);
  let new_program = Program.perform_edit_action(action, program);
  model
  |> put_program(new_program)
  |> update_program(action, new_program)
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

let load_example = (model: t, e: UHExp.t): t =>
  model
  |> put_program(
       Program.mk(
         ~width=model.cell_width,
         Statics_Exp.fix_and_renumber_holes_z(
           Contexts.empty,
           ZExp.place_before(e),
         ),
       ),
     );

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
