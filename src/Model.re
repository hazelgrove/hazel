open Sexplib.Std;
open GeneralUtil;

module ZList = GeneralUtil.ZList;

let init_cardstack = RCStudyCards.cardstack;
//let init_cardstack = TutorialCards.cardstack;
let init_compute_results_flag = false;

type user_newlines = Path.StepsMap.t(unit);

type edit_state = Statics.edit_state;

type card_state = {
  card: Card.t,
  edit_state,
};

type cardstack_state = ZList.t(card_state, card_state);

let mk_cardstack_state = cardstack => {
  let card_states =
    List.map(
      card =>
        {
          card,
          edit_state:
            card.init_block
            |> ZExp.place_before_block
            |> Statics.fix_and_renumber_holes_z(Contexts.empty),
        },
      cardstack,
    );
  GeneralUtil.Opt.get(
    _ => failwith("no cards"),
    ZList.split_at(0, card_states),
  );
};

[@deriving sexp]
type result = (
  Dynamics.DHExp.t,
  Dynamics.DHExp.HoleInstanceInfo.t,
  Dynamics.Evaluator.result,
);

module UserSelectedInstances = {
  [@deriving sexp]
  type t = MetaVarMap.t(Dynamics.inst_num);
  let init = MetaVarMap.empty;
  let update = (usi, inst) => MetaVarMap.insert_or_update(usi, inst);
};

[@deriving sexp]
type context_inspector = {
  next_state: option(Dynamics.DHExp.HoleInstance.t),
  prev_state: option(Dynamics.DHExp.HoleInstance.t),
};

type has_result_state = {
  result,
  context_inspector,
  user_selected_instances: UserSelectedInstances.t,
  selected_instance: option((MetaVar.t, Dynamics.inst_num)),
};

type result_state =
  | ResultsDisabled
  | Result(has_result_state);

type t = {
  cardstack_state,
  /* these are derived from the cardstack state: */
  cursor_info: CursorInfo.t,
  compute_results_flag: bool,
  result_state,
  /* UI state */
  user_newlines,
  selected_example: option(UHExp.block),
  is_cell_focused: bool,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
};

let edit_state_of = model => ZList.prj_z(model.cardstack_state).edit_state;

let cutoff = (m1, m2) => m1 == m2;

let zblock = model => {
  let (zblock, _, _) = edit_state_of(model);
  zblock;
};

let block = model => model |> zblock |> ZExp.erase_block;

let path = model => {
  let (zblock, _, _) = edit_state_of(model);
  Path.of_zblock(zblock);
};

let steps = model => {
  let (steps, _) = model |> path;
  steps;
};

let u_gen = model => {
  let (_, _, u_gen) = edit_state_of(model);
  u_gen;
};

exception MissingCursorInfo;
let cursor_info_of_edit_state = ((zblock, _, _): edit_state): CursorInfo.t =>
  switch (
    CursorInfo.syn_cursor_info_block(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      zblock,
    )
  ) {
  | None => raise(MissingCursorInfo)
  | Some(ci) => ci
  };

exception InvalidInput;
exception DoesNotExpand;
let result_of_edit_state = ((zblock, _, _): edit_state): result => {
  open Dynamics;
  let expanded =
    DHExp.syn_expand_block(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      Delta.empty,
      ZExp.erase_block(zblock),
    );
  switch (expanded) {
  | DoesNotExpand => raise(DoesNotExpand)
  | Expands(d, _, _) =>
    switch (Evaluator.evaluate(d)) {
    | InvalidInput(n) =>
      JSUtil.log("InvalidInput " ++ string_of_int(n));
      raise(InvalidInput);
    | BoxedValue(d) =>
      let (d_renumbered, hii) =
        DHExp.renumber([], DHExp.HoleInstanceInfo.empty, d);
      (d_renumbered, hii, BoxedValue(d_renumbered));
    | Indet(d) =>
      let (d_renumbered, hii) =
        DHExp.renumber([], DHExp.HoleInstanceInfo.empty, d);
      (d_renumbered, hii, Indet(d_renumbered));
    }
  };
};

let result_state_of_edit_state = (edit_state, compute_results_flag) =>
  if (!compute_results_flag) {
    ResultsDisabled;
  } else {
    Result({
      result: result_of_edit_state(edit_state),
      user_selected_instances: UserSelectedInstances.init,
      selected_instance: None,
      context_inspector: {
        next_state: None,
        prev_state: None,
      },
    });
  };

let remove_keys_outside_prefix =
    (~prefix: Path.steps, user_newlines): (user_newlines, list(Path.steps)) => {
  let (inside_prefix, outside_prefix) =
    user_newlines
    |> Path.StepsMap.partition((steps, _) =>
         prefix |> Path.is_prefix_of(steps)
       );
  (
    inside_prefix,
    outside_prefix
    |> Path.StepsMap.bindings
    |> List.map(((steps, _)) => steps),
  );
};

let add_user_newline = (steps, model) => {
  ...model,
  user_newlines: model.user_newlines |> Path.StepsMap.add(steps, ()),
};

let remove_user_newline = (steps, model) => {
  ...model,
  user_newlines: model.user_newlines |> Path.StepsMap.remove(steps),
};

let user_entered_newline_at = (steps, model): bool =>
  model.user_newlines |> Path.StepsMap.mem(steps);

let update_edit_state = ((new_zblock, ty, u_gen): edit_state, model: t): t => {
  let new_block = new_zblock |> ZExp.erase_block;
  let (new_steps, _) = Path.of_zblock(new_zblock);
  let (new_zblock, new_user_newlines) =
    model.user_newlines
    |> Path.StepsMap.bindings
    |> List.fold_left(
         ((new_zblock, newlines), (steps, _)) =>
           switch (new_block |> Path.follow_block_and_place_after(steps)) {
           | None => (new_zblock, newlines |> Path.StepsMap.remove(steps))
           | Some(zblock) =>
             switch (CursorInfo.syn_cursor_info_block(Contexts.empty, zblock)) {
             | None => (new_zblock, newlines |> Path.StepsMap.remove(steps))
             | Some(ci) =>
               let newlines =
                 switch (ci.node) {
                 | Exp(EmptyHole(_))
                 | Line(EmptyLine) => newlines
                 | _ =>
                   // something nontrivial present, no more
                   // need to keep track of ephemeral newline
                   newlines |> Path.StepsMap.remove(steps)
                 };
               if (Path.compare_steps(new_steps, steps) < 0) {
                 let new_path = Path.of_zblock(new_zblock);
                 (
                   new_zblock
                   |> ZExp.erase_block
                   |> Path.prune_trivial_suffix_block(
                        ~steps_of_first_line=steps,
                      )
                   |> Path.follow_block(new_path)
                   |> Opt.get(() => assert(false)),
                   newlines |> Path.StepsMap.remove(steps),
                 );
               } else {
                 (new_zblock, newlines);
               };
             }
           },
         (new_zblock, model.user_newlines),
       );
  let new_edit_state = (new_zblock, ty, u_gen);
  let new_result_state =
    result_state_of_edit_state(new_edit_state, model.compute_results_flag);
  let cardstack_state = model.cardstack_state;
  let card_state = ZList.prj_z(cardstack_state);
  let new_card_state = {...card_state, edit_state: new_edit_state};
  let new_cardstack_state = ZList.replace_z(cardstack_state, new_card_state);
  let new_cursor_info = cursor_info_of_edit_state(new_edit_state);
  {
    ...model,
    cardstack_state: new_cardstack_state,
    cursor_info: new_cursor_info,
    result_state: new_result_state,
    user_newlines: new_user_newlines,
  };
};

let update_cardstack_state = (model, cardstack_state) => {
  let edit_state = ZList.prj_z(cardstack_state).edit_state;
  let result_state =
    result_state_of_edit_state(edit_state, model.compute_results_flag);
  let cursor_info = cursor_info_of_edit_state(edit_state);
  let user_newlines = Path.StepsMap.empty;
  {
    ...model,

    cardstack_state,
    result_state,
    cursor_info,
    user_newlines,
  };
};

let prev_card = model => {
  let cardstack_state = ZList.shift_prev(model.cardstack_state);
  {
    ...update_cardstack_state(model, cardstack_state),

    is_cell_focused: true,
  };
};

let next_card = model => {
  let cardstack_state = ZList.shift_next(model.cardstack_state);
  {
    ...update_cardstack_state(model, cardstack_state),

    is_cell_focused: true,
  };
};

let init = (): t => {
  let cardstack_state = mk_cardstack_state(init_cardstack);
  let edit_state = ZList.prj_z(cardstack_state).edit_state;
  let compute_results_flag = init_compute_results_flag;
  {
    cardstack_state,
    cursor_info: cursor_info_of_edit_state(edit_state),
    compute_results_flag,
    result_state:
      result_state_of_edit_state(edit_state, compute_results_flag),
    left_sidebar_open: false,
    right_sidebar_open: true,
    selected_example: None,
    is_cell_focused: false,
    user_newlines: Path.StepsMap.empty,
  };
};

exception FailedAction;
exception CursorEscaped;
exception CantShift;
let perform_edit_action = (model: t, a: Action.t): t => {
  switch (
    Action.syn_perform_block(
      ~ci=model.cursor_info,
      (VarCtx.empty, Palettes.initial_palette_ctx),
      a,
      edit_state_of(model),
    )
  ) {
  | Failed => raise(FailedAction)
  | CursorEscaped(_) => raise(CursorEscaped)
  | CantShift => raise(CantShift)
  | Succeeded(new_edit_state) => model |> update_edit_state(new_edit_state)
  };
};

let move_to_hole = (model: t, u: MetaVar.t): t => {
  let (zblock, _, _) = ZList.prj_z(model.cardstack_state).edit_state;
  switch (
    Path.path_to_hole(Path.holes_block(ZExp.erase_block(zblock), [], []), u)
  ) {
  | None =>
    JSUtil.log("Path not found!");
    model;
  | Some(hole_path) => perform_edit_action(model, Action.MoveTo(hole_path))
  };
};

let select_hole_instance = (model, (u, i) as inst) => {
  switch (model.result_state) {
  | ResultsDisabled => model
  | Result(has_result_state) =>
    let (_, hii, _) = has_result_state.result;
    let has_result_state = {
      ...has_result_state,
      user_selected_instances:
        UserSelectedInstances.update(
          has_result_state.user_selected_instances,
          inst,
        ),
      selected_instance: Some(inst),
      context_inspector: {
        prev_state: i > 0 ? Some((u, i - 1)) : None,
        next_state:
          i < Dynamics.DHExp.HoleInstanceInfo.num_instances(hii, u) - 1
            ? Some((u, i + 1)) : None,
      },
    };
    {...model, result_state: Result(has_result_state)};
  };
};

let toggle_left_sidebar = (model: t): t => {
  ...model,
  left_sidebar_open: !model.left_sidebar_open,
};

let toggle_right_sidebar = (model: t): t => {
  ...model,
  right_sidebar_open: !model.right_sidebar_open,
};

let load_example = (model: t, block: UHExp.block): t =>
  model
  |> update_edit_state(
       Statics.syn_fix_holes_zblock(
         (VarCtx.empty, PaletteCtx.empty),
         MetaVarGen.init,
         ZExp.place_before_block(block),
       ),
     );

let focus_cell = model => {...model, is_cell_focused: true};

let blur_cell = model => {...model, is_cell_focused: false};
