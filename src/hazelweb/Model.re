open Sexplib.Std;
open GeneralUtil;

module ZList = GeneralUtil.ZList;

type cardstacks = list(CardStack.t);
let cardstacks: cardstacks = [
  TutorialCards.cardstack,
  RCStudyCards.cardstack,
];

let init_compute_results = true;

type user_newlines = CursorPath.StepsMap.t(unit);

type edit_state = Statics.edit_state;

type card_state = {
  card: Card.t,
  edit_state,
};

type cardstack_state = {
  cardstack: CardStack.t,
  zcards: ZList.t(card_state, card_state),
};

type cardstacks_state = ZList.t(cardstack_state, cardstack_state);

let mk_cardstack_state = (cardstack: CardStack.t) => {
  let card_states =
    List.map(
      card =>
        {
          card,
          edit_state:
            card.init_zblock
            |> Statics.fix_and_renumber_holes_z(Contexts.empty),
        },
      cardstack.cards,
    );
  let zcards =
    GeneralUtil.Opt.get(
      _ => failwith("no cards"),
      ZList.split_at(0, card_states),
    );
  {cardstack, zcards};
};

let mk_cardstacks_state = cardstacks => {
  let cardstack_states = List.map(mk_cardstack_state, cardstacks);
  GeneralUtil.Opt.get(
    _ => failwith("no cardstacks"),
    ZList.split_at(0, cardstack_states),
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
  cardstacks,
  cardstacks_state /* these are derived from the cardstack state: */,
  cursor_info: CursorInfo.t,
  compute_results: bool,
  result_state,
  /* UI state */
  user_newlines,
  selected_example: option(UHExp.block),
  is_cell_focused: bool,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  show_content_editable: bool,
  show_presentation: bool,
  undo_history: UndoHistory.t,
};

let cardstack_state_of = model => ZList.prj_z(model.cardstacks_state);

let edit_state_of = model =>
  ZList.prj_z(cardstack_state_of(model).zcards).edit_state;

let cutoff = (m1, m2) => m1 == m2;

let zblock = model => {
  let (zblock, _, _) = edit_state_of(model);
  zblock;
};

let block = model => model |> zblock |> ZExp.erase_block;

let path = model => {
  let (zblock, _, _) = edit_state_of(model);
  CursorPath.of_zblock(zblock);
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
  | Some(ci) =>
    /* uncomment to see where variable is used
           switch (ci.node) {
           | Pat(VarPat(_, uses)) =>
             JSUtil.log_sexp(UsageAnalysis.sexp_of_uses_list(uses))
           | _ => JSUtil.log("not varpat")
           };
       */
    ci
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
    | InvalidInput(_n) =>
      //JSUtil.log("InvalidInput " ++ string_of_int(n));
      raise(InvalidInput)
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

let result_state_of_edit_state = (edit_state, compute_results) =>
  if (!compute_results) {
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
    (~prefix: CursorPath.steps, user_newlines)
    : (user_newlines, list(CursorPath.steps)) => {
  let (inside_prefix, outside_prefix) =
    user_newlines
    |> CursorPath.StepsMap.partition((steps, _) =>
         prefix |> CursorPath.is_prefix_of(steps)
       );
  (
    inside_prefix,
    outside_prefix
    |> CursorPath.StepsMap.bindings
    |> List.map(((steps, _)) => steps),
  );
};

let add_user_newline = (steps, model) => {
  ...model,
  user_newlines: model.user_newlines |> CursorPath.StepsMap.add(steps, ()),
};

let remove_user_newline = (steps, model) => {
  ...model,
  user_newlines: model.user_newlines |> CursorPath.StepsMap.remove(steps),
};

let user_entered_newline_at = (steps, user_newlines): bool =>
  user_newlines |> CursorPath.StepsMap.mem(steps);

let update_edit_state = ((new_zblock, ty, u_gen): edit_state, model: t): t => {
  let new_block = new_zblock |> ZExp.erase_block;
  let (new_steps, _) = CursorPath.of_zblock(new_zblock);
  let (new_zblock, new_user_newlines) =
    model.user_newlines
    |> CursorPath.StepsMap.bindings
    |> List.fold_left(
         ((new_zblock, newlines), (steps, _)) =>
           switch (
             new_block |> CursorPath.follow_block_and_place_after(steps)
           ) {
           | None => (
               new_zblock,
               newlines |> CursorPath.StepsMap.remove(steps),
             )
           | Some(zblock) =>
             switch (CursorInfo.syn_cursor_info_block(Contexts.empty, zblock)) {
             | None => (
                 new_zblock,
                 newlines |> CursorPath.StepsMap.remove(steps),
               )
             | Some(ci) =>
               let newlines =
                 switch (ci.node) {
                 | Exp(EmptyHole(_))
                 | Line(EmptyLine) => newlines
                 | _ =>
                   // something nontrivial present, no more
                   // need to keep track of ephemeral newline
                   newlines |> CursorPath.StepsMap.remove(steps)
                 };
               if (CursorPath.compare_steps(new_steps, steps) < 0) {
                 let new_path = CursorPath.of_zblock(new_zblock);
                 (
                   new_zblock
                   |> ZExp.erase_block
                   |> CursorPath.prune_trivial_suffix_block(
                        ~steps_of_first_line=steps,
                      )
                   |> CursorPath.follow_block(new_path)
                   |> Opt.get(() => assert(false)),
                   newlines |> CursorPath.StepsMap.remove(steps),
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
    result_state_of_edit_state(new_edit_state, model.compute_results);
  let cardstacks_state = model.cardstacks_state;
  let cardstack_state = cardstack_state_of(model);
  let card_state = ZList.prj_z(cardstack_state.zcards);
  let new_card_state = {...card_state, edit_state: new_edit_state};
  let new_cardstack_state = {
    ...cardstack_state,
    zcards: ZList.replace_z(cardstack_state.zcards, new_card_state),
  };
  let new_cardstacks_state =
    ZList.replace_z(cardstacks_state, new_cardstack_state);
  let new_cursor_info = cursor_info_of_edit_state(new_edit_state);
  {
    ...model,
    cardstacks_state: new_cardstacks_state,
    cursor_info: new_cursor_info,
    result_state: new_result_state,
    user_newlines: new_user_newlines,
  };
};

let update_cardstack_state = (model, cardstack_state) => {
  let edit_state = ZList.prj_z(cardstack_state.zcards).edit_state;
  let result_state =
    result_state_of_edit_state(edit_state, model.compute_results);
  let cursor_info = cursor_info_of_edit_state(edit_state);
  let user_newlines = CursorPath.StepsMap.empty;
  let cardstacks_state =
    ZList.replace_z(model.cardstacks_state, cardstack_state);
  {
    ...model,

    cardstacks_state,
    result_state,
    cursor_info,
    user_newlines,
  };
};

let update_cardstacks_state = (model, cardstacks_state) => {
  let model' = {...model, cardstacks_state};
  update_cardstack_state(model', cardstack_state_of(model'));
};

let load_cardstack = (model, idx) => {
  let cardstacks_state_list = ZList.erase(model.cardstacks_state, x => x);
  switch (ZList.split_at(idx, cardstacks_state_list)) {
  | None => model
  | Some(cardstacks_state) =>
    update_cardstacks_state(model, cardstacks_state)
  };
};

let card_of = model => ZList.prj_z(cardstack_state_of(model).zcards).card;

let prev_card = model => {
  let cardstack_state = cardstack_state_of(model);
  let cardstack_state = {
    ...cardstack_state,
    zcards:
      switch (ZList.shift_prev(cardstack_state.zcards)) {
      | None => cardstack_state.zcards
      | Some(card) => card
      },
  };
  {
    ...update_cardstack_state(model, cardstack_state),

    is_cell_focused: true,
  };
};

let next_card = model => {
  let cardstack_state = cardstack_state_of(model);
  let cardstack_state = {
    ...cardstack_state,
    zcards:
      switch (ZList.shift_next(cardstack_state.zcards)) {
      | None => cardstack_state.zcards
      | Some(card) => card
      },
  };
  {
    ...update_cardstack_state(model, cardstack_state),

    is_cell_focused: true,
  };
};

let init = (): t => {
  let cardstacks_state = mk_cardstacks_state(cardstacks);
  let edit_state =
    ZList.prj_z(ZList.prj_z(cardstacks_state).zcards).edit_state;
  let compute_results = init_compute_results;
  {
    cardstacks,
    cardstacks_state,
    cursor_info: cursor_info_of_edit_state(edit_state),
    compute_results,
    result_state: result_state_of_edit_state(edit_state, compute_results),
    user_newlines: CursorPath.StepsMap.empty,
    selected_example: None,
    is_cell_focused: false,
    undo_history: ([], (edit_state, None, 0), []),
    left_sidebar_open: false,
    right_sidebar_open: true,
    show_content_editable: false,
    show_presentation: false,
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
  | Succeeded(new_edit_state) =>
    let new_model = model |> update_edit_state(new_edit_state);
    let new_history =
      if (UndoHistory.undoable_action(a)) {
        UndoHistory.push_edit_state(
          model.undo_history,
          new_edit_state,
          Some(a),
        );
      } else {
        model.undo_history;
      };
    {...new_model, undo_history: new_history};
  };
};

let move_to_hole = (model: t, u: MetaVar.t): t => {
  let (zblock, _, _) = edit_state_of(model);
  switch (
    CursorPath.path_to_hole(
      CursorPath.holes_block(ZExp.erase_block(zblock), [], []),
      u,
    )
  ) {
  | None =>
    //JSUtil.log("Path not found!");
    model
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
