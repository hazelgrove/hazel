open Sexplib.Std;

type cardstacks = list(CardStack.t);
let cardstacks: cardstacks = [
  TutorialCards.cardstack,
  // RCStudyCards.cardstack,
];

let init_compute_results = true;

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
            card.init_zexp
            |> Statics.Exp.fix_and_renumber_holes_z(Contexts.empty),
        },
      cardstack.cards,
    );
  let zcards =
    OptUtil.get(_ => failwith("no cards"), ZList.split_at(0, card_states));
  {cardstack, zcards};
};

let mk_cardstacks_state = cardstacks => {
  let cardstack_states = List.map(mk_cardstack_state, cardstacks);
  OptUtil.get(
    _ => failwith("no cardstacks"),
    ZList.split_at(0, cardstack_states),
  );
};

[@deriving sexp]
type result = (DHExp.t, HoleInstanceInfo.t, Dynamics.Evaluator.result);

module UserSelectedInstances = {
  [@deriving sexp]
  type t = MetaVarMap.t(MetaVarInst.t);
  let init = MetaVarMap.empty;
  let update = (usi, inst) => MetaVarMap.insert_or_update(usi, inst);
};

[@deriving sexp]
type context_inspector = {
  next_state: option(HoleInstance.t),
  prev_state: option(HoleInstance.t),
};

type has_result_state = {
  result,
  context_inspector,
  user_selected_instances: UserSelectedInstances.t,
  selected_instance: option((MetaVar.t, MetaVarInst.t)),
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

let zexp = model => {
  let (ze, _, _) = edit_state_of(model);
  ze;
};

let exp = model => model |> zexp |> ZExp.erase;

let path = model => {
  let (ze, _, _) = edit_state_of(model);
  CursorPath.Exp.of_z(ze);
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
let cursor_info_of_edit_state = ((ze, _, _): edit_state): CursorInfo.t =>
  switch (
    CursorInfo.Exp.syn_cursor_info(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      ze,
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
let result_of_edit_state = ((ze, _, _): edit_state): result => {
  open Dynamics;
  let expanded =
    Exp.syn_expand(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      Delta.empty,
      ze |> ZExp.erase,
    );
  switch (expanded) {
  | DoesNotExpand => raise(DoesNotExpand)
  | Expands(d, _, _) =>
    switch (Evaluator.evaluate(d)) {
    | InvalidInput(_n) =>
      //JSUtil.log("InvalidInput " ++ string_of_int(n));
      raise(InvalidInput)
    | BoxedValue(d) =>
      let (d_renumbered, hii) = Exp.renumber([], HoleInstanceInfo.empty, d);
      (d_renumbered, hii, BoxedValue(d_renumbered));
    | Indet(d) =>
      let (d_renumbered, hii) = Exp.renumber([], HoleInstanceInfo.empty, d);
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

let update_edit_state = ((new_ze, ty, u_gen): edit_state, model: t): t => {
  let new_edit_state = (new_ze, ty, u_gen);
  let new_result_state =
    result_state_of_edit_state(new_edit_state, model.compute_results);
  let cardstacks_state = model.cardstacks_state;
  let cardstack_state = cardstack_state_of(model);
  let card_state = ZList.prj_z(cardstack_state.zcards);
  let new_card_state = {...card_state, edit_state: new_edit_state};
  let new_cardstack_state = {
    ...cardstack_state,
    zcards: cardstack_state.zcards |> ZList.replace_z(new_card_state),
  };
  let new_cardstacks_state =
    cardstacks_state |> ZList.replace_z(new_cardstack_state);
  let new_cursor_info = cursor_info_of_edit_state(new_edit_state);
  {
    ...model,
    cardstacks_state: new_cardstacks_state,
    cursor_info: new_cursor_info,
    result_state: new_result_state,
  };
};

let update_cardstack_state = (model, cardstack_state) => {
  let edit_state = ZList.prj_z(cardstack_state.zcards).edit_state;
  let result_state =
    result_state_of_edit_state(edit_state, model.compute_results);
  let cursor_info = cursor_info_of_edit_state(edit_state);
  let cardstacks_state =
    model.cardstacks_state |> ZList.replace_z(cardstack_state);
  {
    ...model,

    cardstacks_state,
    result_state,
    cursor_info,
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
    selected_example: None,
    is_cell_focused: false,
    undo_history: ([], edit_state, []),
    left_sidebar_open: false,
    right_sidebar_open: true,
    show_content_editable: false,
    show_presentation: false,
  };
};

exception FailedAction;
exception CursorEscaped;
let perform_edit_action = (model: t, a: Action.t): t => {
  switch (
    Action.Exp.syn_perform(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      a,
      edit_state_of(model),
    )
  ) {
  | Failed => raise(FailedAction)
  | CursorEscaped(_) => raise(CursorEscaped)
  | Succeeded(new_edit_state) =>
    let new_model = model |> update_edit_state(new_edit_state);
    let new_history =
      if (UndoHistory.undoable_action(a)) {
        UndoHistory.push_edit_state(model.undo_history, new_edit_state);
      } else {
        model.undo_history;
      };
    {...new_model, undo_history: new_history};
  };
};

let move_to_hole = (model: t, u: MetaVar.t): t => {
  let (ze, _, _) = edit_state_of(model);
  switch (
    CursorPath.steps_to_hole(
      CursorPath.Exp.holes(ze |> ZExp.erase, [], []),
      u,
    )
  ) {
  | None =>
    //JSUtil.log("CursorPath not found!");
    model
  | Some(hole_steps) =>
    perform_edit_action(model, Action.MoveToBefore(hole_steps))
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
          i < HoleInstanceInfo.num_instances(hii, u) - 1
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

let load_example = (model: t, e: UHExp.t): t =>
  model
  |> update_edit_state(
       Statics.Exp.syn_fix_holes_z(
         (VarCtx.empty, PaletteCtx.empty),
         MetaVarGen.init,
         e |> ZExp.place_before,
       ),
     );

let focus_cell = model => {...model, is_cell_focused: true};

let blur_cell = model => {...model, is_cell_focused: false};
