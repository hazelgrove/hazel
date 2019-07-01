[@deriving sexp]
type edit_state = (ZExp.zblock, HTyp.t, MetaVarGen.t);
type result = (
  Dynamics.DHExp.t,
  Dynamics.DHExp.HoleInstanceInfo.t,
  Dynamics.Evaluator.result,
);
module UserSelectedInstances = {
  type t = MetaVarMap.t(Dynamics.inst_num);
  let init = MetaVarMap.empty;
  let update = (usi, inst) => MetaVarMap.insert_or_update(usi, inst);
};
type context_inspector = {
  next_state: option(Dynamics.DHExp.HoleInstance.t),
  prev_state: option(Dynamics.DHExp.HoleInstance.t),
};
type t = {
  edit_state,
  cursor_info: CursorInfo.t,
  result,
  user_selected_instances: UserSelectedInstances.t,
  selected_instance: option((MetaVar.t, Dynamics.inst_num)),
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  selected_example: option(UHExp.block),
  context_inspector,
};

let cutoff = (model1, model2) => {
  let (zblock1, ty1, u_gen1) = model1.edit_state;
  let (zblock2, ty2, u_gen2) = model2.edit_state;
  ZExp.diff_is_just_cursor_movement_within_node(zblock1, zblock2)
  && ty1 == ty2
  && u_gen1 == u_gen2;
};

let get_path = model => {
  let (zblock, _, _) = model.edit_state;
  Path.of_zblock(zblock);
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

let update_edit_state = (model: t, new_edit_state): t => {
  let new_result = result_of_edit_state(new_edit_state);
  let new_cursor_info = cursor_info_of_edit_state(new_edit_state);
  {
    ...model,
    edit_state: new_edit_state,
    cursor_info: new_cursor_info,
    result: new_result,
  };
};

let init = (): t => {
  let (u, u_gen) = MetaVarGen.next(MetaVarGen.init);
  let zblock = ZExp.wrap_in_block(ZExp.place_before_exp(EmptyHole(u)));
  let edit_state = (zblock, HTyp.Hole, u_gen);
  {
    edit_state,
    cursor_info: cursor_info_of_edit_state(edit_state),
    result: result_of_edit_state(edit_state),
    user_selected_instances: UserSelectedInstances.init,
    selected_instance: None,
    left_sidebar_open: false,
    right_sidebar_open: true,
    selected_example: None,
    context_inspector: {
      next_state: None,
      prev_state: None,
    },
  };
};

exception FailedAction;
exception CursorEscaped;
let perform_edit_action = (model: t, a: Action.t): t => {
  switch (
    Action.syn_perform_block(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      a,
      model.edit_state,
    )
  ) {
  | Failed => raise(FailedAction)
  | CursorEscaped(_) => raise(CursorEscaped)
  | Succeeded(new_edit_state) => update_edit_state(model, new_edit_state)
  };
};

let move_to_hole = (model: t, u: MetaVar.t): t => {
  let (zblock, _, _) = model.edit_state;
  switch (
    Path.path_to_hole(Path.holes_block(ZExp.erase_block(zblock), [], []), u)
  ) {
  | None =>
    JSUtil.log("Path not found!");
    model;
  | Some(hole_path) => perform_edit_action(model, Action.MoveTo(hole_path))
  };
};

let select_hole_instance =
    (model: t, (u, i) as inst: (MetaVar.t, Dynamics.inst_num)): t => {
  let (_, hii, _) = model.result;
  {
    ...model,
    user_selected_instances:
      UserSelectedInstances.update(model.user_selected_instances, inst),
    selected_instance: Some(inst),
    context_inspector: {
      prev_state: i > 0 ? Some((u, i - 1)) : None,
      next_state:
        i < Dynamics.DHExp.HoleInstanceInfo.num_instances(hii, u) - 1
          ? Some((u, i + 1)) : None,
    },
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

let load_example = (model: t, block: UHExp.block): t => {
  ...model,
  edit_state:
    Statics.syn_fix_holes_zblock(
      (VarCtx.empty, PaletteCtx.empty),
      MetaVarGen.init,
      ZExp.place_before_block(block),
    ),
};
