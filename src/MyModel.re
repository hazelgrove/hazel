type edit_state = (ZExp.zblock, HTyp.t, MetaVarGen.t);
type result_state = (
  Dynamics.DHExp.t,
  Dynamics.DHExp.HoleInstanceInfo.t,
  Dynamics.Evaluator.result,
);
type t = {
  edit_state,
  result_state,
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  selected_example: option(UHExp.block),
};

let cutoff = (m1, m2) => m1 == m2;

exception InvalidInput;
exception DoesNotExpand;
let result_of_edit_state = ((zblock, _, _): edit_state): result_state => {
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
  let new_result_state = result_of_edit_state(new_edit_state);
  {...model, edit_state: new_edit_state, result_state: new_result_state};
};

let init = (): t => {
  let (u, u_gen) = MetaVarGen.next(MetaVarGen.init);
  let zblock = ZExp.wrap_in_block(ZExp.place_before_exp(EmptyHole(u)));
  let edit_state = (zblock, HTyp.Hole, u_gen);
  {
    edit_state,
    result_state: result_of_edit_state(edit_state),
    left_sidebar_open: false,
    right_sidebar_open: true,
    selected_example: None,
  };
};

exception InvalidAction;
let perform_edit_action = (model: t, a: Action.t): t =>
  switch (
    Action.syn_perform_block(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      a,
      model.edit_state,
    )
  ) {
  | Failed
  | CursorEscaped(_) => raise(InvalidAction)
  | Succeeded(new_edit_state) => update_edit_state(model, new_edit_state)
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
