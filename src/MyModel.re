type t = {
  edit_state: (ZExp.zblock, HTyp.t, MetaVarGen.t),
  result: Dynamics.(DHExp.t, DHExp.HoleInstanceInfo.t, Evaluator.result),
  left_sidebar_open: bool,
  right_sidebar_open: bool,
  selected_example: option(UHExp.block),
};

let cutoff = (m1, m2) => m1 == m2;

let init = (): t => {
  let (u, u_gen) = MetaVarGen.next(MetaVarGen.init);
  let zblock = ref(ZExp.wrap_in_block(ZExp.place_before_exp(EmptyHole(u0))));
  {
    edit_state: (zblock, Hole, u_gen),
    left_sidebar_open: false,
    right_sidebar_open: true,
    selected_example: None,
  };
};

exception DoesNotExpand;
let update_edit_state = (model: t, new_edit_state): t => {
  let (zblock, _, _) = new_edit_state;
  open Dynamics;
  let expanded =
    DHExp.syn_expand_block(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      Delta.empty,
      ZExp.erase_block(zblock),
    );
  let new_result =
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
  {
    ...model,
    edit_state: new_edit_state,
    result: new_result,
  };
};

let perform_edit_action = (model: t, a: EditAction): t =>
  update_edit_state(
    model,
    EditAction.syn_perform_block(
      (VarCtx.empty, Palettes.initial_palette_ctx),
      a,
      model.edit_state,
    )
  );

let toggle_left_sidebar = (model: t): t => {
  ...model,
  left_sidebar_open: !(model.left_sidebar_open),
};

let toggle_right_sidebar = (model: t): t => {
  ...model,
  right_sidebar_open: !(model.right_sidebar_open),
};

let load_example = (model: t, block: UHExp.block): t => {
  ...model,
  edit_state:
    Statics.syn_fix_holes_zblock(
      (VarCtx.empty, PaletteCtx.empty),
      MetaVarGen.init,
      ZExp.place_before_block(block),
    );
};
