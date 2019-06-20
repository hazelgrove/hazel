module EditAction = Action;
open Incr_dom;

module Model = {
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
};

module Action = {
  type t =
    | EditAction(a: EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id);
};

module State = {
  type t = unit;
};

let apply_action = (model: Model.t, action: Action.t, _, ~schedule_action): Model.t =>
  switch (a) {
  | EditAction(a) => Model.perform_edit_action(a, model)
  | ToggleLeftSideBar => Model.toggle_left_sidebar(model)
  | ToggleRightSidebar => Model.toggle_right_sidebar(model)
  | LoadExample(id) => Model.load_example(model, Examples.get(id))
  };

let on_startup = (~schedule_action, _) => Async_kernel.return();

let instance_click_fn = ((u, _) as inst) => {
  let usi = React.S.value(user_selected_instances_rs);
  user_selected_instances_rf(UserSelectedInstances.update(usi, inst));
  move_to_hole(u);
  selected_instance_rf(Some(inst));
};
let result_view = ({ result: (_, _, result) }: Model.t) => {
  open Vdom;
  switch (result) {
  | InvalidInput(_) =>
    Node.div(
      [],
      Node.text("(internal error: expansion or evaluation invariant violated)"),
    ),
  | BoxedValue(d)
  | Indet(d) =>
    Node.div(
      [],
      [view_of_dhexp(instance_click_fn)]
    )
  };
};

let examples_select = (~inject: Action.t => Vdom.Event.t) =>
  Vdom.(
    Node.select(
      [Attr.on_change(ev => inject(Action.LoadExample(ev##.target##.value))],
      [
        Node.option([Attr.value("just_hole")], Node.text("just a hole")),
        Node.option([Attr.value("holey_lambda")], Node.text("holey lambda")),
        Node.option([Attr.value("let_line")], Node.text("let with extra lines")),
        Node.option([Attr.value("map_example")], Node.text("map")),
        Node.option([Attr.value("qsort_example")], Node.text("qsort")),
      ],
    )
  );

let page_view = (model: Model.t, ~inject: Action.t => Vdom.Event.t) => {
  open Vdom;
  Node.div(
    [Attr.id(["root"])],
    [
      Node.div(
        [Attr.classes(["top-bar"])],
        [
          Node.a(
            [Attr.classes(["logo-text"]), Attr.href("https://hazel.org")],
            [Node.text("Hazel")],
          )
        ],
      ),
      Node.div(
        [Attr.classes(["main-area"])],
        [
          Sidebar.left([the_action_panel /*, the_history_panel*/]),
          Node.div(
            [Attr.classes(["flex-wrapper"])],
            [
              Node.div(
                [Attr.classes(["page-area"])],
                [
                  Node.div(
                    [Attr.classes(["page"])],
                    [
                      Node.div([
                        Node.text("Hazel is an experiment in "),
                        Node.strong([Node.text("live functional programming")]),
                        Node.text(" with "),
                        Node.strong([Node.text("typed holes")]),
                        Node.text(
                          ". Use the actions on the left to construct an expression. Navigate using the text cursor in the usual way.",
                        ),
                      ]),
                      /* TODO add pp_view_parent */
                      div(
                        [a_class(["cell-status"])],
                        [
                          div(
                            [a_class(["type-indicator"])],
                            [
                              div(
                                [a_class(["type-label"])],
                                [txt("Result of type: ")],
                              ),
                              div(
                                [a_class(["htype-view"])],
                                [htype_view],
                              ),
                            ],
                          ),
                        ],
                      ),
                      div(
                        [a_class(["result-view"])],
                        [result_view],
                      ),
                    ]
                  ),
                  examples_select,
                ]
              )
            ]
          ),
          Sidebar.right([
            the_cursor_inspector_panel,
            the_context_inspector_panel,
          ]),
        ]
      )
    ]
  )
};

let view = (model: Model.t, ~inject: Action.t => Vdom.Event.t) => {

};

let create = (model, ~old_model, ~inject) => {
  open Incr.Let_syntax;
  let%map model = model;
  let apply_action = apply_action(model);
  let view = view(model, ~inject);
  Component.create(~apply_action, model, view);
};