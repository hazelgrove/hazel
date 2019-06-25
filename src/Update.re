module EditAction = Action;

module Action = {
  type t =
    | EditAction(EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id)
    | SelectHoleInstance(MetaVar.t, Dynamics.inst_num)
    | InvalidVar(string);
};

[@warning "-27"]
let apply_action =
    (model: MyModel.t, action: Action.t, _, ~schedule_action): MyModel.t =>
  switch (action) {
  | EditAction(a) => MyModel.perform_edit_action(model, a)
  | ToggleLeftSidebar => MyModel.toggle_left_sidebar(model)
  | ToggleRightSidebar => MyModel.toggle_right_sidebar(model)
  | LoadExample(id) => MyModel.load_example(model, Examples.get(id))
  | SelectHoleInstance(u, i) => MyModel.select_hole_instance(model, u, i)
  | InvalidVar(x) => model
  };
