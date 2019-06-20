module EditAction = Action;

module Action = {
  type t =
    | EditAction(EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id);
};

let apply_action =
    (model: MyModel.t, action: Action.t, _, ~schedule_action): MyModel.t =>
  switch (action) {
  | EditAction(a) => MyModel.perform_edit_action(model, a)
  | ToggleLeftSidebar => MyModel.toggle_left_sidebar(model)
  | ToggleRightSidebar => MyModel.toggle_right_sidebar(model)
  | LoadExample(id) => MyModel.load_example(model, Examples.get(id))
  };
