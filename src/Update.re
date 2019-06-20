module Action = {
  type t =
    | EditAction(a: EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id);
};

let apply_action = (model: Model.t, action: Action.t, _, ~schedule_action): Model.t =>
  switch (a) {
  | EditAction(a) => Model.perform_edit_action(a, model)
  | ToggleLeftSideBar => Model.toggle_left_sidebar(model)
  | ToggleRightSidebar => Model.toggle_right_sidebar(model)
  | LoadExample(id) => Model.load_example(model, Examples.get(id))
  };
