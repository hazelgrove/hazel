module EditAction = Action;
open Sexplib.Std;

module Action = {
  [@deriving sexp]
  type t =
    | EditAction(EditAction.t)
    | ToggleLeftSidebar
    | ToggleRightSidebar
    | LoadExample(Examples.id)
    | SelectHoleInstance(MetaVar.t, Dynamics.inst_num)
    | InvalidVar(string)
    | MoveToHole(MetaVar.t);
};

[@warning "-27"]
let apply_action =
    (model: Model.t, action: Action.t, _, ~schedule_action): Model.t =>
  switch (action) {
  | EditAction(a) => Model.perform_edit_action(model, a)
  | ToggleLeftSidebar => Model.toggle_left_sidebar(model)
  | ToggleRightSidebar => Model.toggle_right_sidebar(model)
  | LoadExample(id) => Model.load_example(model, Examples.get(id))
  | SelectHoleInstance(u, i) => Model.select_hole_instance(model, (u, i))
  | InvalidVar(x) => model
  | MoveToHole(u) => Model.move_to_hole(model, u)
  };
