open Language;

module Model = AssistantModel;

type t = AssistantUpdateAction.t;

let intermediate_select_curr_node =
    (
      ~zipper: Zipper.t,
      ~info_map: Statics.Map.t,
      ~schedule_editor_action: Editor.Update.t => unit,
    )
    : unit => {
  let curr_node_info =
    AssistantTreeHelper.build_curr_node_info(zipper, info_map);
  switch (curr_node_info) {
  | Some(curr_node_info) =>
    let a =
      Action.Select(
        Tile(
          Id(
            AssistantTreeHelper.id_of(curr_node_info),
            Util.Direction.Right,
          ),
        ),
      );
    schedule_editor_action(a);
  | None =>
    let perform_action = Action.Select(All);
    schedule_editor_action(perform_action);
  // Special case: No let or type alias expressions in the program.
  // Just dump selection. It is assumed that the entire sketch is selected in this case.
  };
};
