open Util;
open API;
open Language;

module Model = AssistantModel;

open AssistantUpdateType;
type t = AssistantUpdateType.t;

let mk_structure_edit_msg = (~tool_call: OpenRouter.tool_call): string =>
  // AddToolLabel_3.0: what should the text content of this tool call to the user be?
  //                   (not to the llm, that is the string returned in AssistantModes.Composition.apply_action)
  try({
    let tool_name = tool_call.tool_name;
    let args = tool_call.args;
    let action =
      CompositionTools.action_of(
        ~tool_name,
        ~args=Json.get_string_kvs(args),
      );
    let _enclose_in_backticks = (str: string) => "```" ++ str ++ "```";
    "Agent called tool: " ++ CompositionTools.string_of(action);
  }) {
  | Failure(err) =>
    "The agent may have called tools with invalid arguments: " ++ err
  | Invalid_argument(e) =>
    "The argument map creation may have failed, or some other fatal issue occurred: "
    ++ e
  };

let apply_structure_action =
    (
      ~tool_call: OpenRouter.tool_call,
      ~apply_action: (~action: CompositionTools.action) => string,
      ~schedule_action: t => unit,
      ~loop_message,
    )
    : unit =>
  // This try block is important, it allows us to handle exceptions and relay them to the agent
  try({
    let action =
      CompositionTools.action_of(
        ~tool_name=tool_call.tool_name,
        ~args=Json.get_string_kvs(tool_call.args),
      );
    // tool_result will be a string returned from apply_action, detailing the effects of
    // the action on the editor so that we can provide the agent with a meaningful "tool response".
    // This follows standard tool calling protocol.
    let tool_result = apply_action(~action);
    schedule_action(loop_message(Success(tool_result)));
  }) {
  | Failure(err) => schedule_action(loop_message(Failure(err)))
  };

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
          Id(AssistantTreeHelper.id_of(curr_node_info), Direction.Right),
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
