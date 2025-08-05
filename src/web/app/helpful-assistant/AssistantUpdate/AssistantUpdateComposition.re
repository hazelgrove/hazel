open Haz3lcore;
open Util;
open API;

module CodeModel = CodeEditable.Model;
module Model = AssistantModel;

open AssistantUpdateUtil;

let mk_structure_edit_msg =
    (
      ~tool_call: OpenRouter.tool_call,
      ~curr_node_info as _: AssistantTreeHelper.node,
    )
    : string =>
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
    (~editor: CodeModel.t, ~schedule_editor_action: Editors.Update.t => unit)
    : unit => {
  let curr_node_info =
    AssistantTreeHelper.build_sub_AST(
      editor.editor.state.zipper,
      editor.statics.info_map,
    );
  let perform_action =
    CodeEditable.Update.Perform(
      Action.Select(
        Tile(
          Id(AssistantTreeHelper.id_of(curr_node_info), Direction.Right),
        ),
      ),
    );
  let cell_action = CellEditor.Update.MainEditor(perform_action);
  let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
  schedule_editor_action(scratch_action);
};
