open Haz3lcore;
open Util;
open API;

module CodeModel = CodeEditable.Model;
module Model = AssistantModel;

open AssistantUpdateBase;

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
    (~editor: CodeModel.t, ~schedule_editor_action: Editors.Update.t => unit)
    : unit => {
  let curr_node_info =
    AssistantTreeHelper.build_curr_node_info(
      editor.editor.state.zipper,
      editor.statics.info_map,
    );
  switch (curr_node_info) {
  | Some(curr_node_info) =>
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
  | None =>
    let perform_action = CodeEditable.Update.Perform(Action.Select(All));
    let cell_action = CellEditor.Update.MainEditor(perform_action);
    let scratch_action = Editors.Update.Scratch(CellAction(cell_action));
    schedule_editor_action(scratch_action);
  // Special case: No let or type alias expressions in the program.
  // Just dump selection. It is assumed that the entire sketch is selected in this case.
  };
};
