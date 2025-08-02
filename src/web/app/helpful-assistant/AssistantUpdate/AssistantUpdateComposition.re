open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open API;
open Util.Maps;

module CodeModel = CodeEditable.Model;
module Model = AssistantModel;

open AssistantUpdateUtil;

let mk_structure_edit_msg =
    (
      ~tool_call: OpenRouter.tool_call,
      ~curr_node_info: AssistantTreeHelper.node,
    )
    : string =>
  // AddToolLabel_4
  try({
    let tool_name = tool_call.tool_name;
    let args = tool_call.args;
    let action =
      CompositionTools.action_of(
        ~tool_name,
        ~args=Json.get_string_kvs(args),
      );
    let _enclose_in_backticks = (str: string) => "```" ++ str ++ "```";
    switch (action) {
    | Nav(GoToParent) =>
      switch (curr_node_info.parent) {
      | None => raise(Failure("This node does not have a parent"))
      | Some(parent) =>
        "Agent moved from \""
        ++ curr_node_info.name
        ++ "\" to its parent \""
        ++ parent.name
        ++ "\""
      }
    | Nav(GoToChild(name, _)) =>
      "Agent moved from \""
      ++ curr_node_info.name
      ++ "\" to its child \""
      ++ name
      ++ "\""
    | Nav(GoToSibling(name, _)) =>
      "Agent moved from \""
      ++ curr_node_info.name
      ++ "\" to its sibling \""
      ++ name
      ++ "\""
    | Read(ViewDefinition) => "Agent viewed the definition of the current node"
    | Edit(UpdateDefinition(code)) =>
      "Agent updated the definition of the current node to " ++ code
    | Edit(UpdateBody(code)) =>
      "Agent updated the body of the current node to " ++ code
    | Edit(UpdatePattern(code)) =>
      "Agent updated the pattern of the current node to " ++ code
    | Edit(UpdateExpression(code)) =>
      "Agent updated the expression of the current node to " ++ code
    | Edit(Delete) => "Agent deleted the current node"
    | Edit(InsertAfter(code)) =>
      "Agent inserted after the current node to " ++ code
    | Edit(InsertBefore(code)) =>
      "Agent inserted before the current node to " ++ code
    };
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
    let tool_result = apply_action(~action);
    schedule_action(
      SendMessage(Composition(Intermediate), None, Id.invalid),
    );
    schedule_action(loop_message(Success(tool_result)));
  }) {
  | Failure(err) =>
    schedule_action(
      SendMessage(Composition(Intermediate), None, Id.invalid),
    );
    schedule_action(loop_message(Failure(err)));
  };
