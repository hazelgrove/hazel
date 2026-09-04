open Util_web;
open Haz3lcore;
open AgentResult;
open AgentModel;

module Utils = AgentUtils;
module ToolCallHandler = AgentToolCallHandler;

let add_tool_result_to_active_subtask =
    (
      ~tool_result: AgentToolResult.tool_result,
      ~action: CompositionActions.action,
      ~model: Model.t,
      ~chat_id: Id.t,
    )
    : Model.t => {
  switch (action) {
  // Only add editor and context tools to subtask tool results for now
  // i.e. don't include workbench tool calls in the workbench view lol
  | EditorAction(_)
  | AgentContextAction(_) =>
    let chat_system =
      ChatSystem.Update.update(
        ChatSystem.Update.Action.ChatAction(
          Chat.Update.Action.WorkbenchAction(
            AgentWorkbench.Update.Action.UIAction(
              AgentWorkbench.Update.Action.UIAction.AddToolResultToActiveSubtask(
                tool_result,
              ),
            ),
          ),
          chat_id,
        ),
        model.chat_system,
      )
      |> ChatSystem.Update.get;
    {
      ...model,
      chat_system,
    };
  | _ => model
  };
};

let mk_diff =
    (
      ~old_editor: Editor.t,
      ~new_editor: Editor.t,
      action: CompositionActions.action,
    )
    : option(AgentToolResult.diff) => {
  switch (action) {
  | EditorAction(edit_action) =>
    switch (
      CompositionGo.Local.get_diff(
        old_editor.state.zipper,
        new_editor.state.zipper,
        edit_action,
        CompositionGo.Public.mk_statics,
        old_editor.syntax,
      )
    ) {
    | Some((old_segment, new_segment)) =>
      Some(
        AgentToolResult.{
          old_segment,
          new_segment,
        },
      )
    | None => None
    }
  | SyntaxProjectorAction(_)
  | ProbeAction(_)
  | StaticsAction(_) =>
    let old_segment = Select.all(old_editor.state.zipper).selection.content;
    let new_segment = Select.all(new_editor.state.zipper).selection.content;
    let old_s = CompositionView.Public.print_segment(old_segment);
    let new_s = CompositionView.Public.print_segment(new_segment);
    if (old_s == new_s) {
      None;
    } else {
      Some(
        AgentToolResult.{
          old_segment,
          new_segment: Some(new_segment),
        },
      );
    };
  | _ => None
  };
};

let mk_segment_snapshots =
    (
      ~old_editor: Editor.t,
      ~new_editor: Editor.t,
      action: CompositionActions.action,
    )
    : (option(Segment.t), option(Segment.t)) => {
  switch (action) {
  | EditorAction(_)
  | InsertAtProgramBoundary(_)
  | ProbeAction(_)
  | StaticsAction(_)
  | SyntaxProjectorAction(_) =>
    let old_segment = Select.all(old_editor.state.zipper).selection.content;
    let new_segment = Select.all(new_editor.state.zipper).selection.content;
    (Some(old_segment), Some(new_segment));
  | _ => (None, None)
  };
};

/** Run one tool; returns chat message to append (caller batches append + one LLM request). */
let execute_one_tool_call =
    (
      ~tool_call: OpenRouter.Reply.Model.tool_call,
      ~model: Model.t,
      ~cell_editor: CellEditor.Model.t,
      ~settings: Settings.t,
      ~chat_id: Id.t,
    )
    : (Model.t, Updated.t(CellEditor.Model.t), Message.Model.t) => {
  switch (
    CompositionUtils.Public.action_of(
      ~tool_name=tool_call.name,
      ~args=tool_call.args,
    )
  ) {
  | Action(action) =>
    switch (
      try(
        ToolCallHandler.update(
          ~settings,
          action,
          model,
          cell_editor.editor,
          chat_id,
        )
      ) {
      | Failure(msg) => Error(Failure.Info(msg))
      | exn =>
        /* Catch all exceptions (e.g. Path not found) — report to agent, do not break state */
        Error(Failure.Info(Printexc.to_string(exn)))
      }
    ) {
    | Ok((model, editor)) =>
      let model =
        Utils.update_context(
          ~session_mode=settings.agent_globals.session_mode,
          model,
          editor,
          chat_id,
        );
      let success_message =
        "The "
        ++ tool_call.name
        ++ " tool call was successful and has been applied to the model.";
      let (before_segment, after_segment) =
        mk_segment_snapshots(
          ~old_editor=cell_editor.editor.editor,
          ~new_editor=editor.editor,
          action,
        );
      let diff_result =
        try(
          Ok(
            mk_diff(
              ~old_editor=cell_editor.editor.editor,
              ~new_editor=editor.editor,
              action,
            ),
          )
        ) {
        | exn => Error(exn)
        };
      switch (diff_result) {
      | Error(exn) =>
        /* mk_diff can raise (e.g. path_to_id); report to agent, keep state */
        let msg = Printexc.to_string(exn);
        let tool_result: AgentToolResult.tool_result = {
          tool_call,
          success: false,
          skipped: false,
          expanded: false,
          diff: None,
          before_segment:
            Some(
              Select.all(cell_editor.editor.editor.state.zipper).selection.
                content,
            ),
          after_segment: None,
          content: msg,
        };
        let model =
          add_tool_result_to_active_subtask(
            ~tool_result,
            ~action,
            ~model,
            ~chat_id,
          );
        (
          model,
          cell_editor |> Updated.return_quiet,
          Message.Utils.mk_tool_result_message(tool_result),
        );
      | Ok(diff) =>
        let tool_result: AgentToolResult.tool_result = {
          tool_call,
          success: true,
          skipped: false,
          expanded: false,
          diff,
          before_segment,
          after_segment,
          content: success_message,
        };
        let model =
          add_tool_result_to_active_subtask(
            ~tool_result,
            ~action,
            ~model,
            ~chat_id,
          );
        (
          model,
          {
            ...cell_editor,
            editor,
          }
          |> Updated.return,
          Message.Utils.mk_tool_result_message(tool_result),
        );
      };
    | Error(error) =>
      switch (error) {
      | Failure.Info(msg) =>
        let before_segment =
          switch (action) {
          | EditorAction(_) =>
            Some(
              Select.all(cell_editor.editor.editor.state.zipper).selection.
                content,
            )
          | _ => None
          };
        let tool_result: AgentToolResult.tool_result = {
          tool_call,
          success: false,
          skipped: false,
          expanded: false,
          diff: None,
          before_segment,
          after_segment: None,
          content: msg,
        };
        let model =
          add_tool_result_to_active_subtask(
            ~tool_result,
            ~action,
            ~model,
            ~chat_id,
          );
        (
          model,
          cell_editor |> Updated.return_quiet,
          Message.Utils.mk_tool_result_message(tool_result),
        );
      }
    }
  | Failure(msg) =>
    let tool_result: AgentToolResult.tool_result = {
      tool_call,
      success: false,
      skipped: false,
      expanded: false,
      diff: None,
      before_segment: None,
      after_segment: None,
      content: msg,
    };
    // Do not add unparseable tool calls to subtask tool results for now
    (
      model,
      cell_editor |> Updated.return_quiet,
      Message.Utils.mk_tool_result_message(tool_result),
    );
  };
};
