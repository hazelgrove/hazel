open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Haz3lcore;

// Format duration in milliseconds to human-readable string
// elapsed_time is stored in milliseconds (from JsUtil.timestamp() differences)
let format_duration_ms = (milliseconds: float): string => {
  let total_seconds = milliseconds /. 1000.0;
  if (milliseconds < 1000.0) {
    Printf.sprintf("%.0fms", milliseconds);
  } else if (milliseconds < 60000.0) {
    Printf.sprintf("%.1fs", total_seconds);
  } else {
    let minutes = floor(total_seconds /. 60.0);
    let remaining_seconds = total_seconds -. minutes *. 60.0;
    Printf.sprintf("%.0fm %.0fs", minutes, remaining_seconds);
  };
};

let view =
    (
      ~globals as _: Globals.t,
      ~agent_model: Agent.Agent.Model.t,
      ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
      ~signal as _: Editors.View.signal => Effect.t(unit),
    )
    : Node.t => {
  let chat_system = agent_model.chat_system;
  let current_chat_id = chat_system.current;
  let current_chat =
    Agent.ChatSystem.Utils.find_chat(current_chat_id, chat_system);
  let workbench: AgentWorkbench.Model.t = current_chat.agent_workbench;

  // Helper to inject workbench UI actions
  let inject_workbench_ui_action =
      (_action: AgentWorkbench.Update.Action.UIAction.action): Effect.t(unit) => {
    Effect.Many([
      agent_inject(
        Agent.Agent.Update.Action.ChatSystemAction(
          Agent.ChatSystem.Update.Action.ChatAction(
            Agent.Chat.Update.Action.WorkbenchAction(
              AgentWorkbench.Update.Action.UIAction(_action),
            ),
            current_chat_id,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  // Toggle archive button
  let toggle_archive = _ => {
    inject_workbench_ui_action(
      AgentWorkbench.Update.Action.UIAction.ToggleShowTaskDictionary,
    );
  };

  // Expand/collapse subtask
  let expand_subtask = (subtask_title: string) => {
    inject_workbench_ui_action(
      AgentWorkbench.Update.Action.UIAction.ExpandSubtask(subtask_title),
    );
  };

  // Toggle tool result expansion
  let toggle_tool_result_expanded =
      (~subtask_title: string, ~tool_result_index: int, ~expanded: bool)
      : Effect.t(unit) => {
    inject_workbench_ui_action(
      AgentWorkbench.Update.Action.UIAction.SetToolResultExpanded(
        subtask_title,
        tool_result_index,
        !expanded,
      ),
    );
  };

  // Render a tool result (reused from ChatMessagesView pattern)
  let render_tool_result =
      (
        ~subtask_title: string,
        ~tool_result_index: int,
        ~tool_result: OpenRouter.Reply.Model.tool_result,
      )
      : Node.t => {
    let toggle_expanded = _ => {
      Effect.Many([
        toggle_tool_result_expanded(
          ~subtask_title,
          ~tool_result_index,
          ~expanded=tool_result.expanded,
        ),
        Effect.Stop_propagation,
      ]);
    };
    let status_icon = tool_result.success ? Icons.confirm : Icons.cancel;
    let status_class =
      tool_result.success ? "tool-call-success" : "tool-call-failure";
    div(
      ~attrs=[clss(["agent-tool-call-inline"])],
      [
        div(
          ~attrs=[
            clss([
              "tool-call-header",
              tool_result.expanded ? "expanded" : "",
            ]),
            Attr.on_click(toggle_expanded),
          ],
          [
            div(
              ~attrs=[clss(["tool-call-status-icon", status_class])],
              [status_icon],
            ),
            div(
              ~attrs=[clss(["tool-call-name"])],
              [text(tool_result.tool_call.name)],
            ),
          ],
        ),
        if (tool_result.expanded) {
          div(
            ~attrs=[clss(["tool-call-content"])],
            [
              div(
                ~attrs=[clss(["tool-call-args"])],
                [
                  div(
                    ~attrs=[clss(["tool-call-args-label"])],
                    [text("Arguments:")],
                  ),
                  div(
                    ~attrs=[clss(["tool-call-args-value"])],
                    [
                      text(
                        Yojson.Safe.pretty_to_string(
                          tool_result.tool_call.args,
                        ),
                      ),
                    ],
                  ),
                ],
              ),
            ],
          );
        } else {
          div(~attrs=[], []);
        },
      ],
    );
  };

  // Render a subtask
  let render_subtask =
      (
        ~task: AgentWorkbench.Model.task,
        ~subtask: AgentWorkbench.Model.subtask,
      )
      : Node.t => {
    let is_active =
      switch (task.active_subtask) {
      | Some(active_item) => active_item == subtask.title
      | None => false
      };
    let is_completed =
      AgentWorkbench.Utils.SubtaskUtils.is_completed(subtask);
    let is_active_and_incomplete = is_active && !is_completed;

    let status_classes =
      ["todo-status-icon"]
      @ (is_completed ? ["completed"] : ["incomplete"])
      @ (is_active_and_incomplete ? ["active-todo"] : []);
    let item_classes =
      ["todo-item"] @ (subtask.subtask_ui.expanded ? ["expanded"] : []);
    div(
      ~attrs=[
        clss(item_classes),
        Attr.on_click(_ => expand_subtask(subtask.title)),
      ],
      [
        div(
          ~attrs=[clss(status_classes)],
          [
            is_completed ? Icons.circle_with_check : Icons.circle_with_no_check,
            if (is_active_and_incomplete) {
              Icons.solid_circle;
            } else {
              Node.text("");
            },
          ],
        ),
        div(
          ~attrs=[clss(["todo-item-content"])],
          [
            div(
              ~attrs=[clss(["todo-item-title-row"])],
              [
                div(
                  ~attrs=[clss(["todo-item-title"])],
                  [text(subtask.title)],
                ),
                switch (subtask.completion_info) {
                | Some(info) =>
                  div(
                    ~attrs=[clss(["todo-item-title-time"])],
                    [text(format_duration_ms(info.elapsed_time))],
                  )
                | None => div(~attrs=[], [])
                },
              ],
            ),
            if (subtask.subtask_ui.expanded) {
              div(
                ~attrs=[clss(["todo-item-details"])],
                [
                  div(
                    ~attrs=[clss(["todo-detail-header-row"])],
                    [
                      div(
                        ~attrs=[clss(["todo-detail-header"])],
                        [text("Description")],
                      ),
                    ],
                  ),
                  div(
                    ~attrs=[clss(["todo-detail-text"])],
                    [text(subtask.description)],
                  ),
                  switch (subtask.completion_info) {
                  | Some(info) =>
                    div(
                      ~attrs=[clss(["todo-detail-section"])],
                      [
                        div(
                          ~attrs=[clss(["todo-detail-header"])],
                          [text("Summary of Changes")],
                        ),
                        div(
                          ~attrs=[clss(["todo-detail-text"])],
                          [text(info.summary)],
                        ),
                      ],
                    )
                  | None => div(~attrs=[], [])
                  },
                  if (List.length(subtask.tools_used) > 0) {
                    div(
                      ~attrs=[clss(["todo-detail-section"])],
                      [
                        div(
                          ~attrs=[clss(["todo-detail-header"])],
                          [text("Tools Used")],
                        ),
                        div(
                          ~attrs=[clss(["todo-tool-results"])],
                          List.mapi(
                            (
                              index: int,
                              tool_result: OpenRouter.Reply.Model.tool_result,
                            ) =>
                              render_tool_result(
                                ~subtask_title=subtask.title,
                                ~tool_result_index=index,
                                ~tool_result,
                              ),
                            subtask.tools_used,
                          ),
                        ),
                      ],
                    );
                  } else {
                    div(~attrs=[], []);
                  },
                ],
              );
            } else {
              div(~attrs=[], []);
            },
          ],
        ),
      ],
    );
  };

  // Render active task
  let render_active_task = (active_task: AgentWorkbench.Model.task): Node.t => {
    div(
      ~attrs=[clss(["todo-list-container"])],
      [
        // Title row
        div(
          ~attrs=[clss(["todo-list-title"])],
          [
            div(
              ~attrs=[clss(["todo-list-title-left"])],
              [text(active_task.title)],
            ),
            switch (active_task.completion_info) {
            | Some(info) =>
              div(
                ~attrs=[clss(["todo-list-title-time"])],
                [text(format_duration_ms(info.elapsed_time))],
              )
            | None => div(~attrs=[], [])
            },
          ],
        ),
        // Subtasks list
        div(
          ~attrs=[clss(["todo-items"])],
          List.map(
            (subtask: AgentWorkbench.Model.subtask) =>
              render_subtask(~task=active_task, ~subtask),
            AgentWorkbench.Utils.TaskUtils.ordered_subtasks_of(active_task),
          ),
        ),
      ],
    );
  };

  // Render task archive menu
  let render_archive_menu = (): Node.t =>
    if (!workbench.t_ui.show_archive) {
      div(~attrs=[], []);
    } else {
      let sorted_tasks =
        AgentWorkbench.Utils.TaskDictUtils.sorted_task_dict(
          workbench.task_dict,
        );
      div(
        ~attrs=[clss(["todo-archive-menu"])],
        [
          div(
            ~attrs=[clss(["history-menu-header"])],
            [text("Task Archive")],
          ),
          div(
            ~attrs=[clss(["history-menu-list"])],
            List.map(
              (task: AgentWorkbench.Model.task) => {
                let is_displayed =
                  switch (workbench.t_ui.display_task) {
                  | Some(displayed_title) => displayed_title == task.title
                  | None => false
                  };
                let switch_to_task = _ => {
                  Effect.Many([
                    inject_workbench_ui_action(
                      AgentWorkbench.Update.Action.UIAction.SetDisplayTask(
                        task.title,
                      ),
                    ),
                    Effect.Stop_propagation,
                  ]);
                };
                div(
                  ~attrs=[
                    clss(
                      ["history-menu-item"]
                      @ (is_displayed ? ["active"] : []),
                    ),
                    Attr.on_click(switch_to_task),
                  ],
                  [
                    div(
                      ~attrs=[clss(["history-menu-item-content"])],
                      [text(task.title)],
                    ),
                    div(
                      ~attrs=[clss(["history-menu-item-actions"])],
                      [
                        div(
                          ~attrs=[clss(["history-menu-item-time"])],
                          [
                            text(
                              TimeUtil.format_time_diff(
                                task.metadata.last_updated_at,
                              ),
                            ),
                          ],
                        ),
                      ],
                    ),
                  ],
                );
              },
              sorted_tasks,
            ),
          ),
        ],
      );
    };

  // Main workbench view
  div(
    ~attrs=[clss(["workbench-view"])],
    [
      // Archive button
      div(
        ~attrs=[clss(["todo-archive-button-container"])],
        [
          div(
            ~attrs=[
              clss(["chat-action-button", "icon"]),
              Attr.on_click(toggle_archive),
              Attr.title("Task Archive"),
            ],
            [Icons.library],
          ),
        ],
      ),
      // Task display or empty state (scrollable content)
      div(
        ~attrs=[clss(["workbench-content"])],
        [
          switch (AgentWorkbench.Utils.MainUtils.displayed_task(workbench)) {
          | Some(displayed_task) => render_active_task(displayed_task)
          | None =>
            switch (AgentWorkbench.Utils.MainUtils.active_task(workbench)) {
            | Some(active_task) => render_active_task(active_task)
            | None =>
              div(
                ~attrs=[clss(["no-todo-list"])],
                [text("No active task.")],
              )
            }
          },
        ],
      ),
      // Archive menu (if shown) - positioned absolutely
      render_archive_menu(),
    ],
  );
};
