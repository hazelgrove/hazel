open Virtual_dom.Vdom;
open Node;
open Util_web.WebUtil;
open Util_web;
open Haz3lcore;

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
      ~globals: Globals.t,
      ~agent_model: Agent.Model.t,
      ~agent_inject: Agent.Update.Action.t => Effect.t(unit),
      ~signal as _: Editors.View.signal => Effect.t(unit),
      ~code_with_statics: CodeWithStatics.Model.t,
    )
    : Node.t => {
  let chat_system = agent_model.chat_system;
  // Build the high-level node map once for this render pass, used by tool-call
  // rows for stale-path detection and cmd/ctrl-click jump targets.
  let node_map: option(HighLevelNodeMap.t) = {
    let z = code_with_statics.editor.state.zipper;
    let info_map = CompositionGo.Public.mk_statics(z);
    HighLevelNodeMap.build(z, info_map);
  };
  let current_chat_id = chat_system.current;
  let current_chat = ChatSystem.Utils.find_chat(current_chat_id, chat_system);
  let workbench: AgentWorkbench.Model.t = current_chat.agent_workbench;

  let inject_workbench_ui_action =
      (_action: AgentWorkbench.Update.Action.UIAction.action): Effect.t(unit) => {
    Effect.Many([
      agent_inject(
        Agent.Update.Action.ChatSystemAction(
          ChatSystem.Update.Action.ChatAction(
            Chat.Update.Action.WorkbenchAction(
              AgentWorkbench.Update.Action.UIAction(_action),
            ),
            current_chat_id,
          ),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };

  let toggle_archive = _ => {
    inject_workbench_ui_action(
      AgentWorkbench.Update.Action.UIAction.ToggleShowTaskDictionary,
    );
  };

  let set_active_as_displayed = _ => {
    switch (AgentWorkbench.Utils.MainUtils.active_task(workbench)) {
    | Some(active_task) =>
      inject_workbench_ui_action(
        AgentWorkbench.Update.Action.UIAction.SetDisplayTask(
          active_task.title,
        ),
      )
    | None => Effect.Ignore
    };
  };

  let expand_subtask = (subtask_title: string) => {
    inject_workbench_ui_action(
      AgentWorkbench.Update.Action.UIAction.ExpandSubtask(subtask_title),
    );
  };

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

  let render_tool_result =
      (
        ~subtask_title: string,
        ~tool_result_index: int,
        ~tool_result: AgentToolResult.tool_result,
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
    ToolResultView.view(
      ~globals,
      ~node_map?,
      ~tool_result,
      ~toggle_expanded,
      (),
    );
  };

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
    let is_failed = AgentWorkbench.Utils.SubtaskUtils.is_failed(subtask);
    let is_active_and_incomplete = is_active && !is_completed;
    let is_expanded = subtask.subtask_ui.expanded;

    let status_classes =
      ["wb-subtask-status"]
      @ (is_completed && !is_failed ? ["completed"] : [])
      @ (is_failed ? ["failed"] : [])
      @ (is_active_and_incomplete ? ["active"] : []);

    let item_classes =
      ["wb-subtask"]
      @ (is_expanded ? ["expanded"] : [])
      @ (is_active_and_incomplete ? ["is-active"] : [])
      @ (is_completed && !is_failed ? ["is-completed"] : [])
      @ (is_failed ? ["is-failed"] : []);

    div(
      ~attrs=[
        clss(item_classes),
        Attr.on_click(_ => expand_subtask(subtask.title)),
      ],
      [
        div(
          ~attrs=[clss(status_classes)],
          [
            if (is_failed) {
              Icons.cancel;
            } else if (is_completed) {
              Icons.circle_with_check;
            } else if (is_active_and_incomplete) {
              Icons.solid_circle;
            } else {
              Icons.circle_with_no_check;
            },
          ],
        ),
        div(
          ~attrs=[clss(["wb-subtask-content"])],
          [
            div(
              ~attrs=[clss(["wb-subtask-header"])],
              [
                span(
                  ~attrs=[clss(["wb-subtask-title"])],
                  [text(subtask.title)],
                ),
                switch (subtask.completion_info) {
                | Some(info) =>
                  span(
                    ~attrs=[clss(["wb-subtask-time"])],
                    [text(format_duration_ms(info.elapsed_time))],
                  )
                | None => Node.none
                },
              ],
            ),
            if (is_expanded) {
              div(
                ~attrs=[clss(["wb-subtask-details"])],
                [
                  div(
                    ~attrs=[clss(["wb-detail-block"])],
                    [
                      div(
                        ~attrs=[clss(["wb-detail-label"])],
                        [text("Description")],
                      ),
                      div(
                        ~attrs=[clss(["wb-detail-body"])],
                        [text(subtask.description)],
                      ),
                    ],
                  ),
                  switch (subtask.completion_info) {
                  | Some(info) =>
                    div(
                      ~attrs=[clss(["wb-detail-block"])],
                      [
                        div(
                          ~attrs=[clss(["wb-detail-label"])],
                          [text("Summary")],
                        ),
                        div(
                          ~attrs=[clss(["wb-detail-body"])],
                          [text(info.summary)],
                        ),
                      ],
                    )
                  | None => Node.none
                  },
                  if (List.length(subtask.tools_used) > 0) {
                    div(
                      ~attrs=[clss(["wb-detail-block"])],
                      [
                        div(
                          ~attrs=[clss(["wb-detail-label"])],
                          [text("Tools Used")],
                        ),
                        div(
                          ~attrs=[clss(["wb-tool-results"])],
                          List.mapi(
                            (
                              index: int,
                              tool_result: AgentToolResult.tool_result,
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
                    Node.none;
                  },
                ],
              );
            } else {
              Node.none;
            },
          ],
        ),
      ],
    );
  };

  let render_task = (task: AgentWorkbench.Model.task): Node.t => {
    let ordered_subtasks =
      AgentWorkbench.Utils.TaskUtils.ordered_subtasks_of(task);
    let total = List.length(ordered_subtasks);
    let completed =
      List.length(
        List.filter(
          AgentWorkbench.Utils.SubtaskUtils.is_completed,
          ordered_subtasks,
        ),
      );
    let is_task_complete =
      switch (task.completion_info) {
      | Some(_) => true
      | None => false
      };

    div(
      ~attrs=[clss(["wb-task-container"])],
      [
        div(
          ~attrs=[clss(["wb-task-header"])],
          [
            div(
              ~attrs=[clss(["wb-task-title-row"])],
              [
                div(
                  ~attrs=[
                    clss([
                      "wb-task-title",
                      is_task_complete ? "completed" : "",
                    ]),
                  ],
                  [text(task.title)],
                ),
                switch (task.completion_info) {
                | Some(info) =>
                  span(
                    ~attrs=[clss(["wb-task-time"])],
                    [text(format_duration_ms(info.elapsed_time))],
                  )
                | None => Node.none
                },
              ],
            ),
            if (total > 0) {
              div(
                ~attrs=[clss(["wb-progress"])],
                [
                  div(
                    ~attrs=[clss(["wb-progress-bar"])],
                    [
                      div(
                        ~attrs=[
                          clss([
                            "wb-progress-fill",
                            is_task_complete ? "complete" : "",
                          ]),
                          Attr.style(
                            Css_gen.create(
                              ~field="width",
                              ~value=
                                (
                                  total > 0
                                    ? string_of_int(completed * 100 / total)
                                    : "0"
                                )
                                ++ "%",
                            ),
                          ),
                        ],
                        [],
                      ),
                    ],
                  ),
                  span(
                    ~attrs=[clss(["wb-progress-label"])],
                    [
                      text(
                        string_of_int(completed)
                        ++ "/"
                        ++ string_of_int(total),
                      ),
                    ],
                  ),
                ],
              );
            } else {
              Node.none;
            },
            switch (task.description) {
            | "" => Node.none
            | desc =>
              div(~attrs=[clss(["wb-task-description"])], [text(desc)])
            },
          ],
        ),
        div(
          ~attrs=[clss(["wb-subtask-list"])],
          List.map(
            (subtask: AgentWorkbench.Model.subtask) =>
              render_subtask(~task, ~subtask),
            ordered_subtasks,
          ),
        ),
      ],
    );
  };

  let render_archive_menu = (): Node.t =>
    if (!workbench.t_ui.show_archive) {
      Node.none;
    } else {
      let sorted_tasks =
        AgentWorkbench.Utils.TaskDictUtils.sorted_task_dict(
          workbench.task_dict,
        );
      div(
        ~attrs=[clss(["wb-archive-overlay"])],
        [
          div(
            ~attrs=[
              clss(["wb-archive-backdrop"]),
              Attr.on_click(toggle_archive),
            ],
            [],
          ),
          div(
            ~attrs=[clss(["wb-archive-panel"])],
            [
              div(
                ~attrs=[clss(["wb-archive-header"])],
                [
                  span(~attrs=[], [text("Task Archive")]),
                  div(
                    ~attrs=[
                      clss(["wb-archive-close", "icon"]),
                      Attr.on_click(toggle_archive),
                    ],
                    [Icons.cancel],
                  ),
                ],
              ),
              div(
                ~attrs=[clss(["wb-archive-list"])],
                List.map(
                  (task: AgentWorkbench.Model.task) => {
                    let is_displayed =
                      switch (workbench.t_ui.display_task) {
                      | Some(displayed_title) => displayed_title == task.title
                      | None => false
                      };
                    let is_active =
                      switch (workbench.active_task) {
                      | Some(active_title) => active_title == task.title
                      | None => false
                      };
                    let is_completed =
                      switch (task.completion_info) {
                      | Some(_) => true
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
                          ["wb-archive-item"]
                          @ (is_displayed ? ["selected"] : [])
                          @ (is_active ? ["active"] : []),
                        ),
                        Attr.on_click(switch_to_task),
                      ],
                      [
                        div(
                          ~attrs=[
                            clss([
                              "wb-archive-item-icon",
                              is_completed ? "completed" : "",
                              is_active && !is_completed ? "active" : "",
                            ]),
                          ],
                          [
                            is_completed
                              ? Icons.circle_with_check
                              : Icons.circle_with_no_check,
                          ],
                        ),
                        div(
                          ~attrs=[clss(["wb-archive-item-content"])],
                          [
                            div(
                              ~attrs=[clss(["wb-archive-item-title"])],
                              [text(task.title)],
                            ),
                            div(
                              ~attrs=[clss(["wb-archive-item-time"])],
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
          ),
        ],
      );
    };

  div(
    ~attrs=[clss(["workbench-view"])],
    [
      div(
        ~attrs=[clss(["wb-toolbar"])],
        [
          div(
            ~attrs=[
              clss(["wb-toolbar-btn", "icon"]),
              Attr.on_click(toggle_archive),
              Attr.title("Task Archive"),
            ],
            [Icons.library],
          ),
          switch (AgentWorkbench.Utils.MainUtils.active_task(workbench)) {
          | Some(active_task) =>
            switch (active_task.completion_info) {
            | None =>
              div(
                ~attrs=[
                  clss(["wb-active-task-pill"]),
                  Attr.on_click(set_active_as_displayed),
                  Attr.title("Jump to active task"),
                ],
                [
                  div(~attrs=[clss(["wb-active-dot"])], []),
                  span(~attrs=[], [text(active_task.title)]),
                ],
              )
            | Some(_) => Node.none
            }
          | None => Node.none
          },
        ],
      ),
      div(
        ~attrs=[clss(["workbench-content"])],
        [
          switch (AgentWorkbench.Utils.MainUtils.displayed_task(workbench)) {
          | Some(displayed_task) => render_task(displayed_task)
          | None =>
            switch (AgentWorkbench.Utils.MainUtils.active_task(workbench)) {
            | Some(active_task) => render_task(active_task)
            | None =>
              div(
                ~attrs=[clss(["wb-empty-state"])],
                [
                  div(~attrs=[clss(["wb-empty-icon"])], [Icons.library]),
                  div(
                    ~attrs=[clss(["wb-empty-text"])],
                    [text("No tasks yet")],
                  ),
                  div(
                    ~attrs=[clss(["wb-empty-hint"])],
                    [
                      text(
                        "Tasks will appear here as the agent works on your requests.",
                      ),
                    ],
                  ),
                ],
              )
            }
          },
        ],
      ),
      render_archive_menu(),
    ],
  );
};
