open Util_web;
open CompositionActions;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type action_wrapper =
  | Action(action)
  | Failure(string);

module Local = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(API.Json.t);

  let tools = [
    ViewTools.expand,
    ViewTools.collapse,
    ProbeTools.place_probe,
    ProbeTools.remove_probe,
    ProbeTools.toggle_probe,
    StaticsTools.place_statics,
    StaticsTools.remove_statics,
    StaticsTools.toggle_statics,
    SyntaxProjectorTools.place_syntax_projector,
    SyntaxProjectorTools.remove_syntax_projector,
    SyntaxProjectorTools.toggle_syntax_projector,
    EditTools.update_definition,
    EditTools.update_body,
    EditTools.update_pattern,
    EditTools.update_binding_clause,
    EditTools.delete_binding_clause,
    EditTools.delete_body,
    EditTools.insert_after,
    EditTools.insert_before,
    WorkbenchTools.create_new_task,
    WorkbenchTools.set_active_task,
    WorkbenchTools.unset_active_task,
    WorkbenchTools.set_active_subtask,
    WorkbenchTools.unset_active_subtask,
    WorkbenchTools.mark_active_task_complete,
    WorkbenchTools.mark_active_task_incomplete,
    WorkbenchTools.mark_active_subtask_complete,
    WorkbenchTools.mark_active_subtask_incomplete,
    WorkbenchTools.mark_active_subtask_failed,
    WorkbenchTools.mark_active_task_failed,
    WorkbenchTools.add_new_subtask_to_active_task,
    WorkbenchTools.reorder_subtasks_in_active_task,
    WorkbenchTools.update_active_task,
    WorkbenchTools.update_active_subtask,
    WorkbenchTools.delete_task,
    WorkbenchTools.delete_subtask,
  ];

  /** Read an optional string field: None when absent or empty-string, Some otherwise.
      Empty-string is treated as absent so LLMs that emit `"path": ""` still hit the
      no-path branch of insert_before/insert_after. */
  let get_optional_string = (args: API.Json.t, field: string): option(string) => {
    switch (API.Json.dot(field, args)) {
    | Some(`String("")) => None
    | Some(`String(s)) => Some(s)
    | _ => None
    };
  };

  let syntax_projector_kind_of_string = (s: string): ProjectorKind.t => {
    let k = ProjectorKind.of_name(String.trim(s));
    if (ProjectorKind.is_refractor(k)) {
      raise(
        Failure(
          "syntax projector kind cannot be probe or statics — use the probe or statics tools",
        ),
      );
    };
    k;
  };

  let action_of = (~tool_name: string, ~args: API.Json.t): action_wrapper => {
    /* Possible arguments */
    /* Parsing here to avoid redundancy */
    /* Argument(s) may or may not be provided depending on the tool called */

    let action =
      try(
        {
          open API.Json.Parsers;
          let action =
            switch (tool_name) {
            | "expand" =>
              AgentContextAction(Expand(get_string_list(args, "paths")))
            | "collapse" =>
              AgentContextAction(Collapse(get_string_list(args, "paths")))
            | "place_probe" =>
              ProbeAction(PlaceProbe(get_string_list(args, "paths")))
            | "remove_probe" =>
              ProbeAction(RemoveProbe(get_string_list(args, "paths")))
            | "toggle_probe" =>
              ProbeAction(ToggleProbe(get_string_list(args, "paths")))
            | "place_statics" =>
              StaticsAction(PlaceStatics(get_string_list(args, "paths")))
            | "remove_statics" =>
              StaticsAction(RemoveStatics(get_string_list(args, "paths")))
            | "toggle_statics" =>
              StaticsAction(ToggleStatics(get_string_list(args, "paths")))
            | "place_syntax_projector" =>
              SyntaxProjectorAction(
                PlaceSyntaxProjector(
                  syntax_projector_kind_of_string(get_string(args, "kind")),
                  get_string_list(args, "paths"),
                ),
              )
            | "remove_syntax_projector" =>
              SyntaxProjectorAction(
                RemoveSyntaxProjector(get_string_list(args, "paths")),
              )
            | "toggle_syntax_projector" =>
              SyntaxProjectorAction(
                ToggleSyntaxProjector(
                  syntax_projector_kind_of_string(get_string(args, "kind")),
                  get_string_list(args, "paths"),
                ),
              )
            | "update_definition" =>
              EditorAction(
                Update(
                  Definition,
                  get_string(args, "path"),
                  get_string(args, "code"),
                ),
              )
            | "update_body" =>
              EditorAction(
                Update(
                  Body,
                  get_string(args, "path"),
                  get_string(args, "code"),
                ),
              )
            | "update_pattern" =>
              EditorAction(
                Update(
                  Pattern,
                  get_string(args, "path"),
                  get_string(args, "code"),
                ),
              )
            | "update_binding_clause" =>
              EditorAction(
                Update(
                  BindingClause,
                  get_string(args, "path"),
                  get_string(args, "code"),
                ),
              )
            | "insert_after" =>
              /* Per-line leading-whitespace trim happens at the paste
                 funnel, [[CompositionGo.PerformUtils.introduce]] */
              let code = get_string(args, "code");
              switch (get_optional_string(args, "path")) {
              | Some(path) => EditorAction(Insert(After, path, code))
              | None => InsertAtProgramBoundary(After, code)
              };
            | "insert_before" =>
              let code = get_string(args, "code");
              switch (get_optional_string(args, "path")) {
              | Some(path) => EditorAction(Insert(Before, path, code))
              | None => InsertAtProgramBoundary(Before, code)
              };
            | "delete_binding_clause" =>
              EditorAction(Delete(BindingClause, get_string(args, "path")))
            | "delete_body" =>
              EditorAction(Delete(Body, get_string(args, "path")))
            | "create_new_task" =>
              WorkbenchAction(
                CreateNewTask(
                  {
                    let task_json =
                      switch (API.Json.dot("task", args)) {
                      | Some(task_obj) =>
                        // Format 1: nested in "task" field (from schema)
                        task_obj
                      | None =>
                        // Format 2: direct format (LLM sends task object directly)
                        args
                      };
                    AgentWorkbench.Utils.TaskUtils.json_to_task(task_json);
                  },
                ),
              )
            | "unset_active_task" => WorkbenchAction(UnsetActiveTask)
            | "set_active_task" =>
              WorkbenchAction(SetActiveTask(get_string(args, "title")))
            | "unset_active_subtask" => WorkbenchAction(UnsetActiveSubtask)
            | "set_active_subtask" =>
              WorkbenchAction(SetActiveSubtask(get_string(args, "title")))
            | "mark_active_task_complete" =>
              WorkbenchAction(
                MarkActiveTaskComplete(get_string(args, "summary")),
              )
            | "mark_active_task_incomplete" =>
              WorkbenchAction(MarkActiveTaskIncomplete)
            | "mark_active_subtask_complete" =>
              WorkbenchAction(
                MarkActiveSubtaskComplete(get_string(args, "summary")),
              )
            | "mark_active_subtask_incomplete" =>
              WorkbenchAction(MarkActiveSubtaskIncomplete)
            | "mark_active_subtask_failed" =>
              WorkbenchAction(
                MarkActiveSubtaskFailed(get_string(args, "reason")),
              )
            | "mark_active_task_failed" =>
              WorkbenchAction(
                MarkActiveTaskFailed(get_string(args, "reason")),
              )
            | "add_new_subtask_to_active_task" =>
              WorkbenchAction(
                AddNewSubtaskToActiveTask(
                  {
                    let subtask_json =
                      API.Json.Parsers.get_json(args, "subtask");
                    AgentWorkbench.Utils.SubtaskUtils.json_to_subtask(
                      subtask_json,
                    );
                  },
                ),
              )
            | "reorder_subtasks_in_active_task" =>
              WorkbenchAction(
                ReorderSubtasksInActiveTask(
                  get_string_list(args, "subtasks_ordering"),
                ),
              )
            | "update_active_task" =>
              WorkbenchAction(
                UpdateActiveTask(
                  get_optional_string(args, "new_title"),
                  get_optional_string(args, "new_description"),
                ),
              )
            | "update_active_subtask" =>
              WorkbenchAction(
                UpdateActiveSubtask(
                  get_optional_string(args, "new_title"),
                  get_optional_string(args, "new_description"),
                ),
              )
            | "delete_task" =>
              WorkbenchAction(DeleteTask(get_string(args, "title")))
            | "delete_subtask" =>
              WorkbenchAction(DeleteSubtask(get_string(args, "title")))
            | _ => raise(Failure("The tool called does not exist."))
            };
          Action(action);
        }
      ) {
      | Failure(s: string) => Failure(s)
      };
    action;
  };
};

module Public = {
  let tools = Local.tools;
  let action_of = (~tool_name: string, ~args: API.Json.t): action_wrapper => {
    Local.action_of(~tool_name, ~args);
  };
};
