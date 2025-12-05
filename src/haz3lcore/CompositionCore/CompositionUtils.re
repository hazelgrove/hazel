open Util;
open CompositionActions;

[@deriving (show({with_path: false}), sexp, yojson)]
type action_wrapper =
  | Action(action)
  | Failure(string);

module Local = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(API.Json.t);

  let tools = [
    // NavTools.go_to_parent, // No current node should have a parent anymore... we nav the top-level nodes
    // NavTools.go_to_child,
    ViewTools.expand,
    ViewTools.collapse,
    EditTools.initialize, // For initializing empty or nodeless programs
    EditTools.update_definition,
    EditTools.update_body,
    EditTools.update_pattern,
    EditTools.update_binding_clause,
    EditTools.delete_binding_clause,
    EditTools.delete_body,
    EditTools.insert_after,
    EditTools.insert_before,
    // ViewTools.view_entire_definition, // No longer needed is this top-level refactor... this is done by default
    // ViewTools.view_context,
    WorkbenchTools.create_new_task,
    WorkbenchTools.set_active_task,
    WorkbenchTools.unset_active_task,
    WorkbenchTools.set_active_subtask,
    WorkbenchTools.unset_active_subtask,
    WorkbenchTools.mark_active_task_complete,
    WorkbenchTools.mark_active_task_incomplete,
    WorkbenchTools.mark_active_subtask_complete,
    WorkbenchTools.mark_active_subtask_incomplete,
  ];

  let get_string_arg = (~arg: option(string), ~fail_with: string) => {
    switch (arg) {
    | Some(arg) => arg
    | None => raise(Failure(fail_with))
    };
  };

  let get_path = (~path: option(string)) => {
    switch (path) {
    | Some(path) => path
    | None => raise(Failure("A path must be provided for the action"))
    };
  };

  let get_paths = (~paths: option(list(string))) => {
    switch (paths) {
    | Some(paths) => paths
    | None =>
      raise(Failure("A list of paths must be provided for the action"))
    };
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
            | "initialize" =>
              EditorAction(Edit(Initialize(get_string(args, "code"))))
            | "update_definition" =>
              EditorAction(
                Edit(
                  UpdateDefinition(
                    get_string(args, "path"),
                    get_string(args, "code"),
                  ),
                ),
              )
            | "update_body" =>
              EditorAction(
                Edit(
                  UpdateBody(
                    get_string(args, "path"),
                    get_string(args, "code"),
                  ),
                ),
              )
            | "update_pattern" =>
              EditorAction(
                Edit(
                  UpdatePattern(
                    get_string(args, "path"),
                    get_string(args, "code"),
                  ),
                ),
              )
            | "update_binding_clause" =>
              EditorAction(
                Edit(
                  UpdateBindingClause(
                    get_string(args, "path"),
                    get_string(args, "code"),
                  ),
                ),
              )
            | "insert_after" =>
              EditorAction(
                Edit(
                  InsertAfter(
                    get_string(args, "path"),
                    get_string(args, "code"),
                  ),
                ),
              )
            | "insert_before" =>
              EditorAction(
                Edit(
                  InsertBefore(
                    get_string(args, "path"),
                    get_string(args, "code"),
                  ),
                ),
              )
            | "delete_binding_clause" =>
              EditorAction(
                Edit(DeleteBindingClause(get_string(args, "path"))),
              )
            | "delete_body" =>
              EditorAction(Edit(DeleteBody(get_string(args, "path"))))
            | "create_new_task" =>
              WorkbenchAction(
                CreateNewTask(
                  {
                    let task_json = API.Json.Parsers.get_json(args, "task");
                    AgentWorkbench.Utils.TaskUtils.json_to_task(task_json);
                  },
                ),
              )
            | "unset_active_task" => WorkbenchAction(UnsetActiveTask)
            | "set_active_task" =>
              WorkbenchAction(SetActiveTask(get_string(args, "task_title")))
            | "unset_active_subtask" => WorkbenchAction(UnsetActiveSubtask)
            | "set_active_subtask" =>
              WorkbenchAction(
                SetActiveSubtask(get_string(args, "subtask_title")),
              )
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
            | _ => raise(Failure("The tool called does not exist."))
            };
          Action(action);
        }
      ) {
      | Failure(s: string) => Failure(s)
      };
    action;
  };

  let string_of = (action: action) => {
    switch (action) {
    | AgentContextAction(Expand(paths)) =>
      "expand(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | AgentContextAction(Collapse(paths)) =>
      "collapse(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | EditorAction(Read(ShowUseSites(path))) =>
      "show_use_sites(\"" ++ path ++ "\")"
    | EditorAction(Read(ShowReferences(path))) =>
      "show_references(\"" ++ path ++ "\")"
    | EditorAction(Edit(Initialize(code))) =>
      "initialize(\"" ++ code ++ "\")"
    | EditorAction(Edit(UpdateDefinition(path, code))) =>
      "update_definition(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Edit(UpdateBody(path, code))) =>
      "update_body(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Edit(UpdatePattern(path, code))) =>
      "update_pattern(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Edit(UpdateBindingClause(path, code))) =>
      "update_binding_clause(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Edit(DeleteBindingClause(path))) =>
      "delete_binding_clause(\"" ++ path ++ "\")"
    | EditorAction(Edit(DeleteBody(path))) =>
      "delete_body(\"" ++ path ++ "\")"
    | EditorAction(Edit(InsertAfter(path, code))) =>
      "insert_after(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Edit(InsertBefore(path, code))) =>
      "insert_before(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | WorkbenchAction(CreateNewTask(task)) =>
      "create_new_task( "
      ++ AgentWorkbench.Utils.TaskUtils.task_to_json_string(task)
      ++ " )"
    | WorkbenchAction(UnsetActiveTask) => "unset_active_task"
    | WorkbenchAction(SetActiveTask(task_title)) =>
      "set_active_task(\"" ++ task_title ++ "\")"
    | WorkbenchAction(UnsetActiveSubtask) => "unset_active_subtask"
    | WorkbenchAction(SetActiveSubtask(subtask_title)) =>
      "set_active_subtask(\"" ++ subtask_title ++ "\")"
    | WorkbenchAction(MarkActiveTaskComplete(summary)) =>
      "mark_active_task_complete(\"" ++ summary ++ "\")"
    | WorkbenchAction(MarkActiveTaskIncomplete) => "mark_active_task_incomplete"
    | WorkbenchAction(MarkActiveSubtaskComplete(summary)) =>
      "mark_active_subtask_complete(\"" ++ summary ++ "\")"
    | WorkbenchAction(MarkActiveSubtaskIncomplete) => "mark_active_subtask_incomplete"
    | WorkbenchAction(AddNewSubtaskToActiveTask(subtask)) =>
      "add_new_subtask_to_active_task( "
      ++ AgentWorkbench.Utils.SubtaskUtils.subtask_to_json_string(subtask)
      ++ ")"
    | WorkbenchAction(ReorderSubtasksInActiveTask(subtasks_ordering)) =>
      "reorder_subtasks_in_active_task( \"["
      ++ String.concat(", ", subtasks_ordering)
      ++ "]\" )"
    };
  };
};

module Public = {
  [@der]
  let tools = Local.tools;
  let action_of = (~tool_name: string, ~args: API.Json.t): action_wrapper => {
    Local.action_of(~tool_name, ~args);
  };
  let string_of = (action: action) => {
    Local.string_of(action);
  };
};
