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
    ViewTools.expand,
    ViewTools.collapse,
    ProbeTools.place_probe,
    ProbeTools.remove_probe,
    ProbeTools.toggle_probe,
    StaticsTools.place_statics,
    StaticsTools.remove_statics,
    StaticsTools.toggle_statics,
    EditTools.initialize,
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
            | "initialize" => Initialize(get_string(args, "code"))
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
              EditorAction(
                Insert(
                  After,
                  get_string(args, "path"),
                  get_string(args, "code"),
                ),
              )
            | "insert_before" =>
              EditorAction(
                Insert(
                  Before,
                  get_string(args, "path"),
                  get_string(args, "code"),
                ),
              )
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
    | ProbeAction(PlaceProbe(paths)) =>
      "place_probe(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | ProbeAction(RemoveProbe(paths)) =>
      "remove_probe(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | ProbeAction(ToggleProbe(paths)) =>
      "toggle_probe(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | StaticsAction(PlaceStatics(paths)) =>
      "place_statics(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | StaticsAction(RemoveStatics(paths)) =>
      "remove_statics(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | StaticsAction(ToggleStatics(paths)) =>
      "toggle_statics(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | LanguageServerAction(ShowUseSites(path)) =>
      "show_use_sites(\"" ++ path ++ "\")"
    | LanguageServerAction(ShowReferences(path)) =>
      "show_references(\"" ++ path ++ "\")"
    | Initialize(code) => "initialize(\"" ++ code ++ "\")"
    | EditorAction(Update(Definition, path, code)) =>
      "update_definition(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Update(Body, path, code)) =>
      "update_body(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Update(Pattern, path, code)) =>
      "update_pattern(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Update(BindingClause, path, code)) =>
      "update_binding_clause(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Delete(BindingClause, path)) =>
      "delete_binding_clause(\"" ++ path ++ "\")"
    | EditorAction(Delete(Body, path)) => "delete_body(\"" ++ path ++ "\")"
    | EditorAction(Delete(Definition | Pattern, path)) =>
      "delete(\"" ++ path ++ "\")"
    | EditorAction(Insert(After, path, code)) =>
      "insert_after(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | EditorAction(Insert(Before, path, code)) =>
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
    | WorkbenchAction(MarkActiveSubtaskFailed(reason)) =>
      "mark_active_subtask_failed(\"" ++ reason ++ "\")"
    | WorkbenchAction(MarkActiveTaskFailed(reason)) =>
      "mark_active_task_failed(\"" ++ reason ++ "\")"
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
  let tools = Local.tools;
  let action_of = (~tool_name: string, ~args: API.Json.t): action_wrapper => {
    Local.action_of(~tool_name, ~args);
  };
  let string_of = (action: action) => {
    Local.string_of(action);
  };
};
