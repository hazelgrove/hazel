open Util;
open CompositionActions;

[@deriving (show({with_path: false}), sexp, yojson)]
type action = CompositionActions.composition_action;

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
    let get_path = () => {
      switch (API.Json.dot("path", args)) {
      | Some(`String(path)) => path
      | _ => raise(Failure("A path must be provided for the action"))
      };
    };
    let get_paths = () => {
      switch (API.Json.dot("paths", args)) {
      | Some(`List(paths)) =>
        List.map(
          path =>
            switch (path) {
            | `String(path) => path
            | _ => raise(Failure("A path must be provided for the action"))
            },
          paths,
        )
      | _ =>
        raise(Failure("A list of paths must be provided for the action"))
      };
    };
    let get_code = () => {
      switch (API.Json.dot("code", args)) {
      | Some(`String(code)) => code
      | _ => raise(Failure("A code must be provided for the action"))
      };
    };
    let get_title = (item: API.Json.t) => {
      switch (API.Json.dot("title", item)) {
      | Some(`String(title)) => title
      | _ => raise(Failure("A title must be provided for the todo item"))
      };
    };
    let get_summary = (item: API.Json.t) => {
      switch (API.Json.dot("summary", item)) {
      | Some(`String(summary)) => summary
      | _ => raise(Failure("A summary must be provided for the todo item"))
      };
    };
    let get_description = (item: API.Json.t) => {
      switch (API.Json.dot("description", item)) {
      | Some(`String(description)) => description
      | _ =>
        raise(Failure("A description must be provided for the todo item"))
      };
    };
    let json_to_subtask =
        (item: API.Json.t): CompositionAgentWorkbench.Model.subtask => {
      let title = get_title(item);
      let description = get_description(item);
      CompositionAgentWorkbench.Utils.SubtaskUtils.mk(~title, ~description);
    };
    let get_subtask = (): CompositionAgentWorkbench.Model.subtask => {
      let subtask_json =
        switch (API.Json.dot("subtask", args)) {
        | Some(subtask_json) => subtask_json
        | _ => raise(Failure("Each subtask must be a JSON object"))
        };
      json_to_subtask(subtask_json);
    };
    let get_subtasks =
        (item: API.Json.t): list(CompositionAgentWorkbench.Model.subtask) => {
      let subtasks_json =
        switch (API.Json.dot("subtasks", item)) {
        | Some(`List(subtasks_json)) => subtasks_json
        | _ =>
          raise(
            Failure("A list of subtasks must be provided for the action"),
          )
        };
      List.map(json_to_subtask, subtasks_json);
    };
    let get_subtasks_ordering = (): list(string) => {
      let subtasks_ordering_json =
        switch (API.Json.dot("subtasks_ordering", args)) {
        | Some(`List(subtasks_ordering_json)) => subtasks_ordering_json
        | _ =>
          raise(
            Failure(
              "A list of subtask titles (subtask ordering) must be provided for the action",
            ),
          )
        };
      List.map(
        (title: API.Json.t) =>
          switch (title) {
          | `String(title) => title
          | _ =>
            raise(Failure("Subtask titles in the ordering must be strings"))
          },
        subtasks_ordering_json,
      );
    };
    let get_task = (): CompositionAgentWorkbench.Model.task => {
      let task_json =
        switch (API.Json.dot("task", args)) {
        | Some(task_json) => task_json
        | _ => raise(Failure("A todo list must be provided for the action"))
        };
      let title = get_title(task_json);
      let description = get_description(task_json);
      let subtasks = get_subtasks(task_json);
      CompositionAgentWorkbench.Utils.TaskUtils.mk(
        ~title,
        ~description,
        ~subtasks,
      );
    };

    let action =
      try({
        let action =
          switch (tool_name) {
          | "expand" => Editor(View(Expand(get_paths())))
          | "collapse" => Editor(View(Collapse(get_paths())))
          | "initialize" => Editor(Edit(Initialize(get_code())))
          | "update_definition" =>
            Editor(Edit(UpdateDefinition(get_path(), get_code())))
          | "update_body" =>
            Editor(Edit(UpdateBody(get_path(), get_code())))
          | "update_pattern" =>
            Editor(Edit(UpdatePattern(get_path(), get_code())))
          | "update_binding_clause" =>
            Editor(Edit(UpdateBindingClause(get_path(), get_code())))
          | "insert_after" =>
            Editor(Edit(InsertAfter(get_path(), get_code())))
          | "insert_before" =>
            Editor(Edit(InsertBefore(get_path(), get_code())))
          | "delete_binding_clause" =>
            Editor(Edit(DeleteBindingClause(get_path())))
          | "delete_body" => Editor(Edit(DeleteBody(get_path())))
          | "create_new_task" => Assistant(CreateNewTask(get_task()))
          | "unset_active_task" => Assistant(UnsetActiveTask)
          | "set_active_task" => Assistant(SetActiveTask(get_title(args)))
          | "unset_active_subtask" => Assistant(UnsetActiveSubtask)
          | "set_active_subtask" =>
            Assistant(SetActiveSubtask(get_title(args)))
          | "mark_active_task_complete" =>
            Assistant(MarkActiveTaskComplete(get_summary(args)))
          | "mark_active_task_incomplete" =>
            Assistant(MarkActiveTaskIncomplete)
          | "mark_active_subtask_complete" =>
            Assistant(MarkActiveSubtaskComplete(get_summary(args)))
          | "mark_active_subtask_incomplete" =>
            Assistant(MarkActiveSubtaskIncomplete)
          | "add_new_subtask_to_active_task" =>
            Assistant(AddNewSubtaskToActiveTask(get_subtask()))
          | "reorder_subtasks_in_active_task" =>
            Assistant(ReorderSubtasksInActiveTask(get_subtasks_ordering()))
          | _ => raise(Failure("The tool called does not exist."))
          };
        Action(action);
      }) {
      | Failure(s: string) => Failure(s)
      };
    action;
  };

  let string_of = (action: action) => {
    switch (action) {
    | Editor(View(Expand(paths))) =>
      "expand(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | Editor(View(Collapse(paths))) =>
      "collapse(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | Editor(Read(ShowUseSites(path))) =>
      "show_use_sites(\"" ++ path ++ "\")"
    | Editor(Read(ShowReferences(path))) =>
      "show_references(\"" ++ path ++ "\")"
    | Editor(Edit(Initialize(code))) => "initialize(\"" ++ code ++ "\")"
    | Editor(Edit(UpdateDefinition(path, code))) =>
      "update_definition(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Editor(Edit(UpdateBody(path, code))) =>
      "update_body(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Editor(Edit(UpdatePattern(path, code))) =>
      "update_pattern(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Editor(Edit(UpdateBindingClause(path, code))) =>
      "update_binding_clause(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Editor(Edit(DeleteBindingClause(path))) =>
      "delete_binding_clause(\"" ++ path ++ "\")"
    | Editor(Edit(DeleteBody(path))) => "delete_body(\"" ++ path ++ "\")"
    | Editor(Edit(InsertAfter(path, code))) =>
      "insert_after(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Editor(Edit(InsertBefore(path, code))) =>
      "insert_before(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Assistant(CreateNewTask(task)) =>
      "create_new_task( "
      ++ CompositionAgentWorkbench.Utils.TaskUtils.task_to_json_string(task)
      ++ " )"
    | Assistant(UnsetActiveTask) => "unset_active_task"
    | Assistant(SetActiveTask(task_title)) =>
      "set_active_task(\"" ++ task_title ++ "\")"
    | Assistant(UnsetActiveSubtask) => "unset_active_subtask"
    | Assistant(SetActiveSubtask(subtask_title)) =>
      "set_active_subtask(\"" ++ subtask_title ++ "\")"
    | Assistant(MarkActiveTaskComplete(summary)) =>
      "mark_active_task_complete(\"" ++ summary ++ "\")"
    | Assistant(MarkActiveTaskIncomplete) => "mark_active_task_incomplete"
    | Assistant(MarkActiveSubtaskComplete(summary)) =>
      "mark_active_subtask_complete(\"" ++ summary ++ "\")"
    | Assistant(MarkActiveSubtaskIncomplete) => "mark_active_subtask_incomplete"
    | Assistant(AddNewSubtaskToActiveTask(subtask)) =>
      "add_new_subtask_to_active_task( "
      ++ CompositionAgentWorkbench.Utils.SubtaskUtils.subtask_to_json_string(
           subtask,
         )
      ++ ")"
    | Assistant(ReorderSubtasksInActiveTask(subtasks_ordering)) =>
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
