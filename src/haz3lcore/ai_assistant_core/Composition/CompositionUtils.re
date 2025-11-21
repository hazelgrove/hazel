open Util;
open CompositionActions;

type action = CompositionActions.composition_action;

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
    TodoListTools.new_todo_list,
    TodoListTools.delete_todo_list,
    TodoListTools.add_todo_items,
    TodoListTools.check_todo_items,
    TodoListTools.uncheck_todo_items,
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

  let action_of = (~tool_name: string, ~args: API.Json.t): action => {
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
    let get_titles_list = () => {
      switch (API.Json.dot("titles", args)) {
      | Some(`List(titles)) =>
        List.map(
          (title: API.Json.t) =>
            switch (title) {
            | `String(title) => title
            | _ => raise(Failure("Titles must be provided for the action"))
            },
          titles,
        )
      | _ =>
        raise(Failure("A list of titles must be provided for the action"))
      };
    };
    let get_title = (item: API.Json.t) => {
      switch (API.Json.dot("title", item)) {
      | Some(`String(title)) => title
      | _ => raise(Failure("A title must be provided for the todo item"))
      };
    };
    let get_description = (item: API.Json.t) => {
      switch (API.Json.dot("description", item)) {
      | Some(`String(description)) => description
      | _ =>
        raise(Failure("A description must be provided for the todo item"))
      };
    };
    let todo_item_of_json =
        (todo_item_json: API.Json.t): AssistantModel.todo_item => {
      title: get_title(todo_item_json),
      description: get_description(todo_item_json),
      completed: false,
    };
    let get_todo_items = (): list(AssistantModel.todo_item) => {
      switch (API.Json.dot("todo_items", args)) {
      | Some(`List(todo_list_json)) =>
        List.map(todo_item_of_json, todo_list_json)
      | _ => raise(Failure("A todo list must be provided for the action"))
      };
    };

    switch (tool_name) {
    | "expand" => Editor(View(Expand(get_paths())))
    | "collapse" => Editor(View(Collapse(get_paths())))
    | "initialize" => Editor(Edit(Initialize(get_code())))
    | "update_definition" =>
      Editor(Edit(UpdateDefinition(get_path(), get_code())))
    | "update_body" => Editor(Edit(UpdateBody(get_path(), get_code())))
    | "update_pattern" =>
      Editor(Edit(UpdatePattern(get_path(), get_code())))
    | "update_binding_clause" =>
      Editor(Edit(UpdateBindingClause(get_path(), get_code())))
    | "insert_after" => Editor(Edit(InsertAfter(get_path(), get_code())))
    | "insert_before" => Editor(Edit(InsertBefore(get_path(), get_code())))
    | "delete_binding_clause" =>
      Editor(Edit(DeleteBindingClause(get_path())))
    | "delete_body" => Editor(Edit(DeleteBody(get_path())))
    | "new_todo_list" =>
      Assistant(TodoAction(NewTodoList(get_todo_items())))
    | "delete_todo_list" => Assistant(TodoAction(DeleteTodoList))
    | "add_todo_items" =>
      Assistant(TodoAction(AddTodoItems(get_todo_items())))
    | "check_todo_items" =>
      Assistant(TodoAction(CheckTodoItems(get_titles_list())))
    | "uncheck_todo_items" =>
      Assistant(TodoAction(UncheckTodoItems(get_titles_list())))
    | _ => raise(Failure("The tool called does not exist."))
    };
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
    | Assistant(TodoAction(NewTodoList(todo_list))) =>
      "new_todo_list(\""
      ++ String.concat(
           ", ",
           List.map(AssistantModel.todo_item_to_string, todo_list),
         )
      ++ "\")"
    | Assistant(TodoAction(DeleteTodoList)) => "delete_todo_list()"
    | Assistant(TodoAction(AddTodoItems(todo_items))) =>
      "add_todo_items("
      ++ String.concat(
           ", ",
           List.map(AssistantModel.todo_item_to_string, todo_items),
         )
      ++ ")"
    | Assistant(TodoAction(CheckTodoItems(titles))) =>
      "check_todo_items(\"" ++ String.concat(", ", titles) ++ "\")"
    | Assistant(TodoAction(UncheckTodoItems(titles))) =>
      "uncheck_todo_items(\"" ++ String.concat(", ", titles) ++ "\")"
    };
  };
};

module Public = {
  let tools = Local.tools;
  let action_of = (~tool_name: string, ~args: API.Json.t): action => {
    Local.action_of(~tool_name, ~args);
  };
  let string_of = (action: action) => {
    Local.string_of(action);
  };
};
