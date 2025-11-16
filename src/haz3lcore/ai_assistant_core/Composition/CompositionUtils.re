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

    switch (tool_name) {
    | "expand" => View(Expand(get_paths()))
    | "collapse" => View(Collapse(get_paths()))
    | "initialize" => Edit(Initialize(get_code()))
    | "update_definition" => Edit(UpdateDefinition(get_path(), get_code()))
    | "update_body" => Edit(UpdateBody(get_path(), get_code()))
    | "update_pattern" => Edit(UpdatePattern(get_path(), get_code()))
    | "update_binding_clause" =>
      Edit(UpdateBindingClause(get_path(), get_code()))
    | "insert_after" => Edit(InsertAfter(get_path(), get_code()))
    | "insert_before" => Edit(InsertBefore(get_path(), get_code()))
    | "delete_binding_clause" => Edit(DeleteBindingClause(get_path()))
    | "delete_body" => Edit(DeleteBody(get_path()))
    | _ => raise(Failure("The tool called does not exist."))
    };
  };

  let string_of = (action: action) => {
    switch (action) {
    | View(Expand(paths)) =>
      "expand(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | View(Collapse(paths)) =>
      "collapse(\"[" ++ String.concat(", ", paths) ++ "]\")"
    | Read(ShowUseSites(path)) => "show_use_sites(\"" ++ path ++ "\")"
    | Read(ShowReferences(path)) => "show_references(\"" ++ path ++ "\")"
    | Edit(Initialize(code)) => "initialize(\"" ++ code ++ "\")"
    | Edit(UpdateDefinition(path, code)) =>
      "update_definition(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Edit(UpdateBody(path, code)) =>
      "update_body(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Edit(UpdatePattern(path, code)) =>
      "update_pattern(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Edit(UpdateBindingClause(path, code)) =>
      "update_binding_clause(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Edit(DeleteBindingClause(path)) =>
      "delete_binding_clause(\"" ++ path ++ "\")"
    | Edit(DeleteBody(path)) => "delete_body(\"" ++ path ++ "\")"
    | Edit(InsertAfter(path, code)) =>
      "insert_after(\"" ++ path ++ "\", \"" ++ code ++ "\")"
    | Edit(InsertBefore(path, code)) =>
      "insert_before(\"" ++ path ++ "\", \"" ++ code ++ "\")"
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
