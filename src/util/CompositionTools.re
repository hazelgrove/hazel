open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

// instructions: to add a tool, you will need to update all AddToolLabel
// locations
// AddToolLabel_0

let tools = [
  NavTools.go_to_parent,
  NavTools.go_to_child,
  NavTools.go_to_sibling,
  EditTools.update_definition,
  EditTools.update_body,
  EditTools.update_pattern,
  EditTools.update_expression,
  EditTools.delete,
  EditTools.insert_after,
  EditTools.insert_before,
  //ViewTools.view_definition,
];

/*
 * ------------------------------
 *  Structure-Based Action Language
 * ------------------------------
 */

// --- Navigation Actions ---
// These actions are used to navigate the AST, and do not modify the program
// or provide additional information to the LLM. They strictly move the cursor
// through the AST.
[@deriving (show({with_path: false}), sexp, yojson)]
type nav_action =
  // Goes to the parent node of the current node in the AST
  | GoToParent
  // Goes to the child node of the current node in the AST
  | GoToChild(string, option(int))
  // Jumps to the root node of the AST
  | GoToSibling(string, option(int));

// --- File-Read Actions ---
// These actions are used purely to read information from the program,
// and do not modify the program or the cursor location in the AST.

[@deriving (show({with_path: false}), sexp, yojson)]
type read_action =
  // Displays the definition of the current node in the AST
  | ViewDefinition;

// --- Edit Actions ---
// These actions are used to modify the program. They do provide additional
// information to the LLM (via reading), but may move the cursor (eg. removing
// a node will require the cursor to be moved elsewhere).
[@deriving (show({with_path: false}), sexp, yojson)]
type edit_action =
  | UpdateDefinition(string)
  | UpdateBody(string)
  | UpdatePattern(string)
  | UpdateExpression(string)
  | Delete
  | InsertAfter(string)
  | InsertBefore(string);

// AddToolLabel_2.1: Make the action types (above)
[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | Nav(nav_action)
  | Read(read_action)
  | Edit(edit_action);

let action_of = (~tool_name: string, ~args: Maps.StringMap.t(string)): action => {
  /* Possible arguments */
  let name = Maps.StringMap.find_opt("name", args);
  let index =
    Option.map(int_of_string, Maps.StringMap.find_opt("index", args));
  let code = Maps.StringMap.find_opt("code", args);

  switch (tool_name) {
  | "go_to_parent" => Nav(GoToParent)
  | "go_to_child" =>
    let name =
      switch (name) {
      | Some(name) => name
      | None =>
        raise(
          Failure(
            "You must specify a name for the child you wish to navigate to",
          ),
        )
      };
    Nav(GoToChild(name, index));
  | "go_to_sibling" =>
    let name =
      switch (name) {
      | Some(name) => name
      | None =>
        raise(
          Failure(
            "You must specify a name for the sibling you wish to navigate to",
          ),
        )
      };
    Nav(GoToSibling(name, index));
  | "view_definition" => Read(ViewDefinition)
  | "update_definition" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the definition you wish to update",
          ),
        )
      };
    Edit(UpdateDefinition(code));
  | "update_body" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure("You must specify a code for the body you wish to update"),
        )
      };
    Edit(UpdateBody(code));
  | "update_pattern" => Edit(UpdatePattern("code"))
  | "update_expression" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to update",
          ),
        )
      };
    Edit(UpdateExpression(code));
  | "insert_after" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to insert after",
          ),
        )
      };
    Edit(InsertAfter(code));
  | "insert_before" =>
    let code =
      switch (code) {
      | Some(code) => code
      | None =>
        raise(
          Failure(
            "You must specify a code for the expression you wish to insert before",
          ),
        )
      };
    Edit(InsertBefore(code));
  | "delete" => Edit(Delete)
  | _ => Nav(GoToParent) // default fallback
  };
};

let string_of_action = (action: action) => {
  switch (action) {
  | Nav(GoToParent) => "go_to_parent"
  | Nav(GoToChild(_, _)) => "go_to_child"
  | Nav(GoToSibling(_, _)) => "go_to_sibling"
  | Read(ViewDefinition) => "view_definition"
  | Edit(UpdateDefinition(_)) => "update_definition"
  | Edit(UpdateBody(_)) => "update_body"
  | Edit(UpdatePattern(_)) => "update_pattern"
  | Edit(UpdateExpression(_)) => "update_expression"
  | Edit(Delete) => "delete"
  | Edit(InsertAfter(_)) => "insert_after"
  | Edit(InsertBefore(_)) => "insert_before"
  };
};
