open Util;

/*
 * ------------------------------
 *  Structure-Based Action Language
 * ------------------------------
 */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
// The path string should be formatted as "name/name/name/..."
type path = string;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type code = string;

// --- File-Read Actions ---
// These actions are used purely to read information from the program,
// and do not modify the program or the cursor location in the AST.
//    Note: We supply several "things to read" to the agent by default
//          on each iteration:
//              - AST Info
//              - structure-based code map
//              - variables referenced
//              - static error info
//         We could, technically, move any of these to here, but need to consider implications
//         of doing so

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type read_action =
  // Lists all the use sites of the indicated variable
  | ShowUseSites(path)
  // Displays the typing context/scope at the current let expression in the AST
  // | ShowContext //todo: technically this is accomplished via showing sibs/parent
  // Displays the entire definition of the current node, with no child/sub definitions abstracted away
  | ShowReferences(path);

// --- Edit Actions ---

[@deriving (show({with_path: false}), sexp, yojson)]
type edit_action =
  | Initialize(code)
  | UpdateDefinition(path, code)
  | UpdateBody(path, code)
  | UpdatePattern(path, code)
  | UpdateBindingClause(path, code)
  | DeleteBindingClause(path)
  | DeleteBody(path)
  | InsertAfter(path, code)
  | InsertBefore(path, code);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type view_action =
  // Leave out option(int) index param for now.
  // TODO: add later. resort to prompting to avoid shadowing ambiguity for now.
  | Expand(list(path))
  | Collapse(list(path));
// TODO: Uncomment once we add functionality for file systems.
// This would require a separate file_path and variable_path in Expand and Collapse actions.
/*
 | Open(path)
 | Close(path);
 */

// AddToolLabel_1.0: Make the action types (above) and add their cases to the funs (below)
[@deriving (show({with_path: false}), sexp, yojson)]
type editor_action =
  | View(view_action) // Main source of ingesting the codebase
  | Read(read_action) // Language server helpers
  | Edit(edit_action); // Main source of editing the codebase

[@deriving (show({with_path: false}), sexp, yojson)]
type editor_payload = (editor_action, AssistantUpdateAction.status => unit);

[@deriving (show({with_path: false}), sexp, yojson)]
type composition_action =
  | Editor(editor_action)
  | Assistant(CompositionAgentWorkbench.Update.Action.BackendAction.action);

[@deriving (show({with_path: false}), sexp, yojson)]
type composition_payload = (
  composition_action,
  AssistantUpdateAction.status => unit,
);
