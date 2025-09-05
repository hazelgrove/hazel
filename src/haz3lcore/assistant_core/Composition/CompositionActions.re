open Util;

/*
 * ------------------------------
 *  Structure-Based Action Language
 * ------------------------------
 */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type via =
  | NameAndIdx(string, option(int))
  | Stepwise(Direction.t);

// --- Navigation Actions ---
// These actions are used to navigate the AST, and do not modify the program
// or provide additional information to the LLM. They strictly move the cursor
// through the AST.
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type nav_action =
  // This is mainly for user's to select the current node
  | SelectCurrent
  // Goes to the parent node of the current node in the AST
  | GoToParent
  // Goes to the child node of the current node in the AST
  | GoToChild(string, option(int))
  // Jumps to the root node of the AST
  | GoToSibling(via)
  // Goes to the binding site of the indicated variable
  | GoToBindingSite(string, option(int))
  | GoToUseSite(string, option(int));

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
  | ShowUseSites
  // Displays the entire definition of the current node, with no child/sub definitions abstracted away
  | ViewEntireDefintion;

// --- Edit Actions ---
// These actions are used to modify the program. They do provide additional
// information to the LLM (via reading), but may move the cursor (eg. removing
// a node will require the cursor to be moved elsewhere).
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type user =
  | LLM(string)
  | Human; // prompt user for string

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type edit_action =
  | UpdateAll(user)
  | UpdateDefinition(user)
  | UpdateBody(user)
  | UpdatePattern(user)
  | UpdateBindingClause(user)
  | DeleteBindingClause
  | DeleteBody
  | InsertAfter(user)
  | InsertBefore(user);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type view_action =
  | ShowReferences;

// AddToolLabel_1.0: Make the action types (above) and add their cases to the funs (below)
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type composition_action =
  | Nav(nav_action)
  | Read(read_action)
  | Edit(edit_action);
