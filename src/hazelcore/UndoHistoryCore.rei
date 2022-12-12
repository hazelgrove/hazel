[@deriving sexp]
type cursor_term = CursorInfo.cursor_term;

[@deriving sexp]
type start_from_insertion = bool;
[@deriving sexp]
type delete_group =
  | Term(cursor_term, start_from_insertion) /* cursor_term is insufficient for space, empty line and type annotation deletion,   so we add the following three constructors */
  | Space
  | EmptyLine
  | TypeAnn;

[@deriving sexp]
type var_group =
  | Insert(cursor_term)
  | Edit({
      start_from: cursor_term,
      end_with: cursor_term,
    });

[@deriving sexp]
type swap_group =
  | Up
  | Down
  | Left
  | Right;

[@deriving sexp]
type action_group =
  | VarGroup(var_group)
  | DeleteEdit(delete_group)
  | ConstructEdit(Action.shape) /* SLine in Action_common.shape stands for both empty line and case rule,   so an extra type CaseRule is added for construction */
  | CaseRule
  | SwapEdit(swap_group)
  | Import
  | Init;

[@deriving sexp]
type cursor_term_info = {
  cursor_term_before: cursor_term,
  cursor_term_after: cursor_term,
  zexp_before: ZExp.t,
  zexp_after: ZExp.t,
  prev_is_empty_line: bool,
  next_is_empty_line: bool,
};

[@deriving sexp]
type timestamp = float;

let get_cursor_pos: cursor_term => CursorPosition.t;

let is_var_insert: action_group => bool;

let is_var_group: action_group => bool /* return true if new action_group can be grouped with the previous action_group */;

let group_action_group: (action_group, action_group) => bool;

let cursor_term_len_larger: (cursor_term, cursor_term) => cursor_term;

let has_typ_ann: cursor_term => bool;

let is_move_action: cursor_term_info => bool;
