open Sexplib.Std;

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
  | ConstructEdit(Action.shape) /* SLine in Action.shape stands for both empty line and case rule,   so an extra type CaseRule is added for construction */
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

let get_cursor_pos = (cursor_term: cursor_term): CursorPosition.t => {
  switch (cursor_term) {
  | ExpOperand(cursor_pos, _)
  | PatOperand(cursor_pos, _)
  | TypOperand(cursor_pos, _)
  | ExpOperator(cursor_pos, _)
  | PatOperator(cursor_pos, _)
  | TypOperator(cursor_pos, _)
  | Line(cursor_pos, _)
  | Rule(cursor_pos, _) => cursor_pos
  };
};

let is_var_insert = (action_group): bool => {
  switch (action_group) {
  | VarGroup(var_group) =>
    switch (var_group) {
    | Insert(_) => true
    | Edit(_) => false
    }
  | _ => false
  };
};

let is_var_group = (action_group): bool => {
  switch (action_group) {
  | VarGroup(var_group) =>
    switch (var_group) {
    | Insert(_) => false
    | Edit(_) => true
    }
  | _ => false /* return true if new action_group can be grouped with the previous action_group */
  };
};

let group_action_group =
    (action_group_prev: action_group, action_group_next: action_group): bool =>
  switch (action_group_prev, action_group_next) {
  | (CaseRule, CaseRule) => true
  | (CaseRule, _) => false
  | (VarGroup(_), VarGroup(_)) => true
  | (VarGroup(_), DeleteEdit(delete_group)) =>
    switch (delete_group) {
    | Term(_, _) => true
    | Space
    | EmptyLine
    | TypeAnn => false
    }
  | (VarGroup(_), ConstructEdit(construct_edit)) =>
    switch (construct_edit) {
    | SLet
    | SCase
    | SFun => true
    | _ => false
    }
  | (VarGroup(_), _) => false /* "insert" and "insert and delete" should be grouped together */
  | (DeleteEdit(Term(_, true)), VarGroup(Insert(_))) => true
  | (DeleteEdit(_), _)
  | (ConstructEdit(_), _)
  | (SwapEdit(_), _)
  | (Import, _)
  | (Init, _) => false
  };

type comp_len_typ =
  | MaxLen
  | MinLen
  | Len(int);

let comp_len_lt =
    (cursor_len_prev: comp_len_typ, cursor_len_next: comp_len_typ): bool => {
  switch (cursor_len_prev, cursor_len_next) {
  | (MaxLen, MaxLen) => false
  | (_, MaxLen) => true
  | (MinLen, _) => true
  | (_, MinLen) => false
  | (MaxLen, _) => false
  | (Len(len1), Len(len2)) => len1 <= len2
  };
};

let cursor_term_len = (cursor_term: cursor_term): comp_len_typ => {
  switch (cursor_term) {
  | ExpOperand(_, operand) =>
    switch (operand) {
    | EmptyHole(_) => MinLen
    | InvalidText(_, t) => Len(String.length(t))
    | Var(_, _, var) => Len(Var.length(var))
    | IntLit(_, num)
    | FloatLit(_, num) => Len(String.length(num))
    | BoolLit(_, _)
    | ListNil(_)
    | Fun(_)
    | Inj(_, _, _)
    | Case(_, _, _)
    | Parenthesized(_) => MaxLen
    }
  | PatOperand(_, operand) =>
    switch (operand) {
    | EmptyHole(_) => MinLen
    | Wild(_) => Len(1)
    | InvalidText(_, t) => Len(String.length(t))
    | Var(_, _, var) => Len(Var.length(var))
    | IntLit(_, num)
    | FloatLit(_, num) => Len(String.length(num))
    | BoolLit(_, _)
    | ListNil(_)
    | Parenthesized(_)
    | TypeAnn(_)
    | Inj(_, _, _) => MaxLen
    }
  | TypOperand(_, operand) =>
    switch (operand) {
    | Hole => MinLen
    | Unit
    | Int
    | Float
    | Bool
    | Parenthesized(_)
    | List(_) => MaxLen
    }
  | ExpOperator(_, _)
  | PatOperator(_, _)
  | TypOperator(_, _)
  | Rule(_, _) => MaxLen
  | Line(_, line) =>
    switch (line) {
    | EmptyLine => MinLen
    | CommentLine(comment) => Len(String.length(comment))
    | LetLine(_)
    | ExpLine(_) => MaxLen
    }
  };
};

let cursor_term_len_larger =
    (cursor_term_prev: cursor_term, cursor_term_next: cursor_term)
    : cursor_term =>
  if (comp_len_lt(
        cursor_term_len(cursor_term_prev),
        cursor_term_len(cursor_term_next),
      )) {
    cursor_term_next;
  } else {
    cursor_term_prev;
  };

let has_typ_ann = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOperand(_, exp) =>
    switch (exp) {
    | Fun(_) => true
    | _ => false
    }
  | Line(_, line_content) =>
    switch (line_content) {
    | LetLine(_) => true
    | _ => false
    }
  | _ => false
  };
};

let is_move_action = (cursor_term_info: cursor_term_info): bool => {
  ZExp.erase(cursor_term_info.zexp_before)
  == ZExp.erase(cursor_term_info.zexp_after);
};
