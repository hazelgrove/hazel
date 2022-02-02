open CursorInfo;

type zoperand =
  | ZExp(ZExp.zoperand)
  | ZTyp(ZTyp.zoperand)
  | ZPat(ZPat.zoperand)
  | ZTPat(ZTPat.t);

let cursor_term_is_editable = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOperand(_, exp) =>
    switch (exp) {
    | EmptyHole(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _) => true
    | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
    | _ => false
    }
  | PatOperand(_, pat) =>
    switch (pat) {
    | EmptyHole(_)
    | Wild(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _) => true
    | _ => false
    }
  | TPat(_, pat) =>
    switch (pat) {
    | EmptyHole
    | TyVar(_) => true
    }
  | TypOperand(_, typ) =>
    switch (typ) {
    | Hole(_)
    | Int
    | Float
    | Bool
    | TyVar(_) => true
    | _ => false
    }
  | ExpOperator(_, _)
  | PatOperator(_, _)
  | TypOperator(_, _) => false
  | Line(_, line) =>
    switch (line) {
    | EmptyLine
    | CommentLine(_) => true
    | LetLine(_)
    | ExpLine(_)
    | TyAliasLine(_) => false
    }
  | Rule(_, _) => false
  };
};

let is_empty_hole = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOperand(_, EmptyHole(_))
  | TypOperand(_, Hole(_))
  | TPat(_, EmptyHole) => true
  | PatOperand(_, EmptyHole(_)) => true
  | TPat(_, _)
  | ExpOperand(_)
  | PatOperand(_)
  | TypOperand(_)
  | ExpOperator(_)
  | PatOperator(_)
  | TypOperator(_)
  | Line(_)
  | Rule(_) => false
  };
};

let is_empty_line = (cursor_term): bool => {
  switch (cursor_term) {
  | Line(_, EmptyLine) => true
  | Line(_, _) => false
  | ExpOperand(_, _)
  | PatOperand(_, _)
  | TPat(_, _)
  | TypOperand(_, _)
  | ExpOperator(_, _)
  | PatOperator(_, _)
  | TypOperator(_, _)
  | Rule(_, _) => false
  };
};

let on_empty_expr_hole: CursorInfo.cursor_term => bool =
  fun
  | ExpOperand(_, EmptyHole(_)) => true
  | ExpOperand(_)
  | PatOperand(_)
  | TPat(_)
  | TypOperand(_)
  | ExpOperator(_)
  | PatOperator(_)
  | TypOperator(_)
  | Line(_)
  | Rule(_) => false;

let on_expr_var: CursorInfo.cursor_term => bool =
  fun
  | ExpOperand(_, Var(_)) => true
  | ExpOperand(_, _) => false
  | _ => false;

let is_end_keyword =
    (term: CursorInfo.cursor_term, keyword: ExpandingKeyword.t) =>
  switch (term) {
  | ExpOperand(OnText(index), _) =>
    index == String.length(ExpandingKeyword.to_string(keyword))
  | _ => false
  };

let mk =
    (
      ~tyuses=?,
      ~uses=?,
      ~parent_info=NoParentInfo,
      typed,
      ctx,
      u_gen,
      cursor_term,
    ) => {
  typed,
  ctx,
  u_gen,
  uses,
  tyuses,
  cursor_term,
  parent_info,
};

let get_ctx = ci => ci.ctx;

let set_after_branch_clause = (ci, is_after_branch_clause) => {
  ...ci,
  parent_info: is_after_branch_clause ? AfterBranchClause : ci.parent_info,
};

let set_is_before_empty_hole_line = (ci, is_before_empty_hole_line) => {
  ...ci,
  parent_info:
    is_before_empty_hole_line ? BeforeEmptyHoleLine : ci.parent_info,
};

/*
 * there are cases we can't determine where to find the uses of a variable
 * immediately after we see its binding site.
 * in this case, we will return a deferrable('t) and go up the tree
 * until we could find uses and feed it to (uses_list => 't).
 */

type deferrable('t) =
  | CursorNotOnDeferredVarPat('t)
  | CursorOnDeferredVarPat(UsageAnalysis.uses_list => 't, Var.t)
  | CursorOnDeferredTyVarPat(UsageAnalysis.uses_list => 't, TyVar.Name.t);
