open CursorInfo;

type zoperand =
  | ZExp(ZExp.zoperand)
  | ZTyp(ZTyp.zoperand)
  | ZPat(ZPat.zoperand);

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
  | TypOperand(_, _)
  | ExpOperator(_, _)
  | PatOperator(_, _)
  | TypOperator(_, _) => false
  | Line(_, line) =>
    switch (line) {
    | EmptyLine
    | CommentLine(_) => true
    | LetLine(_)
    | ExpLine(_) => false
    }
  | Rule(_, _) => false
  };
};

let is_empty_hole = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOperand(_, EmptyHole(_)) => true
  | ExpOperand(_, _) => false
  | PatOperand(_, EmptyHole(_)) => true
  | PatOperand(_, _) => false
  | TypOperand(_, Hole) => true
  | TypOperand(_, _) => false
  | ExpOperator(_, _)
  | PatOperator(_, _)
  | TypOperator(_, _)
  | Line(_, _)
  | Rule(_, _) => false
  };
};

let is_empty_line = (cursor_term): bool => {
  switch (cursor_term) {
  | Line(_, EmptyLine) => true
  | Line(_, _) => false
  | ExpOperand(_, _)
  | PatOperand(_, _)
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
  | ExpOperand(_, _) => false
  | PatOperand(_, EmptyHole(_)) => false
  | PatOperand(_, _) => false
  | TypOperand(_, Hole) => false
  | TypOperand(_, _) => false
  | ExpOperator(_, _)
  | PatOperator(_, _)
  | TypOperator(_, _)
  | Line(_, _)
  | Rule(_, _) => false;

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

let mk = (~uses=?, ~parent_info=NoParentInfo, typed, ctx, cursor_term) => {
  typed,
  ctx,
  uses,
  cursor_term,
  parent_info,
};

let get_ctx = ci => ci.ctx;

let set_end_branch_clause = (ci, is_end_branch_clause) =>
  if (is_end_branch_clause) {
    {...ci, parent_info: EndBranchClause};
  } else {
    ci;
  };

let set_is_empty_hole_line = (ci, is_empty_hole_line) =>
  if (is_empty_hole_line) {
    {...ci, parent_info: EmptyHoleLine};
  } else {
    ci;
  };

/*
 * there are cases we can't determine where to find the uses of a variable
 * immediately after we see its binding site.
 * in this case, we will return a deferrable('t) and go up the tree
 * until we could find uses and feed it to (uses_list => 't).
 */

type deferrable('t) =
  | CursorNotOnDeferredVarPat('t)
  | CursorOnDeferredVarPat(UsageAnalysis.uses_list => 't, Var.t);
