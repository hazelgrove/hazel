open OptUtil.Syntax;
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

let cursor_position_of_cursor_term: CursorInfo.cursor_term => CursorPosition.t =
  fun
  | ExpOperand(pos, _)
  | PatOperand(pos, _)
  | TypOperand(pos, _)
  | ExpOperator(pos, _)
  | PatOperator(pos, _)
  | TypOperator(pos, _)
  | Line(pos, _)
  | Rule(pos, _) => pos;

let index_of_cursor_term = (term: CursorInfo.cursor_term): int =>
  switch (cursor_position_of_cursor_term(term)) {
  | OnText(i) => i
  | _ => 0
  };

let string_of_cursor_term = (term: CursorInfo.cursor_term): string => {
  switch (term) {
  | PatOperand(_, Var(_, _, s))
  | PatOperand(_, InvalidText(_, s))
  | PatOperand(_, IntLit(_, s))
  | PatOperand(_, FloatLit(_, s))
  | ExpOperand(_, Var(_, _, s))
  | ExpOperand(_, InvalidText(_, s))
  | ExpOperand(_, IntLit(_, s))
  | ExpOperand(_, FloatLit(_, s)) => s
  | ExpOperand(_, BoolLit(_, b))
  | PatOperand(_, BoolLit(_, b)) => string_of_bool(b)
  | _ => ""
  };
};

let is_empty_hole = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOperand(_, EmptyHole(_))
  | TypOperand(_, Hole)
  | PatOperand(_, EmptyHole(_)) => true
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

let is_op = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOperator(_, _)
  | PatOperator(_, _)
  | TypOperator(_, _) => true
  | _ => false
  };
};

let is_empty_line = (cursor_term): bool => {
  switch (cursor_term) {
  | Line(_, EmptyLine) => true
  | _ => false
  };
};

let is_comment_line = (cursor_term): bool => {
  switch (cursor_term) {
  | Line(_, CommentLine(_)) => true
  | _ => false
  };
};

[@deriving sexp]
type mode =
  | Analytic
  | Synthetic
  | UnknownMode;

let rec get_types_and_mode = (typed: typed) => {
  switch (typed) {
  | AnaAnnotatedLambda(expected, actual)
  | AnaTypeInconsistent(expected, actual)
  | AnaSubsumed(expected, actual) => (
      Some(expected),
      Some(actual),
      Analytic,
    )

  | AnaWrongLength(_, _, expected)
  | AnaFree(expected)
  | AnaInvalid(expected)
  | AnaKeyword(expected, _)
  | Analyzed(expected) => (Some(expected), None, Analytic)

  | SynErrorArrow(_expected, actual)
  | SynMatchingArrow(_expected, actual) => (
      Some(Arrow(Hole, Hole)),
      Some(actual),
      Synthetic,
    )

  | SynFreeArrow(actual)
  | SynKeywordArrow(actual, _)
  | SynInvalidArrow(actual)
  | Synthesized(actual) => (Some(Hole), Some(actual), Synthetic)

  | SynInvalid
  | SynFree
  | SynKeyword(_) => (Some(Hole), None, Synthetic)

  | SynBranchClause(join, typed, _) =>
    switch (join, typed) {
    | (JoinTy(ty), Synthesized(got_ty)) =>
      switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
      | (true, _) => (Some(Hole), None, Synthetic)
      | _ => (None, None, Synthetic)
      }
    | (NoBranches, _) => get_types_and_mode(typed)
    | _ => (None, None, Synthetic)
    }

  | _ => (None, None, UnknownMode)
  };
};
/**
   * Gets the type of the expression at the cursor.
   * Return HTyp.t
   */
let get_type = (cursor_info: t): option(HTyp.t) => {
  let (expected_ty, _, _) = get_types_and_mode(cursor_info.typed);
  let+ expected_ty = expected_ty;
  expected_ty;
};

let on_empty_expr_hole: CursorInfo.cursor_term => bool =
  fun
  | ExpOperand(_, EmptyHole(_)) => true
  | ExpOperand(_)
  | PatOperand(_)
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
      ~uses=?,
      ~parent_info=NoParentInfo,
      ~enclosing_zopseq=CursorInfo.NoSeq,
      ~enclosing_zoperand=None,
      typed,
      ctx,
      cursor_term,
    )
    : CursorInfo.t => {
  let (expected_ty, actual_ty, _mode) = get_types_and_mode(typed);
  let expected_ty = HTyp.relax(expected_ty);
  {
    typed,
    ctx,
    uses,
    cursor_term,
    expected_ty,
    actual_ty,
    enclosing_zopseq,
    enclosing_zoperand,
    parent_info,
  };
};
let get_ctx = (ci: CursorInfo.t) => ci.ctx;

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
  | CursorOnDeferredVarPat(UsageAnalysis.uses_list => 't, Var.t);
