open OptUtil.Syntax;
open CursorInfo;

type zoperand =
  | ZExp(ZExp.zoperand)
  | ZTyp(ZTyp.zoperand)
  | ZPat(ZPat.zoperand);

let cursor_term_is_editable = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | Exp(_, exp) =>
    switch (exp) {
    | EmptyHole(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _) => true
    | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
    | _ => false
    }
  | Pat(_, pat) =>
    switch (pat) {
    | EmptyHole(_)
    | Wild(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _) => true
    | _ => false
    }
  | Typ(_, _)
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _) => false
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

let string_and_index_of_cursor_term =
    (term: CursorInfo.cursor_term): (string, int) => {
  switch (term) {
  | Pat(OnText(i), Var(_, _, s))
  | Pat(OnText(i), InvalidText(_, s))
  | Pat(OnText(i), IntLit(_, s))
  | Pat(OnText(i), FloatLit(_, s))
  | Exp(OnText(i), Var(_, _, s))
  | Exp(OnText(i), InvalidText(_, s))
  | Exp(OnText(i), IntLit(_, s))
  | Exp(OnText(i), FloatLit(_, s)) => (s, i)
  | Exp(OnText(i), BoolLit(_, b))
  | Pat(OnText(i), BoolLit(_, b)) => (string_of_bool(b), i)
  | _ => ("", 0)
  };
};

let index_of_cursor_term = (term: CursorInfo.cursor_term): int => {
  switch (term) {
  | Exp(OnText(i), _) => i
  | Pat(OnText(i), _) => i
  | _ =>
    print_endline("TODO(andrew): index_of_cursor_term fallthough");
    0;
  };
};

let is_empty_hole = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | Exp(_, EmptyHole(_)) => true
  | Exp(_, _) => false
  | Pat(_, EmptyHole(_)) => true
  | Pat(_, _) => false
  | Typ(_, Hole) => true
  | Typ(_, _) => false
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _)
  | Line(_, _)
  | Rule(_, _) => false
  };
};

let is_op = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _) => true
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

let extract_cursor_term = (zexp: ZExp.t): cursor_term => {
  switch (CursorFrame.mk(zexp)) {
  | [{slice: Line(CursorL(c, s)), _}, ..._] => Line(c, s)
  | [{slice: ExpOperand(CursorE(c, s)), _}, ..._] => Exp(c, s)
  | [{slice: ExpOperator((c, s)), _}, ..._] => ExpOp(c, s)
  | [{slice: Rule(CursorR(c, s)), _}, ..._] => Rule(c, s)
  | [{slice: PatOperand(CursorP(c, s)), _}, ..._] => Pat(c, s)
  | [{slice: PatOperator((c, s)), _}, ..._] => PatOp(c, s)
  | [{slice: TypOperand(CursorT(c, s)), _}, ..._] => Typ(c, s)
  | [{slice: TypOperator((c, s)), _}, ..._] => TypOp(c, s)
  | frame =>
    print_endline(Sexplib.Sexp.to_string_hum(CursorFrame.sexp_of_t(frame)));
    failwith("INVALID FRAME (extract_cursor_term)");
  };
};

let mk =
    (
      ~uses=?,
      ~syntactic_context=CursorInfo.NoSeq,
      ~opParent=None,
      typed,
      ctx,
      cursor_term,
    )
    : CursorInfo.t => {
  let (expected_ty, actual_ty, _mode) = get_types_and_mode(typed);
  let expected_ty =
    switch (expected_ty) {
    | None => HTyp.Hole
    | Some(ty) => ty
    };
  {
    typed,
    ctx,
    uses,
    cursor_term,
    expected_ty,
    actual_ty,
    syntactic_context,
    opParent,
  };
};
let get_ctx = (ci: CursorInfo.t) => ci.ctx;

/*
 * there are cases we can't determine where to find the uses of a variable
 * immediately after we see its binding site.
 * in this case, we will return a deferrable('t) and go up the tree
 * until we could find uses and feed it to (uses_list => 't).
 */

type deferrable('t) =
  | CursorNotOnDeferredVarPat('t)
  | CursorOnDeferredVarPat(UsageAnalysis.uses_list => 't, Var.t);
