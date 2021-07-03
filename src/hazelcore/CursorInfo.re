open Sexplib.Std;
open OptUtil.Syntax;

[@deriving sexp]
type join_of_branches =
  | NoBranches
  // steps to the case
  | InconsistentBranchTys(list(HTyp.t), CursorPath.steps)
  | JoinTy(HTyp.t);

[@deriving sexp]
type mode =
  | Analytic
  | Synthetic
  | UnknownMode;

[@deriving sexp]
type typed =
  // cursor is on a lambda with an argument type annotation
  /* cursor in analytic position */
  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  // cursor is on a type inconsistent expression
  | AnaTypeInconsistent(HTyp.t, HTyp.t)
  // cursor is on a tuple of the wrong length
  | AnaWrongLength
      // expected length
      (
        int,
        // got length
        int,
        // expected type
        HTyp.t,
      )
  // cursor is on a free variable
  | AnaFree(HTyp.t)
  // cursor is on invalid text
  | AnaInvalid(HTyp.t)
  // cursor is on a keyword
  | AnaKeyword(HTyp.t, ExpandingKeyword.t)
  // none of the above and didn't go through subsumption
  | Analyzed(HTyp.t)
  // none of the above and went through subsumption
  | AnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic position */
  // cursor is on the function position of an ap,
  // and that expression does not synthesize a type
  // with a matched arrow type
  | SynErrorArrow
      // expected
      (
        HTyp.t,
        // got
        HTyp.t,
      )
  // cursor is on the function position of an ap,
  // and that expression does synthesize a type
  // with a matched arrow type
  | SynMatchingArrow(HTyp.t, HTyp.t)
  // cursor is on a free variable in the function
  // position of an ap
  | SynFreeArrow(HTyp.t)
  // cursor is on a keyword in the function position of an ap
  | SynKeywordArrow(HTyp.t, ExpandingKeyword.t)
  // cursor is on a case with inconsistent branch types
  // in the function position of an ap
  | SynInconsistentBranchesArrow(list(HTyp.t), CursorPath.steps)
  // cursor is on invalid text in the fuction position of an ap
  | SynInvalidArrow(HTyp.t)
  // cursor is on invalid text
  | SynInvalid
  // none of the above, cursor is on a free variable
  | SynFree
  // cursor is on a keyword
  | SynKeyword(ExpandingKeyword.t)
  // cursor is on the clause of a case
  | SynBranchClause
      // lub of other branches
      (
        join_of_branches,
        // info for the clause
        typed,
        // index of the branch
        int,
      )
  // cursor is on a case with branches of inconsistent types
  // keep track of steps to form that contains the branches
  | SynInconsistentBranches(list(HTyp.t), CursorPath.steps)
  // none of the above
  | Synthesized(HTyp.t)
  /* cursor in analytic pattern position */
  // cursor is on a type inconsistent pattern
  | PatAnaTypeInconsistent(HTyp.t, HTyp.t)
  // cursor is on a tuple pattern of the wrong length
  | PatAnaWrongLength
      // expected length
      (
        int,
        // got length
        int,
        // expected type
        HTyp.t,
      )
  // cursor is on invalid text
  | PatAnaInvalid(HTyp.t)
  // cursor is on a keyword
  | PatAnaKeyword(HTyp.t, ExpandingKeyword.t)
  // none of the above and didn't go through subsumption
  | PatAnalyzed(HTyp.t)
  // none of the above and went through subsumption
  | PatAnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic pattern position */
  // cursor is on a keyword
  | PatSynthesized(HTyp.t)
  | PatSynKeyword(ExpandingKeyword.t)
  /* cursor in type position */
  | OnType
  /* (we will have a richer structure here later)*/
  | OnLetLine(HTyp.t)
  | OnLine
  | OnRule;

[@deriving sexp]
type cursor_term =
  | Exp(CursorPosition.t, UHExp.operand)
  | Pat(CursorPosition.t, UHPat.operand)
  | Typ(CursorPosition.t, UHTyp.operand)
  | ExpOp(CursorPosition.t, UHExp.operator)
  | PatOp(CursorPosition.t, UHPat.operator)
  | TypOp(CursorPosition.t, UHTyp.operator)
  | Line(CursorPosition.t, UHExp.line)
  | Rule(CursorPosition.t, UHExp.rule);

[@deriving sexp]
type syntactic_context =
  // zseq containing the selected operand and the expected type of the whole opseq
  | ExpSeq(
      HTyp.t,
      ZSeq.t(UHExp.operand, UHExp.operator, ZExp.zoperand, ZExp.zoperator),
      ErrStatus.t,
    )
  | NoSeq;

[@deriving sexp]
type opParent = option(ZExp.zoperand);
// TODO refactor into variants
// based on term sort and shape
[@deriving sexp]
type t = {
  cursor_term,
  typed,
  ctx: Contexts.t,
  syntactic_context,
  opParent,
  // hack while merging
  uses: option(UsageAnalysis.uses_list),
};

type pro = {
  expected_ty: HTyp.t,
  actual_ty: option(HTyp.t),
  mode,
  typed,
  term: cursor_term,
  ctx: Contexts.t,
  uses: option(UsageAnalysis.uses_list),
  u_gen: MetaVarGen.t,
  syntactic_context,
  opParent,
};

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

let get_mode = (cursor_info: t) => {
  let (_, _, mode) = get_types_and_mode(cursor_info.typed);
  mode;
};

let promote_cursor_info =
    (
      u_gen: MetaVarGen.t,
      {cursor_term, typed, ctx, uses, syntactic_context, opParent}: t,
    )
    : pro => {
  let (expected_ty, actual_ty, mode) = get_types_and_mode(typed);
  // TODO(andrew): handle more cases in get_types_and_mode
  let expected_ty =
    switch (expected_ty) {
    | None => HTyp.Hole
    | Some(ty) => ty
    };
  {
    expected_ty,
    actual_ty,
    typed,
    mode,
    u_gen,
    term: cursor_term,
    ctx,
    uses,
    syntactic_context,
    opParent,
  };
};

let extract_cursor_term = (zexp: ZExp.t): cursor_term => {
  switch (CursorFrame.get(zexp)) {
  | [{slice: ExpLine(CursorL(c, s)), _}, ..._] => Line(c, s)
  | [{slice: ExpOperand(CursorE(c, s)), _}, ..._] => Exp(c, s)
  | [{slice: ExpOperator((c, s)), _}, ..._] => ExpOp(c, s)
  | [{slice: ExpRule(CursorR(c, s)), _}, ..._] => Rule(c, s)
  | [{slice: PatOperand(CursorP(c, s)), _}, ..._] => Pat(c, s)
  | [{slice: PatOperator((c, s)), _}, ..._] => PatOp(c, s)
  | [{slice: TypOperand(CursorT(c, s)), _}, ..._] => Typ(c, s)
  | [{slice: TypOperator((c, s)), _}, ..._] => TypOp(c, s)
  | frame =>
    print_endline(Sexplib.Sexp.to_string_hum(CursorFrame.sexp_of_t(frame)));
    failwith("INVALID FRAME (extract_cursor_term)");
  };
};
