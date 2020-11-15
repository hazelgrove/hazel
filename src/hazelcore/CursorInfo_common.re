open Sexplib.Std;

[@deriving sexp]
type join_of_branches =
  | NoBranches
  // steps to the case
  | InconsistentBranchTys(list(HTyp.t), CursorPath_common.steps)
  | JoinTy(HTyp.t);

[@deriving sexp]
type typed =
  // cursor is on a lambda with an argument type annotation
  /* cursor in analytic position */
  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  // cursor is on a type inconsistent expression
  | AnaTypeInconsistent(HTyp.t, HTyp.t, string)
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
  // cursor is on a free livelit
  | AnaFreeLivelit(HTyp.t)
  // cursor is on an ApLivelit that is followed by too few args
  | AnaInsufficientLivelitArgs(HTyp.t, HTyp.t)
  // cursor is on invalid text
  | AnaInvalid(HTyp.t)
  // cursor is on a keyword
  | AnaKeyword(HTyp.t, ExpandingKeyword.t)
  // none of the above and didn't go through subsumption
  | Analyzed(HTyp.t)
  // none of the above and went through subsumption
  | AnaSubsumed(HTyp.t, HTyp.t, string)
  /* cursor in synthetic position */
  // cursor is on an ApLivelit, that is followed by too few args
  | SynErrorInsufficientLivelitArgs(HTyp.t)
  // cursor is on the function position of an ap,
  // and that expression does not synthesize a type
  // with a matched arrow type
  | SynErrorArrow
      // expected
      (
        HTyp.t,
        // got
        HTyp.t,
        string,
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
  | SynInconsistentBranchesArrow(list(HTyp.t), CursorPath_common.steps)
  // cursor is on invalid text in the fuction position of an ap
  | SynInvalidArrow(HTyp.t)
  // cursor is on invalid text
  | SynInvalid
  | SynFreeLivelit
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
  | SynInconsistentBranches(list(HTyp.t), CursorPath_common.steps)
  // none of the above
  | Synthesized(HTyp.t, string)
  /* cursor in analytic pattern position */
  // cursor is on a type inconsistent pattern
  | PatAnaTypeInconsistent(HTyp.t, HTyp.t, string)
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
  | PatAnaSubsumed(HTyp.t, HTyp.t, string)
  /* cursor in synthetic pattern position */
  // cursor is on a keyword
  | PatSynthesized(HTyp.t, string)
  | PatSynKeyword(ExpandingKeyword.t)
  /* cursor in type position */
  | OnType
  /* (we will have a richer structure here later)*/
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

// TODO refactor into variants
// based on term sort and shape
//[@deriving sexp]
type t = {
  cursor_term,
  typed,
  ctx: Contexts.t,
  // hack while merging
  uses: option(UsageAnalysis.uses_list),
};

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
    | BoolLit(_, _)
    | StringLit(_) => true
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
    | LetLine(_, _, _)
    | LivelitDefLine(_)
    | AbbrevLine(_)
    | ExpLine(_) => false
    }
  | Rule(_, _) => false
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

let is_empty_line = (cursor_term): bool =>
  switch (cursor_term) {
  | Line(_, EmptyLine) => true
  | Line(_, _) => false
  | Exp(_, _)
  | Pat(_, _)
  | Typ(_, _)
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _)
  | Rule(_, _) => false
  };

let rec is_text_cursor = (ze: ZExp.t): bool => ze |> is_text_cursor_zblock
and is_text_cursor_zblock = ((_, zline, _): ZExp.zblock): bool =>
  zline |> is_text_cursor_zline
and is_text_cursor_zline =
  fun
  | CursorL(cursor, _) =>
    switch (cursor) {
    | OnText(_) => true
    | _ => false
    }
  | ExpLineZ(zopseq) => zopseq |> is_text_cursor_zopseq_exp
  | LetLineZP(zp, _, _) => zp |> is_text_cursor_zopseq_pat
  | LivelitDefLineZExpansionType(_)
  | LivelitDefLineZModelType(_)
  | LivelitDefLineZActionType(_)
  | LetLineZA(_) => false
  | LivelitDefLineZCaptures({captures: zdef, _})
  | LivelitDefLineZInit({init: zdef, _})
  | LivelitDefLineZUpdate({update: zdef, _})
  | LivelitDefLineZView({view: zdef, _})
  | LivelitDefLineZShape({shape: zdef, _})
  | LivelitDefLineZExpand({expand: zdef, _})
  | LetLineZE(_, _, zdef) => zdef |> is_text_cursor
  | AbbrevLineZL(_, _, _, (_, zoperand, _)) =>
    zoperand |> is_text_cursor_zoperand_exp
and is_text_cursor_zopseq_exp =
  fun
  | ZOpSeq(_, ZOperand(zoperand, _)) =>
    zoperand |> is_text_cursor_zoperand_exp
  | ZOpSeq(_, ZOperator(_)) => false
and is_text_cursor_zoperand_exp =
  fun
  | CursorE(OnText(_), StringLit(_, _)) => true
  | CaseZR(_, _, zrules) => {
      let (_, zrule, _) = zrules;
      zrule |> is_text_cursor_zrule;
    }
  | LamZP(_, zp, _, _) => zp |> is_text_cursor_zopseq_pat
  | ParenthesizedZ(ze)
  | LamZE(_, _, _, ze)
  | InjZ(_, _, ze)
  | CaseZE(_, ze, _)
  | SubscriptZE1(_, ze, _, _)
  | SubscriptZE2(_, _, ze, _)
  | SubscriptZE3(_, _, _, ze) => ze |> is_text_cursor
  | ApLivelitZ(_, _, _, _, _, zsi) =>
    is_text_cursor(ZSpliceInfo.prj_ze(zsi))
  | LamZA(_)
  | CursorE(_) => false
and is_text_cursor_zrule =
  fun
  | RuleZP(zp, _) => zp |> is_text_cursor_zopseq_pat
  | RuleZE(_, ze) => ze |> is_text_cursor
  | CursorR(_) => false
and is_text_cursor_zopseq_pat =
  fun
  | ZOpSeq(_, ZOperand(zoperand, _)) =>
    zoperand |> is_text_cursor_zoperand_pat
  | ZOpSeq(_, ZOperator(_)) => false
and is_text_cursor_zoperand_pat =
  fun
  | CursorP(OnText(_), StringLit(_, _)) => true
  | ParenthesizedZ(zp) => zp |> is_text_cursor_zopseq_pat
  | InjZ(_, _, zp) => zp |> is_text_cursor_zopseq_pat
  | CursorP(_) => false;

let is_invalid_escape_sequence = (j, s) =>
  if (String.length(s) > j && s.[j] == '\\') {
    /* |\ */
    if (String.length(s) == j + 1) {
      Some("\\");
    } else {
      let (ch, ind) =
        if (j >= 2 && s.[j - 2] == '\\') {
          (s.[j - 1], j - 2);
        } else {
          (s.[j + 1], j);
        };
      switch (ch) {
      | 'b'
      | 't'
      | 'r'
      | 'n'
      | '\\'
      | '"'
      | '\''
      | ' ' => None
      | 'o' =>
        if (String.length(s) < j + 5) {
          Some(String.sub(s, ind, 2));
        } else {
          let ch1 = s.[j + 2];
          let ch2 = s.[j + 3];
          let ch3 = s.[j + 4];
          if ((ch1 >= '0' && ch1 <= '7')
              && (ch2 >= '0' && ch2 <= '7')
              && ch3 >= '0'
              && ch3 <= '7') {
            None;
          } else {
            Some(String.sub(s, ind, 2));
          };
        }
      | 'x' =>
        if (String.length(s) < j + 4) {
          Some(String.sub(s, ind, 2));
        } else {
          let ch1 = Char.lowercase_ascii(s.[j + 2]);
          let ch2 = Char.lowercase_ascii(s.[j + 3]);
          if ((ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f')
              && (ch2 >= '0' && ch2 <= '9' || ch2 >= 'a' && ch2 <= 'f')) {
            None;
          } else {
            Some(String.sub(s, ind, 2));
          };
        }
      | _ =>
        if (String.length(s) < j + 4) {
          Some(String.sub(s, ind, 2));
        } else {
          let ch1 = s.[j + 1];
          let ch2 = s.[j + 2];
          let ch3 = s.[j + 3];
          if ((ch1 >= '0' && ch1 <= '9')
              && (ch2 >= '0' && ch2 <= '9')
              && ch3 >= '0'
              && ch3 <= '9') {
            None;
          } else {
            Some(String.sub(s, ind, 2));
          };
        }
      };
    };
  } else if (j >= 1 && s.[j - 1] == '\\') {
    /* \| */
    if (String.length(s) == j) {
      Some("\\");
    } else {
      switch (s.[j]) {
      | 'b'
      | 't'
      | 'r'
      | 'n'
      | '\\'
      | '"'
      | '\''
      | ' ' => None
      | 'o' =>
        if (String.length(s) < j + 4) {
          Some(String.sub(s, j - 1, 2));
        } else {
          let ch1 = s.[j + 1];
          let ch2 = s.[j + 2];
          let ch3 = s.[j + 3];
          if ((ch1 >= '0' && ch1 <= '7')
              && (ch2 >= '0' && ch2 <= '7')
              && ch3 >= '0'
              && ch3 <= '7') {
            None;
          } else {
            Some(String.sub(s, j - 1, 2));
          };
        }
      | 'x' =>
        if (String.length(s) < j + 3) {
          Some(String.sub(s, j - 1, 2));
        } else {
          let ch1 = Char.lowercase_ascii(s.[j + 1]);
          let ch2 = Char.lowercase_ascii(s.[j + 2]);
          if ((ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f')
              && (ch2 >= '0' && ch2 <= '9' || ch2 >= 'a' && ch2 <= 'f')) {
            None;
          } else {
            Some(String.sub(s, j - 1, 2));
          };
        }
      | _ =>
        if (String.length(s) < j + 3) {
          Some(String.sub(s, j - 1, 2));
        } else {
          let ch1 = s.[j];
          let ch2 = s.[j + 1];
          let ch3 = s.[j + 2];
          if ((ch1 >= '0' && ch1 <= '9')
              && (ch2 >= '0' && ch2 <= '9')
              && ch3 >= '0'
              && ch3 <= '9') {
            None;
          } else {
            Some(String.sub(s, j - 1, 2));
          };
        }
      };
    };
  } else if (j >= 2 && s.[j - 2] == '\\') {
    /* "\b|" */
    switch (s.[j - 1]) {
    | 'b'
    | 't'
    | 'r'
    | 'n'
    | '\\'
    | '"'
    | '\''
    | ' ' => None
    | 'o' =>
      if (String.length(s) < j + 3) {
        Some(String.sub(s, j - 2, 2));
      } else {
        let ch1 = s.[j];
        let ch2 = s.[j + 1];
        let ch3 = s.[j + 2];
        if ((ch1 >= '0' && ch1 <= '7')
            && (ch2 >= '0' && ch2 <= '7')
            && ch3 >= '0'
            && ch3 <= '7') {
          None;
        } else {
          Some(String.sub(s, j - 2, 2));
        };
      }
    | 'x' =>
      if (String.length(s) < j + 2) {
        Some(String.sub(s, j - 2, 2));
      } else {
        let ch1 = Char.lowercase_ascii(s.[j]);
        let ch2 = Char.lowercase_ascii(s.[j + 1]);
        if ((ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f')
            && (ch2 >= '0' && ch2 <= '9' || ch2 >= 'a' && ch2 <= 'f')) {
          None;
        } else {
          Some(String.sub(s, j - 2, 2));
        };
      }
    | _ =>
      if (String.length(s) < j + 2) {
        Some(String.sub(s, j - 2, 2));
      } else {
        let ch1 = s.[j - 1];
        let ch2 = s.[j];
        let ch3 = s.[j + 1];
        if ((ch1 >= '0' && ch1 <= '9')
            && (ch2 >= '0' && ch2 <= '9')
            && ch3 >= '0'
            && ch3 <= '9') {
          None;
        } else {
          Some(String.sub(s, j - 2, 2));
        };
      }
    };
  } else {
    None;
  };

let mk = (~uses=?, typed, ctx, cursor_term) => {
  typed,
  ctx,
  uses,
  cursor_term,
};

let get_ctx = ci => ci.ctx;

/*
 * there are cases we can't determine where to find the uses of a variable
 * immediately after we see its binding site.
 * in this case, we will return a deferrable('t) and go up the tree
 * until we could find uses and feed it to (uses_list => 't).
 */

type deferrable('t) =
  | CursorNotOnDeferredVarPat('t)
  | CursorOnDeferredVarPat(UsageAnalysis.uses_list => 't, Var.t);
