open Sexplib.Std;

[@deriving sexp]
type opseq_diff_result =
  | ONoDiff
  | ODiff(UHExp.opseq, option(MetaVar.t));

[@deriving sexp]
type block_diff_result =
  | BNoDiff
  | BNonFillDiff
  | BFillDiff(UHExp.opseq, option(MetaVar.t));

/* Helper for `diff_block` */
let remove_empty_comment_lines: UHExp.t => UHExp.t =
  List.filter((line: UHExp.line) =>
    switch (line) {
    | EmptyLine
    | CommentLine(_) => false
    | _ => true
    }
  );

let get_hole_number_of_err_status = (err_status: ErrStatus.t): option(int) =>
  switch (err_status) {
  | NotInHole => None
  | InHole(_, u) => Some(u)
  };

/* Helper for `diff_operand` and `diff_opseq`. Get the hole number of an operand
   if it is a hole, otherwise `None`. */
let get_hole_number_of_operand = (operand: UHExp.operand): option(int) =>
  switch (operand) {
  | EmptyHole(u)
  | InvalidText(u, _) => Some(u)
  | IntLit(err_status, _)
  | FloatLit(err_status, _)
  | BoolLit(err_status, _)
  | ListNil(err_status)
  | Fun(err_status, _, _)
  | Inj(err_status, _, _) => err_status |> get_hole_number_of_err_status
  | Var(err_status, var_err_status, _) =>
    let hole_num = err_status |> get_hole_number_of_err_status;
    let var_hole_num =
      switch (var_err_status) {
      | NotInVarHole => None
      | InVarHole(_, u) => Some(u)
      };
    /* Use either regular hole or variable hole, if applicable */
    switch (hole_num, var_hole_num) {
    | (Some(u), _) => Some(u)
    | (_, u) => u
    };
  | Case(case_err_status, _, _) =>
    switch (case_err_status) {
    | StandardErrStatus(err_status) =>
      err_status |> get_hole_number_of_err_status
    | InconsistentBranches(_, u) => Some(u)
    }
  | Parenthesized(_) => None
  };

let rec diff_operand =
        (
          p1: int,
          seq1: Seq.t(UHExp.operand, UHExp.operator),
          p2: int,
          seq2: Seq.t(UHExp.operand, UHExp.operator),
        )
        : opseq_diff_result => {
  let operand1 = seq1 |> Seq.nth_operand(p1);
  let operand2 = seq2 |> Seq.nth_operand(p2);

  /* The diff if the current node is the root of the change. */
  let diff =
    ODiff(
      OpSeq(Placeholder(p2), seq2),
      operand1 |> get_hole_number_of_operand,
    );

  /* Deciding the root of the diff if there is a sub-`UHExp.t`. See the comment
     below just before the usages of this function. */
  let get_diff_root_subexp = (e1: UHExp.t, e2: UHExp.t): opseq_diff_result =>
    switch (diff_block(e1, e2)) {
    | BNonFillDiff => diff
    | BFillDiff(opseq, None) =>
      switch (operand1 |> get_hole_number_of_operand) {
      | None => ODiff(opseq, None)
      | Some(_) => diff
      }
    | BFillDiff(opseq, u) => ODiff(opseq, u)
    | BNoDiff => ONoDiff
    };

  switch (operand1, operand2) {
  /* Note: it matter if the new hole has a different number/error status than
     the old hole/expression with an error status, because this will change
     the hole numbers in the result (even if everything else is the same). */
  | (EmptyHole(_), EmptyHole(_))
  | (InvalidText(_), InvalidText(_))
  | (Var(_), Var(_))
  | (IntLit(_), IntLit(_))
  | (FloatLit(_), FloatLit(_))
  | (BoolLit(_), BoolLit(_))
  | (ListNil(_), ListNil(_)) => operand1 == operand2 ? ONoDiff : diff

  /* Operands with (a single) subexpression:
     - If non-subexpressions are unequal, then current node is diff.
     - If subexpression has invalid diff, then current node is diff.
     - If subexpression has valid diff but no hole, and current operand is
     a hole, then current node is diff.
     - Else pass through subexpression diff (may or may not be `BNoDiff`).

     Steps 2-4 are condensed into the helper function `get_diff_root_subexp`.
     */
  | (Fun(err_status1, pat1, e1), Fun(err_status2, pat2, e2)) =>
    err_status1 != err_status2 || pat1 != pat2
      ? diff : get_diff_root_subexp(e1, e2)
  | (Inj(err_status1, side1, e1), Inj(err_status2, side2, e2)) =>
    err_status1 != err_status2 || side1 != side2
      ? diff : get_diff_root_subexp(e1, e2)
  | (Case(_), Case(_)) =>
    /* TODO: implement this fully; am too lazy at the moment and this
       is not an important case. Is slightly different because there are
       multiple rules, and if multiple rules have a diff then the current
       node is necessarily the root of the diff. */
    operand1 == operand2 ? ONoDiff : diff
  | (Parenthesized(e1), Parenthesized(e2)) => get_diff_root_subexp(e1, e2)

  /* Not of the same form: current node is necessarily the diff root */
  | (_, _) => diff
  };
}

and diff_opseq = (opseq1: UHExp.opseq, opseq2: UHExp.opseq): opseq_diff_result => {
  let OpSeq(skel1, seq1) = opseq1;
  let OpSeq(skel2, seq2) = opseq2;

  switch (skel1, skel2) {
  /* Necessarily a diff. Check if LHS is a hole expression. */
  | (Placeholder(p), BinOp(_)) =>
    let hole_number = Seq.nth_operand(p, seq1) |> get_hole_number_of_operand;
    ODiff(opseq2, hole_number);
  | (BinOp(err_status, _, _, _), Placeholder(_)) =>
    let hole_number = err_status |> get_hole_number_of_err_status;
    ODiff(opseq2, hole_number);

  /* Recurse through operands. */
  | (Placeholder(p1), Placeholder(p2)) => diff_operand(p1, seq1, p2, seq2)

  /* If the operand is different, or if either child has a diff,
     then there is a diff.

     Cases:
     - If the operator/error status is not equal,
       then the diff root is the current node.
     - Else if both children have a diff, then the diff is the current node.
     - Else if only one child has a diff, then:
       - If the child diff has no hole, and the current `BinOp` is a hole, then
         the current node is the root of the diff.
       - Else pass through the child diff.
     - Else there is no diff.

     If the diff is the current node (BinOp), then the LHS determines whether it
     is a hole fill or not.
     */
  | (
      BinOp(err_status1, op1, skel11, skel12),
      BinOp(err_status2, op2, skel21, skel22),
    ) =>
    /* Diff if the current node is the root of the diff. */
    let curr_diff =
      ODiff(opseq2, err_status1 |> get_hole_number_of_err_status);

    if (err_status1 != err_status2 || op1 != op2) {
      curr_diff;
    } else {
      switch (
        diff_opseq(OpSeq(skel11, seq1), OpSeq(skel21, seq2)),
        diff_opseq(OpSeq(skel12, seq1), OpSeq(skel22, seq2)),
      ) {
      /* Both children have diffs: then the current node is the root of the diff. */
      | (ODiff(_), ODiff(_)) =>
        ODiff(opseq2, err_status1 |> get_hole_number_of_err_status)

      /* Neither child has a diff: then `ONoDiff`. */
      | (ONoDiff, ONoDiff) => ONoDiff

      /* Exactly one child has a diff w/o a hole: check if current node
         is a hole.*/
      | (ODiff(_, None) as child_diff, ONoDiff)
      | (ONoDiff, ODiff(_, None) as child_diff) =>
        switch (err_status1 |> get_hole_number_of_err_status) {
        | None => child_diff
        | Some(_) => curr_diff
        }

      /* Exactly one child has a diff w/ a hole: pass it through */
      | (ODiff(_) as child_diff, ONoDiff)
      | (ONoDiff, ODiff(_) as child_diff) => child_diff
      };
    };
  };
}

and diff_line = (line1: UHExp.line, line2: UHExp.line): block_diff_result =>
  switch (line1, line2) {
  | (LetLine(pat1, block1), LetLine(pat2, block2)) =>
    pat1 != pat2 ? BNonFillDiff : diff_block(block1, block2)
  | (ExpLine(opseq1), ExpLine(opseq2)) =>
    switch (diff_opseq(opseq1, opseq2)) {
    | ONoDiff => BNoDiff
    | ODiff(opseq, u_opt) => BFillDiff(opseq, u_opt)
    }
  | (_, _) => BNonFillDiff
  }

and diff_block = (e1: UHExp.t, e2: UHExp.t): block_diff_result => {
  /* Remove empty and comment lines */
  let e1 = e1 |> remove_empty_comment_lines;
  let e2 = e2 |> remove_empty_comment_lines;
  /* Check that the number of lines are the same */
  if (List.length(e1) != List.length(e2)) {
    BNonFillDiff;
  } else {
    /* Map diff over all of the lines; there should be exactly one `BFillDiff` */
    let line_diffs =
      List.map2(diff_line, e1, e2)
      |> List.filter((diff_result: block_diff_result) =>
           switch (diff_result) {
           | BFillDiff(_) => true
           | _ => false
           }
         );
    switch (line_diffs) {
    | [] => BNoDiff
    | [line_diff] => line_diff
    | _ => BNonFillDiff
    };
  };
};
