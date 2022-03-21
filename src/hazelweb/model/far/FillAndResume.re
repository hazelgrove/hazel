/* Temporary -- later on try to consolidate these with existing errors */
type error =
  | FailedElaboration;
exception FillAndResumeException(error);

let fill = (e: UHExp.t, u: MetaVar.t, prev_result: Result.t): Result.t => {
  /* TODO: remove print statements; for diagnostics */
  print_endline("Attempting to fill expression:");
  e |> UHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
  print_endline("In hole: " ++ string_of_int(u));

  /* Get the hole type from the hole context. */
  let (d_result, delta, hci, dr_result) = prev_result;
  let (_, hole_ty, var_ctx) = delta |> MetaVarMap.find(u);
  /* Elaborate the expression in analytic position against the hole type. */
  let (_d, delta) =
    switch (Elaborator_Exp.ana_elab(var_ctx, delta, e, hole_ty)) {
    | Elaborates(d, _ty, delta) => (d, delta)
    | DoesNotElaborate => raise(FillAndResumeException(FailedElaboration))
    };
  /* TODO: Evaluation */
  /* (ec, dr_result) = evaluate(ec, EmptyEnv, d, hci) */
  /* TODO: Postprocessing */
  /* (hci, result) = postprocess(dr) */
  (d_result, delta, hci, dr_result);
};

/* Diff result for when diffing two `UHExp.opseq` or `UHExp.operand`.

   `ONoDiff`: Opseqs are equal.
   `ODiff`: There is a difference. The first argument is the filled opseq
   (from the second opseq). The hole number is from then first opseq,
   if the replaced expression is a hole.
   */
type opseq_diff_result =
  | ONoDiff
  | ODiff(UHExp.opseq, option(MetaVar.t));

/* Diff result for when diffing two `UHExp.block` or `UHExp.line`.

   `BNoDiff`: Line(s) are equal
   `BNonFillDiff`:
   - Different number of lines
   - Comparison between `LetLine` and `ExpLine` (different types of lines)
   - Multiple lines have differences
   `BFillDiff`: Only a single line (`LetLine` or `ExpLine`) has a difference,
   and all lines are of the same type.
   */
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
  | Var(err_status, _, _)
  | IntLit(err_status, _)
  | FloatLit(err_status, _)
  | BoolLit(err_status, _)
  | ListNil(err_status)
  | Lam(err_status, _, _)
  | Inj(err_status, _, _) => err_status |> get_hole_number_of_err_status
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
  let diff =
    ODiff(
      OpSeq(Placeholder(p2), seq2),
      operand1 |> get_hole_number_of_operand,
    );
  switch (operand1, operand2) {
  /* Note: it matter if the new hole has a different number than the old hole,
     because this will change the hole numbers in the result (even if everything
     else is the same). */

  /* TODO: need to recurse into Lam, Inj, Case, Parenthesized, since we may
     be able to narrow down the hole more. */
  | (EmptyHole(_), EmptyHole(_))
  | (InvalidText(_), InvalidText(_))
  | (Var(_), Var(_))
  | (IntLit(_), IntLit(_))
  | (FloatLit(_), FloatLit(_))
  | (BoolLit(_), BoolLit(_))
  | (ListNil(_), ListNil(_))
  | (Lam(_, _, _), Lam(_, _, _))
  | (Inj(_), Inj(_))
  | (Case(_), Case(_))
  | (Parenthesized(_), Parenthesized(_)) =>
    operand1 == operand2 ? ONoDiff : diff

  /* Not of the same form: necessarily a diff */
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
     - If the operator is not equal, then the diff is the current node.
     - Else if only one child has a diff, then that diff should carry through.
     - Else if both children have a diff, then the diff is the current node.
     - Else there is no diff.

     If the diff is the current node (BinOp), then the LHS determines whether it
     is a hole fill or not.

     Note: is it possible for the error status of the binops to be different
     (and everything else to be the same)? This is not accounted for, because
     I don't believe that can happen.
     */
  | (BinOp(err_status1, op1, skel11, skel12), BinOp(_, op2, skel21, skel22)) =>
    if (op1 != op2) {
      ODiff(opseq2, err_status1 |> get_hole_number_of_err_status);
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

      /* Exactly one child has a diff: pass it through. */
      | (ODiff(_) as diff, ONoDiff)
      | (ONoDiff, ODiff(_) as diff) => diff
      };
    }
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

/* Check if a `UHExp.t` (or `UHExp.block`) has a valid diff.

   This means that there is exactly one line (`ExpLine` or `LetLine`)
   which has a difference (`ODiff`).

   If there are no lines that are different, then there is no diff (`BNoDiff`).

   If there are multiple lines that are different, if there are different line
   types being compared (ignoring empty or comment lines), or if there are a
   differing number of lines (also ignoring empty or comment lines), then there
   is no valid diff for a fill operation (`BNonFillDiff`).

   Note that empty and comment lines are ignored.

   TODO: handle the case when a single line with a hole gets filled with
   multiple lines.

   TODO: handle the case when the root of the diff is not exactly at a hole?
   (would probably not handle it in this function)
   */
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

/* Simpler case: only handle the cases when all the changes lie in one line */
/* Simpler case: don't worry about in non-empty holes for now, only fill if the root is an
   empty hole */
/* Simpler case: only worry about ExpLine for now */
let is_fill_viable =
    (old_prog: Program.t, new_prog: Program.t): option((UHExp.t, MetaVar.t)) => {
  let print_uhexp = (e: UHExp.t): unit =>
    e |> UHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
  let e1 = old_prog |> Program.get_uhexp;
  let e2 = new_prog |> Program.get_uhexp;

  /* TODO: remove print statements; for diagnostics */
  print_endline("Old program:");
  e1 |> print_uhexp;
  print_endline("New program:");
  e2 |> print_uhexp;

  /* Check that only one line is different and has a valid diff */
  switch (diff_block(e1, e2)) {
  | BFillDiff(opseq, Some(u)) => Some(([UHExp.ExpLine(opseq)], u))
  | _ => None
  };
};
