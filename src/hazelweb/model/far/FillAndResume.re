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
  switch (DiffUHExp.diff_block(e1, e2)) {
  | DiffUHExp.BFillDiff(opseq, Some(u)) =>
    Some(([UHExp.ExpLine(opseq)], u))
  | _ => None
  };
};
