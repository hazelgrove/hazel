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
  let (d_result, delta, _, _, es) = prev_result;
  let (_, hole_ty, var_ctx) = delta |> MetaVarMap.find(u);

  /* Elaborate the expression in analytic position against the hole type. */
  let (d, delta, actual_ty) =
    switch (Elaborator_Exp.ana_elab(var_ctx, delta, e, hole_ty)) {
    | Elaborates(d, ty, delta) => (d, delta, ty)
    | DoesNotElaborate => raise(FillAndResumeException(FailedElaboration))
    };

  /* TODO: remove; for diagnostic purposes only */
  print_endline("Analyzing against type:");
  hole_ty |> HTyp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
  print_endline("Actual type:");
  actual_ty |> HTyp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
  print_endline("Elaborates to:");
  d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

  /* Add cast to the correct type. Do we always need this? */
  let d = DHExp.cast(d, actual_ty, hole_ty);
  print_endline("Elaborates to (casted):");
  d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

  /* Perform evaluation with new fill information */
  let es = es |> EvalState.set_far_info(Fill(u, d));
  let (es, dr_result) = Evaluator.evaluate(es, EvalEnv.empty, d_result);

  /* Ordinary postprocessing */
  let (hci, d_result, dr_result) =
    switch (dr_result) {
    | Indet(d) =>
      let (hci, d) = EvalPostprocess.postprocess(d);
      (hci, d, EvaluatorResult.Indet(d));
    | BoxedValue(d) =>
      let (hci, d) = EvalPostprocess.postprocess(d);
      (hci, d, EvaluatorResult.BoxedValue(d));
    };

  (d_result, delta, hci, dr_result, es);
};

let is_fill_viable =
    (old_prog: Program.t, new_prog: Program.t): option((UHExp.t, MetaVar.t)) => {
  let e1 = old_prog |> Program.get_uhexp;
  let e2 = new_prog |> Program.get_uhexp;

  /* TODO: remove print statements; for diagnostics */
  let print_uhexp = (e: UHExp.t): unit =>
    e |> UHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
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
