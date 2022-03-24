/* Temporary -- later on try to consolidate these with existing errors */
type error =
  | FailedElaboration;
exception FillAndResumeException(error);

/* Preprocesses the previous evaluation result before re-evaluating with
   fill-and-resume. Performs two actions:
   - Set the `re_eval` flag of `DHExp.Closure` variants to `true` so they
     will be re-evaluated.
   - Substitute holes with the matching hole number.
   */
let rec preprocess = (u: MetaVar.t, d_fill: DHExp.t, d: DHExp.t): DHExp.t => {
  let preprocess: DHExp.t => DHExp.t = preprocess(u, d_fill);

  switch (d) {
  /* Hole types: fill if the hole number matches. */
  | EmptyHole(u', _)
  | Keyword(u', _, _)
  | FreeVar(u', _, _)
  | InvalidText(u', _, _) => u == u' ? d_fill : d
  | NonEmptyHole(reason, u', i, d) =>
    u == u' ? d_fill : NonEmptyHole(reason, u, i, d |> preprocess)
  | InconsistentBranches(u', i, Case(scrut, rules, case_i)) =>
    u == u'
      ? d_fill
      : InconsistentBranches(
          u',
          i,
          Case(
            scrut |> preprocess,
            rules
            |> List.map((DHExp.Rule(dp, d)) =>
                 DHExp.Rule(dp, d |> preprocess)
               ),
            case_i,
          ),
        )

  /* Generalized closures: need to set the `re_eval` field
     to true. */
  | Closure(env, _, d) => Closure(env, true, d |> preprocess)

  /* Other expressions forms: simply recurse through subexpressions. */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | BoundVar(_)
  | Triv => d
  | Let(dp, d1, d2) => Let(dp, d1 |> preprocess, d2 |> preprocess)
  | FixF(x, ty, d) => FixF(x, ty, d |> preprocess)
  | Lam(dp, ty, d) => Lam(dp, ty, d |> preprocess)
  | Ap(d1, d2) => Ap(d1 |> preprocess, d2 |> preprocess)
  | ApBuiltin(f, args) => ApBuiltin(f, args |> List.map(preprocess))
  | BinBoolOp(op, d1, d2) =>
    BinBoolOp(op, d1 |> preprocess, d2 |> preprocess)
  | BinIntOp(op, d1, d2) => BinIntOp(op, d1 |> preprocess, d2 |> preprocess)
  | BinFloatOp(op, d1, d2) =>
    BinFloatOp(op, d1 |> preprocess, d2 |> preprocess)
  | ListNil(_ty) => d
  | Cons(d1, d2) => Cons(d1 |> preprocess, d2 |> preprocess)
  | Inj(ty, side, d) => Inj(ty, side, d |> preprocess)
  | Pair(d1, d2) => Pair(d1 |> preprocess, d2 |> preprocess)
  | ConsistentCase(Case(scrut, rules, i)) =>
    ConsistentCase(
      Case(
        scrut |> preprocess,
        rules
        |> List.map((DHExp.Rule(dp, d)) => DHExp.Rule(dp, d |> preprocess)),
        i,
      ),
    )
  | Cast(d, ty1, ty2) => Cast(d |> preprocess, ty1, ty2)
  | FailedCast(d, ty1, ty2) => FailedCast(d |> preprocess, ty1, ty2)
  | InvalidOperation(d, reason) => InvalidOperation(d |> preprocess, reason)
  };
};

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

  print_endline("Previous result:");
  d_result |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

  /* TODO: remove; for diagnostics */
  print_endline(
    "Previous number of evaluation steps:"
    ++ (es |> EvalState.get_stats |> EvalStats.get_steps |> string_of_int),
  );

  /* Perform FAR preprocessing */
  let d_result = d_result |> preprocess(u, d);

  print_endline("Preprocessed previous result:");
  d_result |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

  /* TODO: remove FAR information from evaluate */
  /* Perform evaluation with new fill information */
  /* let es = es |> EvalState.set_far_info(Fill(u, d)); */
  let (es, dr_result) = Evaluator.evaluate(es, EvalEnv.empty, d_result);

  /* TODO: remove; for diagnostics */
  print_endline(
    "Current number of evaluation steps:"
    ++ (es |> EvalState.get_stats |> EvalStats.get_steps |> string_of_int),
  );

  /* Ordinary postprocessing */
  let (hci, d_result, dr_result) =
    switch (dr_result) {
    | Indet(d) =>
      let (hci, d) = EvalPostprocess.postprocess(d);
      print_endline("Evaluates to:");
      d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
      (hci, d, EvaluatorResult.Indet(d));
    | BoxedValue(d) =>
      let (hci, d) = EvalPostprocess.postprocess(d);
      print_endline("Evaluates to:");
      d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
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
