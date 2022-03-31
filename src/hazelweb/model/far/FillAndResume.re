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

  /* Generalized closures: need to set the `re_eval` flag
     to true, and recurse through the environment.

     TODO: memoize environments so we don't need to re-preprocess
     the same environment every time it is re-encountered.
     */
  | Closure(env, _, d) =>
    Closure(
      env
      |> EvalEnv.map_keep_id((_, dr) =>
           switch (dr) {
           | Indet(d) => Indet(d |> preprocess)
           | BoxedValue(d) => BoxedValue(d |> preprocess)
           }
         ),
      true,
      d |> preprocess,
    )

  /* Other expressions forms: simply recurse through subexpressions. */
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | BoundVar(_)
  | Triv => d
  | Let(dp, d1, d2) => Let(dp, d1 |> preprocess, d2 |> preprocess)
  | FixF(x, ty, d) => FixF(x, ty, d |> preprocess)
  | Fun(dp, ty, d) => Fun(dp, ty, d |> preprocess)
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

let fill = (d: DHExp.t, u: MetaVar.t, prev_result: Result.t): Result.t => {
  /* TODO: remove print statements; for diagnostics */
  print_endline("==========\n\n\n");
  print_endline("Attempting to fill expression:");
  d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;
  print_endline("In hole: " ++ string_of_int(u));

  print_endline("Previous result:");
  prev_result
  |> Result.get_unpostprocessed_dhexp
  |> DHExp.sexp_of_t
  |> Sexplib.Sexp.to_string
  |> print_endline;

  /* TODO: remove; for diagnostics */
  print_endline(
    "Previous number of evaluation steps:"
    ++ (
      prev_result
      |> Result.get_eval_state
      |> EvalState.get_stats
      |> EvalStats.get_steps
      |> string_of_int
    ),
  );

  /* Perform FAR preprocessing */
  let d_result =
    prev_result |> Result.get_unpostprocessed_dhexp |> preprocess(u, d);

  print_endline("Preprocessed previous result:");
  d_result |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

  /* Re-start evaluation */
  let (es, dr_result) =
    switch (
      d_result
      |> Evaluator.evaluate(
           prev_result |> Result.get_eval_state,
           EvalEnv.empty,
         )
    ) {
    | (es, dr_result) => (es, dr_result)
    | exception (EvaluatorError.Exception(err)) =>
      raise(Program.EvalError(err))
    };

  /* TODO: remove; for diagnostics */
  print_endline(
    "Current number of evaluation steps:"
    ++ (es |> EvalState.get_stats |> EvalStats.get_steps |> string_of_int),
  );

  /* Ordinary postprocessing */
  let (hci, d_result, dr_postprocessed) =
    switch (dr_result) {
    | Indet(d) =>
      print_endline("Resumed evaluation result:");
      d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

      let (hci, d_postprocessed) =
        switch (EvalPostprocess.postprocess(d)) {
        | hci_d => hci_d
        | exception (EvalPostprocessError.Exception(err)) =>
          print_endline(
            "fill and resume postprocessing error: "
            ++ Sexplib.Sexp.to_string(err |> EvalPostprocessError.sexp_of_t),
          );
          raise(Program.PostprocessError(err));
        };

      print_endline("Postprocesses to:");
      d_postprocessed
      |> DHExp.sexp_of_t
      |> Sexplib.Sexp.to_string
      |> print_endline;
      (hci, d, EvaluatorResult.Indet(d_postprocessed));
    | BoxedValue(d) =>
      print_endline("Resumed evaluation result:");
      d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

      let (hci, d_postprocessed) =
        switch (EvalPostprocess.postprocess(d)) {
        | hci_d => hci_d
        | exception (EvalPostprocessError.Exception(err)) =>
          print_endline(
            "fill and resume postprocessing error: "
            ++ Sexplib.Sexp.to_string(err |> EvalPostprocessError.sexp_of_t),
          );
          raise(Program.PostprocessError(err));
        };

      print_endline("Postprocesses to:");
      d_postprocessed
      |> DHExp.sexp_of_t
      |> Sexplib.Sexp.to_string
      |> print_endline;
      (hci, d, EvaluatorResult.BoxedValue(d_postprocessed));
    };

  Result.mk(dr_postprocessed, d_result, hci, es);
};

let is_fill_viable =
    (old_prog: Program.t, new_prog: Program.t): option((DHExp.t, MetaVar.t)) => {
  let e1 = old_prog |> Program.get_uhexp;
  let e2 = new_prog |> Program.get_uhexp;

  let elaborate = (e: UHExp.t): DHExp.t => {
    exception DoesNotElaborateError;
    switch (e |> Elaborator_Exp.syn_elab(VarCtx.empty, Delta.empty)) {
    | Elaborator_Exp.ElaborationResult.Elaborates(d, _, _) => d
    | _ => raise(DoesNotElaborateError)
    };
  };
  let print_dhexp = (d: DHExp.t): unit =>
    d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

  print_endline("==========\n\n\n");
  print_endline("Old program:");
  e1 |> elaborate |> print_dhexp;
  print_endline("New program:");
  e2 |> elaborate |> print_dhexp;

  let diff = DiffDHExp.diff_dhexp(e1 |> elaborate, e2 |> elaborate);

  print_endline("Fill diff:");
  diff |> DiffDHExp.sexp_of_t |> Sexplib.Sexp.to_string |> print_endline;

  switch (diff) {
  | NonFillDiff
  | NoDiff => None
  | FillDiff(d, u) => Some((d, u))
  };
};
