open Alcotest;
open Language;

module UG = Grammar.UnitGrammar;

let testable_exp =
    (~ignore_constructor_types=false, ~ignore_dynamic_errors=false, ()) =>
  testable(
    Fmt.using(Exp.show, Fmt.string),
    Equality.(
      equality({
        ...syntactic_settings,
        ignore_parens: true,
        ignore_function_names: true,
        ignore_function_types: true,
        ignore_unknown_provenance: true,
        ignore_explicit_unlabelling: true,
        ignore_dynamic_errors,
        ignore_constructor_types,
      })
    ).
      exp,
  );
let evaluate = unevaluated => {
  let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, unevaluated);
  result;
};
let dhexp_typ = testable_exp();

let evaluation_test =
    (
      ~ignore_constructor_types=?,
      ~ignore_dynamic_errors=?,
      msg,
      expected,
      unevaluated,
    ) =>
  check(
    testable_exp(~ignore_constructor_types?, ~ignore_dynamic_errors?, ()),
    msg,
    expected,
    evaluate(unevaluated),
  );

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

/* Build probe capture targets from a zipper's refractors: the union of the
 * manual and ephemeral probe ids, each paired with the refs visible at that
 * probe (refs_in for expressions, bound_in for patterns). */
let targets_of_zipper =
    (z: Haz3lcore.Zipper.t, info_map: Statics.Map.t): Sample.targets => {
  /* Extract probe IDs directly from zipper's refractors.
   * Map values to unit since we only need the IDs as keys. */
  let probe_ids =
    Id.Map.union(
      (_, _, _) => Some(),
      Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
      Id.Map.map(_ => (), z.refractors.multis.ephemerals),
    );
  /* Build targets from probe_ids, computing refs for each */
  Id.Map.fold(
    (id, (), acc) => {
      let refs =
        switch (Statics.Map.lookup_exp(id, info_map)) {
        | Some(_) => Statics.Map.refs_in(info_map, id)
        | None =>
          switch (Statics.Map.lookup_pat(id, info_map)) {
          | Some(_) => Statics.Map.bound_in(info_map, id)
          | None => []
          }
        };
      let spec: Sample.capture_spec = {refs: refs};
      Id.Map.add(id, spec, acc);
    },
    probe_ids,
    Id.Map.empty,
  );
};

/* Parse code with probes (^^probe syntax), elaborate it, and build targets */
let parse_with_probes =
    (s: string): (Exp.t, Exp.t, Statics.Map.t, Sample.targets) => {
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, s)) {
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  | Some(z) =>
    let make_term_result = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let term = make_term_result.term;
    /* Build statics map for refs lookup and evaluation */
    let (info_map, elaborated) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    let targets = targets_of_zipper(z, info_map);
    (term, elaborated, info_map, targets);
  };
};

let elaborate = u => {
  let (_, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), u);
  elab;
};

let elaborated_type = (info_map: Statics.Map.t, exp: Exp.t): Typ.t =>
  switch (Statics.Map.lookup_exp(Exp.rep_id(exp), info_map)) {
  | Some({ana, ty, ctx, _}) =>
    Typ.match_synswitch(ana, ty) |> Typ.normalize(ctx) |> Typ.all_ids_temp
  | None =>
    Alcotest.fail(
      "Preservation check failed: No type information found for expression",
    )
  };

(exp, probes) => (
  {
    term: exp,
    annotation: probes,
  }:
    Grammar.pat_t(list(Grammar.exp_t(unit)))
);
let parse_and_evaluate = (s: string) => evaluate(elaborate(parse_exp(s)));

let parse_and_evaluate_test =
    (
      ~msg: option(string)=?,
      ~ignore_constructor_types=?,
      ~ignore_dynamic_errors=?,
      expected: string,
      actual: string,
    ) =>
  evaluation_test(
    ~ignore_constructor_types?,
    ~ignore_dynamic_errors?,
    Option.value(~default=expected ++ " == " ++ actual, msg),
    parse_exp(expected),
    elaborate(parse_exp(actual)),
  );

let equal_limited_result =
    (lr1: Evaluator.limited_result, lr2: Evaluator.limited_result) =>
  switch (lr1, lr2) {
  | (LimitedCompleted((exp1, _)), LimitedCompleted((exp2, _))) =>
    Exp.equal(exp1, exp2)
  | (StepLimitExceeded, StepLimitExceeded) => true
  | _ => false
  };

let step_limited =
  testable(
    Fmt.using(Evaluator.show_limited_result, Fmt.string),
    equal_limited_result,
  );
let single_step = (exp: Exp.t) => {
  let step =
    EvaluatorStep.get_status(
      ~settings=CoreSettings.on,
      exp,
      Environment.empty,
    );
  switch (step) {
  | AutoStep(step) => EvaluatorStep.take_step(step)
  | AvailableSteps([step, ..._]) => EvaluatorStep.take_step(step)
  | AvailableSteps([]) => None
  };
};

let full_small_step_reduction =
    (~step_limit=1000, exp: TermBase.exp_t): Evaluator.limited_result => {
  let rec go = (~steps_counter=0, exp: TermBase.exp_t): option(Exp.t) =>
    if (steps_counter > step_limit) {
      None;
    } else {
      switch (single_step(exp)) {
      | Some(new_exp) => go(~steps_counter=steps_counter + 1, new_exp)
      | None => Some(exp)
      };
    };

  switch (go(~steps_counter=0, exp)) {
  | None => StepLimitExceeded
  | Some(new_exp) => LimitedCompleted((new_exp, EvaluatorState.empty))
  };
};

let full_preservation_test = (uexp: TermBase.exp_t): unit => {
  let (statics, elaborated) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
  let ty = elaborated_type(statics, uexp);

  let (evaluated, _) =
    Evaluator.evaluate(~env=Builtins.env_init, elaborated);
  let (new_statics, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), evaluated);

  let new_ty =
    switch (
      Statics.Map.ty_of(evaluated.annotation.ids |> List.hd_exn, new_statics)
    ) {
    | Some(ty) => ty
    | None =>
      Alcotest.fail(
        "Preservation check failed: No type information found for evaluated expression",
      )
    };

  if (Typ.is_consistent(Ctx.empty, new_ty, ty)) {
    ();
  } else {
    Alcotest.fail(
      "Preservation check failed: "
      ++ Typ.show(ty)
      ++ " !~ "
      ++ Typ.show(new_ty),
    );
  };
};
