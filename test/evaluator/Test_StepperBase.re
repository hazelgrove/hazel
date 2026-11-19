open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Util;
open Haz3lcore;

// Import StepperBase types from web module
module StepperBase = Web.StepperBase;

// Helper constructors for step_kind_model
module StepKindHelpers = {
  // Create a SingleStep that will automatically take the next available step
  let mk_auto_single_step = (exp: Exp.t): StepperBase.step_kind_model => {
    let status =
      EvaluatorStep.get_status(
        ~settings=CoreSettings.on,
        exp,
        Environment.empty,
      );
    switch (status) {
    | AutoStep(step)
    | AvailableSteps([step, ..._]) =>
      StepperBase.SingleStep({
        persistent_evalobj: EvaluatorStep.persist(step),
        evalobj: Calc.Pending,
        next_exp: Calc.Pending,
      })
    | AvailableSteps([]) => failwith("No step available for expression")
    };
  };

  // Create an AxiomStep at a specific subexpression
  let mk_axiom_step =
      (
        ~name="reflexivity",
        ~at_idx=0,
        ~direction=Direction.Right,
        ~equality="Reflexive(==)",
        at_exp,
      ) // Required parameter at the end
      : StepperBase.step_kind_model => {
    StepperBase.AxiomStep({
      name,
      at_idx,
      at_exp,
      direction,
      equality,
      next_exp: Calc.Pending,
    });
  };

  // Create a MissingStep (for testing validation)
  let mk_missing_step = (): StepperBase.step_kind_model =>
    StepperBase.MissingStep(Web.MissingStep.Model.init);

  // Create an InductionStep with a scrutinee expression
  let mk_induction_step =
      (~exp: option(Exp.t)=?, ()): StepperBase.step_kind_model =>
    StepperBase.InductionStep(Web.InductionStep.init(~exp?, ()));
};

// Helper to create a minimal step model for testing
let mk_test_step =
    (~step_kind: StepperBase.step_kind_model, ~next_step=None, ())
    : StepperBase.step_model => {
  expr: Calc.Pending,
  editor: Calc.Pending,
  step_kind,
  next_step,
  hidden: Calc.Pending,
  proof_validity: Calc.Pending,
  editor_info_map: Calc.Pending,
};

// Helper constructors for InductionCase
module InductionCaseHelpers = {
  let mk_pattern_editor = (pattern_str: string): Web.CodeWithStatics.Model.t => {
    let pattern_exp = parse_exp(pattern_str);
    let editor =
      Editor.Model.mk(
        ~root=Exp,
        Zipper.unzip(
          ExpToSegment.exp_to_segment(
            ~settings=ExpToSegment.Settings.editable(~inline=true),
            pattern_exp,
          ),
        ),
      );
    Web.CodeWithStatics.Model.mk(editor);
  };

  // Create an InductionCase from a pattern string
  let mk_case =
      (
        ~pattern_str: string,
        ~inner_exp: option(Exp.t)=?,
        ~last_exp: option(Exp.t)=?,
        ~step: option(StepperBase.step_model)=?,
        (),
      )
      : Web.InductionCase.model'(StepperBase.step_model) => {
    {
      pattern: mk_pattern_editor(pattern_str),
      elab_pattern: Calc.Pending,
      inner_exp:
        inner_exp
        |> Option.map(exp => Calc.Calculated(exp))
        |> Option.value(~default=Calc.Pending),
      step: step |> Option.value(~default=StepperBase.init_step),
      last_exp:
        last_exp
        |> Option.map(exp => Calc.Calculated(exp))
        |> Option.value(~default=Calc.Pending),
      inner_ctx: Calc.Pending,
      hypotheses: Calc.Pending,
      constraint_: Calc.Pending,
    };
  };

  // Build a step chain by linking step_kinds into next_step fields
  let rec build_step_chain =
          (step_kinds: list(StepperBase.step_kind_model))
          : option(StepperBase.step_model) =>
    switch (step_kinds) {
    | [] => None
    | [kind] => Some(mk_test_step(~step_kind=kind, ()))
    | [kind, ...rest] =>
      Some(
        mk_test_step(~step_kind=kind, ~next_step=build_step_chain(rest), ()),
      )
    };

  // Create an induction case from a pattern, goal, and list of step_kinds
  let mk_case_with_steps =
      (
        ~pattern_str: string,
        ~goal_str: string,
        step_kinds: list(StepperBase.step_kind_model),
      )
      : Web.InductionCase.model'(StepperBase.step_model) => {
    let goal_exp = parse_exp(goal_str);
    let step =
      switch (build_step_chain(step_kinds)) {
      | Some(s) => s
      | None => StepperBase.init_step
      };
    mk_case(~pattern_str, ~inner_exp=goal_exp, ~step, ());
  };

  // Create an InductionStep with scrutinee and cases
  let mk_induction_with_cases =
      (
        ~scrut_str: string,
        ~cases: list(Web.InductionCase.model'(StepperBase.step_model)),
      )
      : StepperBase.step_kind_model => {
    // Create a complete InductionStep model directly by matching the structure
    // Since we can't easily access the base fields, we create everything from scratch
    let scrut_editor = mk_pattern_editor(scrut_str);

    let ind_model: Web.InductionStep.model'(StepperBase.step_model) = {
      scrut: scrut_editor,
      cases,
      elab_scrut_raw: Calc.Pending,
      elab_scrut_sub: Calc.Pending,
      scrut_ty: Calc.Pending,
      scrut_co_ctx: Calc.Pending,
      result: Calc.Pending,
      join_exp: Calc.Pending,
      is_exhaustive: Calc.Pending,
      validity: Calc.Pending,
    };

    (StepperBase.InductionStep(ind_model): StepperBase.step_kind_model);
  };
};

// Helper to run calculate with minimal dependencies
let test_calculate =
    (
      ~exp: Exp.t,
      ~ctx=Builtins.ctx_init(Some(Int)),
      ~ana=IdTagged.FreshGrammar.Typ.int(), // Use a fresh type for ana
      step: StepperBase.step_model,
    )
    : (StepperBase.step_model, Calc.t(Exp.t), Calc.t(option(bool))) => {
  let settings = Calc.NewValue(CoreSettings.on);
  let exp_calc = Calc.NewValue(exp);
  let ctx_calc =
    Calc.NewValue(SemanticCtx.of_ctx_and_env(ctx, Builtins.env_init));
  let ana_calc = Calc.NewValue(ana);

  StepperBase.Stepper.calculate(
    ~settings,
    ~exp=exp_calc,
    ~ctx=ctx_calc,
    ~ana=ana_calc,
    step,
  );
};

// Run a single step and return the result expression
let run_single_step = (~exp, ~step_kind) => {
  let step = mk_test_step(~step_kind, ());
  let (_, final_exp, _) = test_calculate(~exp, step);
  Calc.get_value(final_exp);
};

// Run a step chain and return the final result
let run_step_chain =
    (
      ~initial_exp,
      ~step_kinds: list(StepperBase.step_kind_model),
      ~ctx=?,
      (),
    ) => {
  switch (InductionCaseHelpers.build_step_chain(step_kinds)) {
  | Some(root_step) =>
    let (model, final_exp, validity) =
      test_calculate(~exp=initial_exp, ~ctx?, root_step);
    (model, Calc.get_value(final_exp), Calc.get_value(validity));
  | None => failwith("Empty step chain")
  };
};

let fac_stop_program = {|
debug eval($e) in
let fac : Int -> Int =
  fun n ->
    if n < 2 then 1 else n * fac(n - 1)
in
debug stop(fac($v)) in
fac(3)|};

let map_stop_program = {|
debug eval($e) in
let map : ([Int], Int -> Int) -> [Int] =
  fun (xs, f) ->
    case xs
    | [] => []
    | hd :: tl => f(hd) :: map((tl, f))
    end
in
let square : Int -> Int = fun x -> x * x in
debug stop(square($v)) in
map(([1, 2], square))|};

let stepper_ctx =
  SemanticCtx.of_ctx_and_env(Builtins.ctx_init(None), Builtins.closure_env);

let calculate_stepper_view = (~fresh, elab, model) =>
  Web.StepperView.Update.calculate(
    ~settings=Calc.OldValue(CoreSettings.on),
    ~ctx=Calc.OldValue(stepper_ctx),
    fresh ? Calc.NewValue(elab) : Calc.OldValue(elab),
    model,
  );

let update_stepper_view = (action, model) =>
  Web.StepperView.Update.update(
    ~settings=Web.Settings.Model.init,
    action,
    model,
  ).
    model;

let missing_available_steps = (m: Web.MissingStep.Model.t) =>
  m.next_steps
  |> Calc.get_saved_exc(~print="expected calculated missing step")
  |> (
    fun
    | EvaluatorStep.AutoStep(_) => []
    | EvaluatorStep.AvailableSteps(steps) => steps
  );

let action_at_deepest_available =
    (model: StepperBase.step_model): option(StepperBase.step_action) => {
  let action_at_current = (model: StepperBase.step_model) =>
    switch (model.step_kind) {
    | StepperBase.MissingStep(m) =>
      switch (missing_available_steps(m)) {
      | [_, ..._] => Some(StepperBase.StepForward(0))
      | [] => None
      }
    | _ => None
    };

  let rec loop = (model: StepperBase.step_model) =>
    switch (model.next_step) {
    | Some(next) =>
      switch (loop(next)) {
      | Some(action) => Some(StepperBase.NextStep(action))
      | None => action_at_current(model)
      }
    | None => action_at_current(model)
    };

  loop(model);
};

let apply_deepest_available_action = (model: Web.StepperView.Model.t) =>
  switch (action_at_deepest_available(model.root)) {
  | Some(action) => update_stepper_view(action, model)
  | None => Alcotest.fail("expected an available step")
  };

let stepper_pure_exp = elab =>
  elab |> Substitution.in_exp(Builtins.env_init) |> Exp.replace_all_ids;

let step_status_with_stepper_env = exp =>
  EvaluatorStep.get_status(
    ~settings=CoreSettings.on,
    exp,
    Builtins.closure_env,
  );

let rec count_available_steps_with_env = (~limit, ~exp, ~count) =>
  if (limit <= 0) {
    Alcotest.fail("step count exceeded limit");
  } else {
    switch (step_status_with_stepper_env(exp)) {
    | AutoStep(step) =>
      switch (EvaluatorStep.take_step(step)) {
      | None => count
      | Some(exp') =>
        count_available_steps_with_env(~limit=limit - 1, ~exp=exp', ~count)
      }
    | AvailableSteps(steps) =>
      switch (steps) {
      | [] => count
      | [step, ..._] =>
        switch (EvaluatorStep.take_step(step)) {
        | None => count
        | Some(exp') =>
          count_available_steps_with_env(
            ~limit=limit - 1,
            ~exp=exp',
            ~count=count + 1,
          )
        }
      }
    };
  };

/* ============================================================
   Locating a step again (hazelgrove/hazel#2331 and its Asc variant)

   When the stepper records a step it stores the redex together with its
   index among the expressions structurally equal to it
   (`EvaluatorStep.persist` → `ProofHacks.exp_idx`), and later finds it
   again by that index (`refresh_step` → `ProofHacks.nth_exp`). A wrapper
   node sitting directly on the redex (a filter, or an ascription inserted
   by evaluation) used to compare equal to the redex, so the search stopped
   at the wrapper and raised `exp_idx: e1 not found in e2`.
   ============================================================ */

let rec last_step_model = (m: StepperBase.step_model) =>
  switch (m.next_step) {
  | Some(next) => last_step_model(next)
  | None => m
  };

/* Like `action_at_deepest_available`, but takes the available step chosen
   by `pick` (given the number of available steps), so that steps other
   than the first, e.g. a second occurrence of a duplicated redex, are
   exercised too. */
let action_at_deepest_available_by =
    (~pick, model: StepperBase.step_model): option(StepperBase.step_action) => {
  let action_at_current = (model: StepperBase.step_model) =>
    switch (model.step_kind) {
    | StepperBase.MissingStep(m) =>
      switch (missing_available_steps(m)) {
      | [] => None
      | steps => Some(StepperBase.StepForward(pick(List.length(steps))))
      }
    | _ => None
    };
  let rec loop = (model: StepperBase.step_model) =>
    switch (model.next_step) {
    | Some(next) =>
      switch (loop(next)) {
      | Some(action) => Some(StepperBase.NextStep(action))
      | None => action_at_current(model)
      }
    | None => action_at_current(model)
    };
  loop(model);
};

let pick_first = _ => 0;
let pick_last = n => n - 1;

/* Drive the stepper view the way the UI does (take a visible step, then
   recalculate, which persists and refreshes every recorded step) until no
   visible step is left. Returns the final expression and the number of
   visible steps taken. Any `exp_idx`/`nth_exp` failure surfaces as an
   exception here. */
let run_stepper_view = (~limit=300, ~pick=pick_first, source) => {
  let elab = source |> parse_exp |> elaborate;
  let rec go = (taken, model: Web.StepperView.Model.t) =>
    switch (action_at_deepest_available_by(~pick, model.root)) {
    | None => (model, taken)
    | Some(_) when taken >= limit =>
      Alcotest.fail("stepper view did not finish within the step limit")
    | Some(action) =>
      model
      |> update_stepper_view(action)
      |> calculate_stepper_view(~fresh=false, elab)
      |> go(taken + 1)
    };
  let (model, taken) =
    Web.StepperView.Model.init
    |> calculate_stepper_view(~fresh=true, elab)
    |> go(0);
  let final =
    last_step_model(model.root).expr
    |> Calc.get_saved_exc(~print="expected a final expression");
  (DHExp.strip_ascriptions(final), taken);
};

/* Steps the view to the end (taking the first and, separately, the last
   available step each time) and checks the final value. */
let stepper_view_test = (~visible=?, name, source, expected) =>
  test_case(name, `Quick, () =>
    List.iter(
      ((which, pick)) => {
        let (final, taken) = run_stepper_view(~pick, source);
        check(
          dhexp_typ,
          "final result (" ++ which ++ " step each time)",
          parse_exp(expected),
          final,
        );
        switch (visible) {
        | Some(n) => check(int, "visible steps (" ++ which ++ ")", n, taken)
        | None => ()
        };
      },
      [("first", pick_first), ("last", pick_last)],
    )
  );

let steps_of_status: EvaluatorStep.status => list(EvaluatorStep.step) =
  fun
  | AutoStep(step) => [step]
  | AvailableSteps(steps) => steps;

/* The invariant `persist`/`refresh_step` rely on: for every step the
   evaluator offers, at every point of the trace, `persist` succeeds, the
   index `exp_idx` assigns to the redex leads `nth_exp` back to that same
   redex (by id), and `refresh_step` gives back the same step. */
let check_steps_relocatable = (~limit=500, source) => {
  let rec go = (remaining, exp) =>
    if (remaining <= 0) {
      Alcotest.fail(source ++ ": trace did not finish within the step limit");
    } else {
      let status = step_status_with_stepper_env(exp);
      let steps = steps_of_status(status);
      List.iter(
        (step: EvaluatorStep.step) => {
          let id = EvaluatorStep.get_step_id(step);
          /* `persist` searches the context-composed expression; it must
             not raise. */
          let p = EvaluatorStep.persist(step);
          let redex =
            switch (Exp.find_by_id(id, exp)) {
            | Some(redex) => redex
            | None => Alcotest.fail(source ++ ": step redex not in program")
            };
          let idx = ProofHacks.exp_idx(redex, exp);
          switch (ProofHacks.nth_exp(redex, idx, exp)) {
          | Some(found) =>
            check(
              bool,
              source ++ ": nth_exp(exp_idx) finds the redex by id",
              true,
              Exp.rep_id(found) == id,
            )
          | None => Alcotest.fail(source ++ ": nth_exp found no redex")
          };
          switch (
            EvaluatorStep.refresh_step(
              ~settings=CoreSettings.on,
              exp,
              Builtins.closure_env,
              p,
            )
          ) {
          | Some((_, step')) =>
            check(
              bool,
              source ++ ": refresh_step returns the same step",
              true,
              EvaluatorStep.get_step_id(step') == id,
            )
          | None => Alcotest.fail(source ++ ": refresh_step lost the step")
          };
        },
        steps,
      );
      switch (steps) {
      | [] => ()
      | [step, ..._] =>
        switch (EvaluatorStep.take_step(step)) {
        | None => ()
        | Some(exp') => go(remaining - 1, exp')
        }
      };
    };
  go(limit, source |> parse_exp |> elaborate |> stepper_pure_exp);
};

/* Issue #2331's program, completed so that it runs (the issue's `eval`
   had no pattern), in the `debug act(pat) in` syntax. Note the base case
   returns 2, so fac(3) = 3 * 2 * 2. */
let issue_2331_program = {|
debug eval($e) in
let fac : Int -> Int =
  fun n ->
    if n < 2 then 2 else n * fac(n - 1)
in
debug stop(fac($v)) in
fac(3)|};

/* The annotated recursive function raised in review of #2332: evaluation
   wraps each recursive call's result in an ascription, directly on the
   redex. */
let annotated_rec_program = "let f : Int -> Int = fun n -> if n < 1 then 0 else f(n - 1) in f(2)";

let relocation_corpus = [
  fac_stop_program,
  map_stop_program,
  issue_2331_program,
  annotated_rec_program,
  "let f : Int -> Int = fun n -> if n < 1 then 0 else f(n - 1) in debug stop(f($v)) in f(3)",
  "debug eval($e) in let f : Int -> Int = fun n -> if n < 1 then 0 else f(n - 1) in debug stop(f($v)) in f(3)",
  "debug eval($e) in let sum = fun n -> if n < 1 then 0 else n + sum(n - 1) in debug stop(sum($v)) in sum(3)",
  "let g : Int -> Int = fun n -> (n + 1 : Int) in g((g(1) : Int))",
  "((1 + 2 : Int) : Int) + ((1 + 2 : Int) : Int)",
  "(((1 + 2 : Int) : Int) : Int)",
  "((fun x -> x + 1 : Int -> Int) : Int -> Int)(1 + 1)",
  "(([1 + 1, 2] : [Int]) : [Int])",
  "let h : Int -> Int = fun n -> ((n : Int) : Int) in (h(h(1)) : Int)",
  "let x : Int = 1 + 2 in x + x",
  "([1 + 1, 2] : [Int])",
  "debug stop(1 + 2) in (1 + 2) * (1 + 2)",
  "debug eval($e) in debug stop($e + $e) in (1 + 2) + (1 + 2)",
  "debug step($e) in debug eval(1 + 2) in (1 + 2) + (1 + 2)",
  "debug hide($e + 1) in (fun x -> x + 1)(2) + (fun x -> x + 1)(2)",
  "(2 + 3) + (debug stop($e) in 2 + 3)",
  "(2 + 3) + (debug hide($e) in 2 + 3)",
  "(2 + 3) + (debug step($e) in 2 + 3)",
  "(2 + 3) + (debug eval($e) in 2 + 3)",
  "(2 + 3) + (debug eval($e) in debug stop($e) in 2 + 3)",
  "debug eval($e) in debug step($e * $e) in debug stop($e + $e) in (1 + 2) * (3 + 4)",
  "(1 + 2) + (debug stop($e) in (1 + 2) + (1 + 2))",
  "debug 1 in (1 + 2) + (1 + 2)",
];

/* Unit-level form of the same invariant, on hand-built trees where a
   wrapper sits directly on the target. `mk_target()` builds a fresh copy of
   the target (same structure, new ids); `mk(target)` places one copy as the
   target inside the whole expression, alongside further fresh copies. */
let relocation_case = (name, ~expected_idx, ~mk_target, mk) =>
  test_case(
    name,
    `Quick,
    () => {
      let target = mk_target();
      let whole = mk(target);
      let idx = ProofHacks.exp_idx(target, whole);
      check(int, "exp_idx", expected_idx, idx);
      switch (ProofHacks.nth_exp(target, idx, whole)) {
      | Some(found) =>
        check(
          bool,
          "nth_exp finds the target by id",
          true,
          Exp.rep_id(found) == Exp.rep_id(target),
        )
      | None => Alcotest.fail("nth_exp found nothing")
      };
      /* `nth_exp` is also used on a copy of the expression with new ids
         (`refresh_step` after recalculation): the same index must find a
         node with the same shape. */
      switch (ProofHacks.nth_exp(target, idx, Exp.replace_all_ids(whole))) {
      | Some(found) =>
        check(
          bool,
          "nth_exp on a re-id'd copy finds a node identical to the target",
          true,
          Equality.syntactic.exp(found, target),
        )
      | None => Alcotest.fail("nth_exp found nothing in the re-id'd copy")
      };
    },
  );

/* `copy() + x + copy()`: one copy on each side, so the target's index is 1
   whichever order the tree is walked in. */
let between_copies = (copy, x) =>
  Exp.fresh(
    BinOp(Int(Plus), Exp.fresh(BinOp(Int(Plus), copy(), x)), copy()),
  );
let int_exp = n => Exp.fresh(Atom(Int(Bigint.of_int(n))));
let plus_exp = (a, b) => Exp.fresh(BinOp(Int(Plus), a, b));
let two_plus_three = () => plus_exp(int_exp(2), int_exp(3));
let asc_int = e => Exp.fresh(Asc(e, IdTagged.FreshGrammar.Typ.int()));
let user_filter = (act, e) =>
  Exp.fresh(
    Filter(
      Filter({
        act,
        pat: Exp.fresh(FilterSelector(FilterSelector.Exp)),
        ids: IdTagged.IdTag.fresh(),
      }),
      e,
    ),
  );

let tests = (
  "StepperBase",
  [
    // ============================================================
    // Basic calculation tests
    // ============================================================
    test_case(
      "calculate empty missing step",
      `Quick,
      () => {
        let exp = parse_exp("1 + 2");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, final_exp, _) = test_calculate(~exp, step);
        check(
          dhexp_typ,
          "expression unchanged",
          exp,
          Calc.get_value(final_exp),
        );
      },
    ),
    test_case(
      "nth_exp can find residue filter target",
      `Quick,
      () => {
        let target =
          Exp.fresh(
            Filter(
              Residue(1, (FilterAction.Eval, FilterAction.All)),
              Exp.fresh(Atom(Int(Bigint.of_int(18)))),
            ),
          );
        let exp =
          Exp.fresh(
            Filter(
              Filter({
                act: (FilterAction.Eval, FilterAction.All),
                pat: Exp.fresh(FilterSelector(FilterSelector.Exp)),
                ids: IdTagged.IdTag.fresh(),
              }),
              target,
            ),
          );
        check(
          int,
          "residue target index",
          0,
          ProofHacks.exp_idx(target, exp),
        );
        switch (ProofHacks.nth_exp(target, 0, exp)) {
        | Some(found) =>
          check(
            bool,
            "found residue target by id",
            true,
            Exp.rep_id(found) == Exp.rep_id(target),
          )
        | None => Alcotest.fail("expected nth_exp to find residue target")
        };
      },
    ),
    relocation_case(
      "exp_idx/nth_exp: user filter on the target, between equal copies",
      ~expected_idx=1,
      ~mk_target=two_plus_three,
      target =>
      between_copies(
        two_plus_three,
        user_filter((FilterAction.Step, FilterAction.One), target),
      )
    ),
    relocation_case(
      "exp_idx/nth_exp: nested user filters on the target",
      ~expected_idx=1,
      ~mk_target=two_plus_three,
      target =>
      between_copies(
        two_plus_three,
        user_filter(
          (FilterAction.Eval, FilterAction.All),
          user_filter((FilterAction.Step, FilterAction.One), target),
        ),
      )
    ),
    relocation_case(
      "exp_idx/nth_exp: ascription on the target, between ascribed copies",
      ~expected_idx=1,
      ~mk_target=two_plus_three,
      target =>
      between_copies(() => asc_int(two_plus_three()), asc_int(target))
    ),
    relocation_case(
      "exp_idx/nth_exp: ascription and filter on the target",
      ~expected_idx=0,
      ~mk_target=two_plus_three,
      target =>
      asc_int(
        user_filter((FilterAction.Step, FilterAction.All), asc_int(target)),
      )
    ),
    relocation_case(
      "exp_idx/nth_exp: target is an ascription under an ascription",
      ~expected_idx=0,
      ~mk_target=() => asc_int(two_plus_three()),
      target => asc_int(target),
    ),
    relocation_case(
      "exp_idx/nth_exp: target is an ascription between doubly ascribed copies",
      ~expected_idx=1,
      ~mk_target=() => asc_int(two_plus_three()),
      target =>
        between_copies(() => asc_int(asc_int(two_plus_three())), target),
    ),
    test_case(
      "taking recursive stop step does not overflow",
      `Quick,
      () => {
        /* Regression test for hazelgrove/hazel#2331: driving the stepper
           view through the recursive stop steps used to crash inside
           ProofHacks.exp_idx (the step target could not be located in the
           Residue-wrapped expression). The three rounds below must
           complete without raising; apply_deepest_available_action also
           fails the test if an expected step is missing. */
        let elab = fac_stop_program |> parse_exp |> elaborate;
        let pure_count =
          count_available_steps_with_env(
            ~limit=1000,
            ~exp=stepper_pure_exp(elab),
            ~count=0,
          );
        check(int, "pure evaluator visible recursive stops", 3, pure_count);
        let model =
          Web.StepperView.Model.init
          |> calculate_stepper_view(~fresh=true, elab);
        let model =
          model
          |> apply_deepest_available_action
          |> calculate_stepper_view(~fresh=false, elab);
        let model =
          model
          |> apply_deepest_available_action
          |> calculate_stepper_view(~fresh=false, elab);
        let model =
          model
          |> apply_deepest_available_action
          |> calculate_stepper_view(~fresh=false, elab);
        /* After the three visible stops everything left is hidden by
           `debug eval($e) in`, so the stepper must agree with the pure
           evaluator that no further step is available. */
        check(
          bool,
          "no visible step remains after the three stops",
          true,
          action_at_deepest_available(model.root) == None,
        );
      },
    ),
    test_case(
      "recursive stop step under type ascription does not crash",
      `Quick,
      () => {
        /* Regression test for the Asc variant of hazelgrove/hazel#2331:
           the `let map : ... =` annotation makes evaluation distribute an
           ascription over the recursive application, producing
           Asc(Ap(...), _) nodes that are structurally equal (under
           ignore_ascriptions) to the Ap redex they enclose.
           ProofHacks.exp_idx used to anchor the occurrence at the Asc
           wrapper and never reach the redex underneath, raising Failure
           while the stepper view persisted its hidden steps. */
        let elab = map_stop_program |> parse_exp |> elaborate;
        let pure_count =
          count_available_steps_with_env(
            ~limit=2000,
            ~exp=stepper_pure_exp(elab),
            ~count=0,
          );
        check(int, "pure evaluator visible stops", 2, pure_count);
        let model =
          Web.StepperView.Model.init
          |> calculate_stepper_view(~fresh=true, elab);
        let model =
          model
          |> apply_deepest_available_action
          |> calculate_stepper_view(~fresh=false, elab);
        let model =
          model
          |> apply_deepest_available_action
          |> calculate_stepper_view(~fresh=false, elab);
        check(
          bool,
          "no visible step remains after the two stops",
          true,
          action_at_deepest_available(model.root) == None,
        );
      },
    ),
    // ============================================================
    // Stepping to the end through the stepper view (#2331, #2332)
    // ============================================================
    stepper_view_test(
      ~visible=3,
      "recursive stop under eval steps to the end (#2332)",
      fac_stop_program,
      "6",
    ),
    stepper_view_test(
      ~visible=3,
      "issue #2331 program steps to the end",
      issue_2331_program,
      "12",
    ),
    stepper_view_test(
      ~visible=2,
      "annotated map with stop steps to the end",
      map_stop_program,
      "[1, 4]",
    ),
    stepper_view_test(
      "annotated recursive function steps to the end (Asc on the redex)",
      annotated_rec_program,
      "0",
    ),
    stepper_view_test(
      ~visible=4,
      "stop filter on an annotated recursive function steps to the end",
      "debug eval($e) in let f : Int -> Int = fun n -> if n < 1 then 0 else f(n - 1) in debug stop(f($v)) in f(3)",
      "0",
    ),
    stepper_view_test(
      "nested annotations around duplicated redexes",
      "((1 + 2 : Int) : Int) + ((1 + 2 : Int) : Int)",
      "6",
    ),
    stepper_view_test(
      "annotated body and argument",
      "let g : Int -> Int = fun n -> (n + 1 : Int) in g((g(1) : Int))",
      "3",
    ),
    stepper_view_test(
      ~visible=3,
      "debug stop directly around a redex with an equal sibling",
      "(2 + 3) + (debug stop($e) in 2 + 3)",
      "10",
    ),
    stepper_view_test(
      ~visible=2,
      "debug hide directly around a redex with an equal sibling",
      "(2 + 3) + (debug hide($e) in 2 + 3)",
      "10",
    ),
    stepper_view_test(
      ~visible=3,
      "debug step directly around a redex with an equal sibling",
      "(2 + 3) + (debug step($e) in 2 + 3)",
      "10",
    ),
    stepper_view_test(
      ~visible=2,
      "debug eval directly around a redex with an equal sibling",
      "(2 + 3) + (debug eval($e) in 2 + 3)",
      "10",
    ),
    stepper_view_test(
      "nested filters directly around a redex with an equal sibling",
      "(2 + 3) + (debug eval($e) in debug stop($e) in 2 + 3)",
      "10",
    ),
    stepper_view_test(
      "nested filters of three actions",
      "debug eval($e) in debug step($e * $e) in debug stop($e + $e) in (1 + 2) * (3 + 4)",
      "21",
    ),
    stepper_view_test(
      "same expression twice under a stop filter",
      "debug stop(1 + 2) in (1 + 2) * (1 + 2)",
      "9",
    ),
    stepper_view_test(
      "same expression twice under a filter, with an equal copy outside",
      "(1 + 2) + (debug stop($e) in (1 + 2) + (1 + 2))",
      "9",
    ),
    stepper_view_test(
      ~visible=1,
      "an unresolved filter does not get in the way",
      "debug 1 in 1 + 2",
      "3",
    ),
    test_case(
      "every offered step can be located again (filters and ascriptions)",
      `Quick,
      () =>
      List.iter(check_steps_relocatable, relocation_corpus)
    ),
    // ============================================================
    // SingleStep tests
    // ============================================================
    test_case(
      "single step: simple arithmetic",
      `Quick,
      () => {
        let exp = parse_exp("1 + 2");
        let result =
          run_single_step(
            ~exp,
            ~step_kind=StepKindHelpers.mk_auto_single_step(exp),
          );
        check(dhexp_typ, "1 + 2 -> 3", parse_exp("3"), result);
      },
    ),
    test_case(
      "single step: beta reduction",
      `Quick,
      () => {
        let exp =
          Exp.fresh(
            Ap(
              Forward,
              Exp.fresh(
                Fun(
                  Pat.fresh(Var("x")),
                  Exp.fresh(
                    BinOp(
                      Int(Plus),
                      Exp.fresh(Var("x")),
                      Exp.fresh(Atom(Int(Bigint.of_int(1)))),
                    ),
                  ),
                  None,
                  None,
                ),
              ),
              Exp.fresh(Atom(Int(Bigint.of_int(5)))),
            ),
          );
        let elab = elaborate(exp);
        let result =
          run_single_step(
            ~exp=elab,
            ~step_kind=StepKindHelpers.mk_auto_single_step(elab),
          );
        // After beta reduction, should have 5 + 1
        let expected_pattern = parse_exp("5 + 1");
        check(
          dhexp_typ,
          "(fun x -> x + 1)(5) reduces",
          expected_pattern,
          result,
        );
      },
    ),
    test_case(
      "single step: let binding",
      `Quick,
      () => {
        let exp = parse_exp("let x = 5 in x + 1");
        let elab = elaborate(exp);
        let result =
          run_single_step(
            ~exp=elab,
            ~step_kind=StepKindHelpers.mk_auto_single_step(elab),
          );
        // After one step, should substitute x
        let expected = parse_exp("5 + 1");
        check(dhexp_typ, "let x = 5 in x + 1 -> 5 + 1", expected, result);
      },
    ),
    // ============================================================
    // Validity tests
    // ============================================================
    test_case(
      "validity: true expression",
      `Quick,
      () => {
        let exp = parse_exp("true");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, _, validity) = test_calculate(~exp, step);
        check(
          option(bool),
          "true is valid",
          Some(true),
          Calc.get_value(validity),
        );
      },
    ),
    test_case(
      "validity: false expression",
      `Quick,
      () => {
        let exp = parse_exp("false");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, _, validity) = test_calculate(~exp, step);
        check(
          option(bool),
          "false is refutable",
          Some(false),
          Calc.get_value(validity),
        );
      },
    ),
    test_case(
      "validity: unknown expression",
      `Quick,
      () => {
        let exp = parse_exp("42");
        let step =
          mk_test_step(~step_kind=StepKindHelpers.mk_missing_step(), ());
        let (_, _, validity) = test_calculate(~exp, step);
        check(
          option(bool),
          "42 is not a boolean",
          None,
          Calc.get_value(validity),
        );
      },
    ),
    test_case(
      "validity: single step to true",
      `Quick,
      () => {
        let exp = parse_exp("1 == 1");
        let elab = elaborate(exp);
        let step_kind = StepKindHelpers.mk_auto_single_step(elab);
        let step = mk_test_step(~step_kind, ());
        let (_, final_exp, validity) = test_calculate(~exp=elab, step);
        // After evaluation, 1 == 1 should become true
        check(
          dhexp_typ,
          "1 == 1 evaluates to true",
          parse_exp("true"),
          Calc.get_value(final_exp),
        );
        // But validity on a SingleStep checks the *next* expression
        // So we need to check the model's proof_validity field
        check(
          option(bool),
          "validity is Some(true)",
          Some(true),
          Calc.get_value(validity),
        );
      },
    ),
    // ============================================================
    // Step Chain tests
    // ============================================================
    test_case(
      "step chain: two arithmetic steps",
      `Quick,
      () => {
        let exp = parse_exp("1 + 2 + 3");
        let elab = elaborate(exp);
        // First step: 1 + 2 -> 3
        // Second step: 3 + 3 -> 6
        let step1 = StepKindHelpers.mk_auto_single_step(elab);
        let after_first = run_single_step(~exp=elab, ~step_kind=step1);
        let step2 = StepKindHelpers.mk_auto_single_step(after_first);

        let (_, final_exp, _) =
          run_step_chain(~initial_exp=elab, ~step_kinds=[step1, step2], ());
        check(dhexp_typ, "1 + 2 + 3 -> 6", parse_exp("6"), final_exp);
      },
    ),
    test_case(
      "step chain: validity propagation true",
      `Quick,
      () => {
        // Chain that ends in true should have validity Some(true)
        // Use a simpler expression that requires fewer steps
        let exp = parse_exp("1 == 1");
        let elab = elaborate(exp);

        // Take one step: 1 == 1 -> true
        let step1 = StepKindHelpers.mk_auto_single_step(elab);

        let (_, final_exp, validity) =
          run_step_chain(~initial_exp=elab, ~step_kinds=[step1], ());

        check(dhexp_typ, "reduces to true", parse_exp("true"), final_exp);
        check(option(bool), "chain validity is true", Some(true), validity);
      },
    ),
    // ============================================================
    // Equality tests
    // ============================================================
    test_case(
      "equality: expression with same structure",
      `Quick,
      () => {
        // Test that two expressions with same structure but different IDs
        // are considered equal for validity purposes
        let exp1 = parse_exp("true");
        let exp2 = parse_exp("true");
        // They should be equal according to DHExp.fast_equal
        check(
          bool,
          "structurally equal",
          true,
          DHExp.fast_equal(exp1, exp2),
        );
      },
    ),
    // ============================================================
    // InductionStep tests
    // ============================================================
    test_case(
      "induction step: create with scrutinee",
      `Quick,
      () => {
        // Test that we can create an InductionStep with a scrutinee
        let scrut_exp = parse_exp("n");
        let step_kind = StepKindHelpers.mk_induction_step(~exp=scrut_exp, ());
        let step = mk_test_step(~step_kind, ());

        // Just verify it calculates without crashing
        let (_, _, _) = test_calculate(~exp=parse_exp("true"), step);
        // If we get here, the test passed
        check(bool, "induction step created", true, true);
      },
    ),
    test_case(
      "induction step: exhaustiveness check empty",
      `Quick,
      () => {
        // An InductionStep with no cases should not be exhaustive
        let scrut_exp = parse_exp("true");
        let elab = elaborate(scrut_exp);
        let step_kind = StepKindHelpers.mk_induction_step(~exp=elab, ());
        let step = mk_test_step(~step_kind, ());

        let (model, _, _) = test_calculate(~exp=parse_exp("true"), step);

        // Extract the InductionStep from the model to check exhaustiveness
        switch (model.step_kind) {
        | StepperBase.InductionStep(ind_model) =>
          let is_exhaustive =
            ind_model.is_exhaustive
            |> Calc.get_saved_exc(~print="is_exhaustive not calculated");
          check(bool, "empty induction not exhaustive", false, is_exhaustive);
        | _ => failwith("Expected InductionStep")
        };
      },
    ),
    test_case(
      "induction step: validity with no cases on concrete type",
      `Quick,
      () => {
        // An InductionStep with no cases on a concrete type (like Int) should be None (not valid)
        // because it's not exhaustive
        let scrut_exp = parse_exp("42"); // Int type - concrete
        let elab = elaborate(scrut_exp);
        let step_kind = StepKindHelpers.mk_induction_step(~exp=elab, ());
        let step = mk_test_step(~step_kind, ());

        let (model, _, _) = test_calculate(~exp=parse_exp("true"), step);

        // Check that validity is None - concrete type with no cases is not valid
        switch (model.step_kind) {
        | StepperBase.InductionStep(ind_model) =>
          let ind_validity =
            ind_model.validity
            |> Calc.get_saved_exc(~print="validity not calculated");
          check(
            option(bool),
            "no cases on concrete type is not valid",
            None,
            ind_validity,
          );
        | _ => failwith("Expected InductionStep")
        };
      },
    ),
    test_case(
      "induction step: calculate result expression",
      `Quick,
      () => {
        // An InductionStep should calculate a result expression (join_exp)
        let scrut_exp = parse_exp("n");
        let step_kind = StepKindHelpers.mk_induction_step(~exp=scrut_exp, ());
        let step = mk_test_step(~step_kind, ());

        let (model, _, _) = test_calculate(~exp=parse_exp("true"), step);

        // Check that result and join_exp are calculated
        switch (model.step_kind) {
        | StepperBase.InductionStep(ind_model) =>
          let result = ind_model.result |> Calc.get_saved_opt;
          let join_exp = ind_model.join_exp |> Calc.get_saved_opt;
          // Both should be calculated (even if the result might be an error)
          check(bool, "result is calculated", true, Option.is_some(result));
          check(
            bool,
            "join_exp is calculated",
            true,
            Option.is_some(join_exp),
          );
        | _ => failwith("Expected InductionStep")
        };
      },
    ),
    // Complete proof of xs == xs by induction with proof steps
    test_case(
      "induction step: complete proof xs == xs",
      `Quick,
      () => {
        // Build induction proof: for all xs, xs == xs
        // The inductive case generates an inductive hypothesis "ih: tl == tl"
        // by substituting xs->tl in the goal "xs == xs".
        // Base case: [] == [] (by reflexivity)
        // Inductive case: hd::tl == hd::tl (using reflexivity, with IH available)
        // Note: The IH exists and could be used in a more complex proof
        let step =
          mk_test_step(
            ~step_kind=
              InductionCaseHelpers.mk_induction_with_cases(
                ~scrut_str="xs",
                ~cases=[
                  InductionCaseHelpers.mk_case_with_steps(
                    ~pattern_str="[]",
                    ~goal_str="[] == []",
                    [
                      StepKindHelpers.mk_axiom_step(
                        ~name="Reflexive(==)",
                        ~equality="Reflexive(==)",
                        parse_exp("[] == []"),
                      ),
                    ],
                  ),
                  InductionCaseHelpers.mk_case_with_steps(
                    ~pattern_str="hd::tl",
                    ~goal_str="hd::tl == hd::tl",
                    [
                      StepKindHelpers.mk_axiom_step(
                        ~name="ih",
                        ~equality="ih",
                        parse_exp("tl"),
                      ),
                      StepKindHelpers.mk_axiom_step(
                        ~name="Reflexive(==)",
                        ~equality="Reflexive(==)",
                        parse_exp("hd::tl == hd::tl"),
                      ),
                    ],
                  ),
                ],
              ),
            (),
          );

        // Run calculate on the entire proof structure
        // Need to add xs to context with List type for IH generation to work
        let xs_entry: Ctx.entry =
          VarEntry({
            name: "xs",
            id: Id.invalid,
            typ: List(Atom(Int) |> Typ.temp) |> Typ.temp,
            custom_statics: None,
          });
        let ctx_with_xs =
          Ctx.extend(Builtins.ctx_init(Some(Int)), xs_entry);
        let (model, _, _) =
          test_calculate(~exp=parse_exp("xs == xs"), ~ctx=ctx_with_xs, step);

        // Verify the proof
        switch (model.step_kind) {
        | InductionStep({cases, is_exhaustive, validity, _}) =>
          check(int, "should have 2 cases", 2, List.length(cases));
          check(
            bool,
            "should be exhaustive",
            true,
            is_exhaustive
            |> Calc.get_saved_opt
            |> Option.value(~default=false),
          );

          // Verify inductive hypothesis IS generated
          let inductive_case = List.nth(cases, 1);
          let hypotheses =
            inductive_case.Web.InductionCase.hypotheses
            |> Calc.get_saved_opt
            |> Option.value(~default=[]);

          check(
            int,
            "inductive case should have 2 hypotheses (case_eq and ih)",
            2,
            List.length(hypotheses),
          );

          let has_ih =
            List.exists(
              ((binding, _)) => binding.Binding.name == "ih",
              hypotheses,
            );
          check(bool, "should have 'ih' hypothesis", true, has_ih);

          // Proof should be valid with reflexivity steps
          check(
            option(option(bool)),
            "proof should be valid",
            Some(Some(true)),
            validity |> Calc.get_saved_opt,
          );
        | _ => Alcotest.fail("Expected InductionStep")
        };
      },
    ),
  ],
);
