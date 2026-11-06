open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Util;
open Haz3lcore;

module StepperBase = Web.StepperBase;

let mk_induction_step =
    (~exp: option(Exp.t)=?, ()): StepperBase.step_kind_model =>
  StepperBase.InductionStep(Web.InductionStep.init(~exp?, ()));

// Cell/no-proof sentinel used by Stepper.calculate
let empty_hole_proof = (): Calc.t(Proof.t) =>
  Calc.OldValue(Proof.fresh(EmptyHole));

// Helper to create a minimal step_model for testing
let mk_test_step_model =
    (
      ~step_kind: StepperBase.step_kind_model,
      ~next_step=StepperBase.init_step,
      ~proof=Calc.Pending,
      (),
    )
    : StepperBase.step_model => {
  entry: Calc.Pending,
  picker: Web.MissingStep.Model.init,
  step_kind,
  next_step,
  proof,
};

// Wrap a full step as the root next_step
let mk_test_step =
    (
      ~step_kind: StepperBase.step_kind_model,
      ~next_step=StepperBase.init_step,
      (),
    )
    : StepperBase.next_step =>
  StepperBase.NextStep(mk_test_step_model(~step_kind, ~next_step, ()));

let mk_missing_step = (): StepperBase.next_step =>
  StepperBase.MissingStep(Web.MissingStep.Model.init, StepperBase.Finished);

// Helper to run calculate with minimal dependencies.
// Returns one next_step; outgoing/validity come from ProofMap, not the return.
let test_calculate =
    (
      ~exp: Exp.t,
      ~ctx=Builtins.ctx_init(Some(Int)),
      ~env=Builtins.env_init,
      ~ana=IdTagged.FreshGrammar.Typ.int(),
      ~proof=empty_hole_proof(),
      ~proof_map=Calc.OldValue(Language.ProofMap.empty),
      model: StepperBase.next_step,
    )
    : StepperBase.next_step => {
  let settings = Calc.NewValue(CoreSettings.on);
  let exp_calc = Calc.NewValue(exp);
  let ctx_calc = Calc.NewValue(SemanticCtx.of_ctx_and_env(ctx, env));
  let ana_calc = Calc.NewValue(ana);

  StepperBase.Stepper.calculate(
    ~settings,
    ~exp=exp_calc,
    ~ctx=ctx_calc,
    ~ana=ana_calc,
    ~proof,
    ~proof_map,
    model,
  );
};

let is_missing_step = (ns: StepperBase.next_step): bool =>
  StepperBase.is_missing_step(ns);

// A proof sub-term describing one real (axiom) step over `at_exp`
let axiom_step_proof = (~equality="refl_eq", at_exp: Exp.t): Calc.t(Proof.t) =>
  Calc.NewValue(
    Proof.fresh(
      AxiomStep({
        at_idx: Exp.fresh(Atom(Int(Bigint.of_int(0)))),
        at_exp,
        direction: Direction.Right,
        equality: Exp.fresh(Var(equality)),
        instantiation: None,
      }),
    ),
  );

/* ---- Checked programs -------------------------------------------- */

/* Run a program through the checker and hand back its theorem proof and
   proof map, the two inputs a wrapping form's row sources from. */
let checked_proof = (src: string): (Proof.t, Language.ProofMap.t) => {
  let (state, _, elab) = Test_ProofMap.eval_with_proof(parse_exp(src));
  let proof =
    switch (Test_ProofMap.find_theorem_proof(elab)) {
    | Some(p) => p
    | None => Alcotest.fail("no theorem proof in: " ++ src)
    };
  (proof, EvaluatorState.get_proof_map(state));
};

/* Bind `x:Int` in the row's scope. `generalize` only re-quantifies a bare
   in-scope variable (otherwise ProofCheck marks it MalformedGeneralize and
   passes through), and in the app that binding comes from the theorem
   statement's peeled binders — which this unit-level ctx has to stand in
   for. */
let ctx_with_x =
  Ctx.extend(
    Builtins.ctx_init(Some(Int)),
    Ctx.VarEntry({
      name: "x",
      id: Id.mk(),
      typ: IdTagged.FreshGrammar.Typ.int(),
      custom_statics: None,
    }),
  );

/* Promote a MissingStep row against a checked program's proof. */
let promote =
    (~src: string, ~goal: string, ~ctx=ctx_with_x, ()): StepperBase.next_step => {
  let (proof, pm) = checked_proof(src);
  test_calculate(
    ~exp=parse_exp(goal),
    ~ctx,
    ~proof=Calc.NewValue(proof),
    ~proof_map=Calc.NewValue(pm),
    mk_missing_step(),
  );
};

/* The facts a step's inner scope offers, by name: the entries of the
   theorem namespace. */
let facts_of_ctx = (ctx: SemanticCtx.t): list((string, Exp.t)) =>
  SemanticCtx.facts(ctx) |> List.map(((name, _id, fact)) => (name, fact));

/* Every tile id in a segment, children and folded projector interiors
   included (cf. test/Test_EditorTransform.re, which stops at
   projectors). A chip's syntax is still measured, so a repeated id in
   there is the shard-mismatch crash in latent form: `Highlight.of_tile`
   demands exactly one measurement per shard. */
let rec segment_tile_ids = (seg: Segment.t): list(Id.t) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) => [
          Piece.id(p),
          ...segment_tile_ids(List.concat(t.children)),
        ]
      | Projector(pr) => segment_tile_ids([pr.syntax])
      | Grout(_)
      | Secondary(_) => []
      },
    seg,
  );

let duplicated = (ids: list(Id.t)): list(Id.t) => {
  let seen = Hashtbl.create(64);
  List.filter(
    id =>
      switch (Hashtbl.find_opt(seen, id)) {
      | Some () => true
      | None =>
        Hashtbl.add(seen, id, ());
        false;
      },
    ids,
  );
};

/* The last MissingStep row in a chain: the synthesized hole after every
   written step. */
let rec last_missing_step =
        (ns: StepperBase.next_step): option(Web.MissingStep.Model.t) =>
  switch (ns) {
  | StepperBase.MissingStep(m, tail) =>
    switch (last_missing_step(tail)) {
    | Some(_) as later => later
    | None => Some(m)
    }
  | StepperBase.NextStep({next_step, _}) => last_missing_step(next_step)
  | StepperBase.Finished => None
  };

let saved_exc = (~print: string, x: Calc.saved('a)): 'a =>
  switch (x) {
  | Calc.Calculated(v) => v
  | Calc.Pending => Alcotest.fail(print ++ " not calculated")
  };

/* ---- Insertion round-trip helpers (cf. test/Test_EditorTransform.re) -- */

let parse_zipper = (s: string): Zipper.t =>
  switch (Parser.to_zipper(s, ~root=Exp)) {
  | Some(z) => z
  | None => Alcotest.fail("failed to parse zipper: " ++ s)
  };

/* Write `term` over the theorem's (hole) proof, exactly as the
   step-picker's ProofPatch does for a MissingStep row whose backing proof
   is a hole (StepperBase.Patches.add_step with a Replace target). Returns the
   patched zipper so callers can also ask where the caret can land. */
let insert_proof_zipper = (~src: string, term: TermBase.Proof.term): Zipper.t => {
  let z = parse_zipper(src);
  let root = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let target_id =
    switch (Test_ProofMap.find_theorem_proof(root)) {
    | Some(p) => Proof.rep_id(p)
    | None => Alcotest.fail("no theorem proof in: " ++ src)
    };
  EditorTransform.apply_patch(
    z,
    EditorTransform.mk_proof_patch(~target_id, Proof.fresh(term)),
  );
};

let insert_proof_term = (~src: string, term: TermBase.Proof.term): string =>
  insert_proof_zipper(~src, term) |> Printer.of_zipper(~holes="?");

let contains = (haystack: string, needle: string): bool => {
  let hl = String.length(haystack);
  let nl = String.length(needle);
  let rec go = i =>
    i + nl <= hl && (String.sub(haystack, i, nl) == needle || go(i + 1));
  nl == 0 || go(0);
};

let check_contains = (~msg: string, haystack: string, needle: string) =>
  check(
    bool,
    msg ++ " — expected to find:\n" ++ needle ++ "\nin:\n" ++ haystack,
    true,
    contains(haystack, needle),
  );

/* Which rows a proof becomes, and what a recalculation keeps. */
module Calculate = {
  let tests = [
    test_case(
      "after an eval step the trailing row offers the next redex",
      `Quick,
      () => {
        /* One written eval step on `1 + 4 == 5`: the stepper must
           synthesize a trailing MissingStep row for the `5 == 5` goal
           and offer its `==` redex as a clickable eval step. */
        let src = "theorem thm = 1 + 4 == 5 proof eval 1 + 4 at 0 end in thm";
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let result =
          test_calculate(
            ~exp=parse_exp("1 + 4 == 5"),
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: EvalStep(_), next_step, _}) =>
          switch (next_step) {
          | StepperBase.MissingStep(m, _) =>
            switch (m.next_steps |> Calc.get_saved_opt) {
            | Some(EvaluatorStep.AvailableSteps([_])) => ()
            | Some(EvaluatorStep.AvailableSteps(steps)) =>
              Alcotest.fail(
                "expected one available step, got "
                ++ string_of_int(List.length(steps)),
              )
            | Some(EvaluatorStep.AutoStep(_)) =>
              Alcotest.fail("next redex unexpectedly auto-stepped")
            | None => Alcotest.fail("next steps not calculated")
            }
          | StepperBase.Finished =>
            Alcotest.fail("row is Finished; goal not yet discharged")
          | StepperBase.NextStep(_) =>
            Alcotest.fail("unexpected synthesized NextStep")
          }
        | _ => Alcotest.fail("expected NextStep(EvalStep) row")
        };
      },
    ),
    test_case(
      "recalculate with old inputs preserves step editor state",
      `Quick,
      () => {
        /* Editors in step rows carry live UI state (caret, selection).
           A calculate pass whose inputs are all old must not rebuild
           them — rebuilding snaps the caret to a fresh editor's end
           and makes selection in step rows impossible. */
        let src = "theorem thm = 1 + 4 == 5 proof eval 1 + 4 at 0 end in thm";
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let goal = parse_exp("1 + 4 == 5");
        let calculated =
          test_calculate(
            ~exp=goal,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        /* Move the caret in the full step row's editor (fresh editors
           sit at the end, so Start is an observable change). */
        let moved =
          StepperBase.Stepper.update(
            ~settings=Web.Settings.Model.init,
            StepperBase.EditorAction(Web.CodeSelectable.Update.Move(Start)),
            calculated,
          ).
            model;
        /* Recalculate with everything marked old (a pure-UI pass). */
        let recalc =
          StepperBase.Stepper.calculate(
            ~settings=Calc.OldValue(CoreSettings.on),
            ~exp=Calc.OldValue(goal),
            ~ctx=
              Calc.OldValue(
                SemanticCtx.of_ctx_and_env(
                  Builtins.ctx_init(Some(Operators.default_mode)),
                  Builtins.env_init,
                ),
              ),
            ~ana=Calc.OldValue(IdTagged.FreshGrammar.Typ.int()),
            ~proof=Calc.OldValue(proof),
            ~proof_map=Calc.OldValue(pm),
            moved,
          );
        let editor_of = (ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.NextStep({picker, _}) =>
            picker.editor |> Calc.get_saved_opt
          | _ => None
          };
        switch (editor_of(moved), editor_of(recalc)) {
        | (Some(before), Some(after)) =>
          check(
            bool,
            "step editor survives an old-input recalculate",
            true,
            before === after,
          )
        | _ => Alcotest.fail("expected a step editor before and after")
        };
        /* The synthesized trailing row must also keep its model. */
        let missing_of = (ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.NextStep({next_step: MissingStep(m, _), _}) =>
            Some(m)
          | _ => None
          };
        switch (missing_of(moved), missing_of(recalc)) {
        | (Some(m1), Some(m2)) =>
          switch (
            m1.editor |> Calc.get_saved_opt,
            m2.editor |> Calc.get_saved_opt,
          ) {
          | (Some(e1), Some(e2)) =>
            check(
              bool,
              "trailing row editor state survives an old-input recalculate",
              true,
              e1.editor.state.zipper == e2.editor.state.zipper,
            )
          | _ => Alcotest.fail("expected trailing row editors")
          }
        | _ => Alcotest.fail("expected a trailing MissingStep row")
        };
      },
    ),
    test_case(
      "calculate empty missing step",
      `Quick,
      () => {
        // A hole-shaped proof is what a MissingStep row stands in for, so
        // promote-or-stay stays.
        let exp = parse_exp("1 + 2");
        let result = test_calculate(~exp, mk_missing_step());
        check(bool, "stays MissingStep", true, is_missing_step(result));
      },
    ),
    test_case(
      "calculate: MissingStep with a real step proof promotes to NextStep",
      `Quick,
      () => {
        let exp = parse_exp("1 == 1");
        let result =
          test_calculate(
            ~exp,
            ~proof=axiom_step_proof(exp),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: AxiomStep(_), _}) => ()
        | _ => Alcotest.fail("Expected NextStep(AxiomStep)")
        };
      },
    ),
    test_case(
      "calculate: promoted step is filled in by its own kind calculate",
      `Quick,
      () => {
        /* The promoted kind starts as a placeholder; the kind's calculate
           fills its fields in from the proof on the same pass. */
        let exp = parse_exp("1 == 1");
        let result =
          test_calculate(
            ~exp,
            ~proof=axiom_step_proof(exp),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: AxiomStep(m), _}) =>
          check(
            string,
            "equality read from the proof",
            "refl_eq",
            m.equality,
          );
          check(
            bool,
            "rewritten sub-term read from the proof",
            true,
            Exp.fast_equal(m.at_exp, exp),
          );
        | _ => Alcotest.fail("Expected NextStep(AxiomStep)")
        };
      },
    ),
    test_case(
      "calculate: NextStep with EmptyHole proof collapses to MissingStep",
      `Quick,
      () => {
        // A NextStep whose proof head is EmptyHole collapses to a MissingStep.
        let exp = parse_exp("1 + 2");
        let step = mk_test_step(~step_kind=mk_induction_step(), ());
        let result = test_calculate(~exp, step);
        check(
          bool,
          "EmptyHole proof collapses NextStep to MissingStep",
          true,
          is_missing_step(result),
        );
      },
    ),
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
    test_case(
      "induction step: create with scrutinee",
      `Quick,
      () => {
        // Construction-only: step_kind does not embed MissingStep; nested
        // steppers use next_step.
        let scrut_exp = parse_exp("n");
        let step_kind = mk_induction_step(~exp=scrut_exp, ());
        switch (step_kind) {
        | StepperBase.InductionStep(_) =>
          check(bool, "induction step created", true, true)
        | _ => Alcotest.fail("Expected InductionStep")
        };
      },
    ),
  ];
};

/* `assume`, `revert` and `generalize` own a body proof rather than
   continuing a `Seq` chain, so a row for one must exist (`Proofs.kind`),
   hand its body to a nested stepper, and give that stepper the goal and
   scope the checker computed, not a locally re-derived guess. */
module WrappingForms = {
  let tests = [
    test_case(
      "assume: a proof-side assume promotes to an AssumeStep row",
      `Quick,
      () => {
        /* If `assume` mapped to None in Proofs.kind, the step and its
           whole body proof would render nothing at all. */
        let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(_), _}) => ()
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "assume: the body's scope carries the auto-named `assume` hypothesis",
      `Quick,
      () => {
        /* Rows inside an assume cite the hypothesis by its auto-name
           (`axiom assume at ...`), which only works if the nested stepper
           is handed the same scope ProofCheck builds. */
        let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(m), _}) =>
          let facts =
            m.inner_ctx
            |> saved_exc(~print="assume inner_ctx")
            |> facts_of_ctx;
          check(
            bool,
            "a fact named `assume` is in the body's scope",
            true,
            List.mem_assoc("assume", facts),
          );
          check(
            bool,
            "and it is the assumed proposition",
            true,
            Exp.fast_equal(
              List.assoc("assume", facts),
              parse_exp("x == 1"),
            ),
          );
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "assume: the body proof renders as nested rows",
      `Quick,
      () => {
        /* The row must descend into the body, not stop at the assume. */
        let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => axiom assume at 0 on x end; axiom refl_eq at 0 on 1 == 1 end in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(m), _}) =>
          switch (m.inner_stepper) {
          | StepperBase.NextStep({step_kind: AxiomStep(_), _}) => ()
          | StepperBase.NextStep(_) =>
            Alcotest.fail("nested row is not the body's axiom step")
          | StepperBase.MissingStep(_, _)
          | StepperBase.Finished =>
            Alcotest.fail("assume body did not render a step row")
          }
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "assume: implication intro strips the antecedent from the body's goal",
      `Quick,
      () => {
        /* The body's goal is read off the checker's ProofMap, so the intro
           reading (goal `A ==> B`, assume A, body proves B) is reflected in
           the nested rows without being re-derived here. */
        let src = {|theorem t = forall x -> x == 2 ==> x + 1 == 3 proof assume x == 2 => ? in t|};
        switch (promote(~src, ~goal="x == 2 ==> x + 1 == 3", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(m), _}) =>
          let inner = m.inner_exp |> saved_exc(~print="assume inner_exp");
          check(
            bool,
            "body works on the consequent",
            true,
            Exp.fast_equal(inner, parse_exp("x + 1 == 3")),
          );
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "revert: a proof-side revert promotes to a RevertStep row",
      `Quick,
      () => {
        let src = {|theorem t = 1 == 1 proof revert 1 == 1 => ? in t|};
        switch (promote(~src, ~goal="1 == 1", ())) {
        | StepperBase.NextStep({step_kind: RevertStep(_), _}) => ()
        | _ => Alcotest.fail("expected NextStep(RevertStep) row")
        };
      },
    ),
    test_case(
      "revert: the body's goal is the fact implying the old goal",
      `Quick,
      () => {
        /* `revert F` with goal `G` hands the body `F ==> G`; the fact stays
           in scope (that is what makes the ex-falso idiom work), so the
           nested rows see the enclosing context unchanged. */
        let src = {|theorem t = forall x -> x == 1 ==> x == 1 proof assume x == 1 => revert x == 1 => ? in t|};
        switch (promote(~src, ~goal="x == 1 ==> x == 1", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(outer), _}) =>
          switch (outer.inner_stepper) {
          | StepperBase.NextStep({step_kind: RevertStep(m), _}) =>
            let inner = m.inner_exp |> saved_exc(~print="revert inner_exp");
            check(
              bool,
              "body works on `F ==> G`",
              true,
              Exp.fast_equal(inner, parse_exp("x == 1 ==> x == 1")),
            );
            let facts =
              m.inner_ctx
              |> saved_exc(~print="revert inner_ctx")
              |> facts_of_ctx;
            check(
              bool,
              "the reverted fact is still citable in the body",
              true,
              List.mem_assoc("assume", facts),
            );
          | _ => Alcotest.fail("expected a nested RevertStep row")
          }
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "generalize: a proof-side generalize re-quantifies the body's goal",
      `Quick,
      () => {
        let src = {|theorem t = forall x -> x == x proof generalize x => ? in t|};
        switch (promote(~src, ~goal="x == x", ())) {
        | StepperBase.NextStep({step_kind: GeneralizeStep(m), _}) =>
          let inner = m.inner_exp |> saved_exc(~print="generalize inner_exp");
          switch (inner |> Exp.term_of) {
          | Forall(_, _) => ()
          | _ =>
            Alcotest.fail(
              "expected a re-quantified body goal, got: " ++ Exp.show(inner),
            )
          };
        | _ => Alcotest.fail("expected NextStep(GeneralizeStep) row")
        };
      },
    ),
    test_case(
      "generalize: facts about the generalized variable leave the body's scope",
      `Quick,
      () => {
        /* Capture soundness: inside the body every fact mentioning x is
           about the old x, so the nested rows must not offer it (ProofCheck
           removes them; the row mirrors that removal). */
        let src = {|theorem t = forall x -> x == 1 ==> x == x proof assume x == 1 => generalize x => ? in t|};
        switch (promote(~src, ~goal="x == 1 ==> x == x", ())) {
        | StepperBase.NextStep({step_kind: AssumeStep(outer), _}) =>
          let outer_facts =
            outer.inner_ctx
            |> saved_exc(~print="assume inner_ctx")
            |> facts_of_ctx;
          check(
            bool,
            "the hypothesis is in scope before the generalize",
            true,
            List.mem_assoc("assume", outer_facts),
          );
          switch (outer.inner_stepper) {
          | StepperBase.NextStep({step_kind: GeneralizeStep(m), _}) =>
            let facts =
              m.inner_ctx
              |> saved_exc(~print="generalize inner_ctx")
              |> facts_of_ctx;
            check(
              bool,
              "and it is gone inside the generalize",
              false,
              List.mem_assoc("assume", facts),
            );
          | _ => Alcotest.fail("expected a nested GeneralizeStep row")
          };
        | _ => Alcotest.fail("expected NextStep(AssumeStep) row")
        };
      },
    ),
    test_case(
      "generalize: the body's scope drops facts mentioning the variable",
      `Quick,
      () => {
        let (ctx, _) =
          SemanticCtx.add_hypothesis(
            SemanticCtx.of_ctx_and_env(ctx_with_x, Builtins.env_init),
            "assume",
            parse_exp("x == 1"),
          );
        check(
          bool,
          "fact present before",
          true,
          List.mem_assoc("assume", facts_of_ctx(ctx)),
        );
        let ctx' =
          Web.WrappingStep.Generalize.scope(
            ~goal=parse_exp("true"),
            ~info_maps=[],
            ctx,
            parse_exp("x"),
          );
        check(
          bool,
          "fact removed after",
          false,
          List.mem_assoc("assume", facts_of_ctx(ctx')),
        );
      },
    ),
  ];
};

/* `have <exp> proof <sub> => <body>` is the cut rule. Its premises are
   its two proof children: `sub` proves `<exp>` in the enclosing scope,
   and `body` proves the outer goal with `<exp>` in scope as `have`. These
   tests pin each premise's goal, scope and picker; how the two regions
   render is HaveStep.view_content's. */
module Have = {
  let tests = [
    test_case(
      "have: a proof-side have promotes to a HaveStep row",
      `Quick,
      () => {
        let src = {|theorem t = forall x -> x == 1 proof have x == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(_), _}) => ()
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the subproof's nested stepper is rooted at the proposition",
      `Quick,
      () => {
        /* The subproof's goal comes from the checker (ProofMap incoming of
           the subproof node), never re-derived: ProofCheck's Have arm
           checks `sub` against the proposition in the enclosing scope. */
        let src = {|theorem t = forall x -> x == 1 proof have x == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          let sub = m.sub_exp |> saved_exc(~print="have sub_exp");
          check(
            bool,
            "subproof works on the proposition",
            true,
            Exp.fast_equal(sub, parse_exp("x == 1")),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: a hole subproof is a step-picker row rooted at the proposition",
      `Quick,
      () => {
        /* This is the "Prove here" shape (ObligationsPanel.have_patch:
           `Have(goal, ?, region)`). The subproof hole must be a functional
           picker row — whose expression is what its overlay writes into
           the proof text — and it must be rooted at the proposition, not
           at the outer goal. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          switch (m.sub_stepper) {
          | StepperBase.MissingStep(mm, _) =>
            check(
              string,
              "picker row's goal is the proposition",
              "1 == 1",
              mm.full_exp
              |> saved_exc(~print="sub picker full_exp")
              |> Test_ProofMap.print_exp,
            )
          | StepperBase.NextStep(_)
          | StepperBase.Finished =>
            Alcotest.fail("subproof hole did not render a picker row")
          }
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: a completed subproof renders its rows",
      `Quick,
      () => {
        /* The row must descend into the subproof, not stop at the have. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof axiom refl_eq at 0 on 1 == 1 end => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          switch (m.sub_stepper) {
          | StepperBase.NextStep({step_kind: AxiomStep(_), _}) => ()
          | StepperBase.NextStep(_) =>
            Alcotest.fail("nested row is not the subproof's axiom step")
          | StepperBase.MissingStep(_, _)
          | StepperBase.Finished =>
            Alcotest.fail("have subproof did not render a step row")
          };
          /* And it reached `true`, which is what drops premise 1's
             obligation and what the region's target row is compared
             against. */
          check(
            bool,
            "subproof's reached expression is literal true",
            true,
            Exp.fast_equal(
              m.sub_last_exp |> saved_exc(~print="have sub_last_exp"),
              Exp.fresh(Atom(Bool(true))),
            ),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the body chain continues with the outer goal",
      `Quick,
      () => {
        /* Premise 2's goal. `have` is a pass-through (its outgoing is the
           body's), so the body's incoming is the outer goal unchanged —
           read off the checker, not re-derived. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          let body = m.body_exp |> saved_exc(~print="have body_exp");
          check(
            bool,
            "body works on the outer goal",
            true,
            Exp.fast_equal(body, parse_exp("x == 1")),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the body premise's hole is a picker row rooted at the outer goal",
      `Quick,
      () => {
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          switch (m.body_stepper) {
          | StepperBase.MissingStep(mm, _) =>
            check(
              string,
              "picker row's goal is the outer goal",
              "x == 1",
              mm.full_exp
              |> saved_exc(~print="body picker full_exp")
              |> Test_ProofMap.print_exp,
            )
          | StepperBase.NextStep(_)
          | StepperBase.Finished =>
            Alcotest.fail("body hole did not render a picker row")
          }
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the body's scope carries the auto-named `have` hypothesis",
      `Quick,
      () => {
        /* Installed unconditionally (ProofCheck's Have arm), even with the
           subproof still a hole — that is what lets an obligation
           incurred by a step inside the body discharge against it the
           moment the wrapper is written. (Premise 2's own obligation is
           deliberately not discharged this way; see ProofCheck.) */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          let facts =
            m.body_ctx |> saved_exc(~print="have body_ctx") |> facts_of_ctx;
          check(
            bool,
            "a fact named `have` is in the body's scope",
            true,
            List.mem_assoc("have", facts),
          );
          check(
            bool,
            "and it is the have's proposition",
            true,
            Exp.fast_equal(List.assoc("have", facts), parse_exp("1 == 1")),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the subproof is checked in the enclosing scope",
      `Quick,
      () => {
        /* The subproof may not cite the have's own hypothesis, so its
           nested rows must not see it — nesting `have` inside `have` is the
           readable way to pin that. */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof have 2 == 2 proof ? => ? => ? in t|};
        switch (promote(~src, ~goal="x == 1", ())) {
        | StepperBase.NextStep({step_kind: HaveStep(outer), _}) =>
          switch (outer.sub_stepper) {
          | StepperBase.NextStep({step_kind: HaveStep(inner), _}) =>
            let facts =
              inner.body_ctx
              |> saved_exc(~print="inner have body_ctx")
              |> facts_of_ctx;
            /* The inner have's body sees its own hypothesis... */
            check(
              bool,
              "inner have's own hypothesis is in scope",
              true,
              List.exists(
                ((n, e)) =>
                  n == "have" && Exp.fast_equal(e, parse_exp("2 == 2")),
                facts,
              ),
            );
            /* ...and not the outer one, since the outer subproof runs in
               the scope before the outer hypothesis was added. */
            check(
              bool,
              "outer have's hypothesis is not in the subproof's scope",
              false,
              List.exists(
                ((n, e)) =>
                  n == "have" && Exp.fast_equal(e, parse_exp("1 == 1")),
                facts,
              ),
            );
          | _ =>
            Alcotest.fail("expected a nested HaveStep row in the subproof")
          }
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: the (!) panel's Prove-here shape renders both chains",
      `Quick,
      () => {
        /* `f` carries a domain restriction and the theorem quantifies `y`
           without it, so `f(y)` incurs a pending `y != 0` obligation and the
           (!) panel offers "Prove here", which writes
           `Have(y != 0, ?, <the existing region>)`
           (ObligationsPanel.have_patch). Both premise regions must show up,
           each rooted at its own goal. */
        let src = "let f = fun x where x != 0 -> 100 / x in theorem t = forall y -> f(y) == f(y) proof have y != 0 proof ? => ? in t";
        let (proof, pm) = checked_proof(src);
        /* Seeded with the env-inlined statement, as Theorems.re does. */
        let result =
          test_calculate(
            ~exp=
              parse_exp(
                "(fun x where x != 0 -> 100 / x)(y) == (fun x where x != 0 -> 100 / x)(y)",
              ),
            ~ctx=ctx_with_x,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({step_kind: HaveStep(m), _}) =>
          check(
            string,
            "the subproof chain is rooted at the obligation's goal",
            "y != 0",
            m.sub_exp
            |> saved_exc(~print="have sub_exp")
            |> Test_ProofMap.print_exp,
          );
          check(
            string,
            "the body chain continues on the written outer goal",
            "f(y) == f(y)",
            m.body_exp
            |> saved_exc(~print="have body_exp")
            |> Test_ProofMap.print_exp,
          );
          /* Both holes are pickable rows, so the user can actually work
             in either position. */
          check(
            bool,
            "subproof hole is a picker row",
            true,
            is_missing_step(m.sub_stepper),
          );
          check(
            bool,
            "body hole is a picker row",
            true,
            is_missing_step(m.body_stepper),
          );
        | _ => Alcotest.fail("expected NextStep(HaveStep) row")
        };
      },
    ),
    test_case(
      "have: an old-input recalculate keeps both children's saved fields",
      `Quick,
      () => {
        /* Both nested steppers are threaded through `Calc.saved` fields
           like every other step's; a pass whose inputs are all old must
           neither drop them back to Pending nor rebuild the nested rows'
           editors (which carry caret/selection). */
        let src = {|theorem t = forall x -> x == 1 proof have 1 == 1 proof ? => ? in t|};
        let (proof, pm) = checked_proof(src);
        let goal = parse_exp("x == 1");
        let calculated =
          test_calculate(
            ~exp=goal,
            ~ctx=ctx_with_x,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        let recalc =
          StepperBase.Stepper.calculate(
            ~settings=Calc.OldValue(CoreSettings.on),
            ~exp=Calc.OldValue(goal),
            ~ctx=
              Calc.OldValue(
                SemanticCtx.of_ctx_and_env(ctx_with_x, Builtins.env_init),
              ),
            ~ana=Calc.OldValue(IdTagged.FreshGrammar.Typ.int()),
            ~proof=Calc.OldValue(proof),
            ~proof_map=Calc.OldValue(pm),
            calculated,
          );
        let have_of = (ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.NextStep({step_kind: HaveStep(m), _}) => m
          | _ => Alcotest.fail("expected NextStep(HaveStep) row")
          };
        let (before, after) = (have_of(calculated), have_of(recalc));
        check(
          bool,
          "subproof goal is stable",
          true,
          Exp.fast_equal(
            before.sub_exp |> saved_exc(~print="before sub_exp"),
            after.sub_exp |> saved_exc(~print="after sub_exp"),
          ),
        );
        check(
          bool,
          "body goal is stable",
          true,
          Exp.fast_equal(
            before.body_exp |> saved_exc(~print="before body_exp"),
            after.body_exp |> saved_exc(~print="after body_exp"),
          ),
        );
        /* The nested picker rows' editors must be the same objects. */
        let editor_of = (~print: string, ns: StepperBase.next_step) =>
          switch (ns) {
          | StepperBase.MissingStep(mm, _) => mm.editor |> saved_exc(~print)
          | _ => Alcotest.fail(print ++ ": expected a picker row")
          };
        check(
          bool,
          "subproof picker editor survives an old-input recalculate",
          true,
          editor_of(~print="before sub editor", before.sub_stepper)
          === editor_of(~print="after sub editor", after.sub_stepper),
        );
        check(
          bool,
          "body picker editor survives an old-input recalculate",
          true,
          editor_of(~print="before body editor", before.body_stepper)
          === editor_of(~print="after body editor", after.body_stepper),
        );
      },
    ),
  ];
};

/* The step picker inserts a wrapping form with nothing to search at
   once, with an empty argument, and puts the caret in that argument.
   These tests pin both halves: what gets written, and that the hole just
   written is a reachable caret target. */
module Insertion = {
  let tests = [
    test_case(
      "insertion: Assume with no argument writes `assume ? =>`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Terms.assume(~exp=EmptyHole |> Exp.fresh),
          );
        check_contains(
          ~msg="assume step landed with a hole",
          out,
          "assume ?",
        );
        check_contains(~msg="the arrow is written too", out, "=>");
        check_contains(
          ~msg="theorem statement survives",
          out,
          "forall x -> x == 2 ==> x == 2",
        );
      },
    ),
    test_case(
      "insertion: Generalize with no argument writes `generalize ? =>`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == x proof ? in t",
            StepperBase.Terms.generalize(~exp=EmptyHole |> Exp.fresh),
          );
        check_contains(
          ~msg="generalize step landed with a hole",
          out,
          "generalize ?",
        );
      },
    ),
    test_case(
      "insertion: an empty-argument form reparses with a hole argument",
      `Quick,
      () => {
        /* The inserted text must parse back to `Assume(EmptyHole, EmptyHole)`
           — an argument hole (which the user is about to type into) and a
           body hole (which becomes the next step-picker row). If the
           argument came back as anything else, the row the user is dropped
           into is not an empty editable slot. */
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Terms.assume(~exp=EmptyHole |> Exp.fresh),
          );
        switch (Test_ProofMap.find_theorem_proof(parse_exp(out))) {
        | Some({
            term: Assume({term: EmptyHole, _}, None, {term: EmptyHole, _}),
            _,
          }) =>
          check(bool, "argument and body are both holes", true, true)
        | Some(p) =>
          Alcotest.fail("reparsed as something else: " ++ Proof.show(p))
        | None => Alcotest.fail("no theorem proof after insertion")
        };
      },
    ),
    test_case(
      "insertion: the new form's argument hole is a reachable caret target",
      `Quick,
      () => {
      /* Focus mechanics: after inserting, the view jumps the main editor's
         caret to `Terms.arg_id(term)` so the argument's SubEditor splice
         accepts edits (SubEditor.confine_pre drops edits whose caret is
         outside the splice). That id survives into the patched segment
         only because the serializer stamps term ids onto the pieces it
         emits — an EmptyHole argument arrives as a Grout carrying it. If
         this jump cannot resolve, the inserted slot looks and behaves
         dead, so pin it. */
      List.iter(
        ((label, term)) => {
          let arg_id =
            switch (StepperBase.Terms.arg_id(term)) {
            | Some(id) => id
            | None => Alcotest.fail(label ++ ": arg_id returned None")
            };
          let z =
            insert_proof_zipper(
              ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
              term,
            );
          check(
            bool,
            label ++ ": caret can jump to the argument hole",
            true,
            Haz3lcore.Move.jump_to_id_indicated(z, arg_id) != None,
          );
        },
        [
          ("assume", StepperBase.Terms.assume(~exp=EmptyHole |> Exp.fresh)),
          (
            "generalize",
            StepperBase.Terms.generalize(~exp=EmptyHole |> Exp.fresh),
          ),
          /* Revert is picked, not typed, but it is focused the same way
             so the user can adjust the pick. */
          (
            "revert (picked)",
            StepperBase.Terms.revert(~exp=parse_exp("x == 2")),
          ),
        ],
      )
    }),
    test_case(
      "insertion: a step with no argument has no argument id", `Quick, () =>
      check(
        bool,
        "a hole step has no wrapping-form argument",
        true,
        StepperBase.Terms.arg_id(Proof.fresh(EmptyHole).term) == None,
      )
    ),
    test_case(
      "insertion: a picked Assume argument writes `assume <e> => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Terms.assume(~exp=parse_exp("x == 2")),
          );
        check_contains(~msg="assume step landed", out, "assume x == 2 =>");
        check_contains(
          ~msg="theorem statement survives",
          out,
          "forall x -> x == 2 ==> x == 2",
        );
      },
    ),
    test_case(
      "insertion: a picked Revert fact writes `revert <e> => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Terms.revert(~exp=parse_exp("x == 1")),
          );
        check_contains(~msg="revert step landed", out, "revert x == 1 =>");
      },
    ),
    test_case(
      "insertion: a picked Generalize argument writes `generalize <e> => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == x proof ? in t",
            StepperBase.Terms.generalize(~exp=parse_exp("x")),
          );
        check_contains(~msg="generalize step landed", out, "generalize x =>");
      },
    ),
    test_case(
      "insertion: Have with no argument writes `have ? proof ? =>`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Terms.have(~exp=EmptyHole |> Exp.fresh),
          );
        check_contains(
          ~msg="have step landed with a hole proposition",
          out,
          "have ?",
        );
        check_contains(~msg="and an open subproof", out, "proof ?");
        check_contains(~msg="the arrow is written too", out, "=>");
      },
    ),
    test_case(
      "insertion: an empty-argument have reparses with three holes",
      `Quick,
      () => {
        /* `have`'s wrapper carries a second hole (the subproof), so the
           inserted text must parse back to
           `Have(EmptyHole, EmptyHole, EmptyHole)`: the proposition the
           user is about to type into, the subproof that becomes its own
           picker row inside the nested box, and the body that becomes the
           next row of the outer chain. */
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Terms.have(~exp=EmptyHole |> Exp.fresh),
          );
        switch (Test_ProofMap.find_theorem_proof(parse_exp(out))) {
        | Some({
            term:
              Have(
                {term: EmptyHole, _},
                {term: EmptyHole, _},
                {term: EmptyHole, _},
              ),
            _,
          }) =>
          check(
            bool,
            "proposition, subproof and body are all holes",
            true,
            true,
          )
        | Some(p) =>
          Alcotest.fail("reparsed as something else: " ++ Proof.show(p))
        | None => Alcotest.fail("no theorem proof after insertion")
        };
      },
    ),
    test_case(
      "insertion: a picked Have proposition writes `have <e> proof ? => ?`",
      `Quick,
      () => {
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            StepperBase.Terms.have(~exp=parse_exp("1 == 1")),
          );
        check_contains(~msg="have step landed", out, "have 1 == 1 proof ?");
      },
    ),
    test_case(
      "insertion: a new have's proposition hole is a reachable caret target",
      `Quick,
      () => {
        /* `have`'s proposition is child 0 like the other wrapping forms'
           (Form.re: `mk_pre_c(L, ["have", "proof", "=>"], ..., [Exp,
           Proof])`), so `Terms.arg_id` and the caret jump apply unchanged
           — which is what makes the inserted slot typable. */
        let term = StepperBase.Terms.have(~exp=EmptyHole |> Exp.fresh);
        let arg_id =
          switch (StepperBase.Terms.arg_id(term)) {
          | Some(id) => id
          | None => Alcotest.fail("have: Terms.arg_id returned None")
          };
        let z =
          insert_proof_zipper(
            ~src="theorem t = forall x -> x == 1 proof ? in t",
            term,
          );
        check(
          bool,
          "caret can jump to the proposition hole",
          true,
          Haz3lcore.Move.jump_to_id_indicated(z, arg_id) != None,
        );
      },
    ),
    test_case(
      "insertion: an inserted wrapping form reparses as that form",
      `Quick,
      () => {
        /* The patch reflows through ExpToSegment, so the written text must
           parse back to the same proof term — otherwise the row that
           `calculate` synthesizes next pass is not the one just inserted. */
        let out =
          insert_proof_term(
            ~src="theorem t = forall x -> x == 2 ==> x == 2 proof ? in t",
            StepperBase.Terms.assume(~exp=parse_exp("x == 2")),
          );
        switch (Test_ProofMap.find_theorem_proof(parse_exp(out))) {
        | Some({term: Assume(e, None, {term: EmptyHole, _}), _}) =>
          check(
            bool,
            "the assumed expression round-trips",
            true,
            Exp.fast_equal(e, parse_exp("x == 2")),
          )
        | Some(p) =>
          Alcotest.fail("reparsed as something else: " ++ Proof.show(p))
        | None => Alcotest.fail("no theorem proof after insertion")
        };
      },
    ),
  ];
};

/* Goals as rows show them: env-substituted for display, written for
   the step picker. */
module Display = {
  let tests = [
    test_case(
      "rewrite: the row after an env-inlined `at` step shows the rewritten goal",
      `Quick,
      () => {
        /* The stepper overlay writes `at <n>` by counting occurrences in
           the goal as displayed (env-inlined), so `at 2` here names the
           single literal `0` of `sum == fold_left(_, sum, 0)`: the two
           earlier ones live in the inlined body of `sum`. */
        let src = {|let sum(xs) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in
theorem t = sum == fold_left(_, sum, 0)
proof rewrite 0 with 0 + 0 at 2 end
in t|};
        let result = promote(~src, ~goal="sum == fold_left(_, sum, 0)", ());
        switch (result, last_missing_step(result)) {
        | (StepperBase.NextStep({step_kind: AlgebriteStep(_), _}), Some(m)) =>
          check(
            string,
            "next row's goal is the rewritten goal",
            "sum == fold_left(_, sum, 0 + 0)",
            m.eval_exp
            |> saved_exc(~print="next row eval_exp")
            |> Test_ProofMap.print_exp,
          )
        | _ => Alcotest.fail("expected NextStep(AlgebriteStep) row")
        };
      },
    ),
    /* The checker keeps the written goal (`f(y)`); the row must show
       the env-substituted Fun so function folding can print the name.
       Quoting and write-back still work: `nth_exp_env` and
       `EditorTransform.editable` cover them. */
    test_case(
      "missing-step row env-substitutes the checker's written goal",
      `Quick,
      () => {
        let src = "let f = fun x where x != 0 -> 100 / x in theorem t = forall y where y != 0 -> f(y) == f(y) proof ? in t";
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no theorem proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let env =
          switch (EvaluatorState.get_theorems(state)) {
          | [(_, _, env, _), ..._] => env
          | [] => Alcotest.fail("no recorded theorem env")
          };
        /* Seed with the written names — the row must subst them away. */
        let written_goal = parse_exp("f(y) == f(y)");
        let result =
          test_calculate(
            ~exp=written_goal,
            ~ctx=ctx_with_x,
            ~env,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.MissingStep(m, _) =>
          let shown = m.full_exp |> saved_exc(~print="full_exp");
          let rec is_fun_ap = (e: Exp.t): bool =>
            switch (e.term) {
            | Ap(_, {term: Fun(_) | FixF(_) | Closure(_), _}, _) => true
            | Ap(_, inner, _) => is_fun_ap(inner)
            | Parens(inner)
            | Asc(inner, _) => is_fun_ap(inner)
            | BinOp(_, l, r) => is_fun_ap(l) || is_fun_ap(r)
            | _ => false
            };
          check(
            string,
            "row is not the unsubstituted written names",
            "inlined",
            Test_ProofMap.print_exp(shown) == "f(y) == f(y)"
              ? "written" : "inlined",
          );
          check(
            bool,
            "let-bound f is inlined as a function value",
            true,
            is_fun_ap(shown),
          );
        | _ => Alcotest.fail("expected a MissingStep row")
        };
      },
    ),
    test_case(
      "the synthesized trailing hole env-substitutes the step's outgoing",
      `Quick,
      () => {
        /* After every leaf step, calculate_full_step synthesizes a
           trailing MissingStep with `~proof=None`. That row cannot look
           up ProofMap, so it has to receive an already-substituted
           `next_exp` — otherwise the hole under the step shows the
           written names while the step's own row shows the inlined Fun. */
        let src = "let f = fun x -> x in theorem t = f == f proof funext x => ? in t";
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no theorem proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let env =
          switch (EvaluatorState.get_theorems(state)) {
          | [(_, _, env, _), ..._] => env
          | [] => Alcotest.fail("no recorded theorem env")
          };
        let result =
          test_calculate(
            ~exp=parse_exp("f == f"),
            ~ctx=ctx_with_x,
            ~env,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.NextStep({next_step: MissingStep(m, _), _}) =>
          let shown = m.full_exp |> saved_exc(~print="trailing full_exp");
          let rec has_fun = (e: Exp.t): bool =>
            switch (e.term) {
            | Fun(_)
            | FixF(_)
            | Closure(_) => true
            | BinOp(_, l, r) => has_fun(l) || has_fun(r)
            | Ap(_, f, a) => has_fun(f) || has_fun(a)
            | Parens(inner)
            | Asc(inner, _) => has_fun(inner)
            | _ => false
            };
          check(
            string,
            "trailing hole is not the unsubstituted written names",
            "inlined",
            Test_ProofMap.print_exp(shown) == "f == f"
              ? "written" : "inlined",
          );
          check(
            bool,
            "let-bound f is inlined as a function value",
            true,
            has_fun(shown),
          );
        | _ =>
          Alcotest.fail(
            "expected NextStep(Funext) with a synthesized trailing MissingStep",
          )
        };
      },
    ),
    test_case(
      "inlined recursive applications still offer eval steps",
      `Quick,
      () => {
        /* `<sum>([]) == <fold_left+>(_, <sum>, 0)([])`: display subst
           puts FixF in the tree, but the overlay must still quote the
           written names so VarLookup is AvailableSteps, not AutoStep. */
        let src = {|let sum(xs) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in
theorem t = sum([]) == fold_left(_, sum, 0)([])
proof ?
in t|};
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no theorem proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let env =
          switch (EvaluatorState.get_theorems(state)) {
          | [(_, _, env, _), ..._] => env
          | [] => Alcotest.fail("no recorded theorem env")
          };
        let result =
          test_calculate(
            ~exp=parse_exp("sum([]) == fold_left(_, sum, 0)([])"),
            ~ctx=ctx_with_x,
            ~env,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        switch (result) {
        | StepperBase.MissingStep(m, _) =>
          switch (m.next_steps |> Calc.get_saved_opt) {
          | Some(EvaluatorStep.AvailableSteps([_, ..._])) => ()
          | Some(EvaluatorStep.AvailableSteps([])) =>
            Alcotest.fail("inlined applications offered no eval steps")
          | Some(EvaluatorStep.AutoStep(_)) =>
            Alcotest.fail(
              "eval overlay collapsed to AutoStep (FixUnwrap of inlined FixF)",
            )
          | None => Alcotest.fail("next_steps not calculated")
          }
        | _ => Alcotest.fail("expected a MissingStep row")
        };
      },
    ),
    test_case(
      "fold_left overlay ids are unique in the trailing missing step",
      `Quick,
      () => {
        /* The user's program: an `eval` step is already written, so the
           row after it is a synthesized MissingStep whose display term
           has `sum` / `fold_left` inlined. Both the row's own segment
           and the ids it underlines must be free of duplicates, or
           `Highlight.of_tile` raises a shard mismatch. */
        let src = {|let plus = fun (x,y) -> x + y in
let sum(xs) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in
theorem t = sum([]) == fold_left(_, sum, 0)([])
proof eval fold_left(_, sum, 0)([]) at 0 end
in t|};
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no theorem proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let env =
          switch (EvaluatorState.get_theorems(state)) {
          | [(_, _, env, _), ..._] => env
          | [] => Alcotest.fail("no recorded theorem env")
          };
        let result =
          test_calculate(
            ~exp=parse_exp("sum([]) == fold_left(_, sum, 0)([])"),
            ~ctx=ctx_with_x,
            ~env,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        let m =
          switch (last_missing_step(result)) {
          | Some(m) => m
          | None => Alcotest.fail("no trailing MissingStep row")
          };
        let seg =
          (m.editor |> saved_exc(~print="missing step editor")).editor.state.
            zipper
          |> Zipper.unselect_and_zip;
        let ids = segment_tile_ids(seg);
        let dups = duplicated(ids);
        check(
          int,
          "trailing missing-step segment has duplicated tile ids: "
          ++ String.concat(", ", List.map(Id.to_string, dups)),
          0,
          List.length(dups),
        );
        let steps =
          switch (m.next_steps |> Calc.get_saved_opt) {
          | Some(EvaluatorStep.AvailableSteps(steps)) => steps
          | Some(EvaluatorStep.AutoStep(_)) => []
          | None => Alcotest.fail("next_steps not calculated")
          };
        let bad =
          steps
          |> List.map(Web.MissingStep.Update.display_step_id(m))
          |> List.filter(id =>
               List.length(List.filter(i => Id.equal(i, id), ids)) > 1
             );
        check(
          int,
          "overlay step ids that are not unique in the segment: "
          ++ String.concat(", ", List.map(Id.to_string, bad)),
          0,
          List.length(bad),
        );
      },
    ),
    test_case(
      "funext body still offers eval of an applied recursive function",
      `Quick,
      () => {
        /* Display subst inlines `sum` / `fold_left` as `FixF`. Computing
           `get_status` on that term treats hidden FixUnwrap as AutoStep
           and wipes the overlay — the steppable places on
           `<sum>([]) == <fold_left>([], <sum>, 0)` go missing. Eval
           overlay must quote the written goal so VarLookup stays
           available. */
        let src = {|let sum(xs) = case xs
| [] => 0
| x::xs => x + sum(xs)
end
in
theorem t = sum == fold_left(_, sum, 0)
proof funext xs => ?
in t|};
        let (state, _, elab) =
          Test_ProofMap.eval_with_proof(parse_exp(src));
        let proof =
          switch (Test_ProofMap.find_theorem_proof(elab)) {
          | Some(p) => p
          | None => Alcotest.fail("no theorem proof")
          };
        let pm = EvaluatorState.get_proof_map(state);
        let env =
          switch (EvaluatorState.get_theorems(state)) {
          | [(_, _, env, _), ..._] => env
          | [] => Alcotest.fail("no recorded theorem env")
          };
        let result =
          test_calculate(
            ~exp=parse_exp("sum == fold_left(_, sum, 0)"),
            ~ctx=ctx_with_x,
            ~env,
            ~proof=Calc.NewValue(proof),
            ~proof_map=Calc.NewValue(pm),
            mk_missing_step(),
          );
        let inner_missing =
          switch (result) {
          | StepperBase.NextStep({
              step_kind: FunextStep({inner_stepper, _}),
              _,
            }) =>
            switch (inner_stepper) {
            | StepperBase.MissingStep(m, _) => m
            | _ => Alcotest.fail("funext body is not a MissingStep hole")
            }
          | _ => Alcotest.fail("expected NextStep(FunextStep)")
          };
        let shown = inner_missing.full_exp |> saved_exc(~print="funext body");
        let rec has_fix = (e: Exp.t): bool =>
          switch (e.term) {
          | FixF(_) => true
          | Fun(_)
          | Closure(_) => false
          | Ap(_, f, a) => has_fix(f) || has_fix(a)
          | DeferredAp(f, args) => has_fix(f) || List.exists(has_fix, args)
          | BinOp(_, l, r) => has_fix(l) || has_fix(r)
          | Parens(inner)
          | Asc(inner, _) => has_fix(inner)
          | _ => false
          };
        check(
          bool,
          "pointwise goal is display-substituted (FixF inlined)",
          true,
          has_fix(shown),
        );
        switch (inner_missing.next_steps |> Calc.get_saved_opt) {
        | Some(EvaluatorStep.AvailableSteps([_, ..._])) => ()
        | Some(EvaluatorStep.AvailableSteps([])) =>
          Alcotest.fail("funext body offered no eval steps")
        | Some(EvaluatorStep.AutoStep(_)) =>
          Alcotest.fail(
            "funext body eval overlay collapsed to AutoStep (FixUnwrap of inlined FixF)",
          )
        | None => Alcotest.fail("funext body next_steps not calculated")
        };
      },
    ),
  ];
};

let tests = (
  "StepperBase",
  Calculate.tests
  @ WrappingForms.tests
  @ Have.tests
  @ Insertion.tests
  @ Display.tests,
);
