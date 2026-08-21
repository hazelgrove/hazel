open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Phase 4b: the `generalize` proof step (docs/prover-obligations.md,
 * Phase 4).
 *
 * `generalize x => body` re-quantifies an already-peeled binder: the
 * body's incoming goal is `forall x -> G` (`forall x where g -> G` when
 * x carried a `where` restriction), every fact mentioning the old x is
 * removed from the body's scope (capture soundness), and the node's
 * outgoing is `true` only when the body proves the re-quantified goal.
 * The payoff is forall-quantified inductive hypotheses. Reuses the
 * Test_ProofMap harness, like Test_Definedness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of = Test_ProofMap.proof_of;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;
let check_exp = Test_ProofMap.check_exp;

let run = (src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of(elab));
};

let run_named = (name: string, src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of_named(name, elab));
};

/* The immediate body of a `generalize` node (the sub-proof that sees the
 * re-quantified goal). */
let generalize_body = (p: Proof.t): Proof.t =>
  switch (p.term) {
  | Generalize(_, body) => body
  | _ => Alcotest.fail("expected the proof to be a generalize node")
  };

let incoming_of = (pm: ProofMap.t, p: Proof.t): Exp.t =>
  switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
  | Some({incoming: Some(e), _}) => e
  | _ => Alcotest.fail("no incoming goal recorded for the proof node")
  };

let is_forall = (e: Exp.t): bool =>
  switch (e |> Exp.term_of) {
  | Forall(_, _) => true
  | _ => false
  };

let is_forall_where = (e: Exp.t): bool =>
  switch (e |> Exp.term_of) {
  | ForallWhere(_, _, _) => true
  | _ => false
  };

let check_status = (msg, expected: ProofMap.full_status, pm, proof) => {
  let actual = ProofMap.full_status_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.bool,
    msg ++ " (got: " ++ ProofMap.show_full_status(actual) ++ ")",
    true,
    actual == expected,
  );
};

/* --- 1. Mechanics ------------------------------------------------------ */

/* Inside `generalize y` the goal is the re-quantified
 * `forall y -> x + y == y + x`; re-peel it and close by rewrite + refl.
 * The theorem is fully Proven. */
let test_mechanics_requantified_goal = () => {
  let src = {|theorem t = forall x -> forall y -> x + y == y + x proof generalize y => forall y => rewrite x + y with y + x at 0 end; axiom refl_eq at 0 on y + x == y + x end in t|};
  let (pm, proof) = run(src);
  let body = generalize_body(proof);
  let body_goal = incoming_of(pm, body);
  Alcotest.check(
    Alcotest.bool,
    "the body's incoming goal is a plain forall",
    true,
    is_forall(body_goal),
  );
  check_exp(
    "the body's incoming goal re-quantifies y",
    "forall y -> x + y == y + x",
    body_goal,
  );
  check_status("theorem is fully Proven", ProofMap.Proven, pm, proof);
};

/* --- 2. Capture -------------------------------------------------------- */

/* A hypothesis about x must become unavailable inside `generalize x`:
 * citing it fails with the ordinary unknown-equality behavior. */
let test_captured_hypothesis_not_citable = () => {
  let src = {|theorem t = forall x -> x == 1 proof assume x == 1 => generalize x => forall x => axiom assume at 0 on x end in t|};
  let (pm, proof) = run(src);
  Alcotest.check(
    Alcotest.bool,
    "citing the captured assume-fact is UnknownEquality",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.UnknownEquality(_) => true
      | _ => false,
    ),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "the theorem is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* An obligation identical to the captured fact must stay Pending inside
 * the generalize (discharge channel 1 no longer sees the removed fact);
 * the same program without `generalize` discharges it Remote. */
let test_captured_fact_does_not_discharge = () => {
  let src = {|theorem t = forall x -> x == x proof assume x != 0 => generalize x => forall x => assume x != 0 => axiom refl_eq at 0 on x == x end in t|};
  let (pm, proof) = run(src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.int,
    "two assume obligations",
    2,
    List.length(obs),
  );
  Alcotest.check(
    Alcotest.bool,
    "both obligations are Pending (no remote discharge across generalize)",
    true,
    List.for_all(Obligation.is_pending, obs),
  );
  /* Control: without the generalize, the inner assume discharges
   * against the outer one (channel 1, Remote). */
  let control = {|theorem t = forall x -> x == x proof assume x != 0 => assume x != 0 => axiom refl_eq at 0 on x == x end in t|};
  let (pm', proof') = run(control);
  let obs' = ProofMap.obligations_of_proof(pm', proof');
  Alcotest.check(
    Alcotest.bool,
    "control: the inner obligation discharges Remote",
    true,
    List.exists(
      (ob: Obligation.t) =>
        switch (ob.discharge) {
        | Obligation.Remote(_) => true
        | _ => false
        },
      obs',
    ),
  );
};

/* --- 3. Quantified IH end-to-end (the acceptance test) ------------------ */

/* rev_append-style length theorem: the cons case needs the IH at
 * `x :: ys`, a DIFFERENT instantiation than the peeled ys — impossible
 * with the naive unquantified IH, and exactly what `generalize ys`
 * provides. Helper equations are supplied as hole-proof lemmas (the test
 * targets the generalize/IH machinery, not list algebra). */
let quantified_ih_src = {|let ra = fun xs -> fun ys -> case xs | [] => ys | h :: t => ra(t)(h :: ys) end in let len = fun l -> case l | [] => 0 | h :: t => 1 + len(t) end in theorem ra_nil = forall zs -> ra([])(zs) == zs proof ? in theorem ra_cons = forall h -> forall t -> forall zs -> ra(h :: t)(zs) == ra(t)(h :: zs) proof ? in theorem len_nil = len([]) == 0 proof ? in theorem len_cons = forall h -> forall t -> len(h :: t) == 1 + len(t) proof ? in theorem shuffle = forall a -> forall b -> a + (1 + b) == (1 + a) + b proof ? in theorem u = forall xs: [Int] -> forall ys -> len(ra(xs)(ys)) == len(xs) + len(ys) proof generalize ys => induction xs | [] => forall ys => axiom ra_nil at 0 on ra([])(ys) end; axiom len_nil at 0 on len([]) end; rewrite 0 + len(ys) with len(ys) at 0 end; axiom refl_eq at 0 on len(ys) == len(ys) end | x :: xs' => forall ys => axiom ra_cons at 0 on ra(x :: xs')(ys) end; axiom ih at 0 on len(ra(xs')(x :: ys)) end; axiom len_cons at 0 on len(x :: ys) end; axiom len_cons at 0 on len(x :: xs') end; axiom shuffle at 0 on len(xs') + (1 + len(ys)) end; axiom refl_eq at 0 on (1 + len(xs')) + len(ys) == (1 + len(xs')) + len(ys) end end in u|};

let test_quantified_ih_proves = () => {
  let (pm, proof) = run_named("u", quantified_ih_src);
  /* The induction operates on the re-quantified goal, so its generated
   * IHs are forall-quantified over ys. */
  let body = generalize_body(proof);
  Alcotest.check(
    Alcotest.bool,
    "the induction's incoming goal is forall-quantified",
    true,
    is_forall(incoming_of(pm, body)),
  );
  let obs = ProofMap.obligations_of_proof(pm, proof);
  Alcotest.check(Alcotest.int, "no obligations", 0, List.length(obs));
  check_status(
    "theorem u is fully Proven (IH cited at x :: ys)",
    ProofMap.Proven,
    pm,
    proof,
  );
};

/* Negative control: WITHOUT generalize, the IH is the unquantified
 * equation at the peeled ys, and citing it at `x :: ys` fails
 * (RuleDoesNotApply) — the naive IH really is too weak. */
let test_unquantified_ih_too_weak = () => {
  let src = {|let ra = fun xs -> fun ys -> case xs | [] => ys | h :: t => ra(t)(h :: ys) end in let len = fun l -> case l | [] => 0 | h :: t => 1 + len(t) end in theorem ra_cons = forall h -> forall t -> forall zs -> ra(h :: t)(zs) == ra(t)(h :: zs) proof ? in theorem u = forall xs: [Int] -> forall ys -> len(ra(xs)(ys)) == len(xs) + len(ys) proof induction xs | [] => ? | x :: xs' => axiom ra_cons at 0 on ra(x :: xs')(ys) end; axiom ih at 0 on len(ra(xs')(x :: ys)) end end in u|};
  let (pm, proof) = run_named("u", src);
  Alcotest.check(
    Alcotest.bool,
    "citing the naive IH at x :: ys does not apply",
    true,
    has_mark_kind(
      pm,
      proof,
      fun
      | ProofMark.RuleDoesNotApply(_) => true
      | _ => false,
    ),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "the naive proof is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* --- 4. Where-restriction travel ---------------------------------------- */

/* Generalizing a restricted binder re-attaches the restriction:
 * the body's goal is `forall n where n != 0 -> ...`, and after
 * re-peeling, a division axiom's `n != 0` condition discharges against
 * the reinstalled `where` hypothesis. */
let test_where_restriction_travels = () => {
  let src = {|theorem inv = forall w where w != 0 -> w / w == 1 proof ? in theorem t = forall n where n != 0 -> n / n == 1 proof generalize n => forall n => axiom inv at 0 on n / n end; axiom refl_eq at 0 on 1 == 1 end in t|};
  let (pm, proof) = run_named("t", src);
  let body = generalize_body(proof);
  let body_goal = incoming_of(pm, body);
  Alcotest.check(
    Alcotest.bool,
    "the body's incoming goal is a restricted forall",
    true,
    is_forall_where(body_goal),
  );
  let obs = ProofMap.obligations_of_proof(pm, proof);
  Alcotest.check(Alcotest.int, "one guard obligation", 1, List.length(obs));
  switch (obs) {
  | [ob] =>
    check_exp("the obligation is the guard at n", "n != 0", ob.goal);
    Alcotest.check(
      Alcotest.bool,
      "discharged remotely against the re-peeled where hypothesis",
      true,
      switch (ob.discharge) {
      | Obligation.Remote(_) => true
      | _ => false
      },
    );
  | _ => ()
  };
  check_status("theorem t is fully Proven", ProofMap.Proven, pm, proof);
};

/* --- 5. Malformed ------------------------------------------------------- */

let has_malformed_generalize = (pm, proof) =>
  has_mark_kind(
    pm,
    proof,
    fun
    | ProofMark.MalformedGeneralize => true
    | _ => false,
  );

/* `generalize 5`: not a variable — marked, goal passes through. */
let test_generalize_literal_marked = () => {
  let src = {|theorem t = forall x -> x == x proof generalize 5 => axiom refl_eq at 0 on x == x end in t|};
  let (pm, proof) = run(src);
  Alcotest.check(
    Alcotest.bool,
    "MalformedGeneralize mark is emitted",
    true,
    has_malformed_generalize(pm, proof),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "the theorem is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

/* `generalize z` with z not in scope — same mark. */
let test_generalize_out_of_scope_marked = () => {
  let src = {|theorem t = forall x -> x == x proof generalize z => axiom refl_eq at 0 on x == x end in t|};
  let (pm, proof) = run(src);
  Alcotest.check(
    Alcotest.bool,
    "MalformedGeneralize mark is emitted",
    true,
    has_malformed_generalize(pm, proof),
  );
  Alcotest.check(
    Alcotest.option(bool),
    "the theorem is not proven",
    None,
    ProofMap.status_of_proof(pm, proof),
  );
};

let tests = (
  "Evaluator.Generalize",
  [
    test_case(
      "generalize re-quantifies the goal and the theorem proves",
      `Quick,
      test_mechanics_requantified_goal,
    ),
    test_case(
      "a captured hypothesis is not citable inside generalize",
      `Quick,
      test_captured_hypothesis_not_citable,
    ),
    test_case(
      "a captured fact no longer discharges obligations",
      `Quick,
      test_captured_fact_does_not_discharge,
    ),
    test_case(
      "quantified IH end-to-end: generalize + induction proves",
      `Quick,
      test_quantified_ih_proves,
    ),
    test_case(
      "control: the unquantified IH is too weak",
      `Quick,
      test_unquantified_ih_too_weak,
    ),
    test_case(
      "a where restriction travels onto the re-quantified binder",
      `Quick,
      test_where_restriction_travels,
    ),
    test_case(
      "generalize of a literal is marked malformed",
      `Quick,
      test_generalize_literal_marked,
    ),
    test_case(
      "generalize of an out-of-scope name is marked malformed",
      `Quick,
      test_generalize_out_of_scope_marked,
    ),
  ],
);
