open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Phase 4d: explicit instantiation at the citation site
 * (docs/prover-obligations.md, open item 3).
 *
 *   `axiom <name> with <x> = <e> at <i> on <target> end`
 *   `axiomrev <name> with <x> = <e> at <i> on <target> end`
 *   `revert <fact> with <x> = <e> => <proof>`
 *
 * For the axiom step the binding is seeded into `MatchExp`'s match
 * context BEFORE matching, so `UnderdeterminedInstantiation` only fires
 * on metavariables still unresolved AFTER seeding. For `revert` the
 * named binder of the (quantified) in-scope fact is eliminated and the
 * INSTANTIATED fact is what gets cashed into the goal.
 *
 * The Phase-3a instantiation gates apply to the supplied expression
 * exactly as they do to a matched one: possible divergence is refused
 * outright, domain conditions become obligations. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;
let print_exp = Test_ProofMap.print_exp;

let run_named = (name: string, src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of_named(name, elab));
};

let one_line = (s: string): string =>
  String.map(c => c == '\n' ? ' ' : c, s);

/* Substring search. */
let contains = (hay: string, needle: string): bool => {
  let (nh, nn) = (String.length(hay), String.length(needle));
  let rec go = (i: int) =>
    i + nn > nh ? false : String.sub(hay, i, nn) == needle || go(i + 1);
  go(0);
};

let opt_exp_str = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => one_line(print_exp(e))
  | None => "<none>"
  };

let rec dump = (pm: ProofMap.t, p: Proof.t, ind: string): string => {
  let (inc, out, marks) =
    switch (ProofMap.lookup(Proof.rep_id(p), pm)) {
    | Some({incoming, outgoing, marks, _}) => (
        opt_exp_str(incoming),
        opt_exp_str(outgoing),
        String.concat("; ", List.map(ProofMark.show, marks)),
      )
    | None => ("<no entry>", "", "")
    };
  let here =
    ind
    ++ "in:  "
    ++ inc
    ++ "\n"
    ++ ind
    ++ "out: "
    ++ out
    ++ (marks == "" ? "" : "\n" ++ ind ++ "marks: " ++ marks);
  let kids =
    switch (p.term) {
    | Seq(a, b) => [dump(pm, a, ind), dump(pm, b, ind)]
    | Forall(_, b)
    | Assume(_, _, b)
    | Generalize(_, b)
    | Revert(_, _, b) => [dump(pm, b, ind ++ "  ")]
    | Induction(_, _, cases) =>
      List.map(((_, b)) => dump(pm, b, ind ++ "  "), cases)
    | _ => []
    };
  String.concat("\n", [here, ...kids]);
};

let check_mark_free = (msg, pm, proof) =>
  switch (Test_ProofMap.find_marked_sub(pm, proof)) {
  | None => ()
  | Some((_, marks)) =>
    Alcotest.fail(
      msg
      ++ ": expected no marks, got "
      ++ String.concat("; ", List.map(ProofMark.show, marks))
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof, ""),
    )
  };

let check_status = (msg, expected: ProofMap.full_status, pm, proof) => {
  let actual = ProofMap.full_status_of_proof(pm, proof);
  if (actual != expected) {
    Alcotest.fail(
      msg
      ++ "\nexpected: "
      ++ ProofMap.show_full_status(expected)
      ++ "\ngot:      "
      ++ ProofMap.show_full_status(actual)
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof, ""),
    );
  };
};

let obligation_goals = (pm, proof): list(string) =>
  ProofMap.obligations_of_proof(pm, proof)
  |> List.map((ob: Obligation.t) => one_line(print_exp(ob.goal)));

let check_has_obligation = (msg, needle: string, pm, proof) => {
  let goals = obligation_goals(pm, proof);
  if (!List.exists(g => g == needle, goals)) {
    Alcotest.fail(
      msg
      ++ ": expected an obligation `"
      ++ needle
      ++ "`, got ["
      ++ String.concat(", ", goals)
      ++ "]",
    );
  };
};

/* --- Fixture -------------------------------------------------------------

   `lem` is the shape that forced this feature: a conditional rule whose
   binder `n` appears in the ANTECEDENT but not in the conclusion, so
   matching the conclusion can never resolve it. (This is the quantified
   inductive hypothesis of the STLC milestone in miniature.) Its own
   proof is a hole — a cited fact need not be proven for the citation
   machinery to run. */

let prelude = "let f = fun x -> x + 1 in ";

/* A general-recursive function the totality checker cannot accept: the
   recursive call's argument is arithmetic, not a strict subterm. */
let diverger = "let bad = fun x -> bad(x + 1) in ";

let lem = "theorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof ? in ";

let src = (rest: string): string => prelude ++ lem ++ rest;

/* --- axiom + with -------------------------------------------------------- */

/* The control: without a `with` clause the citation is refused, exactly
   as in Phase 3a/4c. */
let test_axiom_underdetermined_without_with = () => {
  let (pm, proof) =
    run_named(
      "g",
      src("theorem g = f(2) == 3 proof axiom lem at 0 on f(2) end in g"),
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnderdeterminedInstantiation(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "citing `lem` without `with` should be underdetermined\n"
      ++ dump(pm, proof, ""),
    );
  };
};

/* The feature: seeding `n` resolves the rule, the rewrite goes through,
   and the instantiated antecedent `1 == 1` is incurred as an obligation
   — discharged by closed evaluation (channel 2), so the theorem is
   fully Proven. */
let test_axiom_with_resolves = () => {
  let (pm, proof) =
    run_named(
      "g",
      src(
        "theorem g = f(2) == 3 proof axiom lem with n = 1 at 0 on f(2) end; eval 3 == 3 at 0 end in g",
      ),
    );
  check_mark_free("axiom+with", pm, proof);
  check_has_obligation(
    "axiom+with incurs its antecedent",
    "1 == 1",
    pm,
    proof,
  );
  check_status("axiom+with closes the goal", ProofMap.Proven, pm, proof);
};

/* `axiomrev` takes the clause too (right-to-left application). */
let test_axiomrev_with_resolves = () => {
  let (pm, proof) =
    run_named(
      "g",
      src(
        "theorem g = 3 == 3 proof axiomrev lem with n = 1 at 0 on 3 end in g",
      ),
    );
  check_mark_free("axiomrev+with", pm, proof);
};

/* A `with` clause naming something that is not a binder of the cited
   rule is marked and passes the goal through. */
let test_axiom_with_unknown_var = () => {
  let (pm, proof) =
    run_named(
      "g",
      src(
        "theorem g = f(2) == 3 proof axiom lem with m = 1 at 0 on f(2) end in g",
      ),
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnknownInstantiationVar(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "`with m = ...` should be an unknown instantiation variable\n"
      ++ dump(pm, proof, ""),
    );
  };
};

/* --- The gates apply to the supplied expression ------------------------- */

/* Divergence is REFUSED, never an obligation (§1.1, §4.1): a supplied
   expression whose call graph reaches a non-structural fix cannot be
   used to instantiate. */
let test_axiom_with_divergent_is_refused = () => {
  let (pm, proof) =
    run_named(
      "g",
      prelude
      ++ diverger
      ++ lem
      ++ "theorem g = f(2) == 3 proof axiom lem with n = bad(1) at 0 on f(2) end in g",
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.PossiblyDivergentInstantiation(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "a divergent supplied instantiation should be refused\n"
      ++ dump(pm, proof, ""),
    );
  };
};

/* A domain error is an OBLIGATION: instantiating at `6 / d` incurs
   `d != 0` through the ordinary scan. */
let test_axiom_with_domain_condition = () => {
  let (pm, proof) =
    run_named(
      "g",
      src(
        "theorem g = forall d: Int -> f(2) == 3 proof forall d => axiom lem with n = 6 / d at 0 on f(2) end; eval 3 == 3 at 0 end in g",
      ),
    );
  check_has_obligation(
    "instantiating at `6 / d` scans for the denominator",
    "d != 0",
    pm,
    proof,
  );
};

/* --- revert + with ------------------------------------------------------- */

/* `p` is a quantified fact in scope. `revert p with n = 5` cashes
   `5 + 0 == 5` (the INSTANCE) into the goal, not the quantified
   statement. */
let quantified = "theorem p = forall n: Int -> n + 0 == n proof ? in ";

let test_revert_with_instantiates = () => {
  let (pm, proof) =
    run_named(
      "g",
      prelude
      ++ quantified
      ++ "theorem g = f(2) == 3 proof revert p with n = 5 => eval 5 + 0 at 0 end; eval 5 == 5 at 0 end; eval true ==> f(2) == 3 at 0 end; eval f(2) at 0 end; eval 2 + 1 at 0 end; eval 3 == 3 at 0 end in g",
    );
  check_mark_free("revert+with", pm, proof);
  check_status("revert+with closes the goal", ProofMap.Proven, pm, proof);
};

/* A quantified INDUCTIVE HYPOTHESIS, cited BY NAME and instantiated.
   This is the milestone's shape in miniature, and it covers two things
   the spelled-out form does not: by-name resolution of a fact out of the
   environment, and identification of a binder whose INSTALLED name
   carries substitution-renaming primes the source never shows. */
let ih_src = (cite: string): string =>
  "type L = +Nil+Cons(L) in "
  ++ "let sz = fun l -> case l | Nil => 0 | Cons(r) => sz(r) + 1 end in "
  ++ "theorem ihp = forall l0: L -> forall k0: Int -> sz(l0) == k0 ==> sz(l0) >= 0 proof generalize k0 => induction l0 "
  ++ "| Nil => ? | Cons(r0) => forall k0 => "
  ++ cite
  ++ " end in ihp";

let test_revert_ih_by_name = () => {
  let (pm, proof) = run_named("ihp", ih_src("revert ih => ?"));
  if (has_mark_kind(
        pm,
        proof,
        fun
        | ProofMark.UnknownFactReverted => true
        | _ => false,
      )) {
    Alcotest.fail(
      "a quantified IH should be revertible BY NAME\n" ++ dump(pm, proof, ""),
    );
  };
};

let test_revert_ih_with_instantiation = () => {
  let (pm, proof) = run_named("ihp", ih_src("revert ih with k0 = 7 => ?"));
  check_mark_free("revert ih with k0 = 7", pm, proof);
  /* The antecedent cashed into the goal is the INSTANCE at 7: the
     quantifier is gone and `7` has replaced the binder. */
  let d = dump(pm, proof, "");
  if (!contains(d, "== 7 ==>")) {
    Alcotest.fail(
      "expected the instantiated antecedent `... == 7 ==> ...` in the "
      ++ "goal:\n"
      ++ d,
    );
  };
};

/* Naming a binder the fact does not quantify over is marked. */
let test_revert_with_unknown_binder = () => {
  let (pm, proof) =
    run_named(
      "g",
      prelude
      ++ quantified
      ++ "theorem g = f(2) == 3 proof revert p with q = 5 => ? in g",
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.RevertFactNotQuantified(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "`revert p with q = ...` should report a non-quantified binder\n"
      ++ dump(pm, proof, ""),
    );
  };
};

/* The gates apply here too. */
let test_revert_with_divergent_is_refused = () => {
  let (pm, proof) =
    run_named(
      "g",
      prelude
      ++ diverger
      ++ quantified
      ++ "theorem g = f(2) == 3 proof revert p with n = bad(1) => ? in g",
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.PossiblyDivergentInstantiation(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "a divergent `revert ... with` instantiation should be refused\n"
      ++ dump(pm, proof, ""),
    );
  };
};

let tests = (
  "Evaluator.Instantiation",
  [
    test_case(
      "control: no `with` is still underdetermined",
      `Quick,
      test_axiom_underdetermined_without_with,
    ),
    test_case(
      "axiom+with resolves an underdetermined rule",
      `Quick,
      test_axiom_with_resolves,
    ),
    test_case(
      "axiomrev+with resolves too",
      `Quick,
      test_axiomrev_with_resolves,
    ),
    test_case(
      "axiom+with: unknown binder name is marked",
      `Quick,
      test_axiom_with_unknown_var,
    ),
    test_case(
      "gate: divergent instantiation is refused",
      `Quick,
      test_axiom_with_divergent_is_refused,
    ),
    test_case(
      "gate: domain condition becomes an obligation",
      `Quick,
      test_axiom_with_domain_condition,
    ),
    test_case(
      "revert+with cashes in the instance",
      `Quick,
      test_revert_with_instantiates,
    ),
    test_case(
      "revert: a quantified IH is citable by name",
      `Quick,
      test_revert_ih_by_name,
    ),
    test_case(
      "revert+with: an IH instantiates by name",
      `Quick,
      test_revert_ih_with_instantiation,
    ),
    test_case(
      "revert+with: non-quantified binder is marked",
      `Quick,
      test_revert_with_unknown_binder,
    ),
    test_case(
      "revert+with: divergent instantiation is refused",
      `Quick,
      test_revert_with_divergent_is_refused,
    ),
  ],
);
