open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* Explicit instantiation at the citation site:
 *
 *   `axiom <name> with <x> = <e> at <i> on <target> end`
 *   `axiomrev <name> with <x> = <e> at <i> on <target> end`
 *   `revert <fact> with <x> = <e> => <proof>`
 *
 * For the axiom step the binding is seeded into `MatchExp`'s match
 * context before matching, so `UnderdeterminedInstantiation` only fires
 * on metavariables still unresolved after seeding. For `revert` the
 * named binder of the (quantified) in-scope fact is eliminated and the
 * instantiated fact is what moves into the goal.
 *
 * The instantiation gate applies to the supplied expression as it does
 * to a matched one: possible divergence is refused, domain conditions
 * become obligations. */

let one_line = (s: string): string =>
  String.map(c => c == '\n' ? ' ' : c, s);

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

   `lem` is a conditional rule whose binder `n` appears in the antecedent
   but not in the conclusion, so matching the conclusion can never
   resolve it. (The quantified inductive hypotheses of
   Test_Milestone_STLC have this shape.) Its own proof is a hole: a
   cited fact need not be proven to be cited. */

let prelude = "let f = fun x -> x + 1 in ";

/* A general-recursive function the totality checker cannot accept: the
   recursive call's argument is arithmetic, not a strict subterm. */
let diverger = "let bad = fun x -> bad(x + 1) in ";

let lem = "theorem lem = forall n: Int -> n == 1 ==> f(2) == 3 proof ? in ";

let src = (rest: string): string => prelude ++ lem ++ rest;

/* --- axiom + with -------------------------------------------------------- */

/* The control: without a `with` clause the citation is refused. */
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
      ++ dump(pm, proof),
    );
  };
};

/* Seeding `n` resolves the rule, the rewrite goes through, and the
   instantiated antecedent `1 == 1` is incurred as an obligation,
   discharged by closed evaluation, so the theorem is fully Proven. */
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
      ++ dump(pm, proof),
    );
  };
};

/* --- The gates apply to the supplied expression ------------------------- */

/* Divergence is refused, never an obligation: a supplied expression
   whose call graph reaches a non-structural fix can't be used to
   instantiate. */
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
      ++ dump(pm, proof),
    );
  };
};

/* A domain error is an obligation: instantiating at `6 / d` incurs
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

/* `p` is a quantified fact in scope. `revert p with n = 5` moves the
   instance `5 + 0 == 5` into the goal, not the quantified statement. */
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

/* A restricted binder keeps its restriction as an antecedent:
   `forall n where n > 0 -> ...` instantiated at 5 is `5 > 0 ==> ...`. */
let test_revert_with_restricted_binder = () => {
  let (pm, proof) =
    run_named(
      "g",
      prelude
      ++ "theorem p = forall n: Int where n > 0 -> n + 0 == n proof ? in "
      ++ "theorem g = f(2) == 3 proof revert p with n = 5 => ? in g",
    );
  check_mark_free("revert+with on a restricted binder", pm, proof);
  let d = dump(pm, proof);
  if (!contains(d, "5 > 0 ==>")) {
    Alcotest.fail(
      "expected the instantiated restriction `5 > 0 ==> ...` in the goal:\n"
      ++ d,
    );
  };
};

/* A quantified inductive hypothesis, cited by name and instantiated, as
   in Test_Milestone_STLC. It covers two things the spelled-out form
   doesn't: by-name resolution of a fact, and identification of a binder
   whose installed name carries substitution-renaming primes the source
   never shows. */
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
      "a quantified IH should be revertible BY NAME\n" ++ dump(pm, proof),
    );
  };
};

let test_revert_ih_with_instantiation = () => {
  let (pm, proof) = run_named("ihp", ih_src("revert ih with k0 = 7 => ?"));
  check_mark_free("revert ih with k0 = 7", pm, proof);
  /* The antecedent moved into the goal is the instance at 7: the
     quantifier is gone and `7` has replaced the binder. */
  let d = dump(pm, proof);
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
      ++ dump(pm, proof),
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
      ++ dump(pm, proof),
    );
  };
};

/* --- Every binder assigned ----------------------------------------------

   `z`'s binder `n` occurs only on its right-hand side, so citing it at a
   bare `0` leaves `n` unassigned, though no assumption mentions it. */

let zero_mul = "theorem z = forall n: Int -> 0 == n * 0 proof ? in ";

let check_underdetermined_passes_through = (src: string) => {
  let (pm, proof) = run_named("g", zero_mul ++ src);
  switch (ProofMap.lookup(Proof.rep_id(proof), pm)) {
  | Some({
      marks: [UnderdeterminedInstantiation({equality: "z"})],
      incoming,
      outgoing,
      _,
    })
      when incoming == outgoing =>
    ()
  | _ =>
    Alcotest.fail(
      "expected the citation refused and the goal unchanged\n"
      ++ dump(pm, proof),
    )
  };
};

let test_unassigned_binder_in_conclusion = () =>
  check_underdetermined_passes_through(
    "theorem g = 0 == 0 proof axiom z at 0 on 0 end in g",
  );

/* The proof's own `n` doesn't count as an assignment of the rule's. */
let test_unassigned_binder_shadowed = () =>
  check_underdetermined_passes_through(
    "theorem g = forall n: Int -> 0 == 0 proof axiom z at 0 on 0 end in g",
  );

let test_discovery_skips_unassigned_binder = () => {
  let rule = ProofRule.exp_to_rule(parse_exp("forall n: Int -> 0 == n * 0"));
  let target = parse_exp("0");
  let (info_map, _) = statics_and_elab(target);
  Alcotest.check(
    Alcotest.bool,
    "not offered at a bare `0`",
    false,
    ProofRule.Rewrite.is_active(
      ~info_map,
      ~env=Environment.empty,
      rule,
      target,
    ),
  );
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
      "revert+with on a restricted binder keeps the restriction",
      `Quick,
      test_revert_with_restricted_binder,
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
    test_case(
      "a binder only in the conclusion must be assigned",
      `Quick,
      test_unassigned_binder_in_conclusion,
    ),
    test_case(
      "a shadowing proof variable doesn't assign a binder",
      `Quick,
      test_unassigned_binder_shadowed,
    ),
    test_case(
      "discovery doesn't offer an unassigned binder",
      `Quick,
      test_discovery_skips_unassigned_binder,
    ),
  ],
);
