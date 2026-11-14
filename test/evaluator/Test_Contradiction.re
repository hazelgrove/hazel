open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* The `contradiction` proof step, ex falso quodlibet as one step.
 *
 * `contradiction F end` / `contradiction F with x = e end` is terminal:
 * it closes any goal by showing that the in-scope fact `F` is false
 * under the rest of the scope's knowledge, so the branch is vacuous.
 * As implemented in `ProofCheck`:
 *
 *   1. `F` is resolved against the in-scope facts as `revert` resolves
 *      its argument (by hypothesis name, or spelled out and
 *      env-substituted, then found with `Exp.fast_equal`).
 *   2. the step's own `with <var> = <exp>` clause, and nothing else, is
 *      applied to `F`, after the checker verifies that `<var> == <exp>`
 *      (either orientation) is an in-scope fact. The checker doesn't
 *      search for equations: the user names the rewrite.
 *   3. the result is evaluated to a fixpoint under the closed-evaluation
 *      fuel bound. `false` ⇒ outgoing `true`, no obligation; anything
 *      else ⇒ mark and pass the goal through.
 *
 * The tests pin the explicit form, the verification of each binding,
 * and the receipt, which is the user's own clause. */

let check_mark = (msg, pm, proof, pred) =>
  if (!has_mark_kind(pm, proof, pred)) {
    Alcotest.fail(msg ++ "\n--- proof dump ---\n" ++ dump(pm, proof));
  };

/* The step's receipt: the binding the checker substituted into the
 * cited fact (the step's own `with` clause), recorded on the
 * Contradiction node's ProofMap entry and rendered as `x := e`. */
let rec find_contradiction = (p: Proof.t): option(Proof.t) =>
  switch (p.term) {
  | Contradiction(_) => Some(p)
  | Seq(a, b) =>
    switch (find_contradiction(a)) {
    | Some(_) as s => s
    | None => find_contradiction(b)
    }
  | Parens(b)
  | Forall(_, b)
  | Funext(_, b)
  | Assume(_, _, b)
  | Alias(_, _, b)
  | Generalize(_, b)
  | Revert(_, _, b) => find_contradiction(b)
  | Induction(_, _, cases) =>
    List.find_map(((_, b)) => find_contradiction(b), cases)
  | Have(_, sub, body) =>
    switch (find_contradiction(sub)) {
    | Some(_) as s => s
    | None => find_contradiction(body)
    }
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | EvalStep => None
  | Focus({body, _}) => find_contradiction(body)
  };

let substitutions_of = (pm: ProofMap.t, proof: Proof.t): list(string) =>
  switch (find_contradiction(proof)) {
  | None => Alcotest.fail("no contradiction step in this proof")
  | Some(c) =>
    switch (ProofMap.lookup(Proof.rep_id(c), pm)) {
    | Some({substitutions, _}) =>
      List.map(
        ((x, v)) => x ++ " := " ++ trunc(print_exp(v)),
        substitutions,
      )
    | None => Alcotest.fail("no proof-map entry for the contradiction step")
    }
  };

/* --- Direct contradiction: the fact is `false` ------------------------ */

/* The degenerate case, needing no substitution at all: `false` is in
 * scope (assume-intro'd from the goal's antecedent), so the branch is
 * vacuous and `contradiction false end` closes the remaining goal. */
let direct_src = "theorem t = forall n: Int -> false ==> n == n proof assume false => contradiction false end in t";

let test_direct = () => {
  let (pm, proof) = run_named("t", direct_src);
  check_mark_free("direct contradiction", pm, proof);
  check_obligation_count("contradiction incurs nothing", 0, pm, proof);
  check_proven("direct contradiction is Proven", pm, proof);
  Alcotest.check(
    Alcotest.(list(string)),
    "no substitutions needed",
    [],
    substitutions_of(pm, proof),
  );
};

/* --- Two conflicting equations on the same variable ------------------- */

/* `n == 1` and `n == 2` cannot both hold. Citing the first one and
 * naming the other equation as the rewrite, `n == 1` becomes `2 == 1`,
 * which evaluates to `false`. The checker confirms `n == 2` really is in
 * scope before substituting; it does not go looking for it. */
let conflicting_src = "theorem t = forall n: Int -> n == 1 ==> n == 2 ==> false proof assume n == 1 => assume n == 2 => contradiction n == 1 with n = 2 end in t";

let test_conflicting = () => {
  let (pm, proof) = run_named("t", conflicting_src);
  check_mark_free("conflicting equations", pm, proof);
  check_obligation_count("conflicting equations incur nothing", 0, pm, proof);
  check_proven("conflicting equations: Proven", pm, proof);
  Alcotest.check(
    Alcotest.(list(string)),
    "receipt records the user's own binding",
    ["n := 2"],
    substitutions_of(pm, proof),
  );
};

/* --- The case_eq case: the metatheory shape --------------------------- */

/* The idiom the step exists for, in miniature: the same shape as the
 * canonical-forms leaves of Test_Milestone_STLC. The split's `case_eq`
 * (`e == S(b)`) rewrites the assumed fact `isz(e) == true` into
 * `isz(S(b)) == true`, which evaluates to `false` even though it is
 * still open in `b`: the match only inspects the constructor.
 *
 * The rewrite is written as `with e = S(b)`. The checker verifies that
 * `e == S(b)` is an in-scope fact (the split's own case_eq) and applies
 * exactly that. */
let adt_prelude = "type Nt = +Z+S(Nt) in let isz = fun e -> case e | Z => true | S(b) => false end in ";

let case_eq_src =
  adt_prelude
  ++ "theorem t = forall e: Nt -> isz(e) == true ==> e == Z proof assume isz(e) == true => induction e "
  /* `induction` substitutes the case pattern into the goal (so the Z
     branch's goal is already `Z == Z`) but not into the hypotheses,
     which is why the `case_eq` substitution below is needed. */
  ++ "| Z => from Z == Z do step end "
  ++ "| S(b) => contradiction isz(e) == true with e = S(b) end "
  ++ "end in t";

let test_case_eq = () => {
  let (pm, proof) = run_named("t", case_eq_src);
  check_mark_free("case_eq substitution", pm, proof);
  check_obligation_count("case_eq substitution incurs nothing", 0, pm, proof);
  check_proven("case_eq substitution: Proven", pm, proof);
  Alcotest.check(
    Alcotest.(list(string)),
    /* Exactly the one binding the user wrote, not also the inductive
       hypothesis `b == Z`. */
    "receipt is exactly the written clause",
    ["e := S(b)"],
    substitutions_of(pm, proof),
  );
};

/* --- No matching fact: mark and pass through -------------------------- */

let unknown_src = "theorem t = forall n: Int -> n == 0 ==> 1 == 1 proof assume n == 0 => contradiction n == 7 end in t";

let test_unknown = () => {
  let (pm, proof) = run_named("t", unknown_src);
  check_mark(
    "contradicting a fact that is not in scope should mark",
    pm,
    proof,
    fun
    | ProofMark.UnknownFactContradicted => true
    | _ => false,
  );
  check_obligation_count(
    "an unmatched contradiction incurs nothing",
    0,
    pm,
    proof,
  );
};

/* --- Found, but it computes to `true` --------------------------------- */

/* The fact is in scope and closed, so it evaluates — to `true`. Nothing
 * is concluded. */
let not_false_src = "theorem t = forall n: Int -> 1 == 1 ==> 1 == 1 proof assume 1 == 1 => contradiction 1 == 1 end in t";

let test_not_false = () => {
  let (pm, proof) = run_named("t", not_false_src);
  check_mark(
    "a fact that computes to `true` should mark",
    pm,
    proof,
    fun
    | ProofMark.ContradictionNotFalse => true
    | _ => false,
  );
  check_obligation_count(
    "a failed contradiction incurs nothing",
    0,
    pm,
    proof,
  );
};

/* --- Found, but still open (and stuck) after substitution ------------- */

/* No `with` clause, so nothing rewrites the cited fact: evaluation gets
 * stuck on the free `n` rather than reaching `false`. Openness itself is
 * not the gate (the case_eq test above is open and succeeds); failing
 * to reach `false` is. */
let open_src = "theorem t = forall n: Int -> n == 0 ==> 1 == 1 proof assume n == 0 => contradiction n == 0 end in t";

let test_open = () => {
  let (pm, proof) = run_named("t", open_src);
  check_mark(
    "a fact still open after substitution should mark",
    pm,
    proof,
    fun
    | ProofMark.ContradictionNotFalse => true
    | _ => false,
  );
};

/* --- An unverifiable `with` binding: mark, don't trust ---------------- */

/* `n == 7` is not in scope (only `n == 0` is), so the rewrite the user
 * asked for is not licensed by anything the branch knows, and the step
 * marks it rather than substituting. The cited fact is found, so this
 * is a different failure from `UnknownFactContradicted`. */
let unverified_src = "theorem t = forall n: Int -> n == 0 ==> 1 == 1 proof assume n == 0 => contradiction n == 0 with n = 7 end in t";

let test_unverified = () => {
  let (pm, proof) = run_named("t", unverified_src);
  check_mark(
    "a `with` binding with no licensing equation in scope should mark",
    pm,
    proof,
    fun
    | ProofMark.ContradictionSubstitutionUnverified(_) => true
    | _ => false,
  );
  check_obligation_count(
    "an unverified substitution incurs nothing",
    0,
    pm,
    proof,
  );
  Alcotest.check(
    Alcotest.(list(string)),
    "a refused binding is not recorded as a receipt",
    [],
    substitutions_of(pm, proof),
  );
};

/* The mirror image: the equation is in scope, written in the other
 * orientation (`0 == n` rather than `n == 0`). `==` is symmetric, so the
 * checker accepts either. Here `n == 1` rewritten by `n := 0` gives
 * `0 == 1`, which evaluates to `false`. */
let flipped_src = "theorem t = forall n: Int -> 0 == n ==> n == 1 ==> false proof assume 0 == n => assume n == 1 => contradiction n == 1 with n = 0 end in t";

let test_flipped = () => {
  let (pm, proof) = run_named("t", flipped_src);
  check_mark_free("flipped equation orientation", pm, proof);
  check_proven("flipped equation orientation: Proven", pm, proof);
  Alcotest.check(
    Alcotest.(list(string)),
    "receipt records the binding as written",
    ["n := 0"],
    substitutions_of(pm, proof),
  );
};

let tests = (
  "Evaluator.Contradiction",
  [
    test_case("direct contradiction on `false`", `Quick, test_direct),
    test_case("conflicting variable equations", `Quick, test_conflicting),
    test_case(
      "case_eq substitution (metatheory shape)",
      `Quick,
      test_case_eq,
    ),
    test_case("unknown fact marks", `Quick, test_unknown),
    test_case("fact computing to `true` marks", `Quick, test_not_false),
    test_case("fact still open after substitution marks", `Quick, test_open),
    test_case("an unlicensed `with` binding marks", `Quick, test_unverified),
    test_case(
      "either orientation of the licensing equation is accepted",
      `Quick,
      test_flipped,
    ),
  ],
);
