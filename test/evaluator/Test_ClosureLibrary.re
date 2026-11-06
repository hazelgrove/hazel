open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* How far the six arithmetic closure lemmas of
 * `src/language/proof/Axioms.re` can be proved in Hazel itself, rather
 * than trusted as axioms.
 *
 * None of the six can be, and all for the same reason. Rewriting can only
 * relate two predicates when some cited equation mentions both, and no
 * built-in axiom mentions two different arithmetic comparisons: the
 * Kleene axioms are purely boolean (`and_comm`, `impl_def`, ...),
 * `refl_eq` is reflexivity, and the only equations relating `>` to `!=`,
 * or `a > 0`/`b > 0` to `a * b > 0`, are the six closure lemmas
 * themselves, which would be circular. Evaluation can't bridge the gap
 * either: on a symbolic operand a comparison is stuck (`NothingToStep`,
 * `test_pin_symbolic_comparison_is_stuck`). Proving them needs
 * monotonicity reasoning over ordered arithmetic, and
 * `pow_pos` also needs induction on the exponent, which no Hazel numeric
 * type provides (`test_pin_no_numeric_induction_hypothesis`).
 *
 * Nor can one lemma be proved from another: no two share a pair of
 * comparisons. (`pos_mul` + `nonzero_of_pos` give `nonzero_mul` only for
 * positive arguments.)
 *
 * So the file pins, as Test_Milestone_STLC does, each lemma's longest
 * mark-free partial proof, its status, its obligations, and a `FRICTION:`
 * note naming what is missing. Also:
 *   - each lemma's true branch is closed (the bool split substitutes in
 *     the goal, leaving the literal `true`), so half of each proof is
 *     real;
 *   - `nonzero_of_pos`'s a1 == 0 sub-case closes by the revert-based ex
 *     falso idiom (`test_zero_case_closes_by_ex_falso`);
 *   - every lemma's closed instances are fully proven by evaluation
 *     (section D). */

let has_mark_kind = Test_ProofMap.has_mark_kind;

let is_remote = (ob: Obligation.t): bool =>
  switch (ob.discharge) {
  | Remote(_) => true
  | _ => false
  };

let is_evaluated = (ob: Obligation.t): bool => ob.discharge == Evaluated;

/* --- Section A: the six statements ------------------------------------- */

/* Each is the corresponding `Axioms.re` entry transcribed into surface
 * syntax, identical up to binder names (a/b -> a1/b1). The Axioms.re
 * builder is quoted above each one so the correspondence is checkable by
 * eye. `==>` is right-associative and the nested antecedent is written
 * without parentheses: a `Parens` node there defeats assume-intro
 * (`test_pin_parens_defeat_assume_intro`), and the paren-free form is
 * the shape `Axioms.foralls` builds.
 *
 * The conclusions are bare booleans, read as `P == true`
 * (`ProofRule.BoolReading`). */

/* forall("a", gt0(a) ==>> neq0(a)) */
let stmt_nonzero_of_pos = "forall a1 -> a1 > 0 ==> a1 != 0";

/* forall("a", lt0(a) ==>> neq0(a)) */
let stmt_nonzero_of_neg = "forall a1 -> a1 < 0 ==> a1 != 0";

/* foralls(["a","b"], neq0(a) ==>> (neq0(b) ==>> neq0(mul(a,b)))) */
let stmt_nonzero_mul = "forall a1 -> forall b1 -> a1 != 0 ==> b1 != 0 ==> a1 * b1 != 0";

/* foralls(["a","b"], gt0(a) ==>> (gt0(b) ==>> gt0(mul(a,b)))) */
let stmt_pos_mul = "forall a1 -> forall b1 -> a1 > 0 ==> b1 > 0 ==> a1 * b1 > 0";

/* foralls(["a","b"], gt0(a) ==>> (gt0(b) ==>> gt0(add(a,b)))) */
let stmt_pos_add = "forall a1 -> forall b1 -> a1 > 0 ==> b1 > 0 ==> a1 + b1 > 0";

/* foralls(["a","b"], gt0(a) ==>> (geq0(b) ==>> gt0(pow(a,b)))) */
let stmt_pow_pos = "forall a1 -> forall b1 -> a1 > 0 ==> b1 >= 0 ==> a1 ** b1 > 0";

let theorem = (name: string, stmt: string, proof: string): string =>
  "theorem " ++ name ++ " = " ++ stmt ++ " proof " ++ proof ++ " in " ++ name;

/* --- Section B: the six partial proofs ---------------------------------- */

/* The shared proof skeleton. The goal core of every closure lemma is
 * the bare predicate `P`, so:
 *   1. assume-intro each antecedent (no obligation);
 *   2. bool-split on P itself. The split substitutes in the goal here
 *      (P is written exactly as it appears, with no env inlining,
 *      unlike Test_Milestone_STLC's computed scrutinees), so the
 *      branches come out as the literals `true` and `false`;
 *   3. the true branch's goal is then `true`, and the hole passes it
 *      through;
 *   4. the false branch is where the arithmetic content is, and where
 *      the proof gets stuck.
 *
 * FRICTION (all six): in the false branch the goal is `false` with the
 * split's `case_eq` (`P == false`) and the intro'd antecedents in scope.
 * Closing it means deriving a contradiction between two different
 * arithmetic comparisons over a symbolic Int (e.g. `a1 > 0` against
 * `(a1 != 0) == false`). Every available move fails:
 *   - Rewriting needs an equation mentioning both comparisons. The only
 *     such equations in scope are the six closure lemmas themselves
 *     (circular).
 *   - Evaluation is stuck: `a1 > 0` on a symbolic `a1` yields
 *     `NothingToStep`, so reverting the antecedent into the goal gives
 *     `a1 > 0 ==> false`, which can't be falsified. Ex falso works only
 *     once `a1` is a literal (`test_zero_case_closes_by_ex_falso`).
 *   - Splitting further doesn't help: Int/Nat case analysis is
 *     `| 0 => | n =>` (a literal and a catch-all,
 *     `test_pin_no_numeric_induction_hypothesis`), so the catch-all
 *     branch reproduces the original goal with no inductive hypothesis.
 *   - Algebrite would "close" it, but only because the checker doesn't
 *     re-verify the CAS equation (`test_pin_algebrite_launders_the_lemma`),
 *     which would rest the lemma on an unchecked step. */
let partial_proof = (antecedents: list(string), predicate: string): string =>
  List.fold_right(
    (a, rest) => "assume " ++ a ++ " => " ++ rest,
    antecedents,
    "induction " ++ predicate ++ " | true => ? | false => ? end",
  );

let src_nonzero_of_pos =
  theorem(
    "nonzero_of_pos_proved",
    stmt_nonzero_of_pos,
    partial_proof(["a1 > 0"], "a1 != 0"),
  );

let src_nonzero_of_neg =
  theorem(
    "nonzero_of_neg_proved",
    stmt_nonzero_of_neg,
    partial_proof(["a1 < 0"], "a1 != 0"),
  );

let src_nonzero_mul =
  theorem(
    "nonzero_mul_proved",
    stmt_nonzero_mul,
    partial_proof(["a1 != 0", "b1 != 0"], "a1 * b1 != 0"),
  );

let src_pos_mul =
  theorem(
    "pos_mul_proved",
    stmt_pos_mul,
    partial_proof(["a1 > 0", "b1 > 0"], "a1 * b1 > 0"),
  );

let src_pos_add =
  theorem(
    "pos_add_proved",
    stmt_pos_add,
    partial_proof(["a1 > 0", "b1 > 0"], "a1 + b1 > 0"),
  );

let src_pow_pos =
  theorem(
    "pow_pos_proved",
    stmt_pow_pos,
    partial_proof(["a1 > 0", "b1 >= 0"], "a1 ** b1 > 0"),
  );

/* Every partial proof pins identically: mark-free (every retained step
 * goes through), no pending obligations, and `Incomplete`, with one
 * hole per lemma. The true branch really is proven; only the
 * false branch is open. */
let check_partial = (name: string, src: string, expected_obs: int): unit => {
  let (pm, proof) = run_named(name, src);
  check_mark_free(name, pm, proof);
  check_obligation_count(
    name ++ ": obligation count",
    expected_obs,
    pm,
    proof,
  );
  if (List.exists(
        Obligation.is_pending,
        ProofMap.obligations_of_proof(pm, proof),
      )) {
    Alcotest.fail(
      name ++ ": no obligation should be Pending\n" ++ dump(pm, proof),
    );
  };
  check_status(
    name ++ " pins as Incomplete (one documented false-branch hole)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let test_nonzero_of_pos_partial = () =>
  check_partial("nonzero_of_pos_proved", src_nonzero_of_pos, 0);
let test_nonzero_of_neg_partial = () =>
  check_partial("nonzero_of_neg_proved", src_nonzero_of_neg, 0);
let test_nonzero_mul_partial = () =>
  check_partial("nonzero_mul_proved", src_nonzero_mul, 0);
let test_pos_mul_partial = () =>
  check_partial("pos_mul_proved", src_pos_mul, 0);
let test_pos_add_partial = () =>
  check_partial("pos_add_proved", src_pos_add, 0);

/* pow_pos is the only one of the six that incurs an obligation: the bool
 * split's domain scan sees `a1 ** b1` and emits `b1 >= 0`, which
 * discharges Remote against the intro'd second antecedent. */
let test_pow_pos_partial = () => {
  check_partial("pow_pos_proved", src_pow_pos, 1);
  let (pm, proof) = run_named("pow_pos_proved", src_pow_pos);
  switch (ProofMap.obligations_of_proof(pm, proof)) {
  | [ob] =>
    Test_ProofMap.check_exp(
      "pow_pos's split emits the ** domain condition",
      "b1 >= 0",
      ob.goal,
    );
    Alcotest.check(
      Alcotest.bool,
      "discharged Remote against the intro'd antecedent",
      true,
      is_remote(ob),
    );
  | obs =>
    Alcotest.fail(
      "expected exactly one obligation, got "
      ++ string_of_int(List.length(obs)),
    )
  };
};

/* The half that is proven: the true branch of each split reaches
 * `true`, so "Incomplete" above is one open branch rather than a proof
 * that never started. The split closes the branch: its goal is already
 * `true`.
 *
 * The false branch's goal is the literal `false`, and
 * `ProofMap.status_of_proof` reads a `false` outgoing as refuted, so the
 * open branch of a partially proven boolean lemma reports `Some(false)`,
 * not `None`. That is sound, and the theorem's own status is still
 * Incomplete (`check_partial`): the branch has been driven to `false`,
 * and what remains is to contradict it against the split's `case_eq`.
 * But the stepper shows such a branch as disproven rather than
 * unknown. */
let test_true_branch_is_really_closed = () => {
  let (pm, proof) = run_named("nonzero_of_pos_proved", src_nonzero_of_pos);
  let rec find_induction = (p: Proof.t): option(Proof.t) =>
    switch (p.term) {
    | Induction(_, _, _) => Some(p)
    | Assume(_, _, b)
    | Forall(_, b)
    | Generalize(_, b)
    | Revert(_, _, b) => find_induction(b)
    | Seq(a, b) =>
      switch (find_induction(a)) {
      | Some(x) => Some(x)
      | None => find_induction(b)
      }
    | _ => None
    };
  switch (find_induction(proof)) {
  | Some({term: Induction(_, _, [(_, true_body), (_, false_body)]), _}) =>
    Alcotest.check(
      Alcotest.option(bool),
      "the true branch proves (outgoing is literal true)",
      Some(true),
      ProofMap.status_of_proof(pm, true_body),
    );
    Alcotest.check(
      Alcotest.option(bool),
      "the false branch is the open one, driven to the literal `false`",
      Some(false),
      ProofMap.status_of_proof(pm, false_body),
    );
  | _ => Alcotest.fail("expected a two-case bool split\n" ++ dump(pm, proof))
  };
};

/* --- Section C: friction pins -------------------------------------------
 *
 * The checker facts the header's conclusion rests on. Each fails if the
 * limitation goes away, which is the signal to revisit this file. */

/* (C1) A comparison on a symbolic operand doesn't step, so closed
 * evaluation can't decide it and ex falso has nothing to compute
 * with. */
let test_pin_symbolic_comparison_is_stuck = () => {
  let (pm, proof) =
    run_named(
      "t",
      theorem(
        "t",
        stmt_nonzero_of_pos,
        "assume a1 > 0 => eval a1 != 0 at 0 end; ?",
      ),
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.NothingToStep(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "a symbolic `a1 != 0` unexpectedly stepped -- evaluation gained "
      ++ "symbolic arithmetic; revisit the closure-library verdict!\n"
      ++ dump(pm, proof),
    );
  };
};

/* (C2) No numeric induction. `induction` on an Int or Nat binder is
 * accepted, but the only patterns available are a literal and a
 * catch-all variable: there is no successor pattern, hence no inductive
 * hypothesis (`ih` is not even in scope). This is what blocks `pow_pos`:
 * its proof wants induction on the exponent with a quantified IH, and
 * `generalize` can supply the quantification but there is no induction
 * principle to hand it to.
 *
 * Making the exponent a `Nat` doesn't help: Nat is an atom type, not an
 * inductive one (the probe below is over Nat and still has no `ih`), and
 * `**` parses to the Int operator class, so a Nat-typed variant would be
 * a lemma about a different operator. */
let test_pin_no_numeric_induction_hypothesis = () => {
  let (pm, proof) =
    run_named(
      "t",
      "theorem t = forall a1: Nat -> a1 != 0 proof induction a1 "
      ++ "| 0 => ? | n => axiom ih at 0 on n != 0 end end in t",
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnknownEquality("ih") => true
          | _ => false,
        )) {
    Alcotest.fail(
      "a numeric induction produced an `ih` -- an induction principle "
      ++ "for Nat/Int landed; pow_pos may now be provable!\n"
      ++ dump(pm, proof),
    );
  };
};

/* (C3) Algebrite, pinned as a hazard. `rewrite (a1 != 0) with true`
 * drives `nonzero_of_pos` to fully Proven, not because the CAS agrees
 * (this is a boolean comparison, not a field identity) but because the
 * checker doesn't re-verify an Algebrite step's equation (see the TODO
 * in ProofCheck's AlgebriteStep arm). The Float gate is the only content
 * gate, and a Bool-typed rewrite passes it.
 *
 * So Algebrite can't be used to prove the closure lemmas: a lemma
 * proven through it would rest on an unchecked step. When the checker
 * re-verifies CAS steps this test will fail. */
let test_pin_algebrite_launders_the_lemma = () => {
  let (pm, proof) =
    run_named(
      "t",
      theorem(
        "t",
        stmt_nonzero_of_pos,
        "assume a1 > 0 => rewrite a1 != 0 with true at 0 end",
      ),
    );
  check_status(
    "an unverified Algebrite rewrite still launders the lemma to Proven "
    ++ "(if this now fails, checker-side CAS verification landed)",
    ProofMap.Proven,
    pm,
    proof,
  );
};

/* (C4) A `Parens` node around the nested antecedent defeats
 * assume-intro. `ProofCheck`'s intro test matches `goal |> Exp.term_of`
 * against `BinOp(Bool(Implies), a, b)`, so with the goal written
 * `A ==> (B ==> C)` (how `Axioms.re`'s `==>>` reads, and the natural way
 * to transcribe a nested implication) the second `assume B` is not an
 * intro: it incurs `B` as an obligation, which stays Pending. The
 * paren-free spelling used throughout this file avoids it. TODO: peel
 * Parens in the intro test. */
let test_pin_parens_defeat_assume_intro = () => {
  let parenthesised = "forall a1 -> forall b1 -> a1 != 0 ==> (b1 != 0 ==> a1 * b1 != 0)";
  let (pm, proof) =
    run_named(
      "t",
      theorem(
        "t",
        parenthesised,
        partial_proof(["a1 != 0", "b1 != 0"], "a1 * b1 != 0"),
      ),
    );
  let obs = ProofMap.obligations_of_proof(pm, proof);
  Alcotest.check(
    Alcotest.int,
    "the parenthesised nested antecedent is baked, not introduced",
    1,
    List.length(obs),
  );
  Alcotest.check(
    Alcotest.bool,
    "and its obligation goes Pending",
    true,
    List.for_all(Obligation.is_pending, obs),
  );
  /* Control: the same statement without the parens introduces both
     antecedents free of charge. */
  let (pm', proof') = run_named("nonzero_mul_proved", src_nonzero_mul);
  check_obligation_count(
    "control: paren-free intro incurs nothing",
    0,
    pm',
    proof',
  );
  ignore(dump(pm, proof));
};

/* (C5) The false branch's a1 == 0 sub-case does close, by the
 * revert-based ex falso idiom. Split the symbolic `a1` into the literal
 * `0` and a catch-all; in the `0` branch the split's `case_eq` is
 * `a1 == 0`, which rewrites `a1` to `0` inside the reverted antecedent,
 * evaluation falsifies `0 > 0`, and `==>` collapses the goal to `true`.
 * Only the catch-all branch (an arbitrary Int) stays open: the missing
 * induction principle of (C2). The obstacle is symbolic operands, not
 * the idiom. */
let ex_falso_src =
  theorem(
    "t",
    stmt_nonzero_of_pos,
    "assume a1 > 0 => induction a1 != 0 "
    ++ "| true => ? "
    ++ "| false => induction a1 "
    /* Inside the inner `a1` split, `case_eq` denotes that split's
     * equation, which shadows the enclosing `a1 != 0` one. */
    ++ "| 0 => revert a1 > 0 => axiom case_eq at 0 on a1 end; "
    ++ "eval 0 > 0 at 0 end; eval false ==> false at 0 end "
    ++ "| n => ? end end",
  );

let test_zero_case_closes_by_ex_falso = () => {
  let (pm, proof) = run_named("t", ex_falso_src);
  check_mark_free("ex falso probe", pm, proof);
  let rec find_revert = (p: Proof.t): option(Proof.t) =>
    switch (p.term) {
    | Revert(_, _, _) => Some(p)
    | Assume(_, _, b)
    | Forall(_, b)
    | Generalize(_, b) => find_revert(b)
    | Seq(a, b) =>
      switch (find_revert(a)) {
      | Some(x) => Some(x)
      | None => find_revert(b)
      }
    | Induction(_, _, cases) =>
      List.fold_left(
        (acc, (_, body)) =>
          switch (acc) {
          | Some(_) => acc
          | None => find_revert(body)
          },
        None,
        cases,
      )
    | _ => None
    };
  switch (find_revert(proof)) {
  | Some(revert_node) =>
    Alcotest.check(
      Alcotest.option(bool),
      "the a1 == 0 sub-case closes outright (ex falso)",
      Some(true),
      ProofMap.status_of_proof(pm, revert_node),
    )
  | None => Alcotest.fail("no revert node found\n" ++ dump(pm, proof))
  };
  check_status(
    "the lemma is still Incomplete (the catch-all branch is open)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

/* --- Section D: closed instances --------------------------------------
 *
 * The lemmas can't be proved in general, but every closed instance of
 * each (the whole `==>` statement instantiated at literals) is decided
 * by evaluation, and assuming one discharges as `Evaluated`. That
 * spot-checks that every closed instance of a theorem evaluates to
 * true. The same `closed_fact` idiom as in Test_Milestone_STLC.
 *
 * Two instances per lemma, including boundary cases: a negative factor
 * for `nonzero_mul`, `b1 == 0` for `pow_pos` (5 ** 0 == 1 > 0, the case
 * the `b >= 0` guard exists to admit while excluding negatives). */

let closed_instance = (name: string, fact: string): string =>
  theorem(
    name,
    "1 == 1",
    "assume " ++ fact ++ " => axiom refl_eq at 0 on 1 == 1 end",
  );

let instances: list((string, string)) = [
  ("nonzero_of_pos_at_7", "7 > 0 ==> 7 != 0"),
  ("nonzero_of_pos_at_1", "1 > 0 ==> 1 != 0"),
  ("nonzero_of_neg_at_neg3", "-3 < 0 ==> -3 != 0"),
  ("nonzero_of_neg_at_neg1", "-1 < 0 ==> -1 != 0"),
  ("nonzero_mul_at_2_3", "2 != 0 ==> 3 != 0 ==> 2 * 3 != 0"),
  ("nonzero_mul_at_neg2_5", "-2 != 0 ==> 5 != 0 ==> -2 * 5 != 0"),
  ("pos_mul_at_2_3", "2 > 0 ==> 3 > 0 ==> 2 * 3 > 0"),
  ("pos_mul_at_1_1", "1 > 0 ==> 1 > 0 ==> 1 * 1 > 0"),
  ("pos_add_at_2_3", "2 > 0 ==> 3 > 0 ==> 2 + 3 > 0"),
  ("pos_add_at_1_7", "1 > 0 ==> 7 > 0 ==> 1 + 7 > 0"),
  ("pow_pos_at_2_3", "2 > 0 ==> 3 >= 0 ==> 2 ** 3 > 0"),
  ("pow_pos_at_5_0", "5 > 0 ==> 0 >= 0 ==> 5 ** 0 > 0"),
];

let check_instance = ((name, fact)): unit => {
  let src = closed_instance(name, fact);
  let (pm, proof) = run_named(name, src);
  check_mark_free(name, pm, proof);
  switch (ProofMap.obligations_of_proof(pm, proof)) {
  | [ob] when is_evaluated(ob) => ()
  | obs =>
    Alcotest.fail(
      name
      ++ ": expected exactly one Evaluated obligation, got "
      ++ String.concat(", ", List.map(show_obligation, obs))
      ++ "\n"
      ++ dump(pm, proof),
    )
  };
  check_status(name ++ " is fully Proven", ProofMap.Proven, pm, proof);
};

let test_instances = () => List.iter(check_instance, instances);

/* --- How the obligations are discharged -------------------------------
 *
 * Every obligation the file incurs, by how it was discharged: all 13 by
 * lookup or closed evaluation. That says nothing about proof
 * completeness: the six open false branches incur no obligation. */

let all_workload: list((string, string)) =
  [
    ("nonzero_of_pos_proved", src_nonzero_of_pos),
    ("nonzero_of_neg_proved", src_nonzero_of_neg),
    ("nonzero_mul_proved", src_nonzero_mul),
    ("pos_mul_proved", src_pos_mul),
    ("pos_add_proved", src_pos_add),
    ("pow_pos_proved", src_pow_pos),
  ]
  @ List.map(
      ((name, fact)) => (name, closed_instance(name, fact)),
      instances,
    );

let test_channel_distribution = () => {
  let (remote, evaluated, pending) =
    List.fold_left(
      ((r, e, p), (name, src)) => {
        let (pm, proof) = run_named(name, src);
        List.fold_left(
          ((r, e, p), ob: Obligation.t) =>
            switch (ob.discharge) {
            | Remote(_) => (r + 1, e, p)
            | Evaluated => (r, e + 1, p)
            | Pending => (r, e, p + 1)
            },
          (r, e, p),
          ProofMap.obligations_of_proof(pm, proof),
        );
      },
      (0, 0, 0),
      all_workload,
    );

  Alcotest.check(
    Alcotest.int,
    "lookup: pow_pos's ** domain condition",
    1,
    remote,
  );
  Alcotest.check(
    Alcotest.int,
    "closed evaluation: the 12 closed instances",
    12,
    evaluated,
  );
  Alcotest.check(Alcotest.int, "nothing pending", 0, pending);
};

let tests = (
  "Evaluator.ClosureLibrary",
  [
    test_case(
      "nonzero_of_pos: partial, blocked on symbolic order reasoning",
      `Quick,
      test_nonzero_of_pos_partial,
    ),
    test_case(
      "nonzero_of_neg: partial, blocked on symbolic order reasoning",
      `Quick,
      test_nonzero_of_neg_partial,
    ),
    test_case(
      "nonzero_mul: partial, blocked on symbolic product reasoning",
      `Quick,
      test_nonzero_mul_partial,
    ),
    test_case(
      "pos_mul: partial, blocked on sign monotonicity of *",
      `Quick,
      test_pos_mul_partial,
    ),
    test_case(
      "pos_add: partial, blocked on monotonicity of +",
      `Quick,
      test_pos_add_partial,
    ),
    test_case(
      "pow_pos: partial; its ** domain condition discharges Remote",
      `Quick,
      test_pow_pos_partial,
    ),
    test_case(
      "the split's true branch is genuinely closed",
      `Quick,
      test_true_branch_is_really_closed,
    ),
    test_case(
      "pin: a symbolic comparison does not step",
      `Quick,
      test_pin_symbolic_comparison_is_stuck,
    ),
    test_case(
      "pin: numeric induction generates no inductive hypothesis",
      `Quick,
      test_pin_no_numeric_induction_hypothesis,
    ),
    test_case(
      "pin: an unverified Algebrite rewrite launders the lemma",
      `Quick,
      test_pin_algebrite_launders_the_lemma,
    ),
    test_case(
      "pin: Parens around a nested antecedent defeats assume-intro",
      `Quick,
      test_pin_parens_defeat_assume_intro,
    ),
    test_case(
      "the a1 == 0 sub-case closes by ex falso",
      `Quick,
      test_zero_case_closes_by_ex_falso,
    ),
    test_case(
      "closed instances are fully proven, guards by evaluation",
      `Quick,
      test_instances,
    ),
    test_case(
      "every obligation discharges by lookup or evaluation",
      `Quick,
      test_channel_distribution,
    ),
  ],
);
