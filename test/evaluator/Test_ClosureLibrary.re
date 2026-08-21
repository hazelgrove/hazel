open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Closure-library self-hosting study (docs/prover-obligations.md §4.2,
 * channel 3).
 *
 * The six arithmetic closure lemmas ship as trusted OCaml-side axioms in
 * `src/language/proof/Axioms.re`, with a recorded decision to migrate
 * them to a self-hosted proven Hazel prelude "once Phase 4 makes them
 * provable". Phase 4 has landed (assume/revert/generalize, conditional
 * rules, bool splits, eval steps, algebrite, discharge channels). This
 * file asks the question the decision deferred: ARE they provable now?
 *
 * VERDICT: no — 0 of 6. Every one of the six is blocked at the SAME
 * wall, and it is not a Phase-4 gap:
 *
 *   The proof calculus is rewriting (docs/prover-obligations.md §2.1:
 *   "the rewriting architecture is the proof calculus"). Rewriting can
 *   only relate two predicates when some cited equation mentions both.
 *   No axiom in `Axioms.initial_hypotheses` mentions two DIFFERENT
 *   arithmetic comparisons: the Kleene set is purely boolean
 *   (`and_comm`, `impl_def`, ...), `refl_eq` is reflexivity, and the
 *   only equations relating `>` to `!=`, or `a > 0`/`b > 0` to
 *   `a * b > 0`, ARE the six closure lemmas themselves — citing which
 *   would be circular. Evaluation cannot bridge the gap either: on a
 *   symbolic operand a comparison is stuck (`NothingToStep`, pinned in
 *   `test_pin_symbolic_comparison_is_stuck`).
 *
 * So the missing vocabulary is **Phase 5**, not Phase 4: the
 * polarity/variance engine over ordered arithmetic, whose sign side
 * conditions are ordinary obligations (§5, "monotonicity lemmas as
 * conditional rules"). `pow_pos` additionally needs an inductive
 * exponent, which no Hazel numeric type provides
 * (`test_pin_no_numeric_induction_hypothesis`).
 *
 * DEPENDENCY ORDER among the six: none exists. The task allowed citing
 * an already-proven sibling; since none is provable, no sibling is
 * available, and in any case the six are pairwise independent — no two
 * of them share a pair of comparisons that would let one bridge
 * another. (The one genuine implication between them,
 * `pos_mul` + `nonzero_of_pos` ==> `nonzero_mul`'s positive cases, does
 * not help: it covers only positive arguments, while `nonzero_mul`
 * quantifies over all nonzero ones.)
 *
 * What this file therefore pins, milestone-style
 * (`Test_Milestone_STLC.re`): for each lemma the maximal MARK-FREE
 * partial proof, its exact intermediate status, its obligation
 * distribution, and a `FRICTION:` note naming the precise missing
 * vocabulary. Plus, so the study is not purely negative:
 *   - each lemma's TRUE branch is genuinely closed in-language (the
 *     bool split does substitute in the goal, leaving the literal
 *     `true`), so exactly half of each proof is real;
 *   - `nonzero_of_pos`'s a1 == 0 sub-case closes outright by the
 *     Phase-4c ex-falso idiom (`test_zero_case_closes_by_ex_falso`);
 *   - every lemma's closed INSTANCES are fully proven by evaluation,
 *     with their guards discharged through channel 2 — the §1.3
 *     operational guarantee, spot-checked (section D).
 *
 * Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;
let print_exp = Test_ProofMap.print_exp;

let run_named = (name: string, src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of_named(name, elab));
};

/* --- Diagnostic dump (same shape as the STLC milestone's) --------------- */

let trunc = (s: string): string => {
  let s = String.map(c => c == '\n' ? ' ' : c, s);
  String.length(s) > 400 ? String.sub(s, 0, 400) ++ " ..." : s;
};

let opt_exp_str = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => trunc(print_exp(e))
  | None => "<none>"
  };

let rec dump_proof = (pm: ProofMap.t, p: Proof.t, indent: string): string => {
  let entry = ProofMap.lookup(Proof.rep_id(p), pm);
  let (inc, out, marks, obs) =
    switch (entry) {
    | Some({incoming, outgoing, marks, obligations, _}) => (
        opt_exp_str(incoming),
        opt_exp_str(outgoing),
        marks
        |> List.map(m => trunc(ProofMark.show(m)))
        |> String.concat("; "),
        obligations
        |> List.map((ob: Obligation.t) =>
             trunc(print_exp(ob.goal))
             ++ " ["
             ++ Obligation.show_discharge(ob.discharge)
             ++ "]"
           )
        |> String.concat("; "),
      )
    | None => ("<no entry>", "", "", "")
    };
  let kind =
    switch (p.term) {
    | EmptyHole => "hole"
    | Invalid(_) => "invalid"
    | MultiHole(_) => "multihole"
    | Seq(_) => "seq"
    | Forall(_) => "forall"
    | Assume(_) => "assume"
    | Generalize(_) => "generalize"
    | Revert(_) => "revert"
    | Induction(_) => "induction"
    | AxiomStep(_) => "axiom"
    | AlgebriteStep(_) => "rewrite"
    | EvalStep(_) => "eval"
    };
  let here =
    indent
    ++ kind
    ++ "\n"
    ++ indent
    ++ "  in:  "
    ++ inc
    ++ "\n"
    ++ indent
    ++ "  out: "
    ++ out
    ++ (marks == "" ? "" : "\n" ++ indent ++ "  marks: " ++ marks)
    ++ (obs == "" ? "" : "\n" ++ indent ++ "  obs: " ++ obs);
  let children =
    switch (p.term) {
    | Seq(a, b) => [dump_proof(pm, a, indent), dump_proof(pm, b, indent)]
    | Forall(_, b)
    | Assume(_, b)
    | Generalize(_, b)
    | Revert(_, _, b) => [dump_proof(pm, b, indent ++ "  ")]
    | Induction(_, cases) =>
      List.mapi(
        (i, (_pat, body)) =>
          indent
          ++ "| case "
          ++ string_of_int(i)
          ++ "\n"
          ++ dump_proof(pm, body, indent ++ "  "),
        cases,
      )
    | _ => []
    };
  String.concat("\n", [here, ...children]);
};

let dump = (pm, proof): string => dump_proof(pm, proof, "");

let check_full_status = (msg, expected: ProofMap.full_status, pm, proof): unit =>
  if (ProofMap.full_status_of_proof(pm, proof) != expected) {
    Alcotest.fail(
      msg
      ++ "\nexpected: "
      ++ ProofMap.show_full_status(expected)
      ++ "\ngot:      "
      ++ ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof))
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof),
    );
  };

let check_mark_free = (msg, pm, proof): unit =>
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(msg ++ " should be mark-free\n" ++ dump(pm, proof));
  };

let check_obligation_count = (msg, expected, pm, proof): unit => {
  let obs = ProofMap.obligations_of_proof(pm, proof);
  if (List.length(obs) != expected) {
    Alcotest.fail(
      msg
      ++ "\nexpected "
      ++ string_of_int(expected)
      ++ " obligations, got "
      ++ string_of_int(List.length(obs))
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof),
    );
  };
};

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
 * WITHOUT parentheses on purpose — a `Parens` node there defeats
 * assume-intro (`test_pin_parens_defeat_assume_intro`), and the
 * paren-free form is also the exact shape `Axioms.foralls` builds.
 *
 * The conclusions are BARE BOOLEANS. They used to be written
 * `((a1 != 0) == true)`, mirroring an `Axioms.re` that wrapped every
 * closure lemma in `eq(..., tt)` so its conclusion would classify as an
 * Equality and the checker would accept the rule. That `== true` is now
 * the reading rather than the notation
 * (`ProofRule.with_bool_fact_reading`), so both the library and these
 * transcriptions say what they mean. The change is visible in the proof
 * skeleton below: the bool split's true branch used to arrive at
 * `true == true` and need a ceremonial `refl_eq`; it now arrives at
 * `true` and is simply done. */

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
 *   1. assume-intro each antecedent (free: no obligation, §2.1);
 *   2. bool-split on P itself. The split DOES substitute in the goal
 *      here (P is written exactly as it appears, no env inlining
 *      involved, unlike the STLC milestone's computed scrutinees), so
 *      the branches come out as the literals `true` and `false`;
 *   3. the true branch is then ALREADY the goal `true` and closes with
 *      no step at all — the hole passes it through
 *      (`ProofCheck`'s EmptyHole case). Under the old `(P == true)`
 *      spelling this branch arrived at `true == true` and needed a
 *      `refl_eq` step whose only job was to undo the `== true` the
 *      statement never needed;
 *   4. the false branch is where the arithmetic content lives, and it
 *      is exactly where the vocabulary runs out.
 *
 * FRICTION (all six, one shared wall): in the false branch the goal is
 * `false` with the split's `case_eq` (`P == false`) and the intro'd
 * antecedents in scope. Closing it means deriving a contradiction
 * between two DIFFERENT arithmetic comparisons over a symbolic Int
 * (e.g. `a1 > 0` against `(a1 != 0) == false`). Every available move
 * fails, and for one reason each:
 *   - REWRITING needs an equation mentioning both comparisons. The only
 *     such equations in scope are the six closure lemmas themselves
 *     (circular); the Kleene axiom set is purely boolean and `refl_eq`
 *     is reflexivity.
 *   - EVALUATION (channel 2) is stuck: `a1 > 0` on a symbolic `a1`
 *     yields `NothingToStep`, so `revert`ing the antecedent into the
 *     goal (the Phase-4c ex-falso idiom) produces
 *     `a1 > 0 ==> false` and then cannot falsify the
 *     antecedent. Ex falso works only once `a1` is a literal — see
 *     `test_zero_case_closes_by_ex_falso`, which closes the a1 == 0
 *     sub-case that way.
 *   - SPLITTING further does not help: Int/Nat case analysis is
 *     `| 0 => | n =>` (a literal and a catch-all, per
 *     `test_pin_no_numeric_induction_hypothesis`), so the catch-all
 *     branch reproduces the original goal with no inductive hypothesis.
 *   - ALGEBRITE would "close" it, but only because the checker does not
 *     yet re-verify the CAS equation
 *     (`test_pin_algebrite_launders_the_lemma`). That is trusting an
 *     unchecked step, i.e. strictly worse than the trusted axiom this
 *     file is trying to retire, so it is deliberately not used.
 * The missing vocabulary is Phase 5's ordered-arithmetic polarity
 * engine (docs/prover-obligations.md §5). Until then the six stay
 * trusted axioms. */
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
 * goes through), no PENDING obligations, and `Incomplete` — the one
 * documented hole per lemma. The true branch really is proven; only the
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
  check_full_status(
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
 * split's domain scan (§4.1) sees `a1 ** b1` and emits `b1 >= 0`, which
 * discharges Remote (channel 1) against the intro'd second antecedent.
 * A tidy demonstration of the design's "obligations about obligations,
 * one mechanism" claim on the very lemma whose guard exists to keep `**`
 * on-domain. */
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
      "discharged Remote (channel 1) against the intro'd antecedent",
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

/* The half that IS proven: assert positively that the true branch of
 * each split reaches literal `true`, so "Incomplete" above is one open
 * branch rather than a proof that never started. With the bare-boolean
 * statement the branch is closed BY THE SPLIT — its goal is already
 * `true` — which is the whole visible effect of retiring the `== true`
 * notation.
 *
 * The false branch pins the OTHER visible effect, and it is worth
 * stating plainly: its goal is now the literal `false` rather than
 * `false == true`, and `ProofMap.status_of_proof` reads a `false`
 * outgoing as Refuted. So the open branch of a partially-proven
 * bare-boolean lemma reports `Some(false)`, not `None`. Nothing is
 * unsound and the THEOREM's own status is unaffected (`check_partial`
 * pins it Incomplete): the branch really has been driven to `false`,
 * and what remains is to contradict it against the split's `case_eq`,
 * which is exactly the missing Phase-5 vocabulary. But it means a
 * partial proof of such a lemma shows a "disproven" branch in the
 * stepper where the `== true` spelling showed an unknown one — a
 * presentation question for whoever renders per-branch status, not a
 * checker one. */
let test_true_branch_is_really_closed = () => {
  let (pm, proof) = run_named("nonzero_of_pos_proved", src_nonzero_of_pos);
  let rec find_induction = (p: Proof.t): option(Proof.t) =>
    switch (p.term) {
    | Induction(_, _) => Some(p)
    | Assume(_, b)
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
  | Some({term: Induction(_, [(_, true_body), (_, false_body)]), _}) =>
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
 * These pin the precise checker facts the verdict rests on. Each is
 * written to FAIL LOUDLY if a future phase removes the limitation, which
 * is the signal to revisit this file and the §4.2 note. */

/* (C1) The core wall: a comparison on a symbolic operand does not step,
 * so channel 2 cannot decide it and ex falso has nothing to compute
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
 * hypothesis (`ih` is not even in scope). This is what blocks `pow_pos`
 * specifically -- its proof wants induction on the exponent with a
 * quantified IH, and `generalize` (Phase 4b) can supply the
 * quantification but there is no induction principle to hand it to.
 *
 * On the Nat-vs-Int typing question: switching the exponent to `Nat`
 * does NOT help. Nat is an atom type here, not an inductive one (the
 * probe below is over Nat and still has no `ih`), and the statement is
 * pinned to Int anyway -- `**` parses to the Int operator class
 * (Axioms.re's note), so an SInt/Nat-typed variant would be a different
 * lemma about different operators, not this one. */
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

/* (C3) The Algebrite escape hatch, pinned as a hazard rather than used.
 * `rewrite (a1 != 0) with true` drives `nonzero_of_pos` to fully Proven
 * -- not because the CAS agrees (it would not: this is a boolean
 * comparison, not a field identity) but because the checker does not yet
 * re-verify the equational content of an Algebrite step at all. The CAS
 * lives in the browser as `window.Algebrite` and the node harness has no
 * CAS, so the step is UI-trusted (docs/prover-obligations.md §4.1's
 * "checker-side CAS re-verification" TODO). The Float gate (§1.5) is the
 * only content gate that fires, and a Bool-typed rewrite sails past it.
 *
 * Consequence for this chunk: Algebrite cannot be used to self-host the
 * closure library, because a lemma "proven" through it would rest on a
 * larger unchecked base than the axiom it replaces. When CAS
 * re-verification lands this test will fail, which is the correct
 * prompt to re-audit both this file and the §4.2 note. */
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
  check_full_status(
    "an unverified Algebrite rewrite still launders the lemma to Proven "
    ++ "(if this now fails, checker-side CAS verification landed)",
    ProofMap.Proven,
    pm,
    proof,
  );
};

/* (C4) A NEW friction finding, adjacent to the Phase-4c note about
 * assume-intro's antecedent test: a `Parens` node around the nested
 * antecedent defeats intro outright. `ProofCheck`'s intro test matches
 * `goal |> Exp.term_of` against `BinOp(Bool(Implies), a, b)`, so with
 * the goal written `A ==> (B ==> C)` -- the shape `Axioms.re`'s
 * `==>>` operator reads as, and the natural way to transcribe a nested
 * implication -- the second `assume B` is not recognised as intro. It
 * falls through to assume-then-bake and incurs `B` as an obligation,
 * which then goes PENDING. The paren-free right-associative spelling
 * used throughout this file avoids it. Cheap fix when someone wants it:
 * peel Parens in the intro test. */
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

/* (C5) The positive result inside the negative one: the false branch's
 * a1 == 0 sub-case DOES close, by the Phase-4c ex-falso idiom. Split the
 * symbolic `a1` into the literal `0` and a catch-all; in the `0` branch
 * the split's `case_eq` is the bare equation `a1 == 0`, which rewrites
 * `a1` to `0` inside the reverted antecedent, evaluation falsifies
 * `0 > 0`, and McCarthy `==>` collapses the goal to `true`. Only the
 * catch-all branch (a1 an arbitrary Int) remains open -- which is
 * exactly the missing induction principle of (C2), and shows the wall is
 * about SYMBOLIC operands specifically, not about the idiom. */
let ex_falso_src =
  theorem(
    "t",
    stmt_nonzero_of_pos,
    "assume a1 > 0 => induction a1 != 0 "
    ++ "| true => ? "
    ++ "| false => induction a1 "
    ++ "| 0 => revert a1 > 0 => axiom case_eq' at 0 on a1 end; "
    ++ "eval 0 > 0 at 0 end; eval false ==> false at 0 end "
    ++ "| n => ? end end",
  );

let test_zero_case_closes_by_ex_falso = () => {
  let (pm, proof) = run_named("t", ex_falso_src);
  check_mark_free("ex falso probe", pm, proof);
  let rec find_revert = (p: Proof.t): option(Proof.t) =>
    switch (p.term) {
    | Revert(_, _, _) => Some(p)
    | Assume(_, b)
    | Forall(_, b)
    | Generalize(_, b) => find_revert(b)
    | Seq(a, b) =>
      switch (find_revert(a)) {
      | Some(x) => Some(x)
      | None => find_revert(b)
      }
    | Induction(_, cases) =>
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
  check_full_status(
    "the lemma is still Incomplete (the catch-all branch is open)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

/* --- Section D: closed instances (the §1.3 operational guarantee) -------
 *
 * The lemmas are unprovable in general, but every CLOSED instance of
 * each one -- guards and conclusion together, i.e. the whole `==>`
 * statement instantiated at literals -- is decided outright by
 * evaluation. Assuming such an instance discharges through channel 2
 * (`Evaluated`), which is both the §1.3 operational-guarantee spot-check
 * ("a proven theorem's every closed instance evaluates to true") and the
 * channel-2 half of the §4.3 metric. Same `closed_fact` idiom the STLC
 * milestone uses to sanity-check its dynamics.
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
      ++ String.concat(
           ", ",
           List.map(
             (ob: Obligation.t) =>
               trunc(print_exp(ob.goal))
               ++ " ["
               ++ Obligation.show_discharge(ob.discharge)
               ++ "]",
             obs,
           ),
         )
      ++ "\n"
      ++ dump(pm, proof),
    )
  };
  check_full_status(name ++ " is fully Proven", ProofMap.Proven, pm, proof);
};

let test_instances = () => List.iter(check_instance, instances);

/* --- Channel distribution (docs/prover-obligations.md §4.3) -------------
 *
 * The §4.3 metric on this arithmetic workload: every obligation the file
 * incurs, bucketed by discharge channel. Target is ~90% in channels 1+2;
 * here it is 100% (13 of 13), with zero channel-3/4 residue -- and that
 * number needs reading with care, because the automation debt of this
 * workload does not show up as obligations at all. It shows up as the six
 * open false branches, which no obligation records. A 100% channel
 * distribution is compatible with 0 of 6 lemmas proven: the metric
 * measures obligation ERGONOMICS, not proof completeness. */

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
  let (remote, evaluated, local, pending) =
    List.fold_left(
      ((r, e, l, p), (name, src)) => {
        let (pm, proof) = run_named(name, src);
        List.fold_left(
          ((r, e, l, p), ob: Obligation.t) =>
            switch (ob.discharge) {
            | Remote(_) => (r + 1, e, l, p)
            | Evaluated => (r, e + 1, l, p)
            | Local(_) => (r, e, l + 1, p)
            | Pending => (r, e, l, p + 1)
            },
          (r, e, l, p),
          ProofMap.obligations_of_proof(pm, proof),
        );
      },
      (0, 0, 0, 0),
      all_workload,
    );
  print_endline(
    "closure-library channel distribution (docs/prover-obligations.md "
    ++ "4.3): total="
    ++ string_of_int(remote + evaluated + local + pending)
    ++ " channel1(lookup/Remote)="
    ++ string_of_int(remote)
    ++ " channel2(evaluation)="
    ++ string_of_int(evaluated)
    ++ " local="
    ++ string_of_int(local)
    ++ " pending="
    ++ string_of_int(pending),
  );
  Alcotest.check(
    Alcotest.int,
    "channel 1 (binder lookup): pow_pos's ** domain condition",
    1,
    remote,
  );
  Alcotest.check(
    Alcotest.int,
    "channel 2 (closed evaluation): the 12 closed instances",
    12,
    evaluated,
  );
  Alcotest.check(Alcotest.int, "no inline subproofs yet", 0, local);
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
      "closed instances are fully proven, guards via channel 2",
      `Quick,
      test_instances,
    ),
    test_case(
      "channel distribution across the whole workload",
      `Quick,
      test_channel_distribution,
    ),
  ],
);
