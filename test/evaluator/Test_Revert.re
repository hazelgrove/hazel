open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Phase 4c: the `revert` proof step and the bare-boolean fact reading
 * (docs/prover-obligations.md, Phase 4).
 *
 * `revert F => body` cashes an in-scope fact back into the goal: the
 * body's incoming goal is `F ==> G`. It is the symmetric partner of
 * assume-intro — sound AND complete (F holds here, so `(F ==> G) == G`),
 * hence obligation-free — and it is what lets the eval/rewrite machinery,
 * which only ever works IN THE GOAL, compute with a hypothesis.
 *
 * Also covered: recursive-ADT inductive hypotheses (the Phase-4c IH
 * generation fix — before it, ADT inductions generated no IHs at all).
 * Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;

let run_named = (name: string, src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of_named(name, elab));
};

/* Compact incoming/outgoing dump, for failure messages. */
let print_exp = Test_ProofMap.print_exp;
let opt_exp_str = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => String.map(c => c == '\n' ? ' ' : c, print_exp(e))
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

let check_proven = (msg, pm, proof) =>
  check_status(msg, ProofMap.Proven, pm, proof);

let check_mark_free = (msg, pm, proof) =>
  switch (Test_ProofMap.find_marked_sub(pm, proof)) {
  | None => ()
  | Some((_, marks)) =>
    Alcotest.fail(
      msg
      ++ ": expected no marks, got "
      ++ String.concat("; ", List.map(ProofMark.show, marks)),
    )
  };

let check_no_obligations = (msg, pm, proof) => {
  let obs = ProofMap.obligations_of_proof(pm, proof);
  if (obs != []) {
    Alcotest.fail(
      msg
      ++ ": expected no obligations, got "
      ++ string_of_int(List.length(obs)),
    );
  };
};

/* --- Ex falso: revert a fact, contradict it in the goal, eval ---------- */

/* Two assumed equations that cannot both hold. Reverting the second puts
 * it in the goal, where the FIRST (still in scope, as a rewrite rule)
 * turns it into `0 == 1`; evaluation then falsifies the antecedent and
 * McCarthy-collapses the implication. This is the ex-falso idiom the step
 * exists for: no absurdity rule, just "move it into the goal and
 * compute". */
/* `as h0` on the OUTER assumption is load-bearing. Both assumptions are
   installed under the fixed name `assume`, and the inner one shadows the
   outer (docs/prover-obligations.md, "Hypothesis naming"), so the bare
   name here would rewrite `n` to `1` rather than to `0`. This proof wants
   the `n == 0` fact, so it says which one it means. */
let ex_falso_src = "theorem t = forall n: Int -> n == 0 ==> n == 1 ==> false proof assume n == 0 as h0 => assume n == 1 => revert n == 1 => axiom h0 at 0 on n end; eval 0 == 1 at 0 end; eval false ==> false at 0 end in t";

let test_ex_falso = () => {
  let (pm, proof) = run_named("t", ex_falso_src);
  check_mark_free("revert ex falso", pm, proof);
  /* revert itself incurs nothing; the two assumes are intros. */
  check_no_obligations("revert ex falso", pm, proof);
  check_proven("revert ex falso is Proven", pm, proof);
};

/* --- The reverted fact stays in scope ---------------------------------- */

/* Reverting does not consume the fact: the very fact that was reverted is
 * still citable as a rewrite rule inside the body. (Symmetrically, `F` is
 * still a discharge-channel-1 fact there.) */
let stays_in_scope_src = "theorem t = forall n: Int -> n == 0 ==> 1 == 1 proof assume n == 0 => revert n == 0 => axiom assume at 0 on n end; eval 0 == 0 ==> 1 == 1 at 0 end; eval true ==> 1 == 1 at 0 end; eval 1 == 1 at 0 end in t";

let test_revert_keeps_fact = () => {
  let (pm, proof) = run_named("t", stays_in_scope_src);
  check_mark_free("revert keeps the fact in scope", pm, proof);
  check_proven("revert keeps the fact in scope: Proven", pm, proof);
};

/* --- No matching fact: mark and pass through -------------------------- */

let unknown_src = "theorem t = forall n: Int -> n == 0 ==> 1 == 1 proof assume n == 0 => revert n == 7 => ? in t";

let test_revert_unknown = () => {
  let (pm, proof) = run_named("t", unknown_src);
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnknownFactReverted => true
          | _ => false,
        )) {
    Alcotest.fail("reverting a fact that is not in scope should mark");
  };
  check_no_obligations("revert of an unknown fact incurs nothing", pm, proof);
};

/* --- Bare-boolean facts read as `F == true` --------------------------- */

/* A `where` guard that is not an equation used to have no rewrite reading
 * at all (`ProofRule.can_eq` refused `Other` conclusions). A CITED fact
 * holds, so it also reads as `F == true`. */
let bool_fact_src = "theorem t = forall b: Bool where b -> b == true proof axiom where at 0 on b end; axiom refl_eq at 0 on true == true end in t";

let test_bool_fact_rewrite = () => {
  let (pm, proof) = run_named("t", bool_fact_src);
  check_mark_free("bare-boolean where-fact as a rewrite", pm, proof);
  check_proven("bare-boolean where-fact rewrite is Proven", pm, proof);
};

/* --- Bare-boolean THEOREM conclusions used as rules ------------------- */

/* The reading is uniform: it applies to every rule, not just to
 * hypotheses. A theorem whose statement concludes in a bare boolean is
 * therefore usable as a rewrite rule by a later theorem, with no
 * `== true` anywhere in the source. `lem`'s own proof is a hole — a
 * rule's usability does not depend on its proof being finished, which is
 * what lets these tests be about the rule mechanism alone. */
let lem_bare = "theorem lem = forall x: Int -> x * 0 != 1 proof ? in ";

/* The same lemma stated the OLD way, with the reading spelled out as an
 * Equality. The pair is an A/B: every assertion below is made against
 * both, and they must agree. */
let lem_eq = "theorem lem = forall x: Int -> (x * 0 != 1) == true proof ? in ";

/* Forward (`axiom`): the conclusion instance rewrites to `true`. */
let fwd_src = "theorem t = 7 * 0 != 1 proof axiom lem at 0 on 7 * 0 != 1 end in t";

/* Reverse (`axiomrev`): a `true` in the goal rewrites to the conclusion
 * instance. Enabled for explicit citations only — rule DISCOVERY
 * suppresses it, since `true` occurs everywhere
 * (`ProofRule.can_eq_inst`). The Phase-4d `with` clause supplies the
 * instantiation that matching on a bare `true` cannot recover, and
 * evaluation then brings the instance back to `true`. */
let rev_src = "theorem t = true proof axiomrev lem with x = 3 at 0 on true end; eval 3 * 0 at 0 end; eval 0 != 1 at 0 end in t";

let check_rule_use = (label: string, prelude: string, body: string) => {
  let (pm, proof) = run_named("t", prelude ++ body);
  check_mark_free(label, pm, proof);
  check_no_obligations(
    label ++ ": unconditional rule incurs nothing",
    pm,
    proof,
  );
  check_proven(label ++ ": Proven", pm, proof);
};

let test_bare_bool_theorem_as_rule_forward = () => {
  check_rule_use(
    "bare-boolean theorem as a rule, forward",
    lem_bare,
    fwd_src,
  );
  /* Regression: an Equality-concluding rule behaves identically. */
  check_rule_use("the `== true` spelling, forward", lem_eq, fwd_src);
};

let test_bare_bool_theorem_as_rule_reverse = () => {
  check_rule_use(
    "bare-boolean theorem as a rule, reverse",
    lem_bare,
    rev_src,
  );
  check_rule_use("the `== true` spelling, reverse", lem_eq, rev_src);
};

/* Negative control: the reading is GATED. A conclusion that is not a
 * boolean proposition stays `Other`, i.e. inert — citing such a rule
 * marks rather than silently rewriting the term to `true`. This is what
 * "if you cannot type it, keep it inert" buys. */
let non_bool_src =
  "theorem lem = forall x: Int -> x * 0 proof ? in "
  ++ "theorem t = 7 * 0 != 1 proof axiom lem at 0 on 7 * 0 end in t";

let test_non_bool_conclusion_stays_inert = () => {
  let (pm, proof) = run_named("t", non_bool_src);
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.RuleDoesNotApply(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "a non-boolean conclusion gained a rewrite reading\n"
      ++ dump(pm, proof, ""),
    );
  };
};

/* --- Recursive-ADT inductive hypotheses (the Phase-4c IH fix) --------- */

/* `induction e` on a recursive ADT installs an IH per recursive
 * sub-pattern. Before Phase 4c the sub-pattern's type (the unrolled `rec`
 * form) never `Typ.fast_equal`ed the scrutinee's type (the alias), so ADT
 * inductions generated ZERO IHs and this `revert` would mark
 * UnknownFactReverted. Reverting the IH by its exact statement is the
 * sharpest available assertion about its shape. */
let adt_prelude = "type Nt = +Z+S(Nt) in let pos = fun e -> case e | Z => true | S(b) => true end in ";

let adt_ih_src =
  adt_prelude
  ++ "theorem t = forall e: Nt -> pos(e) proof induction e "
  ++ "| Z => eval pos(Z) at 0 end "
  ++ "| S(b) => revert pos(b) => axiom ih at 0 on pos(b) end; eval true ==> pos(S(b)) at 0 end; eval pos(S(b)) at 0 end "
  ++ "end in t";

let test_adt_ih = () => {
  let (pm, proof) = run_named("t", adt_ih_src);
  check_mark_free("ADT induction installs an IH", pm, proof);
  check_no_obligations("ADT IH citation incurs nothing", pm, proof);
  check_proven("ADT induction with a cited IH is Proven", pm, proof);
};

/* Negative control: a statement that is NOT an installed fact still
 * marks, so the test above is not vacuous. */
let adt_ih_neg_src =
  adt_prelude
  ++ "theorem t = forall e: Nt -> pos(e) proof induction e "
  ++ "| Z => ? "
  ++ "| S(b) => revert pos(e) => ? "
  ++ "end in t";

let test_adt_ih_negative = () => {
  let (pm, proof) = run_named("t", adt_ih_neg_src);
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnknownFactReverted => true
          | _ => false,
        )) {
    Alcotest.fail("reverting a non-fact should still mark");
  };
};

let tests = (
  "Evaluator.Revert",
  [
    test_case("revert + eval ex falso", `Quick, test_ex_falso),
    test_case(
      "revert keeps the fact in scope",
      `Quick,
      test_revert_keeps_fact,
    ),
    test_case("revert of an unknown fact marks", `Quick, test_revert_unknown),
    test_case(
      "bare-boolean fact reads as F == true",
      `Quick,
      test_bool_fact_rewrite,
    ),
    test_case(
      "bare-boolean theorem as a rule (forward), vs the == true spelling",
      `Quick,
      test_bare_bool_theorem_as_rule_forward,
    ),
    test_case(
      "bare-boolean theorem as a rule (reverse), vs the == true spelling",
      `Quick,
      test_bare_bool_theorem_as_rule_reverse,
    ),
    test_case(
      "a non-boolean conclusion stays inert",
      `Quick,
      test_non_bool_conclusion_stays_inert,
    ),
    test_case("recursive-ADT induction installs IHs", `Quick, test_adt_ih),
    test_case("IH revert negative control", `Quick, test_adt_ih_negative),
  ],
);
