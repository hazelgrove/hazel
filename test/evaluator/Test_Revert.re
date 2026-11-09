open Alcotest;
open Language;
open Test_ProofMap.Harness;

/* The `revert` proof step and the boolean reading of facts.
 *
 * `revert F => body` moves an in-scope fact back into the goal: the
 * body's incoming goal is `F ==> G`. It is the inverse of assume's
 * implication intro, sound and complete (F holds here, so
 * `(F ==> G) == G`), hence obligation-free, and it lets eval and rewrite
 * steps, which work only on the goal, compute with a hypothesis.
 *
 * Also covered: inductive hypotheses for recursive ADTs. */

/* --- Ex falso: revert a fact, contradict it in the goal, eval ---------- */

/* Two assumed equations that cannot both hold. Reverting the second puts
 * it in the goal, where the first (still in scope, as a rewrite rule)
 * turns it into `0 == 1`; evaluation then falsifies the antecedent and
 * McCarthy-collapses the implication. This is the ex-falso idiom the step
 * exists for: no absurdity rule, just "move it into the goal and
 * compute". */
/* The proof needs `as h0` on the outer assumption: both assumptions
   are named `assume`, and the inner one shadows the outer, so `assume`
   here would rewrite `n` to `1` rather than to `0`. */
let ex_falso_src = "theorem t = forall n: Int -> n == 0 ==> n == 1 ==> false proof assume n == 0 as h0 => assume n == 1 => revert n == 1 => from n do axiom h0 end end; from 0 == 1 do eval end; from false ==> false do eval end in t";

let test_ex_falso = () => {
  let (pm, proof) = run_named("t", ex_falso_src);
  check_mark_free("revert ex falso", pm, proof);
  /* revert itself incurs nothing; the two assumes are intros. */
  check_obligation_count("revert ex falso", 0, pm, proof);
  check_proven("revert ex falso is Proven", pm, proof);
};

/* --- The reverted fact stays in scope ---------------------------------- */

/* Reverting doesn't consume the fact: it is still citable as a rewrite
 * rule inside the body, and still discharges obligations by lookup. */
let stays_in_scope_src = "theorem t = forall n: Int -> n == 0 ==> 1 == 1 proof assume n == 0 => revert n == 0 => from n do axiom assume end end; from 0 == 0 do eval end; eval; eval in t";

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
  check_obligation_count(
    "revert of an unknown fact incurs nothing",
    0,
    pm,
    proof,
  );
};

/* --- Bare-boolean facts read as `F == true` --------------------------- */

/* A `where` guard that is not an equation still has a rewrite reading: a
 * cited fact holds, so it reads as `F == true`. */
let bool_fact_src = "theorem t = forall b: Bool where b -> b == true proof from b do axiom where end end; from true == true do axiom refl_eq end end in t";

let test_bool_fact_rewrite = () => {
  let (pm, proof) = run_named("t", bool_fact_src);
  check_mark_free("bare-boolean where-fact as a rewrite", pm, proof);
  check_proven("bare-boolean where-fact rewrite is Proven", pm, proof);
};

/* --- Bare-boolean theorem conclusions used as rules ------------------- */

/* The reading is uniform: it applies to every rule, not just to
 * hypotheses. A theorem whose statement concludes in a bare boolean is
 * therefore usable as a rewrite rule by a later theorem, with no
 * `== true` anywhere in the source. `lem`'s own proof is a hole — a
 * rule's usability does not depend on its proof being finished, which is
 * what lets these tests be about the rule mechanism alone. */
let lem_bare = "theorem lem = forall x: Int -> x * 0 != 1 proof ? in ";

/* The same lemma with the reading spelled out as an Equality. Every
 * assertion below is made against both, and they must agree. */
let lem_eq = "theorem lem = forall x: Int -> (x * 0 != 1) == true proof ? in ";

/* Forward (`axiom`): the conclusion instance rewrites to `true`. */
let fwd_src = "theorem t = 7 * 0 != 1 proof from 7 * 0 != 1 do axiom lem end end in t";

/* Reverse (`axiomrev`): a `true` in the goal rewrites to the conclusion
 * instance. Enabled for explicit citations only; rule discovery
 * suppresses it, since `true` occurs everywhere
 * (`ProofRule.Rewrite.matches`). The `with` clause supplies the
 * instantiation that matching on a bare `true` can't recover, and
 * evaluation then brings the instance back to `true`. */
let rev_src = "theorem t = true proof from true do axiomrev lem with x = 3 end end; from 3 * 0 do eval end; from 0 != 1 do eval end in t";

let check_rule_use = (label: string, prelude: string, body: string) => {
  let (pm, proof) = run_named("t", prelude ++ body);
  check_mark_free(label, pm, proof);
  check_obligation_count(
    label ++ ": unconditional rule incurs nothing",
    0,
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
  /* An Equality-concluding rule behaves identically. */
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

/* Negative control: the reading is gated. A conclusion that is not a
 * boolean proposition stays `Other`, which can't rewrite, so citing such
 * a rule marks rather than rewriting the term to `true`. */
let non_bool_src =
  "theorem lem = forall x: Int -> x * 0 proof ? in "
  ++ "theorem t = 7 * 0 != 1 proof from 7 * 0 do axiom lem end end in t";

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
      "a non-boolean conclusion gained a rewrite reading\n" ++ dump(pm, proof),
    );
  };
};

/* --- Recursive-ADT inductive hypotheses ------------------------------- */

/* `induction e` on a recursive ADT installs an IH per recursive
 * sub-pattern, although the sub-pattern's type (the unrolled `rec` form)
 * is not `Typ.fast_equal` to the scrutinee's (the alias). Reverting the
 * IH by its exact statement pins its shape. */
let adt_prelude = "type Nt = +Z+S(Nt) in let pos = fun e -> case e | Z => true | S(b) => true end in ";

let adt_ih_src =
  adt_prelude
  ++ "theorem t = forall e: Nt -> pos(e) proof induction e "
  ++ "| Z => from pos(Z) do eval end "
  ++ "| S(b) => revert pos(b) => from pos(b) do axiom ih end end; from true ==> pos(S(b)) do eval end; from pos(S(b)) do eval end "
  ++ "end in t";

let test_adt_ih = () => {
  let (pm, proof) = run_named("t", adt_ih_src);
  check_mark_free("ADT induction installs an IH", pm, proof);
  check_obligation_count("ADT IH citation incurs nothing", 0, pm, proof);
  check_proven("ADT induction with a cited IH is Proven", pm, proof);
};

/* Negative control: a statement that is not an installed fact marks, so
 * the test above is not vacuous. */
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
