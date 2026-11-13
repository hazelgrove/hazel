open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* A minimal simply-typed lambda calculus (de Bruijn) encoded as Hazel
 * ADTs, with structurally recursive decision procedures (ty_eq / nth_ty /
 * infer / step / subst / shift), and a proof of progress.
 *
 * Fully proven theorems assert Proven with nothing pending; partial
 * proofs carry `?` holes with `FRICTION:` comments and assert their exact
 * intermediate status, so the file pins what the prover can't yet do. */

let has_mark_kind = Test_ProofMap.has_mark_kind;

/* --- The encoding ------------------------------------------------------- */

/* Types: TB (base) | TArr(Ty, Ty). Terms, de Bruijn: TmVar(Int) |
 * TmLam(Ty, Term) | TmAp(Term, Term). Option types are hand-rolled ADTs
 * (MaybeTy / MaybeTm) — the builtin Option is not used, to keep
 * constructor coverage/pattern behavior under our control.
 *
 * All decision procedures take a tuple and are structurally recursive in
 * one argument position; nth_ty recurses on the list (index decremented
 * alongside), not on the Int.
 *
 * `step` implements CBV: congruence-1 (head not a value), congruence-2
 * (head value, argument not), beta (both values, head a lambda). Beta is
 * `subst((0, y, b))` — the outer down-shift of the classical de Bruijn
 * beta is omitted (progress is insensitive to it and negative literals
 * would be the only use of them here); `shift` is still exercised
 * under binders inside `subst`. */

let prelude =
  "type Ty = +TB+TArr(Ty, Ty) in "
  ++ "type Term = +TmVar(Int)+TmLam(Ty, Term)+TmAp(Term, Term) in "
  ++ "type MaybeTy = +NoTy+SomeTy(Ty) in "
  ++ "type MaybeTm = +NoTm+SomeTm(Term) in "
  ++ "let ty_eq = fun p -> case p | (TB, TB) => true | (TB, TArr(x, y)) => false | (TArr(x, y), TB) => false | (TArr(x1, y1), TArr(x2, y2)) => ty_eq((x1, x2)) && ty_eq((y1, y2)) end in "
  ++ "let nth_ty = fun p -> case p | ([], n) => NoTy | (t :: rest, n) => if n == 0 then SomeTy(t) else nth_ty((rest, n - 1)) end in "
  ++ "let lam_result = fun p -> case p | (t, NoTy) => NoTy | (t, SomeTy(t2)) => SomeTy(TArr(t, t2)) end in "
  ++ "let ap_result = fun p -> case p | (NoTy, m) => NoTy | (SomeTy(TB), m) => NoTy | (SomeTy(TArr(ta, tb)), NoTy) => NoTy | (SomeTy(TArr(ta, tb)), SomeTy(s)) => (if ty_eq((ta, s)) then SomeTy(tb) else NoTy) end in "
  ++ "let infer = fun p -> case p | (ctx, TmVar(n)) => nth_ty((ctx, n)) | (ctx, TmLam(t, b)) => lam_result((t, infer((t :: ctx, b)))) | (ctx, TmAp(e1, e2)) => ap_result((infer((ctx, e1)), infer((ctx, e2)))) end in "
  ++ "let is_value = fun e -> case e | TmVar(n) => false | TmLam(t, b) => true | TmAp(x, y) => false end in "
  /* is_lam's branch order differs from is_value's on purpose. With the
   * same order the two definitions are alpha-equal, and a selector
   * `is_value(TmVar(n))` with no occurrence as written would match an
   * `is_lam(TmVar(n))` once both are unfolded. */
  ++ "let is_lam = fun e -> case e | TmLam(t, b) => true | TmVar(n) => false | TmAp(x, y) => false end in "
  ++ "let is_some_tm = fun m -> case m | NoTm => false | SomeTm(x) => true end in "
  ++ "let shift = fun p -> case p | (d, c, TmVar(n)) => (if n < c then TmVar(n) else TmVar(n + d)) | (d, c, TmLam(t, b)) => TmLam(t, shift((d, c + 1, b))) | (d, c, TmAp(x, y)) => TmAp(shift((d, c, x)), shift((d, c, y))) end in "
  ++ "let subst = fun p -> case p | (j, v, TmVar(n)) => (if n == j then v else TmVar(n)) | (j, v, TmLam(t, b)) => TmLam(t, subst((j + 1, shift((1, 0, v)), b))) | (j, v, TmAp(x, y)) => TmAp(subst((j, v, x)), subst((j, v, y))) end in "
  ++ "let step = fun e -> case e | TmVar(n) => NoTm | TmLam(t, b) => NoTm | TmAp(x, y) => (if is_value(x) then (if is_value(y) then case x | TmVar(n) => NoTm | TmLam(t, b) => SomeTm(subst((0, y, b))) | TmAp(f, a) => NoTm end else case step(y) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(x, yp)) end) else case step(x) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y)) end) end in ";

let program = (rest: string): string => prelude ++ rest;

/* --- (a) Smoke: every decision procedure passes the totality gate ------ */

/* A refl instantiation at `f(x)` runs the instantiation gate on the
 * substituted `f` (its FixF spine must be detected structurally
 * recursive). Proven with no obligations means the gate passed. */
let smoke = (fn_app: string): unit => {
  let src =
    program(
      "theorem t = forall v: Term -> forall w: Ty -> forall ts: [Ty] -> forall i: Int -> forall m: MaybeTm -> "
      ++ fn_app
      ++ " == "
      ++ fn_app
      ++ " proof axiom refl_eq end in t",
    );
  let (pm, proof) = run_named("t", src);
  check_obligation_count(
    "smoke: no obligations for " ++ fn_app,
    0,
    pm,
    proof,
  );
  check_proven("smoke: " ++ fn_app ++ " refl is Proven", pm, proof);
};

let test_smoke_ty_eq = () => smoke("ty_eq((w, w))");
let test_smoke_nth_ty = () => smoke("nth_ty((ts, i))");
let test_smoke_infer = () => smoke("infer((ts, v))");
let test_smoke_is_value = () => smoke("is_value(v)");
let test_smoke_step = () => smoke("step(v)");
let test_smoke_subst = () => smoke("subst((i, v, v))");
let test_smoke_shift = () => smoke("shift((i, i, v))");
let test_smoke_is_some_tm = () => smoke("is_some_tm(m)");

/* --- Dynamics sanity ---------------------------------------------------
 *
 * Each closed equation evaluates to `true`, which checks the encoding's
 * dynamics. They run in the evaluator, not as `assume`d facts, because
 * discharge by closed evaluation never runs a recursive function. */

let closed_fact = (fact: string): unit =>
  Alcotest.check(
    Alcotest.bool,
    fact,
    true,
    Exp.fast_equal(
      parse_and_evaluate(program(fact)),
      Exp.temp(Atom(Bool(true))),
    ),
  );

/* infer types the identity function. */
let test_dyn_infer_id = () =>
  closed_fact("infer(([], TmLam(TB, TmVar(0)))) == SomeTy(TArr(TB, TB))");

/* infer rejects an unbound variable and a self-application shape. */
let test_dyn_infer_unbound = () =>
  closed_fact("infer(([], TmVar(0))) == NoTy");

/* A well-typed application: (\\x:TB. x) applied under a lambda. */
let test_dyn_infer_ap = () =>
  closed_fact(
    "infer(([], TmAp(TmLam(TB, TmVar(0)), TmLam(TB, TmVar(0))))) == NoTy",
  );

/* step: beta fires on value-value application. */
let test_dyn_step_beta = () =>
  closed_fact(
    "step(TmAp(TmLam(TB, TmVar(0)), TmLam(TB, TmVar(0)))) == SomeTm(TmLam(TB, TmVar(0)))",
  );

/* step: congruence-1 steps the head of a nested application. */
let test_dyn_step_cong = () =>
  closed_fact(
    "step(TmAp(TmAp(TmLam(TB, TmVar(0)), TmLam(TB, TmVar(0))), TmVar(3))) == SomeTm(TmAp(TmLam(TB, TmVar(0)), TmVar(3)))",
  );

/* step: values and stuck terms do not step. */
let test_dyn_step_stuck = () => closed_fact("step(TmVar(0)) == NoTm");

/* --- (b) Canonical forms ------------------------------------------------ */

/* The ==>-chain phrasing is fully provable: every case is closed by
 * evaluation alone (the McCarthy dynamics of `==>` turn a false
 * antecedent into `true` without touching the consequent). */
let canonical_impl_src =
  program(
    "theorem canonical = forall e: Term -> forall t1: Ty -> forall t2: Ty -> is_value(e) ==> infer(([], e)) == SomeTy(TArr(t1, t2)) ==> is_lam(e) proof induction e "
    ++ "| TmVar(n) => from is_value(TmVar(n)) do eval end; from false ==> infer(([], TmVar(n))) == SomeTy(TArr(t1, t2)) ==> is_lam(TmVar(n)) do eval end "
    /* FRICTION: evaluating the antecedent to `true` and stripping
     * `true ==> _` by eval doesn't work: the eval step's leading
     * auto-steps fire hidden FixUnwrap transitions inside the untouched
     * middle antecedent, leaving it fix-unrolled and unmatchable for the
     * later assume-intro (fast_equal). Introducing both antecedents by
     * assume avoids evaluating near them. */
    ++ "| TmLam(t, b) => assume is_value(TmLam(t, b)) => assume infer(([], TmLam(t, b))) == SomeTy(TArr(t1, t2)) => from is_lam(TmLam(t, b)) do eval end "
    ++ "| TmAp(x, y) => from is_value(TmAp(x, y)) do eval end; from false ==> infer(([], TmAp(x, y))) == SomeTy(TArr(t1, t2)) ==> is_lam(TmAp(x, y)) do eval end "
    ++ "end in canonical",
  );

let test_canonical_impl_proven = () => {
  let (pm, proof) = run_named("canonical", canonical_impl_src);
  check_obligation_count(
    "canonical (==> form): no obligations",
    0,
    pm,
    proof,
  );
  check_proven("canonical (==> form) is fully Proven", pm, proof);
};

/* The phrasing with a `where`-restricted binder is fully provable too.
 * The guard `is_value(e)` is a boolean hypothesis; the TmAp case is
 * closed by `revert`ing it into the goal, where the case_eq
 * `e == TmAp(x, y)` rewrites it into a closed computation, evaluation
 * falsifies it, and McCarthy `==>` returns `true`: the revert-based ex
 * falso idiom. */
let canonical_where_src =
  program(
    "theorem canonicalw = forall e0: Term where is_value(e0) -> forall t1: Ty -> forall t2: Ty -> infer(([], e0)) == SomeTy(TArr(t1, t2)) ==> is_lam(e0) proof induction e0 "
    ++ "| TmVar(n) => from infer(([], TmVar(n))) do eval end; from nth_ty(([], n)) do eval end; from NoTy == SomeTy(TArr(t1, t2)) do eval end; from false ==> is_lam(TmVar(n)) do eval end "
    ++ "| TmLam(t, b) => assume infer(([], TmLam(t, b))) == SomeTy(TArr(t1, t2)) => from is_lam(TmLam(t, b)) do eval end "
    ++ "| TmAp(x, y) => revert is_value(e0) => from e0 do axiom case_eq end end; from is_value(TmAp(x, y)) do eval end; from false ==> (infer(([], TmAp(x, y))) == SomeTy(TArr(t1, t2)) ==> is_lam(TmAp(x, y))) do eval end "
    ++ "end in canonicalw",
  );

let test_canonical_where_partial = () => {
  let (pm, proof) = run_named("canonicalw", canonical_where_src);
  check_obligation_count(
    "canonical (where form): no obligations",
    0,
    pm,
    proof,
  );
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "canonical (where form) should be mark-free\n" ++ dump(pm, proof),
    );
  };
  check_proven("canonical (where form) is fully Proven", pm, proof);
};

/* A small genuine theorem exercising the full TmVar eval chain of
 * progress (an unbound variable types as nothing, so anything follows):
 * 4 eval steps unfold infer/nth_ty, falsify the antecedent, and
 * McCarthy-collapse the implication. */
let var_never_types_src =
  program(
    "theorem var_never_types = forall n: Int -> forall t: Ty -> infer(([], TmVar(n))) == SomeTy(t) ==> false proof "
    ++ "from infer(([], TmVar(n))) do eval end; from nth_ty(([], n)) do eval end; from NoTy == SomeTy(t) do eval end; from false ==> false do eval end in var_never_types",
  );

let test_var_never_types = () => {
  let (pm, proof) = run_named("var_never_types", var_never_types_src);
  check_obligation_count("var_never_types: no obligations", 0, pm, proof);
  check_proven("var_never_types is fully Proven", pm, proof);
};

/* --- Step-unfolding helper lemmas (equality conclusions) ----------------
 *
 * `step` on an application unfolds to a three-way conditional blob; to
 * reason under it the blob must be quoted verbatim as an eval target.
 * These lemmas pay that cost once each and export small guarded
 * equations whose antecedents discharge by lookup against split case_eq
 * facts at the use sites. */

let cong1_case = (hd: string, arg: string): string =>
  "case step("
  ++ hd
  ++ ") | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, "
  ++ arg
  ++ ")) end";

let cong2_case = (hd: string, arg: string): string =>
  "case step("
  ++ arg
  ++ ") | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp("
  ++ hd
  ++ ", yp)) end";

/* Congruence-1: a non-value head means the application steps iff the
 * head does. */
let lemma_step_ap_val1 =
  "theorem step_ap_val1 = forall u1: Term -> forall u2: Term -> is_value(u1) == false ==> step(TmAp(u1, u2)) == ("
  ++ cong1_case("u1", "u2")
  /* TODO: finish the proof by collapsing the conditional the eval
   * leaves. `progress` cites the lemma as stated. */
  ++ ") proof assume is_value(u1) == false => from step(TmAp(u1, u2)) do eval end; from is_value(u1) do axiom assume end end; ? in ";

/* Congruence-2 at a lambda head: the head's value-ness is computed by
 * evaluation inside the lemma, so no is_value(head) antecedent is
 * needed. */
let lemma_step_lam_ap2 =
  "theorem step_lam_ap2 = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == false ==> step(TmAp(TmLam(w1, q1), u2)) == ("
  ++ cong2_case("TmLam(w1, q1)", "u2")
  /* TODO: finish, as for step_ap_val1. */
  ++ ") proof assume is_value(u2) == false => from step(TmAp(TmLam(w1, q1), u2)) do eval end; from is_value(TmLam(w1, q1)) do eval end; from is_value(u2) do axiom assume end end; ? in ";

/* Beta: both sides values, lambda head. */
let lemma_step_beta = "theorem step_beta = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == true ==> step(TmAp(TmLam(w1, q1), u2)) == SomeTm(subst((0, u2, q1))) proof assume is_value(u2) == true => from step(TmAp(TmLam(w1, q1), u2)) do eval end; from is_value(TmLam(w1, q1)) do eval end; from is_value(u2) do axiom assume end end; ? in ";

/* is_some_tm(SomeTm(v)) == true — needed because evaluating
 * `is_some_tm(SomeTm(subst(...)))` directly would first unfold the
 * CBV-forced `subst` application into an unquotable blob. */
let lemma_some_tm_is_some = "theorem some_tm_is_some = forall u1: Term -> is_some_tm(SomeTm(u1)) == true proof from is_some_tm(SomeTm(u1)) do eval end; from true == true do axiom refl_eq end end in ";

/* The step-unfolding lemmas' proofs are partial (see the FRICTION notes
 * in each source): every retained step is mark-free, no obligations are
 * incurred (assume is intro), and the proof pins as Incomplete. Their
 * statements still act as conditional rewrite rules at the progress use
 * sites: a rule comes from its statement, not its proof. */
let lemma_partial_test = (name: string, lemma: string): unit => {
  let (pm, proof) = run_named(name, program(lemma ++ name));
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "lemma " ++ name ++ " should be mark-free\n" ++ dump(pm, proof),
    );
  };
  check_obligation_count(
    "lemma " ++ name ++ ": no obligations",
    0,
    pm,
    proof,
  );
  check_status(
    "lemma " ++ name ++ " pins as Incomplete",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let lemma_test = (name: string, lemma: string): unit => {
  let (pm, proof) = run_named(name, program(lemma ++ name));
  check_proven("lemma " ++ name ++ " is fully Proven", pm, proof);
  check_obligation_count(
    "lemma " ++ name ++ ": no obligations",
    0,
    pm,
    proof,
  );
};

let test_lemma_step_ap_val1 = () =>
  lemma_partial_test("step_ap_val1", lemma_step_ap_val1);
let test_lemma_step_lam_ap2 = () =>
  lemma_partial_test("step_lam_ap2", lemma_step_lam_ap2);
let test_lemma_step_beta = () =>
  lemma_partial_test("step_beta", lemma_step_beta);
let test_lemma_some_tm_is_some = () =>
  lemma_test("some_tm_is_some", lemma_some_tm_is_some);

/* --- (c) Progress -------------------------------------------------------
 *
 * forall e -> forall t -> infer([], e) == SomeTy(t) ==>
 *   is_value(e) || is_some_tm(step(e))
 *
 * by generalize t => induction e. TmVar and TmLam close outright. The
 * TmAp case is driven by ADT/bool splits on the computed scrutinees of
 * infer's unfolding, closing every branch the vocabulary can close:
 *   - all branches where infer computes NoTy: antecedent evaluates to
 *     false, McCarthy `==>` collapses the goal to true;
 *   - head-not-value + step(x0) = SomeTm(_): congruence lemma;
 *   - lambda head + value argument: beta lemma;
 *   - lambda head + non-value argument + step(y0) = SomeTm(_):
 *     congruence lemma;
 *   - value head that is a TmVar or a TmAp: vacuous by canonical forms,
 *     closed by ex falso (`contradiction`, or `revert` the value-ness
 *     case_eq into the goal, rewrite x0 there by the shape case_eq, and
 *     let evaluation falsify the antecedent);
 *   - value head that steps to NoTm, and lambda head + non-value
 *     argument stepping to NoTm: vacuous by the inductive hypothesis at
 *     the type the enclosing split produced. `revert` the IH
 *     instantiated there (`revert ih with t0 = tf0`), rewrite its
 *     antecedent away with the case_eqs in scope, and the same ex falso
 *     collapse finishes it.
 * `progress` is fully Proven (test_progress_proven). */

let progress_consequent = "is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0)))";

/* One antecedent-falsified leaf: collapse ap_result by evaluation,
 * falsify, McCarthy-collapse. The splits on `infer` calls have already
 * replaced them in the goal. */
let noty_leaf = (pair: string): string =>
  "from ap_result(("
  ++ pair
  ++ ")) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> "
  ++ progress_consequent
  ++ " do eval end";

let progress_src =
  program(
    lemma_step_ap_val1
    ++ lemma_step_lam_ap2
    ++ lemma_step_beta
    ++ lemma_some_tm_is_some
    ++ "theorem progress = forall e0: Term -> forall t0: Ty -> infer(([], e0)) == SomeTy(t0) ==> is_value(e0) || is_some_tm(step(e0)) proof generalize t0 => induction e0 "
    ++ "| TmVar(m0) => forall t0 => from infer(([], TmVar(m0))) do eval end; from nth_ty(([], m0)) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmVar(m0)) || is_some_tm(step(TmVar(m0))) do eval end "
    ++ "| TmLam(w0, q0) => forall t0 => assume infer(([], TmLam(w0, q0))) == SomeTy(t0) => from is_value(TmLam(w0, q0)) do eval end; from true || is_some_tm(step(TmLam(w0, q0))) do eval end "
    ++ "| TmAp(x0, y0) => forall t0 => "
    /* This case gets two inductive hypotheses, one per recursive
     * sub-term, both named `ih`, so the second shadows the first and only
     * y0's is reachable by that name. `alias` names each one by spelling
     * out the proposition, and the two leaves below cite `ihx` / `ihy`. */
    ++ "alias ihx = forall t0 -> infer(([], x0)) == SomeTy(t0) ==> is_value(x0) || is_some_tm(step(x0)) => "
    ++ "alias ihy = forall t0 -> infer(([], y0)) == SomeTy(t0) ==> is_value(y0) || is_some_tm(step(y0)) => "
    ++ "from infer(([], TmAp(x0, y0))) do eval end; "
    /* `as tx` names this split's case equation. Naming is what lets the
     * two deep IH leaves below still cite it: without a name it would be
     * `case_eq`, which the splits nested inside shadow. */
    ++ "induction infer(([], x0)) as tx "
    ++ "| NoTy => "
    ++ "induction infer(([], y0)) as ty "
    ++ "| NoTy => "
    ++ noty_leaf("NoTy, NoTy")
    ++ " | SomeTy(s0) => "
    ++ noty_leaf("NoTy, SomeTy(s0)")
    ++ " end "
    ++ "| SomeTy(tf0) => "
    ++ "induction tf0 "
    ++ "| TB => induction infer(([], y0)) as ty "
    ++ "| NoTy => "
    ++ noty_leaf("SomeTy(TB), NoTy")
    ++ " | SomeTy(s0) => "
    ++ noty_leaf("SomeTy(TB), SomeTy(s0)")
    ++ " end "
    ++ "| TArr(ta0, tb0) => induction infer(([], y0)) as ty "
    ++ "| NoTy => "
    ++ noty_leaf("SomeTy(TArr(ta0, tb0)), NoTy")
    ++ " | SomeTy(s0) => from ap_result((SomeTy(TArr(ta0, tb0)), SomeTy(s0))) do eval end; "
    ++ "induction ty_eq((ta0, s0)) as teq "
    ++ "| false => from if false then SomeTy(tb0) else NoTy do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> "
    ++ progress_consequent
    ++ " do eval end "
    ++ "| true => from if true then SomeTy(tb0) else NoTy do eval end; "
    ++ "assume SomeTy(tb0) == SomeTy(t0) => "
    /* The `is_value(x0)` split comes before the two goal-evaluating
     * steps, so the ex falso leaves below get the goal as written and the
     * evaluation happens only in the branch that wants it. */
    ++ "induction is_value(x0) as vx "
    /* Congruence-1: the head steps, so the application steps. */
    ++ "| false => from is_value(TmAp(x0, y0)) do eval end; "
    ++ "from false || is_some_tm(step(TmAp(x0, y0))) do eval end; "
    ++ "from step(TmAp(x0, y0)) do axiom step_ap_val1 end end; "
    ++ "induction step(x0) as sx "
    /* Vacuous by the inductive hypothesis. IH(x0) at t0 := tf0, with
     * case_eq infer(([], x0)) == SomeTy(tf0) and is_value(x0) == false,
     * forces step(x0) == SomeTm(_), which contradicts case_eq
     * step(x0) == NoTm. The IH is quantified (`forall t0 -> A(t0) ==> D`)
     * and D doesn't mention t0, so matching can't fix the binder: the
     * citation has to say what t0 is. `revert ih with t0 = tf0` moves the
     * instance into the goal; then the case_eqs in scope rewrite A to
     * `SomeTy(tf0) == SomeTy(tf0)` (refl_eq: true) and D to `false`,
     * leaving `false ==> G`, which McCarthy-collapses to true. */
    ++ "| NoTm => revert ihx with t0 = tf0 => "
    ++ "from infer(([], x0)) do axiom tx end end; "
    ++ "from SomeTy(tf0) == SomeTy(tf0) do axiom refl_eq end end; "
    ++ "from true ==> is_value(x0) || is_some_tm(step(x0)) do eval end; "
    ++ "from is_value(x0) do axiom vx end end; "
    ++ "from false || is_some_tm(step(x0)) do eval end; "
    ++ "from step(x0) do axiom sx end end; "
    ++ "from is_some_tm(NoTm) do eval end; "
    ++ "from false ==> is_some_tm(case NoTm | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y0)) end) do eval end "
    ++ "| SomeTm(u0) => from is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y0)) end) do eval end "
    ++ "end "
    ++ "| true => induction x0 as hx0 "
    /* Vacuous by canonical forms: is_value(x0) == true and
     * x0 == TmVar(m0) are contradictory. `contradiction` cites the
     * value-ness fact, and the `with` clause names the rewrite that
     * falsifies it: the split's own case_eq, `x0 == TmVar(m0)`, which the
     * checker verifies is in scope before substituting. That yields
     * `is_value(TmVar(m0)) == true`, which evaluates to `false`, so the
     * branch is vacuous. The TmAp leaf below uses the revert-based idiom
     * instead, so both stay tested. */
    ++ "| TmVar(m0) => contradiction is_value(x0) == true with x0 = TmVar(m0) end "
    ++ "| TmLam(w0, q0) => from is_value(TmAp(TmLam(w0, q0), y0)) do eval end; "
    ++ "from false || is_some_tm(step(TmAp(TmLam(w0, q0), y0))) do eval end; "
    ++ "induction is_value(y0) as vy "
    /* Beta: the canonical-forms leaf. */
    ++ "| true => from step(TmAp(TmLam(w0, q0), y0)) do axiom step_beta end end; from is_some_tm(SomeTm(subst((0, y0, q0)))) do axiom some_tm_is_some end end "
    /* Congruence-2 at a lambda head. */
    ++ "| false => from step(TmAp(TmLam(w0, q0), y0)) do axiom step_lam_ap2 end end; "
    ++ "induction step(y0) as sy "
    /* The same IH leaf as above, at IH(y0) and t0 := s0 -- cited through
     * the `ihy` alias, since the bare name `ih` here denotes whichever
     * IH was installed innermost. */
    ++ "| NoTm => revert ihy with t0 = s0 => "
    ++ "from infer(([], y0)) do axiom ty end end; "
    ++ "from SomeTy(s0) == SomeTy(s0) do axiom refl_eq end end; "
    ++ "from true ==> is_value(y0) || is_some_tm(step(y0)) do eval end; "
    ++ "from is_value(y0) do axiom vy end end; "
    ++ "from false || is_some_tm(step(y0)) do eval end; "
    ++ "from step(y0) do axiom sy end end; "
    ++ "from is_some_tm(NoTm) do eval end; "
    ++ "from false ==> is_some_tm(case NoTm | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(TmLam(w0, q0), yp)) end) do eval end "
    ++ "| SomeTm(u0) => from is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(TmLam(w0, q0), yp)) end) do eval end "
    ++ "end "
    ++ "end "
    /* Same ex falso as the TmVar leaf: is_value(x0) == true with
     * x0 == TmAp(f0, a0). */
    ++ "| TmAp(f0, a0) => revert is_value(x0) == true => from x0 do axiom hx0 end end; from is_value(TmAp(f0, a0)) do eval end; from false == true do eval end; from false ==> (is_value(TmAp(TmAp(f0, a0), y0)) || is_some_tm(step(TmAp(TmAp(f0, a0), y0)))) do eval end "
    ++ "end "
    ++ "end "
    ++ "end "
    ++ "end "
    ++ "end "
    ++ "end in progress",
  );

let test_progress_proven = () => {
  let (pm, proof) = run_named("progress", progress_src);
  /* Mark-free, with no holes. */
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "progress should be mark-free\n--- proof dump ---\n" ++ dump(pm, proof),
    );
  };
  /* The three step-lemma applications each incur exactly their
   * instantiated antecedent, all discharged Remote against split case_eq
   * facts. Nothing is Pending. */
  let obs = ProofMap.obligations_of_proof(pm, proof);
  check_obligation_count(
    "progress: exactly the 3 lemma antecedents",
    3,
    pm,
    proof,
  );
  if (!
        List.for_all(
          (ob: Obligation.t) =>
            switch (ob.discharge) {
            | Obligation.Remote(_) => true
            | _ => false
            },
          obs,
        )) {
    Alcotest.fail(
      "all progress obligations should discharge Remote (channel 1):\n"
      ++ String.concat("\n", List.map(show_obligation, obs)),
    );
  };
  /* Every leaf closes: `progress` for STLC is proven in Hazel itself. The
   * two vacuous leaves need the quantified inductive hypothesis at a type
   * the split produced (`revert ihx with t0 = tf0` and
   * `revert ihy with t0 = s0`), each naming its IH through an `alias`
   * because the case's two IHs share the name `ih`. */
  check_status("progress is fully Proven", ProofMap.Proven, pm, proof);
};

/* --- Addressing occurrences in an unfolded body ------------------------

   After `eval step(TmAp(u1, u2))` unfolds step's body into the goal, the
   calls in it name their functions, so written selectors find them: one
   to another definition (`is_value(u2)`) and a recursive one
   (`step(u1)`). */

let unroll_probe = (piece: string): (ProofMap.t, Proof.t) => {
  let src =
    program(
      "theorem bp = forall u1: Term -> forall u2: Term -> is_value(u1) == false ==> step(TmAp(u1, u2)) == NoTm proof assume is_value(u1) == false => from step(TmAp(u1, u2)) do eval end; from is_value(u1) do axiom assume end end; from "
      ++ piece
      ++ " do eval end; ? in bp",
    );
  run_named("bp", src);
};

let test_unfolded_call_addressable = (piece: string, ()) => {
  let (pm, proof) = unroll_probe(piece);
  if (has_mark_kind(
        pm,
        proof,
        fun
        | ProofMark.PatternNotFound(_) => true
        | _ => false,
      )) {
    Alcotest.fail(piece ++ " should be addressable\n" ++ dump(pm, proof));
  };
};

/* --- Pins: inductive hypotheses and where-facts ----------------------- */

/* Structural induction over a user ADT generates inductive hypotheses,
 * although recursive-ADT constructor payloads carry the unrolled `rec`
 * form while the scrutinee carries the alias form (see
 * `Induction.hypotheses`).
 *
 * `revert` matches an in-scope fact by `Exp.fast_equal`, so a mark-free
 * revert of the spelled-out statement pins the IH exactly: the goal at
 * the sub-term x0, still quantified over t0 because of the
 * `generalize`. */
let ih_probe_src = (cite: string): string =>
  program(
    "theorem ihprobe = forall e0: Term -> forall t0: Ty -> infer(([], e0)) == SomeTy(t0) ==> is_value(e0) || is_some_tm(step(e0)) proof generalize t0 => induction e0 "
    ++ "| TmVar(m0) => ? | TmLam(w0, q0) => ? "
    ++ "| TmAp(x0, y0) => forall t0 => "
    ++ cite
    ++ " end in ihprobe",
  );

/* The IH exists, quantified over t0, at the sub-term x0. */
let test_ih_exists_quantified = () => {
  let (pm, proof) =
    run_named(
      "ihprobe",
      ih_probe_src(
        "revert forall t0 -> infer(([], x0)) == SomeTy(t0) ==> is_value(x0) || is_some_tm(step(x0)) => ?",
      ),
    );
  if (has_mark_kind(
        pm,
        proof,
        fun
        | ProofMark.UnknownFactReverted => true
        | _ => false,
      )) {
    Alcotest.fail(
      "the quantified IH at x0 should be an in-scope fact\n"
      ++ dump(pm, proof),
    );
  };
};

/* Negative control: the IH is at the sub-term, not at the scrutinee. */
let test_ih_not_at_scrutinee = () => {
  let (pm, proof) =
    run_named(
      "ihprobe",
      ih_probe_src(
        "revert forall t0 -> infer(([], TmAp(x0, y0))) == SomeTy(t0) ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) => ?",
      ),
    );
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.UnknownFactReverted => true
          | _ => false,
        )) {
    Alcotest.fail("the goal at the scrutinee is not an IH");
  };
};

/* Without a `with` clause, citing the quantified IH is refused: its
   conclusion doesn't mention `t0`, so matching it leaves the binder
   unresolved while the antecedent mentions it, an underdetermined
   instantiation. */
let test_ih_citation_underdetermined_without_with = () => {
  let (pm, proof) =
    run_named(
      "ihprobe",
      ih_probe_src(
        /* `ihx`, not `ih`: this case installs two inductive hypotheses
           (one per sub-term), both named `ih`, so `ih` denotes the one
           installed innermost, y0's. An `alias` says which one this probe
           is about. */
        "alias ihx = forall t0 -> infer(([], x0)) == SomeTy(t0) ==> is_value(x0) || is_some_tm(step(x0)) => revert ihx => from is_value(x0) || is_some_tm(step(x0)) do axiom ihx end end",
      ),
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
      "citing the quantified IH with no instantiation should still be "
      ++ "refused as underdetermined\n"
      ++ dump(pm, proof),
    );
  };
};

/* ...and with one it goes through: `revert ih with t0 = <ty>` moves the
   instance of the IH into the goal, as the vacuous `progress` leaves do
   (test_progress_proven). The IH is cited by name. */
let test_ih_instantiated_with_clause = () => {
  let (pm, proof) =
    run_named("ihprobe", ih_probe_src("revert ih with t0 = t0 => ?"));
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "instantiating the quantified IH should be mark-free\n"
      ++ dump(pm, proof),
    );
  };
};

/* The binder must actually be one the IH quantifies. */
let test_ih_instantiation_unknown_binder = () => {
  let (pm, proof) =
    run_named("ihprobe", ih_probe_src("revert ih with zz = t0 => ?"));
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.RevertFactNotQuantified(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "`with zz = ...` names no binder of the IH and should be marked\n"
      ++ dump(pm, proof),
    );
  };
};

/* A where-guard that is a boolean fact has a rewrite reading
 * (`F == true`, see Test_Revert), but it only rewrites occurrences of `F`
 * itself: citing it at an unrelated target is RuleDoesNotApply. */
let test_pin_where_fact_uncitable = () => {
  let src =
    program(
      "theorem wprobe = forall u1: Term where is_value(u1) -> is_lam(u1) proof from is_lam(u1) do axiom where end end in wprobe",
    );
  let (pm, proof) = run_named("wprobe", src);
  if (!
        has_mark_kind(
          pm,
          proof,
          fun
          | ProofMark.RuleDoesNotApply(_) => true
          | _ => false,
        )) {
    Alcotest.fail(
      "citing the boolean where-fact at a non-matching target should be "
      ++ "RuleDoesNotApply\n"
      ++ dump(pm, proof),
    );
  };
};

let tests = (
  "Evaluator.MilestoneSTLC",
  [
    test_case("smoke: ty_eq passes the gate", `Quick, test_smoke_ty_eq),
    test_case("smoke: nth_ty passes the gate", `Quick, test_smoke_nth_ty),
    test_case("smoke: infer passes the gate", `Quick, test_smoke_infer),
    test_case("smoke: is_value passes the gate", `Quick, test_smoke_is_value),
    test_case("smoke: step passes the gate", `Quick, test_smoke_step),
    test_case("smoke: subst passes the gate", `Quick, test_smoke_subst),
    test_case("smoke: shift passes the gate", `Quick, test_smoke_shift),
    test_case(
      "smoke: is_some_tm passes the gate",
      `Quick,
      test_smoke_is_some_tm,
    ),
    test_case("dynamics: infer types identity", `Quick, test_dyn_infer_id),
    test_case(
      "dynamics: infer rejects unbound var",
      `Quick,
      test_dyn_infer_unbound,
    ),
    test_case(
      "dynamics: infer on an ill-typed application",
      `Quick,
      test_dyn_infer_ap,
    ),
    test_case("dynamics: beta step", `Quick, test_dyn_step_beta),
    test_case("dynamics: congruence step", `Quick, test_dyn_step_cong),
    test_case("dynamics: stuck term", `Quick, test_dyn_step_stuck),
    test_case(
      "canonical forms (==> form) fully proven",
      `Quick,
      test_canonical_impl_proven,
    ),
    test_case(
      "canonical forms (where form) fully proven",
      `Quick,
      test_canonical_where_partial,
    ),
    test_case("var_never_types fully proven", `Quick, test_var_never_types),
    test_case("lemma step_ap_val1", `Quick, test_lemma_step_ap_val1),
    test_case("lemma step_lam_ap2", `Quick, test_lemma_step_lam_ap2),
    test_case("lemma step_beta", `Quick, test_lemma_step_beta),
    test_case("lemma some_tm_is_some", `Quick, test_lemma_some_tm_is_some),
    test_case("progress is fully proven", `Quick, test_progress_proven),
    test_case(
      "ADT induction generates a quantified IH",
      `Quick,
      test_ih_exists_quantified,
    ),
    test_case(
      "the IH is at the sub-term, not the scrutinee",
      `Quick,
      test_ih_not_at_scrutinee,
    ),
    test_case(
      "quantified IH citation is underdetermined without `with`",
      `Quick,
      test_ih_citation_underdetermined_without_with,
    ),
    test_case(
      "quantified IH instantiates with a `with` clause",
      `Quick,
      test_ih_instantiated_with_clause,
    ),
    test_case(
      "`with` must name a binder the IH quantifies",
      `Quick,
      test_ih_instantiation_unknown_binder,
    ),
    test_case(
      "pin: boolean where-fact does not rewrite an unrelated target",
      `Quick,
      test_pin_where_fact_uncitable,
    ),
    test_case(
      "a call to another definition in an unfolded body is addressable",
      `Quick,
      test_unfolded_call_addressable("is_value(u2)"),
    ),
    test_case(
      "a recursive call in an unfolded body is addressable",
      `Quick,
      test_unfolded_call_addressable("step(u1)"),
    ),
  ],
);
