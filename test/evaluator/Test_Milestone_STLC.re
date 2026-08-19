open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Phase 4 milestone: a minimal simply-typed lambda calculus (de Bruijn)
 * encoded as Hazel ADTs, with structurally-recursive decision
 * procedures (ty_eq / nth_ty / infer / step / subst / shift), driven as
 * far toward PROGRESS as the current proof-step vocabulary allows
 * (docs/prover-obligations.md, Phases 0-4b).
 *
 * This file is an integration test AND a friction study: fully-proven
 * theorems assert Proven/zero-pending; partial proofs carry `?` holes
 * with `FRICTION:` comments and assert their exact intermediate status,
 * so the file is a regression pin for future phases.
 *
 * Reuses the Test_ProofMap harness. */

let eval_with_proof = Test_ProofMap.eval_with_proof;
let proof_of_named = Test_ProofMap.proof_of_named;
let has_mark_kind = Test_ProofMap.has_mark_kind;
let check_exp = Test_ProofMap.check_exp;

let run_named = (name: string, src: string): (ProofMap.t, Proof.t) => {
  let (state, _, elab) = src |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  (pm, proof_of_named(name, elab));
};

/* --- Debug/diagnostic dump: every proof node's incoming/outgoing ------- */

let print_exp = Test_ProofMap.print_exp;

let trunc = (s: string): string => {
  let s = String.map(c => c == '\n' ? ' ' : c, s);
  String.length(s) > 2000 ? String.sub(s, 0, 2000) ++ " ..." : s;
};

let opt_exp_str = (e: option(Exp.t)): string =>
  switch (e) {
  | Some(e) => trunc(print_exp(e))
  | None => "<none>"
  };

let rec dump_proof = (pm: ProofMap.t, p: Proof.t, indent: string): string => {
  let id = Proof.rep_id(p);
  let entry = ProofMap.lookup(id, pm);
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
    | Generalize(_, b) => [dump_proof(pm, b, indent ++ "  ")]
    | Induction(_, cases) =>
      List.map(
        ((pat, body)) =>
          indent
          ++ "| "
          ++ trunc(Pat.show(pat))
          ++ "\n"
          ++ dump_proof(pm, body, indent ++ "  "),
        cases,
      )
    | _ => []
    };
  String.concat("\n", [here, ...children]);
};

/* Short pattern rendering would be nicer; Pat.show is verbose but only
 * appears in failure dumps. */

let dump = (pm, proof): string => dump_proof(pm, proof, "");

let check_full_status =
    (msg, expected: ProofMap.full_status, pm, proof): unit => {
  let actual = ProofMap.full_status_of_proof(pm, proof);
  if (actual != expected) {
    Alcotest.fail(
      msg
      ++ "\nexpected: "
      ++ ProofMap.show_full_status(expected)
      ++ "\ngot:      "
      ++ ProofMap.show_full_status(actual)
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof),
    );
  };
};

let check_proven = (msg, pm, proof) =>
  check_full_status(msg, ProofMap.Proven, pm, proof);

let check_obligation_count = (msg, expected, pm, proof) => {
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

/* --- The encoding ------------------------------------------------------- */

/* Types: TB (base) | TArr(Ty, Ty). Terms, de Bruijn: TmVar(Int) |
 * TmLam(Ty, Term) | TmAp(Term, Term). Option types are hand-rolled ADTs
 * (MaybeTy / MaybeTm) — the builtin Option is not used, to keep
 * constructor coverage/pattern behavior under our control.
 *
 * All decision procedures are structurally recursive in ONE argument
 * position, tuple-style per the milestone signatures; nth_ty recurses on
 * the LIST (index decremented alongside), not on the Int.
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
  /* NOTE: is_lam's branch order deliberately differs from is_value's.
   * With identical branch orders the two definitions are alpha-equal
   * after env substitution, and `nth_exp_env` (which matches at_exp
   * modulo env) resolves `is_value(TmVar(n))` to an `is_lam(TmVar(n))`
   * occurrence — see the friction report. */
  ++ "let is_lam = fun e -> case e | TmLam(t, b) => true | TmVar(n) => false | TmAp(x, y) => false end in "
  ++ "let is_some_tm = fun m -> case m | NoTm => false | SomeTm(x) => true end in "
  ++ "let shift = fun p -> case p | (d, c, TmVar(n)) => (if n < c then TmVar(n) else TmVar(n + d)) | (d, c, TmLam(t, b)) => TmLam(t, shift((d, c + 1, b))) | (d, c, TmAp(x, y)) => TmAp(shift((d, c, x)), shift((d, c, y))) end in "
  ++ "let subst = fun p -> case p | (j, v, TmVar(n)) => (if n == j then v else TmVar(n)) | (j, v, TmLam(t, b)) => TmLam(t, subst((j + 1, shift((1, 0, v)), b))) | (j, v, TmAp(x, y)) => TmAp(subst((j, v, x)), subst((j, v, y))) end in "
  ++ "let step = fun e -> case e | TmVar(n) => NoTm | TmLam(t, b) => NoTm | TmAp(x, y) => (if is_value(x) then (if is_value(y) then case x | TmVar(n) => NoTm | TmLam(t, b) => SomeTm(subst((0, y, b))) | TmAp(f, a) => NoTm end else case step(y) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(x, yp)) end) else case step(x) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y)) end) end in ";

let program = (rest: string): string => prelude ++ rest;

/* --- (a) Smoke: every decision procedure passes the Phase-4a gate ------- */

/* A refl instantiation at `f(x)` runs the instantiation gate on the
 * substituted `f` (its FixF spine must be detected structurally
 * recursive) — Proven with zero obligations means the gate passed
 * silently. */
let smoke = (fn_app: string): unit => {
  let src =
    program(
      "theorem t = forall v: Term -> forall w: Ty -> forall ts: [Ty] -> forall i: Int -> forall m: MaybeTm -> "
      ++ fn_app
      ++ " == "
      ++ fn_app
      ++ " proof axiom refl_eq at 0 on "
      ++ fn_app
      ++ " == "
      ++ fn_app
      ++ " end in t",
    );
  let (pm, proof) = run_named("t", src);
  check_obligation_count("smoke: no obligations for " ++ fn_app, 0, pm, proof);
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

/* --- Dynamics sanity via discharge channel 2 (closed evaluation) --------
 *
 * A closed `assume`d equation that evaluates to `true` discharges as
 * `Evaluated` — this both sanity-checks the encoding's dynamics and
 * exercises channel 2 on real decision-procedure computations. */

let closed_fact = (fact: string): unit => {
  let src =
    program(
      "theorem t = 1 == 1 proof assume "
      ++ fact
      ++ " => axiom refl_eq at 0 on 1 == 1 end in t",
    );
  let (pm, proof) = run_named("t", src);
  let obs = ProofMap.obligations_of_proof(pm, proof);
  switch (obs) {
  | [ob] when ob.discharge == Obligation.Evaluated => ()
  | _ =>
    Alcotest.fail(
      "closed fact did not discharge Evaluated: "
      ++ fact
      ++ "\n--- proof dump ---\n"
      ++ dump(pm, proof),
    )
  };
  check_proven("closed fact Proven: " ++ fact, pm, proof);
};

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

/* The ==>-chain phrasing is FULLY provable: every case is closed by
 * evaluation alone (the McCarthy dynamics of `==>` turn a false
 * antecedent into `true` without touching the consequent). */
let canonical_impl_src =
  program(
    "theorem canonical = forall e: Term -> forall t1: Ty -> forall t2: Ty -> is_value(e) ==> infer(([], e)) == SomeTy(TArr(t1, t2)) ==> is_lam(e) proof induction e "
    ++ "| TmVar(n) => eval is_value(TmVar(n)) at 0 end; eval false ==> infer(([], TmVar(n))) == SomeTy(TArr(t1, t2)) ==> is_lam(TmVar(n)) at 0 end "
    /* NOTE (friction): evaluating the antecedent to `true` and stripping
     * `true ==> _` by eval does NOT work: the eval step's leading
     * auto-steps fire hidden FixUnwrap transitions inside the UNTOUCHED
     * middle antecedent, leaving it fix-unrolled and unmatchable for the
     * later assume-intro (fast_equal). Introducing both antecedents by
     * assume (intro is free) avoids evaluating near them. */
    ++ "| TmLam(t, b) => assume is_value(TmLam(t, b)) => assume infer(([], TmLam(t, b))) == SomeTy(TArr(t1, t2)) => eval is_lam(TmLam(t, b)) at 0 end "
    ++ "| TmAp(x, y) => eval is_value(TmAp(x, y)) at 0 end; eval false ==> infer(([], TmAp(x, y))) == SomeTy(TArr(t1, t2)) ==> is_lam(TmAp(x, y)) at 0 end "
    ++ "end in canonical",
  );

let test_canonical_impl_proven = () => {
  let (pm, proof) = run_named("canonical", canonical_impl_src);
  check_obligation_count("canonical (==> form): no obligations", 0, pm, proof);
  check_proven("canonical (==> form) is fully Proven", pm, proof);
};

/* The task-statement phrasing with a `where`-restricted binder is only
 * PARTIALLY provable: the guard `is_value(e)` is installed as a
 * hypothesis, but it is a bare boolean fact — ProofRule classifies it
 * `Other`, and `can_eq` refuses Other conclusions — so it can never be
 * cited as a rewrite, and there is no ex-falso step to exploit its
 * contradiction with `e == TmAp(x, y)`. TmVar and TmLam close by
 * evaluation; TmAp is pinned with a hole. */
let canonical_where_src =
  program(
    "theorem canonicalw = forall e: Term where is_value(e) -> forall t1: Ty -> forall t2: Ty -> infer(([], e)) == SomeTy(TArr(t1, t2)) ==> is_lam(e) proof induction e "
    ++ "| TmVar(n) => eval infer(([], TmVar(n))) at 0 end; eval nth_ty(([], n)) at 0 end; eval NoTy == SomeTy(TArr(t1, t2)) at 0 end; eval false ==> is_lam(TmVar(n)) at 0 end "
    ++ "| TmLam(t, b) => assume infer(([], TmLam(t, b))) == SomeTy(TArr(t1, t2)) => eval is_lam(TmLam(t, b)) at 0 end "
    /* FRICTION: the where-hypothesis `is_value(e)` is unusable here — a
     * non-equality fact has no rewrite reading (ProofRule.can_eq:
     * Other => (None, None)) and the checker has no ex-falso/absurdity
     * step, so the contradiction `is_value(e)` vs `e == TmAp(x, y)`
     * cannot be cashed in. The ==>-chain variant above avoids the
     * hypothesis altogether by keeping `is_value(e)` in the goal where
     * evaluation can reach it. */
    ++ "| TmAp(x, y) => ? "
    ++ "end in canonicalw",
  );

let test_canonical_where_partial = () => {
  let (pm, proof) = run_named("canonicalw", canonical_where_src);
  /* Exact intermediate status: mark-free, obligation-free, Incomplete. */
  check_obligation_count("canonical (where form): no obligations", 0, pm, proof);
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "canonical (where form) should be mark-free\n" ++ dump(pm, proof),
    );
  };
  check_full_status(
    "canonical (where form) pins as Incomplete (TmAp hole)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

/* A small genuine theorem exercising the full TmVar eval chain of
 * progress (an unbound variable types as nothing, so anything follows):
 * 4 eval steps unfold infer/nth_ty, falsify the antecedent, and
 * McCarthy-collapse the implication. */
let var_never_types_src =
  program(
    "theorem var_never_types = forall n: Int -> forall t: Ty -> infer(([], TmVar(n))) == SomeTy(t) ==> false proof "
    ++ "eval infer(([], TmVar(n))) at 0 end; eval nth_ty(([], n)) at 0 end; eval NoTy == SomeTy(t) at 0 end; eval false ==> false at 0 end in var_never_types",
  );

let test_var_never_types = () => {
  let (pm, proof) = run_named("var_never_types", var_never_types_src);
  check_obligation_count("var_never_types: no obligations", 0, pm, proof);
  check_proven("var_never_types is fully Proven", pm, proof);
};

/* --- Step-unfolding helper lemmas (equality conclusions) ----------------
 *
 * `step` on an application unfolds to a three-way conditional blob; to
 * reason under it the blob must be quoted VERBATIM as an eval target
 * (major step-verbosity friction). These lemmas pay that cost once each
 * and export small guarded EQUATIONS whose antecedents discharge against
 * split case_eq facts (channel 1) at the use sites. */

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
  /* FRICTION (checker limitation, pinned below): after the eval unfolds
   * `step`, the goal's recursive occurrences `step(u1)` / `step(u2)`
   * were created by the hidden FixUnwrap self-substitution, and such
   * self-unrolled occurrences never match written at_exps under
   * nth_exp_env (env-substituted occurrences of OTHER definitions --
   * is_value, subst -- do match). The three-way conditional therefore
   * cannot be collapsed: quoting it whole fails on the step
   * subterms, and no smaller quote contains the Conditional redex.
   * Proof stops here, mark-free. */
  ++ ") proof assume is_value(u1) == false => eval step(TmAp(u1, u2)) at 0 end; axiom assume at 0 on is_value(u1) end; ? in ";

/* Congruence-2 at a lambda head: the head's value-ness is computed by
 * evaluation inside the lemma, so no is_value(head) antecedent is
 * needed. */
let lemma_step_lam_ap2 =
  "theorem step_lam_ap2 = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == false ==> step(TmAp(TmLam(w1, q1), u2)) == ("
  ++ cong2_case("TmLam(w1, q1)", "u2")
  /* Same FixUnwrap-unroll wall as step_ap_val1. */
  ++ ") proof assume is_value(u2) == false => eval step(TmAp(TmLam(w1, q1), u2)) at 0 end; eval is_value(TmLam(w1, q1)) at 0 end; axiom assume at 0 on is_value(u2) end; ? in ";

/* Beta: both sides values, lambda head. */
let lemma_step_beta =
  "theorem step_beta = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == true ==> step(TmAp(TmLam(w1, q1), u2)) == SomeTm(subst((0, u2, q1))) proof assume is_value(u2) == true => eval step(TmAp(TmLam(w1, q1), u2)) at 0 end; eval is_value(TmLam(w1, q1)) at 0 end; axiom assume at 0 on is_value(u2) end; ? in ";

/* is_some_tm(SomeTm(v)) == true — needed because evaluating
 * `is_some_tm(SomeTm(subst(...)))` directly would first unfold the
 * CBV-forced `subst` application into an unquotable blob. */
let lemma_some_tm_is_some =
  "theorem some_tm_is_some = forall u1: Term -> is_some_tm(SomeTm(u1)) == true proof eval is_some_tm(SomeTm(u1)) at 0 end; axiom refl_eq at 0 on true == true end in ";

/* The step-unfolding lemma PROOFS are partial (see the FRICTION notes in
 * each source): every retained step is mark-free, no obligations are
 * incurred (assume is intro), and the proof pins as Incomplete. Their
 * STATEMENTS still act as conditional rewrite rules at the progress
 * use sites -- rule content comes from statements, not proofs. */
let lemma_partial_test = (name: string, lemma: string): unit => {
  let (pm, proof) = run_named(name, program(lemma ++ name));
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "lemma " ++ name ++ " should be mark-free\n" ++ dump(pm, proof),
    );
  };
  check_obligation_count("lemma " ++ name ++ ": no obligations", 0, pm, proof);
  check_full_status(
    "lemma " ++ name ++ " pins as Incomplete",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

let lemma_test = (name: string, lemma: string): unit => {
  let (pm, proof) = run_named(name, program(lemma ++ name));
  check_proven("lemma " ++ name ++ " is fully Proven", pm, proof);
  check_obligation_count("lemma " ++ name ++ ": no obligations", 0, pm, proof);
};

let test_lemma_step_ap_val1 = () =>
  lemma_partial_test("step_ap_val1", lemma_step_ap_val1);
let test_lemma_step_lam_ap2 = () =>
  lemma_partial_test("step_lam_ap2", lemma_step_lam_ap2);
let test_lemma_step_beta = () =>
  lemma_partial_test("step_beta", lemma_step_beta);
let test_lemma_some_tm_is_some = () =>
  lemma_test("some_tm_is_some", lemma_some_tm_is_some);

/* --- (c) PROGRESS -------------------------------------------------------
 *
 * forall e -> forall t -> infer([], e) == SomeTy(t) ==>
 *   is_value(e) || is_some_tm(step(e))
 *
 * by generalize t => induction e. TmVar and TmLam close outright. The
 * TmAp case is driven by ADT/bool splits on the computed scrutinees of
 * infer's unfolding (each split needs an explicit case_eq rewrite --
 * computed-scrutinee splits do not substitute in the goal), closing
 * every branch the vocabulary can close:
 *   - all branches where infer computes NoTy: antecedent evaluates to
 *     false, McCarthy `==>` collapses the goal to true;
 *   - head-not-value + step(x0) = SomeTm(_): congruence lemma;
 *   - lambda head + value argument: beta lemma;
 *   - lambda head + non-value argument + step(y0) = SomeTm(_):
 *     congruence lemma.
 * Exactly FOUR leaves remain as holes, each demanding vocabulary that
 * does not exist yet (see the friction comments in place).
 *
 * Binder naming: all proof-level binders are 0-suffixed, disjoint from
 * every prelude-internal binder, so evaluator inlining never
 * capture-renames and quoted at_exps keep matching. */

let progress_consequent = "is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0)))";

/* One antecedent-falsified leaf: rewrite the remaining infer call by its
 * case_eq, collapse ap_result by evaluation, falsify, McCarthy-collapse.
 * `ceq` is the auto-generated prime-counted hypothesis name -- citing
 * these by prime-counting is itself a friction item. */
let noty_leaf = (ceq: string, pair: string): string =>
  "axiom "
  ++ ceq
  ++ " at 0 on infer(([], y0)) end; eval ap_result(("
  ++ pair
  ++ ")) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> "
  ++ progress_consequent
  ++ " at 0 end";

let progress_src =
  program(
    lemma_step_ap_val1
    ++ lemma_step_lam_ap2
    ++ lemma_step_beta
    ++ lemma_some_tm_is_some
    ++ "theorem progress = forall e0: Term -> forall t0: Ty -> infer(([], e0)) == SomeTy(t0) ==> is_value(e0) || is_some_tm(step(e0)) proof generalize t0 => induction e0 "
    ++ "| TmVar(m0) => forall t0 => eval infer(([], TmVar(m0))) at 0 end; eval nth_ty(([], m0)) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmVar(m0)) || is_some_tm(step(TmVar(m0))) at 0 end "
    ++ "| TmLam(w0, q0) => forall t0 => assume infer(([], TmLam(w0, q0))) == SomeTy(t0) => eval is_value(TmLam(w0, q0)) at 0 end; eval true || is_some_tm(step(TmLam(w0, q0))) at 0 end "
    ++ "| TmAp(x0, y0) => forall t0 => "
    ++ "eval infer(([], TmAp(x0, y0))) at 0 end; "
    ++ "induction infer(([], x0)) "
    ++ "| NoTy => axiom case_eq' at 0 on infer(([], x0)) end; "
    ++ "induction infer(([], y0)) "
    ++ "| NoTy => "
    ++ noty_leaf("case_eq''", "NoTy, NoTy")
    ++ " | SomeTy(s0) => "
    ++ noty_leaf("case_eq''", "NoTy, SomeTy(s0)")
    ++ " end "
    ++ "| SomeTy(tf0) => axiom case_eq' at 0 on infer(([], x0)) end; "
    ++ "induction tf0 "
    ++ "| TB => induction infer(([], y0)) "
    ++ "| NoTy => "
    ++ noty_leaf("case_eq'''", "SomeTy(TB), NoTy")
    ++ " | SomeTy(s0) => "
    ++ noty_leaf("case_eq'''", "SomeTy(TB), SomeTy(s0)")
    ++ " end "
    ++ "| TArr(ta0, tb0) => induction infer(([], y0)) "
    ++ "| NoTy => "
    ++ noty_leaf("case_eq'''", "SomeTy(TArr(ta0, tb0)), NoTy")
    ++ " | SomeTy(s0) => axiom case_eq''' at 0 on infer(([], y0)) end; eval ap_result((SomeTy(TArr(ta0, tb0)), SomeTy(s0))) at 0 end; "
    ++ "induction ty_eq((ta0, s0)) "
    ++ "| false => axiom case_eq'''' at 0 on ty_eq((ta0, s0)) end; eval if false then SomeTy(tb0) else NoTy at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> "
    ++ progress_consequent
    ++ " at 0 end "
    ++ "| true => axiom case_eq'''' at 0 on ty_eq((ta0, s0)) end; eval if true then SomeTy(tb0) else NoTy at 0 end; "
    ++ "assume SomeTy(tb0) == SomeTy(t0) => "
    ++ "eval is_value(TmAp(x0, y0)) at 0 end; "
    ++ "eval false || is_some_tm(step(TmAp(x0, y0))) at 0 end; "
    ++ "induction is_value(x0) "
    /* Congruence-1: the head steps, so the application steps. */
    ++ "| false => axiom step_ap_val1 at 0 on step(TmAp(x0, y0)) end; "
    ++ "induction step(x0) "
    /* FRICTION: this branch is vacuous -- IH(x0) with the case_eq
     * infer(([], x0)) == SomeTy(tf0) and is_value(x0) == false forces
     * step(x0) == SomeTm(_) -- but the IH's conclusion is a disjunction
     * (ProofRule Other: no rewrite reading), there is no modus ponens
     * to instantiate it into a fact, and no ex-falso step to cash in
     * the contradiction with case_eq step(x0) == NoTm. */
    ++ "| NoTm => ? "
    ++ "| SomeTm(u0) => axiom case_eq'''''' at 0 on step(x0) end; eval is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y0)) end) at 0 end "
    ++ "end "
    ++ "| true => induction x0 "
    /* FRICTION: vacuous by canonical forms -- is_value(x0) == true and
     * x0 == TmVar(m0) are contradictory (is_value(TmVar(m0)) computes
     * to false), but the contradiction lives in HYPOTHESES, which no
     * step can evaluate or combine; an ex-falso/absurdity step (or
     * computing inside facts) is missing vocabulary. */
    ++ "| TmVar(m0) => ? "
    ++ "| TmLam(w0, q0) => induction is_value(y0) "
    /* Beta: the crown-jewel leaf -- the canonical-forms moment. */
    ++ "| true => axiom step_beta at 0 on step(TmAp(TmLam(w0, q0), y0)) end; axiom some_tm_is_some at 0 on is_some_tm(SomeTm(subst((0, y0, q0)))) end "
    /* Congruence-2 at a lambda head. */
    ++ "| false => axiom step_lam_ap2 at 0 on step(TmAp(TmLam(w0, q0), y0)) end; "
    ++ "induction step(y0) "
    /* FRICTION: same shape as the NoTm branch above, at IH(y0). */
    ++ "| NoTm => ? "
    ++ "| SomeTm(u0) => axiom case_eq'''''''' at 0 on step(y0) end; eval is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(TmLam(w0, q0), yp)) end) at 0 end "
    ++ "end "
    ++ "end "
    /* FRICTION: vacuous by canonical forms again -- is_value(x0) == true
     * with x0 == TmAp(f0, a0); same missing ex-falso. */
    ++ "| TmAp(f0, a0) => ? "
    ++ "end "
    ++ "end "
    ++ "end "
    ++ "end "
    ++ "end "
    ++ "end in progress",
  );

let test_progress_partial = () => {
  let (pm, proof) = run_named("progress", progress_src);
  /* Mark-free: every step the vocabulary allows goes through; only the
   * four documented holes remain. */
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "progress should be mark-free\n--- proof dump ---\n" ++ dump(pm, proof),
    );
  };
  /* Channel-distribution pin (docs/prover-obligations.md section 4.3):
   * the three step-lemma applications each incur exactly their
   * instantiated antecedent, ALL discharged Remote (channel 1) against
   * split case_eq facts. Nothing is Pending. */
  let obs = ProofMap.obligations_of_proof(pm, proof);
  check_obligation_count(
    "progress: exactly the 3 lemma antecedents",
    3,
    pm,
    proof,
  );
  if (!List.for_all(
        (ob: Obligation.t) =>
          switch (ob.discharge) {
          | Obligation.Remote(_) => true
          | _ => false
          },
        obs,
      )) {
    Alcotest.fail(
      "all progress obligations should discharge Remote (channel 1):\n"
      ++ String.concat(
           "\n",
           List.map(
             (ob: Obligation.t) =>
               trunc(print_exp(ob.goal))
               ++ " ["
               ++ Obligation.show_discharge(ob.discharge)
               ++ "]",
             obs,
           ),
         ),
    );
  };
  check_full_status(
    "progress pins as Incomplete (4 friction holes)",
    ProofMap.Incomplete,
    pm,
    proof,
  );
};

/* --- Checker-limitation pins: at_exp addressability ---------------------

   After `eval step(TmAp(u1, u2))` unfolds step's body into the goal:
   - occurrences inlined from the closure ENVIRONMENT (is_value applied,
     subst applied inside a branch body) still match written at_exps
     (nth_exp_env pre-substitutes both sides) -- positive control below;
   - occurrences of `step` ITSELF, created by the hidden FixUnwrap
     self-substitution, do NOT match the written `step(u1)` --
     PatternNotFound although the application is visibly there. This is
     what blocks the three step-unfolding lemma proofs. */

let unroll_probe = (piece: string): (ProofMap.t, Proof.t) => {
  let src =
    program(
      "theorem bp = forall u1: Term -> forall u2: Term -> is_value(u1) == false ==> step(TmAp(u1, u2)) == NoTm proof assume is_value(u1) == false => eval step(TmAp(u1, u2)) at 0 end; axiom assume at 0 on is_value(u1) end; eval "
      ++ piece
      ++ " at 0 end; ? in bp",
    );
  run_named("bp", src);
};

/* Positive control: env-inlined `is_value(u2)` inside the unfolded body
 * IS addressable (the eval finds and steps it). */
let test_pin_env_inlined_addressable = () => {
  let (pm, proof) = unroll_probe("is_value(u2)");
  if (Test_ProofMap.find_marked_sub(pm, proof) != None) {
    Alcotest.fail(
      "env-inlined is_value(u2) should be addressable\n" ++ dump(pm, proof),
    );
  };
};

/* The pin: the self-unrolled `step(u1)` is NOT addressable. */
let test_pin_self_unrolled_unaddressable = () => {
  let (pm, proof) = unroll_probe("step(u1)");
  if (!has_mark_kind(
        pm,
        proof,
        fun
        | ProofMark.PatternNotFound(_) => true
        | _ => false,
      )) {
    Alcotest.fail(
      "self-unrolled step(u1) unexpectedly became addressable -- a "
      ++ "checker improvement landed; unblock the step lemmas!\n"
      ++ dump(pm, proof),
    );
  };
};

/* --- Friction pins: why the remaining holes cannot be closed ------------ */

/* FRICTION (Phase-4 gap found by this milestone): structural induction
 * over a user ADT generates NO inductive hypotheses at all. IH
 * generation (ProofHacks.get_inductive_hypotheses_inner') keeps a
 * sub-pattern only if its statics type `Typ.fast_equal`s the
 * scrutinee's type -- but recursive-ADT constructor payloads carry the
 * unrolled `rec` form while the scrutinee carries the alias form, so
 * the comparison always fails (list inductions, whose element types
 * compare nominally, are unaffected -- cf. Test_Generalize). Citing
 * `ih` in the TmAp case is therefore UnknownEquality (no such fact),
 * not RuleDoesNotApply. When IH generation normalizes types, this pin
 * should flip to RuleDoesNotApply (the IH would exist but its
 * disjunctive conclusion still has no rewrite reading). */
let test_pin_ih_disjunction_uncitable = () => {
  let src =
    program(
      "theorem ihprobe = forall e0: Term -> forall t0: Ty -> infer(([], e0)) == SomeTy(t0) ==> is_value(e0) || is_some_tm(step(e0)) proof generalize t0 => induction e0 "
      ++ "| TmVar(m0) => ? | TmLam(w0, q0) => ? "
      ++ "| TmAp(x0, y0) => forall t0 => axiom ih at 0 on is_value(TmAp(x0, y0)) end "
      ++ "end in ihprobe",
    );
  let (pm, proof) = run_named("ihprobe", src);
  if (!has_mark_kind(
        pm,
        proof,
        fun
        | ProofMark.UnknownEquality("ih") => true
        | _ => false,
      )) {
    Alcotest.fail(
      "ADT induction should generate no IH (UnknownEquality)\n"
      ++ dump(pm, proof),
    );
  };
};

/* A where-guard that is a bare boolean fact (not an equation) has no
 * rewrite reading either. */
let test_pin_where_fact_uncitable = () => {
  let src =
    program(
      "theorem wprobe = forall u1: Term where is_value(u1) -> is_lam(u1) proof axiom where at 0 on is_lam(u1) end in wprobe",
    );
  let (pm, proof) = run_named("wprobe", src);
  if (!has_mark_kind(
        pm,
        proof,
        fun
        | ProofMark.RuleDoesNotApply(_) => true
        | _ => false,
      )) {
    Alcotest.fail(
      "citing the boolean where-fact should be RuleDoesNotApply\n"
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
      "canonical forms (where form) partial pin",
      `Quick,
      test_canonical_where_partial,
    ),
    test_case("var_never_types fully proven", `Quick, test_var_never_types),
    test_case("lemma step_ap_val1", `Quick, test_lemma_step_ap_val1),
    test_case("lemma step_lam_ap2", `Quick, test_lemma_step_lam_ap2),
    test_case("lemma step_beta", `Quick, test_lemma_step_beta),
    test_case("lemma some_tm_is_some", `Quick, test_lemma_some_tm_is_some),
    test_case("PROGRESS partial proof pin", `Quick, test_progress_partial),
    test_case(
      "pin: disjunctive IH is uncitable",
      `Quick,
      test_pin_ih_disjunction_uncitable,
    ),
    test_case(
      "pin: boolean where-fact is uncitable",
      `Quick,
      test_pin_where_fact_uncitable,
    ),
    test_case(
      "pin: env-inlined occurrences are addressable",
      `Quick,
      test_pin_env_inlined_addressable,
    ),
    test_case(
      "pin: FixUnwrap self-unrolled occurrences are not",
      `Quick,
      test_pin_self_unrolled_unaddressable,
    ),
  ],
);
