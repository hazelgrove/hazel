open Alcotest;
open Language;
open Util;
open Test_Evaluator_Prelude;

module StepperBase = Web.StepperBase;

/* Repro for:
 *   Failure("MissingStep root proof_target: proof is None/Pending (no leaf for ExtendProof)")
 * (StepperBase.re Stepper.view, root MissingStep case).
 *
 * Drives the same calculate pipeline Theorems.re runs for each theorem of
 * a user program, then walks every nested stepper root asserting the
 * invariant `view` failwiths on: a root MissingStep row must have a saved
 * backing proof leaf. */

/* User-reported program: STLC progress development with `?` holes left in
 * the two step(_)==NoTm branches and trailing lemma holes. */
let user_src = {prog|type Ty = +TB+TArr(Ty, Ty) in
type Term = +TmVar(Int)+TmLam(Ty, Term)+TmAp(Term, Term) in
type MaybeTy = +NoTy+SomeTy(Ty) in
type MaybeTm = +NoTm+SomeTm(Term) in
let ty_eq = fun p -> case p | (TB, TB) => true | (TB, TArr(x, y)) => false | (TArr(x, y), TB) => false | (TArr(x1, y1), TArr(x2, y2)) => ty_eq((x1, x2)) && ty_eq((y1, y2)) end in
let nth_ty = fun p -> case p | ([], n) => NoTy | (t :: rest, n) => if n == 0 then SomeTy(t) else nth_ty((rest, n - 1)) end in
let lam_result = fun p -> case p | (t, NoTy) => NoTy | (t, SomeTy(t2)) => SomeTy(TArr(t, t2)) end in
let ap_result = fun p -> case p | (NoTy, m) => NoTy | (SomeTy(TB), m) => NoTy | (SomeTy(TArr(ta, tb)), NoTy) => NoTy | (SomeTy(TArr(ta, tb)), SomeTy(s)) => (if ty_eq((ta, s)) then SomeTy(tb) else NoTy) end in
let infer = fun p -> case p | (ctx, TmVar(n)) => nth_ty((ctx, n)) | (ctx, TmLam(t, b)) => lam_result((t, infer((t :: ctx, b)))) | (ctx, TmAp(e1, e2)) => ap_result((infer((ctx, e1)), infer((ctx, e2)))) end in
let is_value = fun e -> case e | TmVar(n) => false | TmLam(t, b) => true | TmAp(x, y) => false end in
let is_lam = fun e -> case e | TmLam(t, b) => true | TmVar(n) => false | TmAp(x, y) => false end in
let is_some_tm = fun m -> case m | NoTm => false | SomeTm(x) => true end in
let shift = fun p -> case p | (d, c, TmVar(n)) => (if n < c then TmVar(n) else TmVar(n + d)) | (d, c, TmLam(t, b)) => TmLam(t, shift((d, c + 1, b))) | (d, c, TmAp(x, y)) => TmAp(shift((d, c, x)), shift((d, c, y))) end in
let subst = fun p -> case p | (j, v, TmVar(n)) => (if n == j then v else TmVar(n)) | (j, v, TmLam(t, b)) => TmLam(t, subst((j + 1, shift((1, 0, v)), b))) | (j, v, TmAp(x, y)) => TmAp(subst((j, v, x)), subst((j, v, y))) end in
let step = fun e -> case e | TmVar(n) => NoTm | TmLam(t, b) => NoTm | TmAp(x, y) => (if is_value(x) then (if is_value(y) then case x | TmVar(n) => NoTm | TmLam(t, b) => SomeTm(subst((0, y, b))) | TmAp(f, a) => NoTm end else case step(y) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(x, yp)) end) else case step(x) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y)) end) end in
theorem step_ap_val1 = forall u1: Term -> forall u2: Term -> is_value(u1) == false ==> step(TmAp(u1, u2)) == (case step(u1) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, u2)) end) proof assume is_value(u1) == false => eval step(TmAp(u1, u2)) at 0 end; axiom assume at 0 on is_value(u1) end; ? in
theorem step_lam_ap2 = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == false ==> step(TmAp(TmLam(w1, q1), u2)) == (case step(u2) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(TmLam(w1, q1), yp)) end) proof assume is_value(u2) == false => eval step(TmAp(TmLam(w1, q1), u2)) at 0 end; eval is_value(TmLam(w1, q1)) at 0 end; axiom assume at 0 on is_value(u2) end; ? in
theorem step_beta = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == true ==> step(TmAp(TmLam(w1, q1), u2)) == SomeTm(subst((0, u2, q1))) proof assume is_value(u2) == true => eval step(TmAp(TmLam(w1, q1), u2)) at 0 end; eval is_value(TmLam(w1, q1)) at 0 end; axiom assume at 0 on is_value(u2) end; ? in
theorem some_tm_is_some = forall u1: Term -> is_some_tm(SomeTm(u1)) == true proof eval is_some_tm(SomeTm(u1)) at 0 end; axiom refl_eq at 0 on true == true end in
theorem progress = forall e0: Term -> forall t0: Ty -> infer(([], e0)) == SomeTy(t0) ==> is_value(e0) || is_some_tm(step(e0)) proof generalize t0 => induction e0
| TmVar(m0) => forall t0 => eval infer(([], TmVar(m0))) at 0 end; eval nth_ty(([], m0)) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmVar(m0)) || is_some_tm(step(TmVar(m0))) at 0 end
| TmLam(w0, q0) => forall t0 => assume infer(([], TmLam(w0, q0))) == SomeTy(t0) => eval is_value(TmLam(w0, q0)) at 0 end; eval true || is_some_tm(step(TmLam(w0, q0))) at 0 end
| TmAp(x0, y0) => forall t0 => eval infer(([], TmAp(x0, y0))) at 0 end; induction infer(([], x0)) as tx
| NoTy => axiom tx at 0 on infer(([], x0)) end; induction infer(([], y0)) as ty
| NoTy => axiom ty at 0 on infer(([], y0)) end; eval ap_result((NoTy, NoTy)) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) at 0 end
| SomeTy(s0) => axiom ty at 0 on infer(([], y0)) end; eval ap_result((NoTy, SomeTy(s0))) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) at 0 end
end
| SomeTy(tf0) => axiom tx at 0 on infer(([], x0)) end; induction tf0
| TB => induction infer(([], y0)) as ty
| NoTy => axiom ty at 0 on infer(([], y0)) end; eval ap_result((SomeTy(TB), NoTy)) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) at 0 end
| SomeTy(s0) => axiom ty at 0 on infer(([], y0)) end; eval ap_result((SomeTy(TB), SomeTy(s0))) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) at 0 end
end
| TArr(ta0, tb0) => induction infer(([], y0)) as ty
| NoTy => axiom ty at 0 on infer(([], y0)) end; eval ap_result((SomeTy(TArr(ta0, tb0)), NoTy)) at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) at 0 end
| SomeTy(s0) => axiom ty at 0 on infer(([], y0)) end; eval ap_result((SomeTy(TArr(ta0, tb0)), SomeTy(s0))) at 0 end; induction ty_eq((ta0, s0)) as teq
| false => axiom teq at 0 on ty_eq((ta0, s0)) end; eval if false then SomeTy(tb0) else NoTy at 0 end; eval NoTy == SomeTy(t0) at 0 end; eval false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) at 0 end
| true => axiom teq at 0 on ty_eq((ta0, s0)) end; eval if true then SomeTy(tb0) else NoTy at 0 end; assume SomeTy(tb0) == SomeTy(t0) => induction is_value(x0)
| false => eval is_value(TmAp(x0, y0)) at 0 end; eval false || is_some_tm(step(TmAp(x0, y0))) at 0 end; axiom step_ap_val1 at 0 on step(TmAp(x0, y0)) end; induction step(x0) as sx
| NoTm => ?
| SomeTm(u0) => axiom sx at 0 on step(x0) end; eval is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y0)) end) at 0 end
end
| true => induction x0 as hx0
| TmVar(m0) => revert is_value(x0) == true => axiom hx0 at 0 on x0 end; eval is_value(TmVar(m0)) == true at 0 end; eval false == true at 0 end; eval false ==> (is_value(TmAp(TmVar(m0), y0)) || is_some_tm(step(TmAp(TmVar(m0), y0)))) at 0 end
| TmLam(w0, q0) => eval is_value(TmAp(TmLam(w0, q0), y0)) at 0 end; eval false || is_some_tm(step(TmAp(TmLam(w0, q0), y0))) at 0 end; induction is_value(y0)
| true => axiom step_beta at 0 on step(TmAp(TmLam(w0, q0), y0)) end; axiom some_tm_is_some at 0 on is_some_tm(SomeTm(subst((0, y0, q0)))) end
| false => axiom step_lam_ap2 at 0 on step(TmAp(TmLam(w0, q0), y0)) end; induction step(y0) as sy
| NoTm => ?
| SomeTm(u0) => axiom sy at 0 on step(y0) end; eval is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(TmLam(w0, q0), yp)) end) at 0 end
end
end
| TmAp(f0, a0) => revert is_value(x0) == true => axiom hx0 at 0 on x0 end; eval is_value(TmAp(f0, a0)) == true at 0 end; eval false == true at 0 end; eval false ==> (is_value(TmAp(TmAp(f0, a0), y0)) || is_some_tm(step(TmAp(TmAp(f0, a0), y0)))) at 0 end
end
end
end
end
end
end
end in progress|prog};

/* --- Invariant walk ---------------------------------------------------
 * `Stepper.view`'s root switch failwiths when a root MissingStep has no
 * saved backing proof (`m.proof |> Calc.get_saved_opt |> Option.join ==
 * None`) or a root NextStep is Pending. Non-root (tail) missing rows are
 * fine: view falls back to ExtendProof(previous leaf). Every nested
 * stepper (induction case bodies, forall/assume/revert/generalize
 * bodies) is rendered through the same root switch. */

let rec violations_of_root =
        (path: string, ns: StepperBase.next_step): list(string) =>
  switch (ns) {
  | Finished => []
  | MissingStep(m, tail) =>
    let here =
      switch (m.proof |> Calc.get_saved_opt |> Option.join) {
      | Some(_) => []
      | None => [
          path
          ++ ": root MissingStep has no backing proof leaf (view would failwith)",
        ]
      };
    here @ violations_of_tail(path, tail);
  | NextStep(sm) =>
    let here =
      switch (sm.proof |> Calc.get_saved_opt) {
      | Some(_) => []
      | None => [
          path ++ ": root NextStep proof Pending (view would failwith)",
        ]
      };
    here
    @ violations_of_kind(path, sm.step_kind)
    @ violations_of_tail(path, sm.next_step);
  }
and violations_of_tail =
    (path: string, ns: StepperBase.next_step): list(string) =>
  switch (ns) {
  | Finished => []
  | MissingStep(_, tail) => violations_of_tail(path, tail)
  | NextStep(sm) =>
    violations_of_kind(path, sm.step_kind)
    @ violations_of_tail(path, sm.next_step)
  }
and violations_of_kind =
    (path: string, k: StepperBase.step_kind_model): list(string) =>
  switch (k) {
  | InductionStep(m) =>
    m.cases
    |> List.mapi((i, c: Web.InductionCase.model'(StepperBase.next_step)) =>
         violations_of_root(
           Printf.sprintf("%s/induction.case%d", path, i),
           c.step,
         )
       )
    |> List.concat
  | ForallStep(m) => violations_of_root(path ++ "/forall", m.inner_stepper)
  | AssumeStep(m) => violations_of_root(path ++ "/assume", m.inner_stepper)
  | RevertStep(m) => violations_of_root(path ++ "/revert", m.inner_stepper)
  | GeneralizeStep(m) =>
    violations_of_root(path ++ "/generalize", m.inner_stepper)
  /* `have` has two proof children, so both roots must be checked. */
  | HaveStep(m) =>
    violations_of_root(path ++ "/have.subproof", m.sub_stepper)
    @ violations_of_root(path ++ "/have.body", m.body_stepper)
  | AxiomStep(_)
  | AlgebriteStep(_)
  | ContradictionStep(_)
  | EvalStep(_) => []
  };

/* Mimic Theorems.Update.calculate's per-theorem stepper wiring. */
let mk_thm_steppers = (src: string): list((string, StepperBase.next_step)) => {
  let exp = parse_exp(src);
  let (state, info_map, _elab) = Test_ProofMap.eval_with_proof(exp);
  let pm = EvaluatorState.get_proof_map(state);
  let settings = {
    ...CoreSettings.on,
    evaluation: {
      ...CoreSettings.on.evaluation,
      enable_proof: true,
      stepper_history: true,
    },
  };
  EvaluatorState.get_theorems(state)
  |> List.map(((id, name, env, stmt)) => {
       let stmt = stmt |> Substitution.in_exp(Environment.empty);
       let goal = stmt |> ProofRule.peel_binders |> (((_, _, core)) => core);
       let ctx =
         info_map
         |> Statics.Map.ctx_of(id)
         |> Option.value(~default=Ctx.empty);
       let sem_ctx =
         ProofCheck.peel_stmt_binders(
           SemanticCtx.of_ctx_and_env(ctx, env),
           stmt,
         )
         |> fst;
       let proof =
         switch (Statics.Map.lookup_exp(id, info_map)) {
         | Some({user_term, _}) =>
           switch (user_term |> Exp.term_of) {
           | Theorem(_, _, proof, _) => Some(proof)
           | _ => None
           }
         | None => None
         };
       let proof =
         switch (proof) {
         | Some(p) => p
         | None => Proof.fresh(EmptyHole)
         };
       let elab_subst =
         goal |> Substitution.in_exp(Builtins.env_init) |> Exp.replace_all_ids;
       let root =
         StepperBase.Stepper.calculate(
           ~settings=Calc.NewValue(settings),
           ~ctx=Calc.NewValue(sem_ctx),
           ~exp=Calc.NewValue(elab_subst),
           ~ana=Calc.NewValue(Typ.fresh(Atom(Bool))),
           ~proof=Calc.NewValue(proof),
           ~proof_map=Calc.NewValue(pm),
           ~proof_info_map=Calc.NewValue(info_map),
           StepperBase.init_step,
         );
       (name, root);
     });
};

let test_no_root_missing_without_leaf = () => {
  let thms = mk_thm_steppers(user_src);
  check(Alcotest.int, "program registers 5 theorems", 5, List.length(thms));
  let all =
    thms
    |> List.map(((name, root)) => violations_of_root(name, root))
    |> List.concat;
  check(
    Alcotest.(list(string)),
    "every stepper root has a backing proof leaf",
    [],
    all,
  );
};

let tests = (
  "Evaluator.RootMissingRepro",
  [
    test_case(
      "user STLC progress program: stepper root invariant",
      `Quick,
      test_no_root_missing_without_leaf,
    ),
  ],
);
