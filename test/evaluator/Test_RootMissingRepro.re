open Alcotest;

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
theorem step_ap_val1 = forall u1: Term -> forall u2: Term -> is_value(u1) == false ==> step(TmAp(u1, u2)) == (case step(u1) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, u2)) end) proof assume is_value(u1) == false => from step(TmAp(u1, u2)) do eval end; from is_value(u1) do axiom assume end end; ? in
theorem step_lam_ap2 = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == false ==> step(TmAp(TmLam(w1, q1), u2)) == (case step(u2) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(TmLam(w1, q1), yp)) end) proof assume is_value(u2) == false => from step(TmAp(TmLam(w1, q1), u2)) do eval end; from is_value(TmLam(w1, q1)) do eval end; from is_value(u2) do axiom assume end end; ? in
theorem step_beta = forall w1: Ty -> forall q1: Term -> forall u2: Term -> is_value(u2) == true ==> step(TmAp(TmLam(w1, q1), u2)) == SomeTm(subst((0, u2, q1))) proof assume is_value(u2) == true => from step(TmAp(TmLam(w1, q1), u2)) do eval end; from is_value(TmLam(w1, q1)) do eval end; from is_value(u2) do axiom assume end end; ? in
theorem some_tm_is_some = forall u1: Term -> is_some_tm(SomeTm(u1)) == true proof from is_some_tm(SomeTm(u1)) do eval end; from true == true do axiom refl_eq end end in
theorem progress = forall e0: Term -> forall t0: Ty -> infer(([], e0)) == SomeTy(t0) ==> is_value(e0) || is_some_tm(step(e0)) proof generalize t0 => induction e0
| TmVar(m0) => forall t0 => from infer(([], TmVar(m0))) do eval end; from nth_ty(([], m0)) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmVar(m0)) || is_some_tm(step(TmVar(m0))) do eval end
| TmLam(w0, q0) => forall t0 => assume infer(([], TmLam(w0, q0))) == SomeTy(t0) => from is_value(TmLam(w0, q0)) do eval end; from true || is_some_tm(step(TmLam(w0, q0))) do eval end
| TmAp(x0, y0) => forall t0 => from infer(([], TmAp(x0, y0))) do eval end; induction infer(([], x0)) as tx
| NoTy => from infer(([], x0)) do axiom tx end end; induction infer(([], y0)) as ty
| NoTy => from infer(([], y0)) do axiom ty end end; from ap_result((NoTy, NoTy)) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) do eval end
| SomeTy(s0) => from infer(([], y0)) do axiom ty end end; from ap_result((NoTy, SomeTy(s0))) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) do eval end
end
| SomeTy(tf0) => from infer(([], x0)) do axiom tx end end; induction tf0
| TB => induction infer(([], y0)) as ty
| NoTy => from infer(([], y0)) do axiom ty end end; from ap_result((SomeTy(TB), NoTy)) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) do eval end
| SomeTy(s0) => from infer(([], y0)) do axiom ty end end; from ap_result((SomeTy(TB), SomeTy(s0))) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) do eval end
end
| TArr(ta0, tb0) => induction infer(([], y0)) as ty
| NoTy => from infer(([], y0)) do axiom ty end end; from ap_result((SomeTy(TArr(ta0, tb0)), NoTy)) do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) do eval end
| SomeTy(s0) => from infer(([], y0)) do axiom ty end end; from ap_result((SomeTy(TArr(ta0, tb0)), SomeTy(s0))) do eval end; induction ty_eq((ta0, s0)) as teq
| false => from ty_eq((ta0, s0)) do axiom teq end end; from if false then SomeTy(tb0) else NoTy do eval end; from NoTy == SomeTy(t0) do eval end; from false ==> is_value(TmAp(x0, y0)) || is_some_tm(step(TmAp(x0, y0))) do eval end
| true => from ty_eq((ta0, s0)) do axiom teq end end; from if true then SomeTy(tb0) else NoTy do eval end; assume SomeTy(tb0) == SomeTy(t0) => induction is_value(x0)
| false => from is_value(TmAp(x0, y0)) do eval end; from false || is_some_tm(step(TmAp(x0, y0))) do eval end; from step(TmAp(x0, y0)) do axiom step_ap_val1 end end; induction step(x0) as sx
| NoTm => ?
| SomeTm(u0) => from step(x0) do axiom sx end end; from is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(xp) => SomeTm(TmAp(xp, y0)) end) do eval end
end
| true => induction x0 as hx0
| TmVar(m0) => revert is_value(x0) == true => from x0 do axiom hx0 end end; from is_value(TmVar(m0)) == true do eval end; from false == true do eval end; from false ==> (is_value(TmAp(TmVar(m0), y0)) || is_some_tm(step(TmAp(TmVar(m0), y0)))) do eval end
| TmLam(w0, q0) => from is_value(TmAp(TmLam(w0, q0), y0)) do eval end; from false || is_some_tm(step(TmAp(TmLam(w0, q0), y0))) do eval end; induction is_value(y0)
| true => from step(TmAp(TmLam(w0, q0), y0)) do axiom step_beta end end; from is_some_tm(SomeTm(subst((0, y0, q0)))) do axiom some_tm_is_some end end
| false => from step(TmAp(TmLam(w0, q0), y0)) do axiom step_lam_ap2 end end; induction step(y0) as sy
| NoTm => ?
| SomeTm(u0) => from step(y0) do axiom sy end end; from is_some_tm(case SomeTm(u0) | NoTm => NoTm | SomeTm(yp) => SomeTm(TmAp(TmLam(w0, q0), yp)) end) do eval end
end
end
| TmAp(f0, a0) => revert is_value(x0) == true => from x0 do axiom hx0 end end; from is_value(TmAp(f0, a0)) == true do eval end; from false == true do eval end; from false ==> (is_value(TmAp(TmAp(f0, a0), y0)) || is_some_tm(step(TmAp(TmAp(f0, a0), y0)))) do eval end
end
end
end
end
end
end
end in progress|prog};

/* Every row the walk reaches has the checker's entry, and every row with
 * a goal has its state, in each theorem of a large development. */
let test_rows_are_checked = () => {
  let statics = Test_StepperView.Fixture.statics(user_src);
  let cell =
    Test_StepperView.Fixture.calculate(
      ~is_edited=true,
      statics,
      Web.EvalResult.Model.init,
    );
  let names =
    Web.Theorems.Model.theorems(cell.theorems)
    |> List.map(((_, thm: Web.Theorems.Model.theorem)) => thm.name);
  check(int, "the program has 5 theorems", 5, List.length(names));
  let violations = name => {
    let f = Test_StepperView.Fixture.of_cell(~name, statics, cell);
    Test_StepperView.Fixture.all(f)
    |> List.concat_map((row: Web.StepperRows.t) =>
         (
           row.entry == None
             ? [name ++ ": a row the checker didn't reach"] : []
         )
         @ (
           row.goal != None
           && Web.StepperView.Model.find(row.key, f.theorem.stepper) == None
             ? [name ++ ": a row with a goal but no state"] : []
         )
       );
  };
  check(list(string), "violations", [], List.concat_map(violations, names));
};

let tests = (
  "Evaluator.RootMissingRepro",
  [
    /* Slow: this development takes minutes to check. */
    test_case(
      "user STLC progress program: every row is checked",
      `Slow,
      test_rows_are_checked,
    ),
  ],
);
