module T = {
  type state = Context.t;
  type t('result) = state => ('result, state);

  let return: 'a => t('result) = (x, ctx) => (x, ctx);

  let bind: (t('a), 'a => t('b)) => t('b) =
    (a, f, ctx_a) => {
      let (x, ctx_b) = a(ctx_a);
      f(x, ctx_b);
    };

  let map = Monads.MapDefinition.Define_using_bind;
};

include T;
include Monads.State.Make(T);
open Infix;

/* Type Variables */

let tyvars: t(list((Index.Abs.t, TyVar.t, Kind.t))) =
  Context.tyvars +-+ get();

let tyvar = (idx: Index.Abs.t): t(option(TyVar.t)) =>
  Context.tyvar +-+ get() +~. idx;

let tyvar_index = (t: TyVar.t): t(option(Index.Abs.t)) =>
  Context.tyvar_index +-+ get() +~. t;

let tyvar_kind = (idx: Index.Abs.t): t(option(Kind.t)) =>
  Context.tyvar_kind +-+ get() +~. idx;

let add_tyvar = (x: TyVar.t, k: Kind.t): t(unit) =>
  put @>+ Context.add_tyvar +-+ get() +~. x +~. k;

let diff_tyvars = (new_ctx: Context.t): t(list((Index.Abs.t, HTyp.t))) =>
  Context.diff_tyvars(new_ctx) +-+ get();

/* Expression Variables */

let vars: t(list((Index.Abs.t, Var.t, HTyp.t))) = Context.vars +-+ get();

let var = (idx: Index.Abs.t): t(option(Var.t)) =>
  Context.var +-+ get() +~. idx;

let var_index = (x: Var.t): t(option(Index.Abs.t)) =>
  Context.var_index +-+ get() +~. x;

let var_type = (x: Var.t): t(option(HTyp.t)) =>
  Context.var_type +-+ get() +~. x;

let add_var = (x: Var.t, ty: HTyp.t): t(unit) =>
  ctx => put(Context.add_var(ctx, x, ty), ctx);
