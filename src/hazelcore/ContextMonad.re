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

let tyvars: t(list((KindSystem.ContextRef.t, TyVar.t, Kind.t))) =
  Context.tyvars +-+ get();

let tyvar = (cref: KindSystem.ContextRef.t): t(option(TyVar.t)) =>
  Context.tyvar +-+ get() +~. cref;

let tyvar_ref = (t: TyVar.t): t(option(KindSystem.ContextRef.t)) =>
  Context.tyvar_ref +-+ get() +~. t;

let tyvar_kind = (cref: KindSystem.ContextRef.t): t(option(Kind.t)) =>
  Context.tyvar_kind +-+ get() +~. cref;

let add_tyvar = (x: TyVar.t, k: Kind.t): t(unit) =>
  put @>+ Context.add_tyvar +-+ get() +~. x +~. k;

let reduce_tyvars = (old_ctx: Context.t, ty: HTyp.t): t(HTyp.t) =>
  Context.reduce_tyvars +-+ get() +~. old_ctx +~. ty;

/* Expression Variables */

let vars: t(list((KindSystem.ContextRef.t, Var.t, HTyp.t))) =
  Context.vars +-+ get();

let var = (cref: KindSystem.ContextRef.t): t(option(Var.t)) =>
  Context.var +-+ get() +~. cref;

let var_ref = (x: Var.t): t(option(KindSystem.ContextRef.t)) =>
  Context.var_ref +-+ get() +~. x;

let var_type = (x: Var.t): t(option(HTyp.t)) =>
  Context.var_type +-+ get() +~. x;

let add_var = (x: Var.t, ty: HTyp.t): t(unit) =>
  ctx => put(Context.add_var(ctx, x, ty), ctx);
