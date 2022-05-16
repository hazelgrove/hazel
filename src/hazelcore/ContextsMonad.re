module T = {
  type state = Contexts.t;
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

let increment_indices: t(unit) =
  ctx => put(Contexts.increment_indices(ctx), ctx);

let decrement_indices: t(unit) =
  ctx => put(Contexts.decrement_indices(ctx), ctx);

type kind = KindCore.t(Index.absolute);
type htyp = HTypSyntax.t(Index.absolute);
type index = Index.Abs.t;

/* Type Variables */

let tyvars: t(list((index, string, kind))) = Contexts.tyvars +-+ get();

let tyvar = (idx: index): t(option(TyVar.t)) =>
  Contexts.tyvar +-+ get() +~. idx;

let tyvar_index = (t: TyVar.t): t(option(index)) =>
  Contexts.tyvar_index +-+ get() +~. t;

let tyvar_kind = (idx: index): t(option(kind)) =>
  Contexts.tyvar_kind +-+ get() +~. idx;

let add_tyvar = (x: TyVar.t, k: kind): t(unit) =>
  put @>+ Contexts.add_tyvar +-+ get() +~. x +~. k;

let remove_tyvar = (idx: index, ty: htyp): t(bool) =>
  Contexts.remove_tyvar
  +-+ get()
  +~. idx
  +~. ty
  @+> Option.fold(~none=return(false), ~some=ctx => put(ctx) $+. true);

let remove_tyvars = (tyvars: list((index, htyp))): t(bool) =>
  Contexts.remove_tyvars
  +-+ get()
  +~. tyvars
  @+> Option.fold(~none=return(false), ~some=ctx => put(ctx) $+. true);

let diff_tyvars = (new_ctx: Contexts.t): t(list((index, htyp))) =>
  Contexts.diff_tyvars(new_ctx) +-+ get();

/* Expression Variables */

let vars: t(list((index, Var.t, htyp))) = Contexts.vars +-+ get();

let var = (idx: index): t(option(Var.t)) => Contexts.var +-+ get() +~. idx;

let var_index = (x: Var.t): t(option(index)) =>
  Contexts.var_index +-+ get() +~. x;

let var_type = (x: Var.t): t(option(htyp)) =>
  Contexts.var_type +-+ get() +~. x;

let add_var = (x: Var.t, ty: HTyp.t): t(unit) =>
  ctx => put(Contexts.add_var(ctx, x, HTyp.unsafe(ty)), ctx);
