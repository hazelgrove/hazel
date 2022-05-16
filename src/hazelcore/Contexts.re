/** Typing Contexts

  A unified binding context for type and expression variables.

  WARNING: Use HTypSyntax and KindCore in here instead of HTyp and Kind. Using
  HTyp or Kind directly will create a dependency cycle.
 */
// TODO: (eric) merge builtins typing context into the other expression variable context
open Sexplib.Std;

[@deriving sexp]
type binding =
  | TyVarBinding(TyVar.t, KindCore.t(Index.relative))
  | VarBinding(Var.t, HTypSyntax.t(Index.relative));

[@deriving sexp]
type t = list(binding);

let initial: t = [];

let increment_indices = (ctx: t): t =>
  ctx
  |> List.map(
       fun
       | VarBinding(x, ty) =>
         VarBinding(x, HTypSyntax.increment_indices(ty))
       | TyVarBinding(t, k) =>
         TyVarBinding(t, KindCore.increment_indices(k)),
     );

let decrement_indices = (ctx: t): t =>
  ctx
  |> List.map(
       fun
       | VarBinding(x, ty) =>
         VarBinding(x, HTypSyntax.decrement_indices(ty))
       | TyVarBinding(t, k) =>
         TyVarBinding(t, KindCore.decrement_indices(k)),
     );

type kind = KindCore.t(Index.absolute);
type htyp = HTypSyntax.t(Index.absolute);
type index = Index.Abs.t;

/* Type Variables */

let tyvars = (ctx: t): list((index, TyVar.t, kind)) =>
  ctx
  |> List.mapi((offset, binding) => (offset, binding))
  |> List.filter_map(
       fun
       | (offset, TyVarBinding(t, k)) => {
           let idx = Index.Abs.of_int(offset);
           let k = KindCore.to_abs(~offset, k);
           Some((idx, t, k));
         }
       | (_, VarBinding(_)) => None,
     );

let tyvar = (ctx: t, idx: index): option(TyVar.t) =>
  switch (List.nth_opt(ctx, Index.Abs.to_int(idx))) {
  | Some(TyVarBinding(t, _)) => Some(t)
  | Some(VarBinding(_))
  | None => None
  };

let rec tyvar_index = (ctx: t, t: TyVar.t): option(index) =>
  switch (ctx) {
  | [TyVarBinding(t', _), ...ctx'] =>
    TyVar.equal(t, t')
      ? Some(Index.Abs.of_int(0))
      : Option.map(Index.increment, tyvar_index(ctx', t))
  | [VarBinding(_), ...ctx'] =>
    Option.map(Index.increment, tyvar_index(ctx', t))
  | [] => None
  };

let tyvar_kind = (ctx: t, idx: index): option(kind) =>
  switch (List.nth_opt(ctx, Index.Abs.to_int(idx))) {
  | Some(TyVarBinding(_, k)) =>
    Some(KindCore.to_abs(~offset=Index.Abs.to_int(idx), k))
  | Some(VarBinding(_))
  | None => None
  };

let add_tyvar = (ctx: t, t: TyVar.t, k: kind): t => [
  TyVarBinding(t, KindCore.to_rel(~offset=1, k)),
  ...ctx,
];

/* assumes idx does not occur in ty */
let rec remove_tyvar = (ctx: t, idx: index, ty: htyp): option(t) => {
  open OptUtil.Syntax;
  let i = Index.Abs.to_int(idx);
  i < 0
    ? None
    : (
      switch (i, ctx) {
      | (0, [TyVarBinding(_), ...ctx']) => Some(ctx')
      | (0, [VarBinding(_) as binding, ...ctx']) =>
        let+ ctx' = remove_tyvar(ctx', idx, ty);
        [binding, ...ctx'];
      | (_, [TyVarBinding(t, k), ...ctx']) =>
        let k = KindCore.shift_indices(~above=i, ~amount=-1, k);
        let+ ctx' = remove_tyvar(ctx', Index.decrement(idx), ty);
        [TyVarBinding(t, k), ...ctx'];
      | (_, [VarBinding(x, ty_x), ...ctx']) =>
        let ty_x = HTypSyntax.shift_indices(~above=i, ~amount=-1, ty_x);
        let+ ctx' = remove_tyvar(ctx', idx, ty);
        [VarBinding(x, ty_x), ...ctx'];
      | (_, []) => None
      }
    );
};

/* assumes tyvars are ordered by index (asc) */
let rec remove_tyvars = (ctx: t, tyvars: list((index, htyp))): option(t) =>
  switch (tyvars) {
  | [(idx, ty), ...tyvars'] =>
    open OptUtil.Syntax;
    let* ctx = remove_tyvar(ctx, idx, ty);
    remove_tyvars(ctx, tyvars');
  | [] => Some(ctx)
  };

let diff_tyvars = (new_ctx: t, old_ctx: t): list((index, htyp)) => {
  let new_tyvars = tyvars(new_ctx);
  let old_tyvars = tyvars(old_ctx);
  let n = List.length(new_tyvars) - List.length(old_tyvars);
  ListUtil.take(new_tyvars, n)
  |> List.map(((idx, _, k)) => (idx, KindCore.canonical_type(k)));
};

/* Expression Variables */

let vars = (ctx: t): list((index, Var.t, htyp)) =>
  ctx
  |> List.mapi((offset, binding) => (offset, binding))
  |> List.filter_map(
       fun
       | (offset, VarBinding(x, ty)) => {
           let idx = Index.Abs.of_int(offset);
           let ty = HTypSyntax.to_abs(~offset, ty);
           Some((idx, x, ty));
         }
       | (_, TyVarBinding(_)) => None,
     );

let var = (ctx: t, idx: index): option(Var.t) =>
  switch (List.nth_opt(ctx, Index.Abs.to_int(idx))) {
  | Some(VarBinding(x, _)) => Some(x)
  | Some(TyVarBinding(_))
  | None => None
  };

let rec var_index = (ctx: t, x: Var.t): option(index) =>
  switch (ctx) {
  | [VarBinding(x', _), ...ctx'] =>
    Var.eq(x, x')
      ? Some(Index.Abs.of_int(0))
      : Option.map(Index.increment, var_index(ctx', x))
  | [TyVarBinding(_), ...ctx'] =>
    Option.map(Index.increment, var_index(ctx', x))
  | [] => None
  };

let var_index_type = (ctx: t, idx: index): option(htyp) =>
  switch (List.nth_opt(ctx, Index.Abs.to_int(idx))) {
  | Some(VarBinding(_, ty)) =>
    Some(HTypSyntax.to_abs(~offset=Index.Abs.to_int(idx), ty))
  | Some(TyVarBinding(_))
  | None => None
  };

let var_type = (ctx: t, x: Var.t): option(htyp) => {
  open OptUtil.Syntax;
  let* idx = var_index(ctx, x);
  var_index_type(ctx, idx);
};

let add_var = (ctx: t, x: Var.t, ty: htyp): t => [
  VarBinding(x, HTypSyntax.to_rel(~offset=1, ty)),
  ...ctx,
];
