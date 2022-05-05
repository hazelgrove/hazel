/*
 WARNING: Use KindCore instead of Kind in here. Using Kind directly will create
 a dependency cycle.
 */

[@deriving sexp]
type t = (VarMap.t(HTyp.t), TyCtx.t);
let initial: t = (VarMap.empty, TyCtx.empty);

// Expression Variables

let vars = ((_, tyctx): t): VarMap.t(HTyp.t) =>
  TyCtx.vars(tyctx) |> VarMap.map(((_, ty)) => HTyp.of_unsafe(ty));

let bind_var = ((builtins, tyctx): t, x: Var.t, ty: HTyp.t): t => {
  (builtins, TyCtx.bind_var(tyctx, x, HTyp.unsafe(ty)));
};

let var_type = ((_, tyctx): t, x: Var.t): option(HTyp.t) => {
  open OptUtil.Syntax;
  let+ ty = TyCtx.var_type(tyctx, x);
  HTyp.of_unsafe(ty);
};

// Type Variables

let num_tyvars = ((_, tyctx): t): int => TyCtx.num_tyvars(tyctx);

let tyvars = ((_, tyctx): t): TyVarMap.t(KindCore.t(Index.absolute)) =>
  TyCtx.tyvars(tyctx);

let push_tyvar =
    ((builtins, tyctx): t, t: TyVar.t, k: KindCore.t(Index.absolute)): t => {
  (builtins, TyCtx.push_tyvar(tyctx, t, k));
};

let pop_tyvar =
    ((builtins, tyctx): t)
    : (t, option((TyVar.t, KindCore.t(Index.absolute)))) => {
  let (tyctx, popped) = TyCtx.pop_tyvar(tyctx);
  ((builtins, tyctx), popped);
};

let tyvar_index = ((_, tyctx): t, name: string): option(Index.Abs.t) =>
  TyCtx.tyvar_index(tyctx, name);

let tyvar_name = ((_, tyctx): t, i: Index.Abs.t): option(string) =>
  TyCtx.tyvar_name(tyctx, i);

let tyvar_kind =
    ((_, tyctx): t, i: Index.Abs.t): option(KindCore.t(Index.absolute)) =>
  TyCtx.tyvar_kind(tyctx, i);

// Types

let normalize = ((_, tyctx): t, ty: HTyp.t): HTyp.normalized =>
  HTyp.normalize(tyctx, ty);

let head_normalize = ((_, tyctx): t, ty: HTyp.t): HTyp.head_normalized =>
  HTyp.head_normalize(tyctx, ty);

let consistent = (ctx: t, ty: HTyp.t, ty': HTyp.t): bool =>
  HTyp.normalized_consistent(normalize(ctx, ty'), normalize(ctx, ty));

let equivalent = (ctx: t, ty: HTyp.t, ty': HTyp.t): bool =>
  HTyp.normalized_equivalent(normalize(ctx, ty), normalize(ctx, ty'));

let matched_arrow = ((_, tyctx): t, ty: HTyp.t): option((HTyp.t, HTyp.t)) =>
  HTyp.matched_arrow(tyctx, ty);

let matched_list = ((_, tyctx): t, ty: HTyp.t): option(HTyp.t) =>
  HTyp.matched_list(tyctx, ty);

let matched_sum = ((_, tyctx): t, ty: HTyp.t): option((HTyp.t, HTyp.t)) =>
  HTyp.matched_sum(tyctx, ty);

let join_all =
    ((_, tyctx): t, j: HTyp.join, types: list(HTyp.t)): option(HTyp.t) =>
  HTyp.join_all(tyctx, j, types);

let subst_tyvar = ((builtins, tyctx): t, i: Index.Abs.t, ty: HTyp.t): t => {
  let tyctx = TyCtx.subst_tyvar(tyctx, i, HTyp.unsafe(ty));
  (builtins, tyctx);
};

// let unbind0 = ((gamma, builtins, tyctx): t): t => {
//   let kind =
//     TyCtx.tyvar_kind(tyctx, Index.of_int(0))
//     |> Option.value(~default=KindCore.KHole);
//   let gamma = VarMap.subst_tyvar(gamma, 0, Kind.canonical_type(kind));
//   let tyctx = TyCtx.pop_tyvar(tyctx) |> Option.fold(~none=tyctx, ~some=snd);
//   (gamma, builtins, tyctx);
// };
