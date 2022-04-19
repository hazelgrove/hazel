[@deriving sexp]
type t = (VarCtx.t, VarCtx.t, TyCtx.t);
let initial: t = (VarCtx.empty, VarCtx.empty, TyCtx.empty);

let extend_gamma = (ctx: t, binding: (Var.t, HTyp.t)): t => {
  let (gamma, builtins, tyvars) = ctx;
  let gamma' = VarCtx.extend(gamma, binding);
  (gamma', builtins, tyvars);
};

let extend_tyvars = ((gamma, builtins, tyctx): t, name: string, kind: Kind.t) => {
  (gamma, builtins, TyCtx.push_tyvar(tyctx, name, kind));
};

// let unbind0 = ((gamma, builtins, tyctx): t): t => {
//   let kind =
//     TyCtx.tyvar_kind(tyctx, Index.of_int(0))
//     |> Option.value(~default=KindCore.KHole);
//   let gamma = VarCtx.subst_tyvar(gamma, 0, Kind.canonical_type(kind));
//   let tyctx = TyCtx.pop_tyvar(tyctx) |> Option.fold(~none=tyctx, ~some=snd);
//   (gamma, builtins, tyctx);
// };
