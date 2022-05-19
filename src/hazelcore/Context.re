/* module HTyp = KindSystem.HTyp; */
include KindSystem.Context;

let initial: t = [];

/* Type Variables */

let tyvars = (ctx: t): list((Index.Abs.t, TyVar.t, Kind.t)) =>
  ctx
  |> List.filter_map(
       fun
       | TyVarBinding(t, k) => Some((t, k))
       | VarBinding(_) => None,
     )
  |> List.mapi((i, (t, k)) =>
       (Index.Abs.of_int(i), t, KindSystem.Kind.to_abs(~offset=i + 1, k))
     );

let tyvar = (ctx: t, idx: Index.Abs.t): option(TyVar.t) => {
  open OptUtil.Syntax;
  let+ (t, _) = nth_tyvar_binding(ctx, Index.Abs.to_int(idx));
  t;
};

let tyvar_index = (ctx: t, t: TyVar.t): option(Index.Abs.t) => {
  open OptUtil.Syntax;
  let+ (i, _, _) = first_tyvar_binding(ctx, (t', _) => TyVar.equal(t, t'));
  Index.Abs.of_int(i);
};

let add_tyvar = (ctx: t, t: TyVar.t, k: Kind.t): t => [
  TyVarBinding(t, KindSystem.Kind.to_rel(k)),
  ...ctx,
];

let diff_tyvars = (new_ctx: t, old_ctx: t): list((Index.Abs.t, HTyp.t)) => {
  let new_tyvars = tyvars(new_ctx);
  let old_tyvars = tyvars(old_ctx);
  let n = List.length(new_tyvars) - List.length(old_tyvars);
  ListUtil.take(new_tyvars, n)
  |> List.map(((idx, _, k)) =>
       (idx, HTyp.of_syntax(KindSystem.Kind.to_htyp(k)))
     );
};

/* Expression Variables */

let vars = (ctx: t): list((Index.Abs.t, Var.t, HTyp.t)) => {
  let rec go = (i, n, ctx) =>
    switch (ctx) {
    | [TyVarBinding(_), ...ctx] => go(i, n + 1, ctx)
    | [VarBinding(x, ty), ...ctx] => [
        (
          Index.Abs.of_int(i),
          x,
          HTyp.of_syntax(KindSystem.HTyp.to_abs(~offset=n, ty)),
        ),
        ...go(i + 1, n, ctx),
      ]
    | [] => []
    };
  go(0, 0, ctx);
};

let var = (ctx: t, idx: Index.Abs.t): option(Var.t) => {
  open OptUtil.Syntax;
  let+ (_, x, _) = nth_var_binding(ctx, Index.Abs.to_int(idx));
  x;
};

let var_index = (ctx: t, x: Var.t): option(Index.Abs.t) => {
  let rec go = (i, ctx') =>
    switch (ctx') {
    | [VarBinding(x', _), ...ctx'] =>
      Var.eq(x, x') ? Some(Index.Abs.of_int(i)) : go(i + 1, ctx')
    | [TyVarBinding(_), ...ctx'] => go(i, ctx')
    | [] => None
    };
  go(0, ctx);
};

let var_index_type = (ctx: t, idx: Index.Abs.t): option(HTyp.t) => {
  open OptUtil.Syntax;
  let+ (n, _, ty) = nth_var_binding(ctx, Index.Abs.to_int(idx));
  HTyp.of_syntax(KindSystem.HTyp.to_abs(~offset=n, ty));
};

let var_type = (ctx: t, x: Var.t): option(HTyp.t) => {
  print_endline("--- CONTEXTS var_type ---");
  print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(ctx)));
  print_endline(Sexplib.Sexp.to_string_hum(Var.sexp_of_t(x)));
  open OptUtil.Syntax;
  let* idx = var_index(ctx, x);
  var_index_type(ctx, idx);
};

let add_var = (ctx: t, x: Var.t, ty: HTyp.t): t => [
  VarBinding(x, KindSystem.HTyp.to_rel(HTyp.to_syntax(ty))),
  ...ctx,
];
