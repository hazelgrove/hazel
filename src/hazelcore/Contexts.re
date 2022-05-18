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

type kind = KindCore.t(Index.absolute);
type htyp = HTypSyntax.t(Index.absolute);
type index = Index.Abs.t;

/* Type Variables */

let rec nth_tyvar_binding =
        (ctx: t, n: int): option((TyVar.t, KindCore.t(Index.relative))) =>
  switch (n, ctx) {
  | (0, [TyVarBinding(t, k), ..._]) => Some((t, k))
  | (_, [TyVarBinding(_), ...ctx']) => nth_tyvar_binding(ctx', n - 1)
  | (_, [VarBinding(_), ...ctx']) => nth_tyvar_binding(ctx', n)
  | (_, []) => None
  };

let first_tyvar_binding =
    (ctx: t, f: (TyVar.t, KindCore.t(Index.relative)) => bool)
    : option((int, TyVar.t, KindCore.t(Index.relative))) => {
  let rec go = (n, ctx) =>
    switch (ctx) {
    | [TyVarBinding(t, k), ...ctx'] =>
      f(t, k) ? Some((n, t, k)) : go(n + 1, ctx')
    | [VarBinding(_), ...ctx'] => go(n, ctx')
    | [] => None
    };
  go(0, ctx);
};

/* let tyvars = (ctx: t): list((index, TyVar.t, kind)) => { */
/*   let rec go = (i, n, ctx) => */
/*     switch (ctx) { */
/*     | [TyVarBinding(t, k), ...ctx] => [ */
/*         (Index.Abs.of_int(i), t, KindCore.to_abs(~offset=n, k)), */
/*         ...go(i + 1, n, ctx), */
/*       ] */
/*     | [VarBinding(_), ...ctx] => go(i + 1, n, ctx) */
/*     | [] => [] */
/*     }; */
/*   go(0, 0, ctx); */
/* }; */

let tyvars = (ctx: t): list((index, TyVar.t, kind)) =>
  ctx
  |> List.filter_map(
       fun
       | TyVarBinding(t, k) => Some((t, k))
       | VarBinding(_) => None,
     )
  |> List.mapi((i, (t, k)) =>
       (Index.Abs.of_int(i), t, KindCore.to_abs(~offset=i + 1, k))
     );

let tyvar = (ctx: t, idx: index): option(TyVar.t) => {
  open OptUtil.Syntax;
  let+ (t, _) = nth_tyvar_binding(ctx, Index.Abs.to_int(idx));
  t;
};

let tyvar_index = (ctx: t, t: TyVar.t): option(index) => {
  open OptUtil.Syntax;
  let+ (i, _, _) = first_tyvar_binding(ctx, (t', _) => TyVar.equal(t, t'));
  Index.Abs.of_int(i);
};

let tyvar_kind = (ctx: t, idx: index): option(kind) => {
  open OptUtil.Syntax;
  let i = Index.Abs.to_int(idx);
  let+ (_, k) = nth_tyvar_binding(ctx, i);
  KindCore.to_abs(~offset=i, k);
};

let add_tyvar = (ctx: t, t: TyVar.t, k: kind): t => [
  TyVarBinding(t, KindCore.to_rel(k)),
  ...ctx,
];

/* /\* assumes idx does not occur in ty *\/ */
/* let remove_tyvar = (ctx: t, idx: index, ty: htyp): option(t) => { */
/*   open OptUtil.Syntax; */
/*   let i = Index.Abs.to_int(idx); */
/*   let rec go = (n, ctx) => { */
/*     /\* let ty = HTypSyntax.to_rel(~offset=- n, ty); *\/ */
/*     switch (ctx) { */
/*     | [TyVarBinding(_), ...ctx'] => */
/*       let+ ctx' = go(n, ctx'); */
/*       if (n < i) { */
/*         let k = k |> eliminate_tyvar_in_kind |> decrement_tyvar_uses_in_kind; */
/*         [TyVarBinding(t, k), ...ctx']; */
/*       } else { */
/*         ctx'; */
/*       }; */
/*     | [VarBinding(x, ty_x), ...ctx'] => */
/*       let+ ctx' = go(n, ctx'); */
/*       let ty_x = */
/*         n <= i */
/*           ? ty_x |> eliminate_tyvar_in_htyp |> decrement_tyvar_uses_in_htyp */
/*           : ty_x; */
/*       [VarBinding(x, ty_x), ...ctx']; */
/*     | [] => None */
/*     }; */
/*   }; */
/*   go(0, ctx); */
/* }; */

/* let remove_tyvar = (ctx: t, idx: index, ty: htyp): option(t) => { */
/*   open OptUtil.Syntax; */
/*   let i = Index.Abs.to_int(idx); */
/*   let rec go = (j, ctx) => */
/*     if (i < 0 || j > i) { */
/*       None; /\* index out of bounds *\/ */
/*     } else if (j == i) { */
/*       /\* drop the i-th tyvar binding and leave the rest alone *\/ */
/*       switch (ctx) { */
/*       | [TyVarBinding(_), ...ctx'] => Some(ctx') */
/*       | [VarBinding(_) as binding, ...ctx'] => */
/*         let+ ctx' = go(j, ctx'); */
/*         [binding, ...ctx']; */
/*       | [] => None */
/*       }; */
/*     } else { */
/*       let i_rel = i - j - 1; */
/*       let ty = HTypSyntax.to_rel(~offset=i_rel, ty); */
/*       switch (ctx) { */
/*       | [TyVarBinding(t, k), ...ctx'] => */
/*         let+ ctx' = go(j + 1, ctx'); */
/*         /\* remove from preceding bindings *\/ */
/*         let k = KindCore.subst_tyvar(k, Index.Rel.of_int(i - j), ty); */
/*         /\* decrement uses of following bindings *\/ */
/*         let k = KindCore.shift_indices(~above=offset, ~amount=-1, k); */
/*         [TyVarBinding(t, k), ...ctx']; */
/*       | [VarBinding(x, ty_x), ...ctx'] => */
/*         let+ ctx' = go(j + 1, ctx'); */
/*         let ty_x = HTypSyntax.subst(ty_x, Index.Rel.of_int(offset), ty); */
/*         let ty_x = HTypSyntax.shift_indices(~above=offset, ~amount=-1, ty_x); */
/*         [VarBinding(x, ty_x), ...ctx']; */
/*       | [] => None */
/*       }; */
/*     }; */
/*   go(0, ctx); */
/* }; */

/* let remove_tyvars = (ctx: t, tyvars: list((index, htyp))): option(t) => */
/*   List.fold_left( */
/*     (ctx_opt, (idx, ty)) => { */
/*       open OptUtil.Syntax; */
/*       let* ctx = ctx_opt; */
/*       remove_tyvar(ctx, idx, ty); */
/*     }, */
/*     Some(ctx), */
/*     tyvars, */
/*   ); */

let diff_tyvars = (new_ctx: t, old_ctx: t): list((index, htyp)) => {
  let new_tyvars = tyvars(new_ctx);
  let old_tyvars = tyvars(old_ctx);
  let n = List.length(new_tyvars) - List.length(old_tyvars);
  ListUtil.take(new_tyvars, n)
  |> List.map(((idx, _, k)) => (idx, KindCore.canonical_type(k)));
};

/* Expression Variables */

let nth_var_binding =
    (ctx: t, n: int): option((int, Var.t, HTypSyntax.t(Index.relative))) => {
  let rec go = (i, n, ctx) =>
    switch (n, ctx) {
    | (0, [VarBinding(x, ty), ..._]) => Some((i, x, ty))
    | (_, [VarBinding(_), ...ctx']) => go(i, n - 1, ctx')
    | (_, [TyVarBinding(_), ...ctx']) => go(i + 1, n, ctx')
    | (_, []) => None
    };
  go(0, n, ctx);
};

let vars = (ctx: t): list((index, Var.t, htyp)) => {
  let rec go = (i, n, ctx) =>
    switch (ctx) {
    | [TyVarBinding(_), ...ctx] => go(i, n + 1, ctx)
    | [VarBinding(x, ty), ...ctx] => [
        (Index.Abs.of_int(i), x, HTypSyntax.to_abs(~offset=n, ty)),
        ...go(i + 1, n, ctx),
      ]
    | [] => []
    };
  go(0, 0, ctx);
};

let var = (ctx: t, idx: index): option(Var.t) => {
  open OptUtil.Syntax;
  let+ (_, x, _) = nth_var_binding(ctx, Index.Abs.to_int(idx));
  x;
};

let var_index = (ctx: t, x: Var.t): option(index) => {
  let rec go = (i, ctx') =>
    switch (ctx') {
    | [VarBinding(x', _), ...ctx'] =>
      Var.eq(x, x') ? Some(Index.Abs.of_int(i)) : go(i + 1, ctx')
    | [TyVarBinding(_), ...ctx'] => go(i, ctx')
    | [] => None
    };
  go(0, ctx);
};

let var_index_type = (ctx: t, idx: index): option(htyp) => {
  open OptUtil.Syntax;
  let+ (n, _, ty) = nth_var_binding(ctx, Index.Abs.to_int(idx));
  HTypSyntax.to_abs(~offset=n, ty);
};

let var_type = (ctx: t, x: Var.t): option(htyp) => {
  print_endline("--- CONTEXTS var_type ---");
  print_endline(Sexplib.Sexp.to_string_hum(sexp_of_t(ctx)));
  print_endline(Sexplib.Sexp.to_string_hum(Var.sexp_of_t(x)));
  open OptUtil.Syntax;
  let* idx = var_index(ctx, x);
  var_index_type(ctx, idx);
};

let add_var = (ctx: t, x: Var.t, ty: htyp): t => [
  VarBinding(x, HTypSyntax.to_rel(ty)),
  ...ctx,
];
