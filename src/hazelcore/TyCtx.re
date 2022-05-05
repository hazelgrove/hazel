/*
 combined type and tyvar bindings in one context

 lookup => increment outputs (both types of vars and kinds of tyvars)
 extend/bind => no incrementing needed
 unbind (don't need to do this)

 use relative indices internally
 use absolute indices externally

 n = # tyvar bindings in new ctx - # tyvar bindings in old
 */
/*
 relative indices

 index 0 = next entry (to the right)

 */

/*
 WARNING: Use HTypSyntax and KindCore in here instead of HTyp and Kind. Using
 HTyp or Kind directly will create a dependency cycle.
 */

open Sexplib.Std;

[@deriving sexp]
type binding =
  | TyVarBinding(TyVar.t, KindCore.t(Index.relative))
  | VarBinding(Var.t, HTypSyntax.t(Index.relative));

[@deriving sexp]
type t = list(binding);

let empty: t = [];

// let bindings: t => list(binding) = tyctx => tyctx;
// let of_list: list(binding) => t = tyctx => tyctx;

let increment_indices = (tyctx: t): t =>
  tyctx
  |> List.map(
       fun
       | VarBinding(var, ty) =>
         VarBinding(var, HTypSyntax.increment_indices(ty))
       | TyVarBinding(tyvar, kind) =>
         TyVarBinding(tyvar, KindCore.increment_indices(kind)),
     );

let decrement_indices = (tyctx: t): t =>
  tyctx
  |> List.map(
       fun
       | VarBinding(var, ty) =>
         VarBinding(var, HTypSyntax.decrement_indices(ty))
       | TyVarBinding(tyvar, kind) =>
         TyVarBinding(tyvar, KindCore.decrement_indices(kind)),
     );

let subst_tyvar =
    (tyctx: t, i: Index.Abs.t, ty: HTypSyntax.t(Index.absolute)): t => {
  let i = Index.Abs.to_int(i);
  let rec go = (acc, j, bindings) =>
    j < 0
      ? List.rev(acc)
      : (
        switch (bindings) {
        | [TyVarBinding(t, k), ...bindings'] =>
          let ty = HTypSyntax.to_rel(~offset=i - j, ty);
          let k = KindCore.subst_tyvar(k, Index.Rel.of_int(j), ty);
          let binding = TyVarBinding(t, k);
          [binding, ...go(acc, j - 1, bindings')];
        | [VarBinding(x, ty_x), ...bindings'] =>
          let ty = HTypSyntax.to_rel(~offset=i - j, ty);
          let ty_x = HTypSyntax.subst(ty_x, Index.Rel.of_int(j), ty);
          let binding = VarBinding(x, ty_x);
          [binding, ...go(acc, j - 1, bindings')];
        | [] => bindings
        }
      );
  go([], i, tyctx);
};

// List.map(
//   fun
//   | TyVarBinding(t, k) => TyVarBinding(t, KindCore.subst_tyvar(k, i, ty))
//   | VarBinding(x, ty_x) => VarBinding(x, HTypSyntax.subst(ty_x, i, ty)),
//   tyctx,
// );

/* Expression Variables */

let vars = (tyctx: t): VarMap.t(HTypSyntax.t(Index.absolute)) =>
  tyctx
  // remember absolute positions
  |> List.mapi((j, binding) => (j, binding))
  // discard type variable bindings
  |> List.filter_map(
       fun
       | (j, VarBinding(x, ty)) => Some((j, x, ty))
       | (_, TyVarBinding(_)) => None,
     )
  // convert relative positions to absolute positions
  |> List.map(((j, x, ty)) => (x, HTypSyntax.to_abs(~offset=j, ty)))
  |> VarMap.of_list;

let bind_var = (tyctx: t, x: Var.t, ty: HTypSyntax.t(Index.absolute)): t => {
  [VarBinding(x, HTypSyntax.to_rel(ty)), ...tyctx];
};

let var_type = (tyctx: t, x: Var.t): option(HTypSyntax.t(Index.absolute)) => {
  let rec go = (tyctx, offset) =>
    switch (tyctx) {
    | [VarBinding(y, ty_y), ..._] when Var.eq(y, x) =>
      Some(HTypSyntax.to_abs(~offset, ty_y))
    | [VarBinding(_) | TyVarBinding(_), ...tyctx'] => go(tyctx', offset + 1)
    | [] => None
    };
  go(tyctx, 0);
};

/* Type Variables */

let tyvars = (tyctx: t): TyVarMap.t(KindCore.t(Index.absolute)) =>
  tyctx
  // remember absolute positions
  |> List.mapi((j, binding) => (j, binding))
  // discard expression variable bindings
  |> List.filter_map(
       fun
       | (j, TyVarBinding(t, k)) => Some((j, t, k))
       | (_, VarBinding(_)) => None,
     )
  // convert relative positions to absolute positions
  |> List.map(((j, t, k)) => (t, KindCore.to_abs(~offset=j, k)))
  |> TyVarMap.of_list;

let rec tyvar_index = (tyctx: t, name: string): option(Index.Abs.t) => {
  switch (tyctx) {
  | [TyVarBinding(name', _), ...tyctx'] =>
    name == name'
      ? Some(Index.Abs.of_int(0))
      : Option.map(Index.increment, tyvar_index(tyctx', name))
  | [VarBinding(_), ...tyctx'] =>
    Option.map(Index.increment, tyvar_index(tyctx', name))
  | [] => None
  };
};

let tyvar_name = (tyctx: t, i: Index.Abs.t): option(string) =>
  switch (List.nth_opt(tyctx, Index.Abs.to_int(i))) {
  | Some(TyVarBinding(name, _)) => Some(name)
  | Some(VarBinding(_))
  | None => None
  };

let tyvar_kind =
    (tyctx: t, i: Index.Abs.t): option(KindCore.t(Index.absolute)) =>
  switch (List.nth_opt(tyctx, Index.Abs.to_int(i))) {
  | Some(TyVarBinding(_, kind)) =>
    Some(KindCore.to_abs(~offset=Index.Abs.to_int(i), kind))
  | Some(VarBinding(_))
  | None => None
  };

let num_tyvars: t => int = List.length;

let push_tyvar =
    (tyctx: t, name: string, kind: KindCore.t(Index.absolute)): t => {
  [TyVarBinding(name, KindCore.to_rel(kind)), ...tyctx];
};

let pop_tyvar =
    (tyctx: t): (t, option((TyVar.t, KindCore.t(Index.absolute)))) => {
  // skip any leading expression variables
  let (vars, tail) =
    ListUtil.fold_to(
      (vars, binding) =>
        switch (binding) {
        | VarBinding(_) => Some([binding, ...vars])
        | TyVarBinding(_) => None
        },
      [],
      tyctx,
    );
  switch (tail) {
  | [TyVarBinding(tyvar, k), ...tyctx'] =>
    let tyctx = List.rev(vars) @ tyctx';
    (tyctx, Some((tyvar, KindCore.to_abs(~offset=List.length(vars), k))));
  | [VarBinding(_), ..._]
  | [] => (List.rev(vars) @ tail, None)
  };
};
