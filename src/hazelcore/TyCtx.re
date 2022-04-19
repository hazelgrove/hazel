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
 WARNING: only use HTypSyntax and KindCore in here. Using HTyp or Kind directly
 will create a dependency cycle.
 */

open Sexplib.Std;

[@deriving sexp]
type binding =
  | TyVarBinding(TyVar.t, KindCore.t(Index.rel))
  | VarBinding(Var.t, HTypSyntax.t(Index.rel));

[@deriving sexp]
type t = list(binding);

let empty: t = [];

// let bindings: t => list(binding) = tyctx => tyctx;
// let of_list: list(binding) => t = tyctx => tyctx;

/* Type Variables */

// tyvar_bindings
// num_tyvar_bindings

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

let push_tyvar = (tyctx: t, name: string, kind: KindCore.t(Index.abs)): t => {
  let tyvar_binding = TyVarBinding(name, KindCore.abs_to_rel(kind));
  [tyvar_binding, ...tyctx];
};

let pop_tyvar = (tyctx: t): (option((TyVar.t, KindCore.t(Index.rel))), t) => {
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
  | [TyVarBinding(tyvar, kind), ...tyctx'] => (
      Some((tyvar, kind)),
      List.rev(vars) @ tyctx',
    )
  | [VarBinding(_), ..._]
  | [] => (None, List.rev(vars) @ tail)
  };
};

let tyvar_name = (tyctx: t, i: Index.t(Index.abs)): option(string) =>
  switch (List.nth_opt(tyctx, Index.to_int(i))) {
  | Some(TyVarBinding(name, _)) => Some(name)
  | Some(VarBinding(_))
  | None => None
  };

let tyvar_kind =
    (tyctx: t, i: Index.t(Index.abs)): option(KindCore.t(Index.abs)) =>
  switch (List.nth_opt(tyctx, Index.to_int(i))) {
  | Some(TyVarBinding(_, kind)) =>
    Some(KindCore.rel_to_abs(~offset=i, kind))
  | Some(VarBinding(_))
  | None => None
  };

let rec tyvar_index = (tyctx: t, name: string): option(Index.t(Index.abs)) => {
  switch (tyctx) {
  | [TyVarBinding(name', _), ...tyctx'] =>
    name == name'
      ? Some(Index.of_int(0))
      : Option.map(Index.increment, tyvar_index(tyctx', name))
  | [VarBinding(_), ...tyctx'] =>
    Option.map(Index.increment, tyvar_index(tyctx', name))
  | [] => None
  };
};

/* Expression Variables */

// let add_var = (tyctx: t, name: string, ty: HTypSyntax.t): t => [
//   Var(name, ty),
//   ...tyctx,
// ];

// lookup
// let get_var_type = (tyctx, name:string):option(HTypSyntax.t) =>
// switch (tyctx) {

// }

// filter

// map
