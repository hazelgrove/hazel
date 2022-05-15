[@deriving sexp]
type t;

let initial: t;

let increment_indices: t => t;
let decrement_indices: t => t;

type kind := KindCore.t(Index.absolute);
type htyp := HTypSyntax.t(Index.absolute);
type index := Index.Abs.t;

/* Type Variables */

let tyvars: t => list((index, TyVar.t, kind));
let tyvar: (t, index) => option(TyVar.t);
let tyvar_index: (t, TyVar.t) => option(index);
let tyvar_kind: (t, index) => option(kind);
let add_tyvar: (t, TyVar.t, kind) => t;
let remove_tyvar: (t, index, htyp) => option(t);
let remove_tyvars: (t, list((index, htyp))) => option(t);
let diff_tyvars: (t, t) => list((index, htyp));

/* Expression Variables */

let vars: t => list((index, Var.t, htyp));
let var: (t, index) => option(Var.t);
let var_index: (t, Var.t) => option(index);
let var_type: (t, Var.t) => option(htyp);
let add_var: (t, Var.t, htyp) => t;
