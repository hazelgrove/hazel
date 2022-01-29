/** A typing context */

[@deriving sexp]
type t;

let empty: t;

let bound_var: (TyVar.Name.t, t) => bool;
let has_var_index: (Index.t, t) => bool;
let var_index: (TyVar.Name.t, t) => option(Index.t);
let var_kind: (Index.t, t) => option(KindCore.t);
let var_binding: (Index.t, t) => option((TyVar.Name.t, KindCore.t));
let bind_var: (TyVar.Name.t, KindCore.t, t) => t;

let has_hole: (MetaVar.t, t) => bool;
let hole_kind: (MetaVar.t, t) => option(KindCore.t);
