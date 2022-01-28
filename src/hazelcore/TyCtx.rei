open TyVar;

/** Associates a type variable with its kind */
module Vars: {
  // [@deriving sexp]
  type binding = (Name.t, Kind.t);

  // [@deriving sexp]
  type t;

  let empty: t;
  let bind: (Name.t, Kind.t, t) => t;
  let index: (~offset: int=?, Name.t, t) => option(Index.t);
  let has_index: (Index.t, t) => bool;
  let binding: (Index.t, t) => option(binding);
  let bound: (Name.t, t) => bool;
  let kind: (Index.t, t) => option(Kind.t);
};

/** Associates a hole with its kind */
module Holes: {
  type map(+'a);
  include Map.S with type key = Index.t and type t(+'a) := map('a);

  // type t = map(Kind.t);
  type t;

  let kind: (MetaVar.t, t) => option(Kind.t);
  // let sexp_of_t: t => Sexplib.Sexp.t;
  // let t_of_sexp: Sexplib.Sexp.t => map(Kind.t);
};

/** A typing context */
// [@deriving sexp]
// type t = {
//   vars: Vars.t,
//   holes: Holes.t,
// };
type t;

let empty: t;

let bound_var: (Name.t, t) => bool;
let has_var_index: (Index.t, t) => bool;
let var_index: (Name.t, t) => option(Index.t);
let var_kind: (Index.t, t) => option(Kind.t);
let var_binding: (Index.t, t) => option(Vars.binding);
let bind_var: (TyVar.Name.t, Kind.t, t) => t;

let has_hole: (MetaVar.t, t) => bool;
let hole_kind: (MetaVar.t, t) => option(Kind.t);

// [@deriving sexp]
// type t;
// let to_list: t => list((TyId.t, Kind.t));
// let of_list: list((TyId.t, Kind.t)) => t;
// let extend: (t, (TyId.t, Kind.t)) => t;
// let empty: t;
// let index_of: (t, TyId.t) => option(TyVarIndex.t);
// let index_of_exn: (t, TyId.t) => TyVarIndex.t;
// let contains: (t, TyId.t) => bool;
// let tyvar_with_idx: (t, TyVarIndex.t) => (TyId.t, Kind.t);
// let consistent: (t, t) => bool;
