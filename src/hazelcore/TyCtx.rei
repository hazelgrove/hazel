open TyVar;

/** Associates a type variable with its kind */
module Vars: {
  // [@deriving sexp]
  type binding = (Name.t, Kind.t(HTyp.t));

  // [@deriving sexp]
  type t = list(binding);

  let empty: t;
  let extend:
    (Name.t, Kind.t(HTyp.t), ~increment_singleton: binding => binding, t) => t;

  let index: (~offset: int=?, Name.t, t) => option(Index.t);
  let has_index: (Index.t, t) => bool;
  let binding: (Index.t, t) => option(binding);
  let bound: (Name.t, t) => bool;
  let kind: (Index.t, t) => option(Kind.t(HTyp.t));
};

/** Associates a hole with its kind */
module Holes: {
  type map(+'a);
  include Map.S with type key = Index.t and type t(+'a) := map('a);

  type t = map(Kind.t(HTyp.t));

  let kind: (MetaVar.t, t) => option(Kind.t(HTyp.t));
  // let sexp_of_t: t => Sexplib.Sexp.t;
  // let t_of_sexp: Sexplib.Sexp.t => map(Kind.t(HTyp.t));
};

/** A typing context */
// [@deriving sexp]
type t = {
  vars: Vars.t,
  holes: Holes.t,
};

type join =
  | GLB
  | LUB;

let empty: t;
let var_index: (Name.t, t) => option(Index.t);
let var_kind: (Index.t, t) => option(Kind.t(HTyp.t));
let var_bound: (Name.t, t) => bool;
let hole_kind: (MetaVar.t, t) => option(Kind.t(HTyp.t));
let equivalent_kind: (Kind.t(HTyp.t), Kind.t(HTyp.t), t) => bool;
let equivalent: (HTyp.t, HTyp.t, t) => bool;
let consistent: (HTyp.t, HTyp.t, t) => bool;
let join: (join, HTyp.t, HTyp.t, t) => option(HTyp.t);

// [@deriving sexp]
// type t;
// let to_list: t => list((TyId.t, Kind.t(HTyp.t)));
// let of_list: list((TyId.t, Kind.t(HTyp.t))) => t;
// let extend: (t, (TyId.t, Kind.t(HTyp.t))) => t;
// let empty: t;
// let index_of: (t, TyId.t) => option(TyVarIndex.t);
// let index_of_exn: (t, TyId.t) => TyVarIndex.t;
// let contains: (t, TyId.t) => bool;
// let tyvar_with_idx: (t, TyVarIndex.t) => (TyId.t, Kind.t(HTyp.t));
// let consistent: (t, t) => bool;
