open TyVar;

/** Associates a type variable with its kind */
module Vars: {
  [@deriving sexp]
  type binding = (Name.t, Kind.t);

  [@deriving sexp]
  type t = list(binding);

  let empty: t;
  let extend:
    (Name.t, Kind.t, ~increment_singleton: binding => binding, t) => t;

  let index: (Name.t, t) => option(Index.t);
  let has_index: (Index.t, t) => bool;
  let binding: (Index.t, t) => option(binding);
};

/** Associates a hole with its kind */
module Holes: {
  type map(+'a);
  include Map.S with type key = Index.t and type t(+'a) := map('a);

  type t = map(Kind.t);

  let sexp_of_t: t => Sexplib.Sexp.t;
  let t_of_sexp: Sexplib.Sexp.t => map(Kind.t);
};

/** A typing context */
[@deriving sexp]
type t = {
  vars: Vars.t,
  holes: Holes.t,
};

type join =
  | GLB
  | LUB;

let empty: t;
let consistent: (HTyp.t, HTyp.t, t) => bool;
let join: (join, HTyp.t, HTyp.t, t) => option(HTyp.t);

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
