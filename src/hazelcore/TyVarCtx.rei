[@deriving sexp]
type t;

let to_list: t => list((TyId.t, Kind.t));

let of_list: list((TyId.t, Kind.t)) => t;

let empty: list('a);

let index_of: (t, TyId.t) => option(int);

let index_of_exn: (t, TyId.t) => int;

let contains: (t, TyId.t) => bool;

let tyvar_with_idx: (t, HTyp.Index.t) => (TyId.t, Kind.t);
