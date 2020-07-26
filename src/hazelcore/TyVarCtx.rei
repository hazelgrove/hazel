[@deriving sexp]
type t = list((TyId.t, Kind.t));

let empty: list('a);

let index_of: (t, TyId.t) => option(int);

let index_of_exn: (t, TyId.t) => int;

let contains: (t, TyId.t) => bool;

let tyvar_with_idx: (t, HTyp.idx) => (TyId.t, Kind.t);

let extend: (t, (TyId.t, Kind.t)) => t;

let print: t => int;
