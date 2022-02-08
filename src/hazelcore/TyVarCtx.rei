/** A type variable context */

[@deriving sexp]
type t;

let empty: t;

let bound: (TyVar.Name.t, t) => bool;
let has_index: (Index.t, t) => bool;

let bind: (TyVar.Name.t, Kind.t, t) => t;

let index: (TyVar.Name.t, t) => option(Index.t);
let binding: (Index.t, t) => option((TyVar.Name.t, Kind.t));
let kind: (Index.t, t) => option(Kind.t);
