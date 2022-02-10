/** A type variable context */

[@deriving sexp]
type t;

let empty: t;

let has_name: (t, string) => bool;
let has_index: (t, Index.t) => bool;

let bind: (t, string, Kind.t) => t;

let binding: (t, Index.t) => option((string, Kind.t));
let kind: (t, Index.t) => option(Kind.t);
let index: (t, string) => option(Index.t);
