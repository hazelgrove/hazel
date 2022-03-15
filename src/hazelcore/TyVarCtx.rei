/** A type variable context */

[@deriving sexp]
type t;

let empty: t;

let has_name: (t, string) => bool;
let has_index: (t, Index.t) => bool;

let bind: (t, string, KindCore.t) => t;

let binding: (t, Index.t) => option((string, KindCore.t));
let kind: (t, Index.t) => option(KindCore.t);
let name: (t, Index.t) => option(string);
let index: (t, string) => option(Index.t);
