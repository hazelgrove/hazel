module Syntax = KindSystem.Kind;

[@deriving sexp]
type t = Syntax.abs;

let singleton: HTyp.t => t;

let equal: (t, t) => bool;

let consistent_subkind: (Contexts.t, t, t) => bool;
let underlying_htyp: t => HTyp.t;
let subst_tyvars: (t, list((Index.Abs.t, HTyp.t))) => t;
