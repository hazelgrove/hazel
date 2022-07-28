open Sexplib.Std;

[@deriving sexp]
type variant = {
  ctor: Ident.t,
  params: list(Ident.t),
};

[@deriving sexp]
type t = {
  name: Ident.t,
  type_vars: list(Ident.t),
  variants: list(variant),
};
