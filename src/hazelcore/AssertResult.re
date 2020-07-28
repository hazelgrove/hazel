[@deriving sexp]
type t =
  | Pass
  | Fail
  | Indet
  | Comp /*only used for chec*/;
