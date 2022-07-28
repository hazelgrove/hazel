[@deriving sexp]
type t =
  | PWild
  | PVar(Ident.t)
  | PInt(int)
  | PFloat(float)
  | PBool(bool)
  | PNil
  | PCons(t, t)
  | PTuple(list(t))
  | PTriv
  | PCtor(Ident.t, list(t));

let var: Ident.t => t;
let ctor: (Ident.t, list(t)) => t;
