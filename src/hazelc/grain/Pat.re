open Sexplib.Std;

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

let var = x => PVar(x);
let ctor = (name, args) => PCtor(name, args);
