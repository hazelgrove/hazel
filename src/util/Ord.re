type t =
  | Lt
  | Eq
  | Gt;

let flip =
  fun
  | Eq => Eq
  | Lt => Gt
  | Gt => Lt;
