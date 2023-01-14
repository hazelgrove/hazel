type t =
  | Exp
  | Pat
  | Typ;

let to_int =
  fun
  | Exp => 0
  | Pat => 1
  | Typ => 2;

let compare = (s1, s2) => Int.compare(to_int(s1), to_int(s2));

let root = Exp;
