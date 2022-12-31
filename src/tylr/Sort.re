type t =
  | Typ
  | Pat
  | Exp;

// TODO specify as list, use indices
let prec =
  fun
  | Typ => 2
  | Pat => 1
  | Exp => 0;

let compare = (s1, s2) => Int.compare(prec(s1), prec(s2));

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });
