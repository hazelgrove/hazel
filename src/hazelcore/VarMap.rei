[@deriving sexp]
type t_('a) = list((Var.t, 'a));

let empty: list('a);

let is_empty: list('a) => bool;

let drop: (list((String.t, 'a)), String.t) => list((String.t, 'a));

let extend:
  (list((String.t, 'a)), (String.t, 'a)) => list((String.t, 'a));

let union:
  (list((String.t, 'a)), list((String.t, 'a))) => list((String.t, 'a));

let lookup: (list((String.t, 'a)), String.t) => option('a);

let contains: (list((String.t, 'a)), String.t) => bool;

let map: ((('a, 'b)) => 'c, list(('a, 'b))) => list(('a, 'c));

let length: list('a) => int;

let to_list: 'a => 'a;
