[@deriving sexp]
type t('a) = list((int, 'a));

let empty: list('a);

let extend_unique: (list('a), 'a) => list('a);

let drop: (list(('a, 'b)), 'a) => option((list(('a, 'b)), 'b));

let union: (list('a), list('a)) => list('a);

let lookup: (list(('a, 'b)), 'a) => option('b);

let insert_or_update: (list(('a, 'b)), ('a, 'b)) => list(('a, 'b));

let insert_or_map:
  (list(('a, 'b)), 'a, unit => 'b, 'b => 'b) => ('b, list(('a, 'b)));

let map: ('a => 'b, list(('c, 'a))) => list(('c, 'b));

let update_with:
  ('a => 'a, 'b, list(('b, 'a)), 'a) => ('a, list(('b, 'a)));

let length: list('a) => int;

let to_list: 'a => 'a;

let fold: (list('a), ('b, 'a) => 'b, 'b) => 'b;
