[@deriving (show({with_path: false}), sexp, yojson)]
type p('a) =
  | Node('a, list(p('a)));
[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Value
  | Children(int, pos);
let pos_split_last: pos => (int, pos);
let farthest_cond: ('a => bool, p('a), pos) => pos;
let value: p('a) => 'a;
let nth_node: (p('a), pos) => p('a);
let nth: (p('a), pos) => 'a;
let empty: 'a => p('a);
let flatten: p('a) => list('a);
let combine: (p('a), p('b)) => p(('a, 'b));
let map: ('a => 'b, p('a)) => p('b);
let mapi: ((pos, 'a) => 'b, p('a)) => p('b);
let fold_deep: (('a, list('b)) => 'b, p('a)) => 'b;
let map_nth: ('a => 'a, p('a), pos) => p('a);
let put_nth_node: (p('a), p('a), pos) => p('a);
let put_nth: ('a, p('a), pos) => p('a);
let insert: ('a, int, p('a), pos) => p('a);
let remove: (int, p('a), pos) => (p('a), p('a));
