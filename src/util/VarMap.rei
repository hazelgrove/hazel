[@deriving (show({with_path: false}), sexp, yojson)]
type t_('a) = list((string, 'a));
let empty: list('a);
let extend: (list('a), 'a) => list('a);
let lookup: (list(('a, 'b)), 'a) => option('b);
let contains: (list(('a, 'b)), 'a) => bool;
let filter: ('a => bool, list('a)) => list('a);
let to_list: 'a => 'a;
let update: (t_('a), string, 'a => 'a) => t_('a);
