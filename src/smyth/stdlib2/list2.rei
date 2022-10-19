let pure_bind: (list('a), 'a => 'b) => list('b);
let pure: 'a => list('a);
let bind: (list('a), 'a => list('b)) => list('b);

let concat_map: ('a => list('b), list('a)) => list('b);
let maximum: list('a) => option('a);
let repeat: (int, 'a) => list('a);
let sequence: list(list('a)) => list(list('a));
let filter_somes: list(option('a)) => list('a);
let intersperse: ('a, list('a)) => list('a);
/* Inclusive on both ends */
let range: (~low: int, ~high: int) => list(int);
let remove_first: ('a, list('a)) => list('a);
/* Should only use on comparable types */
let permutations: list('a) => list(list('a));
let map3: (('a, 'b, 'c) => 'd, list('a), list('b), list('c)) => list('d);
let hd_opt: list('a) => option('a);
let tl_opt: list('a) => option(list('a));
let uncons: list('a) => option(('a, list('a)));
let is_empty: list('a) => bool;
let transpose: list(list('a)) => list(list('a));
let collapse_equal: list('a) => option('a);
let index_left: list('a) => list((int, 'a));
let index_right: list('a) => list(('a, int));
let find_map: ('a => option('b), list('a)) => option('b);
let sum: list(int) => int;
let fsum: list(float) => float;
let average: list(float) => option(float);
let take: (int, list('a)) => list('a);
let drop: (int, list('a)) => list('a);
let cartesian_product: (list('a), list('b)) => list(('a, 'b));
let count: ('a => bool, list('a)) => int;
