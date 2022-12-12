let is_empty: list('a) => bool;

/**
 * List of ints starting from lo,
 * up to and excluding hi.
 */
let range: (~lo: int=?, int) => list(int);

let sublist: (~lo: int=?, int, list('a)) => list('a);

let join: ('a, list('a)) => list('a);

/**
 * Zips together two lists, returning None if different lengths
 */
let opt_zip: (list('x), list('y)) => option(list(('x, 'y)));

let for_all2_opt: (('a, 'b) => bool, list('a), list('b)) => option(bool);

let map2_opt: (('a, 'b) => 'c, list('a), list('b)) => option(list('c));

/**
 * Zips together the prefixes of two lists,
 * up to the length of the shorter list
 */
let zip: (list('a), list('b)) => list(('a, 'b));

let unzip: list(('a, 'b)) => (list('a), list('b)) /* repeat an element n times */;

let replicate: (int, 'a) => list('a);

let map_zip: ('x => 'y, list('x)) => list(('x, 'y));

let mapi_zip: ((int, 'x) => 'y, list('x)) => list(('x, 'y)) /* remove the first n elements from the given list */;

let drop: (int, list('a)) => list('a);

let update_nth: (int, list('a), 'a => 'a) => list('a);

let _findmapi: (int, list('a), (int, 'a) => option('b)) => option('b);

let findmapi: (list('a), (int, 'a) => option('b)) => option('b);

let filteri: ((int, 'a) => bool, list('a)) => list('a);

let any: (list('a), 'a => bool) => bool;

let first: list('a) => option('a);

let last: list('a) => option('a);

let split_first: list('a) => ('a, list('a));
let split_first_opt: list('a) => option(('a, list('a)));

let split_last: list('a) => (list('a), 'a);
let split_last_opt: list('a) => option((list('a), 'a));

let elem_before: ('a, list('a)) => option('a);

let elem_after: ('a, list('a)) => option('a);

let split_at: (list('a), 'a) => (list('a), list('a));

let partition_i: ((int, 'a) => bool, list('a)) => (list('a), list('a));

let fold_left_i: (('a, (int, 'b)) => 'a, 'a, list('b)) => 'a;

let fold_right_i: (((int, 'a), 'b) => 'b, list('a), 'b) => 'b;

let cons_opt: ('a, option(list('a))) => option(list('a));

let cons_opt2:
  ('a, option(list('a)), 'a, unit => option(list('a))) =>
  option(list('a));

let cons_opt3:
  (
    'a,
    option(list('a)),
    'a,
    unit => option(list('a)),
    'a,
    unit => option(list('a))
  ) =>
  option(list('a));

let combos2: (list('x), list('y)) => list(('x, 'y));

let combos3: (list('x), list('y), list('z)) => list(('x, 'y, 'z));

let take_while: ('x => bool, list('x)) => list('x);

// mapAccumL from Haskell
let map_with_accumulator:
  (('acc, 'x) => ('acc, 'y), 'acc, list('x)) => ('acc, list('y));

// mapAccumLM from Haskell
let map_with_accumulator_opt:
  (('acc, 'x) => option(('acc, 'y)), 'acc, list('x)) =>
  option(('acc, list('y)));

/**
 * `disjoint_pairs(xs)` returns a list of disjoint pairs
 * of consecutive elements in `xs`. If `xs` has an odd
 * number of elements, the last element is dropped.
 */
let disjoint_pairs: list('x) => list(('x, 'x));

let rotate: list('x) => list('x);
