/* List helpers that Stdlib.List lacks. The `_opt` variants return `None`
   where the plain ones raise. */

let rev_if: (bool, list('a)) => list('a);
/** Keeps the first of each group of elements the predicate considers equal. */
let dedup_f: (('a, 'a) => bool, list('a)) => list('a);
let dedup: list('a) => list('a);
/** Groups by key. Neither the groups nor the elements within one keep
    their input order. */
let group_by: ('x => 'k, list('x)) => list(('k, list('x)));
/** Runs of adjacent elements that the predicate groups with their run's
    first element. The runs come back in reverse order. */
let group_consecutive: (('a, 'a) => bool, list('a)) => list(list('a));
/** `range(~lo, hi)` is `[lo, ..., hi - 1]`; `lo` defaults to 0. */
let range: (~lo: int=?, int) => list(int);
/** Splits around the first element satisfying the predicate. */
let split: (list('x), 'x => bool) => (list('x), option('x), list('x));
let combine_opt: (list('a), list('b)) => option(list(('a, 'b)));
/** Same as `intersperse`. */
let join: ('x, list('x)) => list('x);
let hd_opt: list('a) => option('a);
let split_n_opt: (int, list('x)) => option((list('x), list('x)));
let split_n: (int, list('x)) => (list('x), list('x));
/** `split_sublist_opt(i, j, xs)` is `(xs[0, i), xs[i, j), xs[j, ..))`. */
let split_sublist_opt:
  (int, int, list('x)) => option((list('x), list('x), list('x)));
let split_sublist: (int, int, list('x)) => (list('x), list('x), list('x));
let sublist: ((int, int), list('x)) => list('x);
let split_nth_opt: (int, list('a)) => option((list('a), 'a, list('a)));
let put_nth: (int, 'x, list('x)) => list('x);
let map_nth: (int, 'a => 'a, list('a)) => list('a);
let split_last_opt: list('x) => option((list('x), 'x));
let split_last: list('x) => (list('x), 'x);
let last_opt: list('x) => option('x);
let last: list('x) => 'x;
let split_first_opt: list('x) => option(('x, list('x)));
let split_first: list('a) => ('a, list('a));
/** Each adjacent pair. */
let neighbors: list('x) => list(('x, 'x));
/** Alternates `a b a ... a`; the first list must be one longer than the
    second. */
let map_alt: ('a => 'c, 'b => 'c, list('a), list('b)) => list('c);
let interleave: (list('a), list('a)) => list('a);
let count_pred: ('a => bool, list('a)) => int;
let map2_opt: (('a, 'b) => 'c, list('a), list('b)) => option(list('c));
/** Zips to the longer length, padding the shorter list with its default. */
let zip_defaults: (list('a), list('b), 'a, 'b) => list(('a, 'b));
/** Same as `combine_opt`. */
let opt_zip: (list('a), list('b)) => option(list(('a, 'b)));
/** Out of range is a no-op, unlike `map_nth`. */
let update_nth: (int, list('a), 'a => 'a) => list('a);
let findi_opt: ('x => bool, list('x)) => option((int, 'x));
/** The first element the function maps to `Some`, with every other
    element. */
let find_with_rest:
  ('a => option('b), list('a)) => option(('b, list('a)));
/** `List.assoc`, failing with the given message. */
let assoc_err: ('a, list(('a, 'b)), string) => 'b;
/** The runs of `Some`s between `None`s. */
let split_at_nones: list(option('a)) => list(list('a));
/** The first and last element of each non-empty list. */
let first_and_last: list(list('a)) => list(('a, 'a));
/** `List.rev_append`. */
let rev_concat: (list('a), list('a)) => list('a);
let unzip3: list(('a, 'b, 'c)) => (list('a), list('b), list('c));
let intersperse: ('a, list('a)) => list('a);
/** Concatenates the lists with the separator between each. */
let flat_intersperse: ('a, list(list('a))) => list('a);
let is_suffix_of: (~eq: ('a, 'a) => bool=?, list('a), list('a)) => bool;
/** `Some(n)` if the first list is the second with its first `n` elements
    dropped. */
let suffix_at_depth:
  (~eq: ('a, 'a) => bool=?, list('a), list('a)) => option(int);
let truncate: (int, list('a)) => list('a);
let remove_first_n: (int, list('a)) => list('a);
/** `slice(i, k, xs)`: at most `k` elements starting at index `i`. */
let slice: (int, int, list('x)) => list('x);
let take: (int, list('a)) => list('a);
/** Stops walking once the list is longer than `n`. */
let is_length: (int, list('a)) => bool;
/** `insert(x, xs, i)` puts `x` at index `i`. */
let insert: ('a, list('a), int) => list('a);
let remove: (list('a), int) => list('a);
let remove_nth: (int, list('a)) => option(list('a));
let fold_left_opt: (('a, 'b) => option('a), 'a, list('b)) => option('a);
/** The elements of the first list whose key appears in the second. */
let intersection_f: ('a => 'b, list('a), list('a)) => list('a);
/** Like `List.map`, but `f` also sees every result so far, oldest first. */
let map_with_history: ((list('y), 'x) => 'y, list('x)) => list('y);
/** Stops at the first call that isn't `Some(true)` and returns it;
    `Some(false)` if the lengths differ. */
let forall2_opt:
  (('a, 'b) => option(bool), list('a), list('b)) => option(bool);
let assoc_opt_by: (('a, 'b) => bool, 'a, list(('b, 'c))) => option('c);
/** The function receives the key's current value, if any; `None` removes
    the binding. */
let assoc_update:
  ('a, option('b) => option('b), list(('a, 'b))) => list(('a, 'b));
let remove_assoc: ('a, list(('a, 'b))) => list(('a, 'b));
/** The comparison names which argument is greater. */
let max: (('a, 'a) => Direction.t, list('a)) => option('a);
