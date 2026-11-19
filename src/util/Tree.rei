/* A rose tree -- every node holds a value and any number of children -- and
   `pos`, a path from the root to one of its nodes: `Children(i, p)` steps into
   child `i` and continues with `p`, `Value` stops at the current node.

   Functions taking a `pos` raise when it leads past the tree, except
   `farthest_cond`, which stops instead. */

[@deriving (show({with_path: false}), sexp, yojson)]
type p('a) =
  | Node('a, list(p('a)));
[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Value
  | Children(int, pos);
/** The last step of a path and the path to its parent:
    `Children(2, Children(1, Value))` gives `(1, Children(2, Value))`.
    Raises on `Value`. */
let pos_split_last: pos => (int, pos);
/** The longest prefix of the path whose nodes all satisfy the predicate.
    Child indices past the end are clamped to the last child. */
let farthest_cond: ('a => bool, p('a), pos) => pos;
let value: p('a) => 'a;
let nth_node: (p('a), pos) => p('a);
let nth: (p('a), pos) => 'a;
/** A leaf. */
let empty: 'a => p('a);
/** Pre-order. */
let flatten: p('a) => list('a);
/** Zips two trees; raises if their shapes differ. */
let combine: (p('a), p('b)) => p(('a, 'b));
let map: ('a => 'b, p('a)) => p('b);
let mapi: ((pos, 'a) => 'b, p('a)) => p('b);
/** Bottom-up: each node's value with its children's results. */
let fold_deep: (('a, list('b)) => 'b, p('a)) => 'b;
let map_nth: ('a => 'a, p('a), pos) => p('a);
let put_nth_node: (p('a), p('a), pos) => p('a);
let put_nth: ('a, p('a), pos) => p('a);
/** `insert(v, i, t, pos)` adds a leaf `v` as child `i` of the node at `pos`. */
let insert: ('a, int, p('a), pos) => p('a);
/** `remove(i, t, pos)` detaches child `i` of the node at `pos`, returning it
    and the remaining tree. */
let remove: (int, p('a), pos) => (p('a), p('a));
