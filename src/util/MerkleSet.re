open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

/* A set recorded as its unions, the equality witness for subexpression
 * data accumulated bottom-up (probe targets).
 *
 * Shape: a tree-of-unions. Construction is bottom-up; unions are recorded
 * as a Branch node rather than merged, so union is O(1). No global
 * hash-cons table: sharing happens incidentally when the caller reuses
 * the same value across multiple parents.
 *
 * Equality is set equality, whatever the union shape, order or repeats:
 * two analyses (whole-program statics, per-definition statics) build one
 * set differently. The hash is a Bloom signature (an element's bit, OR'd
 * up through unions), so it is shape-free too: unequal hashes are an O(1)
 * reject. Matching hashes compare by shape first (O(1) per shared
 * subtree), then by elements.
 *
 * Suitable when membership and iteration aren't on the hot path. */

[@deriving (show({with_path: false}), sexp, yojson)]
type node('a) =
  | Empty
  | Leaf('a)
  | Branch(t('a), t('a))
and t('a) = {
  hash: int,
  node: node('a),
};

let empty: t('a) = {
  hash: 0,
  node: Empty,
};

/* 32 bits: js_of_ocaml's ints are 32-bit */
let singleton = (x: 'a): t('a) => {
  hash: 1 lsl (Hashtbl.hash(x) land 31),
  node: Leaf(x),
};

/* Empty acts as identity, so that `union_all` is forgiving about empty
 * children */
let union = (a: t('a), b: t('a)): t('a) =>
  switch (a.node, b.node) {
  | (Empty, _) => b
  | (_, Empty) => a
  | _ => {
      hash: a.hash lor b.hash,
      node: Branch(a, b),
    }
  };

let rec same_shape = (a: t('a), b: t('a)): bool =>
  a === b
  || a.hash == b.hash
  && (
    switch (a.node, b.node) {
    | (Empty, Empty) => true
    | (Leaf(x), Leaf(y)) => x == y
    | (Branch(la, ra), Branch(lb, rb)) =>
      same_shape(la, lb) && same_shape(ra, rb)
    | (Empty | Leaf(_) | Branch(_), _) => false
    }
  );

let elements = (t: t('a)): list('a) => {
  let rec go = (acc, t) =>
    switch (t.node) {
    | Empty => acc
    | Leaf(x) => [x, ...acc]
    | Branch(a, b) => go(go(acc, b), a)
    };
  List.sort_uniq(compare, go([], t));
};

let equal = (a: t('a), b: t('a)): bool =>
  a.hash == b.hash && (same_shape(a, b) || elements(a) == elements(b));

let rec mem = (x: 'a, t: t('a)): bool =>
  switch (t.node) {
  | Empty => false
  | Leaf(y) => x == y
  | Branch(a, b) => mem(x, a) || mem(x, b)
  };
