open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

/* Merkle-hashed set, used as an equality witness for subexpression-level
 * data that's accumulated bottom-up.
 *
 * Shape: a tree-of-disjoint-unions. Construction is bottom-up; unions are
 * recorded as a Branch node rather than merged. This makes union O(1) and
 * preserves the construction order in the structure (and therefore the
 * hash). No global hash-cons table — sharing happens incidentally when
 * the caller reuses the same MerkleSet.t value across multiple parents.
 *
 * Equality: hash-based with a structural fallback. `a.hash != b.hash` is a
 * fast O(1) reject; matching hashes confirm via structure to rule out
 * collisions. False *negatives* — semantically equal sets built by
 * different union shapes compare unequal — are by design: the use case
 * (cache invalidation witnesses) accepts those as efficiency-only losses,
 * not soundness bugs. False *positives* are ruled out by the structural
 * fallback.
 *
 * Suitable when:
 *   - unions are disjoint (no merging required)
 *   - membership and iteration aren't on the hot path
 *   - cross-version equality requires construction-shape equality, which
 *     the caller arranges by mirroring traversal order. */

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

let singleton = (x: 'a): t('a) => {
  hash: Hashtbl.hash(`Leaf(x)),
  node: Leaf(x),
};

/* Empty acts as identity so that `union_all` is forgiving about empty
 * children — two traversals that differ only in how many empty subtrees
 * they pass through still produce equal sets. */
let union = (a: t('a), b: t('a)): t('a) =>
  switch (a.node, b.node) {
  | (Empty, _) => b
  | (_, Empty) => a
  | _ => {
      hash: Hashtbl.hash(`Branch((a.hash, b.hash))),
      node: Branch(a, b),
    }
  };

let rec equal = (a: t('a), b: t('a)): bool =>
  a === b
  || a.hash == b.hash
  && (
    switch (a.node, b.node) {
    | (Empty, Empty) => true
    | (Leaf(x), Leaf(y)) => x == y
    | (Branch(la, ra), Branch(lb, rb)) => equal(la, lb) && equal(ra, rb)
    | (Empty | Leaf(_) | Branch(_), _) => false
    }
  );
