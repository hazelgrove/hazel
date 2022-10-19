type t('a) =
  | Leaf
  | Node(t('a), 'a, t('a));

/* Assumes tree is a binary search tree */
let binsert: ('a, t('a)) => t('a);

let pre_order: t('a) => list('a);

let in_order: t('a) => list('a);

let post_order: t('a) => list('a);

let count_leaves: t('a) => int;

let count_nodes: t('a) => int;

let map: ('a => 'b, t('a)) => t('b);

let count_nodes_at_level: (int, t('a)) => int;
