type t('a) =
  | Leaf
  | Node(t('a), 'a, t('a));

let rec binsert: ('a, t('a)) => t('a) = (
  (y, tree) =>
    switch (tree) {
    | Leaf => Node(Leaf, y, Leaf)

    | Node(left, x, right) =>
      if (y === x) {
        tree;
      } else if (y < x) {
        Node(binsert(y, left), x, right);
      } else {
        Node(left, x, binsert(y, right));
      }
    }:
    ('a, t('a)) => t('a) /* y > x */
);

let rec pre_order: t('a) => list('a) = (
  tree =>
    switch (tree) {
    | Leaf => []

    | Node(left, x, right) => [x] @ pre_order(left) @ pre_order(right)
    }:
    t('a) => list('a)
);

let rec in_order: t('a) => list('a) = (
  tree =>
    switch (tree) {
    | Leaf => []

    | Node(left, x, right) => in_order(left) @ [x] @ in_order(right)
    }:
    t('a) => list('a)
);

let rec post_order: t('a) => list('a) = (
  tree =>
    switch (tree) {
    | Leaf => []

    | Node(left, x, right) => post_order(left) @ post_order(right) @ [x]
    }:
    t('a) => list('a)
);

let rec count_leaves: t('a) => int = (
  tree =>
    switch (tree) {
    | Leaf => 1

    | Node(left, _, right) => count_leaves(left) + count_leaves(right)
    }:
    t('a) => int
);

let rec count_nodes: t('a) => int = (
  tree =>
    switch (tree) {
    | Leaf => 0

    | Node(left, _, right) => 1 + count_nodes(left) + count_nodes(right)
    }:
    t('a) => int
);

let rec map: ('a => 'b, t('a)) => t('b) = (
  (f, tree) =>
    switch (tree) {
    | Leaf => Leaf

    | Node(left, x, right) => Node(map(f, left), f(x), map(f, right))
    }:
    ('a => 'b, t('a)) => t('b)
);

let rec count_nodes_at_level: (int, t('a)) => int = (
  (level, tree) =>
    if (level < 0) {
      0;
    } else {
      switch (tree) {
      | Leaf => 0

      | Node(left, _, right) =>
        if (level == 0) {
          1;
        } else {
          count_nodes_at_level(level - 1, left)
          + count_nodes_at_level(level - 1, right);
        }
      };
    }:
    (int, t('a)) => int
);
