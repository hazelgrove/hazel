/*******************************************************************************
 * Parameters
 */

let max_nat: int = (3: int);

let max_nat_list_length: int = (4: int);

let max_nested_nat_list_length: int = (4: int);

let max_nested_nat_inner_list_length: int = (2: int);

let max_bool_list_length: int = (4: int);

let max_nat_tree_size: int = (4: int);

let max_bool_tree_size: int = (6: int);

/*******************************************************************************
 * Enumeration Sampling
 */

/* Generic */

type gen('a) = unit => 'a;

let weight: (int, int) => float = (
  (_elementSize, _size) => 1.0: (int, int) => float
); /* or: elementSize ^ size */

let all:
  ('a, int => list('a), int, int) => ((float, 'a), list((float, 'a))) = (
  (base, shapes, element_size, max_size) => (
    (1.0, base),
    List2.concat_map(
      size =>
        size
        |> shapes
        |> List.map(shape => (weight(element_size, size), shape)),
      List2.range(~low=1, ~high=max_size),
    ),
  ):
    ('a, int => list('a), int, int) => ((float, 'a), list((float, 'a)))
);

let constant: 'a => gen('a) = ((x, ()) => x: 'a => gen('a));

let from: (('a, list('a))) => gen('a) = (
  ((x, xs), ()) => {
    let choice = Random.int(List.length(xs) + 1);

    if (Int.equal(choice, 0)) {
      x;
    } else {
      List.nth(xs, choice - 1);
    };
  }:
    (('a, list('a))) => gen('a)
);

let pair: (gen('a), gen('b)) => gen(('a, 'b)) = (
  (x, y, ()) => (x(), y()): (gen('a), gen('b)) => gen(('a, 'b))
);

let triple: (gen('a), gen('b), gen('c)) => gen(('a, 'b, 'c)) = (
  (x, y, z, ()) => (x(), y(), z()):
    (gen('a), gen('b), gen('c)) => gen(('a, 'b, 'c))
);

/* Semi-Generic */

type list_shape =
  | Nil
  | Cons(list_shape);

let list_base: list_shape = (Nil: list_shape);

let rec list_shapes: int => list(list_shape) = (
  n =>
    if (n == 0) {
      [list_base];
    } else {
      List.map(s => Cons(s), list_shapes(n - 1));
    }:
    int => list(list_shape)
);

let rec list_fill: (gen('a), list_shape) => list('a) = (
  (gen, shape) =>
    switch (shape) {
    | Nil => []

    | Cons(rest) => [gen(), ...list_fill(gen, rest)]
    }:
    (gen('a), list_shape) => list('a)
);

let list: (int, int, gen('a)) => gen(list('a)) = (
  (max_list_size, element_size, element_gen, ()) =>
    all(list_base, list_shapes, element_size, max_list_size)
    |> Pervasives2.uncurry(Random2.weighted)
    |> list_fill(element_gen):
    (int, int, gen('a)) => gen(list('a))
);

let nested_list: (int, int, int, gen('a)) => gen(list(list('a))) = (
  (max_list_length, max_inner_list_length, element_size, element_gen, ()) => {
    let inner_list_size =
      List2.range(~low=0, ~high=max_inner_list_length)
      |> List.map(len => Int2.pow(element_size, len))
      |> List2.sum;

    all(list_base, list_shapes, inner_list_size, max_list_length)
    |> Pervasives2.uncurry(Random2.weighted)
    |> list_fill(list(max_inner_list_length, element_size, element_gen));
  }:
    (int, int, int, gen('a)) => gen(list(list('a)))
);

type tree_shape =
  | Leaf
  | Node(tree_shape, tree_shape);

let tree_base: tree_shape = (Leaf: tree_shape);

let rec tree_shapes: int => list(tree_shape) = (
  n =>
    if (n == 0) {
      [tree_base];
    } else {
      List2.concat_map(
        k =>
          List.map(((left, right)) => Node(left, right)) @@
          List2.cartesian_product(tree_shapes(k), tree_shapes @@ n - 1 - k),
        List2.range(~low=0, ~high=n - 1),
      );
    }:
    int => list(tree_shape)
);

let rec tree_fill: (gen('a), tree_shape) => Tree2.t('a) = (
  (gen, t) =>
    switch (t) {
    | Leaf => Tree2.Leaf

    | Node(left, right) =>
      Tree2.Node(tree_fill(gen, left), gen(), tree_fill(gen, right))
    }:
    (gen('a), tree_shape) => Tree2.t('a)
);

let tree: (int, int, gen('a)) => gen(Tree2.t('a)) = (
  (max_tree_size, element_size, element_gen, ()) =>
    all(tree_base, tree_shapes, element_size, max_tree_size)
    |> Pervasives2.uncurry(Random2.weighted)
    |> tree_fill(element_gen):
    (int, int, gen('a)) => gen(Tree2.t('a))
);

/* Particular */

let nat: gen(int) = (() => Random.int(max_nat + 1): gen(int));

let bool: gen(bool) = (Random.bool: gen(bool));

let nat_list: gen(list(int)) = (
  list(max_nat_list_length, max_nat + 1, nat): gen(list(int))
);

let nested_nat_list: gen(list(list(int))) = (
  nested_list(
    max_nested_nat_list_length,
    max_nested_nat_inner_list_length,
    max_nat + 1,
    nat,
  ):
    gen(list(list(int)))
);

let bool_list: gen(list(bool)) = (
  list(max_bool_list_length, 2, bool): gen(list(bool))
);

let nat_tree: gen(Tree2.t(int)) = (
  tree(max_nat_tree_size, max_nat + 1, nat): gen(Tree2.t(int))
);

let bool_tree: gen(Tree2.t(bool)) = (
  tree(max_bool_tree_size, 2, bool): gen(Tree2.t(bool))
);

/*******************************************************************************
 * IO Sampling
 */

let io: ('a => 'b, gen('a)) => gen(('a, 'b)) = (
  (f, gen, ()) => {
    let x = gen();

    (x, f(x));
  }:
    ('a => 'b, gen('a)) => gen(('a, 'b))
);

let io_trial:
  (~n: int, ~k: int, 'a => 'b, gen('a), option(gen('a))) =>
  list(list(('a, 'b))) = (
  (~n, ~k, ref, input, base_case_opt) =>
    List.init(
      n,
      _ => {
        let amounts =
          (
            switch (base_case_opt) {
            | None => []

            | Some(base_case) => [(1, io(ref, base_case))]
            }
          )
          @ [(k, io(ref, input))];

        Random2.sample_unique(amounts);
      },
    ):
    (~n: int, ~k: int, 'a => 'b, gen('a), option(gen('a))) =>
    list(list(('a, 'b)))
);
