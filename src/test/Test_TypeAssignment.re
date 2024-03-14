open QCheck.Gen;

module Tree: {
  [@deriving show({with_path: false})]
  type t =
    | Leaf(int)
    | Node(t, t);

  let leaf: int => t;
  let node: (t, t) => t;
} = {
  [@deriving show({with_path: false})]
  type t =
    | Leaf(int)
    | Node(t, t);

  let leaf = (x: int): t => Leaf(x);
  let node = (x: t, y: t): t => Node(x, y);
};

let tree_gen =
  QCheck.Gen.sized(
    fix((self, n) =>
      switch (n) {
      | 0 => map(Tree.leaf, nat)
      | n =>
        frequency([
          (1, map(Tree.leaf, nat)),
          (2, map2(Tree.node, self(n / 2), self(n / 2))),
        ])
      }
    ),
  );

let show = Tree.show;
//let arbitrary = QCheck.make(tree_gen, ~print=tree.show);
//let l = QCheck.Gen.generate(~n=5, tree_gen);
//List.iter(_tr => print_endline(tree.yabo(tr)), l);
