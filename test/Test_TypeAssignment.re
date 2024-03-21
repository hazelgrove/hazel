open QCheck.Gen;
open Haz3lcore;
open Haz3lcore.Term;

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

let parens_comb = (u: UExp.t): UExp.t => {ids: [Id.mk()], term: Parens(u)};

let int_comb = (x: int): UExp.t => {ids: [Id.mk()], term: Int(x)};
let int_plus_comb = (u1: UExp.t, u2: UExp.t): UExp.t => {
  ids: [Id.mk()],
  term: BinOp(Int(Plus), u1, u2),
};
let uexp_int_gen =
  QCheck.Gen.sized(
    fix((self, n) =>
      switch (n) {
      | 0 => map(int_comb, int)
      | n =>
        frequency([
          (1, map(int_comb, int)),
          (1, map(parens_comb, self(n / 2))),
          (1, map2(int_plus_comb, self(n / 2), self(n / 2))),
        ])
      }
    ),
  );

let bool_comb = (x: bool): UExp.t => {ids: [Id.mk()], term: Bool(x)};
let bool_lt_comb = (u1: UExp.t, u2: UExp.t): UExp.t => {ids: [Id.mk()], term: BinOp(Int(LessThan), u1, u2)};
let uexp_bool_gen =
	QCheck.Gen.sized(
		fix((self, n) =>
			switch(n) {
			| 0 => map(bool_comb, bool)
			| n => 
				frequency([
					(1, map(bool_comb, bool)),
					(1, map(parens_comb, self(n/2))),
					(1, map(bool_lt_comb, uexp_int_gen, uexp_int_gen)),
				])
			}
		)
	);
//let arbitrary = QCheck.make(tree_gen, ~print=tree.show);
//let l = QCheck.Gen.generate(~n=5, tree_gen);
//List.iter(_tr => print_endline(tree.yabo(tr)), l);
