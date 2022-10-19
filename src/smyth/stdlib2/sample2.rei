type gen('a) = unit => 'a;

let constant: 'a => gen('a);

let from: (('a, list('a))) => gen('a);

let pair: (gen('a), gen('b)) => gen(('a, 'b));

let triple: (gen('a), gen('b), gen('c)) => gen(('a, 'b, 'c));

let nat: gen(int);

let bool: gen(bool);

let nat_list: gen(list(int));

let nested_nat_list: gen(list(list(int)));

let bool_list: gen(list(bool));

let nat_tree: gen(Tree2.t(int));

let bool_tree: gen(Tree2.t(bool));

let io_trial:
  (~n: int, ~k: int, 'a => 'b, gen('a), option(gen('a))) =>
  list(list(('a, 'b)));
