open Sexplib.Std;

// invariant: List.length(as) == List.length(bs) + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t('a, 'b) = (list('a), list('b));

let mk = (as_: list('a), bs: list('b)): t('a, 'b) => {
  assert(List.length(as_) == List.length(bs) + 1);
  (as_, bs);
};

let split_first_a = ((as_, bs): t('a, 'b)): ('a, (list('b), list('a))) => {
  assert(as_ != []);
  (List.hd(as_), (bs, List.tl(as_)));
};
let first_a = (aba: t('a, _)): 'a => fst(split_first_a(aba));

let map_first = (f: 'a => 'a, aba: t('a, 'b)): t('a, 'b) => {
  let (a, (bs, as_)) = split_first_a(aba);
  ([f(a), ...as_], bs);
};

let split_last_a = _ => failwith("todo split_last_a");
let last_a = ((as_, _): t('a, _)): 'a => {
  assert(as_ != []);
  ListUtil.last(as_);
};

let rev = (rev_a, rev_b, (as_, bs): t('a, 'b)): t('a, 'b) => (
  List.rev_map(rev_a, as_),
  List.rev_map(rev_b, bs),
);

let cons = (a: 'a, b: 'b, (as_, bs): t('a, 'b)): t('a, 'b) => (
  [a, ...as_],
  [b, ...bs],
);
let uncons = _ => failwith("todo uncons");

let snoc = ((as_, bs): t('a, 'b), b: 'b, a: 'a): t('a, 'b) => (
  as_ @ [a],
  bs @ [b],
);
let unsnoc = _ => failwith("todo unsnoc");

let singleton = (a: 'a): t('a, _) => ([a], []);

let get_as: t('a, _) => list('a) = fst;
let get_bs: t(_, 'b) => list('b) = snd;

let hd = ((as_, _): t('a, 'b)): 'a => List.hd(as_);

let rec aba_triples = (aba: t('a, 'b)): list(('a, 'b, 'a)) =>
  switch (aba) {
  | ([a1, a2, ...as_], [b, ...bs]) => [
      (a1, b, a2),
      ...aba_triples(([a2, ...as_], bs)),
    ]
  | _ => []
  };

let map_a = (f_a: 'a => 'c, (as_, bs): t('a, 'b)): t('c, 'b) => (
  List.map(f_a, as_),
  bs,
);
let map_b = (f_b: 'b => 'c, (as_, bs): t('a, 'b)): t('a, 'c) => (
  as_,
  List.map(f_b, bs),
);
let map = (f_a, f_b, aba) => aba |> List.map(f_a) |> List.map(f_b);

let map_abas =
    (f_aba: (('a, 'b, 'a)) => 'c, (as_, _) as aba: t('a, 'b)): t('a, 'c) => (
  as_,
  List.map(f_aba, aba_triples(aba)),
);

let trim = ((as_, bs): t('a, 'b)): option(('a, t('b, 'a), 'a)) =>
  switch (bs) {
  | [] => None
  | [_, ..._] =>
    let (l, as_) = ListUtil.split_first(as_);
    let (as_, r) = ListUtil.split_last(as_);
    Some((l, mk(bs, as_), r));
  };

// todo: rename as of_list
let split = (f: 'c => Either.t('a, 'b), cs: list('c)): t(list('a), 'b) =>
  List.fold_right(
    (c, (as_, bs)) =>
      switch (f(c)) {
      | L(a) =>
        let (hd, tl) = ListUtil.split_first(as_);
        ([[a, ...hd], ...tl], bs);
      | R(b) => ([[], ...as_], [b, ...bs])
      },
    cs,
    mk([[]], []),
  );

// todo: rename as to_list
let join = (f_a: 'a => 'c, f_b: 'b => 'c, aba: t('a, 'b)): list('c) => {
  let (as_, a) = ListUtil.split_last(get_as(aba));
  let bs = get_bs(aba);
  List.fold_right2(
    (a, b, cs) => [f_a(a), f_b(b), ...cs],
    as_,
    bs,
    [f_a(a)],
  );
};

let fold_left =
    (f_a: 'a => 'acc, f_ba: ('acc, 'b, 'a) => 'acc, (as_, bs): t('a, 'b))
    : 'acc => {
  let (a, as_) = ListUtil.split_first(as_);
  List.fold_left2(f_ba, f_a(a), bs, as_);
};
let fold_left_map =
    (
      f_a: 'a => ('acc, 'c),
      f_ba: ('acc, 'b, 'a) => ('acc, 'd, 'c),
      aba: t('a, 'b),
    )
    : ('acc, t('c, 'd)) =>
  aba
  |> fold_left(
       a => {
         let (acc, c) = f_a(a);
         (acc, singleton(c));
       },
       ((acc, mapped), b, a) => {
         let (acc, d, c) = f_ba(acc, b, a);
         (acc, snoc(mapped, d, c));
       },
     );

let fold_right =
    (f_ab: ('a, 'b, 'c) => 'c, f_a: 'a => 'c, (as_, bs): t('a, 'b)) => {
  let (as_, a) = ListUtil.split_last(as_);
  List.fold_right2(f_ab, as_, bs, f_a(a));
};
