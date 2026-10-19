open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;
// invariant: List.length(as) == List.length(bs) + 1
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a, 'b) = (list('a), list('b));

let mk = (as_: list('a), bs: list('b)): t('a, 'b) => {
  assert(List.length(as_) == List.length(bs) + 1);
  (as_, bs);
};

let first_a = ((as_, _): t('a, _)): 'a => {
  assert(List.length(as_) > 0);
  List.hd(as_);
};
let last_a = ((as_, _): t('a, _)): 'a => {
  assert(List.length(as_) > 0);
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

/* Stack-safe (built on fold_left over the reversed list): abas here can be
 * segment-sized (tens of thousands of pieces, e.g. splitting a large list
 * value in Segment.reassemble), and stdlib fold_right's per-element
 * recursion overflows the JS stack under js_of_ocaml. */
let split = (f: 'c => Either.t('a, 'b), cs: list('c)): t(list('a), 'b) =>
  List.fold_left(
    ((as_, bs), c) =>
      switch (f(c)) {
      | L(a) =>
        let (hd, tl) = ListUtil.split_first(as_);
        ([[a, ...hd], ...tl], bs);
      | R(b) => ([[], ...as_], [b, ...bs])
      },
    mk([[]], []),
    List.rev(cs),
  );

let join = (f_a: 'a => 'c, f_b: 'b => 'c, aba: t('a, 'b)): list('c) => {
  let (as_, a) = ListUtil.split_last(get_as(aba));
  let bs = get_bs(aba);
  /* rev_map2 (reversed pairs) + fold_left rather than fold_right2:
   * stack-safe (see split), same output order */
  List.fold_left(
    (cs, (a, b)) => [f_a(a), f_b(b), ...cs],
    [f_a(a)],
    List.rev_map2((a, b) => (a, b), as_, bs),
  );
};

let fold_left =
    (f_a: 'a => 'acc, f_ba: ('acc, 'b, 'a) => 'acc, (as_, bs): t('a, 'b))
    : 'acc => {
  let (a, as_) = ListUtil.split_first(as_);
  List.fold_left2(f_ba, f_a(a), bs, as_);
};
let fold_right =
    (f_ab: ('a, 'b, 'c) => 'c, f_a: 'a => 'c, (as_, bs): t('a, 'b)) => {
  let (as_, a) = ListUtil.split_last(as_);
  /* rev_map2 (reversed pairs) + fold_left rather than fold_right2:
   * stack-safe (see split), same evaluation result */
  List.fold_left(
    (acc, (a, b)) => f_ab(a, b, acc),
    f_a(a),
    List.rev_map2((a, b) => (a, b), as_, bs),
  );
};
