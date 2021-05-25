open Sexplib.Std;

[@deriving sexp]
type t('a) = list((UHTag.t, 'a));

let empty: t('a) = [];

let bindings = (map: t('a)): list((UHTag.t, 'a)) => map;

let of_list = (bindings: list((UHTag.t, 'a))): t('a) => bindings;

/* compares tags only */
let compare_bindings =
    ((tag1, _): (UHTag.t, 'a), (tag2, _): (UHTag.t, 'a)): int =>
  UHTag.compare(tag1, tag2);

let equal_tags = (map1: t('a), map2: t('a)): bool => {
  let tags1 = map1 |> List.map(fst);
  let tags2 = map2 |> List.map(fst);
  tags1 === tags2
  || List.fast_sort(UHTag.compare, tags1)
  == List.fast_sort(UHTag.compare, tags2);
};

let equal_bindings =
    (
      val_equal: ('a, 'a) => bool,
      (tag1: UHTag.t, val1: 'a),
      (tag2: UHTag.t, val2: 'a),
    )
    : bool =>
  UHTag.eq(tag1, tag2) && val_equal(val1, val2);

let equal = (val_equal: ('a, 'a) => bool, map1: t('a), map2: t('a)): bool => {
  map1 === map2
  || {
    let map1 = List.fast_sort(compare_bindings, map1);
    let map2 = List.fast_sort(compare_bindings, map2);
    ListUtil.equal(equal_bindings(val_equal), map1, map2);
  };
};

/* sorts on tags only */
let sort = (map: t('a)): t('a) => {
  List.fast_sort(compare_bindings, map);
};

// module Sexp = Sexplib.Sexp;

// [@deriving sexp]
// type binding('v) = (UHTag.t, 'v);

// let sexp_of_t = (sexp_of_v: 'v => Sexp.t, map: t('v)): Sexp.t =>
//   map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
// let t_of_sexp = (v_of_sexp: Sexp.t => 'v, sexp: Sexp.t): t('v) =>
//   sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;
