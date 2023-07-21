open OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type binding('a) = (Constructor.t, 'a);

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = list(binding('a));

let compare = compare;

let empty: t('a) = [];

let is_empty: t('a) => bool =
  fun
  | [] => true
  | _ => false;

let rec add = (ctr: Constructor.t, value: 'a, map: t('a)): t('a) =>
  switch (map) {
  | [] => [(ctr, value)]
  | [(ctr', value') as head, ...tail] =>
    if (Constructor.equal(ctr, ctr')) {
      if (value === value') {
        map;
      } else {
        [(ctr, value), ...tail];
      };
    } else {
      [head, ...add(ctr, value, tail)];
    }
  };

let singleton = (ctr: Constructor.t, value: 'a): t('a) => [(ctr, value)];

let compare_bindings =
    ((ctr1, _): binding('a), (ctr2, _): binding('a)): int =>
  compare(ctr1, ctr2);

/* compares ctrs only */
let equal = (val_equal: ('a, 'a) => bool, map1: t('a), map2: t('a)): bool => {
  let equal_bindings =
      (
        val_equal: ('a, 'a) => bool,
        (ctr1, val1): binding('a),
        (ctr2, val2): binding('a),
      )
      : bool =>
    Constructor.equal(ctr1, ctr2) && val_equal(val1, val2);
  map1 === map2
  || {
    let map1 = List.fast_sort(compare_bindings, map1);
    let map2 = List.fast_sort(compare_bindings, map2);
    List.equal(equal_bindings(val_equal), map1, map2);
  };
};

let cardinal: t('a) => int = List.length;

let ctrs_of = (m: list((Constructor.t, 'a))): list(Constructor.t) =>
  List.map(fst, m);

let same_constructors_same_order = (map1: t('a), map2: t('a)): bool =>
  cardinal(map1) === cardinal(map2)
  && List.for_all2(Constructor.equal, ctrs_of(map1), ctrs_of(map2));

let ctrs_equal = (map1: t('a), map2: t('a)): bool => {
  let ctrs1 = ctrs_of(map1);
  let ctrs2 = ctrs_of(map2);
  ctrs1 === ctrs2
  || List.fast_sort(compare, ctrs1) == List.fast_sort(compare, ctrs2);
};

let for_all: (binding('a) => bool, t('a)) => bool = List.for_all;

let bindings: t('a) => list(binding('a)) = x => x;

let find_opt = (ctr: Constructor.t, map: t('a)): option('a) => {
  let+ binding = List.find_opt(((k, _)) => Constructor.equal(ctr, k), map);
  snd(binding);
};

let map = (f: 'a => 'b, m: t('a)): t('b) => {
  let (ctrs, vals) = List.split(m);
  let vals = List.map(f, vals);
  List.combine(ctrs, vals);
};

/* sorts on ctrs only */
let sort = (map: t('a)): t('a) => {
  List.fast_sort(compare_bindings, map);
};

let of_list: list(binding('a)) => t('a) = x => x;

let rec is_ground = (is_ground_value: 'a => bool, map: t('a)): bool =>
  switch (map) {
  | [] => true
  | [(_, head), ...tail] =>
    is_ground_value(head) && tail |> is_ground(is_ground_value)
  };
