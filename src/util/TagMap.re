open OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type binding('a) = (Tag.t, 'a);

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = list(binding('a));

let compare = compare;

let empty: t('a) = [];

let is_empty: t('a) => bool =
  fun
  | [] => true
  | _ => false;

let rec add = (tag: Tag.t, value: 'a, map: t('a)): t('a) =>
  switch (map) {
  | [] => [(tag, value)]
  | [(tag', value') as head, ...tail] =>
    if (Tag.equal(tag, tag')) {
      if (value === value') {
        map;
      } else {
        [(tag, value), ...tail];
      };
    } else {
      [head, ...add(tag, value, tail)];
    }
  };

let singleton = (tag: Tag.t, value: 'a): t('a) => [(tag, value)];

let compare_bindings =
    ((tag1, _): binding('a), (tag2, _): binding('a)): int =>
  compare(tag1, tag2);

/* compares tags only */
let equal = (val_equal: ('a, 'a) => bool, map1: t('a), map2: t('a)): bool => {
  let equal_bindings =
      (
        val_equal: ('a, 'a) => bool,
        (tag1, val1): binding('a),
        (tag2, val2): binding('a),
      )
      : bool =>
    Tag.equal(tag1, tag2) && val_equal(val1, val2);
  map1 === map2
  || {
    let map1 = List.fast_sort(compare_bindings, map1);
    let map2 = List.fast_sort(compare_bindings, map2);
    List.equal(equal_bindings(val_equal), map1, map2);
  };
};

let cardinal: t('a) => int = List.length;

let tags_of = (m: list((Tag.t, 'a))): list(Tag.t) => List.map(fst, m);

let same_tags_same_order = (map1: t('a), map2: t('a)): bool =>
  cardinal(map1) === cardinal(map2)
  && List.for_all2(Tag.equal, tags_of(map1), tags_of(map2));

let tags_equal = (map1: t('a), map2: t('a)): bool => {
  let tags1 = tags_of(map1);
  let tags2 = tags_of(map2);
  tags1 === tags2
  || List.fast_sort(compare, tags1) == List.fast_sort(compare, tags2);
};

let for_all: (binding('a) => bool, t('a)) => bool = List.for_all;

let bindings: t('a) => list(binding('a)) = x => x;

let find_opt = (tag: Tag.t, map: t('a)): option('a) => {
  let+ binding = List.find_opt(((k, _)) => Tag.equal(tag, k), map);
  snd(binding);
};

let map = (f: 'a => 'b, m: t('a)): t('b) => {
  let (tags, vals) = List.split(m);
  let vals = List.map(f, vals);
  List.combine(tags, vals);
};

/* sorts on tags only */
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
