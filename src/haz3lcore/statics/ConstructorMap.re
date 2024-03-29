open Util.OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type variant('a) =
  | Variant(Constructor.t, list(Id.t), option('a))
  | BadEntry('a);

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = list(variant('a));

let equal_constructor =
    (eq: ('a, 'a) => bool, x: variant('a), y: variant('a)): bool =>
  switch (x, y) {
  | (Variant(ctr1, _, Some(x1)), Variant(ctr2, _, Some(y1))) =>
    Constructor.equal(ctr1, ctr2) && eq(x1, y1)
  | (Variant(ctr1, _, None), Variant(ctr2, _, None)) =>
    Constructor.equal(ctr1, ctr2)
  | (BadEntry(x), BadEntry(y)) => eq(x, y)
  | (Variant(_), Variant(_))
  | (BadEntry(_), Variant(_))
  | (Variant(_), BadEntry(_)) => false
  };

let same_constructor =
    (eq: ('a, 'a) => bool, x: variant('a), y: variant('a)): bool =>
  switch (x, y) {
  | (Variant(ctr1, _, _), Variant(ctr2, _, _)) =>
    Constructor.equal(ctr1, ctr2)
  | (BadEntry(x), BadEntry(y)) => eq(x, y)
  | (BadEntry(_), Variant(_))
  | (Variant(_), BadEntry(_)) => false
  };

let has_bad_entry = (x: t('a)): bool =>
  List.exists(
    fun
    | BadEntry(_) => true
    | Variant(_) => false,
    x,
  );

let has_good_entry = (x: t('a)): bool =>
  List.exists(
    fun
    | BadEntry(_) => false
    | Variant(_) => true,
    x,
  );

let free_variables = (f, m) =>
  m
  |> List.map(
       fun
       | Variant(_, _, Some(value)) => f(value)
       | _ => [],
     )
  |> List.flatten;

let is_ground = is_hole =>
  fun
  | [BadEntry(x)] when is_hole(x) => true
  | _ => false;

/* computes all three regions of a venn diagram of two sets represented as lists */
let venn_regions =
    (f: ('a, 'a) => bool, xs: list('a), ys: list('a))
    : (list(('a, 'a)), list('a), list('a)) => {
  let rec go = (xs, ys, acc, left, right) =>
    switch (xs) {
    | [] => (acc |> List.rev, left |> List.rev, List.rev_append(right, ys))
    | [x, ...xs] =>
      switch (List.partition(f(x, _), ys)) {
      | ([], _) => go(xs, ys, acc, [x, ...left], right)
      | ([y], ys') => go(xs, ys', [(x, y), ...acc], left, right)
      | _ => failwith("Sum type has non-unique constructors")
      }
    };
  go(xs, ys, [], [], []);
};

let join_entry =
    (join: ('a, 'a) => option('a), (x: variant('a), y: variant('a)))
    : option(variant('a)) =>
  switch (x, y) {
  | (Variant(ctr1, ids1, Some(value1)), Variant(ctr2, _, Some(value2)))
      when Constructor.equal(ctr1, ctr2) =>
    let+ value = join(value1, value2);
    Variant(ctr1, ids1, Some(value));
  | (Variant(ctr1, ids1, None), Variant(ctr2, _, None))
      when Constructor.equal(ctr1, ctr2) =>
    Some(Variant(ctr1, ids1, None))
  | (BadEntry(x), BadEntry(y)) =>
    let+ value = join(x, y);
    BadEntry(value);
  | _ => None
  };

let join =
    (
      eq: ('a, 'a) => bool,
      join: ('a, 'a) => option('a),
      m1: t('a),
      m2: t('a),
    )
    : option(t('a)) => {
  let (inter, left, right) = venn_regions(same_constructor(eq), m1, m2);
  let join_entries = List.filter_map(join_entry(join), inter);
  if (List.length(join_entries) == List.length(inter)) {
    switch (
      has_good_entry(left),
      has_bad_entry(m1),
      has_good_entry(right),
      has_bad_entry(m2),
    ) {
    | (_, true, _, true) => Some(join_entries @ left @ right)
    | (false, true, _, _) => Some(join_entries @ right)
    | (_, _, false, true) => Some(join_entries @ left)
    | _ when left == [] && right == [] => Some(join_entries)
    | _ => None
    };
  } else {
    None;
  };
};

let equal = (eq: ('a, 'a) => bool, m1: t('a), m2: t('a)) => {
  switch (venn_regions(same_constructor(eq), m1, m2)) {
  | (inter, [], []) =>
    List.for_all(
      ((x, y)) =>
        switch (x, y) {
        | (Variant(_, _, Some(value1)), Variant(_, _, Some(value2))) =>
          eq(value1, value2)
        | (BadEntry(x), BadEntry(y)) => eq(x, y)
        | _ => false
        },
      inter,
    )
  | _ => false
  };
};

// let get_valid_variants =
//   List.filter_map(
//     fun
//     | Variant(ctr, ids, value) => Some((ctr, ids, value))
//     | BadEntry(_) => None,
//     _,
//   );
// let compare_valid_variants = ((ctr1, _, _), (ctr2, _, _)) =>
//   String.compare(ctr1, ctr2);

// let join =
//     (f: ('a, 'a) => option('a), m1: t('a), m2: t('a)): option(t('a)) => {
//   let join_sum_entries = ((ctr1, ids1, ty1), (ctr2, _, ty2)) =>
//     switch (ty1, ty2) {
//     | (None, None) when ctr1 == ctr2 => Some((ctr1, ids1, None))
//     | (Some(ty1), Some(ty2)) when ctr1 == ctr2 =>
//       let+ ty_join = f(ty1, ty2);
//       (ctr1, ids1, Some(ty_join));
//     | _ => None
//     };
//   let map1 = m1 |> get_valid_variants;
//   let map2 = m2 |> get_valid_variants;
//   /* If same order, retain order for UI */
//   let same_constructors_same_order = {
//     List.length(map1) === List.length(map2)
//     && List.for_all2(
//          (x, y) => compare_valid_variants(x, y) == 0,
//          map1,
//          map2,
//        );
//   };
//   let map1 =
//     same_constructors_same_order
//       ? map1 |> List.fast_sort(compare_valid_variants) : map1;
//   let map2 =
//     same_constructors_same_order
//       ? map2 |> List.fast_sort(compare_valid_variants) : map2;
//   if (List.length(map1) == List.length(map2)) {
//     List.fold_left2(
//       (acc, entry1, entry2) =>
//         switch (acc) {
//         | Some(xs) =>
//           join_sum_entries(entry1, entry2)
//           |> Option.map(x => List.append(xs, [x]))
//         | None => None
//         },
//       Some([]),
//       map1,
//       map2,
//     )
//     |> Option.map(List.map(((a, b, c)) => Variant(a, b, c)));
//   } else {
//     None;
//   };
// };

// let compare = compare;

// let empty: t('a) = [];

// let is_empty: t('a) => bool =
//   fun
//   | [] => true
//   | _ => false;

// // let rec add = (ctr: Constructor.t, value: option('a), map: t('a)): t('a) =>
// //   switch (map) {
// //   | [] => [(ctr, value)]
// //   | [(ctr', value') as head, ...tail] =>
// //     if (Constructor.equal(ctr, ctr')) {
// //       if (value === value') {
// //         map;
// //       } else {
// //         [(ctr, value), ...tail];
// //       };
// //     } else {
// //       [head, ...add(ctr, value, tail)];
// //     }
// //   };

// // let singleton = (ctr: Constructor.t, value: option('a)): t('a) => [
// //   (ctr, value),
// // ];

// let compare_bindings = ((ctr1, _), (ctr2, _)): int => compare(ctr1, ctr2);

// let to_bindings =
//   List.filter_map(
//     fun
//     | Variant(ctr, _, value) => Some((ctr, value))
//     | BadEntry(_) => None,
//     _,
//   );

// /* compares ctrs only */
// let equal =
//     (
//       val_equal: (option('a), option('a)) => bool,
//       map1: t('a),
//       map2: t('a),
//     )
//     : bool => {
//   let equal_bindings = (val_equal, (ctr1, _, val1), (ctr2, _, val2)): bool =>
//     Constructor.equal(ctr1, ctr2) && val_equal(val1, val2);
//   map1 === map2
//   || {
//     let map1 =
//       List.fast_sort(compare_valid_variants, map1 |> get_valid_variants);
//     let map2 =
//       List.fast_sort(compare_valid_variants, map2 |> get_valid_variants);
//     List.equal(equal_bindings(val_equal), map1, map2);
//   };
// };

// let cardinal: t('a) => int = List.length;

// let ctrs_of = (m): list(Constructor.t) => m |> to_bindings |> List.map(fst);

// let same_constructors_same_order = (map1: t('a), map2: t('a)): bool =>
//   cardinal(map1) === cardinal(map2)
//   && List.for_all2(Constructor.equal, ctrs_of(map1), ctrs_of(map2));

// let ctrs_equal = (map1: t('a), map2: t('a)): bool => {
//   let ctrs1 = ctrs_of(map1);
//   let ctrs2 = ctrs_of(map2);
//   ctrs1 === ctrs2
//   || List.fast_sort(compare, ctrs1) == List.fast_sort(compare, ctrs2);
// };

// // let for_all: (binding('a) => bool, t('a)) => bool = List.for_all;

// // let bindings: t('a) => list(binding('a)) = x => x;

// // let find_opt = (ctr: Constructor.t, map: t('a)): option(option('a)) => {
// //   let+ binding =
// //     List.find_opt(
// //       ((k, _)) => Constructor.equal(ctr, k),
// //       map |> to_bindings,
// //     );
// //   snd(binding);
// // };

let map = (f: option('a) => option('b), m: t('a)): t('b) => {
  List.map(
    fun
    | Variant(ctr, args, value) => Variant(ctr, args, f(value))
    | BadEntry(value) => BadEntry(value),
    m,
  );
};

// // /* sorts on ctrs only */
// // let sort = (map: t('a)): t('a) => {
// //   List.fast_sort(compare_bindings, map);
// // };

// // let of_list: list(binding('a)) => t('a) = x => x;

// // let rec is_ground = (is_ground_value: 'a => bool, map: t('a)): bool =>
// //   switch (map) {
// //   | [] => true
// //   | [(_, head), ...tail] =>
// //     is_ground_value(head) && tail |> is_ground(is_ground_value)
// //   };
