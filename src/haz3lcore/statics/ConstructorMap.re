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

let match_synswitch =
    (match_synswitch: ('a, 'a) => 'a, m1: t('a), m2: t('a)): t('a) => {
  List.map(
    fun
    | (Variant(ctr, ids, Some(value1)), Variant(_, _, Some(value2))) =>
      Variant(ctr, ids, Some(match_synswitch(value1, value2)))
    | (v, _) => v,
    List.combine(m1, m2),
  );
};

let equal = (eq: ('a, 'a) => bool, m1: t('a), m2: t('a)) => {
  switch (venn_regions(same_constructor(eq), m1, m2)) {
  | (inter, [], []) =>
    List.for_all(
      ((x, y)) =>
        switch (x, y) {
        | (Variant(_, _, Some(value1)), Variant(_, _, Some(value2))) =>
          eq(value1, value2)
        | (Variant(_, _, None), Variant(_, _, None)) => true
        | (BadEntry(x), BadEntry(y)) => eq(x, y)
        | _ => false
        },
      inter,
    )
  | _ => false
  };
};

let map = (f: option('a) => option('b), m: t('a)): t('b) => {
  List.map(
    fun
    | Variant(ctr, args, value) => Variant(ctr, args, f(value))
    | BadEntry(value) => BadEntry(value),
    m,
  );
};

let get_entry = (ctr, m) =>
  List.find_map(
    fun
    | Variant(ctr', _, value) when Constructor.equal(ctr, ctr') => value
    | Variant(_)
    | BadEntry(_) => None,
    m,
  );

let has_constructor_no_args = ctr =>
  List.exists(
    fun
    | Variant(ctr', _, None) when Constructor.equal(ctr, ctr') => true
    | Variant(_) => false
    | BadEntry(_) => false,
  );
