let rev_if = (b: bool) => b ? List.rev : Fun.id;

let dedup_f = (f, xs) =>
  List.fold_left(
    (deduped, x) => List.exists(f(x), deduped) ? deduped : deduped @ [x],
    [],
    xs,
  );

let dedup = xs => dedup_f((==), xs);

/**
  Groups elements of a list by a specified key.

 {b Note: The groups are not guaranteed to preserve the order of elements from the original list. }

  @param key
  The key function used to determine the grouping key.

  @param xs
  The list of elements to be grouped.

  @return
  A list of tuples where each tuple contains the grouping key and a list of elements that belong to that group.
*/
let group_by = (key: 'x => 'k, xs: list('x)): list(('k, list('x))) =>
  List.fold_left(
    (grouped, x) => {
      let k = key(x);
      let k_group =
        switch (List.assoc_opt(k, grouped)) {
        | None => []
        | Some(xs) => xs
        };
      [(k, [x, ...k_group]), ...List.remove_assoc(k, grouped)];
    },
    [],
    xs,
  );

let rec range = (~lo: int=0, hi: int) =>
  if (lo > hi) {
    raise(Invalid_argument("ListUtil.range"));
  } else if (lo == hi) {
    [];
  } else {
    [lo, ...range(~lo=lo + 1, hi)];
  };

let rec split =
        (l: list('x), cond: 'x => bool): (list('x), option('x), list('x)) => {
  switch (l) {
  | [] => ([], None, [])
  | [x, ...xs] =>
    if (cond(x)) {
      ([], Some(x), xs);
    } else {
      let (pre, x', post) = split(xs, cond);
      ([x, ...pre], x', post);
    }
  };
};

let combine_opt = (xs, ys) =>
  switch (List.combine(xs, ys)) {
  | exception (Invalid_argument(_)) => None
  | xys => Some(xys)
  };

let is_empty =
  fun
  | [] => true
  | _ => false;

let flat_map = (f, l) => List.flatten(List.map(f, l));

let rec join = (sep: 'x, xs: list('x)): list('x) =>
  switch (xs) {
  | [] => []
  | [x] => [x]
  | [x, ...xs] => [x, sep, ...join(sep, xs)]
  };

let hd_opt =
  fun
  | [] => None
  | [hd, ..._] => Some(hd);

/**
 * `split_n_opt(n, xs)` splits the first `n` elements from `xs`
 * if `xs` has `n` or more elements
 */
let split_n_opt = (n: int, xs: list('x)): option((list('x), list('x))) => {
  let rec go = (n: int, xs: list('x)) =>
    if (n < 0) {
      None;
    } else if (n == 0) {
      Some(([], xs));
    } else {
      switch (xs) {
      | [] => None
      | [x, ...xs] =>
        go(n - 1, xs)
        |> Option.map(((prefix, suffix)) => ([x, ...prefix], suffix))
      };
    };
  go(n, xs);
};

// TODO unify with ListFrame
let split_n = (n: int, xs: list('x)): (list('x), list('x)) =>
  switch (split_n_opt(n, xs)) {
  | None =>
    raise(Invalid_argument("ListUtil.split_n: " ++ string_of_int(n)))
  | Some(r) => r
  };

/**
 * Returns sublist from index i (inclusive)
 * to index j (exclusive), coupled with the
 * surrounding prefix/suffix sublists.
 * Returns None if i > j.
 */
let split_sublist_opt =
    (i: int, j: int, xs: list('x))
    : option((list('x), list('x), list('x))) => {
  switch (split_n_opt(j, xs)) {
  | None => None
  | Some((left, right)) =>
    switch (split_n_opt(i, left)) {
    | None => None
    | Some((left, mid)) => Some((left, mid, right))
    }
  };
};
let split_sublist =
    (i: int, j: int, xs: list('x)): (list('x), list('x), list('x)) =>
  switch (split_sublist_opt(i, j, xs)) {
  | None =>
    raise(
      Invalid_argument(
        "ListUtil.split_sublist: "
        ++ string_of_int(i)
        ++ ", "
        ++ string_of_int(j),
      ),
    )
  | Some(r) => r
  };

let sublist = ((i, j), xs: list('x)): list('x) => {
  let (_, sublist, _) = split_sublist(i, j, xs);
  sublist;
};

let rec split_nth_opt = (n, xs) =>
  switch (n, xs) {
  | _ when n < 0 => None
  | (_, []) => None
  | (0, [x, ...suffix]) => Some(([], x, suffix))
  | (_, [x, ...xs]) =>
    split_nth_opt(n - 1, xs)
    |> Option.map(((prefix, subject, suffix)) =>
         ([x, ...prefix], subject, suffix)
       )
  };
let split_nth = (n, xs) =>
  switch (split_nth_opt(n, xs)) {
  | None =>
    raise(Invalid_argument("ListUtil.split_nth: " ++ string_of_int(n)))
  | Some(r) => r
  };

let rec put_nth = (n: int, x: 'x, xs: list('x)): list('x) =>
  switch (n, xs) {
  | (_, []) => failwith("out of bounds")
  | (0, [_, ...tl]) => [x, ...tl]
  | (_, [hd, ...tl]) =>
    let tl = put_nth(n - 1, x, tl);
    [hd, ...tl];
  };

let split_last_opt = (xs: list('x)): option((list('x), 'x)) => {
  let rec go = (acc, xs) =>
    switch (xs) {
    | [] => None
    | [x] => Some((List.rev(acc), x))
    | [x, ...xs] => go([x, ...acc], xs)
    };
  go([], xs);
};

let split_last = (xs: list('x)): (list('x), 'x) =>
  switch (split_last_opt(xs)) {
  | None => raise(Invalid_argument("ListUtil.split_last"))
  | Some(r) => r
  };

let leading = xs => fst(split_last(xs));

let rec last_opt = (xs: list('x)): option('x) =>
  switch (xs) {
  | [] => None
  | [x] => Some(x)
  | [_, ...xs] => last_opt(xs)
  };

let last = (xs: list('x)): 'x =>
  switch (last_opt(xs)) {
  | None => raise(Invalid_argument("ListUtil.last"))
  | Some(x) => x
  };

let split_first_opt = (xs: list('x)): option(('x, list('x))) =>
  switch (xs) {
  | [] => None
  | [first, ...trailing] => Some((first, trailing))
  };

let split_first = xs =>
  split_first_opt(xs)
  |> OptUtil.get_or_raise(Invalid_argument("ListUtil.split_first"));

let rec fold_left_map =
        (f: ('acc, 'x) => ('acc, 'y), start: 'acc, xs: list('x))
        : ('acc, list('y)) =>
  switch (xs) {
  | [] => (start, [])
  | [x, ...xs] =>
    let (new_acc, y) = f(start, x);
    let (final, ys) = fold_left_map(f, new_acc, xs);
    (final, [y, ...ys]);
  };

let rec neighbors = (xs: list('x)): list(('x, 'x)) =>
  switch (xs) {
  | []
  | [_] => []
  | [x1, x2, ...xs] => [(x1, x2), ...neighbors([x2, ...xs])]
  };

let map_alt: ('a => 'c, 'b => 'c, list('a), list('b)) => list('c) =
  (fx, fy, xs, ys) => {
    if (List.length(xs) != List.length(ys) + 1) {
      raise(Invalid_argument("ListUtil.map_alt"));
    };
    List.fold_left2(
      (acc, x, y) => acc @ [fy(y), fx(x)],
      [fx(List.hd(xs))],
      List.tl(xs),
      ys,
    );
  };

let interleave = (xs, ys) => map_alt(x => x, y => y, xs, ys);

let rotate = (xs: list('x)): list('x) =>
  switch (xs) {
  | [] => []
  | [hd, ...tl] => tl @ [hd]
  };

let count_pred = (f: 'a => bool, xs: list('a)): int =>
  List.fold_left((n, x) => f(x) ? n + 1 : n, 0, xs);

let map2_opt =
    (f: ('a, 'b) => 'c, xs: list('a), ys: list('b)): option(list('c)) =>
  switch (List.map2(f, xs, ys)) {
  | b => Some(b)
  | exception (Invalid_argument(_)) => None
  };

let rec zip_defaults =
        (xs: list('a), ys: list('b), default_x: 'a, default_y: 'b)
        : list(('a, 'b)) =>
  switch (xs, ys) {
  | ([], []) => []
  | ([x, ...xs], [y, ...ys]) => [
      (x, y),
      ...zip_defaults(xs, ys, default_x, default_y),
    ]
  | ([], [y, ...ys]) => [
      (default_x, y),
      ...zip_defaults(xs, ys, default_x, default_y),
    ]
  | ([x, ...xs], []) => [
      (x, default_y),
      ...zip_defaults(xs, ys, default_x, default_y),
    ]
  };

let rec update_nth = (n, xs, f) =>
  switch (n, xs) {
  | (_, []) => []
  | (0, [x, ...xs]) => [f(x), ...xs]
  | (n, [x, ...xs]) => [x, ...update_nth(n - 1, xs, f)]
  };

let findi_opt: ('x => bool, list('x)) => option((int, 'x)) =
  (f, xs) => {
    List.mapi((i, x) => (i, x), xs)
    |> List.find_map(((_, x) as pair) =>
         if (f(x)) {
           Some(pair);
         } else {
           None;
         }
       );
  };

let find_with_rest:
  type a b. (a => option(b), list(a)) => option((b, list(a))) =
  (f, xs) => {
    let rec go = (xs, acc) =>
      switch (xs) {
      | [] => None
      | [x, ...xs] =>
        switch (f(x)) {
        | None => go(xs, [x, ...acc])
        | Some(y) => Some((y, List.rev_append(acc, xs)))
        }
      };
    go(xs, []);
  };

let assoc_err = (x, xs, err: string) =>
  switch (List.assoc_opt(x, xs)) {
  | None => failwith(err)
  | Some(y) => y
  };

/* Give a list of optional 'a, split the
 * list up using the Nones as dividers */
let split_at_nones = (xs: list(option('a))): list(list('a)) => {
  let rec go = (xs, acc) =>
    switch (xs) {
    | [] => acc
    | [None, ...xs] => go(xs, [[], ...acc])
    | [Some(x), ...xs] =>
      switch (acc) {
      | [acc, ...accs] => go(xs, [[x, ...acc], ...accs])
      | [] => go(xs, [[x]])
      }
    };
  go(xs, []) |> List.map(List.rev) |> List.rev;
};

/* Give a list of lists, return a list of pairs of
 * the first and last element of each list. */
let first_and_last = (xss: list(list('a))): list(('a, 'a)) =>
  xss
  |> List.filter_map(
       fun
       | [] => None
       | [x] => Some((x, x))
       | [x, ...xs] => Some((x, last(xs))),
     );

let rec rev_concat: (list('a), list('a)) => list('a) =
  (ls, rs) => {
    switch (ls) {
    | [] => rs
    | [hd, ...tl] => rev_concat(tl, [hd, ...rs])
    };
  };

let rec unzip = (lst: list(('a, 'b))): (list('a), list('b)) => {
  switch (lst) {
  | [] => ([], [])
  | [(a, b), ...tail] =>
    let (_as, bs) = unzip(tail);
    ([a, ..._as], [b, ...bs]);
  };
};

let cross = (xs, ys) =>
  List.concat(List.map(x => List.map(y => (x, y), ys), xs));

let rec flat_intersperse = (sep, xss) =>
  switch (xss) {
  | [] => []
  | [xs] => xs
  | [xs, ...xss] => xs @ [sep, ...flat_intersperse(sep, xss)]
  };

/* Given two lists, return their maximum common suffix */
let max_common_suffix = (a: list('a), b: list('a)): list('a) => {
  let rec loop = (a, b, acc) =>
    switch (a, b) {
    | ([], _)
    | (_, []) => acc
    | ([ha, ...ta], [hb, ...tb]) when ha == hb =>
      loop(ta, tb, [ha, ...acc])
    | _ => acc
    };
  loop(List.rev(a), List.rev(b), []);
};

let common_suffix_length = (s1, s2) =>
  List.length(max_common_suffix(s1, s2));

let is_suffix_of = (s1, s2) =>
  common_suffix_length(s1, s2) == List.length(s1);

/* Returns Some(depth) if xs is a suffix of ys at depth, None otherwise */

let suffix_at_depth = (xs: list('a), ys: list('a)): option(int) => {
  let rec go = (depth: int, xs, ys): option(int) =>
    if (xs == ys) {
      Some(depth);
    } else {
      switch (ys) {
      | [] => None
      | [_, ...rest] => go(depth + 1, xs, rest)
      };
    };
  go(0, xs, ys);
};

/* list truncated after at most n elementsnts */
let truncate = (n: int, xs: list('a)): list('a) => {
  let rec loop = (n, xs, acc) =>
    switch (n, xs) {
    | (0, _) => acc
    | (_, []) => acc
    | (n, [x, ...xs]) => loop(n - 1, xs, [x, ...acc])
    };
  loop(n, xs, []);
};

/* list without the first n elements, recurse into list until 0 then return rest */
let rec remove_first_n = (n: int, xs: list('a)): list('a) => {
  switch (n, xs) {
  | (0, _) => xs
  | (_, []) => []
  | (n, [_x, ...xs]) => remove_first_n(n - 1, xs)
  };
};

/* Return at most k elements starting from index i */
let slice = (i: int, k: int, xs: list('x)): list('x) =>
  xs |> remove_first_n(i) |> truncate(k);

let take_up_to_n = (n: int, xs: list('a)): list('a) =>
  switch (split_n_opt(n, xs)) {
  | Some((xs, _)) => xs
  | None => xs
  };

/* Move the first element equal to x to the front of the list */
let lift = (x: 'a, xs: list('a)): list('a) =>
  List.cons(x, List.filter((!=)(x), xs));

// for performance, doesn't check the whole list if already above length
let rec is_length = (n: int, xs: list('a)): bool =>
  switch (xs) {
  | [] when n == 0 => true
  | _ when n <= 0 => false
  | [] => false
  | [_, ...xs] => is_length(n - 1, xs)
  };

let rec remove_nth = (n: int, xs: list('a)): option(list('a)) =>
  switch (n, xs) {
  | (_, []) => None
  | (0, [_hd, ...tl]) => Some(tl)
  | (n, [hd, ...tl]) =>
    remove_nth(n - 1, tl) |> Option.map(tl' => [hd, ...tl'])
  };

let rec fold_left_opt =
        (f: ('a, 'b) => option('a), acc: 'a, xs: list('b)): option('a) => {
  switch (xs) {
  | [] => Some(acc)
  | [x, ...xs] =>
    switch (f(acc, x)) {
    | None => None
    | Some(acc') => fold_left_opt(f, acc', xs)
    }
  };
};
