let rev_if = (b: bool) => b ? List.rev : Fun.id;

let dedup_f = (f, xs) =>
  List.fold_right(
    (x, deduped) => List.exists(f(x), deduped) ? deduped : [x, ...deduped],
    xs,
    [],
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

// heads of prefix and suffix neighbor the subject
type frame('x) = (list('x), list('x));

let rec mk_frame = (n: int, xs: list('x)): frame('x) => {
  let invalid_arg = () => raise(Invalid_argument("ListUtil.mk_frame"));
  if (n < 0) {
    invalid_arg();
  } else if (n == 0) {
    ([], xs);
  } else {
    switch (xs) {
    | [] => invalid_arg()
    | [x, ...xs] =>
      let (prefix, suffix) = mk_frame(n - 1, xs);
      (prefix @ [x], suffix);
    };
  };
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

let rec split_frame = (n: int, xs: list('x)): ('x, frame('x)) =>
  switch (n, xs) {
  | (_, []) => failwith("list index out of bounds")
  | (0, [x, ...xs]) => (x, ([], xs))
  | (_, [x, ...xs]) =>
    let (subj, (prefix, suffix)) = split_frame(n - 1, xs);
    (subj, (prefix @ [x], suffix));
  };

let of_frame = (~subject: list('x)=[], (prefix, suffix): frame('x)) =>
  List.concat([List.rev(prefix), subject, suffix]);

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

let rec nth_opt = (n, xs) =>
  n < 0
    ? None
    : (
      switch (xs) {
      | [] => None
      | [hd, ...tl] => n == 0 ? Some(hd) : nth_opt(n - 1, tl)
      }
    );

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

let rec map_nth = (n: int, f: 'a => 'a, xs: list('a)): list('a) =>
  switch (n, xs) {
  | (_, []) => failwith("out of bounds")
  | (0, [hd, ...tl]) => [f(hd), ...tl]
  | (_, [hd, ...tl]) => [hd, ...map_nth(n - 1, f, tl)]
  };

let rec split_last_opt = (xs: list('x)): option((list('x), 'x)) =>
  switch (xs) {
  | [] => None
  | [x] => Some(([], x))
  | [x, ...xs] =>
    split_last_opt(xs)
    |> Option.map(((leading, last)) => ([x, ...leading], last))
  };
// let last_opt = xs => xs |> split_last_opt |> Option.map(snd);

let split_last = (xs: list('x)): (list('x), 'x) =>
  switch (split_last_opt(xs)) {
  | None => raise(Invalid_argument("ListUtil.split_last"))
  | Some(r) => r
  };
let leading = xs => fst(split_last(xs));
let last = xs => snd(split_last(xs));
let last_opt = xs => {
  let length = List.length(xs);
  if (length == 0) {
    None;
  } else {
    Some(List.nth(xs, length - 1));
  };
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

let rec take_while = (p: 'x => bool, xs: list('x)): (list('x), list('x)) =>
  switch (xs) {
  | [] => ([], [])
  | [hd, ...tl] =>
    if (p(hd)) {
      let (taken, rest) = take_while(p, tl);
      ([hd, ...taken], rest);
    } else {
      ([], xs);
    }
  };

let product = (xs, ys) =>
  xs |> List.map(x => ys |> List.map(y => (x, y))) |> List.flatten;

let rec ordered_pairs = (xs: list('x)): list(('x, 'x)) =>
  switch (xs) {
  | [] => []
  | [hd, ...tl] => List.map(x => (hd, x), tl) @ ordered_pairs(tl)
  };

let rec neighbors = (xs: list('x)): list(('x, 'x)) =>
  switch (xs) {
  | []
  | [_] => []
  | [x1, x2, ...xs] => [(x1, x2), ...neighbors([x2, ...xs])]
  };

module Syntax = {
  let (let+) = (xs, f) => List.map(f, xs);
  let (and+) = product;
  let ( let* ) = (xs, f) => List.concat_map(f, xs);
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

let p_indices = (p: 'a => bool, xs: list('a)): list(int) => {
  let (_, idxs) =
    List.fold_left(
      ((n, idxs), x) => (n + 1, idxs @ (p(x) ? [n] : [])),
      (0, []),
      xs,
    );
  idxs;
};

let splits = (xs: list('x) as 'xs): list(('xs, 'xs)) => {
  let rec go = (split: ('xs, 'xs)): list(('xs, 'xs)) =>
    switch (split) {
    | (_, []) => [split]
    | (l, [hd, ...tl]) => [split, ...go((l @ [hd], tl))]
    };
  go(([], xs));
};

let elem_splits = (xs: list('x) as 'xs): list(('xs, 'x, 'xs)) => {
  let rec go = (split: ('xs, 'x, 'xs)): list(('xs, 'x, 'xs)) =>
    switch (split) {
    | (_, _, []) => [split]
    | (l, x, [hd, ...tl]) => [split, ...go(([x, ...l], hd, tl))]
    };
  switch (xs) {
  | [] => []
  | [x, ...xs] => go(([], x, xs))
  };
};

let rotate = (xs: list('x)): list('x) =>
  switch (xs) {
  | [] => []
  | [hd, ...tl] => tl @ [hd]
  };

let single_elem = (xs: list('x)): option('x) =>
  switch (xs) {
  | [] => None
  | [hd, ...tl] => List.for_all((==)(hd), tl) ? Some(hd) : None
  };

let count_pred = (f: 'a => bool, xs: list('a)): int =>
  List.fold_left((n, x) => f(x) ? n + 1 : n, 0, xs);

let for_all2_opt =
    (f: ('a, 'b) => bool, xs: list('a), ys: list('b)): option(bool) =>
  switch (List.for_all2(f, xs, ys)) {
  | b => Some(b)
  | exception (Invalid_argument(_)) => None
  };

let map2_opt =
    (f: ('a, 'b) => 'c, xs: list('a), ys: list('b)): option(list('c)) =>
  switch (List.map2(f, xs, ys)) {
  | b => Some(b)
  | exception (Invalid_argument(_)) => None
  };

/* repeat an element n times */
let replicate = (n: int, e: 'a): list('a) => {
  /* add c additional copies of e to xs */
  let rec f = (c, xs) =>
    if (c > 0) {
      f(c - 1, [e, ...xs]);
    } else {
      xs;
    };
  f(n, []);
};

/**
 * Zips together two lists, returning None if different lengths
 */
let rec opt_zip = (xs: list('x), ys: list('y)): option(list(('x, 'y))) =>
  switch (xs, ys) {
  | ([], [_, ..._])
  | ([_, ..._], []) => None
  | ([], []) => Some([])
  | ([x, ...xs], [y, ...ys]) =>
    opt_zip(xs, ys) |> Option.map(xys => [(x, y), ...xys])
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

let rec disjoint_pairs = (xs: list('x)): list(('x, 'x)) =>
  switch (xs) {
  | []
  | [_] => []
  | [x1, x2, ...xs] => [(x1, x2), ...disjoint_pairs(xs)]
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

let init_fold: (int, 'b, (int, 'b) => ('b, 'a)) => ('b, list('a)) =
  (n, b, f) => {
    let range = List.init(n, n => n);
    let (acc, rev_xs) =
      List.fold_left(
        ((acc, xs), n) => {
          let (acc', elt) = f(n, acc);
          (acc', [elt, ...xs]);
        },
        (b, []),
        range,
      );
    (acc, List.rev(rev_xs));
  };

let assoc_err = (x, xs, err: string) =>
  switch (List.assoc_opt(x, xs)) {
  | None => failwith(err)
  | Some(y) => y
  };

let update_assoc = ((k, v)) =>
  List.map(((k', v')) => k == k' ? (k, v) : (k', v'));

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

let rec unsnoc = (xs: list('a)): option(('a, list('a))) => {
  switch ((xs: list('a))) {
  | [] => None
  | [head, ...tail] =>
    switch (unsnoc(tail)) {
    | None => Some((head, []))
    | Some((tail_last: 'a, tail_init: list('a))) =>
      Some((tail_last, [head, ...tail_init]))
    }
  };
};

let rec rev_concat: (list('a), list('a)) => list('a) =
  (ls, rs) => {
    switch (ls) {
    | [] => rs
    | [hd, ...tl] => rev_concat(tl, [hd, ...rs])
    };
  };

let rec map3 = (f, xs, ys, zs) =>
  switch (xs, ys, zs) {
  | ([], [], []) => []
  | ([x, ...xs], [y, ...ys], [z, ...zs]) => [
      f(x, y, z),
      ...map3(f, xs, ys, zs),
    ]
  | _ => failwith("Lists are of unequal length")
  };

let rec unzip = (lst: list(('a, 'b))): (list('a), list('b)) => {
  switch (lst) {
  | [] => ([], [])
  | [(a, b), ...tail] =>
    let (_as, bs) = unzip(tail);
    ([a, ..._as], [b, ...bs]);
  };
};
