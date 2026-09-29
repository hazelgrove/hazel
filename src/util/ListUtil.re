let dedup_f = (f, xs) =>
  List.fold_left(
    ~f=
      (deduped, x) =>
        List.exists(~f=f(x), deduped) ? deduped : deduped @ [x],
    ~init=[],
    xs,
  );

let dedup = xs => dedup_f(Poly.equal, xs);

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
    ~f=
      (grouped, x) => {
        let k = key(x);
        let k_group =
          switch (List.Assoc.find(~equal=Poly.equal, grouped, k)) {
          | None => []
          | Some(xs) => xs
          };
        [
          (k, [x, ...k_group]),
          ...List.Assoc.remove(~equal=Poly.equal, grouped, k),
        ];
      },
    ~init=[],
    xs,
  );

/**
  Groups consecutive elements that satisfy a predicate.

  Unlike [group_by], this only groups elements that are adjacent in the list.
  The predicate compares against the first element of the current group.

  @param should_group
  Predicate taking (representative, candidate) - returns true if candidate
  should be grouped with representative (first element of current group).

  @param xs
  The list of elements to be grouped.

  @return
  A list of groups, where each group is a list of consecutive elements
  that satisfied the predicate. Groups are in reverse order.
*/
let group_consecutive =
    (should_group: ('a, 'a) => bool, xs: list('a)): list(list('a)) =>
  List.fold_left(
    ~f=
      (acc: list(list('a)), item: 'a) =>
        switch (acc) {
        | [] => [[item]]
        | [[rep, ..._] as first, ...rest] when should_group(rep, item) => [
            first @ [item],
            ...rest,
          ]
        | _ => [[item], ...acc]
        },
    ~init=[],
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
  switch (List.zip(xs, ys)) {
  | Ok(xys) => Some(xys)
  | Unequal_lengths => None
  };

let join = (sep: 'x, xs: list('x)): list('x) => List.intersperse(xs, ~sep);

let hd_opt = xs => List.hd(xs);

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
        |> Option.map(~f=((prefix, suffix)) => ([x, ...prefix], suffix))
      };
    };
  go(n, xs);
};

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
    |> Option.map(~f=((prefix, subject, suffix)) =>
         ([x, ...prefix], subject, suffix)
       )
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

let last_opt = (xs: list('x)): option('x) => List.last(xs);

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
    List.fold2_exn(
      ~f=(acc, x, y) => acc @ [fy(y), fx(x)],
      ~init=[fx(List.hd_exn(xs))],
      List.tl_exn(xs),
      ys,
    );
  };

let interleave = (xs, ys) => map_alt(x => x, y => y, xs, ys);

let count_pred = (f: 'a => bool, xs: list('a)): int => List.count(xs, ~f);

let map2_opt =
    (f: ('a, 'b) => 'c, xs: list('a), ys: list('b)): option(list('c)) =>
  switch (List.map2(~f, xs, ys)) {
  | Ok(b) => Some(b)
  | Unequal_lengths => None
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

let opt_zip = combine_opt;

let rec update_nth = (n, xs, f) =>
  switch (n, xs) {
  | (_, []) => []
  | (0, [x, ...xs]) => [f(x), ...xs]
  | (n, [x, ...xs]) => [x, ...update_nth(n - 1, xs, f)]
  };

let findi_opt: ('x => bool, list('x)) => option((int, 'x)) =
  (f, xs) => List.findi(xs, ~f=(_, x) => f(x));

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
  switch (List.Assoc.find(~equal=Poly.equal, xs, x)) {
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
  go(xs, []) |> List.map(~f=List.rev) |> List.rev;
};

/* Give a list of lists, return a list of pairs of
 * the first and last element of each list. */
let first_and_last = (xss: list(list('a))): list(('a, 'a)) =>
  xss
  |> List.filter_map(
       ~f=
         fun
         | [] => None
         | [x] => Some((x, x))
         | [x, ...xs] => Some((x, last(xs))),
     );

let rev_concat: (list('a), list('a)) => list('a) =
  (ls, rs) => List.rev_append(ls, rs);

let unzip3 = (lst: list(('a, 'b, 'c))): (list('a), list('b), list('c)) =>
  List.unzip3(lst);

let intersperse = (sep, xs) => List.intersperse(xs, ~sep);

let flat_intersperse = (sep, xss) =>
  List.concat(List.intersperse(xss, ~sep=[sep]));

let is_suffix_of = (~eq: ('a, 'a) => bool=Poly.equal, s1, s2) =>
  List.is_suffix(s2, ~suffix=s1, ~equal=eq);

/* Returns Some(depth) if xs is a suffix of ys at depth, None otherwise */

let suffix_at_depth =
    (~eq: ('a, 'a) => bool=Poly.equal, xs: list('a), ys: list('a))
    : option(int) => {
  let rec go = (depth: int, xs, ys): option(int) =>
    if (List.equal(eq, xs, ys)) {
      Some(depth);
    } else {
      switch (ys) {
      | [] => None
      | [_, ...rest] => go(depth + 1, xs, rest)
      };
    };
  go(0, xs, ys);
};

/* list truncated after at most n elements */
let truncate = (n: int, xs: list('a)): list('a) => List.take(xs, n);

/* list without the first n elements */
let remove_first_n = (n: int, xs: list('a)): list('a) => List.drop(xs, n);

/* Return at most k elements starting from index i */
let slice = (i: int, k: int, xs: list('x)): list('x) =>
  xs |> remove_first_n(i) |> truncate(k);

let take = (n, xs: list('a)) => List.take(xs, n);

// for performance, doesn't check the whole list if already above length
let rec is_length = (n: int, xs: list('a)): bool =>
  switch (xs) {
  | [] when n == 0 => true
  | _ when n <= 0 => false
  | [] => false
  | [_, ...xs] => is_length(n - 1, xs)
  };

let rec insert = (x, xs, i) =>
  switch (xs, i) {
  | (_, 0) => [x, ...xs]
  | ([hd, ...tl], _) => [hd, ...insert(x, tl, i - 1)]
  | ([], _) => failwith("ListUtil.insert")
  };

let rec remove = (xs, i) =>
  switch (xs, i) {
  | ([_, ...tl], 0) => tl
  | ([hd, ...tl], _) => [hd, ...remove(tl, i - 1)]
  | ([], _) => failwith("ListUtil.remove")
  };

let rec remove_nth = (n: int, xs: list('a)): option(list('a)) =>
  switch (n, xs) {
  | (_, []) => None
  | (0, [_hd, ...tl]) => Some(tl)
  | (n, [hd, ...tl]) =>
    remove_nth(n - 1, tl) |> Option.map(~f=tl' => [hd, ...tl'])
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

let map_with_history = (f: (list('y), 'x) => 'y, xs: list('x)): list('y) => {
  let rec aux = (acc: list('y), remaining: list('x)) => {
    switch (remaining) {
    | [] => []
    | [x, ...xs] =>
      let y = f(acc, x);
      let acc' = acc @ [y];
      [y, ...aux(acc', xs)];
    };
  };
  aux([], xs);
};

/**
 * Similar to List.for_all2 but for functions that return option(bool)
 * Returns None if any call returns None
 * Returns Some(false) if any call returns Some(false)
 * Returns Some(true) if all calls return Some(true)
 */
let rec forall2_opt =
        (f: ('a, 'b) => option(bool), l1: list('a), l2: list('b))
        : option(bool) => {
  switch (l1, l2) {
  | ([], []) => Some(true)
  | ([x1, ...rest1], [x2, ...rest2]) =>
    switch (f(x1, x2)) {
    | None => None
    | Some(false) => Some(false)
    | Some(true) => forall2_opt(f, rest1, rest2)
    }
  | _ => Some(false) // Different lengths
  };
};

let assoc_opt_by = (eq, key, assoc) => {
  let rec find = lst =>
    switch (lst) {
    | [] => None
    | [(k, v), ...rest] => eq(key, k) ? Some(v) : find(rest)
    };
  find(assoc);
};

let assoc_update = (key, f, assoc) => {
  let rec go = lst =>
    switch (lst) {
    | [] =>
      switch (f(None)) {
      | Some(v) => [(key, v)]
      | None => []
      }
    | [(k, v), ...rest] =>
      if (Poly.equal(k, key)) {
        switch (f(Some(v))) {
        | Some(v') => [(k, v'), ...rest]
        | None => rest
        };
      } else {
        [(k, v), ...go(rest)];
      }
    };
  go(assoc);
};

let remove_assoc = (key, assoc) =>
  List.filter(~f=((k, _)) => !Poly.equal(k, key), assoc);

let max = (cmp: ('a, 'a) => Direction.t, xs: list('a)): option('a) => {
  switch (xs) {
  | [] => None
  | [x, ...xs] =>
    Some(
      List.fold_left(
        ~f=
          (current_max, candidate) =>
            switch (cmp(current_max, candidate)) {
            | Left => current_max
            | Right => candidate
            },
        ~init=x,
        xs,
      ),
    )
  };
};
