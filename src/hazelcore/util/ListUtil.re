/**
 * List of ints starting from lo,
 * up to and excluding hi.
 */
let rec range = (~lo=0, hi: int): list(int) =>
  if (lo >= hi) {
    [];
  } else {
    [lo, ...range(~lo=lo + 1, hi)];
  };

let sublist = (~lo=0, hi: int, xs: list('a)): list('a) =>
  if (lo < 0 || hi > List.length(xs)) {
    raise(Invalid_argument("GeneralUtil.sublist"));
  } else {
    range(~lo, hi) |> List.map(n => List.nth(xs, n));
  };

let rec join = (sep: 'a, xs: list('a)): list('a) =>
  switch (xs) {
  | [] => []
  | [x] => [x]
  | [x, ...xs] => [x, sep, ...join(sep, xs)]
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
    opt_zip(xs, ys) |> OptUtil.map(xys => [(x, y), ...xys])
  };

/**
 * Zips together the prefixes of two lists,
 * up to the length of the shorter list
 */
let rec zip = (xs: list('a), ys: list('b)): list(('a, 'b)) =>
  switch (xs, ys) {
  | ([], _) => []
  | (_, []) => []
  | ([x, ...xs], [y, ...ys]) => [(x, y), ...zip(xs, ys)]
  };

let rec unzip = (xys: list(('a, 'b))): (list('a), list('b)) =>
  switch (xys) {
  | [] => ([], [])
  | [(x, y), ...xys] =>
    let (xs, ys) = xys |> unzip;
    ([x, ...xs], [y, ...ys]);
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

let map_zip = (f: 'x => 'y, xs: list('x)): list(('x, 'y)) =>
  zip(xs, xs |> List.map(f));

let mapi_zip = (f: (int, 'x) => 'y, xs: list('x)): list(('x, 'y)) =>
  zip(xs, xs |> List.mapi(f));

/* remove the first n elements from the given list */
let rec drop = (n: int, xs: list('a)) =>
  if (n > 0) {
    switch (xs) {
    | [] => []
    | [_, ...tl] => drop(n - 1, tl)
    };
  } else {
    xs;
  };

let rec update_nth = (n, xs, f) =>
  switch (n, xs) {
  | (_, []) => []
  | (0, [x, ...xs]) => [f(x), ...xs]
  | (n, [x, ...xs]) => [x, ...update_nth(n - 1, xs, f)]
  };

let rec _findmapi = (i, xs, f) =>
  switch (xs) {
  | [] => None
  | [x, ...xs] =>
    switch (f(i, x)) {
    | Some(b) => Some(b)
    | None => _findmapi(i + 1, xs, f)
    }
  };

let findmapi = (xs, f) => _findmapi(0, xs, f);

let filteri = (pred, xs) =>
  xs
  |> List.mapi((i, x) => (i, x))
  |> List.filter(((i, x)) => pred(i, x))
  |> List.map(((_, x)) => x);

let any = (xs, f) => xs |> List.find_opt(f) |> OptUtil.test;

let first = (xs: list('a)): option('a) => List.nth_opt(xs, 0);

let last = (xs: list('a)): option('a) => first(List.rev(xs));

let split_last = (xs: list('a)): option((list('a), 'a)) =>
  switch (List.rev(xs)) {
  | [] => None
  | [y, ...ys] => Some((List.rev(ys), y))
  };

let rec elem_before = (x: 'a, xs: list('a)): option('a) =>
  switch (xs) {
  | []
  | [_] => None
  | [y1, y2, ...ys] => x == y2 ? Some(y1) : elem_before(x, [y2, ...ys])
  };

let rec elem_after = (x: 'a, xs: list('a)): option('a) =>
  switch (xs) {
  | []
  | [_] => None
  | [y1, y2, ...ys] => x == y1 ? Some(y2) : elem_after(x, [y2, ...ys])
  };

let rec split_at = (xs, n) =>
  switch (xs) {
  | [] => ([], [])
  | [y, ...ys] =>
    y == n
      ? ([], ys)
      : {
        let (before, after) = split_at(ys, n);
        ([y, ...before], after);
      }
  };

let partition_i =
    (f: (int, 'a) => bool, xs: list('a)): (list('a), list('a)) => {
  let (indexed_left, indexed_right) =
    xs
    |> List.mapi((i, x) => (i, x))
    |> List.partition(((i, x)) => f(i, x));
  let (_, left) = indexed_left |> List.split;
  let (_, right) = indexed_right |> List.split;
  (left, right);
};

let fold_left_i = (f: ('a, (int, 'b)) => 'a, acc: 'a, xs: list('b)): 'a => {
  let ixs = List.mapi((i, x) => (i, x), xs);
  List.fold_left(f, acc, ixs);
};

let fold_right_i = (f: ((int, 'a), 'b) => 'b, xs: list('a), acc: 'b): 'b => {
  let ixs = List.mapi((i, x) => (i, x), xs);
  List.fold_right(f, ixs, acc);
};

let cons_opt = (n: 'a, x: option(list('a))): option(list('a)) =>
  switch (x) {
  | None => None
  | Some(xs) => Some([n, ...xs])
  };

let cons_opt2 =
    (n1: 'a, x1: option(list('a)), n2: 'a, x2: unit => option(list('a)))
    : option(list('a)) =>
  switch (x1) {
  | Some(xs) => Some([n1, ...xs])
  | None =>
    switch (x2()) {
    | Some(xs) => Some([n2, ...xs])
    | None => None
    }
  };

let cons_opt3 =
    (
      n1: 'a,
      x1: option(list('a)),
      n2: 'a,
      x2: unit => option(list('a)),
      n3: 'a,
      x3: unit => option(list('a)),
    )
    : option(list('a)) =>
  switch (x1) {
  | Some(xs) => Some([n1, ...xs])
  | None =>
    switch (x2()) {
    | Some(xs) => Some([n2, ...xs])
    | None =>
      switch (x3()) {
      | Some(xs) => Some([n3, ...xs])
      | None => None
      }
    }
  };

let combos2 = (xs: list('x), ys: list('y)): list(('x, 'y)) =>
  xs |> List.map(x => ys |> List.map(y => (x, y))) |> List.flatten;

let combos3 =
    (xs: list('x), ys: list('y), zs: list('z)): list(('x, 'y, 'z)) =>
  combos2(xs, ys)
  |> List.map(((x, y)) => zs |> List.map(z => (x, y, z)))
  |> List.flatten;
