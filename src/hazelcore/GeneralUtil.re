open Sexplib.Std;

let string_of_sexp = sexp => Sexplib.Sexp.to_string(sexp);

let opt_to_bool =
  fun
  | None => false
  | Some(_) => true;

let num_digits = (n: int): int => String.length(string_of_int(n));

let fmin = (a: float, b: float) => a <= b ? a : b;
let fmax = (a: float, b: float) => a >= b ? a : b;

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

/* Section ListUtil */

let rec join = (sep: 'a, xs: list('a)): list('a) =>
  switch (xs) {
  | [] => []
  | [x] => [x]
  | [x, ...xs] => [x, sep, ...join(sep, xs)]
  };

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

let any = (xs, f) => opt_to_bool(List.find_opt(f, xs));

let contains = (y: 'a, xs: list('a)): bool => List.exists(x => x == y, xs);

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

let rec string_of_list' = string_of_elt =>
  fun
  | [] => ""
  | [x] => string_of_elt(x)
  | [x, ...xs] =>
    string_of_elt(x) ++ ", " ++ string_of_list'(string_of_elt, xs);

let string_of_list = (string_of_elt, xs) =>
  "[" ++ string_of_list'(string_of_elt, xs) ++ "]";

let string_of_pair = (string_of_left, string_of_right, (left, right)) =>
  "(" ++ string_of_left(left) ++ ", " ++ string_of_right(right) ++ ")";

let string_of_opt = string_of_elt =>
  fun
  | None => "None"
  | Some(elt) => "Some(" ++ string_of_elt(elt) ++ ")";

let combos2 = (xs: list('x), ys: list('y)): list(('x, 'y)) =>
  xs
  |> List.map(x =>
    ys |> List.map(y => (x, y))
  )
  |> List.flatten;

let combos3 = (xs: list('x), ys: list('y), zs: list('z)): list(('x, 'y, 'z)) =>
  combos2(xs, ys)
  |> List.map(((x, y)) =>
    zs |> List.map(z => (x, y, z))
  )
  |> List.flatten;

/* End ListUtil */

module ZList = {
  [@deriving sexp]
  type t('z, 'a) = (list('a), 'z, list('a));

  let singleton = (z: 'z): t('z, 'a) => ([], z, []);

  let rec split_at = (n: int, xs: list('a)): option(t('a, 'a)) =>
    switch (n, xs) {
    | (_, []) => None
    | (0, [x, ...xs]) =>
      let prefix = [];
      let suffix = xs;
      Some((prefix, x, suffix));
    | (_, [x, ...xs]) =>
      let n' = n - 1;
      switch (split_at(n', xs)) {
      | None => None
      | Some((prefix, z, suffix)) =>
        let prefix' = [x, ...prefix];
        Some((prefix', z, suffix));
      };
    };

  let replace_z = (zs: t('z, 'a), z: 'z): t('z, 'a) => {
    let (prefix, _, suffix) = zs;
    (prefix, z, suffix);
  };

  let optmap_z =
      (f: 'z1 => option('z2), zs: t('z1, 'a)): option(t('z2, 'a)) => {
    let (prefix, z, suffix) = zs;
    switch (f(z)) {
    | None => None
    | Some(z') => Some((prefix, z', suffix))
    };
  };

  let prj = (zlist: t('z, 'a)): (list('a), 'z, list('a)) => zlist;

  let prj_prefix = (zxs: t('z, 'a)): list('a) => {
    let (prefix, _, _) = zxs;
    prefix;
  };

  let prj_z = (zxs: t('z, 'a)): 'z => {
    let (_, z, _) = zxs;
    z;
  };

  let prj_suffix = (zxs: t('z, 'a)): list('a) => {
    let (_, _, suffix) = zxs;
    suffix;
  };

  let prefix_length = (zxs: t('z, 'a)): int =>
    List.length(prj_prefix(zxs));

  let suffix_length = (zxs: t('z, 'a)): int =>
    List.length(prj_suffix(zxs));

  let length = (zxs: t('z, 'a)): int =>
    prefix_length(zxs) + 1 + suffix_length(zxs);

  let erase = (xs: t('z, 'a), erase_z: 'z => 'a) => {
    let (prefix, z, suffix) = xs;
    let a = erase_z(z);
    prefix @ [a, ...suffix];
  };

  let shift_next = (zxs: t('a, 'a)) => {
    let (prefix, z, suffix) = zxs;
    switch (suffix) {
    | [] => zxs
    | [next, ...suffix] =>
      let prefix = prefix @ [z];
      (prefix, next, suffix);
    };
  };

  let shift_prev = (zxs: t('a, 'a)) => {
    let (prefix, z, suffix) = zxs;
    switch (List.rev(prefix)) {
    | [] => zxs
    | [prev, ...rev_prefix] =>
      let suffix = [z, ...suffix];
      (List.rev(rev_prefix), prev, suffix);
    };
  };
};

/* Section StringUtil */

let str_eqb = String.equal;

let char_le_b = (ch1, ch2) => Char.code(ch1) < Char.code(ch2);

let char_eq_b = (ch1, ch2) => Char.code(ch1) == Char.code(ch2);

let char_in_range_b = (ch, s, e) =>
  if (char_le_b(s, ch)) {
    char_le_b(ch, e);
  } else {
    false;
  };

module NatMap = {
  [@deriving sexp]
  type t('a) = list((int, 'a));

  let empty = [];

  let extend_unique = (delta, x) => [x, ...delta];

  let rec drop = (delta, n) =>
    switch (delta) {
    | [] => None
    | [(y, a), ...delta'] =>
      if (n === y) {
        Some((delta', a));
      } else {
        drop(delta', n);
      }
    };

  let union = List.append;

  let rec lookup = (delta, x) =>
    switch (delta) {
    | [] => None
    | [(y, a), ...delta'] =>
      if (x == y) {
        Some(a);
      } else {
        lookup(delta', x);
      }
    };

  let rec insert_or_update = (delta, x) => {
    let (u, a) = x;
    switch (delta) {
    | [] => [x, ...delta]
    | [(u', a'), ...delta'] =>
      if (u == u') {
        [(u', a), ...delta'];
      } else {
        [(u', a'), ...insert_or_update(delta', x)];
      }
    };
  };

  let rec insert_or_map = (delta, u, a0, f) =>
    switch (delta) {
    | [] =>
      let a0 = a0();
      (a0, [(u, a0), ...delta]);
    | [(u', a), ...delta'] =>
      if (u === u') {
        let a' = f(a);
        (a', [(u', a'), ...delta']);
      } else {
        let (a', delta'') = insert_or_map(delta', u, a0, f);
        (a', [(u', a), ...delta'']);
      }
    };

  let rec map = f =>
    fun
    | [] => []
    | [(u, a), ...delta'] => [(u, f(a)), ...map(f, delta')];

  let rec update_with = (f, u, delta, u_nil) =>
    switch (delta) {
    | [] => (u_nil, delta)
    | [(u', a), ...delta'] =>
      if (u == u') {
        let a' = f(a);
        (a', [(u', a'), ...delta']);
      } else {
        let (a', delta'') = update_with(f, u, delta', u_nil);
        (a', [(u', a), ...delta'']);
      }
    };

  let length = List.length;

  let to_list = delta => delta;

  let fold = (delta, f, b) => List.fold_left(f, b, delta);
};

/* Zippered finite map over nats, used with Z expressions
 * i.e. there is a selected element of type Z and the rest is a int map of type A */
module ZNatMap = {
  [@deriving sexp]
  type t('a, 'z) = (NatMap.t('a), (int, 'z));
  let make =
      (m: NatMap.t('a), (n, _) as nz: (int, 'z)): option(t('a, 'z)) =>
    switch (NatMap.lookup(m, n)) {
    | Some(_) => None
    | None => Some((m, nz))
    };
  let erase = (zmap: t('a, 'z), erase: 'z => 'a) => {
    let (map', (n, z)) = zmap;
    NatMap.insert_or_update(map', (n, erase(z)));
  };
  let prj_map = ((map, _): t('a, 'z)): NatMap.t('a) => map;
  let prj_z_kv = (zmap: t('a, 'z)): (int, 'z) => {
    let (_, nz) = zmap;
    nz;
  };
  let prj_z_v = (zmap: t('a, 'z)): 'z => {
    let (_, (_, z)) = zmap;
    z;
  };
};

/**
 * List containing at least two elements. Used
 * to collect and manipulate tuple elements.
 */
module ListMinTwo = {
  type t('a) =
    | Pair('a, 'a)
    | Cons('a, t('a));

  let rec to_list = (xs: t('a)): list('a) =>
    switch (xs) {
    | Pair(x1, x2) => [x1, x2]
    | Cons(x, xs) => [x, ...to_list(xs)]
    };

  exception LessThanTwoElements;

  let rec to_tuple_list = (xs: list('a)): t('a) =>
    switch (xs) {
    | []
    | [_] => raise(LessThanTwoElements)
    | [x1, x2] => Pair(x1, x2)
    | [x, ...xs] => Cons(x, to_tuple_list(xs))
    };

  let rec length = (xs: t('a)): int =>
    switch (xs) {
    | Pair(_, _) => 2
    | Cons(_, xs) => 1 + length(xs)
    };

  let rec append = (xs: t('a), ys: t('a)): t('a) =>
    switch (xs) {
    | Pair(x1, x2) => Cons(x1, Cons(x2, ys))
    | Cons(x, xs) => Cons(x, append(xs, ys))
    };

  let rec append_list = (xs: t('a), ys: list('a)): t('a) =>
    switch (xs, ys) {
    | (_, []) => xs
    | (Pair(x1, x2), [y]) => Cons(x1, Pair(x2, y))
    | (Pair(_, _), _) => append(xs, to_tuple_list(ys))
    | (Cons(x, xs), _) => Cons(x, append_list(xs, ys))
    };

  let mk = (x: 'a, y: 'a, zs: list('a)): t('a) =>
    append_list(Pair(x, y), zs);

  /**
   * Like List.fold_left, but the initial accumulator is a
   * function f0 on the first two elements in the tuple list.
   */
  let fold_left = (f: ('a, 'b) => 'a, f0: ('b, 'b) => 'a, xs: t('b)): 'a =>
    switch (xs) {
    | Pair(x1, x2) => f0(x1, x2)
    | Cons(x1, xs) =>
      let (x2, ys) =
        switch (xs) {
        | Pair(x2, x3) => (x2, [x3])
        | Cons(x2, xs) => (x2, to_list(xs))
        };
      List.fold_left(f, f0(x1, x2), ys);
    };

  /**
   * Like List.fold_right, but the initial accumulator is a
   * function f0 on the final two elements in the tuple list.
   */
  let rec fold_right = (f: ('a, 'b) => 'b, xs: t('a), f0: ('a, 'a) => 'b): 'b =>
    switch (xs) {
    | Pair(x1, x2) => f0(x1, x2)
    | Cons(x, xs) => f(x, fold_right(f, xs, f0))
    };

  let rec zip_eq = (xs: t('a), ys: t('b)): option(t(('a, 'b))) =>
    switch (xs, ys) {
    | (Pair(x1, x2), Pair(y1, y2)) => Some(Pair((x1, y1), (x2, y2)))
    | (Cons(x, xs), Cons(y, ys)) =>
      switch (zip_eq(xs, ys)) {
      | None => None
      | Some(xys) => Some(Cons((x, y), xys))
      }
    | _ => None
    };

  let rec unzip = (xys: t(('a, 'b))): (t('a), t('b)) =>
    switch (xys) {
    | Pair((x1, y1), (x2, y2)) => (Pair(x1, x2), Pair(y1, y2))
    | Cons((x, y), xys) =>
      let (xs, ys) = unzip(xys);
      (Cons(x, xs), Cons(y, ys));
    };
};

module Opt = {
  let map = (f: 'a => 'b, opt: option('a)): option('b) =>
    switch (opt) {
    | None => None
    | Some(a) => Some(f(a))
    };
  let map_default = (~default: 'b, f: 'a => 'b, opt: option('a)): 'b =>
    switch (opt) {
    | None => default
    | Some(a) => f(a)
    };
  let get = (if_absent: unit => 'a, opt: option('a)): 'a =>
    switch (opt) {
    | None => if_absent()
    | Some(a) => a
    };
  let test = (opt: option(_)): bool =>
    switch (opt) {
    | None => false
    | Some(_) => true
    };
};
