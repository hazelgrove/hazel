open Sexplib.Std;

module Opt = {
  let map = (f: 'a => 'b, opt: option('a)): option('b) =>
    switch (opt) {
    | None => None
    | Some(a) => Some(f(a))
    };
  let map2 =
      (f: ('a, 'b) => 'c, opt1: option('a), opt2: option('b)): option('c) =>
    switch (opt1, opt2) {
    | (None, _)
    | (_, None) => None
    | (Some(a), Some(b)) => Some(f(a, b))
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

let utf8_length = CamomileLibrary.UTF8.length;

let string_of_sexp = sexp => Sexplib.Sexp.to_string(sexp);

let pp_sexp = Sexplib.Sexp.output_hum(Stdlib.stdout);

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
    opt_zip(xs, ys) |> Opt.map(xys => [(x, y), ...xys])
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

let combos2 = (xs: list('x), ys: list('y)): list(('x, 'y)) =>
  xs |> List.map(x => ys |> List.map(y => (x, y))) |> List.flatten;

let combos3 =
    (xs: list('x), ys: list('y), zs: list('z)): list(('x, 'y, 'z)) =>
  combos2(xs, ys)
  |> List.map(((x, y)) => zs |> List.map(z => (x, y, z)))
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

  let join = ((prefix, z, suffix): t('a, 'a)): list('a) =>
    prefix @ [z, ...suffix];

  let replace_z = (z: 'z, (prefix, _, suffix): t('z, 'a)): t('z, 'a) => (
    prefix,
    z,
    suffix,
  );

  let map =
      (fz: 'y => 'z, f: 'a => 'b, (prefix, z, suffix): t('y, 'a))
      : t('z, 'b) => (
    prefix |> List.map(f),
    z |> fz,
    suffix |> List.map(f),
  );

  let mapi =
      (
        fz: (int, 'y) => 'z,
        f: (int, 'a) => 'b,
        (prefix, z, suffix): t('y, 'a),
      )
      : t('z, 'b) => {
    let prefix_len = prefix |> List.length;
    (
      prefix |> List.mapi(f),
      z |> fz(prefix_len),
      suffix |> List.mapi((i, a) => f(prefix_len + 1 + i, a)),
    );
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

  let shift_next = (zxs: t('a, 'a)): option(t('a, 'a)) => {
    let (prefix, z, suffix) = zxs;
    switch (suffix) {
    | [] => None
    | [next, ...suffix] =>
      let prefix = prefix @ [z];
      Some((prefix, next, suffix));
    };
  };

  let shift_prev = (zxs: t('a, 'a)): option(t('a, 'a)) => {
    let (prefix, z, suffix) = zxs;
    switch (List.rev(prefix)) {
    | [] => None
    | [prev, ...rev_prefix] =>
      let suffix = [z, ...suffix];
      Some((List.rev(rev_prefix), prev, suffix));
    };
  };
};

/* Section StringUtil */

let is_empty_string = String.equal("");

/**
 * A string of length n has caret positions 0 through n,
 * where 0 places the caret at the start and n places
 * the caret at the end. Split s at caret_index.
 */
let split_string = (caret_index: int, s: string): (string, string) => (
  String.sub(s, 0, caret_index),
  String.sub(s, caret_index, String.length(s) - caret_index),
);

let insert_string = (caret_index: int, insert_s: string, s: string): string => {
  let (l, r) = s |> split_string(caret_index);
  l ++ insert_s ++ r;
};

let backspace_string = (caret_index: int, s: string): string => {
  let l = String.sub(s, 0, caret_index - 1);
  let r = String.sub(s, caret_index, String.length(s) - caret_index);
  l ++ r;
};

let delete_string = (caret_index: int, s: string): string => {
  let l = String.sub(s, 0, caret_index);
  let r = String.sub(s, caret_index + 1, String.length(s) - caret_index - 1);
  l ++ r;
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
