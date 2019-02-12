type nat = int;

/* Section ListUtil */

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

let rec zip_eq = (xs, ys) =>
  switch (xs, ys) {
  | ([], []) => Some([])
  | ([x, ...xs], [y, ...ys]) =>
    switch (zip_eq(xs, ys)) {
    | None => None
    | Some(tail) => Some([(x, y), ...tail])
    }
  | ([_, ..._], []) => None
  | ([], [_, ..._]) => None
  };

let rec unzip = xs =>
  switch (xs) {
  | [] => ([], [])
  | [(x, y), ...xys] =>
    let (xs, ys) = unzip(xys);
    ([x, ...xs], [y, ...ys]);
  };

let cons_opt = (n: 'a, x: option(list('a))): option(list('a)) =>
  switch (x) {
  | None => None
  | Some(xs) => Some([n, ...xs])
  };

let cons_opt2 =
    (
      n1: 'a,
      x1: option(list('a)),
      n2: 'a,
      x2: unit => option(list('a)),
    )
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

let rec string_of_list' = (string_of_elt) => fun
| [] => ""
| [x] => string_of_elt(x)
| [x, ...xs] => string_of_elt(x) ++ ", " ++ string_of_list'(string_of_elt, xs);

let string_of_list = (string_of_elt, xs) => 
  "[" ++ string_of_list'(string_of_elt, xs) ++ "]";

let string_of_pair = (string_of_left, string_of_right, (left, right)) => 
  "(" ++ string_of_left(left) ++ ", " ++ string_of_right(right) ++ ")";

let string_of_opt = (string_of_elt) => fun
| None => "None"
| Some(elt) => "Some(" ++ string_of_elt(elt) ++ ")";

/* End ListUtil */

module ZList = {
  type t('z, 'a) = (list('a), 'z, list('a));

  let singleton = (z: 'z): t('z, 'a) => ([], z, []);

  let rec split_at = (n: nat, xs: list('a)): option(t('a, 'a)) =>
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

  let rec replace_z = (zs: t('z, 'a), z: 'z): t('z, 'a) => {
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

  let prj = (zlist : t('z, 'a)) : (list('a), 'z, list('a)) => zlist;

  let prj_prefix = (zxs: t('z, 'a)): list('a) => {
    let (prefix, _, _) = zxs;
    prefix;
  };

  let prefix_length = (zxs: t('z, 'a)): nat =>
    List.length(prj_prefix(zxs));

  let prj_z = (zxs: t('z, 'a)): 'z => {
    let (_, z, _) = zxs;
    z;
  };

  let prj_suffix = (zxs: t('z, 'a)): list('a) => {
    let (_, _, suffix) = zxs;
    suffix;
  };

  let erase = (xs: t('z, 'a), erase_z: 'z => 'a) => {
    let (prefix, z, suffix) = xs;
    let a = erase_z(z);
    prefix @ [a, ...suffix];
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
 * i.e. there is a selected element of type Z and the rest is a nat map of type A */
module ZNatMap = {
  type t('a, 'z) = (NatMap.t('a), (nat, 'z));
  let make =
      (m: NatMap.t('a), (n, z) as nz: (nat, 'z)): option(t('a, 'z)) =>
    switch (NatMap.lookup(m, n)) {
    | Some(_) => None
    | None => Some((m, nz))
    };
  let erase = (zmap : t('a, 'z), erase : 'z => 'a) => { 
    let (map', (n, z)) = zmap;
    NatMap.insert_or_update(map', (n, erase(z)));
  };
  let prj_z_kv = (zmap : t('a, 'z)) : (nat, 'z) => {
    let (_, nz) = zmap;
    nz;
  };
  let prj_z_v = (zmap : t('a, 'z)) : 'z => {
    let (_, (_, z)) = zmap;
    z;
  };
};
