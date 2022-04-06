open Sexplib.Std;

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

let map_z = (fz: 'z1 => 'z2, (prefix, z, suffix): t('z1, 'x)): t('z2, 'x) => (
  prefix,
  fz(z),
  suffix,
);

let map =
    (fz: 'y => 'z, f: 'a => 'b, (prefix, z, suffix): t('y, 'a)): t('z, 'b) => (
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

let optmap_z = (f: 'z1 => option('z2), zs: t('z1, 'a)): option(t('z2, 'a)) => {
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

let prefix_length = (zxs: t('z, 'a)): int => List.length(prj_prefix(zxs));

let suffix_length = (zxs: t('z, 'a)): int => List.length(prj_suffix(zxs));

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
let shift_end = (zxs: t('a, 'a)): t('a, 'a) => {
  let (prefix, z, suffix) = zxs;
  switch (List.rev(suffix)) {
  | [] => zxs
  | [last_elt, ...tail] =>
    let prefix = prefix @ [z] @ List.rev(tail);
    (prefix, last_elt, []);
  };
};

let shift_begin = (zxs: t('a, 'a)): t('a, 'a) => {
  let (prefix, z, suffix) = zxs;
  switch (prefix) {
  | [] => zxs
  | [head, ...tail] =>
    let suffix = tail @ [z] @ suffix;
    ([], head, suffix);
  };
};

let shift_to = (n: int, xs: t('a, 'a)): option(t('a, 'a)) => {
  let (prefix, z, suffix) = xs;
  let lst = prefix @ [z, ...suffix];
  split_at(n, lst);
};

let prepend = (n: list('a), xs: t('z, 'a)): t('z, 'a) => {
  let (prefix, z, suffix) = xs;
  (List.rev(n) @ prefix, z, suffix);
};
let append = (n: list('a), xs: t('z, 'a)): t('z, 'a) => {
  let (prefix, z, suffix) = xs;
  (prefix, z, suffix @ n);
};
