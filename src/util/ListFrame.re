open OptUtil.Syntax;

// heads of prefix and suffix neighbor the subject
// NOTE: DEPRECATED
type t('x) = (list('x), list('x));

let rec mk_opt = (n: int, xs: list('x)): option(t('x)) =>
  if (n < 0) {
    None;
  } else if (n == 0) {
    Some(([], xs));
  } else {
    switch (xs) {
    | [] => None
    | [x, ...xs] =>
      let+ (prefix, suffix) = mk_opt(n - 1, xs);
      (prefix @ [x], suffix);
    };
  };
let mk = (n, xs) =>
  mk_opt(n, xs)
  |> OptUtil.get(() => raise(Invalid_argument("ListFrame.mk")));

let rec find_mk =
        (p: 'x => option('y), xs: list('x)): option(('y, t('x))) =>
  switch (xs) {
  | [] => None
  | [x, ...xs] =>
    switch (p(x)) {
    | Some(y) => Some((y, ([], xs)))
    | None =>
      let+ (y, (prefix, suffix)) = find_mk(p, xs);
      (y, (prefix @ [x], suffix));
    }
  };

let orient =
    (d: Direction.t, (prefix, suffix): t('x)): (list('x), list('x)) =>
  d == Left ? (prefix, suffix) : (suffix, prefix);
let unorient = orient;

let rec split_nth = (n: int, xs: list('x)): ('x, t('x)) =>
  switch (n, xs) {
  | (_, []) => failwith("list index out of bounds")
  | (0, [x, ...xs]) => (x, ([], xs))
  | (_, [x, ...xs]) =>
    let (subj, (prefix, suffix)) = split_nth(n - 1, xs);
    (subj, (prefix @ [x], suffix));
  };

let split_sublist = ((i: int, j: int), xs: list('x)): (list('x), t('x)) => {
  let (prefix, suffix) = mk(j, xs);
  let (prefix, sublist) = mk(i, List.rev(prefix));
  (sublist, (prefix, suffix));
};

let to_list = (~subject: list('x)=[], (prefix, suffix): t('x)) =>
  List.concat([List.rev(prefix), subject, suffix]);

let append = ((prefix, suffix): t('x), (prefix', suffix'): t('x)) => (
  prefix @ prefix',
  suffix @ suffix',
);
