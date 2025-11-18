let get = (if_none, o) =>
  switch (o) {
  | None => if_none()
  | Some(a) => a
  };
let get_or_fail = s => get(() => failwith(s));
let get_or_raise = e => get(() => raise(e));

let map2 = (f, o1, o2) =>
  switch (o1, o2) {
  | (None, _)
  | (_, None) => None
  | (Some(v1), Some(v2)) => Some(f(v1, v2))
  };

let some_if = (cond, a) => cond ? Some(a) : None;

let zip = (o1, o2) =>
  switch (o1, o2) {
  | (None, _)
  | (_, None) => None
  | (Some(a), Some(b)) => Some((a, b))
  };
let unzip = (o: option(('a, 'b))): (option('a), option('b)) =>
  switch (o) {
  | None => (None, None)
  | Some((a, b)) => (Some(a), Some(b))
  };
let traverse = (f: 'a => option('b), l: list('a)): option(list('b)) =>
  List.fold_right(
    (x, acc) => map2((y, ys) => [y, ...ys], f(x), acc),
    l,
    Some([]),
  );

let sequence = (l: list(option('a))): option(list('a)) =>
  traverse(Fun.id, l);

let and_then = (f, o) => Option.bind(o, f);

let replace = (f: 'a => option('a), o: 'a): 'a =>
  switch (f(o)) {
  | Some(a) => a
  | None => o
  };
let fold_left_opt:
  type a acc. ((acc, a) => option(acc), list(a), acc) => option(acc) =
  (f, list, init) => {
    let rec aux = (acc, rest) =>
      switch (rest) {
      | [] => Some(acc)
      | [x, ...xs] =>
        switch (f(acc, x)) {
        | None => None
        | Some(nextAcc) => aux(nextAcc, xs)
        }
      };
    aux(init, list);
  };

let filter = (f: 'a => bool, o: option('a)): option('a) =>
  switch (o) {
  | None => None
  | Some(a) => f(a) ? Some(a) : None
  };

/**
 * Returns the first option if it is Some, otherwise returns the second option.
 * This provides a fallback mechanism for option types.
 *
 * @param o1 The primary option to check
 * @param o2 The fallback option if o1 is None
 * @return The first Some option, or o2 if o1 is None
 */
let or_else = (o1: option('a), o2: option('a)): option('a) =>
  switch (o1) {
  | Some(_) => o1
  | None => o2
  };

module Syntax = {
  let ( let* ) = Option.bind;
  let (let+) = (o, f) => Option.map(f, o);
  let (and+) = zip;
};
