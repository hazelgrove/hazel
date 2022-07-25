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

let sequence = (l: list(option('a))): option(list('a)) =>
  List.fold_right(map2((x, xs) => [x, ...xs]), l, Some([]));

let and_then = (f, o) => Option.bind(o, f);

module Syntax = {
  let ( let* ) = Option.bind;
  let (let+) = (o, f) => Option.map(f, o);
  let (and+) = zip;
};
