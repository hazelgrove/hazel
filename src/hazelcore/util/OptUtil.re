let map2 =
    (f: ('a, 'b) => 'c, opt1: option('a), opt2: option('b)): option('c) =>
  switch (opt1, opt2) {
  | (None, _)
  | (_, None) => None
  | (Some(a), Some(b)) => Some(f(a, b))
  };

let get = (if_absent: unit => 'a, opt: option('a)): 'a =>
  switch (opt) {
  | None => if_absent()
  | Some(a) => a
  };

let and_then = (f: 'a => 'b, opt: option('a)): 'b =>
  switch (opt) {
  | None => None
  | Some(a) => f(a)
  };

let filter = (pred: 'a => bool, opt: option('a)): option('a) =>
  switch (opt) {
  | None => None
  | Some(a) => pred(a) ? Some(a) : None
  };

let sequence = (l: list(option('a))): option(list('a)) =>
  List.fold_right(map2((x, xs) => [x, ...xs]), l, Some([]));

module Let_syntax = {
  let map = (x, ~f) => Option.map(f, x);
  let bind = (x, ~f) => Option.bind(x, f);
};
