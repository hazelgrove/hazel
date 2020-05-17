// TODO prune redundant functions now that
// we're on 4.08 and can use stdlib Option

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

let sequence = (l: list(option('a))): option(list('a)) =>
  List.fold_right(map2((x, xs) => [x, ...xs]), l, Some([]));

module Let_syntax = {
  let map = (x, ~f) => Option.map(f, x);
  let bind = (x, ~f) => Option.bind(x, f);
};
