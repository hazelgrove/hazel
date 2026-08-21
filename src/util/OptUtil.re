let get = (if_none, o) => Option.value_or_thunk(o, ~default=if_none);
let get_or_fail = s => get(() => failwith(s));
let get_or_raise = e => get(() => raise(e));

let map2 = (f, o1, o2) => Option.map2(o1, o2, ~f);

let some_if = (cond, a) => Option.some_if(cond, a);

let zip = (o1, o2) => Option.both(o1, o2);
let unzip = (o: option(('a, 'b))): (option('a), option('b)) =>
  switch (o) {
  | None => (None, None)
  | Some((a, b)) => (Some(a), Some(b))
  };
let sequence = (l: list(option('a))): option(list('a)) => Option.all(l);

let traverse = (f: 'a => option('b), l: list('a)): option(list('b)) =>
  sequence(List.map(l, ~f));

let and_then = (f, o) => Option.bind(o, ~f);

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
  Option.filter(o, ~f);

let value_exn = (~none, o) => get(() => raise(none), o);

module Syntax = {
  let ( let* ) = (o, f) => Option.bind(o, ~f);
  let (let+) = (o, f) => Option.map(~f, o);
  let (and+) = zip;
};
