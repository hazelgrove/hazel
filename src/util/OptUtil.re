let get_or_fail = (s, o) =>
  Option.value_or_thunk(o, ~default=() => failwith(s));
let get_or_raise = (e, o) =>
  Option.value_or_thunk(o, ~default=() => raise(e));

let unzip = (o: option(('a, 'b))): (option('a), option('b)) => (
  Option.map(o, ~f=fst),
  Option.map(o, ~f=snd),
);
let traverse = (f: 'a => option('b), l: list('a)): option(list('b)) =>
  Option.all(List.map(l, ~f));

let replace = (f: 'a => option('a), o: 'a): 'a =>
  Option.value(f(o), ~default=o);
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

module Syntax = {
  let ( let* ) = (o, f) => Option.bind(o, ~f);
  let (let+) = (o, f) => Option.map(~f, o);
  let (and+) = Option.both;
};
