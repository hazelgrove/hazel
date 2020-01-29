open Sexplib.Std;

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
