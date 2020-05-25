open Sexplib.Std;

[@deriving sexp]
type t_('a) = list((Var.t, 'a));

let empty = [];

let is_empty =
  fun
  | [] => true
  | [_, ..._] => false;

let rec drop = (ctx, x) =>
  switch (ctx) {
  | [] => ctx
  | [(y, elt), ...ctx'] =>
    if (Var.eq(x, y)) {
      ctx';
    } else {
      [(y, elt), ...drop(ctx', x)];
    }
  };

let extend = (ctx, xa) => {
  let (x, _) = xa;
  [xa, ...drop(ctx, x)];
};

let union = (ctx1, ctx2) => List.fold_left(extend, ctx2, ctx1);

let rec lookup = (ctx, x) =>
  switch (ctx) {
  | [] => None
  | [(y, elt), ...ctx'] =>
    if (Var.eq(x, y)) {
      Some(elt);
    } else {
      lookup(ctx', x);
    }
  };

let contains = (ctx, x) =>
  switch (lookup(ctx, x)) {
  | Some(_) => true
  | None => false
  };

let map = (f, xs) =>
  List.map(
    xa => {
      let (x, _) = xa;
      (x, f(xa));
    },
    xs,
  );

let rec length =
  fun
  | [] => 0
  | [_, ...ctx'] => 1 + length(ctx');

let to_list = ctx => ctx;
