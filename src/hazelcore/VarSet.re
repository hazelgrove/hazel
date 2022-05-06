open Sexplib.Std;

[@deriving sexp]
type t_ = list(Var.t);

let empty = [];

let is_empty =
  fun
  | [] => true
  | [_, ..._] => false;

let extend = (ctx, x) => {
  [x, ...List.filter(x' => !Var.eq(x, x'), ctx)];
};

let union = (ctx1, ctx2) => List.fold_left(extend, ctx2, ctx1);

let contains = (ctx, x) => List.mem(x, ctx);

let filter = List.filter;

let length = List.length;

let to_list = ctx => ctx;
