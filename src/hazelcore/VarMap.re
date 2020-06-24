open Sexplib.Std;

[@deriving sexp]
type t_('a) = list((Var.t, 'a));

let empty = [];

let is_empty =
  fun
  | [] => true
  | [_, ..._] => false;

let extend = (ctx, xa) => {
  let (x, _) = xa;
  [xa, ...List.remove_assoc(x, ctx)];
};

let union = (@);

let lookup = (ctx, x) => List.assoc_opt(x, ctx);

let contains = (ctx, x) => List.mem_assoc(x, ctx);

let map = (f, xs) => List.map(((x, _) as xa) => (x, f(xa)), xs);

let length = List.length;

let to_list = ctx => ctx;
