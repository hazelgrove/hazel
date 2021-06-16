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

let union = (ctx1, ctx2) => List.fold_left(extend, ctx2, ctx1);

let lookup = (ctx, x) => List.assoc_opt(x, ctx);

let contains = (ctx, x) => List.mem_assoc(x, ctx);

let map = (f: ((Var.t, 'a)) => 'b, xas: t_('a)) =>
  List.map(((x, _) as xa) => (x, f(xa)), xas);

let length = List.length;

let to_list = ctx => ctx;
let of_list = ctx => ctx;
