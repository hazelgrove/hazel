open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t_('a) = list((Token.t, 'a));

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

let map = (f, xs) => List.map(((x, _) as xa) => (x, f(xa)), xs);

let filter = List.filter;

let length = List.length;

let to_list = ctx => ctx;
