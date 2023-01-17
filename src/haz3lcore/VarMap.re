open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t_('a) = list((Token.t, 'a));

let empty = [];

let is_empty =
  fun
  | [] => true
  | [_, ..._] => false;

let extend = (ctx, xa) => {
  [xa, ...ctx];
};

// The new things should go to the right when concatenating
let concat = (ctx, new_ctx) => new_ctx @ ctx;

let lookup = (ctx, x) => List.assoc_opt(x, ctx);

let lookup_all = (ctx, x) =>
  List.fold_left((vs, (tok, v)) => {tok == x ? [v, ...vs] : vs}, [], ctx);

let contains = (ctx, x) => List.mem_assoc(x, ctx);

let map = (f, xs) => List.map(((x, _) as xa) => (x, f(xa)), xs);

let filter = List.filter;

let length = List.length;

let to_list = ctx => ctx;

let find_map = List.find_map;
