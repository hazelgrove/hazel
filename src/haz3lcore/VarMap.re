open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

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

let contains = (ctx, x) => List.mem_assoc(x, ctx);

let map = (f, xs) => List.map(((x, _) as xa) => (x, f(xa)), xs);

let filter = List.filter;

let length = List.length;

let to_list = ctx => ctx;

let find_map = List.find_map;

let rec update = (ctx: t_('a), name: string, f: 'a => 'a): t_('a) =>
  switch (ctx) {
  | [] => []
  | [(k, v), ...ctx] =>
    if (name == k) {
      [(k, f(v)), ...ctx];
    } else {
      [(k, v), ...update(ctx, name, f)];
    }
  };
