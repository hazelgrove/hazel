open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type t_('a) = list((string, 'a));

let empty = [];

let extend = (ctx, xa) => {
  [xa, ...ctx];
};

let lookup = (ctx, x) => List.assoc_opt(x, ctx);

let contains = (ctx, x) => List.mem_assoc(x, ctx);

let filter = List.filter;

let to_list = ctx => ctx;

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
