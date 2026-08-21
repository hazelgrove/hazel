open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type t_('a) = list((string, 'a));

let empty = [];

let extend = (ctx, xa) => {
  [xa, ...ctx];
};

let lookup = (ctx, x) => List.Assoc.find(~equal=Poly.equal, ctx, x);

let contains = (ctx, x) => List.Assoc.mem(~equal=Poly.equal, ctx, x);

let filter = (f, xs) => List.filter(~f, xs);

let to_list = ctx => ctx;

let rec update = (ctx: t_('a), name: string, f: 'a => 'a): t_('a) =>
  switch (ctx) {
  | [] => []
  | [(k, v), ...ctx] =>
    if (String.equal(name, k)) {
      [(k, f(v)), ...ctx];
    } else {
      [(k, v), ...update(ctx, name, f)];
    }
  };
