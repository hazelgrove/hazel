open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type paint =
  | Type(Typ.head)
  | Scope(int);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: Id.t,
  level: int,
  paint,
};

let scope_colors = 6;

let scope_color_name = (i: int) => "depth-" ++ string_of_int(i);

let scope_color_names = List.init(scope_colors, scope_color_name);

let paint_name: paint => string =
  fun
  | Type(head) => "type-block-" ++ Typ.head_name(head)
  | Scope(depth) =>
    "scope-block-" ++ scope_color_name(depth mod scope_colors);
