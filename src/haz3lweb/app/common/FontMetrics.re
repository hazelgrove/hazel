open Util;
open Js_of_ocaml;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  row_height: float,
  col_width: float,
};

let init = {row_height: 10., col_width: 10.};

let get_goal =
    (~font_metrics: t, text_box: Js.t(Dom_html.element), loc: Point.t)
    : Point.t => {
  open Float;
  let x_rel = of_int(loc.col) -. text_box##getBoundingClientRect##.left;
  let y_rel = of_int(loc.row) -. text_box##getBoundingClientRect##.top;
  let row = to_int(y_rel /. font_metrics.row_height);
  let col = to_int(round(x_rel /. font_metrics.col_width));
  {row, col};
};
