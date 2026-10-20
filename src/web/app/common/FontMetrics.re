open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  row_height: float,
  col_width: float,
  /* the height of a row's text box; the rest of `row_height` is leading */
  text_height: float,
};

let init = {
  row_height: 10.,
  col_width: 10.,
  text_height: 10.,
};
