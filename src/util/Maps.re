open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module StringKey = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string;
  let compare = String.compare;
};
module StringMap = MapUtil.Make(StringKey);
