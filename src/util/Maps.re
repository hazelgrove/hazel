include Sexplib.Std;
include Ppx_yojson_conv_lib.Yojson_conv.Primitives;

module IntMap =
  MapUtil.Make({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = int;
    let compare = Int.compare;
  });

module BoolMap =
  MapUtil.Make({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = bool;
    let compare = Bool.compare;
  });

module FloatMap =
  MapUtil.Make({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = float;
    let compare = Float.compare;
  });

module StringMap =
  MapUtil.Make({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = string;
    let compare = String.compare;
  });
