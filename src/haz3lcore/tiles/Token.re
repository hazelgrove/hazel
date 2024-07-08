open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

// make an enum
[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

module Index = {
  type t = int;
};

let length = String.length;
let compare = String.compare;
let rm_nth = Util.StringUtil.remove_nth;
let rm_last = Util.StringUtil.remove_last;
let rm_first = Util.StringUtil.remove_first;
let split_nth = Util.StringUtil.split_nth;
let insert_nth = Util.StringUtil.insert_nth;
