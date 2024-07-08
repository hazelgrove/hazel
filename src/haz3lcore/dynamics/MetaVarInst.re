open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/**
 * Hole instance index in DHPat and DHExp
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
