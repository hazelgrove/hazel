open Util;
open Base_quickcheck;
[@deriving (show({with_path: false}), sexp, yojson, quickcheck)]
type t =
  [@quickcheck.generator Generator.string_of(Generator.char_alpha)] string;

let equal = String.equal;
