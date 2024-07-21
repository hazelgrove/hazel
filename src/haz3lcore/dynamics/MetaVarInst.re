open Util;

/**
 * Hole instance index in DHPat and DHExp
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
