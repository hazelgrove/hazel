open Sexplib.Std;

/**
 * Hole instance index in DHPat and DExp
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
