/**
  Identifier for a unique hole closure/instantiation (unique among hole
  closures for a given hole number).
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
