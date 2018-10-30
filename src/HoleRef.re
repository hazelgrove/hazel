open Semantics.Core;

module HoleRef: HOLEREF = {
  let to_var = n => "__hole_ref_" ++ string_of_int(n) ++ "__";
};
