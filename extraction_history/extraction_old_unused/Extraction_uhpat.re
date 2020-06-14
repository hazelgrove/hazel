// this file translate a UHPat pattern to extract_t
// meaning (pattern string in ocaml, inter_t)

open Extraction_types;

let uhpat_trans = (~t: UHPat.t): extract_t =>
  switch (t) {
  | _ => ("", TBD)
  };
