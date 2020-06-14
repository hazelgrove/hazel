// this file translate a UHTyp type to extract_t
// meaning (string in ocaml, inter_t)

open Extraction_types;

let uhtyp_trans = (~t: UHTyp.t): extract_t =>
  switch (t) {
  | _ => ("", TBD)
  };
