// this file translate a UHExp expression to extract_t
// meaning (expression string in ocaml, inter_t)

open Extraction_types;

//The inter_t of a block is the exp-line, let-line should just update the environment and has STR type
let uhexp_trans = (~t: UHExp.t): extract_t =>
  switch (t) {
  | _ => ("", TBD)
  };

// top level of the extraction, called by the website
let extraction_call = (~t: UHExp.t): string => {
  switch (t) {
  | _ => "To be Implemented"
  };
};
