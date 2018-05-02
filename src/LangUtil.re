open Semantics.Core;

let string_of_side side =>
  switch side {
  | UHExp.L => "L"
  | UHExp.R => "R"
  };

/* NOTE: If you change these, they must be changed in HZLex.mll as well! */
let lamSym = "\206\187";

let caseArrowSym = "\226\135\146";

let typeArrowSym = "\226\134\146";
