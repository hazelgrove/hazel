open Semantics.Core;

exception InvalidSyntax(string);

exception IllFormed(UHExp.t);

let string_of_side = side =>
  switch (side) {
  | UHExp.L => "L"
  | UHExp.R => "R"
  };

let lamSym = "\206\187";

let caseArrowSym = "\226\135\146";

let typeArrowSym = "\226\134\146";
