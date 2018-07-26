open Semantics.Core;
exception InvalidSyntax(string);
exception IllFormed(UHExp.t);
let string_of_side = side =>
  switch (side) {
  | UHExp.L => "L"
  | UHExp.R => "R"
  };
let lamSym = "λ";
let caseArrowSym = "⇒";
let typeArrowSym = "→";
