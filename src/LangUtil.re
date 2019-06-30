open SemanticsCommon;
exception InvalidSyntax(string);
exception IllFormed(UHExp.block);
let string_of_side = side =>
  switch (side) {
  | L => "L"
  | R => "R"
  };
let lamSym = "λ";
let caseArrowSym = "⇒";
let typeArrowSym = "→";
let nondisplay2 = "​​";
