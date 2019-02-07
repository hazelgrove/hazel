open SemanticsCommon;
exception InvalidSyntax(string);
exception IllFormed(UHExp.t);
let string_of_side = side =>
  switch (side) {
  | L => "L"
  | R => "R"
  };
let lamSym = "λ";
let caseArrowSym = "⇒";
let typeArrowSym = "→";
