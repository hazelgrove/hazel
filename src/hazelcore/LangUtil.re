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
let nondisplay1 = "​";
let nondisplay2 = "​​";
let nondisplay3 = "​​​";
let nondisplay4 = "​​​​";
