open Alcotest;
open Test_Statics_Prelude;

let tests = (
  "Statics.Parens",
  [
    test_case(
      "Parenthesized inconsistent if-else has inner cls",
      `Quick,
      () => {
        let exp = parse_exp("(if true then 1 else \"\")");
        let cls = Language.Exp.cls_of_term(exp.term);
        check(
          string,
          "cls should be If, not Parens",
          "If expression",
          Language.Exp.show_cls(cls),
        );
      },
    ),
    test_case(
      "Statics on parenthesized inconsistent if-else does not crash",
      `Quick,
      () => {
        let exp = parse_exp("(if true then 1 else \"\")");
        let _ = statics(exp);
        ();
      },
    ),
  ],
);
