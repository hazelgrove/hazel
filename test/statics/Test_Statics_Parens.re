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
    /* Elaboration keeps Parens and Projector nodes, so head checks must
       look through them. */
    test_case(
      "Parenthesized or folded constructor head is a constructor application",
      `Quick,
      () =>
      List.iter(
        src => {
          let pat =
            switch (parse_exp("fun " ++ src ++ " -> ?").term) {
            | Fun(p, _, _, _) => p
            | _ => fail("expected a lambda for: " ++ src)
            };
          check(
            string,
            src,
            "Constructor application",
            Language.Pat.show_cls(Language.Pat.cls_of_term(pat.term)),
          );
        },
        ["Some(x)", "(Some)(x)", "^^fold(Some)(x)"],
      )
    ),
  ],
);
