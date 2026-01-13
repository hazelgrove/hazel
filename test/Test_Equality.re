open Alcotest;

open Language;
open IdTagged.FreshGrammar;

let tests = (
  "Equality",
  [
    test_case(
      "let alpha equivalence",
      `Quick,
      () => {
        let x1 = Exp.let_(Pat.var("x"), Exp.int(1), Exp.var("x"));
        let x2 = Exp.let_(Pat.var("x'"), Exp.int(1), Exp.var("x'"));
        check(
          bool,
          "let x = 1 in x === let x' = 1 in x'",
          true,
          Equality.semantic.exp(x1, x2),
        );
      },
    ),
    test_case(
      "forall type inequality",
      `Quick,
      () => {
        let forall_string =
          Exp.forall(
          Pat.asc(Pat.var("x"), Typ.string()),
          Exp.bin_op(
              Operators.Poly(Operators.Equals),
              Exp.var("x"),
              Exp.var("x"),
            ),
          );
        let forall_int =
          Exp.forall(
            Pat.asc(Pat.var("x"), Typ.int()),
            Exp.bin_op(
              Operators.Poly(Operators.Equals),
              Exp.var("x"),
              Exp.var("x"),
            ),
          );
        check(
          bool,
          "forall x : String -> x == x !== forall x : Int -> x == x",
          false,
          Equality.semantic.exp(forall_string, forall_int),
        );
      },
    ),
  ],
);
