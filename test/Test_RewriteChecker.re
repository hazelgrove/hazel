open Alcotest;
open Language;
open IdTagged.FreshGrammar;

let print = exp =>
  Web.RewriteChecker.print_exp_for_algebrite(~name_other=_ => "unknown", exp);

let tests = (
  "RewriteChecker",
  [
    test_case(
      "Print exact Reals for Algebrite",
      `Quick,
      () => {
        check(string, "pi", "pi", print(Exp.real(Real.Pi)));
        check(
          string,
          "rational",
          "(1/3)",
          print(
            Exp.real(Real.normalize(Bigint.one, Bigint.of_int(3), None)),
          ),
        );
        check(
          string,
          "negative integer",
          "-2",
          print(Exp.real(Real.of_bigint(Bigint.of_int(-2)))),
        );
      },
    ),
    test_case(
      "Print recognized functions for Algebrite",
      `Quick,
      () => {
        check(
          string,
          "elaborated built-in function",
          "sin(pi)",
          print(
            Exp.ap(Forward, Exp.builtin_fun("sin"), Exp.real(Real.Pi)),
          ),
        );
        check(
          string,
          "exact rational argument",
          "cos((1/3))",
          print(
            Exp.ap(
              Forward,
              Exp.builtin_fun("cos"),
              Exp.real(Real.normalize(Bigint.one, Bigint.of_int(3), None)),
            ),
          ),
        );
        check(
          string,
          "shadowed function is not treated as a built-in",
          "unknown",
          print(Exp.ap(Forward, Exp.var("sin"), Exp.real(Real.Pi))),
        );
      },
    ),
    test_case(
      "Print exact Real operators for Algebrite",
      `Quick,
      () => {
        check(
          string,
          "power",
          "(2 ^ 3)",
          print(
            Exp.bin_op(
              Operators.Real(Power),
              Exp.real(Real.of_bigint(Bigint.of_int(2))),
              Exp.real(Real.of_bigint(Bigint.of_int(3))),
            ),
          ),
        );
        check(
          string,
          "negation",
          "(-2)",
          print(
            Exp.un_op(
              Operators.Real(Minus),
              Exp.real(Real.of_bigint(Bigint.of_int(2))),
            ),
          ),
        );
      },
    ),
  ],
);
