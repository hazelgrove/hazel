open Alcotest;
open Test_Evaluator_Prelude;
open Language;

/* Regression tests for the Phase 0 semantics invariant
   (docs/prover-obligations.md section 1.2): off-domain primitive evaluation
   must never yield a boolean verdict; it must produce an error/indet result
   with attributable provenance. Floats are IEEE-total by design (section
   1.5), which is pinned here too. */

/* Strip evaluation wrappers to get at the result's head term. */
let rec unwrap = (d: Exp.t): Exp.t =>
  switch (DHExp.term_of(d)) {
  | Closure(_, d1)
  | Parens(d1)
  | Asc(d1, _) => unwrap(d1)
  | _ => d
  };

let head_error = (d: Exp.t): option(InvalidOperationError.t) =>
  switch (DHExp.term_of(unwrap(d))) {
  | DynamicErrorHole(_, err) => Some(err)
  | _ => None
  };

let is_bool_lit = (d: Exp.t): bool =>
  switch (DHExp.term_of(unwrap(d))) {
  | Atom(Bool(_)) => true
  | _ => false
  };

let error_testable: testable(InvalidOperationError.t) =
  testable(
    Fmt.using(InvalidOperationError.show, Fmt.string),
    InvalidOperationError.equal,
  );

let check_error =
    (msg: string, expected: InvalidOperationError.t, program: string) =>
  check(
    option(error_testable),
    msg,
    Some(expected),
    head_error(parse_and_evaluate(program)),
  );

let check_no_verdict = (msg: string, program: string) => {
  let result = parse_and_evaluate(program);
  check(bool, msg ++ " (no boolean verdict)", false, is_bool_lit(result));
  check(bool, msg ++ " (not a value)", false, ValueChecker.is_value(result));
};

let tests = (
  "Evaluator.OffDomain",
  [
    test_case(
      "Division by zero is an error, not a value",
      `Quick,
      () => {
        check_error(
          "1 / 0 is a DivideByZero error hole",
          DivideByZero,
          "1 / 0",
        );
        check_no_verdict("1 / 0", "1 / 0");
      },
    ),
    test_case(
      "Equality on errors renders no verdict",
      `Quick,
      () => {
        check_no_verdict("1 / 0 == 1 / 0", "1 / 0 == 1 / 0");
        check_no_verdict("1 / 0 == 1", "1 / 0 == 1");
      },
    ),
    test_case(
      "Failed string conversion attributes its error",
      `Quick,
      () => {
        check_error(
          "int_of_string(\"abc\") is an InvalidOfString error hole",
          InvalidOfString,
          "int_of_string(\"abc\")",
        );
        check_no_verdict("int_of_string(\"abc\")", "int_of_string(\"abc\")");
      },
    ),
    test_case(
      "Float to integer conversion is off-domain on nan/inf",
      `Quick,
      () => {
        check_error(
          "int_of_float(nan) is a NonFiniteFloat error hole",
          NonFiniteFloat,
          "int_of_float(nan)",
        );
        check_error(
          "int_of_float(infinity) is a NonFiniteFloat error hole",
          NonFiniteFloat,
          "int_of_float(1. /. 0.)",
        );
        check_error(
          "sint_of_float(nan) is a NonFiniteFloat error hole",
          NonFiniteFloat,
          "sint_of_float(0. /. 0.)",
        );
        check_no_verdict("int_of_float(nan)", "int_of_float(nan)");
      },
    ),
    test_case("IEEE floats are total by design", `Quick, () => {
      /* 1.0 /. 0.0 is the value infinity, not an error: float
         arithmetic is deliberately total (section 1.5), so equality
         on it renders a genuine verdict. */
      evaluation_test(
        "1. /. 0. == 1. /. 0. evaluates to true",
        IdTagged.FreshGrammar.Exp.bool(true),
        elaborate(parse_exp("1. /. 0. == 1. /. 0.")),
      )
    }),
  ],
);
