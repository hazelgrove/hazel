open Alcotest;

/* Unit tests for Haz3lcore.TestGen.parse_model: parsing raw z3 output text
 * into outcomes. Shared by every solver backend, so worth testing directly.
 * Pure (no solver invocation), runs natively and under node. */

module TG = Haz3lcore.TestGen;

let outcome_t =
  Alcotest.testable(
    (fmt, o) => Format.fprintf(fmt, "%s", TG.show_outcome(o)),
    (==),
  );

let case = (name, input, expected) =>
  test_case(name, `Quick, () =>
    check(outcome_t, name, expected, TG.parse_model(input))
  );

let tests = (
  "TestGen.parse_model",
  [
    case(
      "sat single-line model",
      "sat\n((define-fun x () Int 6))",
      TG.Sat([
        {
          name: "x",
          value: "6",
        },
      ]),
    ),
    case(
      "sat multi-line model (z3 format)",
      "sat\n(\n  (define-fun x () Int\n    6)\n)",
      TG.Sat([
        {
          name: "x",
          value: "6",
        },
      ]),
    ),
    case(
      "negative value flattened",
      "sat\n((define-fun y () Int (- 6)))",
      TG.Sat([
        {
          name: "y",
          value: "-6",
        },
      ]),
    ),
    case(
      "string value",
      "sat\n((define-fun s () String \"abcd\"))",
      TG.Sat([
        {
          name: "s",
          value: "abcd",
        },
      ]),
    ),
    case(
      "two assignments",
      "sat\n((define-fun x () Int 7) (define-fun y () Int (- 3)))",
      TG.Sat([
        {
          name: "x",
          value: "7",
        },
        {
          name: "y",
          value: "-3",
        },
      ]),
    ),
    case("unsat", "unsat", TG.Unsat),
    case("unknown", "unknown", TG.Unknown),
    case(
      "leading (error ...) lines before sat are skipped",
      "(error \"line 6 column 23: the logic has already been set\")\n(error \"already declared\")\nsat\n((define-fun x () Int 6))",
      TG.Sat([
        {
          name: "x",
          value: "6",
        },
      ]),
    ),
    case(
      "only errors, no status -> Error",
      "(error \"boom\")",
      TG.Error("(error \"boom\")"),
    ),
  ],
);
