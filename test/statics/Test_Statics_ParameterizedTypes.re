open Test_Statics_Prelude;
open Alcotest;
open Language;

let parse_menhir_exp = (src: string): TermBase.Exp.t =>
  MenhirParser.Interface.parse_program(src)
  |> MenhirParser.Conversion.Exp.of_menhir_ast
  |> Grammar.map_exp_annotation(_ => IdTagged.IdTag.mk_internal([Id.mk()]));

let static_errors = src => {
  let exp = parse_menhir_exp(src);
  statics(exp) |> errors |> List.map(snd) |> List.flatten;
};

let has_mark = (expected: Mark.t, marks: list(Mark.t)): bool =>
  List.exists(mark => equal_mark(mark, expected), marks);

let tests = (
  "Statics.ParameterizedTypes",
  [
    test_case(
      "applied type constructor has kind Type",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option(Int) = ? in x
|},
          );

        Alcotest.check(
          list(testable_issue),
          "Static Errors",
          [],
          List.map(ms => Marks([ms]), marks),
        );
      },
    ),
    test_case(
      "expected Option(Int) drives Some(Int)",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option(Int) = Some(3) in x
|},
          );

        Alcotest.check(
          list(testable_issue),
          "Static Errors",
          [],
          List.map(ms => Marks([ms]), marks),
        );
      },
    ),
    test_case(
      "wrong parameterized constructor payload is rejected",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option(Int) = Some(true) in x
|},
          );

        check(bool, "payload mismatch", true, !List.is_empty(marks));
      },
    ),
    test_case(
      "recursive List(Int) constructor payload checks",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type List(a) = + Nil + Cons(a, List(a)) in
let xs : List(Int) = Cons((1, Nil)) in xs
|},
          );

        Alcotest.check(
          list(testable_issue),
          "Static Errors",
          [],
          List.map(ms => Marks([ms]), marks),
        );
      },
    ),
    test_case(
      "bare type constructor rejected in Type position",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option = ? in x
|},
          );

        check(
          bool,
          "kind mismatch",
          true,
          has_mark(
            Mark.TypKindMismatch({
              expected: TypKind.Type,
              actual: TypKind.Arrow(TypKind.Type, TypKind.Type),
            }),
            marks,
          ),
        );
      },
    ),
    test_case(
      "non-constructor type application rejected",
      `Quick,
      () => {
        let marks = static_errors({|let x : Int(Bool) = ? in x|});

        check(
          bool,
          "apply non-arrow kind",
          true,
          has_mark(Mark.TypApplyNonArrowKind(TypKind.Type), marks),
        );
      },
    ),
  ],
);
