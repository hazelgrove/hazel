open Alcotest;
open Language;
open Haz3lcore;

let tests = [
  (
    "LiftToSegment",
    [
      test_case(
        "lift simple exp to segment",
        `Quick,
        () => {
          let source_program = {hz|(1 + ## 4 + 3) + (5 +     6    -   7)|hz};
          let parsed = Parser.to_segment(source_program) |> Option.get;
          let transformation = (exp: TermBase.exp_t): Exp.t =>
            switch (exp.term) {
            | BinOp(op, left, right) =>
              BinOp(
                op,
                right, // Flipping operands
                left,
              )
              |> Exp.fresh
            | _ => Alcotest.fail("Unexpected expression structure")
            };
          let lifted_segment: Segment.t =
            LiftToSegment.lift_segment(
              ~settings=
                ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
              transformation,
              parsed,
            );

          let serialized = Printer.of_segment(lifted_segment);
          let expected_output = "(5 +     6    -   7) + (1 + ## 4 + 3)";
          Alcotest.check(
            string,
            "Serialized output matches",
            expected_output,
            serialized,
          );
          Alcotest.check(
            string,
            "Lifted segment matches expected output",
            expected_output,
            serialized,
          );
        },
      ),
    ],
  ),
];
