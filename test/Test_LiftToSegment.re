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
      test_case(
        "preserve CST in nested function calls",
        `Quick,
        () => {
          let source_program = {hz|f(g(h(x
          + 5
          + 6)))
          |hz};
          let parsed = Parser.to_segment(source_program) |> Option.get;
          let transformation = (exp: TermBase.exp_t): Exp.t =>
            switch (exp.term) {
            | Ap(Forward, _, _) =>
              // Collect all functions in the call chain in reverse order
              let rec collect_funcs =
                      (e: Exp.t, funcs: list(Exp.t)): (Exp.t, list(Exp.t)) =>
                switch (e.term) {
                | Ap(Forward, func, arg) =>
                  collect_funcs(arg, [func, ...funcs])
                | _ => (e, funcs)
                };
              let (base_arg, func_list) = collect_funcs(exp, []);
              let rec rebuild_chain = (funcs: list(Exp.t)): Exp.t =>
                switch (funcs) {
                | [] => base_arg
                | [func, ...rest] =>
                  Ap(Forward, func, rebuild_chain(rest)) |> Exp.fresh
                };
              rebuild_chain(func_list);
            | _ => exp
            };
          let lifted_segment: Segment.t =
            LiftToSegment.lift_segment(
              ~settings=
                ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
              transformation,
              parsed,
            );

          let serialized = Printer.of_segment(lifted_segment);
          let expected_output = {hz|h(g(f(x
          + 5
          + 6)))|hz};
          Alcotest.check(
            string,
            "Nested function calls preserve CST",
            expected_output,
            serialized,
          );
        },
      ),
      test_case(
        "Wrap let binding around existing program",
        `Quick,
        () => {
          let source_program = {hz|let empty_hole =   in

# Non-empty holes are the red boxes around type errors #
# (you can still run programs with non-empty holes) #
let non_empty_hole : Int = true in

# Booleans #
let bool: Bool = true in
let operators = !true && false || true in
5 + 6|hz};
          let parsed = Parser.to_segment(source_program) |> Option.get;
          let transformation = (exp: TermBase.exp_t): Exp.t => {
            IdTagged.FreshGrammar.(Exp.(let_(Pat.var("y"), int(3), exp)));
          };
          let lifted_segment: Segment.t =
            LiftToSegment.lift_segment(
              ~settings=
                ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off),
              transformation,
              parsed,
            );

          let serialized = Printer.of_segment(~holes="", lifted_segment);
          let expected_output = {|let y = 3 in
let empty_hole =   in

# Non-empty holes are the red boxes around type errors #
# (you can still run programs with non-empty holes) #
let non_empty_hole : Int = true in

# Booleans #
let bool: Bool = true in
let operators = !true && false || true in
5 + 6|};
          Alcotest.check(
            string,
            "Let binding wrapped around existing program preserves existing program",
            expected_output,
            serialized,
          );
        },
      ),
    ],
  ),
];
