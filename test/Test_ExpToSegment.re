open Alcotest;
open Haz3lcore;

let serialize = (exp: Exp.t): string => {
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings=ExpToSegment.Settings.of_core(~inline=true, CoreSettings.on),
      exp,
    );
  let zipper = Zipper.unzip(seg);
  Printer.zipper_to_string(~holes=Some("?"), zipper);
};

let tests = [
  test_case(
    "Integer Literal",
    `Quick,
    () => {
      let exp = Int(0) |> Exp.fresh;
      let serialized = serialize(exp);
      check(string, "0", "0", serialized);
    },
  ),
  test_case(
    "Empty Hole",
    `Quick,
    () => {
      let exp = EmptyHole |> Exp.fresh;
      let serialized = serialize(exp);
      check(string, "?", "?", serialized);
    },
  ),
  test_case(
    "Let expression",
    `Quick,
    () => {
      let exp =
        Let(
          Var("x") |> Pat.fresh,
          Int(1) |> Exp.fresh,
          Var("x") |> Exp.fresh,
        )
        |> Exp.fresh;
      let serialized = serialize(exp);
      check(string, "let x = 1 in x", "let x = 1 in x", serialized);
    },
  ),
  test_case(
    "Function definition",
    `Quick,
    () => {
      let exp =
        Let(
          Var("f") |> Pat.fresh,
          Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None, None)
          |> Exp.fresh,
          Int(1) |> Exp.fresh,
        )
        |> Exp.fresh;
      let serialized = serialize(exp);
      check(
        string,
        "let f = (fun x -> x) in 1",
        "let f = (fun x -> x) in 1",
        serialized,
      );
    },
  ),
];
