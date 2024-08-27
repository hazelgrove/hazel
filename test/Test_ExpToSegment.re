open Alcotest;
open Haz3lcore;

let settings: ExpToSegment.Settings.t =
  ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off);

let print_term = (exp: Exp.term): string =>
  exp
  |> IdTagged.fresh
  |> ExpToSegment.exp_to_segment(~settings)
  |> Printer.of_segment(~holes=Some("?"));

let tests = (
  "ExpToSegment",
  [
    test_case(
      "Literals",
      `Quick,
      () => {
        check(string, "Literal 0", "0", print_term(Int(0)));
        check(string, "Literal true", "true", print_term(Bool(true)));
      },
    ),
    test_case("Invalid", `Quick, () =>
      check(
        string,
        "Invalid",
        "Invalid Message",
        print_term(Invalid("Invalid Message")),
      )
    ),
    test_case("MultiHole", `Quick, () =>
      check(
        string,
        "MultiHole",
        "¿",
        print_term(
          MultiHole([
            Exp(IdTagged.fresh(Int(0): Exp.term)),
            Exp(IdTagged.fresh(Bool(false): Exp.term)),
          ]),
        ),
      )
    ),
  ],
);
