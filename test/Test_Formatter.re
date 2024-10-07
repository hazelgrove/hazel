
open Alcotest;
open Haz3lcore;
open Haz3lformatter;

let parse_exp = (s: string) =>
  MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).term;
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
  test_case("Incomplete Function Definition", `Quick, () => {
    check(
      string,
      "let    = fun x ->   in  ",
      "let ? = (fun x -> ?) in ?",
      Formatter.format(~inline=true, "let    = fun x ->   in  "),
    )
  }),
];
