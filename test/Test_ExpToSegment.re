open Alcotest;
open Haz3lcore;

let segmentize =
  ExpToSegment.exp_to_segment(
    ~settings=ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
    _,
  );

let tests = (
  "ExpToSegment",
  [
    test_case(
      "Empty Ids on ExpToSegment constructor",
      `Quick,
      () => {
        let segment =
          segmentize(
            Let(
              Cast(
                ListLit([]) |> Pat.fresh,
                Sum([Variant("Jg", [], None)]) |> Typ.fresh,
                Float |> Typ.fresh,
              )
              |> Pat.fresh,
              EmptyHole |> Exp.fresh,
              EmptyHole |> Exp.fresh,
            )
            |> Exp.fresh,
          );
        let serialized = Printer.of_segment(~holes=Some("?"), segment);

        check(
          string,
          "ascribed sum type constructor in pattern",
          serialized,
          "let []: (+ Jg) = ? in ?",
        );
      },
    ),
    test_case(
      "Match statement",
      `Quick,
      () => {
        let segment =
          segmentize(
            Match(
              Var("x") |> Exp.fresh,
              [
                (
                  Constructor("A", Unknown(Internal) |> Typ.fresh)
                  |> Pat.fresh,
                  Int(1) |> Exp.fresh,
                ),
                (
                  Constructor("B", Unknown(Internal) |> Typ.fresh)
                  |> Pat.fresh,
                  Int(2) |> Exp.fresh,
                ),
              ],
            )
            |> Exp.fresh,
          );
        let serialized = Printer.of_segment(~holes=Some("?"), segment);

        check(
          string,
          "Match statement",
          serialized,
          "case x | A => 1| B => 2 end",
        );
      },
    ),
  ],
);
