open Alcotest;
open Haz3lcore;

let tests = (
  "ExpToSegment",
  [
    test_case(
      "Empty Ids on ExpToSegment does not throw",
      `Quick,
      () => {
        let _ =
          ExpToSegment.exp_to_segment(
            ~settings=
              ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
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
        ();
      },
    ),
  ],
);
