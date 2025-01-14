open Alcotest;
open Haz3lcore;

let segmentize =
  ExpToSegment.exp_to_segment(
    ~settings={
      inline: true,
      fold_case_clauses: false,
      fold_fn_bodies: true,
      hide_fixpoints: true,
      fold_cast_types: true,
      show_filters: true,
    },
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
          "let []: (+ Jg) = ? in ?",
          serialized,
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
          "case x | A => 1| B => 2 end",
          serialized,
        );
      },
    ),
    test_case(
      "Deferred application",
      `Quick,
      () => {
        let segment =
          segmentize(
            DeferredAp(
              Var("string_sub") |> Exp.fresh,
              [
                String("hello") |> Exp.fresh,
                Int(1) |> Exp.fresh,
                Deferral(InAp) |> Exp.fresh,
              ],
            )
            |> Exp.fresh,
          );
        let serialized = Printer.of_segment(~holes=Some("?"), segment);

        check(
          string,
          "deferral in application",
          {|string_sub("hello", 1, _)|},
          serialized,
        );
      },
    ),
    test_case(
      "Test",
      `Quick,
      () => {
        let segment =
          segmentize(Test(Bool(true) |> Exp.fresh) |> Exp.fresh);
        let serialized = Printer.of_segment(~holes=Some("?"), segment);

        check(string, "Test of true", {|test true end|}, serialized);
      },
    ),
    test_case(
      "Filter",
      `Quick,
      () => {
        let segment =
          segmentize(
            Filter(
              Filter({pat: Int(1) |> Exp.fresh, act: (Step, One)}),
              Int(2) |> Exp.fresh,
            )
            |> Exp.fresh,
          );
        let serialized = Printer.of_segment(~holes=Some("?"), segment);

        check(string, "Pause", serialized, {|pause 1 in 2|});
      },
    ),
    test_case(
      "Right associativity",
      `Quick,
      () => {
        check(
          string,
          "No parens",
          Printer.of_segment(
            ~holes=Some("?"),
            segmentize(
              BinOp(
                Int(Power),
                Int(2) |> Exp.fresh,
                BinOp(Int(Power), Int(3) |> Exp.fresh, Int(4) |> Exp.fresh)
                |> Exp.fresh,
              )
              |> Exp.fresh,
            ),
          ),
          {|2 ** 3 ** 4|},
        );
        check(
          string,
          "Parens",
          Printer.of_segment(
            ~holes=Some("?"),
            segmentize(
              BinOp(
                Int(Power),
                BinOp(Int(Power), Int(2) |> Exp.fresh, Int(3) |> Exp.fresh)
                |> Exp.fresh,
                Int(4) |> Exp.fresh,
              )
              |> Exp.fresh,
            ),
          ),
          {|(2 ** 3) ** 4|},
        );
        check(
          string,
          "Arrow types",
          Printer.of_segment(
            ~holes=Some("?"),
            segmentize(
              TyAlias(
                Var("x") |> TPat.fresh,
                Arrow(
                  Arrow(Int |> Typ.fresh, Bool |> Typ.fresh) |> Typ.fresh,
                  Var("x") |> Typ.fresh,
                )
                |> Typ.fresh,
                Int(1) |> Exp.fresh,
              )
              |> Exp.fresh,
            ),
          ),
          {|type x = (Int -> Bool) -> x in 1|},
        );
      },
    ),
  ],
);
