open Alcotest;
open Haz3lcore;
open Base;

// Id ignoring equality for tiles
let rec equal_segment = (a: segment, b: segment) => {
  List.equal(equal_piece, a, b);
}
and equal_piece = (a: piece, b: piece) => {
  switch (a, b) {
  | (Tile(t1), Tile(t2)) =>
    t1.label == t2.label
    && List.equal(equal_segment, t1.children, t2.children)
    && t1.mold == t2.mold
    && t1.shards == t2.shards
  | (Grout(g1), Grout(g2)) => g1.shape == g2.shape
  | (Secondary(s1), Secondary(s2)) => s1.content == s2.content
  | (Projector(p1), Projector(p2)) =>
    p1.kind == p2.kind
    && p1.model == p2.model
    && equal_piece(p1.syntax, p2.syntax)
  | _ => false
  };
};

let segment = testable(Fmt.using(Segment.show, Fmt.string), equal_segment);
let exp_to_segment =
  ExpToSegment.(
    exp_to_segment(~settings=Settings.of_core(~inline=true, CoreSettings.on))
  );

let zipper_parse = (s: string) =>
  Option.map(Printer.seg_of_zip, Printer.zipper_of_string(s));

let equivalent_to_make_term = (serialized: string) => {
  switch (Printer.zipper_of_string(serialized)) {
  | None => Alcotest.fail("Failed to parse term")
  | Some(zb) =>
    let exp = MakeTerm.from_zip_for_sem(zb).term;
    let seg = Printer.seg_of_zip(zb);
    check(
      segment,
      "Make term equivalent: " ++ serialized,
      seg,
      exp_to_segment(exp),
    );
  };
};

let mk_form = (form_name: Form.compound_form): Piece.t => {
  let form: Form.t = Form.get(form_name);

  Tile({
    id: Id.invalid,
    label: form.label,
    mold: form.mold,
    shards: [0],
    children: [],
  });
};

let segmentize =
  ExpToSegment.exp_to_segment(
    ~settings={
      inline: true,
      fold_case_clauses: false,
      fold_fn_bodies: false,
      hide_fixpoints: false,
      fold_cast_types: false,
      show_filters: true,
      show_unknown_as_hole: true,
    },
    _,
  );

let tests = (
  "ExpToSegment",
  [
    test_case(
      "Literals",
      `Quick,
      () => {
        check(
          segment,
          "Integer",
          [
            Tile({
              id: Id.invalid,
              label: ["1"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(Exp.temp(Int(1))),
        );
        check(
          segment,
          "String",
          [
            Tile({
              id: Id.invalid,
              label: ["\"hello\""],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(Exp.temp(String("hello"))),
        );
      },
    ),
    test_case(
      "Negative ints",
      `Quick,
      () => {
        let _ = Alcotest.skip(); // TODO: Fix negative ints
        equivalent_to_make_term("-1");
      },
    ),
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
      "Tuple",
      `Quick,
      () => {
        check(
          segment,
          "Unit",
          [
            Tile({
              id: Id.invalid,
              label: ["()"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(Exp.temp(Tuple([]))),
        );
        check(
          option(segment),
          "2-ary",
          zipper_parse("(1, 2)"),
          Some(
            exp_to_segment(
              Exp.temp(Tuple([Exp.temp(Int(1)), Exp.temp(Int(2))])),
            ),
          ),
        );
      },
    ),
    test_case(
      "Basic Labeled Tuples",
      `Quick,
      () => {
        check(
          option(segment),
          "Singleton Labeled",
          zipper_parse("(x=1)"),
          Some(
            exp_to_segment(
              Exp.temp(
                Tuple([
                  Exp.temp(
                    TupLabel(Exp.temp(Label("x")), Exp.temp(Int(1))),
                  ),
                ]),
              ),
            ),
          ),
        );
        equivalent_to_make_term({|(x=1, y=2)|});
      },
    ),
    test_case("Doc page labeled tuple example", `Quick, () => {
      equivalent_to_make_term(
        {|let labeled_tuple = (a=1, b=2.000000, c=true) in let prj_a = labeled_tuple.a in prj_a|},
      )
    }),
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
