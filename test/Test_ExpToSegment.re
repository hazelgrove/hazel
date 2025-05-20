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

let exp_to_segment_settings: ExpToSegment.Settings.t = {
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: false,
  hide_fixpoints: false,
  show_filters: true,
  show_unknown_as_hole: true,
};
let equivalent_to_make_term = (serialized: string) => {
  switch (Printer.zipper_of_string(serialized)) {
  | None => Alcotest.fail("Failed to parse term")
  | Some(zb) =>
    let exp = MakeTerm.from_zip_for_sem(zb).term;
    let seg =
      ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings, exp);
    check(
      string,
      "Make term print equivalent: " ++ serialized,
      serialized,
      Printer.of_segment(~holes=Some("?"), seg),
    );
    check(
      segment,
      "Make term equivalent: " ++ serialized,
      seg,
      exp_to_segment(exp),
    );
  };
};

let segmentize =
  ExpToSegment.exp_to_segment(~settings=exp_to_segment_settings, _);

module TempGrammar =
  Grammar.Factory({
    type t = IdTagged.IdTag.t;
    let default_value: unit => IdTagged.IdTag.t = () => {ids: [Id.invalid]};
  });
let tests = (
  "ExpToSegment",
  [
    test_case(
      "Literals",
      `Quick,
      () => {
        open TempGrammar.Exp;
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
          exp_to_segment(int(1)),
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
          exp_to_segment(string("hello")),
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
        open IdTagged.FreshGrammar;
        open Exp;
        let segment =
          segmentize(
            let_(
              Pat.(
                cast(
                  list_lit([]),
                  Typ.(sum([Variant("Jg", [], None)])),
                  Typ.(float()),
                )
              ),
              empty_hole(),
              empty_hole(),
            ),
          );
        let serialized = Printer.of_segment(~holes=Some("?"), segment);

        check(
          Alcotest.string,
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
        open TempGrammar.Exp;
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
          exp_to_segment(tuple([])),
        );
        check(
          option(segment),
          "2-ary",
          zipper_parse("(1, 2)"),
          Some(exp_to_segment(tuple([int(1), int(2)]))),
        );
      },
    ),
    test_case(
      "Basic Labeled Tuples",
      `Quick,
      () => {
        open TempGrammar.Exp;

        check(
          option(segment),
          "Singleton Labeled",
          zipper_parse("(x=1)"),
          Some(exp_to_segment(tuple([tup_label(label("x"), int(1))]))),
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
        open IdTagged.FreshGrammar;
        open Exp;
        let segment =
          segmentize(
            match(
              var("x"),
              [
                (Pat.(constructor("A", None)), int(1)),
                (Pat.(constructor("B", None)), int(2)),
              ],
            ),
          );
        let serialized = Printer.of_segment(~holes=Some("?"), segment);

        check(
          Alcotest.string,
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
          IdTagged.FreshGrammar.Exp.(
            segmentize(
              deferred_ap(
                var("string_sub"),
                [string("hello"), int(1), deferral(InAp)],
              ),
            )
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
          IdTagged.FreshGrammar.Exp.(segmentize(test(bool(true))));
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
            IdTagged.FreshGrammar.Exp.(
              filter(
                Filter({
                  pat: int(1),
                  act: (Step, One),
                }),
                int(2),
              )
            ),
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
              IdTagged.FreshGrammar.Exp.(
                bin_op(
                  Int(Power),
                  int(2),
                  bin_op(Int(Power), int(3), int(4)),
                )
              ),
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
              IdTagged.FreshGrammar.Exp.(
                bin_op(
                  Int(Power),
                  bin_op(Int(Power), int(2), int(3)),
                  int(4),
                )
              ),
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
              IdTagged.FreshGrammar.(
                Exp.ty_alias(
                  TPat.(var("x")),
                  Typ.(arrow(arrow(int(), bool()), var("x"))),
                  Exp.(int(1)),
                )
              ),
            ),
          ),
          {|type x = (Int -> Bool) -> x in 1|},
        );
      },
    ),
    test_case("Unit type", `Quick, () => {
      check(
        string,
        "Unit type",
        "()",
        Printer.of_segment(
          ~holes=Some("?"),
          ExpToSegment.typ_to_segment(
            ~settings=exp_to_segment_settings,
            IdTagged.FreshGrammar.Typ.prod([]),
          ),
        ),
      )
    }),
    test_case("Function call", `Quick, () => {
      equivalent_to_make_term("a(1, 2)")
    }),
    test_case("Unit pattern", `Quick, () => {
      check(
        string,
        "Unit pattern",
        "()",
        Printer.of_segment(
          ~holes=Some("?"),
          ExpToSegment.any_to_segment(
            ~settings={
              inline: true,
              fold_case_clauses: false,
              fold_fn_bodies: false,
              hide_fixpoints: false,

              show_filters: true,
              show_unknown_as_hole: true,
            },
            Pat(IdTagged.FreshGrammar.Pat.tuple([])),
          ),
        ),
      )
    }),
  ],
);
