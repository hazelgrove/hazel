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

let settings = ExpToSegment.Settings.of_core(~inline=true, CoreSettings.on);
let segment =
  testable(Fmt.using(Segment.show, Fmt.string), (x, y) =>
    equal_segment(x, y)
  );
let exp_to_segment = ExpToSegment.exp_to_segment(~settings);

let make_term_parse = (s: string) =>
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

let mk_form = (form_name: string): Piece.t => {
  let form: Form.t = Form.get(form_name);

  Tile({
    id: Id.invalid,
    label: form.label,
    mold: form.mold,
    shards: [0],
    children: [],
  });
};

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
          segment,
          "2-ary",
          [
            Tile({
              id: Id.invalid,
              label: ["1"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
            mk_form("comma_exp"),
            Secondary(Secondary.mk_space(Id.invalid)),
            Tile({
              id: Id.invalid,
              label: ["2"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(
            Exp.temp(Tuple([Exp.temp(Int(1)), Exp.temp(Int(2))])),
          ),
        );
      },
    ),
    test_case(
      "Labeled Tuple",
      `Quick,
      () => {
        check(
          segment,
          "Singleton Labeled",
          [
            Tile({
              id: Id.invalid,
              label: ["x"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
            Secondary(Secondary.mk_space(Id.invalid)),
            mk_form("tuple_labeled_exp"),
            Secondary(Secondary.mk_space(Id.invalid)),
            Tile({
              id: Id.invalid,
              label: ["1"],
              mold: Mold.mk_op(Exp, []),
              shards: [0],
              children: [],
            }),
          ],
          exp_to_segment(
            Exp.temp(
              Tuple([
                Exp.temp(
                  TupLabel(Exp.temp(Label("x")), Exp.temp(Int(1))),
                ),
              ]),
            ),
          ),
        );
        equivalent_to_make_term({|x = 1, y = 2|});
        equivalent_to_make_term({|(x = 1, y = 2)|});
      },
    ),
    test_case("Doc page labeled tuple", `Quick, () => {
      equivalent_to_make_term(
        {|let labeled_tuple = (a = 1, b = 2., c = true) in let prj_a = labeled_tuple . a in prj_a|},
      )
    }),
  ],
);
