open Alcotest;
open Language;

/* Hazel values, rendered as Fumola source: the way in.
 *
 * These build the Hazel value directly rather than parsing one, so a failure
 * here is in the rendering and not in anything upstream of it. */

let int = n => DHExp.fresh(Atom(Int(Bigint.of_int(n))));
let str = s => DHExp.fresh(Atom(String(s)));
let tuple = es => DHExp.fresh(Tuple(es));
let list = es => DHExp.fresh(ListLit(es));
let ctr = name => DHExp.fresh(Constructor(name, None));
let ap = (name, payload) =>
  DHExp.fresh(Ap(Forward, ctr(name), payload));
let labelled = (l, e) =>
  DHExp.fresh(TupLabel(DHExp.fresh(Label(l)), e));

let renders = (name, exp, expected) =>
  test_case(name, `Quick, () =>
    switch (FumolaSource.of_exp(exp)) {
    | Error(m) => Alcotest.fail("expected source, got: " ++ m)
    | Ok(source) => Alcotest.check(Alcotest.string, name, expected, source)
    }
  );

let refuses = (name, exp) =>
  test_case(name, `Quick, () =>
    switch (FumolaSource.of_exp(exp)) {
    | Ok(source) =>
      Alcotest.fail("expected a refusal, got source: " ++ source)
    | Error(_) => ()
    }
  );

let tests = (
  "FumolaSource",
  [
    renders("an integer", int(3), "3"),
    renders("a boolean", DHExp.fresh(Atom(Bool(true))), "true"),
    renders("text", str("hi"), {|"hi"|}),
    renders("unit", tuple([]), "()"),
    renders("a tuple", tuple([int(1), str("a")]), {|(1, "a")|}),
    /* A Fumola record is written with semicolons. */
    renders(
      "a record",
      tuple([labelled("x", int(1)), labelled("y", int(2))]),
      "{x = 1; y = 2}",
    ),
    renders("a list", list([int(1), int(2)]), "[1, 2]"),
    renders("an empty list", list([]), "[]"),
    /* Fumola's option. */
    renders("None is null", ctr("None"), "null"),
    renders("Some is ?(x)", ap("Some", int(3)), "?(3)"),
    /* Hazel's Symbol type, back into Fumola's symbol syntax. */
    renders("a named symbol", ap("Name", str("x")), "`x"),
    renders("a numeric symbol", ap("Num", int(7)), "7"),
    renders(
      "an applied symbol",
      ap("Call", tuple([ap("Name", str("a")), ap("Name", str("b"))])),
      "`a(`b)",
    ),
    renders(
      "a dotted symbol",
      ap("Dot", tuple([ap("Name", str("a")), ap("Name", str("b"))])),
      "`a.`b",
    ),
    /* The shape the mergeSort example takes: symbols paired with numbers. */
    renders(
      "a list of symbol and int pairs",
      list([
        tuple([ap("Name", str("b")), int(2)]),
        tuple([ap("Name", str("a")), int(1)]),
      ]),
      "[(`b, 2), (`a, 1)]",
    ),
    /* Other constructors are variant tags, as Hazel spells them: Fumola
       accepts a capitalised tag, so the capital that translation adds on the
       way in survives the way out. */
    renders("a bare variant", ctr("Circle"), "#Circle"),
    renders("an applied variant", ap("Circle", int(3)), "#Circle(3)"),
    /* Refused rather than guessed at. */
    refuses("a hole", DHExp.fresh(EmptyHole)),
    refuses("a partly labelled tuple", tuple([labelled("x", int(1)), int(2)])),
  ],
);
