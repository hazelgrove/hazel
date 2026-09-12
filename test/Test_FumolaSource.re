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
let ap = (name, payload) => DHExp.fresh(Ap(Forward, ctr(name), payload));
let labelled = (l, e) => DHExp.fresh(TupLabel(DHExp.fresh(Label(l)), e));

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
    /* Other constructors are variant tags, recased on the way out: Hazel's
       `Circle` is Fumola's `#circle`.

       This expectation used to be `#Circle`, on the reasoning that Fumola
       accepts a capitalised tag so the capital added on the way IN could
       survive the way out. It is grammatical and it is not a round trip: a
       value read from Fumola as `#circle` came back as `#Circle`, a
       different tag, silently. The tiles integration found that and put the
       convention in one place, FumolaCase, which both directions now go
       through -- so merging the two integrations changed this answer, and
       this test is where that shows up. See FumolaCase.round_trips for the
       names that still do not survive: a Fumola tag that already begins
       upper-case is lossy in exactly this way. */
    renders("a bare variant", ctr("Circle"), "#circle"),
    renders("an applied variant", ap("Circle", int(3)), "#circle(3)"),
    /* Refused rather than guessed at. */
    refuses("a hole", DHExp.fresh(EmptyHole)),
    refuses(
      "a partly labelled tuple",
      tuple([labelled("x", int(1)), int(2)]),
    ),
    /* The livelit that carries a Hazel value has a four-part model, where the
       others have three or two. Nothing else exercises that match, and a
       model that fails to read back is silent: the livelit falls back to its
       default, so the program quietly loses what was written in it. */
    test_case(
      "fumola_with's model round-trips",
      `Quick,
      () => {
        let encoded =
          Livelit.FumolaWith.model_to_hazel(Livelit.FumolaWith.model_default);
        switch (encoded.term) {
        | Tuple([_, _, _, _]) => ()
        | _ =>
          Alcotest.fail("expected instance, thunk name, program and input")
        };
        switch (Livelit.FumolaWith.model_from_hazel(encoded)) {
        | None => Alcotest.fail("the model did not read back")
        | Some(m) =>
          Alcotest.check(
            Alcotest.string,
            "and survives the round trip",
            Exp.show(encoded),
            Exp.show(Livelit.FumolaWith.model_to_hazel(m)),
          )
        };
      },
    ),
    /* And a livelit that takes no input keeps the shape it had. */
    test_case("the other livelits keep three parts", `Quick, () =>
      switch (
        Livelit.FumolaPutForce.model_to_hazel(
          Livelit.FumolaPutForce.model_default,
        ).
          term
      ) {
      | Tuple([_, _, _]) => ()
      | _ => Alcotest.fail("expected instance, thunk name and program")
      }
    ),
  ],
);
