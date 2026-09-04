open Alcotest;
open Language;

/* Translating a Fumola result into a Hazel value.
 *
 * These tests exist because the Fumola livelit itself can only be exercised
 * in a browser with the wasm runtime loaded, so the step that actually builds
 * Hazel values is otherwise unverified. The JSON here is exactly what the
 * runtime emits; the shapes are covered by tests in the Fumola repo. */

let json = Yojson.Safe.from_string;

let translate = (src: string) => FumolaValue.exp_of_json(json(src));

/* Compare against the printed form of the expression, which is enough to
   pin down the shape without depending on ids. */
let shape = (exp: TermBase.Exp.t) =>
  Language.Exp.show(exp) |> Str.global_replace(Str.regexp("[ \n]+"), " ");

let translates = (name, src, expected) =>
  test_case(name, `Quick, () =>
    switch (translate(src)) {
    | Error(message) => Alcotest.fail("translation failed: " ++ message)
    | Ok(exp) =>
      let printed = shape(exp);
      Alcotest.check(
        Alcotest.bool,
        name ++ ": " ++ printed,
        true,
        Str.string_match(
          Str.regexp(".*" ++ Str.quote(expected)),
          printed,
          0,
        ),
      );
    }
  );

let fails = (name, src) =>
  test_case(name, `Quick, () =>
    switch (translate(src)) {
    | Ok(_) => Alcotest.fail("expected the translation to fail")
    | Error(_) => ()
    }
  );

let tests = (
  "FumolaValue",
  [
    translates("an integer", {|{"tag":"Int","value":"3"}|}, "Int 3"),
    translates(
      "a negative integer",
      {|{"tag":"Int","value":"-7"}|},
      "Int -7",
    ),
    translates("a boolean", {|{"tag":"Bool","value":true}|}, "Bool true"),
    translates(
      "a string",
      {|{"tag":"String","value":"hi"}|},
      "String \"hi\"",
    ),
    /* The motivating case: (get(1), get(2)) reaching Hazel as a pair. */
    test_case("a pair of integers becomes a Hazel tuple", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Tuple","value":[{"tag":"Int","value":"10"},{"tag":"Int","value":"20"}]}|},
        )
      ) {
      | Error(m) => Alcotest.fail("translation failed: " ++ m)
      | Ok({term: Tuple([a, b]), _}) =>
        switch (a.term, b.term) {
        | (Atom(Int(x)), Atom(Int(y))) =>
          Alcotest.check(
            Alcotest.string,
            "first",
            "10",
            Bigint.to_string(x),
          );
          Alcotest.check(
            Alcotest.string,
            "second",
            "20",
            Bigint.to_string(y),
          );
        | _ => Alcotest.fail("tuple elements are not integers")
        }
      | Ok(_) => Alcotest.fail("expected a two-element tuple")
      }
    ),
    test_case("tuples nest", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Tuple","value":[{"tag":"Int","value":"1"},{"tag":"Tuple","value":[{"tag":"Int","value":"2"}]}]}|},
        )
      ) {
      | Ok({term: Tuple([_, {term: Tuple([_]), _}]), _}) => ()
      | Ok(_) => Alcotest.fail("expected a nested tuple")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* Fumola's unit is Hazel's empty tuple. */
    test_case("unit becomes the empty tuple", `Quick, () =>
      switch (translate({|{"tag":"Unit","value":null}|})) {
      | Ok({term: Tuple([]), _}) => ()
      | Ok(_) => Alcotest.fail("expected an empty tuple")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* A Hazel record is a tuple of labelled elements. */
    test_case("a record becomes a labelled tuple", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Record","value":{"x":{"tag":"Int","value":"1"},"y":{"tag":"Int","value":"2"}}}|},
        )
      ) {
      | Ok({term: Tuple([x, y]), _}) =>
        switch (x.term, y.term) {
        | (
            TupLabel({term: Label("x"), _}, _),
            TupLabel({term: Label("y"), _}, _),
          ) =>
          ()
        | _ => Alcotest.fail("record fields are not labelled")
        }
      | Ok(_) => Alcotest.fail("expected a two-field record")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    test_case("a variant with a payload is an applied constructor", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Variant","value":{"name":"some","value":{"tag":"Int","value":"3"}}}|},
        )
      ) {
      | Ok({term: Ap(Forward, {term: Constructor("some", _), _}, _), _}) =>
        ()
      | Ok(_) => Alcotest.fail("expected an applied constructor")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    test_case("a variant without a payload is a bare constructor", `Quick, () =>
      switch (
        translate({|{"tag":"Variant","value":{"name":"none","value":null}}|})
      ) {
      | Ok({term: Constructor("none", _), _}) => ()
      | Ok(_) => Alcotest.fail("expected a bare constructor")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* Fumola's option is Hazel's: null is None, ?(x) is Some(x). */
    test_case("null becomes None", `Quick, () =>
      switch (translate({|{"tag":"Null","value":null}|})) {
      | Ok({term: Constructor("None", _), _}) => ()
      | Ok(_) => Alcotest.fail("expected the None constructor")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    test_case("?(x) becomes Some(x)", `Quick, () =>
      switch (
        translate({|{"tag":"Option","value":{"tag":"Int","value":"5"}}|})
      ) {
      | Ok({term: Ap(Forward, {term: Constructor("Some", _), _}, arg), _}) =>
        switch (arg.term) {
        | Atom(Int(n)) =>
          Alcotest.check(
            Alcotest.string,
            "payload",
            "5",
            Bigint.to_string(n),
          )
        | _ => Alcotest.fail("Some payload is not an integer")
        }
      | Ok(_) => Alcotest.fail("expected an applied Some constructor")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* The Option constructors carry the annotations BuiltinsADT.Option gives
       them, so they elaborate as a hand-written None or Some(x) would. */
    test_case("the Option constructors are annotated", `Quick, () =>
      switch (translate({|{"tag":"Null","value":null}|})) {
      | Ok({term: Constructor("None", Some(Some(_))), _}) => ()
      | Ok(_) => Alcotest.fail("expected None to carry its type")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* peek(s) on a written cell answers ?(v), so an option of a structure is
       an ordinary thing to get back. */
    test_case("an option can carry a structure", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Option","value":{"tag":"Tuple","value":[{"tag":"Int","value":"1"},{"tag":"Int","value":"2"}]}}|},
        )
      ) {
      | Ok({
          term:
            Ap(
              Forward,
              {term: Constructor("Some", _), _},
              {term: Tuple([_, _]), _},
            ),
          _,
        }) =>
        ()
      | Ok(_) => Alcotest.fail("expected Some of a pair")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    test_case("options nest", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Option","value":{"tag":"Option","value":{"tag":"Int","value":"1"}}}|},
        )
      ) {
      | Ok({
          term:
            Ap(
              Forward,
              {term: Constructor("Some", _), _},
              {term: Ap(Forward, {term: Constructor("Some", _), _}, _), _},
            ),
          _,
        }) =>
        ()
      | Ok(_) => Alcotest.fail("expected Some(Some(_))")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* Two occurrences must not share an id: ids key the info and measured
       maps, and duplicates cause misattributed decorations. */
    test_case("two Nones do not share an id", `Quick, () =>
      switch (
        translate({|{"tag":"Null","value":null}|}),
        translate({|{"tag":"Null","value":null}|}),
      ) {
      | (Ok(a), Ok(b)) =>
        Alcotest.check(
          Alcotest.bool,
          "distinct ids",
          false,
          Id.compare(Exp.rep_id(a), Exp.rep_id(b)) == 0,
        )
      | _ => Alcotest.fail("translation failed")
      }
    ),
    /* A component with no translation fails the whole value, rather than the
       tuple arriving as though it were complete with a piece dropped. */
    fails(
      "an untranslatable component fails the whole value",
      {|{"tag":"Tuple","value":[{"tag":"Int","value":"1"},{"tag":"Nope","value":null}]}|},
    ),
    fails("an unknown tag", {|{"tag":"Nope","value":null}|}),
    fails("a missing tag", {|{"value":null}|}),
    fails("an unreadable integer", {|{"tag":"Int","value":"twelve"}|}),
  ],
);
