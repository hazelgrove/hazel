open Alcotest;
open Language;

/* Translating a Fumola result into a Hazel value.
 *
 * These tests exist because the Fumola livelit itself can only be exercised
 * in a browser with the wasm runtime loaded, so the step that actually builds
 * Hazel values is otherwise unverified. The JSON here is exactly what the
 * runtime emits; the shapes are covered by tests in the Fumola repo. */

let json = Yojson.Safe.from_string;

/* Translation is type-directed, so a test supplies the type expected of the
   result. With no expectation and nothing resolvable, a constructor is left
   unannotated for Hazel to mark -- the pre-annotation behaviour. */
let no_tools: LivelitCtx.type_tools = {
  resolve_ctr: (~ana as _, _) => None,
  normalize: ty => ty,
};

let unknown = Typ.fresh(Unknown(Internal));

let translate = (~ana=unknown, ~tools=no_tools, src: string) =>
  FumolaValue.exp_of_json(~ana, ~tools, json(src));

/* A sum type declaring Foo and Bar(Int), as
   `type SomeThing = + Foo + Bar(Int)` would. */
let something: Typ.t =
  Typ.fresh(
    Sum([
      ConstructorMap.Variant(
        "Foo",
        ConstructorMap.mk_variant_ann(~ids=[], ()),
        None,
      ),
      ConstructorMap.Variant(
        "Bar",
        ConstructorMap.mk_variant_ann(~ids=[], ()),
        Some(Typ.fresh(Atom(Int))),
      ),
    ]),
  );

/* Resolves Hazel's builtin Option, as the context would. */
let option_tools: LivelitCtx.type_tools = {
  resolve_ctr: (~ana as _, name) =>
    switch (name) {
    | "None" => Some(BuiltinsADT.Option.t)
    | "Some" =>
      Some(
        Typ.fresh(
          Arrow(Typ.fresh(Unknown(Internal)), BuiltinsADT.Option.t),
        ),
      )
    | _ => None
    },
  normalize: ty => ty,
};

/* Resolves constructors the way Statics does: one carrying a payload has an
   arrow from the payload type to the sum. */
let sum_tools: LivelitCtx.type_tools = {
  resolve_ctr: (~ana as _, name) =>
    switch (name) {
    | "Foo" => Some(something)
    | "Bar" => Some(Typ.fresh(Arrow(Typ.fresh(Atom(Int)), something)))
    | _ => None
    },
  normalize: ty => ty,
};

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
    /* Option is no longer special-cased: None and Some resolve through the
       same path as any other constructor, against whatever Option-shaped type
       is expected here. */
    test_case("the Option constructors resolve like any other", `Quick, () =>
      switch (
        translate(
          ~ana=BuiltinsADT.Option.t,
          ~tools=option_tools,
          {|{"tag":"Null","value":null}|},
        )
      ) {
      | Ok({term: Constructor("None", Some(Some(_))), _}) => ()
      | Ok(_) =>
        Alcotest.fail("expected None to carry the type it resolved to")
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
    /* Type-directed translation. A Fumola tag carries no home type, so the
       expected type is the only place that information can come from. */
    test_case("a variant resolves against the expected type", `Quick, () =>
      switch (
        translate(
          ~ana=something,
          ~tools=sum_tools,
          {|{"tag":"Variant","value":{"name":"Bar","value":{"tag":"Int","value":"3"}}}|},
        )
      ) {
      | Ok({term: Ap(Forward, {term: Constructor("Bar", ann), _}, _), _}) =>
        switch (ann) {
        | Some(Some(_)) => ()
        | _ => Alcotest.fail("Bar should carry the type it resolved to")
        }
      | Ok(_) => Alcotest.fail("expected an applied Bar constructor")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* Unresolvable names are left unannotated rather than guessed at, so
       Hazel marks them free -- the honest outcome. */
    test_case("an unresolvable variant is left unannotated", `Quick, () =>
      switch (
        translate({|{"tag":"Variant","value":{"name":"Nope","value":null}}|})
      ) {
      | Ok({term: Constructor("Nope", None), _}) => ()
      | Ok(_) => Alcotest.fail("expected an unannotated constructor")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* The expectation reaches the payload, not just the tag. */
    test_case("the expected type reaches a variant's payload", `Quick, () =>
      switch (
        translate(
          ~ana=something,
          ~tools=sum_tools,
          {|{"tag":"Variant","value":{"name":"Bar","value":{"tag":"Int","value":"3"}}}|},
        )
      ) {
      | Ok({term: Ap(Forward, _, {term: Atom(Int(n)), _}), _}) =>
        Alcotest.check(Alcotest.string, "payload", "3", Bigint.to_string(n))
      | Ok(_) => Alcotest.fail("expected an Int payload")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* A tuple pushes its element types down, so a variant nested in a pair
       still resolves. */
    test_case("expectations reach into tuples", `Quick, () =>
      switch (
        translate(
          ~ana=Typ.fresh(Prod([Typ.fresh(Atom(Int)), something])),
          ~tools=sum_tools,
          {|{"tag":"Tuple","value":[{"tag":"Int","value":"1"},{"tag":"Variant","value":{"name":"Foo","value":null}}]}|},
        )
      ) {
      | Ok({
          term: Tuple([_, {term: Constructor("Foo", Some(Some(_))), _}]),
          _,
        }) =>
        ()
      | Ok(_) => Alcotest.fail("expected Foo to resolve inside the tuple")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* Arity has to agree for an expectation to mean anything; a mismatched
       one is ignored rather than misapplied. */
    test_case("a mismatched tuple arity falls back", `Quick, () =>
      switch (
        translate(
          ~ana=Typ.fresh(Prod([Typ.fresh(Atom(Int))])),
          ~tools=sum_tools,
          {|{"tag":"Tuple","value":[{"tag":"Int","value":"1"},{"tag":"Int","value":"2"}]}|},
        )
      ) {
      | Ok({term: Tuple([_, _]), _}) => ()
      | Ok(_) => Alcotest.fail("expected a two-element tuple")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    fails("an unknown tag", {|{"tag":"Nope","value":null}|}),
    fails("a missing tag", {|{"value":null}|}),
    fails("an unreadable integer", {|{"tag":"Int","value":"twelve"}|}),
  ],
);
