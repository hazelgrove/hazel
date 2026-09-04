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

/* A stand-in runtime. Tests that involve pointers say what each cell holds,
   so translation can dereference exactly as it would against Fumola. */
let store = (cells: list((string, string)), program: string): Yojson.Safe.t =>
  switch (List.assoc_opt(program, cells)) {
  | Some(result) => json(result)
  | None => json({|{"ok":false,"error":"no such cell"}|})
  };

let no_eval = _ => json({|{"ok":false,"error":"no runtime"}|});

let translate =
    (
      ~instance_id=1,
      ~eval=no_eval,
      ~ana=unknown,
      ~tools=no_tools,
      src: string,
    ) =>
  FumolaValue.exp_of_json(~instance_id, ~eval, ~ana, ~tools, json(src));

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
    /* A symbol becomes its text. This is the one way to get a string out of a
       livelit without writing a quote, since Hazel string literals admit no
       escapes and so a livelit's program cannot contain one. */
    test_case("a symbol becomes its text", `Quick, () =>
      switch (
        translate({|{"tag":"Symbol","value":{"tag":"Name","value":"x"}}|})
      ) {
      | Ok({term: Atom(String("x")), _}) => ()
      | Ok(_) => Alcotest.fail("expected the string \"x\"")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    test_case("a numeric symbol becomes its digits", `Quick, () =>
      switch (
        translate({|{"tag":"Symbol","value":{"tag":"Num","value":"7"}}|})
      ) {
      | Ok({term: Atom(String("7")), _}) => ()
      | Ok(_) => Alcotest.fail("expected the string \"7\"")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* Backticks are dropped at every depth, so one convention holds
       throughout rather than being kept on the leaves. */
    test_case("a structured symbol renders without backticks", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Symbol","value":{"tag":"Call","fun":{"tag":"Name","value":"adapton"},"arg":{"tag":"Name","value":"settings"}}}|},
        )
      ) {
      | Ok({term: Atom(String(text)), _}) =>
        Alcotest.check(Alcotest.string, "rendered", "adapton(settings)", text)
      | Ok(_) => Alcotest.fail("expected a string")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    test_case("a dotted symbol renders with a dot", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Symbol","value":{"tag":"Dot","left":{"tag":"Name","value":"a"},"right":{"tag":"Name","value":"b"}}}|},
        )
      ) {
      | Ok({term: Atom(String("a.b")), _}) => ()
      | Ok(_) => Alcotest.fail("expected the string \"a.b\"")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* A symbol nested in a structure is translated like any other value. */
    test_case("symbols translate inside tuples", `Quick, () =>
      switch (
        translate(
          {|{"tag":"Tuple","value":[{"tag":"Symbol","value":{"tag":"Name","value":"a"}},{"tag":"Int","value":"1"}]}|},
        )
      ) {
      | Ok({term: Tuple([{term: Atom(String("a")), _}, _]), _}) => ()
      | Ok(_) => Alcotest.fail("expected a string in the tuple")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    fails(
      "a symbol form with no text yet",
      {|{"tag":"Symbol","value":{"tag":"BinOp","value":null}}|},
    ),
    /* A pointer becomes the livelit that reads it: the same instance, running
       peek(<the name it points at>)!. So a livelit that returns a pointer
       returns a simpler livelit. */
    test_case("a pointer becomes the livelit that reads it", `Quick, () =>
      switch (
        translate(
          ~instance_id=7,
          {|{"tag":"AdaptonPointer","value":{"source":"`counter","symbol":{"tag":"Name","value":"counter"}}}|},
        )
      ) {
      | Ok({
          term:
            Asc(
              {
                term:
                  Ap(
                    Forward,
                    {term: LivelitName("fumola"), _},
                    {term: Tuple([id, program]), _},
                  ),
                _,
              },
              _,
            ),
          _,
        }) =>
        switch (id.term, program.term) {
        | (Atom(Int(n)), Atom(String(text))) =>
          Alcotest.check(
            Alcotest.string,
            "same instance",
            "7",
            Bigint.to_string(n),
          );
          Alcotest.check(
            Alcotest.string,
            "reads the pointer",
            "peek(`counter)!",
            text,
          );
        | _ => Alcotest.fail("model is not (Int, String)")
        }
      | Ok(_) => Alcotest.fail("expected a fumola livelit application")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* A numeric name is a name too. */
    test_case("a numerically named pointer reads the same way", `Quick, () =>
      switch (
        translate(
          ~instance_id=2,
          {|{"tag":"AdaptonPointer","value":{"source":"7","symbol":{"tag":"Num","value":"7"}}}|},
        )
      ) {
      | Ok({
          term:
            Asc(
              {
                term:
                  Ap(
                    Forward,
                    _,
                    {term: Tuple([_, {term: Atom(String(text)), _}]), _},
                  ),
                _,
              },
              _,
            ),
          _,
        }) =>
        Alcotest.check(Alcotest.string, "reads the pointer", "peek(7)!", text)
      | Ok(_) => Alcotest.fail("expected a fumola livelit application")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* Pointers nested in structures translate like any other component, and
       each carries the instance it came from. */
    test_case("pointers translate inside tuples", `Quick, () =>
      switch (
        translate(
          ~instance_id=3,
          {|{"tag":"Tuple","value":[{"tag":"AdaptonPointer","value":{"source":"`a"}},{"tag":"Int","value":"1"}]}|},
        )
      ) {
      | Ok({
          term:
            Tuple([
              {
                term:
                  Asc(
                    {
                      term: Ap(Forward, {term: LivelitName("fumola"), _}, _),
                      _,
                    },
                    _,
                  ),
                _,
              },
              _,
            ]),
          _,
        }) =>
        ()
      | Ok(_) => Alcotest.fail("expected a livelit inside the tuple")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* The ascription comes from following the pointer: the runtime is asked
       what the cell holds, and the type of that answer types the reference. */
    test_case(
      "a pointer is ascribed the type of what it points at", `Quick, () =>
      switch (
        translate(
          ~eval=
            store([("peek(`n)!", {|{"ok":true,"tag":"Int","value":"41"}|})]),
          {|{"tag":"AdaptonPointer","value":{"source":"`n"}}|},
        )
      ) {
      | Ok({term: Asc(_, {term: Atom(Int), _}), _}) => ()
      | Ok(_) => Alcotest.fail("expected an Int ascription")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    test_case(
      "a pointer to a structure is ascribed that structure", `Quick, () =>
      switch (
        translate(
          ~eval=
            store([
              (
                "peek(`p)!",
                {|{"ok":true,"tag":"Tuple","value":[{"tag":"Int","value":"1"},{"tag":"Bool","value":true}]}|},
              ),
            ]),
          {|{"tag":"AdaptonPointer","value":{"source":"`p"}}|},
        )
      ) {
      | Ok({
          term:
            Asc(
              _,
              {
                term: Prod([{term: Atom(Int), _}, {term: Atom(Bool), _}]),
                _,
              },
            ),
          _,
        }) =>
        ()
      | Ok(_) => Alcotest.fail("expected a (Int, Bool) ascription")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* A pointer to a pointer is typed by following the whole chain. */
    test_case("dereferencing follows a chain of pointers", `Quick, () =>
      switch (
        translate(
          ~eval=
            store([
              (
                "peek(`a)!",
                {|{"ok":true,"tag":"AdaptonPointer","value":{"source":"`b"}}|},
              ),
              ("peek(`b)!", {|{"ok":true,"tag":"String","value":"hi"}|}),
            ]),
          {|{"tag":"AdaptonPointer","value":{"source":"`a"}}|},
        )
      ) {
      | Ok({term: Asc(_, {term: Atom(String), _}), _}) => ()
      | Ok(_) => Alcotest.fail("expected a String ascription")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* A cell holding a pointer back to itself must terminate rather than
       dereferencing forever. */
    test_case("a pointer cycle terminates", `Quick, () =>
      switch (
        translate(
          ~eval=
            store([
              (
                "peek(`loop)!",
                {|{"ok":true,"tag":"AdaptonPointer","value":{"source":"`loop"}}|},
              ),
            ]),
          {|{"tag":"AdaptonPointer","value":{"source":"`loop"}}|},
        )
      ) {
      | Ok({term: Asc(_, {term: Unknown(_), _}), _}) => ()
      | Ok(_) => Alcotest.fail("expected Unknown for a cycle")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* A cell that cannot be read tells us nothing about its type, and an
       unreadable one must not fail the whole translation. */
    test_case("an unreadable cell is ascribed Unknown", `Quick, () =>
      switch (
        translate({|{"tag":"AdaptonPointer","value":{"source":"`gone"}}|})
      ) {
      | Ok({term: Asc(_, {term: Unknown(_), _}), _}) => ()
      | Ok(_) => Alcotest.fail("expected an Unknown ascription")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    /* The pointer itself survives the ascription -- the reference is
       preserved, not replaced by the value it points at. */
    test_case("the pointer survives being typed", `Quick, () =>
      switch (
        translate(
          ~instance_id=5,
          ~eval=
            store([("peek(`n)!", {|{"ok":true,"tag":"Int","value":"1"}|})]),
          {|{"tag":"AdaptonPointer","value":{"source":"`n"}}|},
        )
      ) {
      | Ok({
          term:
            Asc(
              {
                term:
                  Ap(
                    Forward,
                    {term: LivelitName("fumola"), _},
                    {
                      term:
                        Tuple([
                          {term: Atom(Int(n)), _},
                          {term: Atom(String(text)), _},
                        ]),
                      _,
                    },
                  ),
                _,
              },
              _,
            ),
          _,
        }) =>
        Alcotest.check(
          Alcotest.string,
          "same instance",
          "5",
          Bigint.to_string(n),
        );
        Alcotest.check(
          Alcotest.string,
          "still reads the pointer",
          "peek(`n)!",
          text,
        );
      | Ok(_) => Alcotest.fail("expected the pointer to survive")
      | Error(m) => Alcotest.fail(m)
      }
    ),
    fails(
      "a pointer with no source text",
      {|{"tag":"AdaptonPointer","value":{"symbol":{"tag":"Name","value":"x"}}}|},
    ),
    fails("an unknown tag", {|{"tag":"Nope","value":null}|}),
    fails("a missing tag", {|{"value":null}|}),
    fails("an unreadable integer", {|{"tag":"Int","value":"twelve"}|}),
  ],
);
