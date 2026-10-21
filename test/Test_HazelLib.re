/* Hazel as a JS library (HazelLibCore, window.Hazel): what a page gets
   back from a program, as JS values, and a Hazel function called from JS. */
open Alcotest;
open Js_of_ocaml;

let json = (v: Js.Unsafe.any): string =>
  Js.to_string(Js.Unsafe.meth_call(Js._JSON, "stringify", [|v|]));

let value = (text: string): Js.Unsafe.any =>
  Js.Unsafe.get(HazelLibCore.run(text), "value");

let errors = (text: string): int =>
  Js.Unsafe.get(Js.Unsafe.get(HazelLibCore.run(text), "errors"), "length");

let call = (f: Js.Unsafe.any, args: list(Js.Unsafe.any)): Js.Unsafe.any =>
  Js.Unsafe.fun_call(f, Array.of_list(args));

let num = (n: float) => Js.Unsafe.inject(Js.number_of_float(n));

let as_js = (j: string): Js.Unsafe.any =>
  Js.Unsafe.meth_call(
    Js._JSON,
    "parse",
    [|Js.Unsafe.inject(Js.string(j))|],
  );

let is = (what, expected, text) =>
  test_case(what, `Quick, () =>
    check(string, text, expected, json(value(text)))
  );

/* A program of modules as the Scripting panel gives it: the preamble,
   📚 Common, the trap room, and the tail (Test_ModuleProgram has the
   texts) */
module Panel = Test_ModuleProgram;

let spec = (~common=Panel.common, ~trap=Panel.trap, ~tail=Panel.tail, ()) =>
  HazelLibCore.js_object([
    ("preamble", HazelLibCore.js_string(Panel.preamble)),
    (
      "modules",
      HazelLibCore.js_array([
        HazelLibCore.js_object([
          ("name", HazelLibCore.js_string("Common")),
          ("text", HazelLibCore.js_string(common)),
        ]),
        HazelLibCore.js_object([
          ("name", HazelLibCore.js_string("TrapRoom")),
          ("text", HazelLibCore.js_string(trap)),
        ]),
      ]),
    ),
    ("tail", HazelLibCore.js_string(tail)),
  ]);

/* The program as the panel joins it today, as text (scriptProgram) */
let joined = (~common=Panel.common, ~trap=Panel.trap, ()) =>
  String.concat(
    "\n",
    [
      Panel.preamble,
      "module Common = {\n" ++ common ++ "\n} in",
      "module TrapRoom = {\n" ++ trap ++ "\n} in",
      Panel.tail,
    ],
  );

/* A pull of the left lever, as the game asks a room's rule */
let pull_left = (rules: Js.Unsafe.any): string =>
  json(
    call(
      Js.Unsafe.get(rules, "trap_room"),
      [
        as_js("{\"lever_L\":true,\"lever_R\":false,\"wiring\":\"normal\"}"),
        as_js("{\"tag\":\"Pull\",\"value\":\"L\"}"),
      ],
    ),
  );

let field = (o: Js.Unsafe.any, k: string): Js.Unsafe.any =>
  Js.Unsafe.get(o, k);

let tests = (
  "HazelLib",
  [
    is("a number", "3", "1 + 2"),
    is("a list of booleans", "[true,false]", "[true, false]"),
    is("a string", "\"ab\"", "\"a\" ++ \"b\""),
    is(
      "a labeled tuple, as an object",
      "{\"x\":1,\"y\":\"a\"}",
      "(x = 1, y = \"a\")",
    ),
    is("a tuple, as an array", "[1,true]", "(1, true)"),
    is("a labeled tuple of one field, as an object", "{\"x\":1}", "(x = 1)"),
    is("unit, as null", "null", "()"),
    is(
      "a constructor, applied",
      "{\"tag\":\"B\",\"value\":3}",
      "type T = + A + B(Int) in B(3)",
    ),
    is(
      "a constructor alone",
      "{\"tag\":\"A\"}",
      "type T = + A + B(Int) in A",
    ),
    test_case(
      "errors are reported, and the program still runs",
      `Quick,
      () => {
        check(int, "1 + true has an error", 1, min(1, errors("1 + true")));
        check(int, "1 + 2 has none", 0, errors("1 + 2"));
      },
    ),
    test_case(
      "a function comes back as a JS function",
      `Quick,
      () => {
        let f = value("fun x -> x + 1");
        check(string, "its type", "function", Js.to_string(Js.typeof(f)));
        check(string, "called with 41", "42", json(call(f, [num(41.)])));
      },
    ),
    test_case(
      "several arguments go to Hazel as a tuple",
      `Quick,
      () => {
        let f = value("fun (a, b) -> a * b");
        check(
          string,
          "called with 6 and 7",
          "42",
          json(call(f, [num(6.), num(7.)])),
        );
      },
    ),
    test_case(
      "an object goes to Hazel as a labeled tuple",
      `Quick,
      () => {
        let f = value("fun r : (x = Int, y = Int) -> r.x + r.y");
        check(
          string,
          "called with {x: 1, y: 2}",
          "3",
          json(call(f, [as_js("{\"x\":1,\"y\":2}")])),
        );
      },
    ),
    test_case(
      "a lever rule returns a command tree to interpret",
      `Quick,
      () => {
        let f =
          value(
            "type Cmd = + Done + Flip(Int, Cmd) in\n"
            ++ "type Event = + Pull(Int) in\n"
            ++ "fun e : Event -> case e | Pull(l) => Flip(l, Flip(l + 1, Done)) end",
          );
        check(
          string,
          "Pull(2)",
          "{\"tag\":\"Flip\",\"value\":[2,{\"tag\":\"Flip\",\"value\":[3,{\"tag\":\"Done\"}]}]}",
          json(call(f, [as_js("{\"tag\":\"Pull\",\"value\":2}")])),
        );
      },
    ),
    test_case(
      "a constructor argument, the parameter unascribed",
      `Quick,
      () => {
        let f =
          value(
            "type Cmd = + Done + Flip(Int, Cmd) in\n"
            ++ "type Event = + Pull(Int) in\n"
            ++ "fun e -> case e | Pull(l) => Flip(l, Flip(l + 1, Done)) end",
          );
        check(
          string,
          "Pull(2)",
          "{\"tag\":\"Flip\",\"value\":[2,{\"tag\":\"Flip\",\"value\":[3,{\"tag\":\"Done\"}]}]}",
          json(call(f, [as_js("{\"tag\":\"Pull\",\"value\":2}")])),
        );
      },
    ),
    test_case(
      "a constructor argument with no value, the parameter ascribed",
      `Quick,
      () => {
        let f =
          value(
            "type Event = + Tick + Pull(Int) in\n"
            ++ "fun e : Event -> case e | Tick => 0 | Pull(l) => l end",
          );
        check(
          string,
          "Tick",
          "0",
          json(call(f, [as_js("{\"tag\":\"Tick\"}")])),
        );
        check(
          string,
          "Pull(5)",
          "5",
          json(call(f, [as_js("{\"tag\":\"Pull\",\"value\":5}")])),
        );
      },
    ),
    /* ^ticket(482): the Scripting panel's rules take the game's state and the event as one ascribed tuple,
       fun (g, e) : (Globals, Event) -> ..., and the page calls them with two arguments */
    test_case(
      "a constructor inside an ascribed tuple of arguments, as the Scripting panel calls its rules",
      `Quick,
      () => {
        let f =
          value(
            "type Cmd = + Done + Flip(String, Cmd) in\n"
            ++ "type Event = + Pull(String) in\n"
            ++ "type Globals = (lever_L = Bool, lever_R = Bool, wiring = String) in\n"
            ++ "fun (g, e) : (Globals, Event) -> case e | Pull(s) => Flip(s, Done) end",
          );
        check(
          string,
          "Pull(\"L\")",
          "{\"tag\":\"Flip\",\"value\":[\"L\",{\"tag\":\"Done\"}]}",
          json(
            call(
              f,
              [
                as_js(
                  "{\"lever_L\":true,\"lever_R\":false,\"wiring\":\"normal\"}",
                ),
                as_js("{\"tag\":\"Pull\",\"value\":\"L\"}"),
              ],
            ),
          ),
        );
      },
    ),
    /* ^ticket(520): the panel's modules as one program, built by Hazel from
       their texts rather than joined by the page */
    test_case(
      "runProgram: the panel's four rules, as through today's text join",
      `Quick,
      () =>
      List.iter(
        ((what, trap, expected)) => {
          let joined = HazelLibCore.run(joined(~trap, ()));
          let program = HazelLibCore.run_program(spec(~trap, ()));
          let n = (r, k) => Js.Unsafe.get(field(r, k), "length");
          check(
            bool,
            what ++ ": errors, both ways",
            n(joined, "errors") > 0,
            n(program, "errors") > 0,
          );
          check(
            bool,
            what ++ ": ok",
            n(joined, "errors") == 0,
            Js.to_bool(Js.Unsafe.coerce(field(program, "ok"))),
          );
          let by_join = pull_left(field(joined, "value"));
          check(
            string,
            what ++ ": the joined text's pull",
            expected,
            by_join,
          );
          check(
            string,
            what ++ ": runProgram's pull",
            by_join,
            pull_left(field(program, "value")),
          );
        },
        [
          (
            "the shipped rule",
            Panel.trap,
            "{\"tag\":\"Flip\",\"value\":[\"lever_L\",{\"tag\":\"Done\"}]}",
          ),
          (
            "Common.flip2",
            Panel.flip2_rule,
            "{\"tag\":\"Flip\",\"value\":[\"lever_L\",{\"tag\":\"Flip\",\"value\":[\"lever_R\",{\"tag\":\"Done\"}]}]}",
          ),
          (
            "Say",
            Panel.say_rule,
            "{\"tag\":\"Say\",\"value\":[\"hello from Hazel\",{\"tag\":\"Done\"}]}",
          ),
          (
            "Flip(1, Done), an error",
            Panel.error_rule,
            "{\"tag\":\"Flip\",\"value\":[1,{\"tag\":\"Done\"}]}",
          ),
        ],
      )
    ),
    test_case(
      "runProgram: an error carries its module",
      `Quick,
      () => {
        let r = HazelLibCore.run_program(spec(~trap=Panel.error_rule, ()));
        let errors = Js.to_array(Js.Unsafe.coerce(field(r, "errors")));
        check(bool, "there is one", true, Array.length(errors) > 0);
        Array.iter(
          e =>
            check(
              string,
              "its module",
              "TrapRoom",
              Js.to_string(Js.Unsafe.coerce(field(e, "module"))),
            ),
          errors,
        );
      },
    ),
    test_case(
      "analyzeProgram: a hole is not ok, and is counted in its module",
      `Quick,
      () => {
        let r =
          HazelLibCore.analyze_program(
            spec(
              ~trap="let on_event = fun (g, e) : (Globals, Event) -> ?",
              (),
            ),
          );
        check(
          bool,
          "ok",
          false,
          Js.to_bool(Js.Unsafe.coerce(field(r, "ok"))),
        );
        check(
          string,
          "holes",
          "{\"Common\":0,\"TrapRoom\":1}",
          json(field(r, "holes")),
        );
        check(string, "programHoles", "0", json(field(r, "programHoles")));
        check(string, "errors", "[]", json(field(r, "errors")));
      },
    ),
    test_case(
      "runProgram: the tail can ascribe the rule its type",
      `Quick,
      () => {
        let r =
          HazelLibCore.run_program(
            spec(~trap=Panel.flip2_rule, ~tail=Panel.ascribed_tail, ()),
          );
        check(
          bool,
          "ok",
          true,
          Js.to_bool(Js.Unsafe.coerce(field(r, "ok"))),
        );
        check(
          string,
          "the pull",
          "{\"tag\":\"Flip\",\"value\":[\"lever_L\",{\"tag\":\"Flip\",\"value\":[\"lever_R\",{\"tag\":\"Done\"}]}]}",
          pull_left(field(r, "value")),
        );
      },
    ),
  ],
);
