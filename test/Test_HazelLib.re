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

/* A body as the panel puts it in the program today (scriptBody): without a
   last `;`, and trimmed */
let script_body = (text: string): string => {
  let right = Str.global_replace(Str.regexp("[ \t\n\r]+$"), "", text);
  let n = String.length(right);
  String.strip(
    n > 0 && Char.equal(right.[n - 1], ';')
      ? String.sub(right, ~pos=0, ~len=n - 1) : text,
  );
};

/* The program as the panel joins it today, as text (scriptProgram in
   kids-choice-games, dress-up/js/25-scripting.js): each body as
   script_body gives it, and a blank one in its module's place */
let joined = (~common=Panel.common, ~trap=Panel.trap, ()) => {
  let or_else = (default, body) => String.is_empty(body) ? default : body;
  String.concat(
    ~sep="\n",
    [
      Panel.preamble,
      "module Common = {\n"
      ++ or_else("let nothing = ()", script_body(common))
      ++ "\n} in",
      "module TrapRoom = {\n"
      ++ or_else(
           "let on_event = fun (g, e) : (Globals, Event) -> Done",
           script_body(trap),
         )
      ++ "\n} in",
      Panel.tail,
    ],
  );
};

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

let is_ok = (r: Js.Unsafe.any): bool =>
  Js.to_bool(Js.Unsafe.coerce(field(r, "ok")));

/* each error's module, "(program)" for null */
let error_modules = (r: Js.Unsafe.any): list(string) =>
  Array.to_list(Js.to_array(Js.Unsafe.coerce(field(r, "errors"))))
  |> List.map(~f=e => {
       let m = field(e, "module");
       Js.Unsafe.equals(m, Js.null)
         ? "(program)" : Js.to_string(Js.Unsafe.coerce(m));
     });

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
    /* (^ticket(792)) a big Int from JS comes in whole: an int is 32 bits
       under js_of_ocaml, and 4293918720 passed in came in as -1048576; and
       one past a safe integer goes out a BigInt, as compiled code has it */
    test_case(
      "an Int past 2^31 from JS comes in whole",
      `Quick,
      () => {
        let untyped = value("fun x -> x + 1");
        check(
          string,
          "4293918720, untyped",
          "4293918721",
          json(call(untyped, [num(4293918720.)])),
        );
        check(
          string,
          "-4293918720, untyped",
          "-4293918719",
          json(call(untyped, [num(-4293918720.)])),
        );
        check(
          string,
          "2^53 - 2, untyped: an Int, not a Float",
          "9007199254740991",
          json(call(untyped, [num(9007199254740990.)])),
        );
        let typed = value("fun (x : Int) -> x + 1");
        check(
          string,
          "4293918720, as an Int",
          "4293918721",
          json(call(typed, [num(4293918720.)])),
        );
        let big =
          call(
            typed,
            [
              Js.Unsafe.fun_call(
                Js.Unsafe.js_expr("BigInt"),
                [|Js.Unsafe.inject(Js.string("18446744073709551615"))|],
              ),
            ],
          );
        check(
          string,
          "a BigInt in, as an Int, and past 2^53 a BigInt out",
          "bigint 18446744073709551616",
          Js.to_string(Js.typeof(big))
          ++ " "
          ++ Js.to_string(
               Js.Unsafe.fun_call(Js.Unsafe.js_expr("String"), [|big|]),
             ),
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
    /* (^ticket(703)) a JS object's keys need not be in the labeled
       tuple's order: its fields go by label, a list field too */
    test_case(
      "an object's keys in another order than its type's fields",
      `Quick,
      () => {
        let x = value("fun (p : (x = Int, y = [Int])) -> p.x");
        check(
          string,
          "{y: [1], x: 5}, its x",
          "5",
          json(call(x, [as_js("{\"y\":[1],\"x\":5}")])),
        );
        let p = value("fun (p : (x = Int, y = [Int])) -> p");
        check(
          string,
          "{y: [1], x: 5}, back in the type's order",
          "{\"x\":5,\"y\":[1]}",
          json(call(p, [as_js("{\"y\":[1],\"x\":5}")])),
        );
        let aliased =
          value("type P = (x = Int, y = [Int]) in fun (p : P) -> p.x");
        check(
          string,
          "through a type's name",
          "7",
          json(call(aliased, [as_js("{\"y\":[1],\"x\":7}")])),
        );
        let nested =
          value("fun (p : (x = Int, y = [(a = Int, b = Bool)])) -> p.y");
        check(
          string,
          "objects in a list, each in another order",
          "[{\"a\":1,\"b\":true}]",
          json(
            call(nested, [as_js("{\"y\":[{\"b\":true,\"a\":1}],\"x\":2}")]),
          ),
        );
        let two =
          value("fun (m : (n = Int, ps = [Int]), e : Int) -> m.n + e");
        check(
          string,
          "one of several arguments",
          "5",
          json(call(two, [as_js("{\"ps\":[1],\"n\":4}"), num(1.)])),
        );
      },
    ),
    /* (^ticket(736)) a JS array is a tuple where the type wants a tuple, a
       list where it wants a list: by the parameter's declared type, down
       through a constructor's value and a recursive type */
    test_case(
      "an array where a tuple is expected",
      `Quick,
      () => {
        let f =
          value(
            "type S = + Two(Int, Bool) + One(Int) in\n"
            ++ "fun s : S -> case s | Two(m, t) => m + (if t then 100 else 0) | One(n) => n end",
          );
        check(
          string,
          "{tag: Two, value: [3, true]}",
          "103",
          json(call(f, [as_js("{\"tag\":\"Two\",\"value\":[3,true]}")])),
        );
        check(
          string,
          "{tag: One, value: 4}, as before",
          "4",
          json(call(f, [as_js("{\"tag\":\"One\",\"value\":4}")])),
        );
        let g =
          value("fun (m, t) : (Int, Bool) -> m + (if t then 100 else 0)");
        check(
          string,
          "one argument, [3, true]",
          "103",
          json(call(g, [as_js("[3,true]")])),
        );
        check(
          string,
          "two arguments, 3 and true, as before",
          "103",
          json(call(g, [num(3.), as_js("true")])),
        );
        let xs = value("fun xs : [Int] -> xs");
        check(
          string,
          "a list is still a list",
          "[1,2]",
          json(call(xs, [as_js("[1,2]")])),
        );
        let half = value("fun x : Float -> x /. 2.");
        check(
          string,
          "a whole number where a Float is expected",
          "1",
          json(call(half, [num(2.)])),
        );
        let tree = "{\"tag\":\"Flip\",\"value\":[2,{\"tag\":\"Flip\",\"value\":[3,{\"tag\":\"Done\"}]}]}";
        let sum =
          value(
            "type Cmd = + Done + Flip(Int, Cmd) in\n"
            ++ "let sum = fun c : Cmd -> case c | Done => 0 | Flip(l, rest) => l + sum(rest) end in sum",
          );
        check(
          string,
          "a command tree back in from JS, a recursive type",
          "5",
          json(call(sum, [as_js(tree)])),
        );
        let typed =
          value(
            "type Cmd = + Done + Flip(Int, Cmd) in\n"
            ++ "let sum : Cmd -> Int = fun c -> case c | Done => 0 | Flip(l, rest) => l + sum(rest) end in sum",
          );
        check(
          string,
          "the same, its type on the let rather than the parameter",
          "5",
          json(call(typed, [as_js(tree)])),
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
    /* (^ticket(896)) a function typed on its let, called from JS through
       the interpreter with whole numbers where its type says Float: they
       come in as Floats (an Int there stopped the arithmetic) */
    test_case(
      "a function typed on its let takes whole numbers as Floats where its type says Float",
      `Quick,
      () => {
        let f =
          value("let f : (x = Float) -> Float = fun r -> r.x +. 0.5 in f");
        check(
          string,
          "{x: 2}",
          "2.5",
          json(call(f, [as_js("{\"x\": 2}")])),
        );
        let g =
          value(
            "let g : (Float, (y = Float, n = Int)) -> Float = fun (a, b) -> a +. b.y in g",
          );
        check(
          string,
          "3, {y: 4, n: 1}",
          "7",
          json(call(g, [num(3.), as_js("{\"y\": 4, \"n\": 1}")])),
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
        ~f=((what, trap, expected)) => {
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
      )
    ),
    test_case(
      "runProgram: an error carries its module",
      `Quick,
      () => {
        let r = HazelLibCore.run_program(spec(~trap=Panel.error_rule, ()));
        let errors = Js.to_array(Js.Unsafe.coerce(field(r, "errors")));
        check(bool, "there is one", true, Array.length(errors) > 0);
        Array.iter(errors, ~f=e =>
          check(
            string,
            "its module",
            "TrapRoom",
            Js.to_string(Js.Unsafe.coerce(field(e, "module"))),
          )
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
      "analyzeProgram and runProgram say how long they took",
      `Quick,
      () => {
        /* (^ticket(671)) ms read 0, or a little below, in every build:
           the clock was read twice at the end */
        let ms = r => Js.float_of_number(Js.Unsafe.coerce(field(r, "ms")));
        let a = HazelLibCore.analyze_program(spec());
        check(
          bool,
          "analyzeProgram's ms is above 0",
          true,
          Float.(ms(a) > 0.),
        );
        let r = HazelLibCore.run_program(spec());
        check(bool, "runProgram's ms is above 0", true, Float.(ms(r) > 0.));
      },
    ),
    test_case(
      "runProgram where it differs from today's text join, on purpose",
      `Quick,
      () => {
        let both = trap => (
          HazelLibCore.run(joined(~trap, ())),
          HazelLibCore.run_program(spec(~trap, ())),
        );
        /* a last `;`: the join drops it; to runProgram, as to the tile
           editor, the member after it is still to be written */
        let (by_join, by_program) = both(Panel.trap ++ ";");
        check(
          int,
          "a last `;`: the join has no errors",
          0,
          Js.Unsafe.get(field(by_join, "errors"), "length"),
        );
        check(
          bool,
          "a last `;`: runProgram is not ok",
          false,
          is_ok(by_program),
        );
        check(
          string,
          "a last `;`: a hole in TrapRoom",
          "{\"Common\":0,\"TrapRoom\":1}",
          json(field(by_program, "holes")),
        );
        /* a blank room: the join gives it a rule that does nothing;
           runProgram, the empty module, which has no on_event */
        let (by_join, by_program) = both("  \n");
        check(
          string,
          "a blank room: the join's pull",
          "{\"tag\":\"Done\"}",
          pull_left(field(by_join, "value")),
        );
        check(bool, "a blank room: not ok", false, is_ok(by_program));
        check(
          list(string),
          "a blank room: the error is the room's",
          ["TrapRoom"],
          error_modules(by_program),
        );
      },
    ),
    test_case(
      "runProgram as today's text join: a blank Common",
      `Quick,
      () => {
        let by_join =
          HazelLibCore.run(joined(~common="", ~trap=Panel.say_rule, ()));
        let by_program =
          HazelLibCore.run_program(
            spec(~common="", ~trap=Panel.say_rule, ()),
          );
        check(bool, "ok", true, is_ok(by_program));
        check(
          string,
          "the same pull",
          pull_left(field(by_join, "value")),
          pull_left(field(by_program, "value")),
        );
      },
    ),
    test_case(
      "analyzeProgram: a closer left out, or one too many, is incomplete in its module",
      `Quick,
      () =>
      List.iter(
        [
          (
            "a stray } in Common",
            Panel.common_stray_brace,
            Panel.flip2_rule,
            "{\"Common\":1,\"TrapRoom\":0}",
          ),
          (
            "Common missing its last )",
            Panel.common_missing_paren,
            Panel.flip2_rule,
            "{\"Common\":1,\"TrapRoom\":0}",
          ),
          (
            "TrapRoom missing its end",
            Panel.common,
            Panel.trap_missing_end,
            "{\"Common\":0,\"TrapRoom\":1}",
          ),
        ],
        ~f=((what, common, trap, expected)) => {
          let r = HazelLibCore.analyze_program(spec(~common, ~trap, ()));
          check(bool, what ++ ": ok", false, is_ok(r));
          check(
            string,
            what ++ ": incomplete",
            expected,
            json(field(r, "incomplete")),
          );
          check(
            string,
            what ++ ": programIncomplete",
            "0",
            json(field(r, "programIncomplete")),
          );
        },
      )
    ),
    test_case(
      "analyzeProgram: what a page leaves out or gets wrong in the spec",
      `Quick,
      () =>
      List.iter(
        [
          ("no spec", Js.Unsafe.inject(Js.undefined), true),
          ("null", Js.Unsafe.inject(Js.null), true),
          ("modules null", as_js({|{"modules": null}|}), true),
          (
            "a module null, and one whose name is a number",
            as_js({|{"modules": [null, {"name": 1}]}|}),
            false,
          ),
        ],
        ~f=((what, spec, ok)) => {
          let r = HazelLibCore.analyze_program(spec);
          let typeof = k =>
            Js.to_string(Js.typeof(Js.Unsafe.coerce(field(r, k))));
          check(bool, what ++ ": ok", ok, is_ok(r));
          check(
            list(string),
            what ++ ": the fields' types",
            [
              "boolean",
              "object",
              "object",
              "number",
              "object",
              "number",
              "number",
            ],
            List.map(
              [
                "ok",
                "errors",
                "holes",
                "programHoles",
                "incomplete",
                "programIncomplete",
                "ms",
              ],
              ~f=typeof,
            ),
          );
          check(
            bool,
            what ++ ": errors is an array",
            true,
            Js.to_bool(
              Js.Unsafe.fun_call(
                Js.Unsafe.js_expr("Array.isArray"),
                [|field(r, "errors")|],
              ),
            ),
          );
        },
      )
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
    /* hazel-jit through the library: Hazel.compile and Hazel.jit */
    test_case(
      "jit: a function runs compiled, answering as run's",
      `Quick,
      () => {
        let r = HazelLibCore.jit("fun x -> x + 1");
        check(
          bool,
          "compiled",
          true,
          Js.to_bool(Js.Unsafe.coerce(field(r, "compiled"))),
        );
        check(
          string,
          "called with 41",
          "42",
          json(call(field(r, "value"), [num(41.)])),
        );
      },
    ),
    test_case(
      "jit: a labeled tuple of functions, an object of compiled functions",
      `Quick,
      () => {
        let r =
          HazelLibCore.jit(
            "(inc = fun (x : Int) -> x + 1, both = fun (xs : [Int]) -> xs @ xs)",
          );
        check(
          bool,
          "compiled",
          true,
          Js.to_bool(Js.Unsafe.coerce(field(r, "compiled"))),
        );
        let v = field(r, "value");
        check(
          string,
          "inc",
          "8",
          json(call(field(v, "inc"), [num(7.)])),
        );
        check(
          string,
          "both",
          "[1,2,1,2]",
          json(call(field(v, "both"), [as_js("[1,2]")])),
        );
      },
    ),
    test_case(
      "jit: data, as run gives it",
      `Quick,
      () => {
        let src = "(a = [1, 2], b = (3, true), c = \"s\", d = 2.5)";
        check(
          string,
          "the same value",
          json(value(src)),
          json(field(HazelLibCore.jit(src), "value")),
        );
      },
    ),
    test_case(
      "jit: labels where JS's == would mix up fields (0 and false)",
      `Quick,
      () => {
        /* A compiled tuple's labels are found by the value at each index,
           each label used up once found. Found from the last index first
           (Core's List.init calls f that way), 0 took false's label. */
        let src = "(b = false, a = 0, c = 5)";
        check(
          string,
          "a tuple",
          json(value(src)),
          json(field(HazelLibCore.jit(src), "value")),
        );
        let f = "let f : Int -> (a = Int, b = Bool, c = Int) = fun n -> (a = 0, b = false, c = n) in f";
        check(
          string,
          "a compiled function's answer",
          json(call(value(f), [num(5.)])),
          json(call(field(HazelLibCore.jit(f), "value"), [num(5.)])),
        );
      },
    ),
    test_case(
      "jit: converters by declared type give run's answers, and fall back where a value does not fit",
      `Quick,
      () => {
        /* (^report(54)) a function whose types are declared: its argument and answer converted straight between the
           page's values and compiled code's, no Hazel value between */
        let f = {|type Shape = + Circle(Float) + Rect((w = Float, h = Float)) in
let area : Shape -> Float = fun s -> case s | Circle(r) => 3.0 *. r *. r | Rect(d) => d.w *. d.h end in
let total : [Shape] -> (sum = Float, count = Int) = fun shapes -> case shapes | [] => (sum = 0.0, count = 0) | s :: rest => let t = total(rest) in (sum = area(s) +. t.sum, count = t.count + 1) end in
total|};
        let shapes =
          as_js(
            {|[{"tag": "Circle", "value": 1}, {"tag": "Rect", "value": {"w": 2, "h": 3}}]|},
          );
        check(
          string,
          "a list of constructors and records",
          json(call(value(f), [shapes])),
          json(call(field(HazelLibCore.jit(f), "value"), [shapes])),
        );
        /* a tag the sum does not have: the converters give the call to the old way, which answers as run does. The answer
           is stuck, a Hazel term printed whole; run and jit each parse the program, minting their own ids, so the printed
           ids are left out of the comparison */
        let odd = as_js({|[{"tag": "Square", "value": 2}]|});
        let no_ids = (s: string): string =>
          Js.to_string(
            Js.Unsafe.meth_call(
              Js.string(s),
              "replace",
              [|
                Js.Unsafe.js_expr({|/Haz3lcore\.Id\.v\(\\"[^\\]*\\"\)/g|}),
                Js.Unsafe.inject(Js.string("id")),
              |],
            ),
          );
        check(
          string,
          "a tag the sum does not have",
          no_ids(json(call(value(f), [odd]))),
          no_ids(json(call(field(HazelLibCore.jit(f), "value"), [odd]))),
        );
      },
    ),
    test_case(
      "jit: converted-closed: a value handed back crosses with no descent, the page's own object back",
      `Quick,
      () => {
        /* (Matthew, 2026-11-23) a node met again, in a type's memo, was converted with everything below it: the
           converter stops there. A list of records into the compiled identity and out again is the page's own
           array, not a copy; and so again on the next call */
        let same =
          field(
            HazelLibCore.jit(
              "let same : [(x = Int, y = Int)] -> [(x = Int, y = Int)] = fun ps -> ps in same",
            ),
            "value",
          );
        let ps = as_js({|[{"x": 1, "y": 2}, {"x": 3, "y": 4}]|});
        let is = (a, b) =>
          Js.to_bool(
            Js.Unsafe.fun_call(Js.Unsafe.js_expr("Object.is"), [|a, b|]),
          );
        check(
          bool,
          "the page's own array back",
          true,
          is(call(same, [ps]), ps),
        );
        check(bool, "and again", true, is(call(same, [ps]), ps));
      },
    ),
    test_case(
      "jit: a page's changed copy of an answer crosses as the copy; a frozen object crosses",
      `Quick,
      () => {
        /* (^ticket(795)) the memo links a page's object to its compiled value by a hidden property, not enumerable, so
           {...answer} does not carry it into a copy the page then changes, as Gnome world copies World.step's answer;
           and an object that cannot take the link (frozen) still crosses, through a WeakMap */
        let step =
          field(
            HazelLibCore.jit(
              "let step : (x = Int, y = Int) -> (x = Int, y = Int) = fun g -> (x = g.x + 1, y = g.y * 2) in step",
            ),
            "value",
          );
        let a = call(step, [as_js({|{"x": 1, "y": 1}|})]);
        let b =
          Js.Unsafe.fun_call(
            Js.Unsafe.js_expr(
              "(a => { const b = { ...a }; b.y = 100; return b; })",
            ),
            [|a|],
          );
        check(
          string,
          "the copy's answer",
          {|{"x":3,"y":200}|},
          json(call(step, [b])),
        );
        let frozen =
          Js.Unsafe.fun_call(
            Js.Unsafe.js_expr("Object.freeze"),
            [|as_js({|{"x": 5, "y": 5}|})|],
          );
        check(
          string,
          "a frozen object",
          {|{"x":6,"y":10}|},
          json(call(step, [frozen])),
        );
        check(
          string,
          "and again",
          {|{"x":6,"y":10}|},
          json(call(step, [frozen])),
        );
      },
    ),
    test_case(
      "jit: a page's record read by position where the type is unknown is answered as run answers it",
      `Quick,
      () => {
        /* (^ticket(851)) the converters mark a page's record with its labels, as compiled code's tuples are marked, so
           a pattern by position of a type the statics do not know stops for the interpreter, which binds the labeled
           entry itself, rather than reading the component */
        let src = "let f : (x = Int, y = Int) -> Int = fun r -> (fun p -> let (a, b) = p in a - b)(r) in f";
        let r = () => as_js({|{"x": 5, "y": 2}|});
        check(
          string,
          "as run answers it",
          json(call(value(src), [r()])),
          json(call(field(HazelLibCore.jit(src), "value"), [r()])),
        );
      },
    ),
    test_case(
      "jit: a program's functions, its tail a record, come back compiled, one calling another",
      `Quick,
      () => {
        /* (^ticket(847), idea A) the tail (start = M.start, has = M.has) is a record, an object: each of its functions
           made a compiled one with its converters (jit_record), not one that runs on the interpreter */
        let spec =
          as_js(
            {|{"preamble": "type G = (body = [Int], dir = Int, t = Int) in", "modules": [{"name": "M", "text": "let start : Int -> G = fun n -> (body = [n, n + 1], dir = 1, t = 0);\nlet mem : (Int, [Int]) -> Bool = fun (v, xs) -> case xs | [] => false | x :: rest => x == v || mem(v, rest) end;\nlet has : G -> Bool = fun g -> mem(5, g.body)"}], "tail": "(start = M.start, has = M.has)"}|},
          );
        let v = field(HazelLibCore.jit_program(spec), "value");
        let g = call(field(v, "start"), [num(5.)]);
        check(string, "has", "true", json(call(field(v, "has"), [g])));
      },
    ),
    test_case(
      "jitProgramSaved: the program as text, made again by a page without Hazel, gives the same answers",
      `Quick,
      () => {
        /* (^ticket(890)) a page keeps saved and, on its next load, makes the value again from it alone: the compiled
           JavaScript run with its runtime, and each function wrapped by its own converters' source */
        let spec =
          as_js(
            {|{"preamble": "type G = (body = [Int], dir = Int, t = Int) in", "modules": [{"name": "M", "text": "let start : Int -> G = fun n -> (body = [n, n + 1], dir = 1, t = 0);\nlet mem : (Int, [Int]) -> Bool = fun (v, xs) -> case xs | [] => false | x :: rest => x == v || mem(v, rest) end;\nlet has : G -> Bool = fun g -> mem(5, g.body)"}], "tail": "(start = M.start, has = M.has)"}|},
          );
        let r = HazelLibCore.jit_program_saved(spec);
        let saved = field(r, "saved");
        let again =
          Js.Unsafe.fun_call(
            Js.Unsafe.pure_js_expr(
              {|(s => { const R = (0, eval)("(" + s.runtime + ")"), v = new Function("$R", s.js)(R), out = {};
                 for (const k of Object.keys(s.wrap)) out[k] = (0, eval)(s.wrap[k])(v[k], () => { throw new Error("the old way, " + k); });
                 return out; })|},
            ),
            [|saved|],
          );
        check(
          string,
          "the fields wrapped",
          {|["start","has"]|},
          json(
            Js.Unsafe.fun_call(
              Js.Unsafe.pure_js_expr("(o => Object.keys(o))"),
              [|again|],
            ),
          ),
        );
        let v = field(r, "value");
        check(
          string,
          "start, made again",
          json(call(field(v, "start"), [num(5.)])),
          json(call(field(again, "start"), [num(5.)])),
        );
        let g = call(field(again, "start"), [num(5.)]);
        check(
          string,
          "has, made again",
          "true",
          json(call(field(again, "has"), [g])),
        );
        let g4 = call(field(again, "start"), [num(1.)]);
        check(
          string,
          "and false",
          "false",
          json(call(field(again, "has"), [g4])),
        );
      },
    ),
    test_case(
      "jit: under Hazel.development(true), what crosses is frozen; off, it is not",
      `Quick,
      () => {
        /* (^ticket(847), decision 4) the rule for a page, checked in development */
        let frozen = (x: Js.Unsafe.any): bool =>
          Js.to_bool(
            Js.Unsafe.coerce(
              Js.Unsafe.fun_call(
                Js.Unsafe.pure_js_expr("(x => Object.isFrozen(x))"),
                [|x|],
              ),
            ),
          );
        let f =
          field(
            HazelLibCore.jit(
              "let f : ((x = Int, y = Int), [Int]) -> ((x = Int, y = Int), [Int]) = fun (p, xs) -> ((x = p.y, y = p.x), 0 :: xs) in f",
            ),
            "value",
          );
        /* the pair as one array, f([p, xs]), or spread, f(p, xs): both through the converters (only the old way, the
           interpreter's, gives back an answer it has not frozen, so a frozen answer says the call went the fast way) */
        let call_with = (~spread) => {
          let p = as_js({|{"x": 1, "y": 2}|});
          let xs = as_js("[3, 4]");
          (
            p,
            xs,
            spread
              ? call(f, [p, xs])
              : call(f, [Js.Unsafe.inject(Js.array([|p, xs|]))]),
          );
        };
        let _ = HazelLibCore.development(true);
        let (p, xs, r) = call_with(~spread=false);
        let (_, _, r2) = call_with(~spread=true);
        let _ = HazelLibCore.development(false);
        check(
          list(bool),
          "frozen: the record given, the list given, the answer, its record, its list; the answer to a spread call",
          [true, true, true, true, true, true],
          [
            frozen(p),
            frozen(xs),
            frozen(r),
            frozen(Js.Unsafe.get(r, 0)),
            frozen(Js.Unsafe.get(r, 1)),
            frozen(r2),
          ],
        );
        check(string, "the answer", {|[{"x":2,"y":1},[0,3,4]]|}, json(r));
        check(string, "the answer, spread", json(r), json(r2));
        let (p, xs, r) = call_with(~spread=false);
        check(
          list(bool),
          "off: nothing frozen",
          [false, false, false],
          [frozen(p), frozen(xs), frozen(r)],
        );
      },
    ),
    test_case(
      "jit: a call the compiled code cannot finish is the interpreter's",
      `Quick,
      () => {
        let src = "fun (x : Int) -> 10 / x";
        let f = field(HazelLibCore.jit(src), "value");
        check(string, "10 / 5", "2", json(call(f, [num(5.)])));
        check(
          string,
          "10 / 0, as run answers it",
          json(call(value(src), [num(0.)])),
          json(call(f, [num(0.)])),
        );
      },
    ),
    test_case(
      "compile: the program's JS and its runtime, run apart from Hazel",
      `Quick,
      () => {
        let r =
          HazelLibCore.compile("let sq = fun (x : Int) -> x * x in sq(12)");
        check(
          bool,
          "compiled",
          true,
          Js.to_bool(Js.Unsafe.coerce(field(r, "compiled"))),
        );
        let v =
          Js.Unsafe.fun_call(
            Js.Unsafe.pure_js_expr(
              "(function (js, rt) { return new Function('$R', js)(eval('(' + rt + ')')); })",
            ),
            [|field(r, "js"), field(r, "runtime")|],
          );
        check(string, "sq(12)", "144", json(v));
      },
    ),
    test_case(
      "jit: a form hazel-jit does not compile, run by the interpreter",
      `Quick,
      () => {
        let r = HazelLibCore.jit("test 1 == 1 end");
        check(
          bool,
          "compiled",
          false,
          Js.to_bool(Js.Unsafe.coerce(field(r, "compiled"))),
        );
        check(
          string,
          "why",
          "a test",
          Js.to_string(Js.Unsafe.coerce(field(r, "reason"))),
        );
      },
    ),
  ],
);
