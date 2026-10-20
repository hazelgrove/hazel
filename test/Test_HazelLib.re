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
  ],
);
