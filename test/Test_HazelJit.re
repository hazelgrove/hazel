/* hazel-jit (^ticket(694)) agrees with the interpreter: each program is elaborated as the evaluator's tests do, run by the
   interpreter, compiled by HazelJit and run as JS, and the two values compared shape by shape (numbers by value, a float
   within a relative 1e-9; lists, tuples, labels and constructors by structure; a function only as a function). And a term
   with a form hazel-jit does not compile says why, rather than compiling wrongly. (The jit's own tests, for dev: those of
   trial/794-games-aot that need no livelit; every evaluator test is also run through the jit, Test_HazelJit_Agree's
   sweep.) */

open Alcotest;
open Language;
open Js_of_ocaml;
/* (structural comparisons of strings, floats and options throughout: as dev's Test_FastParse) */
open Poly;

let elaborate = Test_Evaluator_Prelude.elaborate;
let parse_exp = Test_Evaluator_Prelude.parse_exp;

/* (the comparison, run_js and agree: Test_HazelJit_Agree, shared with the evaluator tests' sweep, ^ticket(756)) */
include Test_HazelJit_Agree;

let agrees = src => {
  let elab = elaborate(parse_exp(src));
  let expected = Test_Evaluator_Prelude.evaluate(elab);
  let clip = js =>
    String.length(js) > 4000
      ? String.sub(js, ~pos=0, ~len=4000) ++ " ..." : js;
  switch (HazelJit.try_compile(~env=Builtins.env_init, elab)) {
  | Error(reason) => fail("not compiled: " ++ reason)
  | Ok(js_src) =>
    switch (run_js(HazelJit.Runtime.source, js_src)) {
    /* (^ticket(759)) both stop at the same error: the compiled run at an invalid operation, the interpreter's stuck at it */
    | exception (Js_error.Exn(e))
        when
          switch (invalid_of(Js_error.message(e))) {
          | Some(err) => stuck_at(expected, err)
          | None => false
          } =>
      ()
    | exception (Js_error.Exn(e)) =>
      fail(
        "the compiled JS threw: "
        ++ Js_error.message(e)
        ++ "\nthe interpreter: "
        ++ clip(Exp.show(expected))
        ++ "\n"
        ++ clip(js_src),
      )
    | got =>
      switch (agree(expected, got)) {
      | None => ()
      | Some(diff) =>
        fail(
          "differs from the interpreter: "
          ++ diff
          ++ "\nthe compiled JS: "
          ++ clip(js_src),
        )
      }
    }
  };
};
let agreement = (name, src) => test_case(name, `Quick, () => agrees(src));

/* (^ticket(766)) mixed mode: the program rewritten, the functions named compiled, and its answer the interpreter's alone;
   ~fell_back, whether some call went back to the interpreter */
let mixed = (~fell_back=false, name, src, names) =>
  test_case(
    name,
    `Quick,
    () => {
      let elab = elaborate(parse_exp(src));
      let expected = Test_Evaluator_Prelude.evaluate(elab);
      let rewritten = HazelJitMixed.rewrite(elab);
      check(
        list(string),
        "compiled (not: "
        ++ String.concat(
             ~sep="; ",
             List.map(HazelJitMixed.not_compiled^, ~f=((f, why)) =>
               f ++ ", " ++ why
             ),
           )
        ++ ")",
        names,
        HazelJitMixed.compiled_names^,
      );
      let fell_before = HazelJitMixed.fallbacks^;
      let got = Test_Evaluator_Prelude.evaluate(rewritten);
      check(
        bool,
        "the interpreter's answer: "
        ++ Exp.show(expected)
        ++ "\nmixed: "
        ++ Exp.show(got),
        true,
        same_value(expected, got),
      );
      check(
        bool,
        "a call fell back",
        fell_back,
        HazelJitMixed.fallbacks^ > fell_before,
      );
    },
  );

let not_compiled = (name, src, reason) =>
  test_case(name, `Quick, () =>
    switch (
      HazelJit.try_compile(
        ~env=Builtins.env_init,
        elaborate(parse_exp(src)),
      )
    ) {
    | Ok(js) => fail("compiled, but should not be: " ++ js)
    | Error(r) => check(string, "the reason", reason, r)
    }
  );

/* the Petri net rule of ^report(43), its modules written as lets (hazel-jit does not compile modules yet) */

let petri = {|
type Arc = (place = Int, weight = Int) in
type Transition = (name = String, inputs = [Arc], outputs = [Arc]) in
type Net = (marking = [Int], transitions = [Transition], draw = Float) in
type Event = + Fire(Int) + Step in
type Cmd = + Done + Take(Int, Int, Cmd) + Put(Int, Int, Cmd) + Say(String, Cmd) in
let nth = fun (xs, i) : ([Int], Int) ->
  case xs | [] => 0 | x :: rest => if i == 0 then x else nth(rest, i - 1) end in
let len = fun xs : [Int] -> case xs | [] => 0 | _ :: rest => 1 + len(rest) end in
let pick = fun (ts, i) : ([Transition], Int) ->
  case ts | [] => None | t :: rest => if i == 0 then Some(t) else pick(rest, i - 1) end in
let has = fun (m, arcs) : ([Int], [Arc]) ->
  case arcs | [] => true | a :: rest => nth(m, a.place) >= a.weight && has(m, rest) end in
let enabled = fun (m, t) : ([Int], Transition) -> has(m, t.inputs) in
let ready = fun (m, ts, i) : ([Int], [Transition], Int) ->
  case ts | [] => [] | t :: rest =>
    if enabled(m, t) then i :: ready(m, rest, i + 1) else ready(m, rest, i + 1) end in
let takes = fun (arcs, k) : ([Arc], Cmd) ->
  case arcs | [] => k | a :: rest => Take(a.place, a.weight, takes(rest, k)) end in
let puts = fun (arcs, k) : ([Arc], Cmd) ->
  case arcs | [] => k | a :: rest => Put(a.place, a.weight, puts(rest, k)) end in
let fire = fun t : Transition -> Say(t.name, takes(t.inputs, puts(t.outputs, Done))) in
let fire_at = fun (n, i) : (Net, Int) ->
  case pick(n.transitions, i)
  | None => Say("there is no transition " ++ string_of_int(i), Done)
  | Some(t) => if enabled(n.marking, t) then fire(t) else Say(t.name ++ " cannot fire now", Done)
  end in
let on_event = fun (n, e) : (Net, Event) ->
  case e
  | Fire(i) => fire_at(n, i)
  | Step =>
    let r = ready(n.marking, n.transitions, 0) in
    let k = len(r) in
    if k == 0 then Say("deadlock: nothing can fire", Done)
    else fire_at(n, nth(r, int_of_float(n.draw *. float_of_int(k))))
  end in
let a = fun p : Int -> (place = p, weight = 1) in
let net = (marking = [1, 0, 1, 0, 1], transitions = [
  (name = "enter1", inputs = [a(0), a(4)], outputs = [a(1)]),
  (name = "leave1", inputs = [a(1)], outputs = [a(0), a(4)]),
  (name = "enter2", inputs = [a(2), a(4)], outputs = [a(3)]),
  (name = "leave2", inputs = [a(3)], outputs = [a(2), a(4)])], draw = 0.7) in
(on_event(net, Fire(0)), on_event(net, Fire(1)), on_event(net, Step), on_event(net, Fire(9)))
|};

/* each run's reading, as one line of JSON, for ^report(45)'s data */

let rec same = (a: Exp.t, b: Exp.t): option(string) => {
  let differ = what =>
    Some(what ++ ":\n  want " ++ Exp.show(a) ++ "\n  got  " ++ Exp.show(b));
  let rec items = (e: Exp.t): option(list(Exp.t)) =>
    switch (Exp.term_of(e)) {
    | Parens(e) => items(e)
    | ListLit(es) => Some(es)
    | Cons(h, t) => Option.map(items(t), ~f=rest => [h, ...rest])
    | _ => None
    };
  let all = (xs, ys) =>
    List.length(xs) != List.length(ys)
      ? differ("lengths")
      : List.fold2_exn(xs, ys, ~init=None, ~f=(acc, x, y) =>
          acc == None ? same(x, y) : acc
        );
  switch (Exp.term_of(a), Exp.term_of(b)) {
  | (Parens(a), _) => same(a, b)
  | (_, Parens(b)) => same(a, b)
  | (Atom(Int(m)), Atom(Int(n))) =>
    Bigint.equal(m, n) ? None : differ("ints")
  | (Atom(Float(x)), Atom(Float(y))) =>
    Float.abs(x -. y) <= 1e-9
    *. Float.max(1., Float.abs(x))
    || Float.is_nan(x)
    && Float.is_nan(y)
      ? None : differ("floats")
  | (Atom(Bool(x)), Atom(Bool(y))) => x == y ? None : differ("bools")
  | (Atom(String(x)), Atom(String(y))) => x == y ? None : differ("strings")
  | (ListLit(_) | Cons(_), ListLit(_) | Cons(_)) =>
    switch (items(a), items(b)) {
    | (Some(xs), Some(ys)) => all(xs, ys)
    | _ => differ("lists")
    }
  | (Tuple(xs), Tuple(ys)) => all(xs, ys)
  | (TupLabel(l1, x), TupLabel(l2, y)) =>
    switch (Exp.term_of(l1), Exp.term_of(l2)) {
    | (Label(m), Label(n)) when m == n => same(x, y)
    | _ => differ("labels")
    }
  | (Constructor(m, _), Constructor(n, _)) =>
    m == n ? None : differ("constructors")
  /* a function only as a function: what it does is checked by applying it (a handler, below) */
  | (Fun(_) | FixF(_) | Closure(_), Fun(_) | FixF(_) | Closure(_)) => None
  | (
      Ap(Forward, {term: Constructor(m, _), _}, x),
      Ap(Forward, {term: Constructor(n, _), _}, y),
    ) =>
    m == n ? same(x, y) : differ("constructors")
  | _ => differ("forms")
  };
};

/* A program's value, by the interpreter and by its compiled JS carried back (HazelJitValue.of_js, by the compiled
   term's types), the same */
let comes_back = (name, src) =>
  test_case(
    name,
    `Quick,
    () => {
      let elab = elaborate(parse_exp(src));
      let expected = Test_Evaluator_Prelude.evaluate(elab);
      let c = HazelJit.compile_full(~env=Builtins.env_init, elab);
      let got =
        HazelJitValue.of_js(
          HazelJitValue.types_of(c),
          None,
          HazelJitValue.instantiate(c.src),
        );
      switch (same(expected, got)) {
      | None => ()
      | Some(diff) => fail("not the interpreter's value: " ++ diff)
      };
    },
  );

/* A view through the boundary: the definition compiled once (compile_view), the model carried into JS, the answer
   carried back (run_view); the same as the interpreter's answer for the same model */

let diff_cases = [
  (
    "one changed in the middle",
    "let set = fun (xs, k, p) -> case xs | [] => [] | x :: rest => if k == 0 then p :: rest else x :: set(rest, k - 1, p) end in let xs = [1, 2, 3, 4, 5] in (xs, set(xs, 2, 30))",
    Some({|{"l":[{"k":2},{"e":{"n":30}}]}|}),
  ),
  (
    "one put at the head",
    "let xs = [1, 2, 3] in (xs, 0 :: xs)",
    Some({|{"l":[{"i":0}]}|}),
  ),
  (
    "the head dropped",
    "let xs = [1, 2, 3] in (xs, case xs | _ :: r => r | [] => [] end)",
    Some({|{"l":[{"x":1}]}|}),
  ),
  ("one put at the end", "let xs = [1, 2, 3] in (xs, xs @ [4])", None),
  (
    "cut after two",
    "let xs = [1, 2, 3] in (xs, case xs | a :: b :: _ => [a, b] | _ => [] end)",
    Some({|{"l":[{"k":2},{"cut":1}]}|}),
  ),
  ("emptied", "let xs = [1, 2, 3] in (xs, [])", Some({|{"n":null}|})),
  ("nothing changed", "let xs = [1, 2] in (xs, xs)", Some("null")),
  (
    "a constructor's argument",
    "let a = (Some(1), 2) in (a, (Some(5), 2))",
    Some({|{"t":[[0,{"c":{"n":5}}]]}|}),
  ),
  (
    "a labeled field set, the other kept",
    "let p = (x = 1, y = [1, 2]) in (p, (x = 2, y = p.y))",
    Some({|{"t":[[0,{"n":2}]]}|}),
  ),
  (
    "a float in a list",
    "let xs = [1.5, 2.0] in (xs, case xs | a :: _ => [a, 3.0] | _ => [] end)",
    None,
  ),
];
let diff_case = ((name, src, want)) =>
  test_case(
    "diff: " ++ name,
    `Quick,
    () => {
      let c =
        HazelJit.compile_full(
          ~env=Builtins.env_init,
          elaborate(parse_exp(src)),
        );
      let pair = HazelJitValue.instantiate(c.src);
      let types = HazelJitValue.types_of(c);
      let old_js = Js.Unsafe.get(pair, 0);
      let new_js = Js.Unsafe.get(pair, 1);
      let d = HazelJitValue.diff(old_js, new_js);
      let shown =
        Js.to_string(
          Js.Unsafe.fun_call(
            Js.Unsafe.pure_js_expr("JSON.stringify"),
            [|d|],
          ): Js.t(Js.js_string),
        );
      switch (want) {
      | Some(w) when w != shown =>
        fail("the diff is " ++ shown ++ ", not " ++ w)
      | _ => ()
      };
      let patched =
        HazelJitValue.patch(
          types,
          None,
          HazelJitValue.of_js(types, None, old_js),
          d,
        );
      switch (same(HazelJitValue.of_js(types, None, new_js), patched)) {
      | None => ()
      | Some(why) => fail("diff " ++ shown ++ ": " ++ why)
      };
    },
  );

/* (^ticket(709), slow) The way back as a diff. A list of n points, (x = i, y = 2i), its middle one replaced by compiled
   Hazel (a functional set: a new cell for each element before it, the tail after it the same JS object). The new list
   carried back whole (of_js), and as a diff (HazelJitValue.diff) applied to the last value (patch); the two must agree,
   and the patched list must keep the old list's own elements wherever nothing changed. */

let big_int_tests = [
  agreement("an Int past 2^53", "9007199254740991 + 2"),
  agreement(
    "a factorial past 2^53",
    "let fact = fun n -> if n == 0 then 1 else n * fact(n - 1) in fact(25)",
  ),
  agreement("a big Int divided, toward zero", "100000000000000000000 / 7"),
  agreement("a negative one, toward zero", "(0 - 100000000000000000001) / 2"),
  agreement("back under 2^53", "(9007199254740991 + 10) - 20"),
  agreement("a power", "3 ** 40"),
  agreement("compared", "9007199254740993 > 9007199254740992"),
  agreement("as a string", "string_of_int(2 ** 70)"),
  agreement("as a float", "float_of_int(2 ** 60)"),
  test_case("dividing by zero stops the compiled run", `Quick, () =>
    switch (
      HazelJit.try_compile(
        ~env=Builtins.env_init,
        elaborate(parse_exp("let z = 0 in 7 / z")),
      )
    ) {
    | Error(why) => fail("not compiled: " ++ why)
    | Ok(js) =>
      switch (run_js(HazelJit.Runtime.source, js)) {
      | _ => fail("ran on past a division by zero")
      | exception (Js_error.Exn(e)) =>
        check(
          string,
          "the error",
          "hazel-jit: invalid operation: DivideByZero",
          Js_error.message(e),
        )
      }
    }
  ),
];

/* (^ticket(759)) an operation the interpreter stops at with an error: the compiled run stops at the same one */
let invalid_tests = [
  agreement("an Int divided by zero", "let z = 0 in 7 / z"),
  /* (int_mod by zero: the interpreter itself raises InvalidBuiltin("mod"), ^ticket(764)) */
  agreement("a negative Int power", "2 ** (0 - 1)"),
  agreement("int_of_string, not a number", "int_of_string(\"seven\")"),
  agreement("a Float divided by zero is infinity, not an error", "7. /. 0."),
];

/* (^ticket(761)) the string builtins the sweep found missing, each against the interpreter's */
let string_builtin_tests = [
  agreement(
    "string_match",
    "(string_match(\"b+\", \"abbbc\"), string_match(\"^z\", \"abc\"))",
  ),
  agreement(
    "string_replace, a $ kept",
    "string_replace(\"o\", \"foo boo\", \"$0\")",
  ),
  agreement(
    "string_search",
    "(string_search(\"a\", \"banana\", 2), string_search(\"z\", \"banana\", 0))",
  ),
  agreement(
    "string_search, by graphemes",
    "string_search(\"c\", \"a👍🏽bc\", 0)",
  ),
  agreement("string_join", "string_join(\", \", [\"a\", \"b\", \"c\"])"),
  agreement("string_escaped", "string_escaped(\"\\hello é\")"),
  agreement(
    "string_capitalize and string_uncapitalize",
    "(string_capitalize(\"élan\"), string_uncapitalize(\"ÉLAN\"))",
  ),
  agreement(
    "string_compare",
    "(string_compare(\"a\", \"b\"), string_compare(\"b\", \"a\"), string_compare(\"ab\", \"ab\"), string_compare(\"ab\", \"a\"))",
  ),
  agreement(
    "int_compare and invert_ord",
    "(int_compare(1, 2), int_compare(5, 5), invert_ord(int_compare(1, 2)))",
  ),
  agreement(
    "float_compare, nan below every float",
    "(float_compare(nan, 1.), float_compare(nan, nan), float_compare(2., 1.))",
  ),
];

/* (^ticket(761)) partial application, f(a, _): one deferred, several, and one handed to List.map */
let partial_tests = [
  agreement(
    "a partial application",
    "let add = fun (a, b) -> a + b in let inc = add(1, _) in inc(41)",
  ),
  agreement(
    "two deferred, filled in order",
    "let f = fun (a, b, c) -> a * 100 + b * 10 + c in let g = f(_, 2, _) in g(1, 3)",
  ),
  /* (its own map: List is no module in the tests' environment, where List.map read as a constructor's projection) */
  agreement(
    "a partial application handed to a map",
    "let map = fun (f, xs) -> case xs | [] => [] | x :: rest => f(x) :: map(f, rest) end in let add = fun (a, b) -> a + b in map(add(10, _), [1, 2, 3])",
  ),
];

/* (^ticket(761)) recursion over more than one name, a tuple of functions */
let mutual_tests = [
  agreement(
    "even and odd",
    "let (even, odd) = (fun n -> if n == 0 then true else odd(n - 1), fun n -> if n == 0 then false else even(n - 1)) in (even(10), odd(7), even(3))",
  ),
  agreement(
    "even and odd, the names' types declared",
    "let (even : Int -> Bool, odd : Int -> Bool) = (fun n -> if n == 0 then true else odd(n - 1), fun n -> if n == 0 then false else even(n - 1)) in even(4)",
  ),
  agreement(
    "three in a cycle, closing over an outer name",
    "let t = 100 in let (a, b, c) = (fun n -> if n > 8 then t + n else b(n + 1), fun n -> c(n + 2), fun n -> a(n * 2)) in a(1)",
  ),
  agreement(
    "the fixpoint written out",
    "let (even, odd) = (fix (even, odd) -> (fun n -> if n == 0 then true else odd(n - 1), fun n -> if n == 0 then false else even(n - 1))) in even(1)",
  ),
  /* (^ticket(761)) a part that is not a function, naming none of the names, is assigned in order with the others */
  agreement(
    "the fixpoint written out, with a part that is not a function",
    "fix (a, b) -> (1, fun n -> a + n)",
  ),
  not_compiled(
    "a part that is not a function and names one of them stays with the interpreter",
    "fix (a, b) -> (b(1), fun n -> n)",
    "a recursive definition of more than one name whose part that is not a function names one of them",
  ),
];

/* (^ticket(763)) a compiled term run with a step budget: its runtime's fuel set for this run */
let run_js_limited = (runtime: string, src: string, fuel: int): Js.Unsafe.any =>
  Js.Unsafe.fun_call(
    Js.Unsafe.pure_js_expr(
      "(function (rt, src, fuel) { const R = eval('(' + rt + ')'); R.limit(fuel, null); return new Function('$R', src)(R); })",
    ),
    [|
      Js.Unsafe.inject(Js.string(runtime)),
      Js.Unsafe.inject(Js.string(src)),
      Js.Unsafe.inject(fuel),
    |],
  );
let fib_src = n =>
  "let fib = fun n -> if n < 2 then n else fib(n - 1) + fib(n - 2) in fib("
  ++ string_of_int(n)
  ++ ")";
let compiled_src = src =>
  switch (
    HazelJit.try_compile(~env=Builtins.env_init, elaborate(parse_exp(src)))
  ) {
  | Ok(js) => js
  | Error(why) => fail("not compiled: " ++ why)
  };
let step_limit_tests = [
  test_case("a long run stops at its step limit", `Quick, () =>
    switch (
      run_js_limited(
        HazelJit.Runtime.source,
        compiled_src(fib_src(25)),
        1000,
      )
    ) {
    | _ => fail("ran past 1000 calls")
    | exception (Js_error.Exn(e)) =>
      check(
        string,
        "the error",
        "hazel-jit: step limit (the run's calls)",
        Js_error.message(e),
      )
    }
  ),
  test_case("under its step limit, a run goes to the end", `Quick, () =>
    check(
      string,
      "fib(15)",
      "610",
      Js.to_string(
        Js.Unsafe.meth_call(
          run_js_limited(
            HazelJit.Runtime.source,
            compiled_src(fib_src(15)),
            100000,
          ),
          "toString",
          [||],
        ),
      ),
    )
  ),
  test_case("with no budget set, no limit", `Quick, () =>
    check(
      string,
      "fib(20)",
      "6765",
      Js.to_string(
        Js.Unsafe.meth_call(
          run_js(HazelJit.Runtime.source, compiled_src(fib_src(20))),
          "toString",
          [||],
        ),
      ),
    )
  ),
];

/* (^ticket(761)) undefined compiled: List.head's empty case, say; a run that reaches it stops, for the interpreter */
let head_src = "let head = fun xs -> case xs | [] => undefined | x :: _ => x end in ";
let undefined_tests = [
  agreement("undefined, not reached", head_src ++ "head([4, 5]) + 1"),
  test_case(
    "undefined, reached: the run stops for the interpreter", `Quick, () =>
    switch (
      run_js(HazelJit.Runtime.source, compiled_src(head_src ++ "head([])"))
    ) {
    | _ => fail("ran past undefined")
    | exception (Js_error.Exn(e)) =>
      check(
        string,
        "the error",
        "hazel-jit: indeterminate: undefined",
        Js_error.message(e),
      )
    }
  ),
];

/* (^ticket(761)) tuple extension and the label builtins, as the evaluator's own tests have them; and x |> f */
/* (^ticket(766)) mixed mode measured: the interpreter alone against mixed, the same answer, each timed once after a
   first run (which also warms the JIT of the JS engine), printed for ^report(54) */
let petri_typed = {|
type Arc = (place = Int, weight = Int) in
type Transition = (name = String, inputs = [Arc], outputs = [Arc]) in
type Net = (marking = [Int], transitions = [Transition], draw = Float) in
type Event = + Fire(Int) + Step in
type Cmd = + Done + Take(Int, Int, Cmd) + Put(Int, Int, Cmd) + Say(String, Cmd) in
type Picked = + NoTransition + Found(Transition) in
let nth : ([Int], Int) -> Int = fun (xs, i) ->
  case xs | [] => 0 | x :: rest => if i == 0 then x else nth(rest, i - 1) end in
let len : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: rest => 1 + len(rest) end in
let pick : ([Transition], Int) -> Picked = fun (ts, i) ->
  case ts | [] => NoTransition | t :: rest => if i == 0 then Found(t) else pick(rest, i - 1) end in
let has : ([Int], [Arc]) -> Bool = fun (m, arcs) ->
  case arcs | [] => true | a :: rest => nth(m, a.place) >= a.weight && has(m, rest) end in
let enabled : ([Int], Transition) -> Bool = fun (m, t) -> has(m, t.inputs) in
let ready : ([Int], [Transition], Int) -> [Int] = fun (m, ts, i) ->
  case ts | [] => [] | t :: rest =>
    if enabled(m, t) then i :: ready(m, rest, i + 1) else ready(m, rest, i + 1) end in
let takes : ([Arc], Cmd) -> Cmd = fun (arcs, k) ->
  case arcs | [] => k | a :: rest => Take(a.place, a.weight, takes(rest, k)) end in
let puts : ([Arc], Cmd) -> Cmd = fun (arcs, k) ->
  case arcs | [] => k | a :: rest => Put(a.place, a.weight, puts(rest, k)) end in
let fire : Transition -> Cmd = fun t -> Say(t.name, takes(t.inputs, puts(t.outputs, Done))) in
let fire_at : (Net, Int) -> Cmd = fun (n, i) ->
  case pick(n.transitions, i)
  | NoTransition => Say("there is no transition " ++ string_of_int(i), Done)
  | Found(t) => if enabled(n.marking, t) then fire(t) else Say(t.name ++ " cannot fire now", Done)
  end in
let on_event : (Net, Event) -> Cmd = fun (n, e) ->
  case e
  | Fire(i) => fire_at(n, i)
  | Step =>
    let r = ready(n.marking, n.transitions, 0) in
    let k = len(r) in
    if k == 0 then Say("deadlock: nothing can fire", Done)
    else fire_at(n, nth(r, int_of_float(n.draw *. float_of_int(k))))
  end in
let a : Int -> Arc = fun p -> (place = p, weight = 1) in
let net = (marking = [1, 0, 1, 0, 1], transitions = [
  (name = "enter1", inputs = [a(0), a(4)], outputs = [a(1)]),
  (name = "leave1", inputs = [a(1)], outputs = [a(0), a(4)]),
  (name = "enter2", inputs = [a(2), a(4)], outputs = [a(3)]),
  (name = "leave2", inputs = [a(3)], outputs = [a(2), a(4)])], draw = 0.7) in
let events = fun (k, acc) -> if k == 0 then acc else events(k - 1, on_event(net, Step) :: on_event(net, Fire(k)) :: acc) in
events(50, [])
|};

let mixed_measured = (~parse=parse_exp, name, src) =>
  test_case(
    "mixed measured: " ++ name,
    `Quick,
    () => {
      let elab = elaborate(parse(src));
      let time = f => {
        let t0 = HazelJitValue.now();
        let v = f();
        (v, HazelJitValue.now() -. t0);
      };
      let _ = Test_Evaluator_Prelude.evaluate(elab);
      let (expected, interp_ms) =
        time(() => Test_Evaluator_Prelude.evaluate(elab));
      let (rewritten, rewrite_ms) = time(() => HazelJitMixed.rewrite(elab));
      let names = HazelJitMixed.compiled_names^;
      let _ = Test_Evaluator_Prelude.evaluate(rewritten);
      let calls0 = HazelJitMixed.calls^
      and fell0 = HazelJitMixed.fallbacks^;
      let (got, mixed_ms) =
        time(() => Test_Evaluator_Prelude.evaluate(rewritten));
      /* (printed at the end of the run: a passing test's own output is kept in its log file) */
      let line =
        Printf.sprintf(
          "hazel-jit mixed measured, %s: the interpreter %.1f ms, mixed %.1f ms (its rewrite and compile %.1f ms); %d functions compiled (%s); %d calls compiled, %d fell back\n",
          name,
          interp_ms,
          mixed_ms,
          rewrite_ms,
          List.length(names),
          String.concat(~sep=" ", names),
          HazelJitMixed.calls^ - calls0,
          HazelJitMixed.fallbacks^ - fell0,
        );
      /* (ticket 768: the astrology rules' answer parted in mixed mode, CI run 44320263066, and the whole terms in the
         message were too long for the log to keep their start) where the two shown terms first differ, printed after the
         run; none, when they show the same and part only beyond what is shown */
      let same = same_value(expected, got);
      let parted =
        if (same) {
          "";
        } else {
          let (se, sg) = (Exp.show(expected), Exp.show(got));
          let n = min(String.length(se), String.length(sg));
          let i = ref(0);
          while (i^ < n && se.[i^] == sg.[i^]) {
            incr(i);
          };
          let flat = s => String.map(s, ~f=c => c == '\n' ? ' ' : c);
          let around = s => {
            let a = max(0, i^ - 400);
            flat(
              String.sub(s, ~pos=a, ~len=min(String.length(s) - a, 800)),
            );
          };
          i^ >= n && String.length(se) == String.length(sg)
            ? Printf.sprintf(
                "hazel-jit mixed parted, %s: the two answers show the same (%d characters), so they part only beyond what is shown\n",
                name,
                n,
              )
            : Printf.sprintf(
                "hazel-jit mixed parted, %s: the shown answers first differ at character %d of %d and %d\n  the interpreter: %s\n  mixed: %s\n",
                name,
                i^,
                String.length(se),
                String.length(sg),
                around(se),
                around(sg),
              );
        };
      /* (and on the trail CI prints after the tests: dune cut the middle of the test output, where this was printed,
         CI run 44337523695) */
      if (!same) {
        trail(line ++ parted);
      };
      at_exit(() => {
        print_string(line ++ parted);
        Stdlib.flush(Stdlib.stdout);
      });
      check(
        bool,
        "mixed mode's answer is the interpreter's (where they part: printed after the run, and on the trail)",
        true,
        same,
      );
    },
  );

let mixed_measured_tests = [
  mixed_measured(
    "fib(16), all compiled",
    "let fib : Int -> Int = fun n -> if n < 2 then n else fib(n - 1) + fib(n - 2) in fib(16)",
  ),
  mixed_measured(
    "500 calls across, from an interpreted loop",
    "let sq : Int -> Int = fun x -> x * x in let loop = fun (i, acc) -> if i == 0 then acc else loop(i - 1, acc + sq(i)) in loop(500, 0)",
  ),
  mixed_measured(
    "the Petri net rule, whole types declared, 100 events",
    petri_typed,
  ),
];

let mixed_tests = [
  mixed(
    "mixed: a recursive function, its type declared",
    "let fact : Int -> Int = fun n -> if n == 0 then 1 else n * fact(n - 1) in fact(10)",
    ["fact"],
  ),
  mixed(
    "mixed: a function calling one before it",
    "let sq : Int -> Int = fun x -> x * x in let sum_sq : (Int, Int) -> Int = fun (a, b) -> sq(a) + sq(b) in sum_sq(3, 4)",
    ["sq", "sum_sq"],
  ),
  mixed(
    "mixed: a hole elsewhere in the program, the function still compiled",
    "let double : Int -> Int = fun x -> x * 2 in (double(21), ?)",
    ["double"],
  ),
  mixed(
    ~fell_back=true,
    "mixed: a hole in the argument, that call the interpreter's",
    "let inc : Int -> Int = fun x -> x + 1 in inc(?)",
    ["inc"],
  ),
  mixed(
    ~fell_back=true,
    "mixed: divide by zero, that call the interpreter's, stuck at the error",
    "let d : Int -> Int = fun x -> 10 / x in (d(5), d(0))",
    ["d"],
  ),
  mixed(
    "mixed: lists and a constructor across",
    "type Shape = + Circle(Float) + Square(Float) in let areas : [Shape] -> [Float] = fun shapes -> case shapes | [] => [] | Circle(r) :: rest => (3.0 *. r *. r) :: areas(rest) | Square(s) :: rest => (s *. s) :: areas(rest) end in areas([Circle(1.0), Square(2.0)])",
    ["areas"],
  ),
  mixed(
    "mixed: a module's members, one calling another",
    "module M = { let sq : Int -> Int = fun x -> x * x; let cube : Int -> Int = fun x -> x * sq(x) } in M.cube(3) + M.sq(4)",
    ["sq", "cube"],
  ),
  mixed(
    "mixed: a module's member calling a function before the module",
    "let inc : Int -> Int = fun x -> x + 1 in module M = { let twice : Int -> Int = fun x -> inc(inc(x)) } in M.twice(40)",
    ["inc", "twice"],
  ),
  mixed(
    "mixed: a Float that is whole comes back a Float",
    "let half : Float -> Float = fun x -> x /. 2.0 in half(4.0)",
    ["half"],
  ),
];

let label_tests = [
  agreement(
    "a reverse application, and one to a constructor",
    "let inc = fun x -> x + 1 in (41 |> inc, 3 |> Some)",
  ),
  /* (^ticket(765)) labels a JS array already has a use for: kept under another key (HazelJit.label_key) */
  agreement(
    "a label named length",
    "let r = (length = 3, width = 4) in (r.length * r.width, r)",
  ),
  agreement(
    "a label named length, in a pattern",
    "let (length = n, width = w) = (length = 5, width = 2) in n * w",
  ),
  agreement(
    "a module's member named length",
    "module M = { let length = fun xs -> case xs | [] => 0 | _ :: r => 1 + length(r) end } in M.length([1, 2, 3])",
  ),
  agreement("a tuple extended", "(a=1, b=2) ... (c=3)"),
  agreement("an unlabeled tuple extended", "(1, 2) ... (3, 4)"),
  agreement("an extension replacing a label", "(a=0, b=2) ... (a=1, 3, c=4)"),
  agreement(
    "project_labels",
    "project_labels((a=1, b=2, c=3), `c`, `a`, `c`)",
  ),
  agreement("select_labels", "select_labels((a=1, b=2, c=3), `a`, `b`)"),
  agreement("omit_labels", "omit_labels((a=1, b=2, c=3), `a`, `b`)"),
  /* (^ticket(761)) the sweep's last limits: an explicitly unlabeled entry, from_lvs, a module member that is an
     expression */
  agreement("an explicitly unlabeled entry", "let (_=x) = (_=1) in x"),
  agreement("an explicitly unlabeled entry, kept", "(_=1)"),
  agreement(
    "project_labels to a tuple of one",
    "project_labels((a=1, b=2), `a`)",
  ),
  agreement(
    "from_lvs",
    {|from_lvs([(label="col1", value=3), (label="col2", value=true)])|},
  ),
  agreement("from_lvs of one", {|from_lvs([(label="col", value=3)])|}),
  agreement("from_lvs of none", "from_lvs([])"),
  /* (^ticket(761), found by the slides' check) */
  agreement("to_lvs", "to_lvs((a=1, b=true))"),
  agreement(
    "to_lvs, then from_lvs",
    "from_lvs(to_lvs((col1=3, col2=\"x\")))",
  ),
  agreement(
    "an operator as a builtin",
    "(int_plus(1, 2), float_times(1.5, 2.), string_concat(\"a\", \"b\"), int_lt(1, 2))",
  ),
  agreement(
    "an operator as a builtin, passed along",
    "let apply = fun (f, a, b) -> f(a, b) in apply(int_times, 6, 7)",
  ),
  agreement(
    "a module's member that is an expression",
    "{ 1 + 1; let x = 2 }",
  ),
  agreement(
    "a module's member that is an expression, between two others",
    "module M = { let x = 1; x + 1; let y = x + 2 } in M.y",
  ),
];

/* (^ticket(761), found 11-16) strings beyond ASCII, which the evaluator's tests never use: lengths and offsets in
   graphemes, as HazelString counts them (an emoji, an e with a combining accent, a flag of two code points) */
let unicode_string_tests = [
  agreement(
    "string_length in graphemes",
    "(string_length(\"\u{1F44B}\"), string_length(\"e\u{301}\"), string_length(\"h\u{e9}llo\"), string_length(\"\u{1F1FA}\u{1F1F8}\"))",
  ),
  agreement(
    "string_sub in graphemes",
    "string_sub(\"\u{1F44B}h\u{e9}llo\", 1, 3)",
  ),
  agreement("string_sub out of range", "string_sub(\"abc\", 2, 5)"),
  agreement(
    "string_split, a separator and none",
    "(string_split(\",\", \"a,b,,c\"), string_split(\"\", \"\u{1F44B}e\u{301}x\"))",
  ),
];

/* (^ticket(756)) the slides' own programs, beside the evaluator's tests: every program under hazel-programs (outside
   mega/), parsed as the evaluator's tests parse; one with a static error stays with the interpreter (^ticket(760)), as
   does one hazel-jit does not compile; the rest are run both ways and their values compared as the sweep compares
   them. Printed for ^report(54); a difference, or compiled JS that throws where the interpreter has a value, fails. */

let tests = (
  "HazelJit",
  invalid_tests
  @ big_int_tests
  @ mutual_tests
  @ partial_tests
  @ string_builtin_tests
  @ step_limit_tests
  @ undefined_tests
  @ label_tests
  @ unicode_string_tests
  @ mixed_tests
  @ mixed_measured_tests
  @ List.map(diff_cases, ~f=diff_case)
  @ [
    agreement("arithmetic", "1 + 2 * 3 - 4 / 2"),
    agreement("floats", "1.5 *. 2. +. 0.25"),
    agreement(
      "a projection through a list, a table's column",
      "let t = [(name=\"a\", acne=true), (name=\"b\", acne=false)] in t.acne",
    ),
    agreement(
      "a projection of a tuple and of a list together",
      "let r = (a=1, rows=[(a=2), (a=3)]) in (r.a, r.rows.a)",
    ),
    agreement("a function", "let f = fun x -> x * 2 in f(21)"),
    agreement(
      "recursion",
      "let fact = fun n -> if n == 0 then 1 else n * fact(n - 1) in fact(10)",
    ),
    agreement("a tuple pattern", "let (a, b) = (3, 4) in a * b"),
    agreement(
      "labels and projection",
      "let p = (x = 3, y = 4) in (p.x * 10 + p.y, p)",
    ),
    agreement(
      "a list, folded by a match",
      "let sum = fun xs -> case xs | [] => 0 | x :: rest => x + sum(rest) end in sum([1, 2, 3, 4])",
    ),
    agreement(
      "a list built",
      "let map = fun (f, xs) -> case xs | [] => [] | x :: r => f(x) :: map(f, r) end in map(fun x -> x * x, [1, 2, 3])",
    ),
    agreement(
      "constructors with payloads",
      "type Shape = + Circle(Float) + Rect(Float, Float) in let area = fun s : Shape -> case s | Circle(r) => 3.14 *. r *. r | Rect(w, h) => w *. h end in (area(Circle(1.)), area(Rect(2., 3.)), Rect(1., 2.))",
    ),
    agreement(
      "options",
      "let f = fun x -> if x > 0 then Some(x) else None in (f(3), f(0 - 1))",
    ),
    agreement("strings", {|"a" ++ string_of_int(42) ++ "b"|}),
    agreement(
      "structural equality",
      {|([1, 2] == [1, 2], (1, "a") == (1, "b"), Some(3) == Some(3), [1] == [1, 2])|},
    ),
    agreement("list concatenation", "[1, 2] @ [3]"),
    agreement(
      "closures",
      "let add = fun x -> fun y -> x + y in let add5 = add(5) in add5(10)",
    ),
    agreement(
      "nested patterns",
      "case (1, [2, 3]) | (a, b :: c) => a + b | _ => 0 end",
    ),
    agreement("float builtins", "sqrt(16.) +. float_of_int(2)"),
    agreement("a function is a function", "fun x -> x + 1"),
    agreement("the Petri net rule", petri),
    agreement(
      "a module, a member using another",
      "module M = { let x = 1; let f = fun y -> y + x } in M.f(41)",
    ),
    agreement(
      "a module as a value",
      {|module M = { let x = 1; let y = (2, "b") } in M|},
    ),
    agreement("the length builtin", "length([1, 2, 3])"),
    agreement("the map builtin", "map([1, 2, 3], fun x -> x * 2)"),
    agreement(
      "the fold_left builtin",
      "fold_left([1, 2, 3], fun (acc, x) -> acc + x, 10)",
    ),
    comes_back(
      "back: whole floats stay floats, by their constructors' types",
      "type A = + SetX(Float) + Pick(Int) + Move((x = Float, y = Float)) in [SetX(3.), Pick(2), Move((x = 1., y = 2.5))]",
    ),
    comes_back(
      "back: through a type alias",
      "type P = (x = Float, y = Float) in type A = + Go(P) + Stop in (Go((x = 2., y = 3.)), Stop)",
    ),
    comes_back(
      "back: labels with no type to go by",
      {|(a = 1, b = "x", c = [true, false])|},
    ),
    comes_back("back: the Petri net rule's commands", petri),
    not_compiled("a hole", "1 + ?", "a hole"),
    /* (a cast through ? can fail at run time, where the interpreter stops and erased types run on) */
    not_compiled("a type hole", "(1 : ?) + 1", "a type hole"),
    not_compiled(
      "a cast through a type hole, the evaluator's ascription around a deferral",
      "let f : [Int] -> ? = fun xs -> xs in f([1.0, 2.0] : ?)",
      "a type hole",
    ),
  ],
);
