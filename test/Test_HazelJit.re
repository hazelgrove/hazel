/* hazel-jit (^ticket(694)) agrees with the interpreter: each program is elaborated as the evaluator's tests do, run by the
   interpreter, compiled by HazelJit and run as JS, and the two values compared shape by shape (numbers by value, a float
   within a relative 1e-9; lists, tuples, labels and constructors by structure; a function only as a function). And a term
   with a form hazel-jit does not compile says why, rather than compiling wrongly. */
open Alcotest;
open Language;
open Js_of_ocaml;

let elaborate = Test_Evaluator_Prelude.elaborate;
let parse_exp = Test_Evaluator_Prelude.parse_exp;

/* run a compiled term: its source as the body of new Function("$R", src), given the runtime */
let run_js: (string, string) => Js.Unsafe.any =
  (runtime, src) =>
    Js.Unsafe.fun_call(
      Js.Unsafe.pure_js_expr(
        "(function (rt, src) { const R = eval('(' + rt + ')'); return new Function('$R', src)(R); })",
      ),
      [|
        Js.Unsafe.inject(Js.string(runtime)),
        Js.Unsafe.inject(Js.string(src)),
      |],
    );

let typeof = (v: Js.Unsafe.any): string =>
  Js.to_string(Js.typeof(Js.Unsafe.coerce(v)));
let get = (v: Js.Unsafe.any, k: string): Js.Unsafe.any =>
  Js.Unsafe.get(v, k);
let is_null = (v: Js.Unsafe.any): bool => Js.Unsafe.equals(v, Js.null);
let num = (v: Js.Unsafe.any): float =>
  Js.float_of_number(Js.Unsafe.coerce(v));

/* None when the JS value is the interpreter's value, else what differs */
let rec agree = (e: Exp.t, js: Js.Unsafe.any): option(string) => {
  let want = (ok, what) => ok ? None : Some(what ++ ": " ++ Exp.show(e));
  let is_array = v =>
    Js.to_bool(
      Js.Unsafe.meth_call(
        Js.Unsafe.get(Js.Unsafe.global, "Array"),
        "isArray",
        [|Js.Unsafe.inject(v)|],
      ),
    );
  switch (Exp.term_of(e)) {
  | Parens(e) => agree(e, js)
  | Atom(Int(n))
  | Atom(Nat(n)) =>
    want(
      typeof(js) == "number" && num(js) == Bigint.to_float(n),
      "an integer",
    )
  | Atom(SInt(n)) =>
    want(
      typeof(js) == "number" && num(js) == float_of_int(n),
      "an integer",
    )
  | Atom(Float(f)) =>
    want(
      typeof(js) == "number"
      && (
        Float.is_nan(f)
        && Float.is_nan(num(js))
        || Float.abs(num(js) -. f) <= 1e-9
        *. Float.max(1., Float.abs(f))
      ),
      "a float",
    )
  | Atom(Bool(b)) =>
    want(
      typeof(js) == "boolean" && Js.to_bool(Js.Unsafe.coerce(js)) == b,
      "a bool",
    )
  | Atom(String(s)) =>
    want(
      typeof(js) == "string" && Js.to_string(Js.Unsafe.coerce(js)) == s,
      "a string",
    )
  | ListLit(es) =>
    let rec walk = (es, v) =>
      switch (es) {
      | [] => want(is_null(v), "the end of a list")
      | [e, ...rest] =>
        is_null(v)
          ? Some("a list shorter than " ++ Exp.show(e))
          : (
            switch (agree(e, get(v, "h"))) {
            | None => walk(rest, get(v, "t"))
            | d => d
            }
          )
      };
    walk(es, js);
  | Cons(h, t) =>
    is_null(js)
      ? Some("[] for a cons")
      : (
        switch (agree(h, get(js, "h"))) {
        | None => agree(t, get(js, "t"))
        | d => d
        }
      )
  | Tuple(es) =>
    !is_array(js)
    || num(get(js, "length")) != float_of_int(List.length(es))
      ? Some("a tuple of " ++ string_of_int(List.length(es)))
      : List.fold_left(
          ((acc, i), e: Exp.t) =>
            (
              switch (acc) {
              | Some(_) => acc
              | None =>
                switch (Exp.match_tup_label(e)) {
                | Some((l, e')) => agree(e', get(js, l))
                | None => agree(e, Js.Unsafe.get(js, i))
                }
              },
              i + 1,
            ),
          (None, 0),
          es,
        )
        |> fst
  | TupLabel(_) =>
    switch (Exp.match_tup_label(e)) {
    | Some((l, e')) => agree(e', get(js, l))
    | None => Some("a label")
    }
  | Constructor(name, _) =>
    want(
      typeof(js) == "object"
      && !is_null(js)
      && Js.to_string(Js.Unsafe.coerce(get(js, "c"))) == name,
      "the constructor " ++ name,
    )
  | Ap(Forward, {term: Constructor(name, _), _}, arg) =>
    typeof(js) == "object"
    && !is_null(js)
    && Js.to_string(Js.Unsafe.coerce(get(js, "c"))) == name
      ? agree(arg, get(js, "v")) : Some("the constructor " ++ name)
  | Module(items) =>
    List.fold_left(
      (acc, item: Mod.t) =>
        switch (acc, item.term) {
        | (Some(_), _) => acc
        | (None, ModVal(x, v)) => agree(v, get(js, x))
        | (None, _) => None
        },
      None,
      items,
    )
  | Fun(_)
  | FixF(_)
  | Closure(_)
  | BuiltinFun(_) => want(typeof(js) == "function", "a function")
  | _ => Some("a value hazel-jit's tests do not compare: " ++ Exp.show(e))
  };
};

let agrees = src => {
  let elab = elaborate(parse_exp(src));
  let expected = Test_Evaluator_Prelude.evaluate(elab);
  let clip = js =>
    String.length(js) > 4000 ? String.sub(js, 0, 4000) ++ " ..." : js;
  switch (HazelJit.try_compile(~env=Builtins.env_init, elab)) {
  | Error(reason) => fail("not compiled: " ++ reason)
  | Ok(js_src) =>
    let got =
      try(run_js(HazelJit.Runtime.source, js_src)) {
      | Js_error.Exn(e) =>
        fail(
          "the compiled JS threw: "
          ++ Js_error.message(e)
          ++ "\n"
          ++ clip(js_src),
        )
      };
    switch (agree(expected, got)) {
    | None => ()
    | Some(diff) =>
      fail(
        "differs from the interpreter: "
        ++ diff
        ++ "\nthe compiled JS: "
        ++ clip(js_src),
      )
    };
  };
};
let agreement = (name, src) => test_case(name, `Quick, () => agrees(src));

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

/* (slow: not in the quick tests) the best case, measured: a program run n times by the interpreter, and its compiled JS run n
   times, after compiling it once; medians in ms, written to the test's output (its file under _build/_tests) */
let timing_run = (name, src, n) => {
  let elab = elaborate(parse_exp(src));
  let now = () => Util.JsUtil.precise_timestamp();
  let median = xs => {
    let a = Array.of_list(xs);
    Array.sort(compare, a);
    a[Array.length(a) / 2];
  };
  let interp =
    List.init(
      n,
      _ => {
        let t0 = now();
        ignore(Test_Evaluator_Prelude.evaluate(elab));
        now() -. t0;
      },
    );
  let t0 = now();
  let js_src = HazelJit.compile(~env=Builtins.env_init, elab);
  let compile_ms = now() -. t0;
  /* the module members hazel-jit left as stubs, and why */
  let stubs =
    Str.full_split(Str.regexp({|\$R\.missing("\([^"]*\)")|}), js_src)
    |> List.filter_map(
         fun
         | Str.Delim(d) => Some(d)
         | Str.Text(_) => None,
       );
  Printf.printf(
    "hazel-jit stubs in %s: %d %s; compiled JS %d bytes\n",
    name,
    List.length(stubs),
    String.concat(" ", stubs),
    String.length(js_src),
  );
  let f: Js.Unsafe.any =
    Js.Unsafe.fun_call(
      Js.Unsafe.pure_js_expr(
        "(function (rt, src) { const R = eval('(' + rt + ')'); const f = new Function('$R', src); return function () { return f(R); }; })",
      ),
      [|
        Js.Unsafe.inject(Js.string(HazelJit.Runtime.source)),
        Js.Unsafe.inject(Js.string(js_src)),
      |],
    );
  let jit =
    List.init(
      n,
      _ => {
        let t0 = now();
        ignore(Js.Unsafe.fun_call(f, [||]): Js.Unsafe.any);
        now() -. t0;
      },
    );
  Printf.printf(
    "hazel-jit timing %s: n=%d interpreter median %.3f ms (min %.3f), compile %.3f ms, compiled median %.4f ms (min %.4f), %.0fx\n",
    name,
    n,
    median(interp),
    List.fold_left(min, infinity, interp),
    compile_ms,
    median(jit),
    List.fold_left(min, infinity, jit),
    median(interp) /. Float.max(median(jit), 1e-6),
  );
};
let timing = (name, src, n) =>
  test_case("timing: " ++ name, `Slow, () => timing_run(name, src, n));

/* The Polygons slide's livelit as a plain module, its view applied to the model the slide's own `shirt` gives it:
   the slide's text up to the livelit, then `module P = { <the livelit's body> } in P.view(<shirt's model>)`. Found by
   scanning brackets, skipping Hazel's comments (# to #) and strings. */
let polygons_view_program = (): option(string) => {
  let path = "hazel-programs/docs/livelits/playground/polygons.hz";
  let path = Sys.file_exists(path) ? path : "../" ++ path;
  switch (CorpusUtil.read_file(path)) {
  | None => None
  | Some(src) =>
    let find = (from, needle) => {
      let n = String.length(needle);
      let rec go = i =>
        i < 0 || i + n > String.length(src)
          ? (-1) : String.sub(src, i, n) == needle ? i : go(i + 1);
      go(from);
    };
    /* the index of the bracket closing the one at i */
    let matching = (i, opening, closing) => {
      let rec go = (j, depth, in_str, in_comment) =>
        if (j < 0 || j >= String.length(src)) {
          (-1);
        } else {
          let c = src.[j];
          if (in_comment) {
            go(j + 1, depth, false, c != '#');
          } else if (in_str) {
            go(j + 1, depth, c != '"', false);
          } else if (c == '#') {
            go(j + 1, depth, false, true);
          } else if (c == '"') {
            go(j + 1, depth, true, false);
          } else if (c == opening) {
            go(j + 1, depth + 1, false, false);
          } else if (c == closing) {
            depth == 1 ? j : go(j + 1, depth - 1, false, false);
          } else {
            go(j + 1, depth, false, false);
          };
        };
      go(i, 0, false, false);
    };
    let def = find(0, "let ^polygons = {");
    let open_ = def + String.length("let ^polygons = ");
    let close = def < 0 ? (-1) : matching(open_, '{', '}');
    let use = close < 0 ? (-1) : find(close, "^polygons(");
    let arg_open = use + String.length("^polygons");
    let arg_close = use < 0 ? (-1) : matching(arg_open, '(', ')');
    arg_close < 0
      ? None
      : Some(
          String.sub(src, 0, def)
          ++ "module P = "
          ++ String.sub(src, open_, close - open_ + 1)
          ++ " in\nP.view("
          ++ String.sub(src, arg_open + 1, arg_close - arg_open - 1)
          ++ ")",
        );
  };
};
let polygons_view = (name, speed, check) =>
  test_case(name, speed, () =>
    switch (polygons_view_program()) {
    | None => fail("the Polygons slide was not found, or not as expected")
    | Some(src) => check(src)
    }
  );

let tests = (
  "HazelJit",
  [
    agreement("arithmetic", "1 + 2 * 3 - 4 / 2"),
    agreement("floats", "1.5 *. 2. +. 0.25"),
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
    agreement("to_fixed", "(to_fixed(3.14159, 2), to_fixed(2., 0))"),
    polygons_view("the Polygons slide's view", `Quick, agrees),
    not_compiled("a hole", "1 + ?", "a hole"),
    timing("the Petri net rule, four events", petri, 30),
    polygons_view("timing: the Polygons slide's view", `Slow, src =>
      timing_run("the Polygons slide's view", src, 30)
    ),
    timing(
      "a recursive sum of 1,000",
      "let sum = fun n -> if n == 0 then 0 else n + sum(n - 1) in sum(1000)",
      30,
    ),
  ],
);
