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
let get = (v: Js.Unsafe.any, k: string): Js.Unsafe.any => Js.Unsafe.get(v, k);
let is_null = (v: Js.Unsafe.any): bool => Js.Unsafe.equals(v, Js.null);
let num = (v: Js.Unsafe.any): float => Js.float_of_number(Js.Unsafe.coerce(v));

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
    want(typeof(js) == "number" && num(js) == float_of_int(n), "an integer")
  | Atom(Float(f)) =>
    want(
      typeof(js) == "number"
      && (
        Float.is_nan(f)
        && Float.is_nan(num(js))
        || Float.abs(num(js) -. f) <= 1e-9 *. Float.max(1., Float.abs(f))
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
  | Fun(_)
  | FixF(_)
  | Closure(_)
  | BuiltinFun(_) => want(typeof(js) == "function", "a function")
  | _ => Some("a value hazel-jit's tests do not compare: " ++ Exp.show(e))
  };
};

let agreement = (name, src) =>
  test_case(name, `Quick, () => {
    let elab = elaborate(parse_exp(src));
    let expected = Test_Evaluator_Prelude.evaluate(elab);
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
            ++ js_src,
          )
        };
      switch (agree(expected, got)) {
      | None => ()
      | Some(diff) =>
        fail(
          "differs from the interpreter: "
          ++ diff
          ++ "\nthe compiled JS: "
          ++ js_src,
        )
      };
    };
  });

let not_compiled = (name, src, reason) =>
  test_case(name, `Quick, () =>
    switch (
      HazelJit.try_compile(~env=Builtins.env_init, elaborate(parse_exp(src)))
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
    not_compiled("a hole", "1 + ?", "a hole"),
  ],
);
