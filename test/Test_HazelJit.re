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

/* each run's reading, as one line of JSON, for ^report(45)'s data */
let print_raw = (name, series: list((string, list(float)))) =>
  Printf.printf(
    "hazel-jit raw %s: {%s}\n",
    name,
    String.concat(
      ", ",
      List.map(
        ((k, xs)) =>
          Printf.sprintf(
            "\"%s\": [%s]",
            k,
            String.concat(", ", List.map(Printf.sprintf("%.4f"), xs)),
          ),
        series,
      ),
    ),
  );

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
  print_raw(
    name,
    [
      ("interpreter_ms", interp),
      ("compile_ms", [compile_ms]),
      ("compiled_ms", jit),
    ],
  );
};
let timing = (name, src, n) =>
  test_case("timing: " ++ name, `Slow, () => timing_run(name, src, n));

/* The Polygons slide's livelit as a plain module, its view applied to the model the slide's own `shirt` gives it:
   the slide's text up to the livelit, then `module P = { <the livelit's body> } in P.view(<shirt's model>)`. Found by
   scanning brackets, skipping Hazel's comments (# to #) and strings. */
let polygons_parts = (): option((string, string, string)) => {
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
      : Some((
          String.sub(src, 0, def),
          String.sub(src, open_, close - open_ + 1),
          String.sub(src, arg_open + 1, arg_close - arg_open - 1),
        ));
  };
};
let polygons_view_program = (): option(string) =>
  polygons_parts()
  |> Option.map(((prefix, body, model)) =>
       prefix ++ "module P = " ++ body ++ " in\nP.view(" ++ model ++ ")"
     );
/* (slice 3) Two Hazel values the same, shape by shape: an Int and a Float are different, a list is its elements however
   it was built, labels by name, constructors by name (not their type annotations); None when they are, else where not */
let rec same = (a: Exp.t, b: Exp.t): option(string) => {
  let differ = what =>
    Some(what ++ ":\n  want " ++ Exp.show(a) ++ "\n  got  " ++ Exp.show(b));
  let rec items = (e: Exp.t): option(list(Exp.t)) =>
    switch (Exp.term_of(e)) {
    | Parens(e) => items(e)
    | ListLit(es) => Some(es)
    | Cons(h, t) => Option.map(rest => [h, ...rest], items(t))
    | _ => None
    };
  let all = (xs, ys) =>
    List.length(xs) != List.length(ys)
      ? differ("lengths")
      : List.fold_left2(
          (acc, x, y) => acc == None ? same(x, y) : acc,
          None,
          xs,
          ys,
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
let view_comes_back = (~prefix="", def_src, model_src) => {
  let program = s => elaborate(parse_exp(prefix ++ s));
  let expected =
    Test_Evaluator_Prelude.evaluate(
      program("module P = " ++ def_src ++ " in P.view(" ++ model_src ++ ")"),
    );
  let model = Test_Evaluator_Prelude.evaluate(program(model_src));
  switch (
    HazelJitValue.compile_view(
      ~env=Builtins.env_init,
      program("module P = " ++ def_src ++ " in P"),
    )
  ) {
  | Error(why) => fail("not compiled: " ++ why)
  | Ok(v) =>
    switch (HazelJitValue.run_view(v, model)) {
    | Error(why) => fail("not run: " ++ why)
    | Ok(got) =>
      switch (same(expected, got)) {
      | None => ()
      | Some(diff) => fail("not the interpreter's answer: " ++ diff)
      }
    }
  };
};

/* (^ticket(715)) The Polygons livelit's own update, on its slide's model, compiled and its new model carried back as a diff
   (HazelJitValue.call_update), against the interpreter's (the definition's update applied, its command run): the same
   model, every action; what it did not change shared. With ~timing, each way timed (medians of n, in ms): the
   interpreter as the app ran it before (the definition evaluated, then its update), its update alone, and compiled. */
let polygons_update_run = (~timing=0, ()) =>
  switch (polygons_parts()) {
  | None => fail("the Polygons slide was not found, or not as expected")
  | Some((prefix, body, model_src)) =>
    module G = IdTagged.FreshGrammar;
    let program = s => elaborate(parse_exp(prefix ++ s));
    let model0 = Test_Evaluator_Prelude.evaluate(program(model_src));
    let update_prog = program("module P = " ++ body ++ " in P.update");
    let update_fn = Test_Evaluator_Prelude.evaluate(update_prog);
    let v =
      switch (
        HazelJitValue.compile_view(
          ~env=Builtins.env_init,
          program("module P = " ++ body ++ " in P"),
        )
      ) {
      | Ok(v) => v
      | Error(why) => fail("not compiled: " ++ why)
      };
    /* the actions, made in the livelit's own scope (elaborated, as its handlers make them): a member added to it */
    let acts_src = {|[Next, Prev, NewPolygon, NewPath, Toggle, Remove, SetName("roof"), SetFill("#000000"), SetWidth("5"), Down(40, 50), MoveTo(60, 70), Up]|};
    let with_acts =
      String.sub(body, 0, String.rindex(body, '}'))
      ++ ";\n  let test_actions = "
      ++ acts_src
      ++ "\n}";
    let acts =
      switch (
        Exp.term_of(
          Test_Evaluator_Prelude.evaluate(
            program("module P = " ++ with_acts ++ " in P.test_actions"),
          ),
        )
      ) {
      | ListLit(xs) when List.length(xs) == 12 => Array.of_list(xs)
      | _ => fail("the actions did not evaluate to a list of 12")
      };
    let interp = (m, a) =>
      switch (
        Haz3lcore.UpdateCmdRunner.run(
          Test_Evaluator_Prelude.evaluate(
            G.Exp.ap(Forward, G.Exp.ap(Forward, update_fn, m), a),
          ),
        )
      ) {
      | Ok((m', _)) => m'
      | Error(e) => fail("the interpreter's update: " ++ e)
      };
    let compiled = (m, a) =>
      switch (HazelJitValue.call_update(v, m, a)) {
      | Ok(m') => m'
      | Error(why) => fail("not compiled: " ++ why)
      };
    /* a drag: Down adds a point and holds it; MoveTo moves it */
    let held = interp(model0, acts[9]);
    let cases = [
      ("Next", model0, acts[0]),
      ("Prev", model0, acts[1]),
      ("NewPolygon", model0, acts[2]),
      ("NewPath", model0, acts[3]),
      ("Toggle", model0, acts[4]),
      ("Remove", model0, acts[5]),
      ("SetName", model0, acts[6]),
      ("SetFill", model0, acts[7]),
      ("SetWidth", model0, acts[8]),
      ("Down", model0, acts[9]),
      ("MoveTo, held", held, acts[10]),
      ("Up, held", held, acts[11]),
    ];
    /* the shapes of the new model that are the old model's own (the same OCaml values) */
    let shapes = (m: Exp.t) =>
      switch (Exp.term_of(m)) {
      | Tuple([s, ..._]) =>
        switch (Exp.term_of(s)) {
        | TupLabel(_, l) =>
          switch (Exp.term_of(l)) {
          | ListLit(xs) => xs
          | _ => []
          }
        | ListLit(xs) => xs
        | _ => []
        }
      | _ => []
      };
    let now = () => Util.JsUtil.precise_timestamp();
    let median = xs => {
      let a = Array.of_list(xs);
      Array.sort(compare, a);
      a[Array.length(a) / 2];
    };
    let times = f => {
      for (_ in 1 to 5) {
        f();
      };
      List.init(
        timing,
        _ => {
          let t0 = now();
          f();
          now() -. t0;
        },
      );
    };
    List.iter(
      ((name, m, a)) => {
        let want = interp(m, a);
        let got = compiled(m, a);
        switch (same(want, got)) {
        | None => ()
        | Some(diff) =>
          fail(name ++ ": not the interpreter's model: " ++ diff)
        };
        let old_shapes = shapes(m);
        let new_shapes = shapes(got);
        let kept =
          List.length(
            List.filter(
              s => List.exists(o => o === s, old_shapes),
              new_shapes,
            ),
          );
        if (timing > 0) {
          /* (the definition elaborated once, as the app has it, and evaluated each time, as the app did per event) */
          let with_def =
            times(() => {
              let u = Test_Evaluator_Prelude.evaluate(update_prog);
              ignore(
                Haz3lcore.UpdateCmdRunner.run(
                  Test_Evaluator_Prelude.evaluate(
                    G.Exp.ap(Forward, G.Exp.ap(Forward, u, m), a),
                  ),
                ),
              );
            });
          let alone = times(() => ignore(interp(m, a)));
          let jit = times(() => ignore(compiled(m, a)));
          Printf.printf(
            "hazel-jit update, %s: the interpreter with the definition median %.3f ms, its update alone median %.3f ms; compiled, the model back as a diff, median %.4f ms; %d of %d shapes the old ones\n",
            name,
            median(with_def),
            median(alone),
            median(jit),
            kept,
            List.length(new_shapes),
          );
          print_raw(
            "the Polygons update, " ++ name,
            [
              ("interpreter_with_definition_ms", with_def),
              ("interpreter_update_ms", alone),
              ("compiled_diff_ms", jit),
              ("shapes_kept", [float_of_int(kept)]),
              ("shapes", [float_of_int(List.length(new_shapes))]),
            ],
          );
        };
      },
      cases,
    );
  };

/* (^ticket(709)) A diff applied to the last value is the new value, carried back whole: each program gives a pair
   (old, new), new made from old by compiled code; the diff, when given, is what diff must say */
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
let diff_timing_run = () => {
  let src = "let set = fun (xs, k, p) -> case xs | [] => [] | x :: rest => if k == 0 then p :: rest else x :: set(rest, k - 1, p) end in set";
  let c =
    HazelJit.compile_full(
      ~env=Builtins.env_init,
      elaborate(parse_exp(src)),
    );
  let set = HazelJitValue.instantiate(c.src);
  let types = HazelJitValue.types_of(c);
  let points: Js.Unsafe.any =
    Js.Unsafe.pure_js_expr(
      "(function (n) { var l = null; for (var i = n - 1; i >= 0; i--) { var p = [i, 2 * i]; p.x = i; p.y = 2 * i; l = {h: p, t: l}; } return l; })",
    );
  let point: Js.Unsafe.any =
    Js.Unsafe.pure_js_expr(
      "(function () { var p = [-1, -1]; p.x = -1; p.y = -1; return p; })()",
    );
  let now = () => Util.JsUtil.precise_timestamp();
  let median = xs => {
    let a = Array.of_list(xs);
    Array.sort(compare, a);
    a[Array.length(a) / 2];
  };
  let times = (reps, f) => {
    for (_ in 1 to 5) {
      f();
    };
    List.init(
      reps,
      _ => {
        let t0 = now();
        f();
        now() -. t0;
      },
    );
  };
  let items = (e: Exp.t) =>
    switch (Exp.term_of(e)) {
    | ListLit(xs) => xs
    | _ => fail("not a list: " ++ Exp.show(e))
    };
  List.iter(
    n => {
      let reps = 15;
      let old_js: Js.Unsafe.any =
        Js.Unsafe.fun_call(
          points,
          [|HazelJitValue.number(float_of_int(n))|],
        );
      let old_hz = HazelJitValue.of_js(types, None, old_js);
      let new_js: Js.Unsafe.any =
        Js.Unsafe.fun_call(
          set,
          [|
            Js.Unsafe.inject(
              Js.array([|
                old_js,
                HazelJitValue.number(float_of_int(n / 2)),
                point,
              |]),
            ),
          |],
        );
      let d = HazelJitValue.diff(old_js, new_js);
      let whole = HazelJitValue.of_js(types, None, new_js);
      let patched = HazelJitValue.patch(types, None, old_hz, d);
      switch (same(whole, patched)) {
      | None => ()
      | Some(why) => fail("patched is not carried back whole: " ++ why)
      };
      let shared =
        List.fold_left2(
          (k, a, b) => a === b ? k + 1 : k,
          0,
          items(old_hz),
          items(patched),
        );
      let full =
        times(reps, () => ignore(HazelJitValue.of_js(types, None, new_js)));
      let diffing =
        times(reps, () => ignore(HazelJitValue.diff(old_js, new_js)));
      let patching =
        times(reps, () =>
          ignore(HazelJitValue.patch(types, None, old_hz, d))
        );
      let shown =
        n <= 100
          ? Js.to_string(
              Js.Unsafe.fun_call(
                Js.Unsafe.pure_js_expr("JSON.stringify"),
                [|d|],
              ): Js.t(Js.js_string),
            )
          : "(" ++ string_of_int(n) ++ " elements)";
      Printf.printf(
        "hazel-jit diff, %d points, the middle one changed: carried back whole median %.4f ms; diffed median %.4f ms, patched median %.4f ms; %d of %d elements the old ones, shared; the diff %s\n",
        n,
        median(full),
        median(diffing),
        median(patching),
        shared,
        n,
        shown,
      );
      print_raw(
        "the way back as a diff, " ++ string_of_int(n) ++ " points",
        [
          ("whole_ms", full),
          ("diff_ms", diffing),
          ("patch_ms", patching),
          ("shared_elements", [float_of_int(shared)]),
        ],
      );
    },
    [10, 100, 1000, 10000],
  );
};

/* (2026-11-07) Two virtual DOM nodes, made raw (virtual-dom's own objects), compared as data: every field, keys in order,
   functions (the event handlers) only as functions, and a cycle cut. "same (N chars)" or where they first differ. */
let compare_raw: (Js.Unsafe.any, Js.Unsafe.any) => string = {
  let f: Js.Unsafe.any =
    Js.Unsafe.pure_js_expr(
      {|(function (a, b) {
  function norm(x, path) {
    if (typeof x === "function") return "<fn>";
    if (x === null || typeof x !== "object") return x;
    if (path.indexOf(x) >= 0) return "<cycle>";
    path.push(x);
    var r;
    if (Array.isArray(x)) r = x.map(function (y) { return norm(y, path); });
    else {
      r = {};
      Object.keys(x).sort().forEach(function (k) { r[k] = norm(x[k], path); });
      if (x.constructor && x.constructor.name && x.constructor.name !== "Object") r.__ctor = x.constructor.name;
    }
    path.pop();
    return r;
  }
  var sa = JSON.stringify(norm(a, [])), sb = JSON.stringify(norm(b, []));
  if (sa === sb) return "same (" + sa.length + " chars)";
  var i = 0; while (i < sa.length && sa[i] === sb[i]) i++;
  return "differ at " + i + ": " + sa.slice(Math.max(0, i - 160), i + 160) + " ||| " + sb.slice(Math.max(0, i - 160), i + 160);
})|},
    );
  (a, b) =>
    Js.to_string(Js.Unsafe.fun_call(f, [|a, b|]): Js.t(Js.js_string));
};
let ignoring_mvu: Haz3lcore.HazelDOM.t = {
  inject: (_, _) => Virtual_dom.Vdom.Effect.Ignore,
  view_term: _ => Virtual_dom.Vdom.Node.none,
  splice_view: _ => None,
  commit: Haz3lcore.HazelDOM.State,
};
/* The Polygons view's answer drawn both ways: carried back as Hazel, run and drawn by HazelDOM; and drawn straight from
   its JS by HazelJitDOM. The two nodes made raw and compared. */
let drawn_both_ways =
    (v: HazelJitValue.view, js_answer: Js.Unsafe.any): string => {
  let hazel =
    switch (
      Haz3lcore.ViewCmdRunner.run(
        HazelJitValue.of_js(v.types, None, js_answer),
      )
    ) {
    | Ok(h) => Haz3lcore.HazelDOM.go(ignoring_mvu, h)
    | Error(why) => fail("the view command did not run: " ++ why)
    };
  let direct = Haz3lcore.HazelJitDOM.go(ignoring_mvu, v.types, js_answer);
  compare_raw(
    Js.Unsafe.inject(Virtual_dom.Vdom.Node.to_raw(hazel)),
    Js.Unsafe.inject(Virtual_dom.Vdom.Node.to_raw(direct)),
  );
};

/* (slow) A render of the Polygons view, timed several ways, medians of n in ms: the interpreter applying the evaluated
   view to the model (what eval_view did, less CallMemo); the compiled view through the boundary (run_view: the model
   into JS, the call, the answer back), its first n renders after compiling and then n more, warmed up; and each of its
   three steps alone: the model into JS, the compiled call on a model already in JS, and the answer back.
   (2026-11-06 18:52: run first and cold, the whole render's runs fell from 1.8 to 0.45 ms over 30 runs while Node
   optimized the way back, so a median of them said more about warming up than about a render; every series but the
   cold one now runs `warm` times untimed first.) */
let boundary_timing_run = n =>
  switch (polygons_parts()) {
  | None => fail("the Polygons slide was not found, or not as expected")
  | Some((prefix, body, model_src)) =>
    let program = s => elaborate(parse_exp(prefix ++ s));
    let now = () => Util.JsUtil.precise_timestamp();
    let median = xs => {
      let a = Array.of_list(xs);
      Array.sort(compare, a);
      a[Array.length(a) / 2];
    };
    let warm = 10;
    let timed = f =>
      List.init(
        n,
        _ => {
          let t0 = now();
          f();
          now() -. t0;
        },
      );
    let times = f => {
      for (_ in 1 to warm) {
        f();
      };
      timed(f);
    };
    let view_fn =
      Test_Evaluator_Prelude.evaluate(
        program("module P = " ++ body ++ " in P.view"),
      );
    let model = Test_Evaluator_Prelude.evaluate(program(model_src));
    switch (
      HazelJitValue.compile_view(
        ~env=Builtins.env_init,
        program("module P = " ++ body ++ " in P"),
      )
    ) {
    | Error(why) => fail("not compiled: " ++ why)
    | Ok(v) =>
      let interp =
        times(() =>
          ignore(
            Test_Evaluator_Prelude.evaluate(
              IdTagged.FreshGrammar.Exp.ap(Forward, view_fn, model),
            ),
          )
        );
      let render = () =>
        switch (HazelJitValue.run_view(v, model)) {
        | Ok(_) => ()
        | Error(why) => fail("not run: " ++ why)
        };
      /* the first n renders after compiling, cold, as a livelit's first renders are */
      let through_cold = timed(render);
      let js_model = HazelJitValue.to_js(model);
      let call =
        times(() =>
          ignore(Js.Unsafe.fun_call(v.fn, [|js_model|]): Js.Unsafe.any)
        );
      /* (Matthew, 2026-11-06: which trip costs more) the trip in alone, and the trip back alone, on one answer */
      let into = times(() => ignore(HazelJitValue.to_js(model)));
      let js_answer: Js.Unsafe.any = Js.Unsafe.fun_call(v.fn, [|js_model|]);
      let back =
        times(() => ignore(HazelJitValue.of_js(v.types, None, js_answer)));
      /* and the whole render again, warmed up like its parts */
      let through = times(render);
      /* of the trip back, what making the terms' ids costs: each term of_js builds gets a fresh id (IdTag.fresh, a
         random UUID), so the same number of fresh ids, timed alone. The terms counted are the answer's, not a closure's
         body, which is the compiled definition's own and not made again. */
      let made = ref(0);
      let _ =
        Exp.map_term(
          ~f_exp=
            (cont, e) => {
              incr(made);
              switch (Exp.term_of(e)) {
              | Closure(_) => e
              | _ => cont(e)
              };
            },
          HazelJitValue.of_js(v.types, None, js_answer),
        );
      let ids =
        times(() =>
          for (_ in 1 to made^) {
            ignore(IdTagged.IdTag.fresh());
          }
        );
      /* (2026-11-07) the same, made as Id.mk made them before 810df240ea's successor: eight concatenations an id */
      let old_mk = {
        let byte = Array.init(256, i => String.make(1, Char.chr(i)));
        let tail = "\x80\x01\x02\x03\x04\x05\x06\x07";
        let lo = ref(0);
        (
          () => {
            incr(lo);
            let l = lo^;
            let h = 0;
            Uuidm.unsafe_of_bytes(
              byte[l land 0xff]
              ++ byte[l lsr 8 land 0xff]
              ++ byte[l lsr 16 land 0xff]
              ++ byte[l lsr 24 land 0x3f]
              ++ byte[h land 0xff]
              ++ byte[h lsr 8 land 0xff]
              ++ byte[0x40 lor h lsr 16 land 0x0f]
              ++ byte[h lsr 20 land 0xff]
              ++ tail,
            );
          }
        );
      };
      let ids_old =
        times(() =>
          for (_ in 1 to made^) {
            ignore(IdTagged.IdTag.mk_internal([old_mk()]));
          }
        );
      /* and the trip back with no ids to make: every term given one shared id */
      HazelJitValue.shared_ids := true;
      let back_shared =
        times(() => ignore(HazelJitValue.of_js(v.types, None, js_answer)));
      HazelJitValue.shared_ids := false;
      /* (Matthew, 2026-11-07: can a view's answer skip Hazel altogether?) what the app does with the Hazel answer after the
         trip back: ViewCmdRunner runs the command down to its Html, HazelDOM draws that Html as a virtual DOM node (with
         event handlers that dispatch to an mvu that ignores them here), and the page turns the node into virtual-dom's
         own (to_raw) to diff it against the page */
      let mvu: Haz3lcore.HazelDOM.t = {
        inject: (_, _) => Virtual_dom.Vdom.Effect.Ignore,
        view_term: _ => Virtual_dom.Vdom.Node.none,
        splice_view: _ => None,
        commit: Haz3lcore.HazelDOM.State,
      };
      let answer = HazelJitValue.of_js(v.types, None, js_answer);
      let html =
        switch (Haz3lcore.ViewCmdRunner.run(answer)) {
        | Ok(h) => h
        | Error(why) => fail("the view command did not run: " ++ why)
        };
      let run_cmd = times(() => ignore(Haz3lcore.ViewCmdRunner.run(answer)));
      let draw = times(() => ignore(Haz3lcore.HazelDOM.go(mvu, html)));
      let node = Haz3lcore.HazelDOM.go(mvu, html);
      let raw = times(() => ignore(Virtual_dom.Vdom.Node.to_raw(node)));
      /* (Matthew, 2026-11-07) the direct route: the JS answer drawn straight to the virtual DOM, against the whole Hazel
         route from the same JS answer (the trip back, the command run, HazelDOM's drawing) */
      let hazel_route =
        times(() =>
          switch (
            Haz3lcore.ViewCmdRunner.run(
              HazelJitValue.of_js(v.types, None, js_answer),
            )
          ) {
          | Ok(h) => ignore(Haz3lcore.HazelDOM.go(mvu, h))
          | Error(why) => fail("not run: " ++ why)
          }
        );
      let direct =
        times(() =>
          ignore(Haz3lcore.HazelJitDOM.go(mvu, v.types, js_answer))
        );
      let same = drawn_both_ways(v, js_answer);
      if (String.length(same) < 4 || String.sub(same, 0, 4) != "same") {
        fail("drawn straight from JS, not HazelDOM's drawing: " ++ same);
      };
      /* how much each trip carries: the nodes the walk visits (as of_js walks them, each function's captured values
         included), how many of those are distinct objects, and how many it visits inside captured values */
      let census: Js.Unsafe.any =
        Js.Unsafe.pure_js_expr(
          {|(function (root) {
  var c = {visits: 0, distinct_objects: 0, in_captures: 0, functions: 0, cons_cells: 0, tuples: 0, constructors: 0, atoms: 0};
  var seen = new Set(), names = {};
  function walk(v, cap) {
    for (;;) {
      c.visits++; if (cap) c.in_captures++;
      var t = typeof v;
      if (v === null || v === undefined || (t !== "object" && t !== "function")) { c.atoms++; return; }
      if (!seen.has(v)) { seen.add(v); c.distinct_objects++; }
      if (t === "function") { c.functions++; if (v.$e) for (var i = 0; i < v.$e.length; i++) walk(v.$e[i], true); return; }
      if (Array.isArray(v)) { c.tuples++; for (var j = 0; j < v.length; j++) walk(v[j], cap); return; }
      if (Object.hasOwn(v, "c")) { c.constructors++; names[v.c] = (names[v.c] || 0) + 1; if (Object.hasOwn(v, "v")) { v = v.v; continue; } return; }
      if (Object.hasOwn(v, "h")) { c.cons_cells++; walk(v.h, cap); if (v.t === null) return; v = v.t; continue; }
      return;
    }
  }
  walk(root, false);
  c.names = names;
  return JSON.stringify(c);
})|},
        );
      let count = js =>
        Js.to_string(
          Js.Unsafe.fun_call(census, [|js|]): Js.t(Js.js_string),
        );
      Printf.printf(
        "hazel-jit boundary timing, the Polygons view: n=%d (warmed up by %d), compile %.2f ms; interpreter median %.3f ms (min %.3f); through the boundary, the first %d renders, median %.3f ms; through the boundary, warm, median %.3f ms (min %.3f), %.0fx; the call alone median %.4f ms; the trip in alone median %.4f ms, the trip back alone median %.4f ms\n",
        n,
        warm,
        v.compile_ms,
        median(interp),
        List.fold_left(min, infinity, interp),
        n,
        median(through_cold),
        median(through),
        List.fold_left(min, infinity, through),
        median(interp) /. Float.max(median(through), 1e-6),
        median(call),
        median(into),
        median(back),
      );
      Printf.printf(
        "hazel-jit boundary census: the model %s; the answer %s; the trip back makes %d terms, and %d fresh ids alone take median %.4f ms (made the old way, %.4f ms); the trip back with one shared id, median %.4f ms\n",
        count(js_model),
        count(js_answer),
        made^,
        made^,
        median(ids),
        median(ids_old),
        median(back_shared),
      );
      Printf.printf(
        "hazel-jit downstream of the trip back: the view command run, median %.4f ms; the Html drawn as a vdom node (HazelDOM.go), median %.4f ms; the node made raw (to_raw), median %.4f ms\n",
        median(run_cmd),
        median(draw),
        median(raw),
      );
      Printf.printf(
        "hazel-jit direct route: from the JS answer to a vdom node through Hazel (trip back, command, HazelDOM), median %.4f ms; straight from JS (HazelJitDOM), median %.4f ms, %.1fx; the two drawings %s\n",
        median(hazel_route),
        median(direct),
        median(hazel_route) /. Float.max(median(direct), 1e-6),
        same,
      );
      print_raw(
        "the Polygons view through the boundary",
        [
          ("compile_ms", [v.compile_ms]),
          ("interpreter_ms", interp),
          ("through_boundary_first_ms", through_cold),
          ("through_boundary_ms", through),
          ("call_alone_ms", call),
          ("trip_in_ms", into),
          ("trip_back_ms", back),
          ("fresh_ids_ms", ids),
          ("fresh_ids_old_ms", ids_old),
          ("trip_back_shared_id_ms", back_shared),
          ("view_cmd_run_ms", run_cmd),
          ("hazeldom_draw_ms", draw),
          ("vdom_to_raw_ms", raw),
          ("hazel_route_draw_ms", hazel_route),
          ("direct_draw_ms", direct),
        ],
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
    /* (a view's answer is a constructor, ViewCmd's: a number outside any constructor has no type to be read by, and a
       whole one comes back an Int) */
    test_case("a view through the boundary", `Quick, () =>
      view_comes_back(
        "{ type Model = (x = Float, tags = [String]); type Action = + Bump(Float); type Answer = + Shown((Float, [String], Action)); let view = fun m : Model -> Shown((m.x *. 2., m.tags, Bump(1.))) }",
        {|(x = 2., tags = ["a", "b"])|},
      )
    ),
    test_case(
      "a handler through the boundary, applied",
      `Quick,
      () => {
        /* the view's handler captures the model's index; carried back as a Hazel closure, the interpreter applies it */
        let def = "{ type Action = + Set(Int, String); type Attr = + OnInput(String -> Action); let view = fun m : (i = Int) -> let i = m.i * 10 in OnInput(fun v -> Set(i, v)) }";
        let model = "(i = 4)";
        let program = s => elaborate(parse_exp(s));
        switch (
          HazelJitValue.compile_view(
            ~env=Builtins.env_init,
            program("module P = " ++ def ++ " in P"),
          )
        ) {
        | Error(why) => fail("not compiled: " ++ why)
        | Ok(v) =>
          switch (
            HazelJitValue.run_view(
              v,
              Test_Evaluator_Prelude.evaluate(program(model)),
            )
          ) {
          | Error(why) => fail("not run: " ++ why)
          | Ok({term: Ap(Forward, _, handler), _}) =>
            let applied =
              Test_Evaluator_Prelude.evaluate(
                IdTagged.FreshGrammar.Exp.ap(
                  Forward,
                  handler,
                  IdTagged.FreshGrammar.Exp.string("typed"),
                ),
              );
            let expected =
              Test_Evaluator_Prelude.evaluate(
                program(
                  "module P = "
                  ++ def
                  ++ " in case P.view("
                  ++ model
                  ++ ") | OnInput(h) => h(\"typed\") end",
                ),
              );
            switch (same(expected, applied)) {
            | None => ()
            | Some(diff) =>
              fail("the handler does not do what it did: " ++ diff)
            };
          | Ok(e) => fail("not OnInput(handler): " ++ Exp.show(e))
          }
        };
      },
    ),
    test_case("the Polygons slide's view through the boundary", `Quick, () =>
      switch (polygons_parts()) {
      | None => fail("the Polygons slide was not found, or not as expected")
      | Some((prefix, body, model)) => view_comes_back(~prefix, body, model)
      }
    ),
    not_compiled("a hole", "1 + ?", "a hole"),
    timing("the Petri net rule, four events", petri, 30),
    polygons_view("timing: the Polygons slide's view", `Slow, src =>
      timing_run("the Polygons slide's view", src, 30)
    ),
    test_case(
      "timing: the Polygons view through the boundary, a render", `Slow, () =>
      boundary_timing_run(30)
    ),
    test_case(
      "timing: the way back as a diff, one point of n changed", `Slow, () =>
      diff_timing_run()
    ),
    test_case(
      "the Polygons view drawn straight from JS, as HazelDOM draws it",
      `Quick,
      () =>
      switch (polygons_parts()) {
      | None => fail("the Polygons slide was not found, or not as expected")
      | Some((prefix, body, model_src)) =>
        let program = s => elaborate(parse_exp(prefix ++ s));
        let model = Test_Evaluator_Prelude.evaluate(program(model_src));
        switch (
          HazelJitValue.compile_view(
            ~env=Builtins.env_init,
            program("module P = " ++ body ++ " in P"),
          )
        ) {
        | Error(why) => fail("not compiled: " ++ why)
        | Ok(v) =>
          let js_answer =
            Js.Unsafe.fun_call(v.fn, [|HazelJitValue.to_js(model)|]);
          let same = drawn_both_ways(v, js_answer);
          if (String.length(same) < 4 || String.sub(same, 0, 4) != "same") {
            fail("not HazelDOM's drawing: " ++ same);
          };
        };
      }
    ),
    timing(
      "a recursive sum of 1,000",
      "let sum = fun n -> if n == 0 then 0 else n + sum(n - 1) in sum(1000)",
      30,
    ),
    ...List.map(diff_case, diff_cases),
  ]
  @ [
    /* (^ticket(714)) more of HazelDOM's forms than the Polygons view has, and a Bind: drawn straight from the view's JS,
       the same virtual DOM as through Hazel */
    test_case(
      "the Polygons update, compiled, its model back as a diff: the interpreter's model",
      `Quick,
      () =>
      polygons_update_run()
    ),
    test_case(
      "timing: the Polygons update, compiled, its model back as a diff",
      `Slow,
      () =>
      polygons_update_run(~timing=30, ())
    ),
    test_case(
      "a view of many forms, drawn straight from JS, as HazelDOM draws it",
      `Quick,
      () => {
        let def = {|{ type Action = + Go + Typed(String) + At((Int, Int)) + Wheel((Int, Int, Float, Float));
  let view = fun m : (n = Int, label = String) ->
    Bind(Pure(Text(m.label)), fun t -> Pure(Div([Class("c"), Classes(["a", "b"]), Id("x"), Title("t"), Data(("k", "v")), Style([("color", "red"), ("width", "3px")])],
      [t, Int(m.n), Float(2.5), Bool(true), Br, Hr([]),
       Span([OnClick(Go), OnDoubleClick(Go), OnMouseEnter(Go), OnFocus(Go)], [Text("s")]),
       Input([Type("text"), Value("v"), Placeholder("p"), Disabled(false), Checked(true), MaxLength(5), Name("n"), OnInput(fun s -> Typed(s)), OnChange(fun s -> Typed(s))]),
       TextArea([ReadOnly(true)], "words"),
       Select([Required(true)], [Option([Selected(true)], "one"), Option([], "two")]),
       Img([Src("a.png"), Alt("an a")]),
       A([Href("https://example.org"), Target("_blank")], [Text("a link")]),
       Table([], [Tr([], [Td([ColSpan(2)], [Text("cell")])])]),
       Button([OnClickAt(fun p -> At(p)), OnWheelAt(fun w -> Wheel(w)), BoolAttr(("hidden", false)), AutoFocus(true)], [Text("b")]),
       Node(("svg", [Width("10"), Height("10")], [Node(("circle", [Create(("r", "3"))], []))]))]))) }|};
        let model = {|(n = 7, label = "hello")|};
        let program = s => elaborate(parse_exp(s));
        switch (
          HazelJitValue.compile_view(
            ~env=Builtins.env_init,
            program("module P = " ++ def ++ " in P"),
          )
        ) {
        | Error(why) => fail("not compiled: " ++ why)
        | Ok(v) =>
          let js_answer =
            Js.Unsafe.fun_call(
              v.fn,
              [|
                HazelJitValue.to_js(
                  Test_Evaluator_Prelude.evaluate(program(model)),
                ),
              |],
            );
          let same = drawn_both_ways(v, js_answer);
          print_endline("hazel-jit many forms: " ++ same);
          if (String.length(same) < 4 || String.sub(same, 0, 4) != "same") {
            fail("not HazelDOM's drawing: " ++ same);
          };
        };
      },
    ),
  ],
);
