/* hazel-jit (^ticket(694)) against the interpreter: run a compiled term, and compare its JS value with the interpreter's,
   shape by shape (numbers by value, a float within a relative 1e-9; lists, tuples, labels and constructors by structure; a
   function only as a function). Test_HazelJit's agreement tests use it, and (^ticket(756)) so does every evaluator test,
   through Test_Evaluator_Prelude.evaluation_test: the sweep below. */
open Language;
open Js_of_ocaml;

/* what agree says of an interpreter result it does not compare (an error, a stuck term: not a value) */
let not_a_value = "a value hazel-jit's tests do not compare";

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
  /* (^tickets(756, 757)) exactly: its digits, a JS number's or a BigInt's, against the interpreter's Bigint */
  | Atom(Int(n))
  | Atom(Nat(n)) =>
    want(
      (typeof(js) == "number" || typeof(js) == "bigint")
      && Js.to_string(
           Js.Unsafe.fun_call(Js.Unsafe.js_expr("String"), [|js|]),
         )
      == Bigint.to_string(n),
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
  | _ => Some(not_a_value ++ ": " ++ Exp.show(e))
  };
};

/* (^ticket(756); Matthew and Cyrus, 2026-11-13: "we want semantic consistency with Hazel the interpreter, and to ensure that
   with CI") Every program of the evaluator's own tests (Test_Evaluator_Prelude.evaluation_test: 314 checks) is run by
   hazel-jit too, where it compiles, and its value compared with the interpreter's. Reported, not yet checked: at the end of
   the run, a summary in the log, how many compiled, agreed, differ, threw, or have an interpreter result that is not a value
   (an error or a stuck term, what ^ticket(758) and ^ticket(759) are about), each by its test's name. Next, the partings it
   finds are listed here as known, and a new one fails the test. */
type parting = {
  test: string,
  what: string,
};
let swept = ref(0);
let compiled = ref(0);
let agreed = ref(0);
let differs: ref(list(parting)) = ref([]);
let threw: ref(list(parting)) = ref([]);
let not_compared: ref(list(parting)) = ref([]);
let not_compiled: Hashtbl.t(string, int) = Hashtbl.create(16);
let sweep_failed = ref(0);

let clip = (n, s) =>
  String.length(s) > n ? String.sub(s, 0, n) ++ " ..." : s;
let one_line = s => String.map(c => c == '\n' ? ' ' : c, s) |> clip(240);
let starts_with = (pre, s) =>
  String.length(s) >= String.length(pre)
  && String.sub(s, 0, String.length(pre)) == pre;

let sweep = (test: string, elab: Exp.t, result: Exp.t): unit =>
  try(
    {
      incr(swept);
      switch (HazelJit.try_compile(~env=Builtins.env_init, elab)) {
      | Error(reason) =>
        Hashtbl.replace(
          not_compiled,
          reason,
          1
          + Option.value(~default=0, Hashtbl.find_opt(not_compiled, reason)),
        )
      | Ok(js_src) =>
        incr(compiled);
        switch (run_js(HazelJit.Runtime.source, js_src)) {
        | exception (Js_error.Exn(e)) =>
          threw :=
            [
              {
                test,
                what:
                  one_line(
                    Js_error.message(e)
                    ++ "; the interpreter: "
                    ++ Exp.show(result),
                  ),
              },
              ...threw^,
            ]
        | got =>
          switch (agree(result, got)) {
          | None => incr(agreed)
          | Some(d) when starts_with(not_a_value, d) =>
            not_compared :=
              [
                {
                  test,
                  what: one_line(d),
                },
                ...not_compared^,
              ]
          | Some(d) =>
            differs :=
              [
                {
                  test,
                  what: one_line(d),
                },
                ...differs^,
              ]
          }
        };
      };
    }
  ) {
  | _ => incr(sweep_failed)
  };

let report = () =>
  if (swept^ > 0) {
    let p = Printf.printf;
    p(
      "\nhazel-jit sweep (ticket 756): %d evaluator checks; %d compiled, %d agreed, %d differ, %d threw, %d with an interpreter result that is not a value; the sweep itself failed on %d\n",
      swept^,
      compiled^,
      agreed^,
      List.length(differs^),
      List.length(threw^),
      List.length(not_compared^),
      sweep_failed^,
    );
    Hashtbl.fold((r, n, acc) => [(n, r), ...acc], not_compiled, [])
    |> List.sort(compare)
    |> List.rev
    |> List.iter(((n, r)) => p("  not compiled, %d: %s\n", n, r));
    let each = (label, xs) =>
      List.iter(
        (x: parting) => p("  %s  %s: %s\n", label, x.test, x.what),
        List.rev(xs),
      );
    each("DIFFERS", differs^);
    each("THREW", threw^);
    each("NOT A VALUE", not_compared^);
    flush(stdout);
  };
let () = at_exit(report);
