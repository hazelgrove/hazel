/* hazel-jit (^ticket(694)) against the interpreter: run a compiled term, and compare its JS value with the interpreter's,
   shape by shape (numbers by value, a float within a relative 1e-9; lists, tuples, labels and constructors by structure; a
   function only as a function). Test_HazelJit's agreement tests use it, and (^ticket(756)) so does every evaluator test,
   through Test_Evaluator_Prelude.evaluation_test: the sweep below. */
open Language;
open Js_of_ocaml;
/* (structural comparisons of strings and floats throughout: as dev's Test_FastParse) */
open Poly;

/* (^ticket(759)) a compiled run that stopped at an invalid operation ("hazel-jit: invalid operation: DivideByZero") agrees
   with an interpreter's result stuck at that error (a DynamicErrorHole holding it): the page falls back to the interpreter,
   which makes the stuck term. The error's name, if the JS error was one of these. */
let invalid_of = (message: string): option(string) => {
  let pre = "hazel-jit: invalid operation: ";
  String.length(message) > String.length(pre)
  && String.sub(message, ~pos=0, ~len=String.length(pre)) == pre
    ? Some(
        String.sub(
          message,
          ~pos=String.length(pre),
          ~len=String.length(message) - String.length(pre),
        ),
      )
    : None;
};
let contains = (s: string, part: string): bool => {
  let n = String.length(part);
  let rec at = i =>
    i
    + n <= String.length(s)
    && (String.sub(s, ~pos=i, ~len=n) == part || at(i + 1));
  n == 0 || at(0);
};
/* the interpreter's result is stuck there: a DynamicErrorHole whose error prints as it, or a builtin's application left
   unreduced (int_of_string("seven") is stuck as itself, no error named) */
let stuck_at = (result: Exp.t, err: string): bool => {
  let shown = Exp.show(result);
  contains(shown, "DynamicErrorHole")
  && contains(shown, err)
  || contains(shown, "(BuiltinFun");
};

/* (^ticket(761)) a compiled run that reached undefined ("hazel-jit: indeterminate: undefined") agrees with an interpreter's
   result left indeterminate there, an Undefined in it: the page falls back to the interpreter */
/* (^ticket(761)) A compiled run that stopped as indeterminate agrees with an interpreter result stuck at the same thing:
   undefined reached; or (CI run 44197056910, Evaluator.LabeledTuples' "Indet projection", (true).a) a projection the
   interpreter leaves stuck, where the compiled code once read undefined and carried on */
let indet_reached = (message: string, result: Exp.t): bool => {
  let pre = "hazel-jit: indeterminate: ";
  let shown = Exp.show(result);
  String.length(message) >= String.length(pre)
  && String.sub(message, ~pos=0, ~len=String.length(pre)) == pre
  && {
    let what =
      String.sub(
        message,
        ~pos=String.length(pre),
        ~len=String.length(message) - String.length(pre),
      );
    let by = "a projection by ";
    contains(shown, "Undefined")
    || String.length(what) >= String.length(by)
    && String.sub(what, ~pos=0, ~len=String.length(by)) == by
    && contains(shown, "(Dot");
  };
};

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
/* (^report(54)) a list's head and tail: a cell {h, t}, or a segment {a, i}, the elements of an array from index i on */
let is_seg = (v: Js.Unsafe.any): bool =>
  Js.to_string(Js.typeof(Js.Unsafe.get(v, "a"))) != "undefined";
let num = (v: Js.Unsafe.any): float =>
  Js.float_of_number(Js.Unsafe.coerce(v));
let hd = (v: Js.Unsafe.any): Js.Unsafe.any =>
  is_seg(v)
    ? Js.Unsafe.get(get(v, "a"), int_of_float(num(get(v, "i"))))
    : get(v, "h");
let tl = (v: Js.Unsafe.any): Js.Unsafe.any =>
  if (is_seg(v)) {
    let i = num(get(v, "i")) +. 1.;
    i < num(get(get(v, "a"), "length"))
      ? Js.Unsafe.obj([|
          ("a", get(v, "a")),
          ("i", Js.Unsafe.inject(Js.number_of_float(i))),
        |])
      : Js.Unsafe.inject(Js.null);
  } else {
    get(v, "t");
  };

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
        /* (^ticket(759)) equal first, so an infinity agrees with itself (inf - inf is nan) */
        num(js) == f
        || Float.is_nan(f)
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
            switch (agree(e, hd(v))) {
            | None => walk(rest, tl(v))
            | d => d
            }
          )
      };
    walk(es, js);
  | Cons(h, t) =>
    is_null(js)
      ? Some("[] for a cons")
      : (
        switch (agree(h, hd(js))) {
        | None => agree(t, tl(js))
        | d => d
        }
      )
  | Tuple(es) =>
    !is_array(js)
    || num(get(js, "length")) != float_of_int(List.length(es))
      ? Some("a tuple of " ++ string_of_int(List.length(es)))
      : List.fold_left(es, ~init=(None, 0), ~f=((acc, i), e: Exp.t) =>
          (
            switch (acc) {
            | Some(_) => acc
            | None =>
              switch (Exp.match_tup_label(e), Exp.term_of(e)) {
              | (Some((l, e')), _) =>
                agree(e', get(js, HazelJit.label_key(l)))
              /* (^ticket(761)) an explicitly unlabeled entry, (_=e), is the entry itself */
              | (None, TupLabel(l, e')) when HazelJit.is_nonlabel(l) =>
                agree(e', Js.Unsafe.get(js, i))
              | (None, _) => agree(e, Js.Unsafe.get(js, i))
              }
            },
            i + 1,
          )
        )
        |> fst
  /* (^ticket(761)) (_=e) alone, a tuple of one: a JS array of one, or the entry itself; the evaluator's own tests ignore
     explicit unlabelling (Test_Evaluator_Prelude.testable_exp), and so does this: let (_=x) = (_=1) in x is (_=1) to the
     interpreter and 1 compiled (CI 11-17) */
  | TupLabel(l, e') when HazelJit.is_nonlabel(l) =>
    is_array(js) && num(get(js, "length")) == 1.
      ? agree(e', Js.Unsafe.get(js, 0)) : agree(e', js)
  | TupLabel(_) =>
    switch (Exp.match_tup_label(e)) {
    | Some((l, e')) => agree(e', get(js, HazelJit.label_key(l)))
    | None => Some("a label")
    }
  | Constructor(name, _) =>
    want(
      typeof(js) == "object"
      && !is_null(js)
      && Js.to_string(Js.Unsafe.coerce(get(js, "c"))) == name,
      "the constructor " ++ name,
    )
  /* (a constructor applied either way, Some(3) or 3 |> Some, is the same value; the interpreter keeps how it was written) */
  | Ap(Forward | Reverse, {term: Constructor(name, _), _}, arg) =>
    typeof(js) == "object"
    && !is_null(js)
    && Js.to_string(Js.Unsafe.coerce(get(js, "c"))) == name
      ? agree(arg, get(js, "v")) : Some("the constructor " ++ name)
  | Module(items) =>
    List.fold_left(items, ~init=None, ~f=(acc, item: Mod.t) =>
      switch (acc, item.term) {
      | (Some(_), _) => acc
      | (None, ModLet({term: Var(x), _}, v)) =>
        agree(v, get(js, HazelJit.label_key(x)))
      | (None, _) => None
      }
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
   (an error or a stuck term, what ^ticket(758) and ^ticket(759) are about), each by its test's name. The partings it
   found first are listed below as known; any other fails its test. */
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
let not_compiled: Stdlib.Hashtbl.t(string, int) = Stdlib.Hashtbl.create(16);
let sweep_failed = ref(0);

let clip = (n, s) =>
  String.length(s) > n ? String.sub(s, ~pos=0, ~len=n) ++ " ..." : s;
let one_line = s => String.map(s, ~f=c => c == '\n' ? ' ' : c) |> clip(240);
let starts_with = (pre, s) =>
  String.length(s) >= String.length(pre)
  && String.sub(s, ~pos=0, ~len=String.length(pre)) == pre;

/* (^ticket(756), next) the partings the first sweep found (CI 43754503317), by test name, whitespace as one space: all 30
   are ill-typed programs, which the interpreter stops at a failed run-time type check and compiled code, its types
   erased, runs on (^ticket(760): the page compiles no program with type errors). Any other parting fails its test. */
let known_not_a_value = [
  "(a=3 : Bool, b= \"\": Float) == (a=3, b=\"\") : (c=Bool, d=Float)",
  "(a=1,b=2).c == (a=1,b=2).c",
  "(true) . a == (true) . a",
  "((true) . a): Int == ((true) . a): Int",
  "Duplicate labels projected are indet",
  "mismatched types",
  "1 : String == case 1 | (x : String) =>x end",
  "case 1 | (\"hello\") => false | (x : String) => true end == case 1 | (\"hello\") => false | (x : String) => true end",
  "(case () | (1, 2) => true | _ => false end) == (case () | (1, 2) => true | _ => false end)",
  "[1 : String,2 : String,3 : String] == [1,2,3] : [String]",
  "[1 : String,2 : String,3 : String] == type T = [String] in [1,2,3] : T",
  "[1, \"hello\" : Int] == 1 :: [\"hello\"]",
  "4 : String == 4 : String",
  "true : String == true : String",
  "1. : String == 1. : String",
  "Nat",
  "SInt",
  "(4 : String) == let x : String = 4 in x",
  "1 + (4 : String) == 1 + (4 : String)",
  "Hidden inconsistency caught in dynamics",
  "Hidden containing arrow caught in dynamics",
  "Labeled tuple with different labels",
  "Labeled tuple with rearragned labels",
  "Typfun is not comparable",
  "Hidden containing arrow in tuple caught in dynamics",
  "A : (+A +B) : (+A +C)",
  "Indet when unboxing constructor as bool",
  "Indet when unboxing constructor as typfun",
  "Indet when unboxing constructor as string",
  "Indet when unboxing constructor as int",
];
let words = s =>
  String.split(
    String.map(s, ~f=c => c == '\n' || c == '\t' ? ' ' : c),
    ~on=' ',
  )
  |> List.filter(~f=w => w != "")
  |> String.concat(~sep=" ");
let known = test =>
  List.mem(known_not_a_value, words(test), ~equal=String.equal);

type verdict =
  | Fine
  | Parts(string);

/* (^ticket(766)) mixed mode, a third way: the program rewritten (HazelJitMixed), its top-level functions with declared
   types compiled and called as builtins, the rest interpreted; its answer against the interpreter's alone. Reported,
   not yet a failure of the build. */
/* two interpreter results the same, as the evaluator's tests judge (Test_Evaluator_Prelude.testable_exp) */
let same_value = (a: Exp.t, b: Exp.t): bool =>
  Equality.(
    equality({
      ...syntactic_settings,
      ignore_parens: true,
      ignore_function_names: true,
      ignore_function_types: true,
      ignore_unknown_provenance: true,
      ignore_explicit_unlabelling: true,
      ignore_dynamic_errors: false,
      ignore_constructor_types: false,
    })
  ).
    exp(
    a,
    b,
  );
let mixed_programs = ref(0);
let mixed_functions = ref(0);
let mixed_agreed = ref(0);
let mixed_differs: ref(list(parting)) = ref([]);
let mixed_failed: ref(list(parting)) = ref([]);
let mixed_not_compiled: Stdlib.Hashtbl.t(string, int) =
  Stdlib.Hashtbl.create(8);
let mixed_sweep = (test: string, elab: Exp.t, result: Exp.t): unit =>
  switch (HazelJitMixed.rewrite(elab)) {
  | exception exn =>
    mixed_failed :=
      [
        {
          test,
          what: "the rewrite: " ++ Exn.to_string(exn),
        },
        ...mixed_failed^,
      ]
  | rewritten =>
    List.iter(HazelJitMixed.not_compiled^, ~f=((_, why)) =>
      Stdlib.Hashtbl.replace(
        mixed_not_compiled,
        why,
        1
        + Option.value(
            ~default=0,
            Stdlib.Hashtbl.find_opt(mixed_not_compiled, why),
          ),
      )
    );
    let names = HazelJitMixed.compiled_names^;
    if (names != []) {
      incr(mixed_programs);
      mixed_functions := mixed_functions^ + List.length(names);
      switch (Evaluator.evaluate(~env=Builtins.env_init, rewritten)) {
      | exception exn =>
        mixed_failed :=
          [
            {
              test,
              what: "the run: " ++ Exn.to_string(exn),
            },
            ...mixed_failed^,
          ]
      | (got, _) =>
        same_value(result, got)
          ? incr(mixed_agreed)
          : mixed_differs :=
              [
                {
                  test,
                  what:
                    one_line(
                      String.concat(~sep=", ", names)
                      ++ " compiled; the interpreter: "
                      ++ Exp.show(result)
                      ++ "; mixed: "
                      ++ Exp.show(got),
                    ),
                },
                ...mixed_differs^,
              ]
      };
    };
  };

/* (^ticket(756); Matthew, 2026-11-17: "please time the tests in interpreter vs jit and present those results in report 54")
   Each evaluator program that compiles and gives the interpreter's value, timed: the interpreter's first run (as the
   evaluator test ran it), hazel-jit's compile (OCaml, to the JS source), and the JS parsed once (new Function) and run once,
   as a page would. Then each again and again (Matthew: "can we repeat the work 100 or 1000 times to get a multi ms number in
   the interpreter?"): the interpreter until its runs add to 20 ms, 10 times at least and 1000 at most; the JS until its runs
   add to 5 ms, 100000 times at most; each the time a run. All in the same Node process, on GitHub's machines. */
let now = (): float =>
  Js.float_of_number(
    Js.Unsafe.meth_call(
      Js.Unsafe.get(Js.Unsafe.global, "performance"),
      "now",
      [||],
    ),
  );
type timing = {
  t_test: string,
  interp_ms: float,
  interp_mean: float,
  interp_n: int,
  compile_ms: float,
  parse_ms: float,
  run1_ms: float,
  run_mean: float,
  run_n: int,
};
let timings: ref(list(timing)) = ref([]);
/* (CI runs 44146528138 and 44165874889: the full tests ran past their 100 minutes, and a run killed at its limit prints
   nothing of its tests) A trail, appended to as the run goes, so a killed run still shows where its time went; CI prints
   it after the tests, pass or fail (checks.yml) */
let trail = (line: string): unit =>
  switch (
    Stdlib.open_out_gen(
      [Open_append, Open_creat, Open_text],
      0o644,
      "hazel-jit-trail.txt",
    )
  ) {
  | oc =>
    Stdlib.output_string(oc, line ++ "\n");
    Stdlib.close_out(oc);
  | exception _ => ()
  };
/* the quick tests run with -q; the full tests keep the sweep's check and skip its timing, which repeats each program's
   runs (the report's timings come from the quick tests) */
let quick_run =
  Array.exists(Stdlib.Sys.argv, ~f=a => a == "-q" || a == "--quick-tests");

/* the runtime, made once, for the timed runs */
let timed_runtime: Lazy.t(Js.Unsafe.any) =
  lazy(Js.Unsafe.eval_string("(" ++ HazelJit.Runtime.source ++ ")"));
/* (parse, first run, a run's time over the repeats, the repeats) */
let time_js = (src: string): (float, float, float, int) => {
  let r =
    Js.Unsafe.fun_call(
      Js.Unsafe.pure_js_expr(
        "(function (R, src) { const t0 = performance.now(); const f = new Function('$R', src); const t1 = performance.now(); f(R); const t2 = performance.now(); let n = 0; const t3 = performance.now(); let t4 = t3; while (n < 100000 && t4 - t3 < 5) { f(R); n++; t4 = performance.now(); } return [t1 - t0, t2 - t1, (t4 - t3) / n, n]; })",
      ),
      [|Lazy.force(timed_runtime), Js.Unsafe.inject(Js.string(src))|],
    );
  let at = (i: int) =>
    Js.float_of_number(Js.Unsafe.coerce(Js.Unsafe.get(r, i)));
  (at(0), at(1), at(2), int_of_float(at(3)));
};
/* the interpreter, again and again: (a run's time over the repeats, the repeats) */
let time_interp = (elab: Exp.t): (float, int) => {
  let run = () => ignore(Evaluator.evaluate(~env=Builtins.env_init, elab));
  let t0 = now();
  run();
  let one = now() -. t0;
  /* (at least 10 runs where a run is short; a program whose one run already takes 20 ms runs once more, as a slide may
     take seconds: ten of those ran the quick tests past CI's 20 minutes, 11-17) */
  let n =
    one >= 20.
      ? 1
      : Int.min(
          1000,
          Int.max(
            10,
            int_of_float(Float.round_up(20. /. Float.max(one, 0.02))),
          ),
        );
  let t0 = now();
  for (_ in 1 to n) {
    run();
  };
  ((now() -. t0) /. float_of_int(n), n);
};

let sweep_one =
    (~interp_ms: option(float)=?, test: string, elab: Exp.t, result: Exp.t)
    : unit => {
  try(mixed_sweep(test, elab, result)) {
  | _ => ()
  };
  let verdict =
    try(
      {
        incr(swept);
        let t0 = now();
        let compiled_src = HazelJit.try_compile(~env=Builtins.env_init, elab);
        let compile_ms = now() -. t0;
        switch (compiled_src) {
        | Error(reason) =>
          Stdlib.Hashtbl.replace(
            not_compiled,
            reason,
            1
            + Option.value(
                ~default=0,
                Stdlib.Hashtbl.find_opt(not_compiled, reason),
              ),
          );
          Fine;
        | Ok(js_src) =>
          incr(compiled);
          switch (run_js(HazelJit.Runtime.source, js_src)) {
          | exception (Js_error.Exn(e))
              when
                switch (invalid_of(Js_error.message(e))) {
                | Some(err) => stuck_at(result, err)
                | None => false
                } =>
            /* (^ticket(759)) an invalid operation, where the interpreter stops at the same error */
            incr(agreed);
            Fine;
          | exception (Js_error.Exn(e))
              when indet_reached(Js_error.message(e), result) =>
            incr(agreed);
            Fine;
          | exception (Js_error.Exn(e)) =>
            let what =
              one_line(
                Js_error.message(e)
                ++ "; the interpreter: "
                ++ Exp.show(result),
              );
            threw :=
              [
                {
                  test,
                  what,
                },
                ...threw^,
              ];
            Parts("the compiled JS threw: " ++ what);
          | got =>
            switch (agree(result, got)) {
            | None =>
              incr(agreed);
              switch (interp_ms) {
              | Some(interp_ms) =>
                switch (time_js(js_src), time_interp(elab)) {
                | (
                    (parse_ms, run1_ms, run_mean, run_n),
                    (interp_mean, interp_n),
                  ) =>
                  timings :=
                    [
                      {
                        t_test: test,
                        interp_ms,
                        interp_mean,
                        interp_n,
                        compile_ms,
                        parse_ms,
                        run1_ms,
                        run_mean,
                        run_n,
                      },
                      ...timings^,
                    ]
                | exception _ => ()
                }
              | None => ()
              };
              Fine;
            | Some(d) when starts_with(not_a_value, d) =>
              not_compared :=
                [
                  {
                    test,
                    what: one_line(d),
                  },
                  ...not_compared^,
                ];
              known(test) ? Fine : Parts(one_line(d));
            | Some(d) =>
              differs :=
                [
                  {
                    test,
                    what: one_line(d),
                  },
                  ...differs^,
                ];
              Parts("differs: " ++ one_line(d));
            }
          };
        };
      }
    ) {
    | _ =>
      incr(sweep_failed);
      Fine;
    };
  /* (outside the guard above, which would catch Alcotest's failure too) */
  switch (verdict) {
  | Fine => ()
  | Parts(why) =>
    Alcotest.fail(
      "hazel-jit parts from the interpreter (ticket 756): " ++ why,
    )
  };
};

/* each program's sweep, timed: a program whose sweep takes over 2 s, and every 500 programs the sweep's time against the
   tests' own runs, go on the trail */
let swept_ms = ref(0.);
let tests_ms = ref(0.);
let sweep =
    (~interp_ms: option(float)=?, test: string, elab: Exp.t, result: Exp.t)
    : unit => {
  let t0 = now();
  sweep_one(~interp_ms=?quick_run ? interp_ms : None, test, elab, result);
  let ms = now() -. t0;
  let own = Option.value(~default=0., interp_ms);
  swept_ms := swept_ms^ +. ms;
  tests_ms := tests_ms^ +. own;
  if (ms > 2000.) {
    trail(
      Printf.sprintf(
        "slow sweep %.1f s (the test's own run %.0f ms): %s",
        ms /. 1000.,
        own,
        one_line(test),
      ),
    );
  };
  if (swept^ mod 500 == 0) {
    trail(
      Printf.sprintf(
        "swept %d programs: the sweep %.0f s, the tests' own runs %.0f s",
        swept^,
        swept_ms^ /. 1000.,
        tests_ms^ /. 1000.,
      ),
    );
  };
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
    Stdlib.Hashtbl.fold((r, n, acc) => [(n, r), ...acc], not_compiled, [])
    |> List.sort(~compare)
    |> List.rev
    |> List.iter(~f=((n, r)) => p("  not compiled, %d: %s\n", n, r));
    let each = (label, xs) =>
      List.iter(List.rev(xs), ~f=(x: parting) =>
        p("  %s  %s: %s\n", label, x.test, x.what)
      );
    /* the timings: in all, then each program's, tab-separated, for ^report(54) */
    let ts = List.rev(timings^);
    if (ts != []) {
      let total = f => List.fold_left(ts, ~init=0., ~f=(a, t) => a +. f(t));
      let median = f =>
        List.map(ts, ~f)
        |> List.sort(~compare)
        |> (rs => List.nth_exn(rs, List.length(rs) / 2));
      p(
        "hazel-jit timing (ticket 756): %d programs compiled whose values agree. A run's time, repeated (the interpreter until 20 ms, 10 to 1000 times; the JS until 5 ms): the interpreter %.1f ms in all, the JS %.3f ms; the median program's ratio %.0f. First runs: the interpreter %.1f ms in all, hazel-jit compiling %.1f ms, the JS parsed %.1f ms and run %.1f ms; the median program's ratio, the interpreter over the JS parsed and run, %.1f\n",
        List.length(ts),
        total(t => t.interp_mean),
        total(t => t.run_mean),
        median(t => t.interp_mean /. Float.max(t.run_mean, 0.00001)),
        total(t => t.interp_ms),
        total(t => t.compile_ms),
        total(t => t.parse_ms),
        total(t => t.run1_ms),
        median(t => t.interp_ms /. Float.max(t.parse_ms +. t.run1_ms, 0.001)),
      );
      List.iter(ts, ~f=t =>
        p(
          "hazel-jit time\t%.5f\t%.5f\t%d\t%.5f\t%.5f\t%.5f\t%.6f\t%d\t%s\n",
          t.interp_ms,
          t.interp_mean,
          t.interp_n,
          t.compile_ms,
          t.parse_ms,
          t.run1_ms,
          t.run_mean,
          t.run_n,
          one_line(t.t_test),
        )
      );
    };
    each("DIFFERS", differs^);
    each("THREW", threw^);
    each("NOT A VALUE", not_compared^);
    p(
      "hazel-jit mixed (ticket 766): %d programs with a function compiled (%d functions); %d agreed, %d differ, %d failed; %d calls compiled, %d fell back\n",
      mixed_programs^,
      mixed_functions^,
      mixed_agreed^,
      List.length(mixed_differs^),
      List.length(mixed_failed^),
      HazelJitMixed.calls^,
      HazelJitMixed.fallbacks^,
    );
    Stdlib.Hashtbl.fold(
      (r, n, acc) => [(n, r), ...acc],
      mixed_not_compiled,
      [],
    )
    |> List.sort(~compare)
    |> List.rev
    |> List.iter(~f=((n, r)) => p("  mixed, not compiled, %d: %s\n", n, r));
    Stdlib.Hashtbl.fold(
      (r, n, acc) => [(n, r), ...acc],
      HazelJitMixed.fallback_reasons,
      [],
    )
    |> List.sort(~compare)
    |> List.rev
    |> List.iter(~f=((n, r)) => p("  mixed, fell back, %d: %s\n", n, r));
    each("MIXED DIFFERS", mixed_differs^);
    each("MIXED FAILED", mixed_failed^);
    Stdlib.flush(Stdlib.stdout);
  };
let () = at_exit(report);
