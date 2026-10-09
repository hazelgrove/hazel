/**
 * Round-trip tests for the eval-worker encodings (issue #2368).
 *
 * Two things matter: Marshal (the active encoding) stays depth-proof through
 * structuredClone, and every encoding is isomorphic (decode ∘ encode = id) on a
 * normal program. Deep failure of direct/sexp is NOT asserted — raw
 * structuredClone on a deep graph overflows V8's native stack and segfaults
 * node uncatchably (in-browser it throws a catchable RangeError; that asymmetry
 * is why the app wraps the metrics path in try/catch).
 */
open Alcotest;
open Language;

/* The same serializer postMessage applies to its argument. */
let structured_clone: 'a. 'a => 'a =
  x =>
    Js_of_ocaml.Js.Unsafe.fun_call(
      Js_of_ocaml.Js.Unsafe.pure_js_expr(
        "(function (x) { return structuredClone(x); })",
      ),
      [|Js_of_ocaml.Js.Unsafe.inject(x)|],
    );

/* Encode -> (clone?) -> decode through one encoding; the abstract encoded type
 * stays inside the closure so this composes over all encodings uniformly. */
let rt_of_encoding =
    (encoding: (module WorkerServer.ENCODING))
    : (
        (~clone: bool, WorkerServer.ServerMessage.t) =>
        WorkerServer.ServerMessage.t
      ) => {
  module M = (val encoding);
  (~clone, msg) => {
    let w = M.encode_response(msg);
    let w = clone ? structured_clone(w) : w;
    M.decode_response(w);
  };
};

/* The evaluator time the worker reports back. Carried by the fixture so every
 * encoding is exercised on a Time_ns.Span crossing the boundary, not just on the
 * expression. */
let eval_time: Util.TimeUtil.span = Core.Time_ns.Span.of_ms(12.5);

let response_of_exp = (e: Exp.t): WorkerServer.ServerMessage.t =>
  WorkerServer.ServerMessage.Result({
    request_id: 1,
    response: [("cell", Ok((e, EvaluatorState.empty)))],
    eval_time,
  });

let parse = (s: string): Exp.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => fail("Failed to parse: " ++ s)
  };

/* Marshal must stay depth-proof: a `Parens`-nesting 20k deep round-trips
 * through structuredClone. Built iteratively so the fixture itself doesn't
 * recurse; compared as marshaled bytes since polymorphic `=` would recurse. */
let test_marshal_depth_proof = (): test_case(_) =>
  test_case(
    "Marshal: deep (20k) through structuredClone",
    `Quick,
    () => {
      let e = ref(Exp.fresh(EmptyHole));
      for (_ in 1 to 20000) {
        e := Exp.fresh(Parens(e^));
      };
      let rt = rt_of_encoding((module WorkerServer.MarshalEncoding));
      let resp = response_of_exp(e^);
      check(
        string,
        "round-trips to identical bytes",
        Marshal.to_string(resp, []),
        Marshal.to_string(rt(~clone=true, resp), []),
      );
    },
  );

/* Every encoding is isomorphic on a normal program: decode ∘ encode preserves
 * the expression. */
let test_isomorphic =
    (~name: string, encoding: (module WorkerServer.ENCODING)): test_case(_) =>
  test_case(
    name ++ ": isomorphic",
    `Quick,
    () => {
      let rt = rt_of_encoding(encoding);
      let e = parse("let x = [1, 2, 3] in x");
      switch (rt(~clone=true, response_of_exp(e))) {
      | WorkerServer.ServerMessage.Result({
          response: [("cell", Ok((e', _))), ..._],
          _,
        }) =>
        check(bool, "decoded equals original", true, Exp.fast_equal(e, e'))
      | _ => fail("round-trip did not preserve response shape")
      };
    },
  );

/* The Evaluation panel reads its `eval` column straight off the wire, so the
 * span has to survive the active encoding intact — it is an Int63 under the
 * hood, not a plain int or float. */
let test_eval_time_round_trips = (): test_case(_) =>
  test_case(
    "Marshal: evaluator time survives the round trip",
    `Quick,
    () => {
      let rt = rt_of_encoding((module WorkerServer.MarshalEncoding));
      switch (rt(~clone=true, response_of_exp(parse("1 + 1")))) {
      | WorkerServer.ServerMessage.Result({eval_time: span, _}) =>
        check(
          bool,
          "same span",
          true,
          Core.Time_ns.Span.equal(span, eval_time),
        )
      | _ => fail("round-trip did not preserve response shape")
      };
    },
  );

/* Span's yojson converters are hand-written (Core provides none), so pin the
 * representation: integer nanoseconds out, the same span back. Exercised on the
 * converters directly rather than through a whole message, since some types
 * inside a response define yojson converters that raise. */
let test_span_yojson = (): test_case(_) =>
  test_case(
    "yojson: a span round-trips as nanoseconds",
    `Quick,
    () => {
      let json = Util.TimeUtil.yojson_of_span(eval_time);
      check(
        string,
        "encoded as nanoseconds",
        Yojson.Safe.to_string(json),
        "12500000",
      );
      check(
        bool,
        "same span",
        true,
        Core.Time_ns.Span.equal(
          Util.TimeUtil.span_of_yojson(json),
          eval_time,
        ),
      );
    },
  );

/* The cost gate: a request takes the incremental path when its program is
 * large or its previous evaluation was expensive. Driven through the worker's
 * own per-item pipeline (`run_item_sync`: reuse plan, then sliced evaluation),
 * with id-preserving edits — re-parsing would mint fresh ids and nothing could
 * be reused regardless of the gate. */
let request =
    (~prev=IncrEval.empty, exp: Exp.t)
    : (WorkerServer.key, WorkerServer.Request.value) => {
  let (info_map, elab) = Test_Evaluator_Incremental.statics_and_elab(exp);
  (
    "cell",
    {
      expr: elab,
      eval_info_map:
        Test_Evaluator_Incremental.eval_info_of_statics(info_map),
      prev,
    },
  );
};

/* Run a request and return its plan, the cache it leaves, and its slices. */
let run = (~prev=?, exp: Exp.t) => {
  let (plan, response, slices) =
    WorkerServer.run_item_sync(request(~prev?, exp));
  switch (response) {
  | Ok((_, state)) => (plan, state.incr_eval, slices)
  | Error(_) => fail("evaluation failed")
  };
};

/* Id-preserving edit of the program's top-level `_ + n`: only the addend
 * literal's payload changes, every id stays (the zipper keeps the ids of
 * untouched tokens; this keeps even the edited one). */
let set_addend = (n: int, exp: Exp.t): Exp.t => {
  let rec go = (e: Exp.t): Exp.t =>
    switch (e.term) {
    | Let(p, def, body) => {
        ...e,
        term: Let(p, def, go(body)),
      }
    | BinOp(op, lhs, {term: Atom(Int(_)), _} as rhs) => {
        ...e,
        term:
          BinOp(
            op,
            lhs,
            {
              ...rhs,
              term: Atom(Int(Bigint.of_int(n))),
            },
          ),
      }
    | _ => fail("expected `let ... in _ + n`")
    };
  go(exp);
};

/* Id of the `fib(19)` application in the elaboration. */
let fib_19_id = (elab: Exp.t): Id.t => {
  let found = ref(None);
  let rec strip = (e: Exp.t) =>
    switch (e.term) {
    | Parens(e) => strip(e)
    | _ => e
    };
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Ap(_, _, arg) =>
      switch (strip(arg).term) {
      | Atom(Int(n)) when Bigint.to_string(n) == "19" =>
        found := Some(Exp.rep_id(e))
      | _ => ()
      }
    | _ => ()
    };
    continue(e);
  };
  ignore(TermBase.Exp.map_term(~f_exp, elab));
  switch (found^) {
  | Some(id) => id
  | None => fail("no fib(19) application in the elaboration")
  };
};

/* the bug report's program, verbatim */
let fib_src = "let fib = fun x -> if x < 1 then 1 else fib(x-1) + fib(x-2) in\nfib(19) + 1";

let test_expensive_small_program_reuses = (): test_case(_) =>
  test_case(
    "an expensive small program reuses its last evaluation",
    `Quick,
    () => {
      let exp1 = Test_Evaluator_Prelude.parse_exp(fib_src);
      let (_, req1) = request(exp1);
      check(
        bool,
        "small enough that the size gate alone would skip incremental",
        true,
        Id.Map.cardinal(req1.eval_info_map.statics)
        < WorkerServer.incremental_min_statics,
      );
      let (plan1, incr1, cold_slices) = run(exp1);
      check(
        bool,
        "cold run has nothing to reuse",
        true,
        IncrEval.is_empty(plan1),
      );
      /* the cold run went from scratch (no prev) and still recorded entries */
      let cold_steps = WorkerServer.prev_steps(incr1);
      check(
        bool,
        Printf.sprintf(
          "cold run is over the cost threshold (%d steps)",
          cold_steps,
        ),
        true,
        cold_steps >= WorkerServer.incremental_min_prev_steps,
      );

      let exp2 = set_addend(2, exp1);
      let (_, req2) = request(~prev=incr1, exp2);
      let fib_id = fib_19_id(req2.expr);
      let (plan2, incr2, warm_slices) = run(~prev=incr1, exp2);
      check(
        bool,
        "the reuse plan includes fib(19)",
        true,
        Id.Map.mem(fib_id, plan2.entries),
      );
      check(
        bool,
        Printf.sprintf(
          "warm run is far cheaper (%d slices vs %d cold)",
          warm_slices,
          cold_slices,
        ),
        true,
        warm_slices * 20 <= cold_slices,
      );
      /* replayed entries carry their steps, so the gate stays on */
      check(
        int,
        "cost survives reuse",
        cold_steps,
        WorkerServer.prev_steps(incr2),
      );

      let exp3 = set_addend(3, exp2);
      let (plan3, _, _) = run(~prev=incr2, exp3);
      check(
        bool,
        "and the next edit reuses fib(19) again",
        true,
        Id.Map.mem(fib_id, plan3.entries),
      );
    },
  );

let test_cheap_small_program_skips_prepass = (): test_case(_) =>
  test_case(
    "a cheap small program still evaluates from scratch",
    `Quick,
    () => {
      let exp1 =
        Test_Evaluator_Prelude.parse_exp(
          "let x = 1 + 2 in let y = x + 10 in y + 7",
        );
      let (_, incr1, _) = run(exp1);
      check(
        bool,
        "the cold run left a cache",
        false,
        IncrEval.is_empty(incr1),
      );
      let exp2 = set_addend(2, exp1);
      check(
        bool,
        "the cache has something reusable for the edit",
        true,
        Test_Evaluator_Incremental.has_reuse(
          Test_Evaluator_Incremental.reuse_plan(~prev=incr1, exp2),
        ),
      );
      let (plan2, _, _) = run(~prev=incr1, exp2);
      check(
        bool,
        "the worker skips the pre-pass anyway",
        true,
        IncrEval.is_empty(plan2),
      );
    },
  );

let tests = [
  (
    "WorkerServer cost gate",
    [
      test_expensive_small_program_reuses(),
      test_cheap_small_program_skips_prepass(),
    ],
  ),
  (
    "WorkerServer encodings",
    [
      test_marshal_depth_proof(),
      test_eval_time_round_trips(),
      test_span_yojson(),
      test_isomorphic(~name="Marshal", (module WorkerServer.MarshalEncoding)),
      test_isomorphic(~name="Direct", (module WorkerServer.DirectEncoding)),
      test_isomorphic(~name="Sexp", (module WorkerServer.SexpEncoding)),
    ],
  ),
];
