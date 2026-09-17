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

let tests = [
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
