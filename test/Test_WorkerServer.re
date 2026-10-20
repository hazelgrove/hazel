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
    final_streams: [],
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

/* streams and the reuse plan ship only what the main thread reads: entry
   keys, [seq] and states; the reuse payload stays in the worker */
let test_slim_entries = (): test_case(_) =>
  test_case(
    "slimmed entries keep keys and order, drop the payload",
    `Quick,
    () => {
      let id = Util.Id.mk();
      let entry: IncrEval.entry(EvaluatorState.t) = {
        prev_elab: parse("1 + 2"),
        prev_ana: Typ.fresh(Unknown(Internal)),
        prev_reuse_map: IncrEval.empty_reuse_map,
        prev_probe_targets: EvalInfo.ProbeTargets(SubexpProbeTargets.empty),
        value: parse("3"),
        state: EvaluatorState.empty,
        seq: 7,
      };
      let slim =
        WorkerServer.slim_entries(
          IncrEval.add_entry(id, entry, IncrEval.empty),
        );
      switch (Util.Id.Map.find_opt(id, slim.entries)) {
      | None => fail("entry dropped")
      | Some(e) =>
        check(int, "seq kept", 7, e.seq);
        check(bool, "value dropped", true, e.value.term == EmptyHole);
        check(bool, "elab dropped", true, e.prev_elab.term == EmptyHole);
      };
    },
  );

/* Responses are shared like requests (WorkerServer.share_response on the
   worker, WorkerClient.unshare on the client), over the active encoding and
   structuredClone as on the real boundary. The two tables live in one JS
   runtime here. */
let shared_rt = (msg: WorkerServer.ServerMessage.t) => {
  module M = WorkerServer.MarshalEncoding;
  let w = M.encode_response(WorkerServer.share_response(msg));
  let w = structured_clone(w);
  (String.length(Obj.magic(w): string), M.decode_response(w));
};
let reset_sharing = () => {
  Util.ShareCodec.Client.reset();
  Util.ShareCodec.Worker.reset();
  WorkerServer.response_reset := true;
  Web.WorkerClient.awaiting_response_reset := false;
};
let result_exp = (msg: WorkerServer.ServerMessage.t): Exp.t =>
  switch (msg) {
  | WorkerServer.ServerMessage.Result({
      response: [("cell", Ok((e, _))), ..._],
      _,
    }) => e
  | _ => fail("not a result")
  };
let unshared = (wire: WorkerServer.ServerMessage.t) =>
  switch (Web.WorkerClient.unshare(wire)) {
  | Some(Ok(msg)) => msg
  | Some(Error ()) => fail("unshare: unknown reference")
  | None => fail("unshare: ignored")
  };
let a_result = (~id: int, e: Exp.t): WorkerServer.ServerMessage.t =>
  WorkerServer.ServerMessage.Result({
    request_id: id,
    response: [("cell", Ok((e, EvaluatorState.empty)))],
    eval_time,
    final_streams: [],
  });

let test_shared_responses = (): test_case(_) =>
  test_case(
    "Shared responses: round trip, and an unchanged result is small and shared",
    `Quick,
    () => {
      reset_sharing();
      let e = Test_ShareCodec.elab(Test_ShareCodec.program(40));
      let (first, wire1) = shared_rt(a_result(~id=1, e));
      let (second, wire2) = shared_rt(a_result(~id=2, e));
      check(
        bool,
        "the first resets the client's table",
        true,
        switch (wire1) {
        | WorkerServer.ServerMessage.Shared(true, _) => true
        | _ => false
        },
      );
      let e1 = result_exp(unshared(wire1));
      let e2 = result_exp(unshared(wire2));
      check(bool, "the result arrives", true, Util.ValueHash.same(e, e1));
      check(bool, "the second is much smaller", true, second * 5 < first);
      check(bool, "and is the very value received", true, e1 === e2);
    },
  );

let test_shared_response_recovery = (): test_case(_) =>
  test_case(
    "Shared responses: a value the client lost is an error, then ignored until the worker resets",
    `Quick,
    () => {
      reset_sharing();
      let e = Test_ShareCodec.elab(Test_ShareCodec.program(10));
      ignore(unshared(snd(shared_rt(a_result(~id=1, e)))));
      Util.ShareCodec.Worker.reset(); /* the client's table is lost */
      let after = snd(shared_rt(a_result(~id=2, e)));
      check(
        bool,
        "an error",
        true,
        Web.WorkerClient.unshare(after) == Some(Error()),
      );
      let stale = snd(shared_rt(a_result(~id=3, e)));
      check(
        bool,
        "the rest is ignored",
        true,
        Web.WorkerClient.unshare(stale) == None,
      );
      WorkerServer.response_reset := true; /* ResetResponses */
      let fresh = snd(shared_rt(a_result(~id=4, e)));
      check(
        bool,
        "after the reset, whole again",
        true,
        Util.ValueHash.same(e, result_exp(unshared(fresh))),
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
      test_slim_entries(),
      test_isomorphic(~name="Marshal", (module WorkerServer.MarshalEncoding)),
      test_isomorphic(~name="Direct", (module WorkerServer.DirectEncoding)),
      test_isomorphic(~name="Sexp", (module WorkerServer.SexpEncoding)),
      test_shared_responses(),
      test_shared_response_recovery(),
    ],
  ),
];
