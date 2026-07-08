/**
 * Round-trip tests for the eval-worker wire protocols (issue #2368).
 *
 * Each WorkerServer.WIRE variant is exercised through the same battery: a
 * payload is encoded, optionally run through structuredClone (the same
 * serializer postMessage hands payloads to — so this is the real boundary),
 * then decoded. These run under js_of_ocaml (node); node's test stack is 8MB,
 * so the deep cases guard the encoding and round-trip fidelity rather than
 * Chrome's smaller clone-stack limit.
 *
 * Coverage is per variant by what each can safely carry:
 *   - marshal is depth-proof — deep + wide + shallow.
 *   - direct, sexp only get shallow/narrow payloads. Their failure on
 *     deep input is NOT asserted here: raw structuredClone on a deep graph
 *     overflows V8's *native* stack, which segfaults node uncatchably (in a
 *     browser it throws a catchable RangeError — that asymmetry is exactly why
 *     the app wraps the metrics path in try/catch). Verified manually in-browser.
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

/* A round-trip driver for one wire variant: encode -> (clone?) -> decode.
 * The abstract wire type stays inside the closure (only Response.t escapes),
 * so this composes over all variants uniformly. */
let rt_of_wire =
    (wire: (module WorkerServer.WIRE))
    : ((~clone: bool, WorkerServer.Response.t) => WorkerServer.Response.t) => {
  module M = (val wire);
  (~clone, resp) => {
    let w = M.encode_response(resp);
    let w = clone ? structured_clone(w) : w;
    M.decode_response(w);
  };
};

/* Build a `Parens`-nested expression `depth` levels deep, iteratively so that
 * constructing the fixture does not itself recurse `depth` frames deep. */
let deep_exp = (depth: int): Exp.t => {
  let e = ref(Exp.fresh(EmptyHole));
  for (_ in 1 to depth) {
    e := Exp.fresh(Parens(e^));
  };
  e^;
};

let response_of_exp = (e: Exp.t): WorkerServer.Response.t => [
  ("cell", Ok((e, EvaluatorState.init))),
];

/* Reaching decode without raising is the Stack_overflow guard. For
 * correctness we compare marshaled bytes of the public Response.t (test-only;
 * jsoo's Marshal is iterative and deterministic) rather than the values,
 * whose polymorphic `=` would itself recurse `depth` deep. */
let assert_round_trips =
    (~rt, ~clone: bool, resp: WorkerServer.Response.t): unit => {
  let restored = rt(~clone, resp);
  check(
    string,
    "round-trips to identical bytes",
    Marshal.to_string(resp, []),
    Marshal.to_string(restored, []),
  );
};

let test_deep_round_trip =
    (~rt, ~name: string, ~clone: bool, ~depth: int): test_case(_) =>
  test_case(name, `Quick, () =>
    assert_round_trips(~rt, ~clone, response_of_exp(deep_exp(depth)))
  );

let test_wide_list_round_trip = (~rt, ~name: string, ~n: int): test_case(_) =>
  test_case(name, `Quick, () =>
    assert_round_trips(
      ~rt,
      ~clone=true,
      response_of_exp(
        Exp.fresh(ListLit(List.init(n, i => Exp.fresh(Atom(SInt(i)))))),
      ),
    )
  );

let parse = (s: string): Exp.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => fail("Failed to parse: " ++ s)
  };

/* A normal (shallow) program still round-trips, and the decoded expression is
 * equal to the original — the protocol change touches every evaluation. */
let test_realistic_round_trip =
    (~rt, ~name: string, ~code: string): test_case(_) =>
  test_case(
    name,
    `Quick,
    () => {
      let e = parse(code);
      let restored = rt(~clone=true, response_of_exp(e));
      switch (restored) {
      | [("cell", Ok((e', _))), ..._] =>
        check(
          bool,
          "decoded expression equals original",
          true,
          Exp.fast_equal(e, e'),
        )
      | _ => fail("round-trip did not preserve response shape")
      };
    },
  );

/* A block reaching the boundary twice must decode as one block referenced
 * twice, not two copies — otherwise DAG-shaped payloads (e.g. IncrEval.prev)
 * expand on the way across. This is asserted for the variants that preserve
 * sharing: marshal records sharing, and structuredClone (direct) preserves
 * aliasing. (sexp is a tree format that legitimately duplicates shared
 * structure, so it is excluded — see its group below.) */
let test_sharing_preserved = (~rt): test_case(_) =>
  test_case(
    "Sharing preserved",
    `Quick,
    () => {
      let shared = parse("[1, 2, 3]");
      let resp: WorkerServer.Response.t = [
        ("a", Ok((shared, EvaluatorState.init))),
        ("b", Ok((shared, EvaluatorState.init))),
      ];
      switch (rt(~clone=true, resp)) {
      | [("a", Ok((a, _))), ("b", Ok((b, _)))] =>
        check(bool, "decoded occurrences are physically equal", true, a === b)
      | _ => fail("round-trip did not preserve response shape")
      };
    },
  );

/* Shared battery every variant must pass. Sharing preservation is asserted
 * per group (below) rather than here: it holds for all variants except sexp,
 * which is a tree serialization and legitimately duplicates shared structure. */
let shallow_tests = (~rt): list(test_case(_)) => [
  test_realistic_round_trip(
    ~rt,
    ~name="Let with list",
    ~code="let x = [1, 2, 3] in x",
  ),
  test_realistic_round_trip(
    ~rt,
    ~name="Factorial",
    ~code=
      "let fact = fun n -> if n <= 0 then 1 else n * fact(n - 1) in fact(5)",
  ),
];

let marshal = rt_of_wire((module WorkerServer.MarshalWire));
let sexp = rt_of_wire((module WorkerServer.SexpWire));
let direct = rt_of_wire((module WorkerServer.DirectWire));

let tests = [
  (
    /* Marshal is iterative, so also depth- and width-proof. */
    "WorkerServer.MarshalWire",
    [
      test_deep_round_trip(
        ~rt=marshal,
        ~name="Deep through structuredClone (20k)",
        ~clone=true,
        ~depth=20000,
      ),
      test_wide_list_round_trip(
        ~rt=marshal,
        ~name="Wide list through structuredClone (10k)",
        ~n=10_000,
      ),
    ]
    @ shallow_tests(~rt=marshal)
    @ [test_sharing_preserved(~rt=marshal)],
  ),
  (
    /* Direct = identity. clone=false is a trivial identity round-trip; deep
     * clone=true is the #2368 crash and deliberately not exercised (see file
     * header). Sharing + shallow still validate the identity path. */
    "WorkerServer.DirectWire",
    [
      test_deep_round_trip(
        ~rt=direct,
        ~name="Deep identity (20k)",
        ~clone=false,
        ~depth=20000,
      ),
    ]
    @ shallow_tests(~rt=direct)
    @ [test_sharing_preserved(~rt=direct)],
  ),
  (
    /* Sexp converters recurse per level, so deep payloads are out of scope.
     * No sharing assertion: sexp is a tree format and legitimately duplicates
     * shared sub-structure (the derived converters don't record DAG sharing). */
    "WorkerServer.SexpWire",
    shallow_tests(~rt=sexp),
  ),
];
