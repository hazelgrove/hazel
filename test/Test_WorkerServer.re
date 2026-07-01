/**
 * Round-trip tests for the eval-worker wire protocol (issue #2368).
 *
 * These run under js_of_ocaml (node), so they exercise jsoo's real Marshal —
 * iterative, so it doesn't stack-overflow on deep structures. They guard the
 * encoding, not Chrome's structured-clone limit (which the string sidesteps and
 * node doesn't reproduce).
 */
open Alcotest;
open Language;

module Wire = WorkerServer.Wire;

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

/* Reaching decode without raising is the Stack_overflow guard. For correctness
 * we compare marshaled bytes of the public Response.t (Marshal is
 * deterministic) rather than the values, whose polymorphic `=` would itself
 * recurse `depth` deep. */
let assert_marshal_round_trips = (resp: WorkerServer.Response.t): unit => {
  let restored = Wire.decode_response(Wire.encode_response(resp));
  check(
    string,
    "round-trips to identical bytes",
    Marshal.to_string(resp, []),
    Marshal.to_string(restored, []),
  );
};

let test_deep_round_trip = (~name: string, ~depth: int): test_case(_) =>
  test_case(name, `Quick, () =>
    assert_marshal_round_trips(response_of_exp(deep_exp(depth)))
  );

let parse = (s: string): Exp.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => fail("Failed to parse: " ++ s)
  };

/* A normal (shallow) program still round-trips, and the decoded expression is
 * equal to the original — the protocol change touches every evaluation. */
let test_realistic_round_trip = (~name: string, ~code: string): test_case(_) =>
  test_case(
    name,
    `Quick,
    () => {
      let e = parse(code);
      let resp = response_of_exp(e);
      let restored = Wire.decode_response(Wire.encode_response(resp));
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

let tests = [
  (
    "WorkerServer.Wire",
    [
      test_deep_round_trip(~name="Deep expression (1k)", ~depth=1000),
      test_deep_round_trip(~name="Deep expression (20k)", ~depth=20000),
      test_realistic_round_trip(
        ~name="Let with list",
        ~code="let x = [1, 2, 3] in x",
      ),
      test_realistic_round_trip(
        ~name="Factorial",
        ~code=
          "let fact = fun n -> if n <= 0 then 1 else n * fact(n - 1) in fact(5)",
      ),
    ],
  ),
];
