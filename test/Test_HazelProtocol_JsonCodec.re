open Alcotest;
open Haz3lcore;

/* Test utilities for converting between strings, JSON, and Hazel terms */
let string_to_exp = (s: string): option(Language.Term.Exp.t) =>
  Parser.to_term(s);

let exp_to_string = (exp: Language.Term.Exp.t): string => {
  let settings = ExpToSegment.Settings.editable(~inline=true);
  let segment = ExpToSegment.exp_to_segment(~settings, exp);
  Printer.of_segment(~holes="?", segment);
};

let yojson_testable = testable(Yojson.Safe.pp, Yojson.Safe.equal);
let yojson_result_testable = result(yojson_testable, string);
let exp_result_testable =
  result(testable(Language.Term.Exp.pp, Language.Term.Exp.equal), string);

/* Test helper that checks JSON conversion from Hazel string to expected JSON */
let test_to_json = (~name, ~hazel_str, ~expected_json) =>
  test_case(name, `Quick, () => {
    switch (string_to_exp(hazel_str)) {
    | Some(exp) =>
      let result = Haz3lcore.HazelProtocol.JsonCodec.exp_to_yojson(exp);
      check(yojson_result_testable, name, Ok(expected_json), result);
    | None => fail("Failed to parse Hazel string: " ++ hazel_str)
    }
  });

/* Test helper that checks JSON conversion from JSON to expected Hazel string */
let test_from_json = (~name, ~json, ~expected_hazel) =>
  test_case(
    name,
    `Quick,
    () => {
      let result = Haz3lcore.HazelProtocol.JsonCodec.yojson_to_exp(json);
      switch (result) {
      | Ok(exp) =>
        let hazel_str = exp_to_string(exp);
        check(string, name, expected_hazel, hazel_str);
      | Error(msg) => fail("JSON conversion failed: " ++ msg)
      };
    },
  );

/* Test helper for round-trip conversion (Hazel -> JSON -> Hazel) */
let test_round_trip = (~name, ~hazel_str) =>
  test_case(name, `Quick, () => {
    switch (string_to_exp(hazel_str)) {
    | Some(original_exp) =>
      let json_result =
        Haz3lcore.HazelProtocol.JsonCodec.exp_to_yojson(original_exp);
      switch (json_result) {
      | Ok(json) =>
        let back_result =
          Haz3lcore.HazelProtocol.JsonCodec.yojson_to_exp(json);
        switch (back_result) {
        | Ok(back_exp) =>
          let back_str = exp_to_string(back_exp);
          check(string, name, hazel_str, back_str);
        | Error(msg) => fail("Round trip failed at decode: " ++ msg)
        };
      | Error(msg) => fail("Round trip failed at encode: " ++ msg)
      };
    | None => fail("Failed to parse Hazel string: " ++ hazel_str)
    }
  });

/* Test helper for expected error cases */
let test_error = (~name, ~json, ~expected_error) =>
  test_case(
    name,
    `Quick,
    () => {
      let result = Haz3lcore.HazelProtocol.JsonCodec.yojson_to_exp(json);
      check(exp_result_testable, name, Error(expected_error), result);
    },
  );

/* Stage 1 tests: Integer support only */
let tests = (
  "HazelProtocol.JsonCodec",
  [
    /* Basic integer conversions */
    test_to_json(
      ~name="exp_to_json: 42",
      ~hazel_str="42",
      ~expected_json=`Int(42),
    ),
    test_to_json(
      ~name="exp_to_json: 0",
      ~hazel_str="0",
      ~expected_json=`Int(0),
    ),
    test_to_json(
      ~name="exp_to_json: negative",
      ~hazel_str="999",
      ~expected_json=`Int(999),
    ),
    test_from_json(
      ~name="json_to_exp: 456",
      ~json=`Int(456),
      ~expected_hazel="456",
    ),
    test_from_json(
      ~name="json_to_exp: 0",
      ~json=`Int(0),
      ~expected_hazel="0",
    ),
    test_from_json(
      ~name="json_to_exp: large",
      ~json=`Int(123456),
      ~expected_hazel="123456",
    ),
    /* Round-trip tests */
    test_round_trip(~name="round_trip: 42", ~hazel_str="42"),
    test_round_trip(~name="round_trip: 0", ~hazel_str="0"),
    test_round_trip(~name="round_trip: large", ~hazel_str="999999"),
    /* Error cases for unsupported types */
    test_error(
      ~name="unsupported: float",
      ~json=`Float(3.14),
      ~expected_error="Float values not yet supported in JsonCodec",
    ),
    test_error(
      ~name="unsupported: string",
      ~json=`String("hello"),
      ~expected_error="String values not yet supported in JsonCodec",
    ),
    test_error(
      ~name="unsupported: bool",
      ~json=`Bool(true),
      ~expected_error="Bool values not yet supported in JsonCodec",
    ),
    test_error(
      ~name="unsupported: list",
      ~json=`List([`Int(1), `Int(2)]),
      ~expected_error="List values not yet supported in JsonCodec",
    ),
    test_error(
      ~name="unsupported: object",
      ~json=`Assoc([("key", `String("value"))]),
      ~expected_error="Object values not yet supported in JsonCodec",
    ),
    test_error(
      ~name="unsupported: null",
      ~json=`Null,
      ~expected_error="Null values not supported in JsonCodec",
    ),
  ],
);
