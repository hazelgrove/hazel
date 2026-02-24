open Alcotest;
open Haz3lcore;

/* Test utilities for converting between strings, JSON, and Hazel terms */
let string_to_exp = (s: string): option(Language.Exp.t) =>
  Parser.to_term(s);

let exp_to_string = (exp: Language.Exp.t): string => {
  let settings = ExpToSegment.Settings.editable(~inline=Inline);
  let segment = ExpToSegment.exp_to_segment(~settings, exp);
  Printer.of_segment(~holes="?", segment);
};

let yojson_testable = testable(Yojson.Safe.pp, Yojson.Safe.equal);
let yojson_result_testable = result(yojson_testable, string);
let exp_result_testable =
  result(testable(Language.Exp.pp, Language.Exp.equal), string);

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

let test_to_json_error =
    (~name: string, ~hazel_str: string, ~expected_error: string) =>
  test_case(name, `Quick, () => {
    switch (string_to_exp(hazel_str)) {
    | Some(exp) =>
      let result = Haz3lcore.HazelProtocol.JsonCodec.exp_to_yojson(exp);
      check(yojson_result_testable, name, Error(expected_error), result);
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

/* Stage 2 tests: Base types (int, float, string, bool) */
let tests = (
  "HazelProtocol.JsonCodec",
  [
    /* Integer tests */
    test_to_json(
      ~name="int_to_json: 42",
      ~hazel_str={|42|},
      ~expected_json=`Int(42),
    ),
    test_to_json(
      ~name="int_to_json: 0",
      ~hazel_str={|0|},
      ~expected_json=`Int(0),
    ),
    test_to_json(
      ~name="int_to_json: negative",
      ~hazel_str={|999|},
      ~expected_json=`Int(999),
    ),
    test_from_json(
      ~name="json_to_int: 456",
      ~json=`Int(456),
      ~expected_hazel={|456|},
    ),
    test_from_json(
      ~name="json_to_int: 0",
      ~json=`Int(0),
      ~expected_hazel={|0|},
    ),
    test_from_json(
      ~name="json_to_int: large",
      ~json=`Int(123456),
      ~expected_hazel={|123456|},
    ),
    test_round_trip(~name="int_round_trip: 42", ~hazel_str={|42|}),
    test_round_trip(~name="int_round_trip: 0", ~hazel_str={|0|}),
    test_round_trip(~name="int_round_trip: large", ~hazel_str={|999999|}),
    /* Float tests */
    test_to_json(
      ~name="float_to_json: 3.14",
      ~hazel_str={|3.14|},
      ~expected_json=`Float(3.14),
    ),
    test_to_json(
      ~name="float_to_json: 0.0",
      ~hazel_str={|0.0|},
      ~expected_json=`Float(0.0),
    ),
    //TODO(andrew): hazel floats are sort of broken...
    // test_to_json(
    //   ~name="float_to_json: negative",
    //   ~hazel_str="-2.5",
    //   ~expected_json=`Float(-2.5),
    // ),
    // test_from_json(
    //   ~name="json_to_float: 1.5",
    //   ~json=`Float(1.5),
    //   ~expected_hazel="1.5",
    // ),
    // test_from_json(
    //   ~name="json_to_float: 0.0",
    //   ~json=`Float(0.0),
    //   ~expected_hazel="0.",
    // ),
    // test_round_trip(~name="float_round_trip: 3.14", ~hazel_str="3.14"),
    // test_round_trip(~name="float_round_trip: negative", ~hazel_str="-1.23"),
    /* String tests */
    test_to_json(
      ~name="string_to_json: hello",
      ~hazel_str={|"hello"|},
      ~expected_json=`String("hello"),
    ),
    test_to_json(
      ~name="string_to_json: empty",
      ~hazel_str={|""|},
      ~expected_json=`String(""),
    ),
    test_to_json(
      ~name="string_to_json: spaces",
      ~hazel_str={|"hello world"|},
      ~expected_json=`String("hello world"),
    ),
    test_from_json(
      ~name="json_to_string: test",
      ~json=`String("test"),
      ~expected_hazel={|"test"|},
    ),
    test_from_json(
      ~name="json_to_string: empty",
      ~json=`String(""),
      ~expected_hazel={|""|},
    ),
    test_round_trip(~name="string_round_trip: hello", ~hazel_str={|"hello"|}),
    test_round_trip(~name="string_round_trip: empty", ~hazel_str={|""|}),
    /* Bool tests */
    test_to_json(
      ~name="bool_to_json: true",
      ~hazel_str={|true|},
      ~expected_json=`Bool(true),
    ),
    test_to_json(
      ~name="bool_to_json: false",
      ~hazel_str={|false|},
      ~expected_json=`Bool(false),
    ),
    test_from_json(
      ~name="json_to_bool: true",
      ~json=`Bool(true),
      ~expected_hazel={|true|},
    ),
    test_from_json(
      ~name="json_to_bool: false",
      ~json=`Bool(false),
      ~expected_hazel={|false|},
    ),
    test_round_trip(~name="bool_round_trip: true", ~hazel_str="true"),
    test_round_trip(~name="bool_round_trip: false", ~hazel_str="false"),
    /* Stage 3: List tests */
    test_to_json(
      ~name="list_to_json: empty",
      ~hazel_str={|[]|},
      ~expected_json=`List([]),
    ),
    test_to_json(
      ~name="list_to_json: single_int",
      ~hazel_str={|[42]|},
      ~expected_json=`List([`Int(42)]),
    ),
    test_to_json(
      ~name="list_to_json: multiple_ints",
      ~hazel_str={|[1, 2, 3]|},
      ~expected_json=`List([`Int(1), `Int(2), `Int(3)]),
    ),
    test_to_json(
      ~name="list_to_json: single_string",
      ~hazel_str={|["hello"]|},
      ~expected_json=`List([`String("hello")]),
    ),
    test_to_json(
      ~name="list_to_json: multiple_strings",
      ~hazel_str={|["a", "b", "c"]|},
      ~expected_json=`List([`String("a"), `String("b"), `String("c")]),
    ),
    test_to_json(
      ~name="list_to_json: bools",
      ~hazel_str={|[true, false, true]|},
      ~expected_json=`List([`Bool(true), `Bool(false), `Bool(true)]),
    ),
    test_from_json(
      ~name="json_to_list: empty",
      ~json=`List([]),
      ~expected_hazel={|[]|},
    ),
    test_from_json(
      ~name="json_to_list: single_int",
      ~json=`List([`Int(42)]),
      ~expected_hazel={|[42]|},
    ),
    test_from_json(
      ~name="json_to_list: multiple_ints",
      ~json=`List([`Int(1), `Int(2), `Int(3)]),
      ~expected_hazel={|[1, 2, 3]|},
    ),
    test_from_json(
      ~name="json_to_list: strings",
      ~json=`List([`String("hello"), `String("world")]),
      ~expected_hazel={|["hello", "world"]|},
    ),
    test_from_json(
      ~name="json_to_list: bools",
      ~json=`List([`Bool(true), `Bool(false)]),
      ~expected_hazel={|[true, false]|},
    ),
    test_round_trip(~name="list_round_trip: empty", ~hazel_str="[]"),
    test_round_trip(~name="list_round_trip: single", ~hazel_str="[42]"),
    test_round_trip(
      ~name="list_round_trip: multiple",
      ~hazel_str={|[1, 2, 3]|},
    ),
    test_round_trip(
      ~name="list_round_trip: strings",
      ~hazel_str={|["a", "b"]|},
    ),
    test_round_trip(
      ~name="list_round_trip: bools",
      ~hazel_str={|[true, false]|},
    ),
    /* Stage 4: Tuple tests (plain tuples wrapped in parens) */
    test_to_json(
      ~name="tuple_to_json: empty",
      ~hazel_str={|()|},
      ~expected_json=`Assoc([]),
    ),
    test_to_json(
      ~name="tuple_to_json: pair",
      ~hazel_str={|(1, 2)|},
      ~expected_json=`Assoc([("0", `Int(1)), ("1", `Int(2))]),
    ),
    test_to_json(
      ~name="tuple_to_json: mixed_types",
      ~hazel_str={|(42, "hello", true)|},
      ~expected_json=
        `Assoc([
          ("0", `Int(42)),
          ("1", `String("hello")),
          ("2", `Bool(true)),
        ]),
    ),
    test_from_json(
      ~name="json_to_tuple: empty",
      ~json=`Assoc([]),
      ~expected_hazel={|()|},
    ),
    test_from_json(
      ~name="json_to_tuple: pair",
      ~json=`Assoc([("0", `Int(1)), ("1", `Int(2))]),
      ~expected_hazel={|(1, 2)|},
    ),
    test_from_json(
      ~name="json_to_tuple: mixed_types",
      ~json=
        `Assoc([
          ("0", `Int(42)),
          ("1", `String("hello")),
          ("2", `Bool(true)),
        ]),
      ~expected_hazel={|(42, "hello", true)|},
    ),
    test_round_trip(~name="tuple_round_trip: empty", ~hazel_str={|()|}),
    test_round_trip(~name="tuple_round_trip: pair", ~hazel_str={|(1, 2)|}),
    test_round_trip(
      ~name="tuple_round_trip: mixed",
      ~hazel_str={|(42, "hello", true)|},
    ),
    /* Stage 4: Labeled tuple tests */
    test_to_json(
      ~name="labeled_tuple_to_json: singleton",
      ~hazel_str={|(label=42)|},
      ~expected_json=`Assoc([("label", `Int(42))]),
    ),
    test_to_json(
      ~name="labeled_tuple_to_json: all_labeled",
      ~hazel_str={|(x=10, y=20, name="point")|},
      ~expected_json=
        `Assoc([
          ("x", `Int(10)),
          ("y", `Int(20)),
          ("name", `String("point")),
        ]),
    ),
    test_to_json(
      ~name="labeled_tuple_to_json: keyword label unsanitized",
      ~hazel_str={|(type__=42)|},
      ~expected_json=`Assoc([("type", `Int(42))]),
    ),
    test_to_json(
      ~name="labeled_tuple_to_json: multiple keyword labels",
      ~hazel_str={|(type__=1, let__=2)|},
      ~expected_json=`Assoc([("type", `Int(1)), ("let", `Int(2))]),
    ),
    test_from_json(
      ~name="json_to_labeled_tuple: singleton",
      ~json=`Assoc([("label", `Int(42))]),
      ~expected_hazel={|(label=42)|},
    ),
    test_from_json(
      ~name="json_to_labeled_tuple: all_labeled",
      ~json=
        `Assoc([
          ("x", `Int(10)),
          ("y", `Int(20)),
          ("name", `String("point")),
        ]),
      ~expected_hazel={|(x=10, y=20, name="point")|},
    ),
    test_from_json(
      ~name="json_to_labeled_tuple: keyword label sanitized",
      ~json=`Assoc([("type", `Int(42))]),
      ~expected_hazel={|(type__=42)|},
    ),
    test_round_trip(
      ~name="labeled_tuple_round_trip: singleton",
      ~hazel_str={|(label=42)|},
    ),
    test_round_trip(
      ~name="labeled_tuple_round_trip: all_labeled",
      ~hazel_str={|(x=10, y=20, name="point")|},
    ),
    test_round_trip(
      ~name="labeled_tuple_round_trip: keyword label",
      ~hazel_str={|(type__=42)|},
    ),
    test_to_json_error(
      ~name="labeled_tuple_to_json: keyword unsanitization duplicate",
      ~hazel_str={|(type__=1, type__=2)|},
      ~expected_error=
        "Duplicate labeled tuple key after keyword unsanitization: type",
    ),
    test_error(
      ~name="json_to_labeled_tuple: keyword sanitization duplicate",
      ~json=`Assoc([("type__", `Int(1)), ("type", `Int(2))]),
      ~expected_error=
        "Duplicate labeled tuple key after keyword sanitization: type__",
    ),
    /* Stage 5: ADT tests */
    test_to_json(
      ~name="adt_to_json: nullary_constructor",
      ~hazel_str={|None|},
      ~expected_json=`Assoc([("t", `String("None"))]),
    ),
    test_to_json(
      ~name="adt_to_json: constructor_with_value",
      ~hazel_str={|Some(42)|},
      ~expected_json=`Assoc([("t", `String("Some")), ("v", `Int(42))]),
    ),
    test_to_json(
      ~name="adt_to_json: constructor_with_string",
      ~hazel_str={|Error("message")|},
      ~expected_json=
        `Assoc([("t", `String("Error")), ("v", `String("message"))]),
    ),
    test_to_json(
      ~name="adt_to_json: constructor_with_tuple",
      ~hazel_str={|Pair(1, 2)|},
      ~expected_json=
        `Assoc([
          ("t", `String("Pair")),
          ("v", `Assoc([("0", `Int(1)), ("1", `Int(2))])),
        ]),
    ),
    test_from_json(
      ~name="json_to_adt: nullary_constructor",
      ~json=`Assoc([("t", `String("None"))]),
      ~expected_hazel={|None|},
    ),
    test_from_json(
      ~name="json_to_adt: constructor_with_value",
      ~json=`Assoc([("t", `String("Some")), ("v", `Int(42))]),
      ~expected_hazel={|Some(42)|},
    ),
    test_from_json(
      ~name="json_to_adt: constructor_with_string",
      ~json=`Assoc([("t", `String("Ok")), ("v", `String("success"))]),
      ~expected_hazel={|Ok("success")|},
    ),
    test_round_trip(~name="adt_round_trip: nullary", ~hazel_str={|None|}),
    test_round_trip(
      ~name="adt_round_trip: with_int",
      ~hazel_str={|Some(123)|},
    ),
    test_round_trip(
      ~name="adt_round_trip: with_string",
      ~hazel_str={|Result("data")|},
    ),
    /* Compositional tests: nested combinations of all supported types */
    test_to_json(
      ~name="compositional: list_of_tuples",
      ~hazel_str={|[(1, 2), (3, 4), (5, 6)]|},
      ~expected_json=
        `List([
          `Assoc([("0", `Int(1)), ("1", `Int(2))]),
          `Assoc([("0", `Int(3)), ("1", `Int(4))]),
          `Assoc([("0", `Int(5)), ("1", `Int(6))]),
        ]),
    ),
    test_to_json(
      ~name="compositional: list_of_labeled_tuples",
      ~hazel_str={|[(x=1, y=2), (x=3, y=4)]|},
      ~expected_json=
        `List([
          `Assoc([("x", `Int(1)), ("y", `Int(2))]),
          `Assoc([("x", `Int(3)), ("y", `Int(4))]),
        ]),
    ),
    test_to_json(
      ~name="compositional: tuple_with_list",
      ~hazel_str={|([1, 2, 3], "hello", true)|},
      ~expected_json=
        `Assoc([
          ("0", `List([`Int(1), `Int(2), `Int(3)])),
          ("1", `String("hello")),
          ("2", `Bool(true)),
        ]),
    ),
    test_to_json(
      ~name="compositional: adt_with_list",
      ~hazel_str={|Success([1, 2, 3])|},
      ~expected_json=
        `Assoc([
          ("t", `String("Success")),
          ("v", `List([`Int(1), `Int(2), `Int(3)])),
        ]),
    ),
    test_to_json(
      ~name="compositional: adt_with_tuple",
      ~hazel_str={|Point(10, 20)|},
      ~expected_json=
        `Assoc([
          ("t", `String("Point")),
          ("v", `Assoc([("0", `Int(10)), ("1", `Int(20))])),
        ]),
    ),
    test_to_json(
      ~name="compositional: adt_with_labeled_tuple",
      ~hazel_str={|Person(name="Alice", age=30)|},
      ~expected_json=
        `Assoc([
          ("t", `String("Person")),
          ("v", `Assoc([("name", `String("Alice")), ("age", `Int(30))])),
        ]),
    ),
    test_to_json(
      ~name="compositional: list_of_adts",
      ~hazel_str={|[Some(1), None, Some(2)]|},
      ~expected_json=
        `List([
          `Assoc([("t", `String("Some")), ("v", `Int(1))]),
          `Assoc([("t", `String("None"))]),
          `Assoc([("t", `String("Some")), ("v", `Int(2))]),
        ]),
    ),
    test_to_json(
      ~name="compositional: nested_lists",
      ~hazel_str={|[[1, 2], [3, 4], []]|},
      ~expected_json=
        `List([
          `List([`Int(1), `Int(2)]),
          `List([`Int(3), `Int(4)]),
          `List([]),
        ]),
    ),
    test_to_json(
      ~name="compositional: complex_nested",
      ~hazel_str=
        {|Result((success=true, data=[1, 2, 3], meta=(count=3, kind="list")))|},
      ~expected_json=
        `Assoc([
          ("t", `String("Result")),
          (
            "v",
            `Assoc([
              ("success", `Bool(true)),
              ("data", `List([`Int(1), `Int(2), `Int(3)])),
              (
                "meta",
                `Assoc([("count", `Int(3)), ("kind", `String("list"))]),
              ),
            ]),
          ),
        ]),
    ),
    test_from_json(
      ~name="compositional: json_to_list_of_tuples",
      ~json=
        `List([
          `Assoc([("0", `Int(1)), ("1", `Int(2))]),
          `Assoc([("0", `Int(3)), ("1", `Int(4))]),
        ]),
      ~expected_hazel={|[(1, 2), (3, 4)]|},
    ),
    test_from_json(
      ~name="compositional: json_to_adt_with_list",
      ~json=
        `Assoc([
          ("t", `String("Data")),
          ("v", `List([`String("a"), `String("b"), `String("c")])),
        ]),
      ~expected_hazel={|Data(["a", "b", "c"])|},
    ),
    test_from_json(
      ~name="compositional: json_to_nested_structure",
      ~json=
        `Assoc([
          ("t", `String("Config")),
          (
            "v",
            `Assoc([
              ("enabled", `Bool(true)),
              ("values", `List([`Int(1), `Int(2)])),
              ("metadata", `Assoc([("version", `String("1.0"))])),
            ]),
          ),
        ]),
      ~expected_hazel=
        {|Config(enabled=true, values=[1, 2], metadata=(version="1.0"))|},
    ),
    test_round_trip(
      ~name="compositional: round_trip_complex",
      ~hazel_str={|[(Some(1), None), (Some(2), Some(3))]|},
    ),
    test_round_trip(
      ~name="compositional: round_trip_nested_tuples",
      ~hazel_str={|((x=1, y=2), (x=3, y=4))|},
    ),
    test_round_trip(
      ~name="compositional: round_trip_adt_list_tuple",
      ~hazel_str=
        {|Response(status="ok", data=[(name="Alice", score=95), (name="Bob", score=87)])|},
    ),
    /* Error cases for unsupported types (future stages) */
    test_error(
      ~name="unsupported: object with mixed keys",
      ~json=`Assoc([("0", `Int(1)), ("key", `String("value"))]),
      ~expected_error="Mixed labeled/unlabeled tuples not supported",
    ),
    test_error(
      ~name="unsupported: null",
      ~json=`Null,
      ~expected_error="Null values not supported in JsonCodec",
    ),
  ],
);
