open Alcotest;
open Haz3lcore;

let yojson_testable = testable(Yojson.Safe.pp, Yojson.Safe.equal);
let yojson_result_testable = result(yojson_testable, string);

let test_round_trip = (~name, ~json: Yojson.Safe.t) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp_result = HazelJson.JsonADT.yojson_to_exp(json);
      switch (exp_result) {
      | Ok(exp) =>
        let json_result = HazelJson.JsonADT.exp_to_yojson(exp);
        check(yojson_result_testable, name, Ok(json), json_result);
      | Error(msg) => fail("yojson_to_exp failed: " ++ msg)
      };
    },
  );

let tests = (
  "HazelJson.JsonADT",
  [
    /* Null */
    test_round_trip(~name="null", ~json=`Null),
    /* Bool */
    test_round_trip(~name="bool_true", ~json=`Bool(true)),
    test_round_trip(~name="bool_false", ~json=`Bool(false)),
    /* Int */
    test_round_trip(~name="int_zero", ~json=`Int(0)),
    test_round_trip(~name="int_positive", ~json=`Int(42)),
    test_round_trip(~name="int_negative", ~json=`Int(-7)),
    test_round_trip(~name="int_large", ~json=`Int(999999)),
    /* Float */
    test_round_trip(~name="float_positive", ~json=`Float(3.14)),
    test_round_trip(~name="float_zero", ~json=`Float(0.0)),
    test_round_trip(~name="float_negative", ~json=`Float(-2.5)),
    /* String */
    test_round_trip(~name="string_hello", ~json=`String("hello")),
    test_round_trip(~name="string_empty", ~json=`String("")),
    test_round_trip(~name="string_spaces", ~json=`String("hello world")),
    /* List */
    test_round_trip(~name="list_empty", ~json=`List([])),
    test_round_trip(~name="list_single_int", ~json=`List([`Int(42)])),
    test_round_trip(
      ~name="list_multiple_ints",
      ~json=`List([`Int(1), `Int(2), `Int(3)]),
    ),
    test_round_trip(
      ~name="list_mixed_types",
      ~json=`List([`Int(1), `String("two"), `Bool(true)]),
    ),
    test_round_trip(
      ~name="list_nested",
      ~json=`List([`List([`Int(1), `Int(2)]), `List([`Int(3)])]),
    ),
    /* Assoc (JSON objects) */
    test_round_trip(~name="assoc_empty", ~json=`Assoc([])),
    test_round_trip(
      ~name="assoc_single",
      ~json=`Assoc([("key", `Int(42))]),
    ),
    test_round_trip(
      ~name="assoc_multiple",
      ~json=`Assoc([("name", `String("Alice")), ("age", `Int(30))]),
    ),
    /* Nested/compositional */
    test_round_trip(
      ~name="nested_assoc_in_list",
      ~json=
        `List([
          `Assoc([("id", `Int(1)), ("name", `String("Alice"))]),
          `Assoc([("id", `Int(2)), ("name", `String("Bob"))]),
        ]),
    ),
    test_round_trip(
      ~name="nested_list_in_assoc",
      ~json=`Assoc([("items", `List([`Int(1), `Int(2), `Int(3)]))]),
    ),
    test_round_trip(
      ~name="deeply_nested",
      ~json=
        `Assoc([
          (
            "data",
            `Assoc([("values", `List([`Int(1), `Null, `Bool(true)]))]),
          ),
          ("meta", `String("test")),
        ]),
    ),
    test_round_trip(
      ~name="null_in_list",
      ~json=`List([`Null, `Int(1), `Null]),
    ),
    test_round_trip(
      ~name="complex_nested",
      ~json=
        `Assoc([
          (
            "users",
            `List([
              `Assoc([
                ("name", `String("Alice")),
                ("scores", `List([`Int(95), `Int(87), `Int(92)])),
                ("active", `Bool(true)),
              ]),
              `Assoc([
                ("name", `String("Bob")),
                ("scores", `List([])),
                ("active", `Bool(false)),
              ]),
            ]),
          ),
          ("count", `Int(2)),
        ]),
    ),
  ],
);
