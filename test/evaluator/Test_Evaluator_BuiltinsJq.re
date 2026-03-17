open Alcotest;
open Test_Evaluator_Prelude;

// Helper: a small JSON object for reuse across tests
let obj = {|Assoc([("name", String("Alice")), ("age", Int(30))])|};
let arr = {|List([Int(1), Int(2), Int(3)])|};
let nested = {|Assoc([("users", List([
  Assoc([("name", String("Alice")), ("age", Int(31))]),
  Assoc([("name", String("Bob")), ("age", Int(25))])
]))])|};

let tests = (
  "Evaluator.BuiltinsJq",
  [
    // ---- Tier 1: Basic Filters ----
    test_case("jq_identity on Int", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(42)]|},
        {|jq_identity(Int(42))|},
      )
    ),
    test_case("jq_identity on Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_identity(Null)|},
      )
    ),
    test_case("jq_iterate on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(1), Int(2), Int(3)]|},
        {|jq_iterate(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq_iterate on Assoc returns values", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice"), Int(30)]|},
        {|jq_iterate(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_iterate on Int returns empty", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[]|},
        {|jq_iterate(Int(5))|},
      )
    ),
    test_case("jq_keys on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("name"), String("age")])]|},
        {|jq_keys(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_keys on List returns indices", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([Int(0), Int(1), Int(2)])]|},
        {|jq_keys(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq_values on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("Alice"), Int(30)])]|},
        {|jq_values(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_values on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([Int(1), Int(2), Int(3)])]|},
        {|jq_values(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq_length on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(2)]|},
        {|jq_length(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_length on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(3)]|},
        {|jq_length(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq_length on String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(5)]|},
        {|jq_length(String("hello"))|},
      )
    ),
    test_case("jq_length on Null returns 0", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(0)]|},
        {|jq_length(Null)|},
      )
    ),
    test_case("jq_type on Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("null")]|},
        {|jq_type(Null)|},
      )
    ),
    test_case("jq_type on Bool", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("boolean")]|},
        {|jq_type(Bool(true))|},
      )
    ),
    test_case("jq_type on Int", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("number")]|},
        {|jq_type(Int(42))|},
      )
    ),
    test_case("jq_type on String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("string")]|},
        {|jq_type(String("hi"))|},
      )
    ),
    test_case("jq_type on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("array")]|},
        {|jq_type(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq_type on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("object")]|},
        {|jq_type(|} ++ obj ++ {|)|},
      )
    ),
    // ---- Tier 2: Parameterized Filters ----
    test_case("jq_field on Assoc hit", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|jq_field("name")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_field on Assoc miss", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_field("missing")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_field on non-Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_field("name")(Int(5))|},
      )
    ),
    test_case("jq_index on List hit", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(2)]|},
        {|jq_index(1)(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq_index on List out of bounds", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_index(10)(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq_index on non-List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_index(0)(Null)|},
      )
    ),
    test_case("jq_has on Assoc true", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|jq_has("name")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_has on Assoc false", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_has("missing")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_has on non-Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_has("x")(Int(5))|},
      )
    ),
    // ---- Tier 3: Higher-Order Combinators ----
    test_case("jq_pipe composes two filters", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|jq_pipe(jq_field("name"), jq_identity)(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_pipe: field then iterate", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(1), Int(2), Int(3)]|},
        {|jq_pipe(jq_field("items"), jq_iterate)(Assoc([("items", |}
        ++ arr
        ++ {|)]))|},
      )
    ),
    test_case("jq_select keeps matching", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(42)]|},
        {|jq_select(jq_identity)(Int(42))|},
      )
    ),
    test_case("jq_select drops Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[]|},
        {|jq_select(jq_identity)(Null)|},
      )
    ),
    test_case("jq_select drops Bool(false)", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[]|},
        {|jq_select(fun json -> [Bool(false)])(Int(1))|},
      )
    ),
    test_case("jq_map on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("Alice"), Int(30)])]|},
        {|jq_map(jq_identity)(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_map with field on List of objects", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("Alice"), String("Bob")])]|},
        {|jq_map(jq_field("name"))(List([
            Assoc([("name", String("Alice"))]),
            Assoc([("name", String("Bob"))])
          ]))|},
      )
    ),
    // ---- jq pipeline combinator ----
    test_case("jq pipeline: single filter", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|jq([jq_field("name")])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq pipeline: field then iterate", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice"), String("Bob")]|},
        {|jq([jq_field("users"), jq_iterate, jq_field("name")])(|}
        ++ nested
        ++ {|)|},
      )
    ),
    test_case("jq pipeline: empty filter list is identity", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(42)]|},
        {|jq([])(Int(42))|},
      )
    ),
    // ---- Tier 4: Mutation Combinators ----
    test_case("jq_set adds new field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("email", String("a@b.c")), ("name", String("Alice")), ("age", Int(30))])]|},
        {|jq_set("email", String("a@b.c"))(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_set overwrites existing field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("Bob")), ("age", Int(30))])]|},
        {|jq_set("name", String("Bob"))(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_set on non-Assoc returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_set("x", Int(1))(Int(5))|},
      )
    ),
    test_case("jq_update existing field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("ALICE")), ("age", Int(30))])]|},
        {|jq_update("name", fun v -> case v | String(s) => [String(string_uppercase(s))] | _ => [v] end)(|}
        ++ obj
        ++ {|)|},
      )
    ),
    test_case("jq_update missing field returns unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|jq_update("missing", fun v -> [Int(0)])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_update on non-Assoc returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_update("x", fun v -> [v])(Int(5))|},
      )
    ),
    test_case("jq_del removes field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("age", Int(30))])]|},
        {|jq_del("name")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_del missing field returns same object", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|jq_del("missing")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_del on non-Assoc returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_del("x")(Int(5))|},
      )
    ),
    // ---- Tier 5: Structural Combinators ----
    test_case("jq_to_entries on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([Assoc([("key", String("name")), ("value", String("Alice"))]), Assoc([("key", String("age")), ("value", Int(30))])])]|},
        {|jq_to_entries(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_to_entries on non-Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_to_entries(Int(5))|},
      )
    ),
    test_case("jq_from_entries on List of entry objects", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("Alice")), ("age", Int(30))])]|},
        {|jq_from_entries(List([Assoc([("key", String("name")), ("value", String("Alice"))]), Assoc([("key", String("age")), ("value", Int(30))])]))|},
      )
    ),
    test_case("jq_from_entries on non-List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_from_entries(Int(5))|},
      )
    ),
    test_case("jq_to_entries then jq_from_entries roundtrip", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|jq([jq_to_entries, jq_from_entries])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_startswith true", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|jq_startswith("hel")(String("hello"))|},
      )
    ),
    test_case("jq_startswith false", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_startswith("world")(String("hello"))|},
      )
    ),
    test_case("jq_startswith on non-String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_startswith("x")(Int(5))|},
      )
    ),
    test_case("jq_startswith empty prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|jq_startswith("")(String("hello"))|},
      )
    ),
    test_case("jq_startswith longer than string", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_startswith("hello world")(String("hello"))|},
      )
    ),
    test_case("jq_endswith true", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|jq_endswith("llo")(String("hello"))|},
      )
    ),
    test_case("jq_endswith false", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_endswith("world")(String("hello"))|},
      )
    ),
    test_case("jq_endswith on non-String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_endswith("x")(Int(5))|},
      )
    ),
    test_case("jq_endswith empty suffix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|jq_endswith("")(String("hello"))|},
      )
    ),
    // ---- Integration: pipeline with mutation ----
    test_case("jq pipeline with select and field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice"), String("Bob")]|},
        {|jq([jq_field("users"), jq_iterate, jq_select(jq_has("age")), jq_field("name")])(|}
        ++ nested
        ++ {|)|},
      )
    ),
    test_case("jq pipeline with startswith filter", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|jq([jq_field("users"), jq_iterate, jq_field("name"), jq_select(jq_startswith("Al"))])(|}
        ++ nested
        ++ {|)|},
      )
    ),
    // ---- Tier 6: Helper Combinators ----
    // jq1 tests
    test_case("jq1 returns first result", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|String("Alice")|},
        {|jq1([jq_field("name")])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq1 on empty pipeline", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Int(42)|},
        {|jq1([])(Int(42))|},
      )
    ),
    test_case("jq1 with iterate returns first element", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Int(1)|},
        {|jq1([jq_iterate])(|} ++ arr ++ {|)|},
      )
    ),
    test_case("jq1 on empty result returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Null|},
        {|jq1([jq_iterate])(Int(5))|},
      )
    ),
    // jq_add tests
    test_case("jq_add on Int", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(52)]|},
        {|jq_add(10)(Int(42))|},
      )
    ),
    test_case("jq_add negative", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(37)]|},
        {|jq_add(-5)(Int(42))|},
      )
    ),
    test_case("jq_add on non-Int returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_add(1)(String("hello"))|},
      )
    ),
    // jq_not tests
    test_case("jq_not on Bool(true)", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_not(Bool(true))|},
      )
    ),
    test_case("jq_not on Bool(false)", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|jq_not(Bool(false))|},
      )
    ),
    test_case("jq_not on Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|jq_not(Null)|},
      )
    ),
    test_case("jq_not on truthy value", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|jq_not(Int(42))|},
      )
    ),
    // jq_entry tests
    test_case("jq_entry constructs entry object", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("key", String("Alice")), ("value", Int(30))])]|},
        {|jq_entry(jq_field("name"), jq_field("age"))(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_entry with literal key", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("key", String("greeting")), ("value", String("Alice"))])]|},
        {|jq_entry(fun _ -> [String("greeting")], jq_field("name"))(|}
        ++ obj
        ++ {|)|},
      )
    ),
    // jq_with_entries tests
    test_case("jq_with_entries identity", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|jq_with_entries(jq_identity)(|} ++ obj ++ {|)|},
      )
    ),
    test_case("jq_with_entries modify values", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("ALICE")), ("age", String("30"))])]|},
        {|jq_with_entries(fun entry ->
            case entry
            | Assoc(pairs) =>
              case assoc_opt(pairs, "value")
              | Some(String(s)) => [Assoc([("key", case assoc_opt(pairs, "key") | Some(k) => k | None => Null end), ("value", String(string_uppercase(s)))])
                ]
              | Some(Int(n)) => [Assoc([("key", case assoc_opt(pairs, "key") | Some(k) => k | None => Null end), ("value", String(string_of_int(n)))])
                ]
              | _ => [entry]
              end
            | _ => [entry]
          end)(|}
        ++ obj
        ++ {|)|},
      )
    ),
    // jq_merge tests
    test_case("jq_merge two objects", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("b", Int(2)), ("a", Int(1))])]|},
        {|jq_merge(Assoc([("a", Int(1))]), Assoc([("b", Int(2))]))|},
      )
    ),
    test_case("jq_merge overlapping keys", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("a", Int(99)), ("b", Int(2))])]|},
        {|jq_merge(Assoc([("a", Int(1)), ("b", Int(2))]), Assoc([("a", Int(99))]))|},
      )
    ),
    test_case("jq_merge non-object returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_merge(Int(1), Assoc([("a", Int(1))]))|},
      )
    ),
    // jq_string_sub tests
    test_case("jq_string_sub replaces substring", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello world")]|},
        {|jq_string_sub("planet", "world")(String("hello planet"))|},
      )
    ),
    test_case("jq_string_sub no match unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|jq_string_sub("xyz", "abc")(String("hello"))|},
      )
    ),
    test_case("jq_string_sub on non-String returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_string_sub("a", "b")(Int(5))|},
      )
    ),
    // jq_ltrimstr tests
    test_case("jq_ltrimstr trims prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("world")]|},
        {|jq_ltrimstr("hello ")(String("hello world"))|},
      )
    ),
    test_case("jq_ltrimstr no match returns unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|jq_ltrimstr("world")(String("hello"))|},
      )
    ),
    test_case("jq_ltrimstr on non-String returns unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(5)]|},
        {|jq_ltrimstr("x")(Int(5))|},
      )
    ),
    test_case("jq_ltrimstr empty prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|jq_ltrimstr("")(String("hello"))|},
      )
    ),
    // jq_string_prepend tests
    test_case("jq_string_prepend prepends", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello world")]|},
        {|jq_string_prepend("hello ")(String("world"))|},
      )
    ),
    test_case("jq_string_prepend empty prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|jq_string_prepend("")(String("hello"))|},
      )
    ),
    test_case("jq_string_prepend on non-String returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|jq_string_prepend("x")(Int(5))|},
      )
    ),
  ],
);
