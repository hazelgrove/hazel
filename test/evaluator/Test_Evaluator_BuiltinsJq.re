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
    test_case("Jq.identity on Int", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(42)]|},
        {|Jq.identity(Int(42))|},
      )
    ),
    test_case("Jq.identity on Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.identity(Null)|},
      )
    ),
    test_case("Jq.iterate on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(1), Int(2), Int(3)]|},
        {|Jq.iterate(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.iterate on Assoc returns values", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice"), Int(30)]|},
        {|Jq.iterate(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.iterate on Int returns empty", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[]|},
        {|Jq.iterate(Int(5))|},
      )
    ),
    test_case("Jq.keys on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("name"), String("age")])]|},
        {|Jq.keys(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.keys on List returns indices", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([Int(0), Int(1), Int(2)])]|},
        {|Jq.keys(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.values on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("Alice"), Int(30)])]|},
        {|Jq.values(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.values on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([Int(1), Int(2), Int(3)])]|},
        {|Jq.values(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.length on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(2)]|},
        {|Jq.length(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.length on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(3)]|},
        {|Jq.length(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.length on String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(5)]|},
        {|Jq.length(String("hello"))|},
      )
    ),
    test_case("Jq.length on Null returns 0", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(0)]|},
        {|Jq.length(Null)|},
      )
    ),
    test_case("Jq.type_ on Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("null")]|},
        {|Jq.type_(Null)|},
      )
    ),
    test_case("Jq.type_ on Bool", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("boolean")]|},
        {|Jq.type_(Bool(true))|},
      )
    ),
    test_case("Jq.type_ on Int", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("number")]|},
        {|Jq.type_(Int(42))|},
      )
    ),
    test_case("Jq.type_ on String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("string")]|},
        {|Jq.type_(String("hi"))|},
      )
    ),
    test_case("Jq.type_ on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("array")]|},
        {|Jq.type_(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.type_ on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("object")]|},
        {|Jq.type_(|} ++ obj ++ {|)|},
      )
    ),
    // ---- Tier 2: Parameterized Filters ----
    test_case("Jq.field on Assoc hit", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|Jq.field("name")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.field on Assoc miss", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.field("missing")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.field on non-Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.field("name")(Int(5))|},
      )
    ),
    test_case("Jq.index on List hit", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(2)]|},
        {|Jq.index(1)(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.index on List out of bounds", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.index(10)(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.index on non-List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.index(0)(Null)|},
      )
    ),
    test_case("Jq.has on Assoc true", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|Jq.has("name")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.has on Assoc false", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.has("missing")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.has on non-Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.has("x")(Int(5))|},
      )
    ),
    // ---- Tier 3: Higher-Order Combinators ----
    test_case("Jq.pipe composes two filters", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|Jq.pipe(Jq.field("name"), Jq.identity)(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.pipe: field then iterate", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(1), Int(2), Int(3)]|},
        {|Jq.pipe(Jq.field("items"), Jq.iterate)(Assoc([("items", |}
        ++ arr
        ++ {|)]))|},
      )
    ),
    test_case("Jq.select keeps matching", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(42)]|},
        {|Jq.select(Jq.identity)(Int(42))|},
      )
    ),
    test_case("Jq.select drops Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[]|},
        {|Jq.select(Jq.identity)(Null)|},
      )
    ),
    test_case("Jq.select drops Bool(false)", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[]|},
        {|Jq.select(fun json -> [Bool(false)])(Int(1))|},
      )
    ),
    test_case("Jq.map on List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("Alice"), Int(30)])]|},
        {|Jq.map(Jq.identity)(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.map with field on List of objects", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([String("Alice"), String("Bob")])]|},
        {|Jq.map(Jq.field("name"))(List([
            Assoc([("name", String("Alice"))]),
            Assoc([("name", String("Bob"))])
          ]))|},
      )
    ),
    // ---- Jq.run pipeline combinator ----
    test_case("Jq.run pipeline: single filter", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|Jq.run([Jq.field("name")])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.run pipeline: field then iterate", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice"), String("Bob")]|},
        {|Jq.run([Jq.field("users"), Jq.iterate, Jq.field("name")])(|}
        ++ nested
        ++ {|)|},
      )
    ),
    test_case("Jq.run pipeline: empty filter list is identity", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(42)]|},
        {|Jq.run([])(Int(42))|},
      )
    ),
    // ---- Tier 4: Mutation Combinators ----
    test_case("Jq.set adds new field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("email", String("a@b.c")), ("name", String("Alice")), ("age", Int(30))])]|},
        {|Jq.set("email", String("a@b.c"))(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.set overwrites existing field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("Bob")), ("age", Int(30))])]|},
        {|Jq.set("name", String("Bob"))(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.set on non-Assoc returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.set("x", Int(1))(Int(5))|},
      )
    ),
    test_case("Jq.update existing field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("ALICE")), ("age", Int(30))])]|},
        {|Jq.update("name", fun v -> case v | String(s) => [String(string_uppercase(s))] | _ => [v] end)(|}
        ++ obj
        ++ {|)|},
      )
    ),
    test_case("Jq.update missing field returns unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|Jq.update("missing", fun v -> [Int(0)])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.update on non-Assoc returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.update("x", fun v -> [v])(Int(5))|},
      )
    ),
    test_case("Jq.del removes field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("age", Int(30))])]|},
        {|Jq.del("name")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.del missing field returns same object", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|Jq.del("missing")(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.del on non-Assoc returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.del("x")(Int(5))|},
      )
    ),
    // ---- Tier 5: Structural Combinators ----
    test_case("Jq.to_entries on Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[List([Assoc([("key", String("name")), ("value", String("Alice"))]), Assoc([("key", String("age")), ("value", Int(30))])])]|},
        {|Jq.to_entries(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.to_entries on non-Assoc", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.to_entries(Int(5))|},
      )
    ),
    test_case("Jq.from_entries on List of entry objects", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("Alice")), ("age", Int(30))])]|},
        {|Jq.from_entries(List([Assoc([("key", String("name")), ("value", String("Alice"))]), Assoc([("key", String("age")), ("value", Int(30))])]))|},
      )
    ),
    test_case("Jq.from_entries on non-List", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.from_entries(Int(5))|},
      )
    ),
    test_case("Jq.to_entries then Jq.from_entries roundtrip", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|Jq.run([Jq.to_entries, Jq.from_entries])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.startswith true", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|Jq.startswith("hel")(String("hello"))|},
      )
    ),
    test_case("Jq.startswith false", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.startswith("world")(String("hello"))|},
      )
    ),
    test_case("Jq.startswith on non-String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.startswith("x")(Int(5))|},
      )
    ),
    test_case("Jq.startswith empty prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|Jq.startswith("")(String("hello"))|},
      )
    ),
    test_case("Jq.startswith longer than string", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.startswith("hello world")(String("hello"))|},
      )
    ),
    test_case("Jq.endswith true", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|Jq.endswith("llo")(String("hello"))|},
      )
    ),
    test_case("Jq.endswith false", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.endswith("world")(String("hello"))|},
      )
    ),
    test_case("Jq.endswith on non-String", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.endswith("x")(Int(5))|},
      )
    ),
    test_case("Jq.endswith empty suffix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|Jq.endswith("")(String("hello"))|},
      )
    ),
    // ---- Integration: pipeline with mutation ----
    test_case("Jq.run pipeline with select and field", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice"), String("Bob")]|},
        {|Jq.run([Jq.field("users"), Jq.iterate, Jq.select(Jq.has("age")), Jq.field("name")])(|}
        ++ nested
        ++ {|)|},
      )
    ),
    test_case("Jq.run pipeline with startswith filter", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("Alice")]|},
        {|Jq.run([Jq.field("users"), Jq.iterate, Jq.field("name"), Jq.select(Jq.startswith("Al"))])(|}
        ++ nested
        ++ {|)|},
      )
    ),
    // ---- Tier 6: Helper Combinators ----
    // Jq.run1 tests
    test_case("Jq.run1 returns first result", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|String("Alice")|},
        {|Jq.run1([Jq.field("name")])(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.run1 on empty pipeline", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Int(42)|},
        {|Jq.run1([])(Int(42))|},
      )
    ),
    test_case("Jq.run1 with iterate returns first element", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Int(1)|},
        {|Jq.run1([Jq.iterate])(|} ++ arr ++ {|)|},
      )
    ),
    test_case("Jq.run1 on empty result returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|Null|},
        {|Jq.run1([Jq.iterate])(Int(5))|},
      )
    ),
    // Jq.add tests
    test_case("Jq.add on Int", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(52)]|},
        {|Jq.add(10)(Int(42))|},
      )
    ),
    test_case("Jq.add negative", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(37)]|},
        {|Jq.add(-5)(Int(42))|},
      )
    ),
    test_case("Jq.add on non-Int returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.add(1)(String("hello"))|},
      )
    ),
    // Jq.not tests
    test_case("Jq.not on Bool(true)", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.not(Bool(true))|},
      )
    ),
    test_case("Jq.not on Bool(false)", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|Jq.not(Bool(false))|},
      )
    ),
    test_case("Jq.not on Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(true)]|},
        {|Jq.not(Null)|},
      )
    ),
    test_case("Jq.not on truthy value", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Bool(false)]|},
        {|Jq.not(Int(42))|},
      )
    ),
    // Jq.entry tests
    test_case("Jq.entry constructs entry object", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("key", String("Alice")), ("value", Int(30))])]|},
        {|Jq.entry(Jq.field("name"), Jq.field("age"))(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.entry with literal key", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("key", String("greeting")), ("value", String("Alice"))])]|},
        {|Jq.entry(fun _ -> [String("greeting")], Jq.field("name"))(|}
        ++ obj
        ++ {|)|},
      )
    ),
    // Jq.with_entries tests
    test_case("Jq.with_entries identity", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[|} ++ obj ++ {|]|},
        {|Jq.with_entries(Jq.identity)(|} ++ obj ++ {|)|},
      )
    ),
    test_case("Jq.with_entries modify values", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("name", String("ALICE")), ("age", String("30"))])]|},
        {|Jq.with_entries(fun entry ->
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
    // Jq.merge tests
    test_case("Jq.merge two objects", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("b", Int(2)), ("a", Int(1))])]|},
        {|Jq.merge(Assoc([("a", Int(1))]), Assoc([("b", Int(2))]))|},
      )
    ),
    test_case("Jq.merge overlapping keys", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Assoc([("a", Int(99)), ("b", Int(2))])]|},
        {|Jq.merge(Assoc([("a", Int(1)), ("b", Int(2))]), Assoc([("a", Int(99))]))|},
      )
    ),
    test_case("Jq.merge non-object returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.merge(Int(1), Assoc([("a", Int(1))]))|},
      )
    ),
    // Jq.string_sub tests
    test_case("Jq.string_sub replaces substring", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello world")]|},
        {|Jq.string_sub("planet", "world")(String("hello planet"))|},
      )
    ),
    test_case("Jq.string_sub no match unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|Jq.string_sub("xyz", "abc")(String("hello"))|},
      )
    ),
    test_case("Jq.string_sub on non-String returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.string_sub("a", "b")(Int(5))|},
      )
    ),
    // Jq.ltrimstr tests
    test_case("Jq.ltrimstr trims prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("world")]|},
        {|Jq.ltrimstr("hello ")(String("hello world"))|},
      )
    ),
    test_case("Jq.ltrimstr no match returns unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|Jq.ltrimstr("world")(String("hello"))|},
      )
    ),
    test_case("Jq.ltrimstr on non-String returns unchanged", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Int(5)]|},
        {|Jq.ltrimstr("x")(Int(5))|},
      )
    ),
    test_case("Jq.ltrimstr empty prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|Jq.ltrimstr("")(String("hello"))|},
      )
    ),
    // Jq.string_prepend tests
    test_case("Jq.string_prepend prepends", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello world")]|},
        {|Jq.string_prepend("hello ")(String("world"))|},
      )
    ),
    test_case("Jq.string_prepend empty prefix", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[String("hello")]|},
        {|Jq.string_prepend("")(String("hello"))|},
      )
    ),
    test_case("Jq.string_prepend on non-String returns Null", `Quick, () =>
      parse_and_evaluate_test(
        ~ignore_constructor_types=true,
        {|[Null]|},
        {|Jq.string_prepend("x")(Int(5))|},
      )
    ),
  ],
);
