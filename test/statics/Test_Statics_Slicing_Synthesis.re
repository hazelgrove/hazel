// Synthesis slicing tests
open Test_Statics_Slicing_Prelude;

let atoms = [
  synthesis_case("int-lit", "1", "Int", "1"),
  synthesis_case("bool-lit", "true", "Bool", "true"),
  synthesis_case("string-lit", "\"s\"", "String", "\"s\""),
  synthesis_case("float-lit", "1.0", "Float", "1.0"),
  synthesis_case("hole-pure", "?", "?", "?"),
  synthesis_case(
    ~assumptions=[("x", "Int")],
    "free-var-int",
    "x",
    "Int",
    "x",
  ),
];

let ctors = [
  synthesis_case(
    ~ctx=prelude_ctx("type T = A in"),
    ~constructors=[("A", "T")],
    "ctor-nullary-only",
    "A",
    "T",
    "A",
  ),
  synthesis_case(
    ~ctx=prelude_ctx("type T = A + B in"),
    ~aliases=[("T", "? + B")],
    ~constructors=[("B", "T")],
    "ctor-nullary-choice",
    "B",
    "T",
    "B",
  ),
  synthesis_case(
    ~ctx=prelude_ctx("type T = A(Int) in"),
    ~constructors=[("A", "Int -> T")],
    "ctor-payload-result",
    "A(1)",
    "T",
    "A(?)",
  ),
  synthesis_case(
    ~ctx=prelude_ctx("type T = A(Int) + B(Bool) in"),
    ~constructors=[("A", "Int -> T")],
    "ctor-payload-sensitive",
    "A(1)",
    "A(Int)",
    "A(?)",
  ),
  synthesis_case(
    ~ctx=prelude_ctx("type T = A(Int) + B(Bool) in"),
    ~aliases=[("T", "A(?) + ?")],
    ~constructors=[("A", "Int -> T")],
    "ctor-payload-choice",
    "A(1)",
    "T",
    "A(?)",
  ),
];

let tests = ("Statics.Slicing.Synthesis", atoms @ ctors);
