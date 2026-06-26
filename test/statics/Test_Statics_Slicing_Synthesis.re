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

let wrappers = [
  synthesis_case("ascription-int", "(1 : Int)", "Int", "(? : Int)"),
  synthesis_case("label-full", "(l=1)", "(l=Int)", "(l=1)"),
  synthesis_case("label-gradual", "(l=1)", "(l=?)", "(l=?)"),
  synthesis_case("explicit-nonlabel", "(~1)", "(~Int)", "(~1)"),
];

let products = [
  synthesis_case("tuple-full", "(1, true)", "(Int, Bool)", "(1, true)"),
  synthesis_case("tuple-left", "(1, true)", "(Int, ?)", "(1, ?)"),
  synthesis_case("tuple-right", "(1, true)", "(?, Bool)", "(?, true)"),
  synthesis_case("tuple-shape-only", "(1, true)", "(?, ?)", "(?, ?)"),
  synthesis_case(
    "tuple-nested",
    "(1, (true, \"s\"))",
    "(?, (Bool, ?))",
    "(?, (true, ?))",
  ),
  synthesis_case("dot-left", "(a=1, b=true).a", "Int", "(a=1, ?).a"),
  synthesis_case(
    "tuple-extension-left",
    "(a=1) ... (b=true)",
    "(a=Int, b=?)",
    "(a=1) ... (b=?)",
  ),
  synthesis_case(
    "tuple-extension-right",
    "(a=1) ... (b=true)",
    "(a=?, b=Bool)",
    "(a=?) ... (b=true)",
  ),
  synthesis_case("list-single-full", "[1]", "[Int]", "[1]"),
  synthesis_case("list-single-gradual", "[1]", "[?]", "[?]"),
  synthesis_case("list-empty-gradual", "[]", "[?]", "[]"),
  synthesis_case("list-cons-full", "1 :: []", "[Int]", "1 :: ?"),
  synthesis_case("list-cons-gradual", "1 :: []", "[?]", "? :: ?"),
  synthesis_case("list-concat-full", "[1] @ []", "[Int]", "[1] @ ?"),
  synthesis_case("list-concat-gradual", "[1] @ []", "[?]", "? @ ?"),
];

let tests = (
  "Statics.Slicing.Synthesis",
  atoms @ ctors @ wrappers @ products,
);
