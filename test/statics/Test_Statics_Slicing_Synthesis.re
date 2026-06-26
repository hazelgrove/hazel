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

let functions = [
  synthesis_case(
    "fun-full",
    "fun (x : Int) -> x",
    "Int -> Int",
    "fun (x : Int) -> x",
  ),
  synthesis_case(
    "fun-body-demand",
    "fun (x : Int) -> x",
    "? -> Int",
    "fun (x : Int) -> x",
  ),
  synthesis_case(
    "fun-domain-only",
    "fun (x : Int) -> 1",
    "Int -> ?",
    "fun (x : Int) -> ?",
  ),
  synthesis_case(
    "fun-shape-only",
    "fun (x : Int) -> 1",
    "? -> ?",
    "fun ? -> ?",
  ),
  synthesis_case(
    "fun-unannotated-shape",
    "fun x -> x",
    "? -> ?",
    "fun ? -> ?",
  ),
  synthesis_case(
    "fun-nested-arrow-left",
    "fun (f : Int -> Bool) -> f",
    "(Int -> ?) -> (Int -> ?)",
    "fun (f : Int -> ?) -> f",
  ),
  synthesis_case(
    "fun-nested-arrow-cross",
    "fun (f : Int -> Bool) -> f",
    "(Int -> ?) -> (? -> Bool)",
    "fun (f : Int -> Bool) -> f",
  ),
  synthesis_case(
    ~ctx=ctx_var("f", "Int -> Bool"),
    ~assumptions=[("f", "? -> Bool")],
    "app-free-fn",
    "f(1)",
    "Bool",
    "f(?)",
  ),
  synthesis_case(
    ~ctx=ctx_var("f", "Int -> Bool"),
    ~focus=e => exp_var(e, "f"),
    ~assumptions=[("f", "Int -> Bool")],
    "app-function-focus",
    "f(1)",
    "Int -> Bool",
    "f(?)",
  ),
  synthesis_case(
    "app-inline-id",
    "(fun (x : Int) -> x)(1)",
    "Int",
    "(fun (x : Int) -> x)(?)",
  ),
  synthesis_case(
    "app-inline-const",
    "(fun (x : Int) -> true)(1)",
    "Bool",
    "(fun ? -> true)(?)",
  ),
  synthesis_case(
    ~ctx=ctx_var("add", "(Int, Int) -> Int"),
    ~assumptions=[("add", "(Int, ?) -> Int")],
    "deferred-app",
    "add(_, 1)",
    "Int -> Int",
    "add(_, ?)",
  ),
  synthesis_case(
    ~assumptions=[("string_length", "? -> Int")],
    "builtin-string-length",
    "string_length(\"s\")",
    "Int",
    "string_length(?)",
  ),
  synthesis_case(
    "typabs-value",
    "abs A -> fun (x : A) -> x",
    "poly A -> A -> A",
    "abs A -> fun (x : A) -> x",
  ),
];

let typaps = [
  synthesis_case(
    ~ctx=ctx_var("id", "poly A -> A -> A"),
    ~assumptions=[("id", "poly A -> A -> A")],
    "typap-single-call",
    "id@<Int>(1)",
    "Int",
    "id@<Int>(?)",
  ),
  synthesis_case(
    ~ctx=ctx_var("pair", "poly A, B -> A -> B -> (A, B)"),
    ~assumptions=[("pair", "poly A, B -> A -> B -> (A, B)")],
    "typap-multi-call",
    "pair@<Int, Bool>(1)(true)",
    "(Int, Bool)",
    "pair@<Int, Bool>(?)(?)",
  ),
  synthesis_case(
    ~ctx=ctx_var("const", "poly A, B -> A -> B -> A"),
    ~assumptions=[("const", "poly A, ? -> A -> ? -> A")],
    "typap-shaped-query",
    "const@<Int, Bool>",
    "Int -> ? -> Int",
    "const@<Int,?>",
  ),
  synthesis_case(
    ~ctx=prelude_ctx("type Option = typfun A -> None + Some(A) in"),
    ~aliases=[("Option", "typfun A -> ? + Some(A)")],
    "param-option-explicit",
    "Some@<Int>(1)",
    "Option(Int)",
    "Some@<Int>(?)",
  ),
  synthesis_case(
    ~ctx=
      prelude_ctx(
        "type Either = typfun A -> typfun B -> Left(A) + Right(B) in",
      ),
    ~aliases=[("Either", "typfun A -> typfun B -> ? + Right(B)")],
    "param-either-explicit",
    "Right@<Int, Bool>(true)",
    "Either(?, Bool)",
    "Right@<?, Bool>(?)",
  ),
  synthesis_case(
    ~ctx=prelude_ctx("type List = typfun A -> Nil + Cons(A, List(A)) in"),
    ~aliases=[("List", "typfun A -> ? + Cons(?)")],
    "param-list-recursive",
    "Cons@<Int>(1, Nil)",
    "List(Int)",
    "Cons@<Int>(?)",
  ),
  synthesis_case(
    ~ctx=prelude_ctx("type Option = typfun A -> None + Some(A) in"),
    ~aliases=[("Option", "typfun A -> ? + Some(A)")],
    "param-option-annotation",
    "(Some(1) : Option(Int))",
    "Option(Int)",
    "(Some(?) : Option(Int))",
  ),
  synthesis_case(
    ~ctx=
      prelude_ctx(
        "type Either = typfun A -> typfun B -> Left(A) + Right(B) in",
      ),
    ~aliases=[("Either", "typfun A -> typfun B -> ? + Right(B)")],
    "param-either-annotation",
    "(Right(true) : Either(Int, Bool))",
    "Either(?, Bool)",
    "(Right(?) : Either(?, Bool))",
  ),
];

let tests = (
  "Statics.Slicing.Synthesis",
  atoms @ ctors @ wrappers @ products @ functions @ typaps,
);
