// Analysis slicing tests
open Test_Statics_Slicing_Prelude;

let examples = [
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "ana-fun-param-ann",
    "fun (x : Int) -> 1",
    "Int",
    "fun (? : Int) -> ?",
  ),
  analysis_case(
    ~ctx=ctx_var("f", "Int -> Bool"),
    ~focus=first_int,
    ~assumptions=[("f", "Int -> ?")],
    "ana-app-arg",
    "f(1)",
    "Int",
    "f(?)",
  ),
  // SLICING_DERIVATIONS.md: ana-app-function
  analysis_case(
    ~ctx=ctx_var("f", "Int -> Bool"),
    ~focus=e => exp_var(e, "f"),
    "ana-app-function",
    "(f(1) : Bool)",
    "? -> Bool",
    "(? (?) : Bool)",
  ),
  analysis_case(
    ~focus=first_int,
    "ana-ascription-inner",
    "(1 : Int)",
    "Int",
    "(? : Int)",
  ),
  analysis_case(
    ~focus=first_int,
    "ana-tuple-element",
    "((1, true) : (Int, Bool))",
    "Int",
    "((?, ?) : (Int, ?))",
  ),
  analysis_case(
    ~focus=first_int,
    "ana-list-element",
    "([1] : [Int])",
    "Int",
    "([?] : [Int])",
  ),
  analysis_case(
    "regression-ana-fun-list-param-query",
    "fun [] -> 1",
    "[?] -> ?",
    "fun [] -> ?",
  ),
  // SLICING_DERIVATIONS.md: regression-ana-projection-base-query
  analysis_case(
    ~ctx=ctx_var("x", "(A=())"),
    ~focus=e => exp_var(e, "x"),
    "regression-ana-projection-base-query",
    "x.A",
    "(A=?)",
    "?.A",
  ),
  analysis_case(
    ~focus=e => cons_head_ctor(e, "Z"),
    // SLICING_DERIVATIONS.md: regression-ana-sum-list-cons-query
    "regression-ana-sum-list-cons-query",
    "Z:: Z",
    "+ Z + ?",
    "?:: ?",
  ),
  analysis_case(
    ~focus=e => exp_var(e, "a"),
    // SLICING_DERIVATIONS.md: regression-ana-let-list-pattern-query
    "regression-ana-let-list-pattern-query",
    "let [] = a in (())",
    "[?]",
    "let [] = ? in ?",
  ),
  analysis_case(
    ~focus=e => exp_ctor(e, "Z"),
    // SLICING_DERIVATIONS.md: regression-ana-fix-sum-body-query
    "regression-ana-fix-sum-body-query",
    "fix Z -> Z",
    "+ Z + ?",
    "fix Z -> ?",
  ),
  analysis_case(
    ~focus=e => fix_body_int(e, 0),
    "regression-ana-fix-atom-body-query",
    "fix 0 -> 0",
    "Int",
    "fix 0 -> ?",
  ),
  // SLICING_DERIVATIONS.md: regression-ana-fix-atom-body-query (control)
  analysis_case(
    ~focus=e => exp_var(e, "x"),
    "ana-fix-annotated-binder-body",
    "fix (x : Int) -> x",
    "Int",
    "fix (? : Int) -> ?",
  ),
  analysis_case(
    ~focus=first_empty_list,
    // SLICING_DERIVATIONS.md: regression-ana-cons-tail-head-query
    "regression-ana-cons-tail-head-query",
    "0:: [[]]",
    "Int",
    "0:: [?]",
  ),
  analysis_case(
    ~focus=first_exp_hole,
    "regression-ana-fix-annotated-hole-query",
    "fix(Z:(? -> ?)) -> ?",
    "? -> ?",
    "fix(? : (? -> ?)) -> ?",
  ),
  analysis_case(
    ~ctx=ctx_var("c", "Bool"),
    ~focus=first_int,
    "ana-if-branch",
    "(if c then 1 else 2) : Int",
    "Int",
    "(if ? then ? else ?) : Int",
  ),
  analysis_case(
    ~ctx=prelude_ctx("type T = A + B in"),
    ~focus=first_int,
    "ana-match-branch",
    "(case A | A => 1 | B => 2 end) : Int",
    "Int",
    "(case ? | ? => ? | ? => ? end) : Int",
  ),
  analysis_case(~focus=first_int, "ana-op-operand", "1 + 2", "Int", "? + ?"),
  analysis_case(
    ~focus=first_binop,
    "ana-test-body",
    "test 1 == 1 end",
    "Bool",
    "test ? end",
  ),
  analysis_case(
    ~focus=first_bool,
    "ana-filter-body",
    "(hide 1 in true) : Bool",
    "Bool",
    "(hide ? in ?) : Bool",
  ),
];

let tests = ("Statics.Slicing.Analysis", examples);
