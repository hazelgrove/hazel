// Slices of binding defs
open Test_Statics_Slicing_Prelude;

let binding_synthesis = [
  synthesis_case("bind-var-used", "let x = 1 in x", "Int", "let x = 1 in x"),
  synthesis_case(
    "bind-var-unused",
    "let x = 1 in true",
    "Bool",
    "let ? = ? in true",
  ),
  synthesis_case(
    "bind-tuple-one",
    "let (x, y) = (1, true) in x",
    "Int",
    "let (x, ?) = (1, ?) in x",
  ),
  synthesis_case(
    "bind-tuple-none",
    "let (x, y) = (1, true) in 0",
    "Int",
    "let ? = ? in 0",
  ),
  synthesis_case(
    "bind-tuple-nested",
    "let (x, (y, z)) = (1, (true, \"s\")) in y",
    "Bool",
    "let (?, (y, ?)) = (?, (true, ?)) in y",
  ),
  synthesis_case(
    ~ctx=ctx_var("f", "Bool -> Int"),
    "bind-tuple-combined-demands",
    "let (x, y, z) = (1, true, f) in (x, z(?))",
    "(Int, Int)",
    "let (x, ?, z) = (1, ?, f) in (x, z(?))",
  ),
  synthesis_case(
    ~ctx=ctx_var("f", "Bool -> Int"),
    "bind-tuple-annotated-part",
    "let (x : Int, y, z) = (1, true, f) in (x, z(?))",
    "(Int, Int)",
    "let (x : Int, ?, z) = (?, ?, f) in (x, z(?))",
  ),
  synthesis_case(
    "bind-ctor-used",
    "type T = +A(Int) in let A(x) = A(1) in x",
    "Int",
    "type T = +A(Int) in let A(x) = ? in x",
  ),
  synthesis_case(
    "bind-ctor-sum-used",
    "type T = +A(Int) in let A(x) = A(1) in x",
    "Int",
    "type T = +A(Int) in let A(x) = ? in x",
  ),
  synthesis_case(
    "bind-ctor-unused",
    "type T = +A(Int) in let A(x) = A(1) in 0",
    "Int",
    "type ? = ? in let ? = ? in 0",
  ),
  synthesis_case(
    "bind-ctor-pat-subtracts",
    "type T = +A((Int, Bool)) in let A((x, y)) = A((1, true)) in x",
    "Int",
    "type T = +A((Int, ?)) in let A((x, ?)) = ? in x",
  ),
  synthesis_case(
    "bind-ann-pat-subtracts",
    "let (x : Int) = 1 in x",
    "Int",
    "let (x : Int) = ? in x",
  ),
  synthesis_case(
    "bind-poly-ctor-pat",
    "type T = typfun A -> +C(A) in let C(x) = C@<Int>(1) in x",
    "Int",
    "type T = typfun A -> +C(A) in let C(x) = C@<Int>(?) in x",
  ),
  /* Polymorphic type info can come from the pattern AND the definition:
       type T = typfun A -> typfun B -> +D(A, B) in
       let D@<Int, ?>(x,y) = D@<?>@<Bool>(1, true) in (x, y)   @ (Int, Bool)
     keeping both instantiations and omitting only the arguments `1, true`.
     Skipped: this form does not yet parse on the polymorphism branch. */
  Alcotest.test_case("bind-poly-ctor-pat-both", `Quick, () => Alcotest.skip()),
  synthesis_case(
    "bind-joined-uses",
    "let x = (a=1, b=true) in (x.a, x.b)",
    "(Int, Bool)",
    "let x = (a=1, b=true) in (x.a, x.b)",
  ),
  synthesis_case(
    "bind-match-one-branch",
    "type T = A(Int) + B(Bool) in case A(1) | A(x) => x | B(y) => 0 end",
    "Int",
    "type T = A(Int) + ? in case ? | A(x) => x | ? => ? end",
  ),
  synthesis_case(
    "match-reverse-tuple-pat",
    "case (1, 2) | (x, y) => x end",
    "Int",
    "case (1, ?) | (x, ?) => x end",
  ),
  synthesis_case(
    "match-reverse-var-pat",
    "case (1, 2) | x => x end",
    "(Int, ?)",
    "case (1, ?) | x => x end",
  ),
  synthesis_case(
    "bind-recursive-value",
    "let loop(n : Int) : Int = loop(n) in loop",
    "Int -> Int",
    "let loop(? : Int) : Int = ? in loop",
  ),
  synthesis_case(
    "bind-recursive-call",
    "let loop(n : Int) : Int = loop(n) in loop(1)",
    "Int",
    "let loop(?) : Int = ? in loop(?)",
  ),
  synthesis_case(
    "bind-forall-bool",
    "forall x -> true",
    "Bool",
    "forall ? -> ?",
  ),
  synthesis_case(
    "bind-theorem-result",
    "theorem p = true in 1",
    "Int",
    "theorem ? = ? in 1",
  ),
  synthesis_case(
    "bind-param-option",
    "type Option(A) = None + Some(A) in Some@<Int>(1)",
    "Option(Int)",
    "type Option(A) = ? + Some(A) in Some@<Int>(?)",
  ),
  synthesis_case(
    ~focus=e => exp_var(e, "x"),
    "bind-var-focus-full",
    "let x : Int -> Int = ? in x",
    "Int -> Int",
    "let x : Int -> Int = ? in x",
  ),
  synthesis_case(
    ~focus=e => exp_var(e, "x"),
    "bind-var-focus-refined",
    "let x : Int -> Int = ? in x",
    "Int -> ?",
    "let x : Int -> ? = ? in x",
  ),
  synthesis_case(
    ~focus=first_tuple,
    "bind-syn-tuple-ann-def",
    "let x : (Int, Int) = (1, ?) in ?",
    "(Int, ?)",
    "let ? = (1, ?) in ?",
  ),
  synthesis_case(
    ~focus=first_tuple,
    "bind-syn-tuple-ann-def-full",
    "let x : (Int, Int) = (1, 2) in ?",
    "(Int, Int)",
    "let ? = (1, 2) in ?",
  ),
  synthesis_case(
    ~focus=first_fun,
    "bind-syn-fun-err-def",
    "let x : (Int, Int) = fun y -> y in ?",
    "? -> ?",
    "let ? = fun ? -> ? in ?",
  ),
  synthesis_case(
    ~focus=first_tuple,
    "bind-syn-tuple-gap",
    "let x : (Int, Int) = (1, ?) in ?",
    "?",
    "let ? = ? in ?",
  ),
  synthesis_case(
    "bind-param-curried",
    "type Result(E, A) = Error(E) + Ok(A) in Ok@<String, Bool>(true)",
    "Error(?) + Ok(Bool)",
    "type Result(?, A) = ? + Ok(A) in Ok@<?, Bool>(?)",
  ),
];

let modules = [
  synthesis_case(
    "module-shaped",
    "{ let x = 1; let y = true }",
    "(x=Int, y=?)",
    "{ let x = 1; let y = ? }",
  ),
  synthesis_case(
    "module-projection",
    "{ let x = 1; let y = true }.x",
    "Int",
    "{ let x = 1; ? }.x",
  ),
  synthesis_case(
    "module-keyword",
    "module M = { let x = 1; let y = true } in M.x",
    "Int",
    "module M = { let x = 1; ? } in M.x",
  ),
  synthesis_case(
    "module-type-item",
    "{ type T = Int; let x = (1 : T) }",
    "(x=Int)",
    "{ type T = Int; let x = (? : T) }",
  ),
  synthesis_case(
    "signature-projection",
    "let m : { let x : Int; let y : Bool } = { let x = 1; let y = true } in m.x",
    "Int",
    "let m : { let x : Int; ? } = { let x = 1; ? } in m.x",
  ),
  synthesis_case(
    "signature-type-member",
    "let m : { type T = Int; let x : T } = { type T = Int; let x = (1 : T) } in m.x",
    "T",
    "let m : { type T = Int; let x : T } = { type T = Int; let x = (? : T) } in m.x",
  ),
];

let binding_analysis = [
  analysis_case(
    ~focus=e => exp_var(e, "x"),
    "bind-ana-let-body",
    "(let x = 1 in x) : Int",
    "Int",
    "(let ? = ? in ?) : Int",
  ),
  analysis_case(
    ~focus=first_int,
    "bind-ana-let-ann-def",
    "let x : Int = 1 in x",
    "Int",
    "let ? : Int = ? in ?",
  ),
  analysis_case(
    ~focus=first_fun,
    "bind-ana-let-ann-fun-def",
    "let f : Int -> Int = fun x : ? -> x in f",
    "Int -> Int",
    "let ? : Int -> Int = ? in ?",
  ),
  analysis_case(
    ~focus=first_fun,
    "bind-ana-let-ann-unused-fun-def",
    "let f : Int -> Int = fun x : ? -> x in ?",
    "Int -> Int",
    "let ? : Int -> Int = ? in ?",
  ),
  analysis_case(
    ~focus=first_int,
    "bind-ana-tuple-ann-def",
    "let p : (Int, Bool) = (1, true) in p",
    "Int",
    "let ? : (Int, ?) = (?, ?) in ?",
  ),
  analysis_case(
    ~focus=first_int,
    "bind-ana-ctor-ann-def",
    "type T = +A(Int) in let v : T = A(1) in v",
    "Int",
    "type T = +A(Int) in let ? = A(?) in ?",
  ),
  analysis_case(
    ~focus=first_int,
    "bind-ana-module-item",
    "({ let x = 1; let y = true } : (x=Int, y=Bool))",
    "Int",
    "({ let x = ?; ? } : (x=Int, y=?))",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "bind-ana-fun-domain",
    "((fun x -> 0) : Int -> Int)",
    "Int",
    "((fun ? -> ?) : Int -> ?)",
  ),
];

let pattern_focus = [
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-ann-let",
    "let (x : Int) = 1 in 0",
    "Int",
    "let (? : Int) = ? in ?",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-ann-fun",
    "fun (x : Int) -> 0",
    "Int",
    "fun (? : Int) -> ?",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-tuple-component",
    "((fun (x, y) -> 0) : (Int, Bool) -> Int)",
    "Int",
    "((fun (?, ?) -> ?) : (Int, ?) -> ?)",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-list-component",
    "let [x] : [Int] = [1] in 0",
    "Int",
    "let [?] : [Int] = ? in ?",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-ctor-component",
    "type T = +A(Int) in let A(x) : T = A(1) in 0",
    "Int",
    "type T = +A(Int) in let A(?) : ? = ? in ?",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-ctor-shadow-ann",
    "type T1 = +A(Int) in type T2 = +A(String) in let A(x) : T1 = A(1) in 0",
    "Int",
    "type T1 = +A(Int) in type T2 = +A(String) in let A(?) : T1 = ? in ?",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-ctor-shadow-unann",
    "type T1 = +A(Int) in type T2 = +A(String) in let A(x) = A(\"s\") in 0",
    "String",
    "type ? = ? in type T2 = +A(String) in let A(?) = ? in ?",
  ),
  analysis_case(
    ~focus=pat_wild,
    "pat-wild",
    "((fun _ -> 0) : Int -> Int)",
    "Int",
    "((fun ? -> ?) : Int -> ?)",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-cons",
    "let x :: xs : [Int] = [1] in 0",
    "Int",
    "let ? :: ? : [Int] = ? in ?",
  ),
  analysis_case(
    ~focus=e => pat_var(e, "x"),
    "pat-label",
    "let (a=x) : (a=Int) = (a=1) in 0",
    "Int",
    "let (a=?) : (a=Int) = ? in ?",
  ),
];

let tests = (
  "Statics.Slicing.Binding",
  binding_synthesis @ modules @ binding_analysis @ pattern_focus,
);
