open Alcotest;
open Language;
open FumolaGrammar;

/* The printer's corpus, and the two things it is checked against.

   Here: that each term prints to the string this file claims. That is a fast,
   pure check and it runs with the rest of the suite.

   Elsewhere: that each of those strings is accepted by the real Fumola
   parser, and means there what it means here. That needs cargo, so it lives
   in scripts/check-fumola-roundtrip.sh, which reads the corpus this test
   writes out. Keeping one list and two checks is deliberate -- a printer
   verified only against our own expectations would agree with itself about a
   grammar it had misread. */

let e = (t: exp_term(IdTagged.IdTag.t)): FumolaTermBase.t =>
  IdTagged.fresh(t);
let d = (t): FumolaGrammar.dec(IdTagged.IdTag.t) => IdTagged.fresh(t);
let p = (t): FumolaGrammar.pat(IdTagged.IdTag.t) => IdTagged.fresh(t);

let v = x => e(Var(x));
let n = i => e(Lit(Nat(string_of_int(i))));

/* (name, term, the source it must print to) */
let corpus: list((string, FumolaTermBase.t, string)) = [
  /* --- atoms --- */
  ("var", v("x"), "x"),
  ("nat", n(1), "1"),
  ("unit", e(Lit(Unit)), "()"),
  ("null", e(Lit(Null)), "null"),
  ("bool", e(Lit(Bool(true))), "true"),
  ("tuple", e(Tuple([n(1), n(2)])), "(1, 2)"),
  ("array", e(Array(false, [n(1), n(2)])), "[1, 2]"),
  ("var array", e(Array(true, [n(1)])), "[var 1]"),
  ("quoted id", e(QuotedId("t")), "`t"),
  ("prim", e(Prim("adaptonNow")), "prim \"adaptonNow\""),
  /* --- the precedence chain. The first two are the cases where Fumola
     differs from every language that spells these operators the same way. --- */
  (
    "bitor binds tighter than add",
    e(Bin(e(Bin(n(1), BitOr, n(2))), Add, n(3))),
    "1 | 2 + 3",
  ),
  (
    "add inside bitor needs parens",
    e(Bin(n(1), BitOr, e(Bin(n(2), Add, n(3))))),
    "1 | (2 + 3)",
  ),
  (
    "add is left-associative",
    e(Bin(e(Bin(n(1), Add, n(2))), Add, n(3))),
    "1 + 2 + 3",
  ),
  (
    "a right-nested add needs parens",
    e(Bin(n(1), Add, e(Bin(n(2), Add, n(3))))),
    "1 + (2 + 3)",
  ),
  (
    "mul is tighter than add",
    e(Bin(n(1), Add, e(Bin(n(2), Mul, n(3))))),
    "1 + 2 * 3",
  ),
  (
    "add inside mul needs parens",
    e(Bin(e(Bin(n(1), Add, n(2))), Mul, n(3))),
    "(1 + 2) * 3",
  ),
  (
    "shifts are non-associative",
    e(Bin(e(Bin(n(1), ShL, n(2))), ShL, n(3))),
    "(1 << 2) << 3",
  ),
  (
    "pow is left-associative",
    e(Bin(e(Bin(n(2), Pow, n(3))), Pow, n(2))),
    "2 ** 3 ** 2",
  ),
  (
    "a right-nested pow needs parens",
    e(Bin(n(2), Pow, e(Bin(n(3), Pow, n(2))))),
    "2 ** (3 ** 2)",
  ),
  (
    "rel is looser than add",
    e(Rel(e(Bin(n(1), Add, n(2))), Eq, n(3))),
    "1 + 2 == 3",
  ),
  (
    "and is looser than rel",
    e(And(e(Rel(v("a"), Eq, v("b"))), v("c"))),
    "a == b and c",
  ),
  (
    "or is loosest",
    e(Or(e(And(v("a"), v("b"))), v("c"))),
    "a and b or c",
  ),
  (
    "an or inside an and needs parens",
    e(And(e(Or(v("a"), v("b"))), v("c"))),
    "(a or b) and c",
  ),
  (
    "not binds tighter than and",
    e(And(e(Not(v("a"))), v("b"))),
    "not a and b",
  ),
  /* --- application and the unary forms --- */
  (
    "application is left-nested",
    e(Ap(e(Ap(v("f"), v("a"))), v("b"))),
    "f a b",
  ),
  (
    "an argument that is not an atom needs parens",
    e(Ap(v("f"), e(Ap(v("g"), v("x"))))),
    "f (g x)",
  ),
  (
    "application is tighter than add",
    e(Bin(e(Ap(v("f"), n(1))), Add, n(2))),
    "f 1 + 2",
  ),
  ("projection", e(Proj(v("e"), "0")), "e.0"),
  ("index", e(Index(v("a"), n(0))), "a[0]"),
  ("variant, no payload", e(Variant("tag", None)), "#tag"),
  ("variant with a payload", e(Variant("tag", Some(n(1)))), "#tag 1"),
  (
    "a variant payload is an atom",
    e(Variant("tag", Some(e(Bin(n(1), Add, n(2)))))),
    "#tag (1 + 2)",
  ),
  ("option", e(Opt(n(1))), "?1"),
  ("negation", e(Un(Neg, n(1))), "-1"),
  /* --- the adapton core. Every one of these is looser than every operator,
     so each needs parentheses to sit inside one; `force x + 1` is a syntax
     error in Fumola, not a misparse. --- */
  ("force", e(Force(v("x"))), "force x"),
  ("get", e(Get(v("cell"))), "@ cell"),
  ("put", e(Put(n(0), e(Lit(Null)))), "0 := null"),
  (
    "put is right-associative",
    e(Put(n(0), e(Put(n(1), n(2))))),
    "0 := 1 := 2",
  ),
  ("thunk", e(Thunk([d(DExp(n(1)))])), "thunk { 1 }"),
  (
    "a force inside an operator needs parens",
    e(Bin(e(Force(v("x"))), Add, n(1))),
    "(force x) + 1",
  ),
  (
    "a get inside an operator needs parens",
    e(Bin(e(Get(v("c"))), Add, n(1))),
    "(@ c) + 1",
  ),
  (
    "a thunk inside an operator needs parens",
    e(Bin(n(1), Add, e(Thunk([d(DExp(n(2)))])))),
    "1 + (thunk { 2 })",
  ),
  (
    "a get as an argument needs parens",
    e(Ap(v("f"), e(Get(v("c"))))),
    "f (@ c)",
  ),
  (
    "a put inside a comparison needs parens",
    e(Rel(e(Paren(e(Get(v("cell"))))), Eq, e(Lit(Null)))),
    "(@ cell) == null",
  ),
  (
    "do @",
    e(DoPutForce(e(QuotedId("k")), e(Block([d(DExp(n(1)))])))),
    "do @ `k { 1 }",
  ),
  (
    "do within",
    e(
      DoNav(
        Within,
        v("time"),
        e(QuotedId("t")),
        [d(DExp(e(Put(v("cell"), e(Opt(n(1)))))))],
      ),
    ),
    "do within time `t { cell := ?1 }",
  ),
  /* --- blocks. Bare braces are a block only in a nest position; anywhere
     else they read as an object literal, and `{ x }` is the record {x = 5}
     rather than the value of x. So a block outside a nest goes out as
     `do { … }`, which is a block wherever it stands. --- */
  (
    "a block outside a nest",
    e(Block([d(DLet(p(PVar("x")), n(1)))])),
    "do { let x = 1 }",
  ),
  (
    "a block of several decs",
    e(Block([d(DLet(p(PVar("x")), n(1))), d(DExp(v("x")))])),
    "do { let x = 1; x }",
  ),
  ("empty block", e(Block([])), "do { }"),
  (
    "a block inside an operator needs parens too",
    e(Bin(n(1), Add, e(Block([d(DExp(n(2)))])))),
    "1 + (do { 2 })",
  ),
  (
    "func takes a parenthesized parameter",
    e(Block([d(DFunc("f", p(PVar("x")), [d(DExp(v("x")))]))])),
    "do { func f(x) { x } }",
  ),
  /* --- control --- */
  (
    "a block in a nest position keeps its bare braces",
    e(
      If(
        e(Rel(v("b"), Eq, n(0))),
        e(Block([d(DExp(v("a")))])),
        Some(
          e(Block([d(DLet(p(PVar("y")), n(1))), d(DExp(v("y")))])),
        ),
      ),
    ),
    "if (b == 0) { a } else { let y = 1; y }",
  ),
  (
    "switch",
    e(
      Switch(
        v("x"),
        [
          {
            pat: p(PVariant("some", Some(p(PVar("y"))))),
            body: v("y"),
          },
          {
            pat: p(PWild),
            body: n(0),
          },
        ],
      ),
    ),
    "switch x { case (#some y) y; case _ 0 }",
  ),
  (
    "assert",
    e(
      Assert(e(Paren(e(Rel(e(Get(v("cell"))), Eq, e(Lit(Null))))))),
    ),
    "assert ((@ cell) == null)",
  ),
];

/* The corpus as the round-trip script consumes it: one program per line, and
   alongside it the same terms with every grouping made explicit. The script
   evaluates the two files line by line and requires them to agree, which is
   what gives it the power to catch a precedence level we read wrong -- both
   spellings parse, so `fumola check` alone would notice nothing. */
let corpus_path = "fumola-corpus.txt";
let explicit_corpus_path = "fumola-corpus-explicit.txt";

let write_corpus = () => {
  /* Both files come from the printer, not from the expectations above. The
     test cases tie the expectations to the printer; the script ties the
     printer to Fumola. Writing our claims here instead would have the script
     checking this file rather than the code. */
  let oc = open_out(corpus_path);
  corpus
  |> List.iter(((_, term, _)) =>
       output_string(oc, FumolaPrint.of_exp(term) ++ "\n")
     );
  close_out(oc);
  let oc = open_out(explicit_corpus_path);
  corpus
  |> List.iter(((_, term, _)) =>
       output_string(oc, FumolaPrint.of_exp(~explicit=true, term) ++ "\n")
     );
  close_out(oc);
};

let test_print = ((name, term, expected)) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, FumolaPrint.of_exp(term))
  );

/* A term with a hole has no Fumola spelling, and the caller has to know
   before it prints rather than after the runtime rejects it. */
let test_has_hole = () => {
  check(
    bool,
    "a plain term has no hole",
    false,
    FumolaPrint.has_hole(n(1)),
  );
  check(
    bool,
    "a hole nested in an operator is found",
    true,
    FumolaPrint.has_hole(e(Bin(n(1), Add, e(Hole(EmptyHole))))),
  );
  check(
    bool,
    "a hole nested in a block is found",
    true,
    FumolaPrint.has_hole(
      e(Thunk([d(DLet(p(PVar("x")), e(Hole(EmptyHole))))])),
    ),
  );
};

let tests = (
  "FumolaPrint",
  [
    test_case("write the corpus for the round-trip script", `Quick, () =>
      write_corpus()
    ),
    test_case("holes are found before printing", `Quick, test_has_hole),
  ]
  @ List.map(test_print, corpus),
);
