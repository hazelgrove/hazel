open Alcotest;
open Language;

/* The round trip the parser exists for.

   Test_FumolaPrint checks that a term prints to the string we claim, and the
   round-trip script checks that the real Fumola parser accepts that string
   and agrees about what it means. Neither can catch a printer that is
   *self-consistently* wrong: if the printer and the corpus both spell a form
   the same wrong way, they agree with each other and the Fumola parser is
   never shown the difference.

   `parse(print(t)) == t` can. It compares terms, not text, and the parser
   reads Fumola's own grammar rather than ours -- juxtaposed application, for
   one, which the tiles cannot even spell. The two share exactly one thing,
   the precedence ladder in FumolaPrint, which is deliberate: a level changed
   there changes both directions at once, so they cannot drift.

   Terms are compared with ids erased, since the parser makes fresh ones. */

let rec erase = (e: FumolaTermBase.t): FumolaTermBase.t => {
  let go = erase;
  let ann = IdTagged.IdTag.mk_internal([]);
  let term: FumolaTermBase.exp_term =
    switch (Annotated.term_of(e)) {
    | Hole(h) => Hole(h)
    | Hazel(h) => Hazel(h)
    | Var(x) => Var(x)
    | Lit(l) => Lit(l)
    | QuotedId(x) => QuotedId(x)
    | Prim(s) => Prim(s)
    | Paren(e) => Paren(go(e))
    | Tuple(es) => Tuple(List.map(go, es))
    | Array(v, es) => Array(v, List.map(go, es))
    | Block(ds) => Block(List.map(erase_dec, ds))
    | Thunk(ds) => Thunk(List.map(erase_dec, ds))
    | Ap(a, b) => Ap(go(a), go(b))
    | Proj(e, f) => Proj(go(e), f)
    | Index(a, b) => Index(go(a), go(b))
    | Bang(e) => Bang(go(e))
    | Variant(t, p) => Variant(t, Option.map(go, p))
    | Opt(e) => Opt(go(e))
    | Un(u, e) => Un(u, go(e))
    | Not(e) => Not(go(e))
    | Unquote(e) => Unquote(go(e))
    | Bin(a, o, b) => Bin(go(a), o, go(b))
    | Rel(a, o, b) => Rel(go(a), o, go(b))
    | And(a, b) => And(go(a), go(b))
    | Or(a, b) => Or(go(a), go(b))
    | If(c, t, f) => If(go(c), go(t), Option.map(go, f))
    | Switch(e, cs) =>
      Switch(
        go(e),
        List.map(
          (c: FumolaGrammar.case(_, _)): FumolaGrammar.case(_, _) =>
            {
              pat: erase_pat(c.pat),
              body: go(c.body),
            },
          cs,
        ),
      )
    | Assert(e) => Assert(go(e))
    | Ignore(e) => Ignore(go(e))
    | Return(e) => Return(Option.map(go, e))
    | Force(e) => Force(go(e))
    | Get(e) => Get(go(e))
    | Put(a, b) => Put(go(a), go(b))
    | DoPutForce(a, b) => DoPutForce(go(a), go(b))
    | DoNav(n, d, e, ds) =>
      DoNav(n, go(d), go(e), List.map(erase_dec, ds))
    };
  {
    term,
    annotation: ann,
  };
}

and erase_dec = (d: FumolaTermBase.dec): FumolaTermBase.dec => {
  let ann = IdTagged.IdTag.mk_internal([]);
  let term: FumolaGrammar.dec_term(_, _) =
    switch (Annotated.term_of(d)) {
    | DHole(h) => DHole(h)
    | DExp(e) => DExp(erase(e))
    | DLet(p, e) => DLet(erase_pat(p), erase(e))
    | DVar(p, e) => DVar(erase_pat(p), erase(e))
    | DImport(p, e) => DImport(erase_pat(p), erase(e))
    | DFunc(n, p, ds) => DFunc(n, erase_pat(p), List.map(erase_dec, ds))
    };
  {
    term,
    annotation: ann,
  };
}

and erase_pat = (p: FumolaTermBase.pat): FumolaTermBase.pat => {
  let ann = IdTagged.IdTag.mk_internal([]);
  let term: FumolaGrammar.pat_term(_, _) =
    switch (Annotated.term_of(p)) {
    | PHole(h) => PHole(h)
    | PVar(x) => PVar(x)
    | PWild => PWild
    | PLit(l) => PLit(l)
    | PParen(p) => PParen(erase_pat(p))
    | PTuple(ps) => PTuple(List.map(erase_pat, ps))
    | PVariant(t, p) => PVariant(t, Option.map(erase_pat, p))
    | POpt(p) => POpt(erase_pat(p))
    };
  {
    term,
    annotation: ann,
  };
};

let parsed = (src: string): FumolaTermBase.t =>
  switch (FumolaParse.exp(src)) {
  | Ok(e) => e
  | Error({at, message}) =>
    fail(message ++ ", at character " ++ string_of_int(at) ++ ": " ++ src)
  };

/* Sources the parser must read, each also a thing the printer must produce.
   Written the way Fumola writes them, including the two the tiles cannot --
   `#tag` and juxtaposed application. */
let corpus = [
  "x",
  "1",
  "()",
  "null",
  "true",
  "(1, 2)",
  "[1, 2]",
  "[var 1]",
  "`t",
  "prim \"adaptonNow\"",
  /* the ladder, including the two levels intuition gets backwards */
  "1 | 2 + 3",
  "1 | (2 + 3)",
  "1 + 2 + 3",
  "1 + (2 + 3)",
  "1 + 2 * 3",
  "(1 + 2) * 3",
  "(1 << 2) << 3",
  "2 ** 3 ** 2",
  "1 + 2 == 3",
  "a == b and c",
  "a and b or c",
  "(a or b) and c",
  "not a and b",
  /* application is juxtaposition, which no tile can spell */
  "f a",
  "f a b",
  "f (g x)",
  "f 1 + 2",
  "e.0",
  "a[0]",
  /* variants, spelled Fumola's way */
  "#tag",
  "#tag 1",
  "#tag (1 + 2)",
  "#tag 1 + 2",
  "?1",
  "-1",
  /* the adapton core */
  "force x",
  "@ cell",
  "0 := null",
  "0 := 1 := 2",
  "thunk { 1 }",
  "(force x) + 1",
  "(@ c) + 1",
  "1 + (thunk { 2 })",
  "f (@ c)",
  "(@ cell) == null",
  "do @ `k { 1 }",
  "do within time `t { cell := ?1 }",
  /* blocks and declarations */
  "do { let x = 1 }",
  "do { let x = 1; x }",
  "do { func f(x) { x } }",
  /* The `=` is optional in Fumola's own grammar and always printed here, so
     the round trip fixes on the spelling with it. */
  "do { import Seq = \"fumola/collections/levelTree\" }",
  "do { import Seq = \"fumola/collections/levelTree\"; Seq }",
  "if (b == 0) { a } else { let y = 1; y }",
  "switch x { case (#some y) y; case _ 0 }",
  "assert ((@ cell) == null)",
];

/* print(parse(s)) == s: the parser read every part of the source, and the
   printer put all of it back. A form either side mishandles shows up as a
   difference in the text. */
let test_print_parse = (src: string) =>
  test_case(src, `Quick, () =>
    check(string, src, src, Fumola.of_exp(parsed(src)))
  );

/* parse(print(t)) == t, structurally. The stronger of the two: it compares
   terms, so a printer and a parser that agreed on a wrong *string* would
   still be caught by the term they disagree about. */
let test_parse_print = (src: string) =>
  test_case(
    src,
    `Quick,
    () => {
      let once = parsed(src);
      let twice = parsed(Fumola.of_exp(once));
      check(
        bool,
        "the term survives a trip through its own printed form",
        true,
        erase(once) == erase(twice),
      );
    },
  );

/* A source the parser cannot read says where, rather than silently
   producing a hole. */
let test_errors = () => {
  let message = src =>
    switch (FumolaParse.exp(src)) {
    | Ok(_) => "parsed"
    | Error({message, _}) => message
    };
  check(string, "an unclosed paren", "expected )", message("(1"));
  check(string, "nothing at all", "expected an expression", message(""));
  check(
    string,
    "a stray delimiter",
    "there is more here than one expression",
    message("1 }"),
  );
};

let tests = (
  "FumolaParse",
  [test_case("errors say what was expected", `Quick, test_errors)]
  @ List.map(test_print_parse, corpus)
  @ List.map(test_parse_print, corpus),
);
