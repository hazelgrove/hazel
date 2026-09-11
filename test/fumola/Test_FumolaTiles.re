open Alcotest;
open Language;

/* The tile route, end to end: text typed into an editor becomes tiles, tiles
   become a FumolaGrammar term, and FumolaPrint turns that back into Fumola
   source.

   What each check is for:

   - `parses` is the claim that the tile grammar reads the Fumola form at all.
     A form Hazel has no tile for comes back as a hole, and the printer refuses
     to print a hole, so this separates "we can build it" from "we print it
     right".

   - `prints` is the claim that the term built from tiles prints to the Fumola
     source we mean. It is the same contract Test_FumolaPrint checks, but
     reached through the editor rather than by constructing terms by hand,
     which is what catches a MakeTerm case that reads a tile as the wrong
     constructor.

   The sources here go into the same corpus the round-trip script feeds to the
   real Fumola parser, so a tile route that builds something ungrammatical is
   caught there rather than here. */

let parse = (s: string): Exp.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("failed to parse: " ++ s)
  };

/* Pull the Fumola program out of a `fumola <instance> in … end`. */
let fumola_of = (e: Exp.t): option((FumolaTermBase.t, FumolaTermBase.t)) =>
  switch (e.term) {
  | FumolaQuote(name, body) => Some((name, body))
  | _ => None
  };

/* Every source here puts the form at the top level, so looking through the
   wrappers a whole-program parse can add is all that is needed. */
let rec find_fumola =
        (e: Exp.t): option((FumolaTermBase.t, FumolaTermBase.t)) =>
  switch (fumola_of(e)) {
  | Some(p) => Some(p)
  | None =>
    switch (e.term) {
    | Let(_, _, body)
    | Seq(_, body)
    | Filter(_, body)
    | Parens(body) => find_fumola(body)
    | _ => None
    }
  };

/* (name, what is typed into the editor, the instance, the Fumola source) */
let corpus: list((string, string, string, string)) = [
  ("a bare variable", "fumola store in x end", "store", "x"),
  ("a literal", "fumola store in 1 end", "store", "1"),
  ("an instance named something else", "fumola other in x end", "other", "x"),
  ("addition", "fumola store in 1 + 2 end", "store", "1 + 2"),
  (
    "multiplication binds tighter",
    "fumola store in 1 + 2 * 3 end",
    "store",
    "1 + 2 * 3",
  ),
  (
    "bitor binds tighter than addition, as the grammar has it",
    "fumola store in 1 | 2 + 3 end",
    "store",
    "1 | 2 + 3",
  ),
  (
    "and parentheses come back where they are needed",
    "fumola store in 1 | (2 + 3) end",
    "store",
    "1 | (2 + 3)",
  ),
  ("comparison", "fumola store in a == b end", "store", "a == b"),
  ("a put", "fumola store in 0 := 1 end", "store", "0 := 1"),
  ("a get", "fumola store in @ cell end", "store", "@ cell"),
  ("a force", "fumola store in force t end", "store", "force t"),
  (
    "a get inside an operator, which Fumola rejects without parentheses",
    "fumola store in (@ c) + 1 end",
    "store",
    "(@ c) + 1",
  ),
  ("application", "fumola store in f(a) end", "store", "f a"),
  ("a block", "fumola store in {x} end", "store", "do { x }"),
  /* Fumola spells a variant `#tag`, which Hazel cannot tokenize because `#`
     is its comment delimiter. The tile is `$tag` and the printer puts the
     `#` back; see Token.is_fumola_tag. */
  ("a variant", "fumola store in $tag end", "store", "#tag"),
  (
    "a variant with a payload",
    "fumola store in $tag(1) end",
    "store",
    "#tag 1",
  ),
  (
    "a variant payload that is not an atom keeps its parentheses",
    "fumola store in $tag(1 + 2) end",
    "store",
    "#tag (1 + 2)",
  ),
  (
    "a variant is tighter than an operator",
    "fumola store in $tag(1) + 2 end",
    "store",
    "#tag 1 + 2",
  ),
  /* `hazel … end` is the way back in: a Hazel expression standing where a
     Fumola term does. The livelit could carry one value, at the boundary of
     an opaque string; here it is a tile subtree, and there can be several,
     anywhere in the program. FumolaSource renders each as Fumola source. */
  ("a hazel expression", "fumola store in hazel 1 end end", "store", "(1)"),
  (
    "a hazel expression inside an operator",
    "fumola store in hazel 1 end + 2 end",
    "store",
    "(1) + 2",
  ),
  (
    "a hazel tuple crosses as a fumola tuple",
    "fumola store in hazel (1, true) end end",
    "store",
    "((1, true))",
  ),
  (
    "two of them, which the livelit's single input slot could not do",
    "fumola store in hazel 1 end + hazel 2 end end",
    "store",
    "(1) + (2)",
  ),
  (
    "a hazel expression as the argument of a force",
    "fumola store in force hazel 1 end end",
    "store",
    "force (1)",
  ),
];

let test_parses = ((name, src, instance, _)) =>
  test_case(name ++ " [parses]", `Quick, () => {
    switch (find_fumola(parse(src))) {
    | None => fail("no fumola term: " ++ src)
    | Some((n, body)) =>
      /* Report what could not be written, rather than only that something
         could not: the reason is the whole content of the failure. */
      check(
        string,
        "the instance and the program are both complete",
        "complete",
        Fumola.has_hole(n) || Fumola.has_hole(body)
          ? Option.value(
              ~default="incomplete",
              Fumola.why_unprintable(body),
            )
          : "complete",
      );
      check(string, "instance", instance, Fumola.of_exp(n));
    }
  });

let test_prints = ((name, src, _, expected)) =>
  test_case(name ++ " [prints]", `Quick, () => {
    switch (find_fumola(parse(src))) {
    | None => fail("no fumola term: " ++ src)
    | Some((_, body)) =>
      check(string, "fumola source", expected, Fumola.of_exp(body))
    }
  });

/* Fumola is a closed sub-language: a Hazel form written inside it must not
   expand into Hazel's own, or `let` would become `let _ = _ in`. */
let test_closed = () =>
  switch (find_fumola(parse("fumola store in x end"))) {
  | None => fail("no fumola term")
  | Some((_, body)) =>
    check(
      string,
      "the program is Fumola's, not Hazel's",
      "x",
      Fumola.of_exp(body),
    )
  };

let corpus_path = "fumola-tiles-corpus.txt";
let explicit_corpus_path = "fumola-tiles-corpus-explicit.txt";

/* What the tile route actually produced, both ways, for the round-trip
   script: the minimal spelling and the one with every grouping explicit.
   A term with a hole is left out -- it has no Fumola spelling, and sending
   one would be asking the parser about something we already know is not a
   program. */
let write_corpus = () => {
  let oc = open_out(corpus_path);
  let oc_x = open_out(explicit_corpus_path);
  corpus
  |> List.iter(((_, src, _, _)) =>
       switch (find_fumola(parse(src))) {
       | Some((_, body)) when !Fumola.has_hole(body) =>
         output_string(oc, Fumola.of_exp(body) ++ "\n");
         output_string(oc_x, Fumola.of_exp(~explicit=true, body) ++ "\n");
       | _ => ()
       }
     );
  close_out(oc);
  close_out(oc_x);
};

let tests = (
  "FumolaTiles",
  [
    test_case("write the tile corpus", `Quick, () => write_corpus()),
    test_case("fumola is a closed sub-language", `Quick, test_closed),
  ]
  @ List.map(test_parses, corpus)
  @ List.map(test_prints, corpus),
);
