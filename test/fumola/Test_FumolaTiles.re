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

/* Pull the instance, the mode and the program out of a `fumola … end`. */
type parsed = {
  instance: FumolaTermBase.t,
  mode: FumolaTermBase.t,
  body: FumolaTermBase.t,
};

let fumola_of = (e: Exp.t): option(parsed) =>
  switch (e.term) {
  | FumolaQuote(instance, mode, body) =>
    Some({
      instance,
      mode,
      body,
    })
  | _ => None
  };

/* Every source here puts the form at the top level, so looking through the
   wrappers a whole-program parse can add is all that is needed. */
let rec find_fumola = (e: Exp.t): option(parsed) =>
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
  /* The instance's adapton semantics, written beside its name. Changing it
     resets the instance, so it is not something a program sets in passing;
     leaving it out asks for no mode rather than for the default, so that one
     expression cannot reset an instance another has configured. */
  (
    "an instance with a mode",
    "fumola $graphical as store in 1 end",
    "store",
    "1",
  ),
  ("simple mode", "fumola $simple as store in 1 end", "store", "1"),
  ("a bare variable", "fumola ? as store in x end", "store", "x"),
  ("a literal", "fumola ? as store in 1 end", "store", "1"),
  (
    "an instance named something else",
    "fumola ? as other in x end",
    "other",
    "x",
  ),
  ("addition", "fumola ? as store in 1 + 2 end", "store", "1 + 2"),
  (
    "multiplication binds tighter",
    "fumola ? as store in 1 + 2 * 3 end",
    "store",
    "1 + 2 * 3",
  ),
  (
    "bitor binds tighter than addition, as the grammar has it",
    "fumola ? as store in 1 | 2 + 3 end",
    "store",
    "1 | 2 + 3",
  ),
  (
    "and parentheses come back where they are needed",
    "fumola ? as store in 1 | (2 + 3) end",
    "store",
    "1 | (2 + 3)",
  ),
  ("comparison", "fumola ? as store in a == b end", "store", "a == b"),
  ("a put", "fumola ? as store in 0 := 1 end", "store", "0 := 1"),
  ("a get", "fumola ? as store in @ cell end", "store", "@ cell"),
  ("a force", "fumola ? as store in force t end", "store", "force t"),
  (
    "a get inside an operator, which Fumola rejects without parentheses",
    "fumola ? as store in (@ c) + 1 end",
    "store",
    "(@ c) + 1",
  ),
  ("application", "fumola ? as store in f(a) end", "store", "f a"),
  ("a block", "fumola ? as store in {x} end", "store", "do { x }"),
  /* Fumola spells a variant `#tag`, which Hazel cannot tokenize because `#`
     is its comment delimiter. The tile is `$tag` and the printer puts the
     `#` back; see Token.is_fumola_tag. */
  ("a variant", "fumola ? as store in $tag end", "store", "#tag"),
  (
    "a variant with a payload",
    "fumola ? as store in $tag(1) end",
    "store",
    "#tag 1",
  ),
  (
    "a variant payload that is not an atom keeps its parentheses",
    "fumola ? as store in $tag(1 + 2) end",
    "store",
    "#tag (1 + 2)",
  ),
  (
    "a variant is tighter than an operator",
    "fumola ? as store in $tag(1) + 2 end",
    "store",
    "#tag 1 + 2",
  ),
  /* `hazel … end` is the way back in: a Hazel expression standing where a
     Fumola term does. The livelit could carry one value, at the boundary of
     an opaque string; here it is a tile subtree, and there can be several,
     anywhere in the program. FumolaSource renders each as Fumola source. */
  /* Fumola spells a string as Hazel does, so unlike `#tag` the token needs
     no respelling: it carries its quotes from the tile into Lit(Text) and
     out through the printer unchanged. */
  (
    "a string literal",
    "fumola ? as store in \"abc\" end",
    "store",
    "\"abc\"",
  ),
  (
    "a path with slashes in it, which is what imports need one for",
    "fumola ? as store in \"fumola/collections/levelTree\" end",
    "store",
    "\"fumola/collections/levelTree\"",
  ),
  /* The `=` is sugar in Fumola's own LetImport production, which accepts it
     either way; the tile is shaped like `let`, so it is always written. */
  (
    "an import",
    "fumola ? as store in import Seq = \"fumola/collections/levelTree\" end",
    "store",
    "do { import Seq = \"fumola/collections/levelTree\" }",
  ),
  (
    "an import and a use of what it binds",
    "fumola ? as store in {import Seq = \"fumola/collections/levelTree\"; Seq} end",
    "store",
    "do { import Seq = \"fumola/collections/levelTree\"; Seq }",
  ),
  /* Projection is what makes an import worth having: it is how the module
     the import binds is reached. */
  ("a projection", "fumola ? as store in e.x end", "store", "e.x"),
  (
    "a projection chains to the left",
    "fumola ? as store in e.x.y end",
    "store",
    "e.x.y",
  ),
  (
    "a numeric projection, which is the same node",
    "fumola ? as store in e.0 end",
    "store",
    "e.0",
  ),
  (
    "a projection applied, which is how a library function is called",
    "fumola ? as store in Seq.fromList(l) end",
    "store",
    "Seq.fromList l",
  ),
  ("unit", "fumola ? as store in () end", "store", "()"),
  (
    "a hazel expression",
    "fumola ? as store in hazel 1 end end",
    "store",
    "(1)",
  ),
  (
    "a hazel expression inside an operator",
    "fumola ? as store in hazel 1 end + 2 end",
    "store",
    "(1) + 2",
  ),
  (
    "a hazel tuple crosses as a fumola tuple",
    "fumola ? as store in hazel (1, true) end end",
    "store",
    "((1, true))",
  ),
  (
    "two of them, which the livelit's single input slot could not do",
    "fumola ? as store in hazel 1 end + hazel 2 end end",
    "store",
    "(1) + (2)",
  ),
  (
    "a hazel expression as the argument of a force",
    "fumola ? as store in force hazel 1 end end",
    "store",
    "force (1)",
  ),
];

let test_parses = ((name, src, instance, _)) =>
  test_case(name ++ " [parses]", `Quick, () => {
    switch (find_fumola(parse(src))) {
    | None => fail("no fumola term: " ++ src)
    | Some({instance: n, body, _}) =>
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
    | Some({body, _}) =>
      check(string, "fumola source", expected, Fumola.of_exp(body))
    }
  });

/* Fumola is a closed sub-language: a Hazel form written inside it must not
   expand into Hazel's own, or `let` would become `let _ = _ in`. */
let test_closed = () =>
  switch (find_fumola(parse("fumola ? as store in x end"))) {
  | None => fail("no fumola term")
  | Some({body, _}) =>
    check(
      string,
      "the program is Fumola's, not Hazel's",
      "x",
      Fumola.of_exp(body),
    )
  };

/* The mode is read from the syntax, and only $simple and $graphical are it. */
let test_mode = () => {
  let mode_of = src =>
    switch (find_fumola(parse(src))) {
    | None => "no fumola term"
    | Some({mode, _}) =>
      Fumola.has_hole(mode) ? "none" : Fumola.of_exp(mode)
    };
  check(
    string,
    "no mode written",
    "none",
    mode_of("fumola ? as s in 1 end"),
  );
  check(
    string,
    "graphical",
    "#graphical",
    mode_of("fumola $graphical as s in 1 end"),
  );
  check(
    string,
    "simple",
    "#simple",
    mode_of("fumola $simple as s in 1 end"),
  );
  /* A mode can come from Hazel, so an instance's configuration can be written
     once in Hazel's own terms rather than repeated in Fumola's. */
  check(
    string,
    /* Recased on the way out, so a mode written Hazel's way reaches the
       runtime spelled Fumola's way. See FumolaCase. */
    "a mode written in Hazel",
    "(#graphical)",
    mode_of("fumola hazel Graphical end as s in 1 end"),
  );
};

/* What the runtime is actually asked for, which is where a Hazel-written mode
   and a Fumola-written one have to agree. */
let test_mode_resolves = () => {
  let resolved = src =>
    switch (find_fumola(parse(src))) {
    | None => "no fumola term"
    | Some({mode, _}) =>
      switch (FumolaRun.mode_of(mode)) {
      | Ok(None) => "leave it alone"
      | Ok(Some(m)) => FumolaRun.mode_source(m)
      | Error(message) => "error: " ++ message
      }
    };
  check(
    string,
    "hole",
    "leave it alone",
    resolved("fumola ? as s in 1 end"),
  );
  check(
    string,
    "fumola's spelling",
    "graphical",
    resolved("fumola $graphical as s in 1 end"),
  );
  check(
    string,
    "hazel's spelling",
    "graphical",
    resolved("fumola hazel Graphical end as s in 1 end"),
  );
  check(
    string,
    "simple, from hazel",
    "simple",
    resolved("fumola hazel Simple end as s in 1 end"),
  );
  /* A variable bound to a mode cannot be read here: the program runs during
     elaboration, before anything is substituted. The message says so rather
     than silently leaving the mode alone. */
  check(
    bool,
    "a bound variable says why it cannot be read",
    true,
    switch (
      find_fumola(
        parse("let m = Graphical in fumola hazel m end as s in 1 end"),
      )
    ) {
    | Some({mode, _}) =>
      switch (FumolaRun.mode_of(mode)) {
      | Error(message) => String.length(message) > 0
      | _ => false
      }
    | None => false
    },
  );
};

/* Substitution reaches a Hazel expression embedded in a Fumola program. It
   did not until the traversals in TermBase were taught to enter a Fumola
   term: a variable bound outside the program never reached the escape that
   named it, so `hazel m end` rendered the *expression* `m`, which has no
   Fumola source.

   This is the half of the escape that works without deciding where the
   Fumola runtime lives. Actually *using* a bound variable also needs the
   program to run after substitution rather than during elaboration, which
   is the open question in docs/fumola-tiles-design.md. */
let test_substitution = () => {
  let printed = (body: FumolaTermBase.t) =>
    Fumola.has_hole(body)
      ? Option.value(~default="incomplete", Fumola.why_unprintable(body))
      : Fumola.of_exp(body);
  switch (find_fumola(parse("fumola ? as s in hazel m end end"))) {
  | None => fail("no fumola term")
  | Some({body, _}) =>
    check(
      string,
      "before substitution the escape holds the variable, which has no source",
      "no Fumola source for this expression",
      printed(body),
    );
    let bound =
      Substitution.in_exp(
        Environment.extend(
          Environment.Empty,
          ("m", DHExp.fresh(Atom(Int(Bigint.of_int(1))))),
        ),
        IdTagged.fresh(
          Grammar.FumolaQuote(
            IdTagged.fresh(FumolaGrammar.Var("s")),
            IdTagged.fresh(FumolaGrammar.Hole(EmptyHole)),
            body,
          ),
        ),
      );
    switch (bound.term) {
    | FumolaQuote(_, _, body) =>
      check(
        string,
        "after substitution it holds the value",
        "(1)",
        printed(body),
      )
    | _ => fail("not a fumola quote")
    };
  };
};

/* The naming convention on the boundary, in both directions.

   Nothing tested this before, which is how the two directions came to
   disagree: FumolaValue capitalised on the way in and FumolaSource left the
   name alone on the way out, so `#leaf` came back as `#Leaf`. A test that
   only ever went one way could not see it. */
let test_case_conversion = () => {
  check(string, "fumola to hazel", "Leaf", FumolaCase.to_hazel("leaf"));
  check(string, "hazel to fumola", "leaf", FumolaCase.to_fumola("Leaf"));
  check(
    string,
    "a camelCase tag keeps its humps",
    "AddNode",
    FumolaCase.to_hazel("addNode"),
  );
  check(
    string,
    "and gets them back",
    "addNode",
    FumolaCase.to_fumola("AddNode"),
  );
  /* The whole point: a tag that goes out must come back as itself. */
  List.iter(
    tag =>
      check(
        string,
        "round trip of " ++ tag,
        tag,
        FumolaCase.to_fumola(FumolaCase.to_hazel(tag)),
      ),
    ["leaf", "bin", "addNode", "forceBegin", "x"],
  );
  /* And the one shape that cannot: a tag already upper-case. Said plainly
     rather than left for a caller to discover. */
  check(
    bool,
    "a lower-case tag round trips",
    true,
    FumolaCase.round_trips("leaf"),
  );
  check(
    bool,
    "an upper-case tag does not",
    false,
    FumolaCase.round_trips("Leaf"),
  );
};

/* The bridge itself, not just the convention: a Hazel constructor must reach
   Fumola as the tag Fumola would write. */
let test_source_recases = () => {
  let rendered = (e: Exp.t) =>
    switch (FumolaSource.of_exp(e)) {
    | Ok(s) => s
    | Error(m) => "error: " ++ m
    };
  check(
    string,
    "a nullary constructor",
    "#leaf",
    rendered(DHExp.fresh(Constructor("Leaf", None))),
  );
};

/* The cursor inspector reads the info map, and an id missing from it reports
   as whitespace -- which is what every Fumola subterm did before there was a
   traversal to put them there. This checks the map itself rather than the
   panel: for each id in the program, an InfoFumola entry naming the form. */
let test_info_map = () =>
  test_case(
    "every Fumola subterm is in the info map",
    `Quick,
    () => {
      let e = parse("fumola ? as store in {let x = 1; $tag(x)} end");
      let (m, _) =
        Language.Statics.mk(CoreSettings.on, Builtins.ctx_init(None), e);
      let classes =
        Id.Map.bindings(m)
        |> List.filter_map(((_, info)) =>
             switch ((info: Info.t)) {
             | InfoFumola(f) => Some(FumolaCls.show(FumolaInfo.cls_of(f)))
             | _ => None
             }
           );
      let has = c =>
        Alcotest.check(
          Alcotest.bool,
          c ++ " is reported",
          true,
          List.mem(c, classes),
        );
      has("Variant");
      has("Let Declaration");
      has("Pattern Variable");
      has("Integer Literal");
      has("Variable Reference");
      has("Block");
    },
  );

/* The editor looks an info up by the id of the *piece* under the cursor
   (Indicated.ci_of), so entries filed under some other id never reach the
   panel. This walks the segment the editor would hold and asks for each
   tile by its own id. */
let test_info_map_by_piece = () =>
  test_case(
    "every Fumola tile's own id has an info",
    `Quick,
    () => {
      let src = "fumola ? as store in {let x = 1; $tag(x)} end";
      let seg =
        switch (Haz3lcore.Parser.to_segment(src, ~root=Exp)) {
        | Some(seg) => seg
        | None => Alcotest.fail("failed to parse: " ++ src)
        };
      /* The term has to come from this same segment: two parses of one string
         mint different ids, and the ids are the whole point here. */
      let term =
        Haz3lcore.MakeTerm.from_zip_for_sem(
          Haz3lcore.Zipper.unzip(seg),
          ~root=Exp,
        ).
          term;
      let (m, _) =
        Language.Statics.mk(CoreSettings.on, Builtins.ctx_init(None), term);
      let rec tiles = (seg: Haz3lcore.Segment.t) =>
        seg
        |> List.concat_map((p: Haz3lcore.Piece.t) =>
             switch (p) {
             | Tile(t) => [t, ...List.concat_map(tiles, t.children)]
             | _ => []
             }
           );
      let missing =
        tiles(seg)
        |> List.filter_map((t: Haz3lcore.Tile.t) =>
             Id.Map.mem(t.id, m) ? None : Some(String.concat("", t.label))
           );
      Alcotest.check(
        Alcotest.(list(string)),
        "tiles with no info",
        [],
        missing,
      );
    },
  );

/* Typing a program passes through every prefix of it, and Parser.to_segment
   inserts character by character down the same path the editor uses. A
   prefix that raises is a crash a reader would hit mid-word -- which no
   whole-program test can see. */
let test_prefixes = () =>
  test_case(
    "every prefix of every tile program parses",
    `Quick,
    () => {
      let sources = List.map(((_, src, _, _)) => src, corpus);
      let failures =
        sources
        |> List.concat_map(src => {
             let n = String.length(src);
             List.init(n, i => String.sub(src, 0, i + 1));
           })
        |> List.filter_map(prefix =>
             switch (Haz3lcore.Parser.to_segment(prefix, ~root=Exp)) {
             | _ => None
             | exception exn =>
               Some(prefix ++ " -> " ++ Printexc.to_string(exn))
             }
           );
      Alcotest.check(
        Alcotest.(list(string)),
        "prefixes that raise",
        [],
        failures,
      );
    },
  );

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
       | Some({body, _}) when !Fumola.has_hole(body) =>
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
    test_info_map(),
    test_info_map_by_piece(),
    test_prefixes(),
    test_case(
      "the instance's mode is read from the syntax",
      `Quick,
      test_mode,
    ),
    test_case(
      "the mode resolves to what the runtime is asked for",
      `Quick,
      test_mode_resolves,
    ),
    test_case("substitution reaches the escape", `Quick, test_substitution),
    test_case(
      "names recase in both directions",
      `Quick,
      test_case_conversion,
    ),
    test_case(
      "the bridge recases on the way out",
      `Quick,
      test_source_recases,
    ),
  ]
  @ List.map(test_parses, corpus)
  @ List.map(test_prints, corpus),
);
