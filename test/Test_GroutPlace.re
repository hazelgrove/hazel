open Alcotest;
open Haz3lcore;

/* Artifact-side grout placement: policy pins + the invariants the
   virtual-grout integration spec requires be in place BEFORE the
   placement layer is wired into the live artifact path.

   Policy cases are ported from virtual-grout's Test_HolePlacement
   (PR #2165), translated to material grout: the hole is a real piece
   occupying its own cell, INSERTED at the policy position (one space
   from its anchor token; blank-line slots; comments as walls). The
   original branch drew zero-width/replace-a-cell decorations instead;
   those degradations don't exist materially — see the note in
   GroutPlace.re on why consume-a-space placement was rejected (the
   invariants below caught it as non-idempotent on first run).

   Renders are piece-faithful (~indent=""): rows show exactly the
   secondaries in the segment, no measured indentation — placement is
   about position within the run, display indentation is Measured's
   separate concern.

     ?   convex hole (missing operand)
     ~   concave hole (missing operator)

   Invariants (the spec's ratchet, guarded from day one):
     G-DETERMINISM   place(seg) twice is identical, ids included
     G-IDEMPOTENCE   place(place(seg)) == place(seg)
     G-STRIP-BLIND   place(seg) == place(strip(seg)) — input grout
                     carries no information
     G-PURITY        the Grout module's mutable refs (cache_id,
                     suppressed-space) cannot influence placement
     G-ROUNDTRIP     serializing holes as nothing, reparsing, and
                     re-placing reproduces the same placement exactly
                     (the round-trip bug class as a property)
   Plus the coincidence probes: completion-minted grout ids diverge
   across independent derivations today (the gap this layer closes),
   and place() makes them coincide. */

let string_testable = testable(Fmt.string, String.equal);

let parse = (s: string): Segment.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, s)) {
  | None => Alcotest.fail("parse failed: " ++ String.escaped(s))
  | Some(z) => Zipper.unselect_and_zip(z)
  };

let print = (~holes="?", ~concave_holes="~", seg): string => {
  let measured = Measured.of_segment(seg, Id.Map.empty, Id.Map.empty);
  Printer.of_segment(~holes, ~concave_holes, ~indent="", ~measured, seg);
};

let place_text = (s: string): string =>
  s |> parse |> GroutPlace.place |> print;

let t = (name: string, input: string, expected: string) =>
  test_case(name, `Quick, () =>
    check(
      string_testable,
      Printf.sprintf("%s (input: %s)", name, String.escaped(input)),
      expected,
      place_text(input),
    )
  );

/* PLACEMENT RULE CHANGED 2026-07-26 (andrew): interior gaps put AT
   MOST ONE space before the hole, remainder after — reverting toward
   virtual-grout's anchor rule and away from the centering tried on
   2026-07-24. Rationale: P10 FILL-POSITION AFFINITY (the hole must
   sit at/after the caret so typed content REPLACES it rather than
   vanishing from behind the caret) plus trajectory weighting (gaps
   of 1-3 are what left-to-right entry visits; wider gaps are
   marginal). Also maximally stable: the hole moves only on the 1->2
   gap transition. Pins in this file with a leading space before the
   sigil were re-judged under that rule. */
let space_runs = [
  t("operand hole, one space", "let x = in x", "let x =? in x"),
  t("operand hole, two spaces", "let x =  in x", "let x = ? in x"),
  t("operand hole, three spaces", "let x =   in x", "let x = ?  in x"),
  t(
    "operand hole, long run anchors left",
    "let x =     in x",
    "let x = ?    in x",
  ),
  t("operator hole, one space", "1 2", "1~ 2"),
  t("operator hole, two spaces", "1  2", "1 ~ 2"),
  t("operator hole, long run anchors left", "1     2", "1 ~    2"),
  t("adjacent operators", "1 + + 2", "1 +? + 2"),
  t("leading operand hole in child", "let x = + 2 in x", "let x =? + 2 in x"),
];

let empty_runs = [
  t("no whitespace, child trailing", "(1 +)", "(1 +?)"),
  t("no whitespace, child leading", "(* 2)", "(?* 2)"),
  t("leading at top level, no whitespace", "* 2", "?* 2"),
  t("leading anchors right: one space before token", "  * 2", "?  * 2"),
  t("leading long run anchors right", "    * 2", "  ?  * 2"),
];

let trailing_edge = [
  t("trailing at top level, no whitespace", "1 +", "1 +?"),
  t("trailing space survives next to the hole", "1 + ", "1 + ?"),
  t("trailing two spaces: hole after the first", "1 +  ", "1 + ? "),
  t(
    "trailing long run: one space after, rest continues",
    "1 +    ",
    "1 + ?   ",
  ),
  t("trailing at top level, linebreak", "1 +\n", "1 +\n?"),
  t(
    "let chain trailing hole gets next line",
    "let x = 1 in\n",
    "let x = 1 in\n?",
  ),
  t(
    "space before trailing linebreak: hole still next line",
    "let a = 1 in \n",
    "let a = 1 in \n?",
  ),
  t("trailing indent after linebreak: hole at caret", "1 +\n  ", "1 +\n  ?"),
  t(
    "trailing extra linebreaks don't drag the hole down",
    "1 +\n\n",
    "1 +\n?\n",
  ),
  t(
    "trailing blank then blank: first blank line wins",
    "1 +\n  \n",
    "1 +\n  ?\n",
  ),
  t("blank program: hole stays on the first line", "\n\n\n", "?\n\n\n"),
  t("blank program with typed indent: hole at its end", "  \n\n", "  ?\n\n"),
  t(
    "leading blank lines before content: hole on the first",
    "\n\n* 2",
    "?\n\n* 2",
  ),
];

let linebreaks = [
  t(
    "mid-segment linebreak: end of previous line",
    "let x =\nin x",
    "let x =?\nin x",
  ),
  t(
    "mid-segment space then linebreak: space survives",
    "let x = \nin x",
    "let x = ?\nin x",
  ),
  t(
    "case scrutinee: stays on case's line",
    "case\n| 1 => 1 end",
    "case?\n| 1 => 1 end",
  ),
  t(
    "case scrutinee with indented rule",
    "case\n  | 1 => 1 end",
    "case?\n  | 1 => 1 end",
  ),
  t(
    "blank line inside gap is the prepared slot",
    "let x =\n\nin x",
    "let x =\n?\nin x",
  ),
  t(
    "indented blank line: hole at end of indent",
    "let a =\n  \nin a",
    "let a =\n  ?\nin a",
  ),
  t(
    "multiple blank lines: first one, never further down",
    "let x =\n\n\nin x",
    "let x =\n?\n\nin x",
  ),
  t(
    "blank line beats trailing spaces on the owner's line",
    "let a =  \n  \nin a",
    "let a =  \n  ?\nin a",
  ),
  t(
    "comment occupies the blank line: hole falls back to owner's line",
    "let a =\n  #c#\nin a",
    "let a =?\n  #c#\nin a",
  ),
  t(
    "spaces before linebreak: hole on previous line",
    "let x =  \nin x",
    "let x = ? \nin x",
  ),
];

let gallery = [
  t(
    "missing if condition, one space",
    "if then 2 else 3",
    "if? then 2 else 3",
  ),
  t(
    "missing if condition, two spaces",
    "if  then 2 else 3",
    "if ? then 2 else 3",
  ),
  t("missing list element", "[1, , 3]", "[1,? , 3]"),
  t("missing fun pattern", "fun -> 2", "fun? -> 2"),
  t("missing ap argument", "f( )", "f(? )"),
  t("adjacent operands", {|"a" "b"|}, {|"a"~ "b"|}),
  t("missing operand before in", "let x = 1 + in x", "let x = 1 +? in x"),
  t("trailing hole after multiline program", "1 +\n2 +\n", "1 +\n2 +\n?"),
];

let comments = [
  t("space then comment: hole after the space", "1 + #c#", "1 +? #c#"),
  t("comment directly adjacent: pinched before it", "1 +#c#", "1 +?#c#"),
  t("linebreak then comment: end of previous line", "1 +\n#c#", "1 +?\n#c#"),
  t("no conflict across comment line", "1 +\n#c#\n2", "1 +\n#c#\n2"),
];

/* ---------- invariants ---------- */

let corpus: list(string) = [
  "let x = in x",
  "let x =  in x",
  "1 2",
  "1 + + 2",
  "(1 +)",
  "(* 2)",
  "* 2",
  "  * 2",
  "1 +",
  "1 + ",
  "1 +    ",
  "1 +\n",
  "let x = 1 in\n",
  "1 +\n  ",
  "1 +\n\n",
  "\n\n\n",
  "let x =\nin x",
  "case\n| 1 => 1 end",
  "let x =\n\nin x",
  "let a =\n  #c#\nin a",
  "if then 2 else 3",
  "[1, , 3]",
  "fun -> 2",
  "f( )",
  {|"a" "b"|},
  "1 + #c#",
  "1 +#c#",
  "1 +\n#c#\n2",
];

let sx = seg => Base.show_segment(seg);

let flat = (s: string): string =>
  String.split_on_char('\n', s) |> String.concat(" ⏎ ");

/* run `f` over the corpus, collecting violation lines; green == "" */
let sweep = (name: string, f: (string, Segment.t) => option(string)) =>
  test_case(
    name,
    `Quick,
    () => {
      let out =
        corpus
        |> List.map(s =>
             switch (f(s, parse(s))) {
             | None => ""
             | Some(v) =>
               Printf.sprintf("input=%s %s\n", String.escaped(s), v)
             }
           )
        |> String.concat("");
      check(string_testable, name, "", out);
    },
  );

let invariants = [
  sweep("G-DETERMINISM", (_, seg) => {
    let a = GroutPlace.place(seg);
    let b = GroutPlace.place(seg);
    sx(a) == sx(b) ? None : Some("placements differ");
  }),
  sweep("G-IDEMPOTENCE", (_, seg) => {
    let a = GroutPlace.place(seg);
    let b = GroutPlace.place(a);
    sx(a) == sx(b)
      ? None
      : Some("once=" ++ flat(print(a)) ++ " twice=" ++ flat(print(b)));
  }),
  sweep("G-STRIP-BLIND", (_, seg) => {
    let a = GroutPlace.place(seg);
    let b = GroutPlace.place(GroutPlace.strip(seg));
    sx(a) == sx(b)
      ? None
      : Some(
          "with-grout=" ++ flat(print(a)) ++ " stripped=" ++ flat(print(b)),
        );
  }),
  sweep("G-PURITY", (_, seg) => {
    let a = GroutPlace.place(seg);
    Grout.cache_id(Some(Id.mk()));
    Grout.mark_space_owed(Id.mk());
    let b = GroutPlace.place(seg);
    Grout.cache_id(None);
    sx(a) == sx(b) ? None : Some("mutable refs influenced placement");
  }),
  /* holes are not text: serializing them as nothing gives back exactly
     the segment's secondaries, so reparse + re-place must reproduce
     the same placement (positions; ids differ with the reparse's fresh
     tile ids, as they should) */
  sweep("G-ROUNDTRIP", (_, seg) => {
    let a = GroutPlace.place(seg);
    let plain = print(~holes="", ~concave_holes="", a);
    switch (Parser.to_zipper(~root=Sort.Exp, plain)) {
    | None => Some("unparseable: " ++ String.escaped(plain))
    | Some(z) =>
      let b = GroutPlace.place(Zipper.unselect_and_zip(z));
      print(a) == print(b)
        ? None
        : Some(
            "diverges: placed="
            ++ flat(print(a))
            ++ " reparsed="
            ++ flat(print(b)),
          );
    };
  }),
];

/* ---------- id coincidence ---------- */

let rec grout_ids = (seg: Segment.t): list(string) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Grout(g) => [Id.to_string(g.id)]
      | Tile(t) => List.concat_map(grout_ids, t.children)
      | _ => []
      },
    seg,
  )
  |> List.sort(String.compare);

/* Two independent completions of the same segment: the grout
   completion mints carries fresh random ids, so RAW completion
   outputs disagree on hole ids — the mechanism behind the
   coincidence gap the promise render's determinism argument
   silently skipped. The "minted-differ" pin documents that raw
   divergence, which REMAINS by design (regrout is untouched); the
   wired artifact path closes it by composing place() after reify —
   the strong form lives in Test_CompletionDisplay ("every artifact
   grout id in reified info_map"). The place() probes below show the
   closure at the pure layer: strip+re-place makes both derivations
   agree on positions AND ids. */
let coincidence = {
  let seg = parse("let x = 1");
  let complete = () =>
    CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg).
      completed_seg;
  let c1 = complete();
  let c2 = complete();
  [
    test_case("completion output carries grout", `Quick, () =>
      check(bool, "has grout", true, grout_ids(c1) != [])
    ),
    test_case("completion output is placed: ids coincide", `Quick, ()
      /* the joined step sealed the historical gap at the completion
         boundary itself — complete_segment_deep returns PLACED
         output, so independent derivations agree by construction
         (this pin read "minted-differ" while raw completion leaked
         its internal random-id grout) */
      =>
        check(
          string_testable,
          "gap",
          "coincide",
          grout_ids(c1) == grout_ids(c2) ? "coincide" : "minted-differ",
        )
      ),
    test_case("place makes independent derivations coincide", `Quick, () =>
      check(
        string_testable,
        "ids",
        "coincide",
        grout_ids(GroutPlace.place(c1)) == grout_ids(GroutPlace.place(c2))
          ? "coincide" : "differ",
      )
    ),
    test_case("place makes placements text-identical", `Quick, () =>
      check(
        string_testable,
        "text",
        print(GroutPlace.place(c1)),
        print(GroutPlace.place(c2)),
      )
    ),
  ];
};

/* P9 HARNESS FIDELITY: the felt harness and the DISPLAY path must
   place holes in the same cells. FeltPrint exists to approximate the
   screen; when the two drifted (the retired `reorder` pass hopped
   holes past their gap's spaces in the display path only) every felt
   pin could pass while the editor was visibly wrong. This guard
   fails on any such drift.

   Compared: FeltPrint.render of the placed segment vs the display
   path's own render with ghost material and markers stripped. What
   it does NOT cover: the DOM/pixel layer (Code.re margins and the
   thin-X overlay) — that remains eyes-only. */
let fidelity = {
  let strip_ghost = (s: string): string => {
    let rec go = (s: string): string =>
      switch (Test_CompletionDisplay.split_first("⟪", s)) {
      | None => s
      | Some((pre, rest)) =>
        switch (Test_CompletionDisplay.split_first("⟫", rest)) {
        | None => pre
        | Some((_, post)) => pre ++ go(post)
        }
      };
    go(s);
  };
  let strip_caret = (s: string): string => {
    let rec go = s =>
      switch (Test_CompletionDisplay.split_first("¦", s)) {
      | None => s
      | Some((pre, post)) => pre ++ go(post)
      };
    go(s);
  };
  let corpus = [
    "let=",
    "let =",
    "let  =",
    "let   =",
    "let    =",
    "let     =",
    "1 +",
    "1 +  ",
    "f( )",
    "[1, , 3]",
    "(1 +)",
    "let x =  in x",
    "case\n| 1 => 1 end",
  ];
  [
    test_case(
      "harness agrees with the display path",
      `Quick,
      () => {
        let out =
          corpus
          |> List.map(src => {
               let z =
                 Test_Editing.perform(
                   Zipper.init(),
                   Test_Editing.mk(src ++ "¦"),
                 );
               let placed =
                 GroutPlace.place(
                   Zipper.unselect_and_zip(~erase_buffer=true, z),
                 );
               /* compare POSITIONS, not glyph vocabulary: the felt
                  renderer distinguishes pinch sigils (‽ ∻) where the
                  display printer prints plain ? ~ */
               let felt =
                 FeltPrint.render(
                   ~sigil=
                     (_, shape: Grout.shape) =>
                       switch (shape) {
                       | Convex => "?"
                       | Concave => "~"
                       },
                   placed,
                 );
               let disp =
                 switch (
                   Test_CompletionDisplay.display_state(~chips=false, z)
                 ) {
                 | s => strip_caret(strip_ghost(s))
                 | exception _ => "RAISED"
                 };
               /* TRAILING-PAD NORMALISATION. The felt side places the
                  RAW segment, so its last hole is line-end and takes
                  a pad (LineEndPadded). The display side has GHOST
                  material after that hole (`⟪in ?⟫`), so the same
                  hole is not line-end there and takes none; the ghost
                  TEXT is stripped after rendering, which hides that
                  structural difference. The two segments genuinely
                  differ in what follows the final hole, so the pad
                  legitimately differs. Normalise it away on both
                  sides — this guard exists to catch POSITION
                  divergence (the reorder class), which it still
                  does. */
               let norm = (x: string): string =>
                 x
                 |> String.split_on_char('\n')
                 |> List.map(line =>
                      String.length(line) >= 2
                      && line.[String.length(line) - 1] == '?'
                      && line.[String.length(line) - 2] == ' '
                        ? String.sub(line, 0, String.length(line) - 2) ++ "?"
                        : line
                    )
                 |> String.concat("\n");
               norm(felt) == norm(disp)
                 ? ""
                 : Printf.sprintf(
                     "src=%s felt=%s disp=%s\n",
                     String.escaped(src),
                     String.escaped(felt),
                     String.escaped(disp),
                   );
             })
          |> String.concat("");
        check(string_testable, "felt == display placement", "", out);
      },
    ),
  ];
};

/* P12 PADDING SOUNDNESS. Two spacing rules exist and answer different
   questions: SpaceNormalize.needs_space is LEXICAL SAFETY (would these
   tokens glom? is separation required?), deliberately conservative so
   the pretty-printer never disturbs user spacing; the completion
   oracle's f1_pad_style is STYLE (how formatted code looks), a
   superset — it pads `,` `?` and `x` `=`, which the printer leaves
   alone. Reuse was tried 2026-07-26 and rejected: consuming
   needs_space under-padded system material to `f(a,?,?)` and
   `let x= ? in ?`.

   They may differ, but must never CONTRADICT: anything lexically
   required must also be style-padded, or the display would glom two
   tokens together. This pins that implication so the two cannot drift
   into conflict. */
let pad_corpus = [
  "let",
  "in",
  "if",
  "then",
  "else",
  "case",
  "end",
  "fun",
  "=>",
  "|",
  "x",
  "foo",
  "1",
  "42",
  "?",
  "=",
  "+",
  "-",
  "!",
  "*",
  "::",
  "&&",
  "<",
  "(",
  ")",
  "[",
  "]",
  ",",
  ";",
  "\"s\"",
];

/* tokens that can mold as a prefix operator (Form: Not, UnaryMinus,
   TypSumSingle) — the P15 hug is a MOLD fact, so the implication is
   swept under both moldings of these */
let prefixable = ["!", "-", "+"];

let padding_soundness = [
  test_case(
    "lexically-required spacing is always style-padded",
    `Quick,
    () => {
      /* P15 scope note: `?` in this corpus stands for the HOLE, which
         is not a token — nothing can lexically glom with a hole, so
         needs_space's verdict on the literal glyph `?` reasons about
         a character that isn't in the document. Hole junctions are
         therefore exempt from the lexical premise here (their padding
         is pinned by the felt corpus); before this exemption the
         implication "passed" on pairs like `!`|`?` for exactly that
         bogus reason. */
      let hole = PadStyle.hole_token;
      let bad =
        List.concat_map(
          a =>
            List.concat_map(
              b =>
                if (a == hole || b == hole) {
                  [];
                } else {
                  List.filter_map(
                    l_prefix =>
                      SpaceNormalize.needs_space(a, b)
                      && !CanonicalCompletion.f1_pad_style(~l_prefix, a, b)
                        ? Some(
                            Printf.sprintf(
                              "%s|%s%s",
                              a,
                              b,
                              l_prefix ? "(pre)" : "",
                            ),
                          )
                        : None,
                    List.mem(a, prefixable) ? [false, true] : [false],
                  );
                },
              pad_corpus,
            ),
          pad_corpus,
        );
      check(
        string_testable,
        "no contradictions",
        "",
        String.concat(" ", bad),
      );
    },
  ),
];

let tests = [
  ("GroutPlace: padding soundness", padding_soundness),
  ("GroutPlace: harness fidelity", fidelity),
  ("GroutPlace: space runs", space_runs),
  ("GroutPlace: empty runs", empty_runs),
  ("GroutPlace: trailing edge", trailing_edge),
  ("GroutPlace: linebreaks", linebreaks),
  ("GroutPlace: gallery", gallery),
  ("GroutPlace: comments", comments),
  ("GroutPlace: invariants", invariants),
  ("GroutPlace: id coincidence", coincidence),
];
