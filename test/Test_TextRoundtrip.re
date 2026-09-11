/* Tests for Haz3lcore.MarkerParse's text round-trip.
 *
 * Property under test: any *parser-originated* program survives the
 * text round-trip — `to_text(p) == to_text(of_text(to_text(p)) |> persist)`.
 * Programs that originate from text are parser-canonical by construction,
 * so all the grout placement decisions have already been settled by the
 * parser before to_text sees them.
 *
 *   - DocSlides (`Slow`): per-slide text fixed-point check. The shipped
 *     slides were created via the editor, which routes every keystroke
 *     through the parser, so they qualify. Complemented by
 *     Test_ReparseDocSlides, which asserts the stronger structural
 *     equality on the persisted `backup_text` field — text equality
 *     alone can mask a segment-shape divergence that prints the same.
 *   - TextReproducers (`Quick`): hand-picked text inputs that exercise
 *     specific shapes (let with hole, `?` token, `?((x))`, ...). These
 *     run on `-q` so we can iterate quickly.
 *   - Property (`Slow`): QCheck generates an arbitrary `Exp.t`, renders
 *     it through the standard show path (ExpToSegment + Printer with
 *     `~holes="?"`) to get a Hazel source text, then parses that text
 *     via the round-trip to establish a parser-canonical baseline, and
 *     checks the fixed-point. This sidesteps the ExpToSegment-vs-parser
 *     discrepancy (e.g. `Ap(EmptyHole, Parens(Var x))` where ExpToSegment
 *     emits a Grout in the function position that the parser doesn't
 *     reconstruct after marker deletion).
 *
 * Projectors round-trip through trigger syntax (`^^fold(body)` etc.):
 * `Printer.of_segment` unfolds them via `Triggers.projector_to_invoke`
 * and the parser reconstructs the wrapper via `Triggers.expand_projector`. */

open Alcotest;
open Haz3lcore;

let parse_or_fail = (~root=Sort.Exp, text) =>
  switch (MarkerParse.of_text(~root, text)) {
  | Some(z) => z
  | None => Alcotest.fail("of_text returned None on: " ++ text)
  };

let roundtripped_text = (~root=Sort.Exp, z: Zipper.t): string =>
  MarkerParse.to_text(z) |> parse_or_fail(~root) |> MarkerParse.to_text;

let slide_roundtrip_case =
    ((name, root, z): (string, Sort.t, unit => Zipper.t)) =>
  test_case(
    name,
    `Slow,
    () => {
      let z = z();
      let before = MarkerParse.to_text(z);
      let after = roundtripped_text(~root, z);
      check(
        string,
        "marker text round-trip is fixed-point for " ++ name,
        before,
        after,
      );
    },
  );

/* Slides are text-backed (committed .hz): materialize each via the load
   path so the usual fixed-point check applies. (Includes the B2T2
   slides: they were excluded when each cost ~2s via the typing parser,
   but the fast path loads them in milliseconds.) Materialization is
   DEFERRED into the test body: eager unpersist here would pin every
   slide's zipper — six of them mega-scale — for the whole suite run. */
let doc_slide_cases =
  Web.Init.documentation_slides
  |> List.filter(((name, _, _)) => !CorpusUtil.mega_scale(name))
  |> List.map(((name, root, p: PersistentZipper.t)) =>
       (name, root, () => PersistentZipper.unpersist(p, ~root))
     )
  |> List.map(slide_roundtrip_case);

let text_fixed_point_case = (~name, text) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = text |> parse_or_fail;
      let before = MarkerParse.to_text(z);
      let after = roundtripped_text(z);
      check(
        string,
        "text round-trip is fixed-point starting from: " ++ text,
        before,
        after,
      );
    },
  );

let text_reproducer_cases = [
  text_fixed_point_case(~name="flat let with hole", "let x = ¿ in x"),
  text_fixed_point_case(~name="explicit ? token", "?"),
  text_fixed_point_case(~name="?((x))", "?((x))"),
  text_fixed_point_case(~name="?(([]))", "?(([]))"),
  text_fixed_point_case(~name="?((1))", "?((1))"),
  /* Edge cases — small inputs that exercise places the round-trip could
   * plausibly trip up: holes in non-Exp positions, holes adjacent to
   * comments, refractor + hole, multiple holes, backtick identifiers,
   * trailing whitespace, etc. Add new shapes here as we find them. */
  text_fixed_point_case(~name="empty input", ""),
  text_fixed_point_case(~name="only whitespace", "  \n  "),
  text_fixed_point_case(~name="only comment", "# hello #"),
  text_fixed_point_case(~name="comment then hole", "# leading comment #\n¿"),
  text_fixed_point_case(~name="hole in pattern position", "let ¿ = 1 in 2"),
  text_fixed_point_case(~name="hole in type position", "let x : ¿ = 1 in x"),
  text_fixed_point_case(~name="multiple adjacent holes", "let x = ¿ in ¿"),
  text_fixed_point_case(~name="hole inside list literal", "[1, ¿, 3]"),
  text_fixed_point_case(~name="hole inside tuple", "(1, ¿, 3)"),
  text_fixed_point_case(~name="refractor wrapping a hole", "^^probe(¿)"),
  text_fixed_point_case(
    ~name="nested refractors with hole",
    "^^probe(^^statics(¿))",
  ),
  text_fixed_point_case(
    ~name="backtick identifier with hole",
    "let `weird name` = ¿ in `weird name`",
  ),
  text_fixed_point_case(
    ~name="trailing newline preserved",
    "let x = 1 in x\n",
  ),
  text_fixed_point_case(
    ~name="string literal alongside hole",
    "let s = \"hello\" in ¿",
  ),
  /* Shape from Tuples slide: let with Seq RHS, hole body, inside parens. */
  text_fixed_point_case(
    ~name="seq-RHS let with hole body in parens",
    "((let x = a;b in ¿))",
  ),
  text_fixed_point_case(
    ~name="seq-RHS let inside projector",
    "^^fold((let x = a;b in ¿))",
  ),
];

/* Render an arbitrary `Exp.t` to source text (same path
 * `QCheck_Util.arb_exp` uses for `show`), then parse it. Going through
 * the parser canonicalizes the segment so the fixed-point check is
 * apples-to-apples. */
let render_exp_as_text = (exp: Language.Exp.t): string =>
  exp
  |> ExpToSegment.exp_to_segment(
       ~settings=ExpToSegment.Settings.editable(~inline=true),
       _,
     )
  |> Printer.of_segment(~holes="?", _);

let arb_exp_roundtrip =
  QCheck.Test.make(
    ~name="TextRoundtrip: parser-canonical exp text round-trips",
    ~count=50,
    QCheck_Util.arb_exp(~minimal_idents=true, 5),
    exp => {
      let text = render_exp_as_text(exp);
      switch (MarkerParse.of_text(~root=Exp, text)) {
      | None => false
      | Some(z) => MarkerParse.to_text(z) == roundtripped_text(z)
      };
    },
  );

/* Concave-grout marker (`⧖`): an operator hole. The fast parse must
   accept it (that is the point of the marker — `1 ¿ 2` lexes as three
   operands and rejects, poisoning every reload of a text-persisted
   document that contains an infix hole), and the canonical print must
   be a fast-parse fixed point. */
let concave_fast_case = (~name, text) =>
  test_case(
    name,
    `Quick,
    () => {
      switch (FastParse.parsed_of_text(~root=Sort.Exp, text)) {
      | Error(why) => fail("fast parse rejected: " ++ why)
      | Ok(_) => ()
      };
      /* fast-parse fixed point via the persistence load path */
      switch (
        PersistentZipper.parse_text(~source="test", ~root=Sort.Exp, text)
      ) {
      | None => fail("parse_text returned None")
      | Some(z) =>
        let printed = MarkerParse.to_text(z);
        check(string, "print is the input", text, printed);
        switch (FastParse.parsed_of_text(~root=Sort.Exp, printed)) {
        | Error(why) => fail("reprint not fast-parseable: " ++ why)
        | Ok(_) => ()
        };
      };
    },
  );

let concave_marker_cases = [
  concave_fast_case(
    ~name="infix hole",
    "let x : Int = 1 \xe2\xa7\x96 2 in\nx",
  ),
  concave_fast_case(
    ~name="infix hole chain",
    "1 \xe2\xa7\x96 2 \xe2\xa7\x96 3",
  ),
  concave_fast_case(
    ~name="infix hole among operators",
    "1 + 2 \xe2\xa7\x96 3 * 4",
  ),
  /* NB `¿ ⧖ 2` standalone is NOT a legitimate case: load-time
     normalization (parse_text remold_regrouts fast results) deletes
     grout at fitting junctions, and no real zipper persists
     non-normal text. The canonical operand-hole-at-misfit shape: */
  concave_fast_case(~name="operand hole at misfit", "1 + \xc2\xbf * 2"),
  concave_fast_case(
    ~name="operator and operand holes in one program",
    "let x : Int = \xc2\xbf in\n1 \xe2\xa7\x96 x",
  ),
  /* legacy `¿`-as-infix text (persisted before the concave marker):
     still loads via the recovering parser and CANONICALIZES to the
     new spelling — the reprint must fast-parse */
  test_case("legacy infix ¿ canonicalizes to ⧖", `Quick, () =>
    switch (MarkerParse.of_text(~root=Sort.Exp, "1 \xc2\xbf 2")) {
    | None => fail("legacy parse failed")
    | Some(z) =>
      let printed = MarkerParse.to_text(z);
      switch (FastParse.parsed_of_text(~root=Sort.Exp, printed)) {
      | Error(why) =>
        fail("canonicalized legacy text not fast-parseable: " ++ why)
      | Ok(_) => ()
      };
    }
  ),
  /* slow-path parity: the marker also round-trips through the
     recovering parser */
  text_fixed_point_case(
    ~name="infix hole (recovering parser)",
    "1 \xe2\xa7\x96 2",
  ),
];

let debug_pieces = (tag, text) =>
  test_case(
    "DBG " ++ tag,
    `Quick,
    () => {
      switch (MarkerParse.of_text(~root=Sort.Exp, text)) {
      | None => print_endline(tag ++ ": slow parse None")
      | Some(z) =>
        let seg = Zipper.unselect_and_zip(z);
        print_endline(
          tag
          ++ " SLOW: "
          ++ String.concat(
               " ",
               List.map(
                 (p: Piece.t) =>
                   switch (p) {
                   | Tile(t) => "T(" ++ String.concat("", t.label) ++ ")"
                   | Grout({shape: Convex, _}) => "Gcvx"
                   | Grout({shape: Concave, _}) => "Gccv"
                   | Secondary(_) => "_"
                   | Projector(_) => "P"
                   },
                 seg,
               ),
             ),
        );
      };
      switch (FastParse.parsed_of_text(~root=Sort.Exp, text)) {
      | Error(why) => print_endline(tag ++ " FAST: Error " ++ why)
      | Ok({segment, _}) =>
        print_endline(
          tag
          ++ " FAST: "
          ++ String.concat(
               " ",
               List.map(
                 (p: Piece.t) =>
                   switch (p) {
                   | Tile(t) => "T(" ++ String.concat("", t.label) ++ ")"
                   | Grout({shape: Convex, _}) => "Gcvx"
                   | Grout({shape: Concave, _}) => "Gccv"
                   | Secondary(_) => "_"
                   | Projector(_) => "P"
                   },
                 segment,
               ),
             ),
        )
      };
    },
  );

let debug_parse_text = (tag, text) =>
  test_case("DBG PT " ++ tag, `Quick, () =>
    switch (
      FastParse.parsed_of_text(
        ~materialize=Triggers.invoked_projector,
        ~collect_refractors=true,
        ~root=Sort.Exp,
        text,
      )
    ) {
    | Error(why) => print_endline(tag ++ " PT-FAST Error: " ++ why)
    | Ok(_) => print_endline(tag ++ " PT-FAST Ok")
    }
  );

let tests = [
  ("TextRoundtrip.TextReproducers", text_reproducer_cases),
  ("TextRoundtrip.ConcaveMarker", concave_marker_cases),
  ("TextRoundtrip.DocSlides", doc_slide_cases),
  (
    "TextRoundtrip.Property",
    [QCheck_alcotest.to_alcotest(~speed_level=`Slow, arb_exp_roundtrip)],
  ),
];
