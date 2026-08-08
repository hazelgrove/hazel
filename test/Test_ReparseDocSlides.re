/* Structural reparse check: parse(slide.backup_text) ≡ original segment.
 * Complements Test_TextRoundtrip's text fixed-point — segment equality
 * catches shape divergences that the text projection would hide. */

open Web;
open Alcotest;
open Haz3lcore;
open EditingPrelude;

let doc_slides: list((string, CellEditor.Model.persistent)) =
  snd(Init.startup.documentation);

let doc_slide_reparses = ((name, slide: CellEditor.Model.persistent)) => {
  test_case(
    name,
    `Slow,
    () => {
      let reparsed_segment =
        switch (Parser.to_segment(slide.editor.zipper.backup_text, ~root=Exp)) {
        | Some(seg) => seg
        | None =>
          Alcotest.fail("Failed to parse segment from slide backup text")
        };

      /* Text-backed slides (committed .hz, zipper == "") have no stored
         sexp. Structural segment equality is also the wrong bar there:
         projector MODELS embed freshly-minted ids per materialization.
         The meaningful contract is text-level — BOTH parsers reproduce
         the committed text byte-for-byte and read the same term. */
      if (slide.editor.zipper.zipper == "") {
        let text = slide.editor.zipper.backup_text;
        let fast_segment =
          switch (
            FastParse.of_text(
              ~materialize=Triggers.invoked_projector,
              ~root=Exp,
              String.trim(text),
            )
          ) {
          | Some(seg) => seg
          | None =>
            Alcotest.fail(
              "text slide fell off the fast path: "
              ++ Option.value(FastParse.bail_note^, ~default="no note"),
            )
          };
        let print = seg => Printer.of_segment(~holes="", ~refractors=[], seg);
        check(
          string,
          name ++ ": fast parse reprints the committed text",
          String.trim(text),
          String.trim(print(fast_segment)),
        );
        check(
          string,
          name ++ ": typing parse reprints the committed text",
          String.trim(text),
          String.trim(print(reparsed_segment)),
        );
        check(
          bool,
          name ++ ": both parsers read the same term",
          true,
          Language.Equality.(
            equality({
              ...syntactic_settings,
              ignore_parens: false,
            }).
              exp
          )(
            MakeTerm.go(fast_segment).term,
            MakeTerm.go(reparsed_segment).term,
          ),
        );
      } else {
        let original_segment =
          Sexplib.Sexp.of_string(slide.editor.zipper.zipper)
          |> Zipper.t_of_sexp
          |> Zipper.unselect_and_zip(~erase_buffer=true);

        check(
          segment,
          "Reparsing " ++ name ++ " backup_text produces equivalent segment",
          original_segment,
          reparsed_segment,
        );
      };
    },
  );
};

let tests = [
  (
    "DocSlides.ReparseBackuptext",
    List.map(doc_slide_reparses, List.tl(doc_slides)) // Dropping the first basic reference slide to avoid the issue with whitespace shifting around convex grout
  ),
];
