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
         The meaningful contract is text-level: the LOAD path (fast or
         fallback) reproduces the committed text byte-for-byte, and when
         the fast path succeeds it reads the same term as the typing
         parse. */
      if (slide.editor.zipper.zipper == "") {
        let text = slide.editor.zipper.backup_text;
        let z = PersistentZipper.from_backup_text(text, ~root=Exp);
        check(
          string,
          name ++ ": load path reproduces the committed text",
          String.trim(text),
          String.trim(TextRoundtrip.to_text(PersistentSegment.persist(z))),
        );
        switch (
          FastParse.of_text(
            ~materialize=Triggers.invoked_projector,
            ~collect_refractors=true,
            ~root=Exp,
            String.trim(text),
          )
        ) {
        | None => () /* fallback slide (menhir gap): fidelity checked above */
        | Some(fast_segment) =>
          check(
            bool,
            name ++ ": fast and typing parses read the same term",
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
          )
        };
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
  ("DocSlides.ReparseBackuptext", List.map(doc_slide_reparses, doc_slides)),
];
