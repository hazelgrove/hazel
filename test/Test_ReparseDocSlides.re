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

      let original_segment =
        Sexplib.Sexp.of_string(slide.editor.zipper.zipper)
        |> Zipper.t_of_sexp
        |> Zipper.unselect_and_zip(~erase_buffer=true);

      print_endline(
        "Original segment: "
        ++ Segment.to_string(
             original_segment,
             ~projector_to_segment=_ => [],
             ~refractor_seg_to_seg=(r, s) => (r, s),
           ),
      );
      print_endline(
        "Reparsed segment: "
        ++ Segment.to_string(
             reparsed_segment,
             ~projector_to_segment=_ => [],
             ~refractor_seg_to_seg=(r, s) => (r, s),
           ),
      );

      check(
        segment,
        "Reparsing " ++ name ++ " backup_text produces equivalent segment",
        original_segment,
        reparsed_segment,
      );
    },
  );
};

/* Editor-authored slides whose persisted segment differs structurally from a
 * reparse of their backup_text: Basic Reference has whitespace shifting around
 * convex grout; Fisheries Case Study has projector-internal syntax (fold/csv/
 * probe contents) that regenerates differently on parse. Their text round-trip
 * is still covered by Test_TextRoundtrip. */
let reparse_exempt = ["Basic Reference", "Fisheries Case Study"];

let tests = [
  (
    "DocSlides.ReparseBackuptext",
    List.map(
      doc_slide_reparses,
      List.filter(
        ((name, _)) => !List.mem(name, reparse_exempt),
        doc_slides,
      ),
    ),
  ),
];
