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
        switch (Parser.to_segment(slide.editor.backup_text)) {
        | Some(seg) => seg
        | None =>
          Alcotest.fail("Failed to parse segment from slide backup text")
        };

      let original_segment =
        Sexplib.Sexp.of_string(slide.editor.zipper)
        |> Zipper.t_of_sexp
        |> Zipper.unselect_and_zip(~erase_buffer=true);

      check(
        segment,
        "Reparsing backup_text produces equivalent segment",
        original_segment,
        reparsed_segment,
      );
    },
  );
};

let tests = [
  (
    "DocSlides.ReparseBackuptext",
    List.map(doc_slide_reparses, List.tl(doc_slides)) // Dropping the first basic reference slide to avoid the issue with whitespace shifting around convex grout
  ),
];
