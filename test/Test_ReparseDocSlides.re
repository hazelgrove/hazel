open Web;
open Alcotest;
open Haz3lcore;
open EditingPrelude;

let doc_slides: list((string, CellEditor.Model.persistent)) =
  snd(Init.startup.documentation);

let doc_slide_reparses =
    ((name, slide): (string, CellEditor.Model.persistent)) => {
  test_case(
    name,
    `Slow,
    () => {
      let reparsed_segment =
        switch (Parser.to_segment(slide.backup_text)) {
        | Some(seg) => seg
        | None =>
          Alcotest.fail("Failed to parse segment from slide backup text")
        };

      let original_segment =
        Sexplib.Sexp.of_string(slide.zipper)
        |> Zipper.t_of_sexp
        |> Zipper.seg_without_buffer;

      check(
        segment,
        "Reparses to equivalent segment",
        original_segment,
        reparsed_segment,
      );
    },
  );
};

let tests = ("ReparseDocSlides", List.map(doc_slide_reparses, doc_slides));
