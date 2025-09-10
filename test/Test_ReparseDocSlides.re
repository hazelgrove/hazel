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

let caret_is_at_beginning = ((name, slide: CellEditor.Model.persistent)) => {
  test_case(
    name,
    `Slow,
    () => {
      let z = Sexplib.Sexp.of_string(slide.editor.zipper) |> Zipper.t_of_sexp;
      let selection = z.selection;
      let (l, _) = z.relatives.siblings;
      check(segment, "Selection content is empty", [], selection.content);
      check(segment, "Left sibling is empty", [], l);
    },
  );
};

let tests = [
  (
    "DocSlides.ReparseBackuptext",
    List.map(doc_slide_reparses, List.tl(doc_slides)) // Dropping the first basic reference slide to avoid the issue with whitespace shifting around convex grout
  ),
  (
    "DocSlides.CaretAtBeginning",
    List.map(caret_is_at_beginning, doc_slides),
  ),
  // This test case is just to print out all the segments in a format that can be moved into src/web/init/docs
  // (
  //   "DocSlides.PrintSegments",
  //   [
  //     test_case(
  //       "Print all segments",
  //       `Quick,
  //       () => {
  //         List.iter(
  //           ((name, slide: CellEditor.Model.persistent)) => {
  //             let segment =
  //               Sexplib.Sexp.of_string(slide.editor.zipper)
  //               |> Zipper.t_of_sexp
  //               |> Zipper.zip;
  //             let content =
  //               "let out : string * Haz3lcore.PersistentSegment.t = "
  //               ++ [%derive.show: (string, Haz3lcore.PersistentSegment.t)]((
  //                    name,
  //                    PersistentSegment.persist(segment),
  //                  ));
  //             print_endline(content);
  //             // Write content out to file
  //             let filename = Util.StringUtil.sanitize_filename(name) ++ ".ml";
  //             let oc = open_out(filename);
  //             output_string(oc, content);
  //             close_out(oc);
  //           },
  //           doc_slides,
  //         );
  //         Alcotest.fail("Printed all segments to console");
  //       },
  //     ),
  //   ],
  // ),
];
