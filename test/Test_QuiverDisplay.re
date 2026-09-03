open Alcotest;
open Haz3lcore;
open Web;

/* Quiver chip DISPLAY pins: the rendered bubble list (positions,
   coalescing, delimiter order) — not the engine's insertion list,
   which Test_CanonicalCompletion covers. Bubble text must follow
   ENGINE order (landing-site order in the completed program, the
   order tab applies), never pixel order: typing the `=` of a pending
   `=>` makes the end chip's pin rest BEHIND the typed prefix, and
   pixel-ordered merging read "end =>" (andrew, 2026-09-01). */

let font_metrics: FontMetrics.t = {
  row_height: 20.0,
  col_width: 10.0,
};

let chips_of = (input: string): list((int, int, string)) => {
  let z =
    Test_Editing.perform(Zipper.init(), Test_Editing.mk(input ++ "¦"));
  let syntax = CachedSyntax.init(z);
  let engine_seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let caret = Zipper.Caret.point(syntax.measured, z);
  let result = CanonicalCompletion.for_editor(engine_seg);
  let positioned =
    result.insertions
    |> List.mapi((idx, ins) =>
         QuiverDec.resolve_position(
           ~idx,
           ~seg=engine_seg,
           ~caret_pos=Some((caret.row, caret.col)),
           syntax.measured,
           ins,
         )
       )
    |> List.filter_map(x => x);
  let sorted =
    List.sort(
      (a: QuiverDec.positioned_insertion, b: QuiverDec.positioned_insertion) => {
        let row_cmp = Int.compare(a.row, b.row);
        row_cmp != 0 ? row_cmp : Int.compare(a.col, b.col);
      },
      positioned,
    );
  QuiverDec.coalesce_overlaps(~font_metrics, sorted)
  |> List.map((c: QuiverDec.positioned_insertion) =>
       (
         c.row,
         c.col,
         c.delimiters
         |> List.map((d: CanonicalCompletion.delimiter_info) =>
              d.text
              ++ (
                switch (d.typed_len) {
                | Some(n) => Printf.sprintf("~%d", n)
                | None => ""
                }
              )
            )
         |> String.concat(" "),
       )
     );
};

let show = chips =>
  chips
  |> List.map(((r, c, s)) => Printf.sprintf("(%d,%d)[%s]", r, c, s))
  |> String.concat(" ");

let chip_case = (~name, ~input, ~expected) =>
  test_case(name, `Quick, () =>
    check(string, name, expected, show(chips_of(input)))
  );

let tests = [
  (
    "QuiverDisplay: bubble order",
    [
      chip_case(
        ~name="pending rule arrow + end coalesce in engine order",
        ~input="case true\n| false ",
        ~expected="(1,8)[=> end]",
      ),
      chip_case(
        /* the typed prefix's chip stays at the FRONT of the bubble,
           pinned at the prefix; the end chip's wandered pin (it
           rests behind the typed `=`) must not lead the merge */
        ~name="typed = of => keeps the arrow at the bubble front",
        ~input="case true\n| false =",
        ~expected="(1,9)[=>~1 end]",
      ),
      chip_case(
        ~name="let-wrapped: whole tail keeps engine order",
        ~input="let f = case true\n| false =",
        ~expected="(1,9)[=>~1 end in]",
      ),
      chip_case(
        ~name="witness for outer in + inner closer (agreement case)",
        ~input="let x = (1 i",
        ~expected="(0,10)[) in~1]",
      ),
    ],
  ),
];
