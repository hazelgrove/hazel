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
  QuiverDec.bubbles(
    ~measured=syntax.measured,
    ~font_metrics,
    ~caret_pos=Some((caret.row, caret.col)),
    ~owned=CompletionQuery.chips_at_caret(~seg=engine_seg, z),
    engine_seg,
  )
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

let flagpole_edges =
  test_case("flagpole joins every caret profile", `Quick, () => {
    List.iter(
      font_metrics =>
        List.iter(
          side =>
            List.iter(
              shape =>
                List.iter(
                  row =>
                    List.iter(
                      col => {
                        let geometry =
                          QuiverDec.flagpole_geometry(
                            ~font_metrics,
                            ~row,
                            ~col,
                            ~caret_form=Some((side, shape)),
                          );
                        /* Read the rendered caret path's first horizontal edge, independently
                           of the flagpole positioning code. Include fractional font sizes. */
                        switch (CaretDec.caret_base_path(side, shape)) {
                        | [Util.SvgUtil.Path.M({x, y}), H_({dx}), ..._] =>
                          let origin_x =
                            float_of_int(col)
                            *. font_metrics.FontMetrics.col_width;
                          let origin_y =
                            float_of_int(row) *. font_metrics.row_height;
                          check(
                            float(1e-8),
                            "left edge",
                            origin_x +. x *. font_metrics.col_width,
                            geometry.left,
                          );
                          check(
                            float(1e-8),
                            "caret width",
                            dx *. font_metrics.col_width,
                            geometry.width,
                          );
                          check(
                            float(1e-8),
                            "meets the top without overlap",
                            origin_y +. y *. font_metrics.row_height,
                            geometry.top +. geometry.height,
                          );
                        | _ =>
                          fail("Caret must start with a horizontal top edge")
                        };
                      },
                      [0, 7],
                    ),
                  [0, 1, 4, 9],
                ),
              [None, Some(Util.Direction.Left), Some(Right)],
            ),
          [Util.Direction.Left, Right],
        ),
      [
        font_metrics,
        FontMetrics.{
          row_height: 25.135,
          col_width: 10.405,
        },
      ],
    )
  });

let tests = [
  ("QuiverDisplay: geometry", [flagpole_edges]),
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
        /* the ) lands first in the completed program, but the caret
           is pinned to the in-witness: Tab types `n`, so the bubble
           sits at the caret and leads with in; the ) merged in by
           pixel overlap trails (Test_TabDisplayParity) */
        ~name="witness for outer in + inner closer: caret's chip leads",
        ~input="let x = (1 i",
        ~expected="(0,12)[in~1 )]",
      ),
    ],
  ),
];
