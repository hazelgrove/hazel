open Alcotest;
open Haz3lcore;
open Language;

/* Headless VISUAL-placement tests: the engine's anchors are covered
   elsewhere; this drives QuiverLayout.resolve_position with
   live-faithful inputs — the segment CachedSyntax actually uses
   (unselect WITHOUT erasing the suggestion buffer, so TyDi ghosts
   are part of the measured layout), real Measured, and the real
   caret point. */

let string_testable = testable(Fmt.string, String.equal);

let statics_of = (z: Zipper.t) => {
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  fst(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));
};

/* the live per-edit-cycle buffer set (Editor.calculate path) */
let with_tydi_buffer = (z: Zipper.t): Zipper.t => {
  let info_map = statics_of(z);
  let ci = Indicated.ci_of(z, info_map);
  switch (TyDi.set_buffer(~ci, z)) {
  | Some(z) => z
  | None => z
  };
};

let pins = (~tydi=true, code: string): string => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
  let z = tydi ? with_tydi_buffer(z) : z;
  /* live-faithful: DISPLAY segment keeps the suggestion ghost
     (CachedSyntax does not erase); the ENGINE segment is the user's
     real program (the QuiverDec fix) — resolving engine insertions
     against the display measured is exactly what the view does */
  let display_seg = Zipper.unselect_and_zip(z);
  let engine_seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let measured = Measured.of_segment(display_seg, Id.Map.empty, Id.Map.empty);
  let caret = Zipper.Caret.point(measured, z);
  let seg = engine_seg;
  CanonicalCompletion.for_editor(seg).insertions
  |> List.filter_map(
       QuiverLayout.resolve_position(
         ~seg,
         ~caret_pos=Some((caret.Util.Point.row, caret.Util.Point.col)),
         measured,
       ),
     )
  |> List.map((pi: QuiverLayout.positioned_insertion) =>
       Printf.sprintf(
         "%s@%d:%d",
         pi.delimiters
         |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
         |> String.concat("+"),
         pi.row,
         pi.col,
       )
     )
  |> String.concat(" | ");
};

let pin_case = (~name, ~code, ~tydi=true, ~expected, ()) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, pins(~tydi, code))
  );
let _ = pin_case;

let probe2 = [
  test_case(
    "PROBE andrew states",
    `Quick,
    () => {
      let full = code => {
        let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
        let eseg = Zipper.unselect_and_zip(~erase_buffer=true, z);
        let mat =
          CanonicalCompletion.materialize_all(~sort=Sort.Exp, eseg)
          |> Printer.of_segment(~holes="?", ~concave_holes="~");
        pins(code) ++ "  MAT<" ++ mat ++ ">";
      };
      check(
        string_testable,
        "states",
        "A: )+=+in@1:10  MAT<let a = 2 in\nlet _: (  ?)=?in?>\n"
        ++ "B: =+in@1:19  MAT<let a = 2 in\nlet _: (Int, Bool)=?in? >",
        "A: "
        ++ full("let a = 2 in\nlet _: (  ¦")
        ++ "\nB: "
        ++ full("let a = 2 in\nlet _: (Int, Bool) ¦"),
      );
    },
  ),
];

let tests = [
  ("QuiverLayout: probe2", probe2),
  (
    "QuiverLayout: placement",
    [
      /* the ghost-buffer pollution bug: with the engine fed the
         display segment, suggestion-active states (Bo -> Bool ghost)
         split the in off to 1:0 — left of the let. Engine on the
         erased segment keeps one coalesced chip tracking the text. */
      test_case(
        "typing trajectory: stable line-end placement",
        `Quick,
        () => {
          let states = [
            "(In",
            "(Int",
            "(Int,",
            "(Int, ",
            "(Int, B",
            "(Int, Bo",
            "(Int, Boo",
            "(Int, Bool",
          ];
          check(
            string_testable,
            "trajectory",
            "(In -> [)+=+in@1:13]\n"
            ++ "(Int -> [)+=+in@1:14]\n"
            ++ "(Int, -> [)+=+in@1:15]\n"
            ++ "(Int,  -> [)+=+in@1:16]\n"
            ++ "(Int, B -> [)+=+in@1:17]\n"
            ++ "(Int, Bo -> [)+=+in@1:18]\n"
            ++ "(Int, Boo -> [)+=+in@1:19]\n"
            ++ "(Int, Bool -> [)+=+in@1:20]",
            states
            |> List.map(suf =>
                 suf
                 ++ " -> ["
                 ++ pins("let a = 2 in\nlet (_ ): " ++ suf ++ "¦")
                 ++ "]"
               )
            |> String.concat("\n"),
          );
        },
      ),
    ],
  ),
];
