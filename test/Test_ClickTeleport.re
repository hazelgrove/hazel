open Alcotest;
open Haz3lcore;
open Language;

/* Landing-parity gate for the click teleport (Move.to_point): for a
   grid of goals from both extremes of a corpus program, the teleport
   path must land the caret exactly where the pure walk does, without
   changing the program.
     bash test/run_node.sh test 'ClickTeleport' */

let corpus_seg = CorpusUtil.corpus_seg(~root=Exp);

let ids_of = (z: Zipper.t): list(Id.t) =>
  Zipper.unselect_and_zip(z) |> List.map(Piece.id);

let case = (file: string, ()) =>
  switch (corpus_seg(file)) {
  | None => fail("corpus unreadable: " ++ file)
  | Some(seg) =>
    let measured =
      Measured.of_segment(seg, ProjectorCore.Shape.Map.empty, Id.Map.empty);
    let starts = [
      ("from-start", Zipper.unzip(~direction=Left, seg)),
      ("from-end", Zipper.unzip(~direction=Right, seg)),
    ];
    let goals = [
      (3, 4),
      (57, 12),
      (120, 0),
      (300, 6),
      (452, 30),
      (700, 2),
      (871, 15),
      (950, 20),
    ];
    List.iter(
      ((label, z0)) =>
        List.iter(
          ((row, col)) => {
            let goal =
              Measured.Point.{
                row,
                col,
              };
            let name =
              Printf.sprintf("%s %s->(%d,%d)", file, label, row, col);
            switch (
              Move.to_point(~measured, ~goal, z0),
              Move.to_point_walk(~measured, ~goal, z0),
            ) {
            | (Some(zt), Some(zw)) =>
              let pt = Zipper.Caret.point(measured, zt);
              let pw = Zipper.Caret.point(measured, zw);
              check(int, name ++ ":row", pw.row, pt.row);
              check(int, name ++ ":col", pw.col, pt.col);
              check(
                bool,
                name ++ ":program unchanged",
                true,
                ids_of(zt) == ids_of(zw)
                && ids_of(zt) == List.map(Piece.id, seg),
              );
            | (None, None) => ()
            | _ => fail(name ++ ": one path failed, other succeeded")
            };
          },
          goals,
        ),
      starts,
    );
  };

let tests = (
  "ClickTeleport",
  [test_case("mega-1k landing parity", `Quick, case("mega-1k.hz"))],
);
