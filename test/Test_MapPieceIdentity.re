/* ZipperBase.MapPiece keeps the pieces it does not change as the same
   objects. A projector update away from the caret maps over the whole
   zipper (ProjectorPerform.update, through fast_local_seg's general case);
   rebuilding every tile it passed made the whole program new objects, both
   a full copy and a new program to every layer downstream that keys on
   piece identity (Segment.ptr_eq). */
open Alcotest;
open Haz3lcore;

let segment_of = (text: string): Segment.t =>
  switch (PersistentZipper.parse_text(~source="t", ~root=Exp, text)) {
  | Some(z) => Zipper.unselect_and_zip(z)
  | None => fail("did not parse")
  };

/* The last tile, depth-first: deep inside the last item. */
let rec last_tile = (seg: Segment.t): option(Tile.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        switch (
          List.fold_left(
            (a, c) =>
              switch (last_tile(c)) {
              | Some(t) => Some(t)
              | None => a
              },
            None,
            t.children,
          )
        ) {
        | Some(inner) => Some(inner)
        | None => Some(t)
        }
      | _ => acc
      },
    None,
    seg,
  );

/* Replace the piece with this id by a copy: a new object, same content. */
let replace_by_copy = (id: Id.t, p: Piece.t): Segment.t =>
  switch (p) {
  | Tile(t) when t.id == id => [
      Tile({
        ...t,
        id: t.id,
      }),
    ]
  | p => [p]
  };

let program = "let a = 1 in\nlet b = (2 + 3) * a in\nlet c = [a, b] in\nc";

let bench_enabled = Sys.getenv_opt("HAZEL_BENCH") == Some("1");

let tests = (
  "MapPieceIdentity",
  [
    test_case(
      "an update that changes nothing returns the segment",
      `Quick,
      () => {
        let seg = segment_of(program);
        check(
          bool,
          "same object",
          true,
          ZipperBase.MapPiece.of_segment(p => [p], seg) === seg,
        );
      },
    ),
    test_case(
      "one tile changed: the pieces before it are kept",
      `Quick,
      () => {
        let seg = segment_of(program);
        let target =
          switch (last_tile(seg)) {
          | Some(t) => t.id
          | None => fail("no tile")
          };
        let out =
          ZipperBase.MapPiece.of_segment(replace_by_copy(target), seg);
        check(bool, "the segment changed", false, out === seg);
        check(
          bool,
          "the first item's pieces are the same objects",
          true,
          List.hd(out) === List.hd(seg),
        );
        let kept =
          List.length(
            List.filter(((a, b)) => a === b, List.combine(out, seg)),
          );
        check(
          bool,
          "most top-level pieces are kept",
          true,
          kept >= List.length(seg) - 2,
        );
      },
    ),
  ]
  @ (
    bench_enabled
      ? [
        test_case(
          "BENCH: one tile changed in 2,000 lets",
          `Slow,
          () => {
            let text =
              String.concat(
                "",
                List.init(2000, i =>
                  Printf.sprintf("let x%d = %d in\n", i, i)
                ),
              )
              ++ "0";
            let seg = segment_of(text);
            let target =
              switch (last_tile(seg)) {
              | Some(t) => t.id
              | None => fail("no tile")
              };
            let t0 = Unix.gettimeofday();
            for (_ in 1 to 50) {
              ignore(
                ZipperBase.MapPiece.of_segment(replace_by_copy(target), seg),
              );
            };
            Printf.printf(
              "BENCH MapPiece one change in 2000 lets: %.2f ms per update\n",
              (Unix.gettimeofday() -. t0) *. 1000. /. 50.,
            );
          },
        ),
      ]
      : []
  ),
);
