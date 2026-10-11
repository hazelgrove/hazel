/* Segment.incomplete_tiles_deep remembers each tile's answer by the
   tile's identity. It must answer as the plain walk did, in the same
   order, including on a segment that shares tiles with one asked before. */
open Alcotest;
open Haz3lcore;

/* the walk before the memo */
let rec reference = (acc, seg: Segment.t) =>
  List.fold_left(
    (acc, p) =>
      switch (p) {
      | Piece.Tile(t) =>
        let acc = !Tile.is_complete(t) ? [t, ...acc] : acc;
        List.fold_left(reference, acc, t.children);
      | _ => acc
      },
    acc,
    seg,
  );
let reference = seg => List.rev(reference([], seg));

let parse = (s: string): Segment.t =>
  switch (Parser.to_segment(~root=Exp, s)) {
  | Some(seg) => seg
  | None => fail("did not parse: " ++ s)
  };

let ids = (ts: list(Tile.t)) =>
  List.map((t: Tile.t) => Id.to_string(t.id), ts);

let programs = [
  "let x = 1 in x",
  "let x = ",
  "if 1 then 2",
  "(1 + [2, 3",
  "fun x ->",
  "let a = (if 1 then [2, (3 in a",
  "case 1 | 2 => 3",
  "let f = fun x -> if x then (1, [2 in f(",
];

let tests = (
  "Segment.incomplete_tiles_deep",
  [
    test_case(
      "the same tiles, in the same order, as the plain walk",
      `Quick,
      () => {
        List.iter(
          src => {
            let seg = parse(src);
            check(
              list(string),
              src,
              ids(reference(seg)),
              ids(Segment.incomplete_tiles_deep(seg)),
            );
            check(
              list(string),
              src ++ " (again, remembered)",
              ids(reference(seg)),
              ids(Segment.incomplete_tiles_deep(seg)),
            );
          },
          programs,
        );
        check(
          bool,
          "the programs have incomplete tiles",
          true,
          List.length(
            List.concat_map(src => reference(parse(src)), programs),
          )
          >= 6,
        );
      },
    ),
    test_case(
      "a segment sharing tiles with one asked before",
      `Quick,
      () => {
        let a = parse("let x = (1 + [2");
        let b = parse("if 1 then");
        ignore(Segment.incomplete_tiles_deep(a));
        let both = a @ b;
        check(
          list(string),
          "shared tiles",
          ids(reference(both)),
          ids(Segment.incomplete_tiles_deep(both)),
        );
        check(
          bool,
          "it found some",
          true,
          List.length(Segment.incomplete_tiles_deep(both)) >= 2,
        );
      },
    ),
  ],
);
