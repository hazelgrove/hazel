open Alcotest;
open Haz3lcore;

let infer = (marked, replacement) => {
  let z = Test_Editing.parse_zipper(marked);
  let grab = Indicated.index(z) |> Option.get;
  let old_seg = Zipper.unselect_and_zip(z);
  let new_seg = Parser.to_segment(replacement, ~root=Exp) |> Option.get;
  let c = SyntaxCorrespondence.infer(~grab, ~old_seg, ~new_seg, ());
  check(bool, "shape unchanged", true, IdMatch.same_seg(new_seg, c.segment));
  check(
    int,
    "unique ids",
    0,
    List.length(IdMatch.duplicate_ids(c.segment)),
  );
  let allowed =
    Id.Set.of_list(IdMatch.ids_seg(old_seg) @ IdMatch.ids_seg(new_seg));
  check(
    bool,
    "ids have source or result provenance",
    true,
    List.for_all(id => Id.Set.mem(id, allowed), IdMatch.ids_seg(c.segment)),
  );
  (grab, old_seg, c);
};

let tests = [
  (
    "Syntax Correspondence",
    [
      test_case(
        "cross-level movement retains a whole subtree",
        `Quick,
        () => {
          let (grab, old_seg, c) = infer("1 ¦+ 2", "(1 + 2) * 3");
          let old_tiles =
            SyntaxCorrespondence.nodes(old_seg)
            |> List.map((n: SyntaxCorrespondence.node) => Piece.id(n.piece));
          check(
            bool,
            "every old tile survives wrapping",
            true,
            List.for_all(
              id => List.mem(id, IdMatch.ids_seg(c.segment)),
              old_tiles,
            ),
          );
          check(
            bool,
            "moved root supplies a result target",
            true,
            c.destination == Some(Result(grab)),
          );
        },
      ),
      test_case(
        "a consumed map converges on the live surviving map",
        `Quick,
        () => {
          let (grab, old_seg, c) =
            infer("map(¦map(xs, f), g)", "map(xs, fun x -> g(f(x)))");
          let outer =
            SyntaxCorrespondence.nodes(old_seg)
            |> List.hd
            |> ((n: SyntaxCorrespondence.node) => Piece.id(n.piece));
          check(
            bool,
            "live outer map is the destination",
            true,
            c.destination == Some(Live(outer)),
          );
          check(
            bool,
            "held token survives",
            true,
            List.mem(grab, IdMatch.ids_seg(c.segment)),
          );
          check(
            bool,
            "other occurrence converges without duplicated ids",
            true,
            List.mem((outer, grab), c.merges),
          );
        },
      ),
      test_case(
        "duplication records fresh copies",
        `Quick,
        () => {
          let (grab, _, c) = infer("¦x", "(x, x)");
          check(int, "one clone", 1, List.length(c.copies));
          let (copy, source) = List.hd(c.copies);
          check(
            bool,
            "copy emerges from original",
            true,
            source == grab && copy != grab,
          );
        },
      ),
      test_case(
        "an unchanged root is parked",
        `Quick,
        () => {
          let (_, _, c) = infer("¦map(xs, f)", "map(xs, g)");
          check(
            bool,
            "no motion caused merely by descendant editing",
            true,
            c.destination == None,
          );
        },
      ),
    ],
  ),
];
