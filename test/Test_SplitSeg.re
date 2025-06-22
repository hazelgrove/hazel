open Alcotest;
open Haz3lcore;
open Language;
open Web;
open Example;

// Create simple testable types
let segment_typ =
  testable(
    Fmt.using(Segment.show, Fmt.string),
    Segment.equal((_, _) => true),
  );

let id_map_segment_typ =
  testable(
    Fmt.using(
      s => s |> Id.Map.sexp_of_t(Segment.sexp_of_t) |> Sexplib.Sexp.to_string,
      Fmt.string,
    ),
    Id.Map.equal(Segment.equal((_, _) => true)),
  );

let tests = (
  "TermRanges",
  [
    test_case(
      "split with empty id list",
      `Quick,
      () => {
        let p = int("666");
        let seg = [p];
        let result = TermRanges.split([], seg);
        check(id_map_segment_typ, "empty id list", Id.Map.empty, result);
      },
    ),
    test_case(
      "split with non-existent ids",
      `Quick,
      () => {
        let p = int("666");
        let seg = [p];
        let fake_id = Id.mk();
        let result = TermRanges.split([fake_id], seg);
        check(id_map_segment_typ, "non-existent id", Id.Map.empty, result);
      },
    ),
    test_case(
      "split single tile segment",
      `Quick,
      () => {
        let p = int("666");
        let single_tile = [p];
        let tile_id = Piece.id(p);
        let result = TermRanges.split([tile_id], single_tile);
        let expected = Id.Map.singleton(tile_id, single_tile);
        check(id_map_segment_typ, "single tile", expected, result);
      },
    ),
    test_case(
      "split with mixed existing and non-existing ids",
      `Quick,
      () => {
        let p1 = int("1");
        let p2 = int("2");
        let seg = [p1, plus(), p2];
        let id1 = Piece.id(p1);
        let fake_id = Id.mk();
        let result = TermRanges.split([id1, fake_id], seg);
        let expected = Id.Map.singleton(id1, [p1]);
        check(id_map_segment_typ, "mixed ids", expected, result);
      },
    ),
    test_case(
      "split segment with multiple tiles",
      `Quick,
      () => {
        let p1 = int("1");
        let p2 = int("2");
        let seg = [p1, plus(), p2];
        let id1 = Piece.id(p1);
        let id2 = Piece.id(p2);
        let result = TermRanges.split([id1, id2], seg);
        let expected =
          Id.Map.empty |> Id.Map.add(id1, [p1]) |> Id.Map.add(id2, [p2]);
        check(id_map_segment_typ, "multiple tiles", expected, result);
      },
    ),
    test_case(
      "split with nested bidelimited context",
      `Quick,
      () => {
        let p1 = int("1");
        let p2 = int("2");
        let parens = mk_parens_exp([[p1, plus(), p2]]);
        let nested = [parens];
        let id1 = Piece.id(p1);
        let id2 = Piece.id(p2);
        let parens_id = Piece.id(parens);
        let result = TermRanges.split([id1, id2, parens_id], nested);
        let expected =
          Id.Map.empty
          |> Id.Map.add(id1, [p1])
          |> Id.Map.add(id2, [p2])
          |> Id.Map.add(parens_id, nested);
        check(id_map_segment_typ, "nested bidelimited", expected, result);
      },
    ),
    test_case(
      "split expression 1 + 2*3 by term ids",
      `Quick,
      () => {
        let p1 = int("1");
        let p2 = int("2");
        let p3 = int("3");
        let times = times();
        let plus_op = plus();

        let seg = [p1, plus_op, p2, times, p3];

        let id1 = Piece.id(p1);
        let id2 = Piece.id(p2);
        let id2_times_3 = Piece.id(times);

        let result = TermRanges.split([id1, id2, id2_times_3], seg);

        // Expected: id1 -> [1], id2 -> [2], id2_times_3 -> [2, *, 3]
        let expected =
          Id.Map.empty
          |> Id.Map.add(id1, [p1])
          |> Id.Map.add(id2, [p2])
          |> Id.Map.add(id2_times_3, [p2, times, p3]);

        check(id_map_segment_typ, "expression 1 + 2*3", expected, result);
      },
    ),
  ],
);
