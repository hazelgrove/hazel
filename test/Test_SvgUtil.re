open Alcotest;
open Util.SvgUtil;

let row = (row, left, right): Rows.row => {
  row,
  left,
  right,
};

let close = (a: float, b: float) => Float.abs(a -. b) < 1e-9;

/* Only the commands the row outline emits; anything else is a failure. */
let equal_cmd = (a: Path.cmd, b: Path.cmd) =>
  switch (a, b) {
  | (M(p), M(q)) => close(p.x, q.x) && close(p.y, q.y)
  | (H({x: a}), H({x: b}))
  | (V({y: a}), V({y: b})) => close(a, b)
  | (Z, Z) => true
  | _ => false
  };

let path =
  testable(
    (fmt, p) =>
      Format.pp_print_string(
        fmt,
        String.concat(" ", List.map(Path.string_of_command, p)),
      ),
    (a, b) =>
      List.length(a) == List.length(b) && List.for_all2(equal_cmd, a, b),
  );

let m = (x, y) =>
  Path.M({
    x,
    y,
  });
let h = x => Path.H({x: x});
let v = y => Path.V({y: y});

let check_outline = (name, ~inset_x, ~inset_y, rows, expected) =>
  check(path, name, expected, Rows.inset_outline(~inset_x, ~inset_y, rows));

let test_step_without_inset =
  test_case(
    "inset_outline: zero inset is the plain stepped outline", `Quick, () =>
    check_outline(
      "[0,10] over [4,8]",
      ~inset_x=0.,
      ~inset_y=0.,
      [row(0, 0, 10), row(1, 4, 8)],
      [
        m(0., 0.),
        h(10.),
        v(1.),
        h(8.),
        v(2.),
        h(4.),
        v(1.),
        h(0.),
        v(0.),
        Z,
      ],
    )
  );

let test_step_edges_move_inward =
  test_case(
    "inset_outline: every edge, steps included, moves inward", `Quick, () =>
    check_outline(
      "[0,10] over [4,8]",
      ~inset_x=0.5,
      ~inset_y=0.25,
      [row(0, 0, 10), row(1, 4, 8)],
      [
        m(0.5, 0.25),
        h(9.5),
        v(0.75),
        h(7.5),
        v(1.75),
        h(4.5),
        v(0.75),
        h(0.5),
        v(0.25),
        Z,
      ],
    )
  );

let test_origin_is_leftmost_row =
  test_case(
    "inset_outline: coordinates are relative to the leftmost row", `Quick, () =>
    check_outline(
      "[4,10] over [0,8]",
      ~inset_x=0.,
      ~inset_y=0.,
      [row(0, 4, 10), row(1, 0, 8)],
      [
        m(4., 0.),
        h(10.),
        v(1.),
        h(8.),
        v(2.),
        h(0.),
        v(1.),
        h(4.),
        v(0.),
        Z,
      ],
    )
  );

let test_narrow_row_collapses =
  test_case(
    "inset_outline: a row narrower than twice the inset collapses to its midline",
    `Quick,
    () =>
    check_outline(
      "[3,4] inset 1",
      ~inset_x=1.,
      ~inset_y=0.,
      [row(0, 3, 4)],
      [m(0.5, 0.), h(0.5), v(1.), h(0.5), v(0.), Z],
    )
  );

let groups = list(list(int));
let rows_of = (gs: list(list(Rows.row))) =>
  List.map(List.map((r: Rows.row) => r.row), gs);

let test_split_disjoint =
  test_case(
    "split: consecutive rows sharing no columns are separate groups",
    `Quick,
    () =>
    check(
      groups,
      "[40,45] over [2,5]",
      [[0], [1]],
      rows_of(Rows.split(~inset_x=0., [row(0, 40, 45), row(1, 2, 5)])),
    )
  );

let test_split_overlapping =
  test_case("split: overlapping consecutive rows are one group", `Quick, () =>
    check(
      groups,
      "[0,10] over [4,8]",
      [[0, 1]],
      rows_of(Rows.split(~inset_x=0., [row(0, 0, 10), row(1, 4, 8)])),
    )
  );

let test_split_gap =
  test_case("split: a missing row splits the group", `Quick, () =>
    check(
      groups,
      "rows 0 and 2",
      [[0], [2]],
      rows_of(Rows.split(~inset_x=0., [row(0, 0, 10), row(2, 0, 10)])),
    )
  );

let tests = (
  "SvgUtil",
  [
    test_step_without_inset,
    test_step_edges_move_inward,
    test_origin_is_leftmost_row,
    test_narrow_row_collapses,
    test_split_disjoint,
    test_split_overlapping,
    test_split_gap,
  ],
);
