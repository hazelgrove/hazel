open Alcotest;
open Util.SvgUtil;

/* A row's box between its grid lines, `inset` moved inward on every side. */
let box = (~inset=0., row, left, right): Rows.box => {
  row,
  top: float_of_int(row) +. inset /. 2.,
  bottom: float_of_int(row + 1) -. inset /. 2.,
  left: float_of_int(left) +. inset,
  right: float_of_int(right) -. inset,
};

let close = (a: float, b: float) => Float.abs(a -. b) < 1e-9;

/* Only absolute commands; a relative one is a failure. */
let equal_cmd = (a: Path.cmd, b: Path.cmd) =>
  switch (a, b) {
  | (M(p), M(q))
  | (L(p), L(q)) => close(p.x, q.x) && close(p.y, q.y)
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

let check_outline = (name, boxes, expected) =>
  check(path, name, expected, Rows.outline(boxes));

let test_step =
  test_case("outline: a narrower row steps in at the row boundary", `Quick, () =>
    check_outline(
      "[0,10] over [4,8]",
      [box(0, 0, 10), box(1, 4, 8)],
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

let test_inset_steps =
  test_case(
    "outline: a narrowing step sits at the upper box's bottom, a widening one at the lower box's top",
    `Quick,
    () =>
    check_outline(
      "[0,10] over [4,8], inset",
      [box(~inset=0.5, 0, 0, 10), box(~inset=0.5, 1, 4, 8)],
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

let test_wider_row =
  test_case("outline: a wider row steps out at its own top", `Quick, () =>
    check_outline(
      "[4,10] over [0,8]",
      [box(0, 4, 10), box(1, 0, 8)],
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

let groups = list(list(int));
let rows_of = (gs: list(list(Rows.box))) =>
  List.map(List.map((b: Rows.box) => b.row), gs);

let test_split_disjoint =
  test_case(
    "split: consecutive rows sharing no columns are separate groups",
    `Quick,
    () =>
    check(
      groups,
      "[40,45] over [2,5]",
      [[0], [1]],
      rows_of(Rows.split([box(0, 40, 45), box(1, 2, 5)])),
    )
  );

let test_split_overlapping =
  test_case("split: overlapping consecutive rows are one group", `Quick, () =>
    check(
      groups,
      "[0,10] over [4,8]",
      [[0, 1]],
      rows_of(Rows.split([box(0, 0, 10), box(1, 4, 8)])),
    )
  );

let test_split_gap =
  test_case("split: a missing row splits the group", `Quick, () =>
    check(
      groups,
      "rows 0 and 2",
      [[0], [2]],
      rows_of(Rows.split([box(0, 0, 10), box(2, 0, 10)])),
    )
  );

let rect =
  testable(
    (fmt, r: Rect.t) =>
      Format.fprintf(fmt, "%f %f %f %f", r.min.x, r.min.y, r.width, r.height),
    (a: Rect.t, b: Rect.t) =>
      close(a.min.x, b.min.x)
      && close(a.min.y, b.min.y)
      && close(a.width, b.width)
      && close(a.height, b.height),
  );

let test_transform_resolves_relative =
  test_case(
    "Path.transform: relative commands come out absolute, through the map",
    `Quick,
    () => {
      let pt = ({x, y}: Point.t): Point.t => {
        x: 2. *. x,
        y: 3. *. y,
      };
      check(
        path,
        "scaled",
        [
          m(2., 3.),
          h(6.),
          v(6.),
          Path.L({
            x: 4.,
            y: 6.,
          }),
          Path.Z,
        ],
        Path.transform(
          ~pt,
          ~scale={
            dx: 2.,
            dy: 3.,
          },
          [
            m(1., 1.),
            Path.H_({dx: 2.}),
            Path.V_({dy: 1.}),
            Path.L_({
              dx: (-1.),
              dy: 0.,
            }),
            Path.Z,
          ],
        ),
      );
    },
  );

let test_transform_keeps_edges_straight =
  test_case(
    "Path.transform: H and V stay straight under a per-cell shift",
    `Quick,
    () => {
      /* columns from 2 on are pushed right by 10 */
      let pt = ({x, y}: Point.t): Point.t => {
        x: x >= 2. ? x +. 10. : x,
        y,
      };
      check(
        path,
        "H maps its end, V keeps the x it reached",
        [m(0., 0.), h(13.), v(1.), h(0.)],
        Path.transform(
          ~pt,
          ~scale={
            dx: 1.,
            dy: 1.,
          },
          [m(0., 0.), h(3.), v(1.), h(0.)],
        ),
      );
    },
  );

let test_bbox =
  test_case(
    "Path.bbox: the extent of the points reached",
    `Quick,
    () => {
      check(
        option(rect),
        "a rectangle",
        Some({
          min: {
            x: 1.,
            y: 1.,
          },
          width: 2.,
          height: 1.,
        }),
        Path.bbox([
          m(1., 1.),
          Path.H_({dx: 2.}),
          Path.V_({dy: 1.}),
          Path.Z,
        ]),
      );
      check(option(rect), "empty", None, Path.bbox([]));
    },
  );

let tests = (
  "SvgUtil",
  [
    test_transform_resolves_relative,
    test_transform_keeps_edges_straight,
    test_bbox,
    test_step,
    test_inset_steps,
    test_wider_row,
    test_split_disjoint,
    test_split_overlapping,
    test_split_gap,
  ],
);
