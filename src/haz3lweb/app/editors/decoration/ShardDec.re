open Util;
open Haz3lcorep;
open Virtual_dom.Vdom;

type tip = option(Nib.Shape.t);

type shard_dims = {
  font_metrics: FontMetrics.t,
  measurement: Measured.measurement,
  tips: (tip, tip),
};

let tip_width = 0.32; /* Width of caret + piece shard tips */
let tip_height = 0.5; /* (Half) height of caret + piece shard tips */
let concave_offset = 0.256; /* Horizontal spacing offset for concave tips */
let convex_offset = 0.192; /* Horizontal spacing offset for convex tips  */
let shadow_dy = 0.04; /* Syncs indication arms and shard shadows. Keep sync with editor.css --off-y */

let tips_of_shapes = ((l, r): (Nib.Shape.t, Nib.Shape.t)): (tip, tip) => (
  Some(l),
  Some(r),
);

let shape_adjust = (d1: Direction.t, d2: Direction.t): float =>
  switch (d1, d2) {
  | (Left, Left) => -. convex_offset
  | (Right, Right) => convex_offset
  | (Left, Right) => concave_offset
  | (Right, Left) => -. concave_offset
  };

let shape_adjust = (side: Direction.t, shape: option(Direction.t)) =>
  switch (shape) {
  | None => 0.
  | Some(d2) => shape_adjust(side, d2)
  };

let caret_run = (shape: option(Direction.t)) =>
  switch (shape) {
  | None => 0.
  | Some(Left) => +. tip_width
  | Some(Right) => -. tip_width
  };

let chevronf = (run: float, rise: float): list(SvgUtil.Path.cmd) =>
  SvgUtil.Path.[
    L_({
      dx: -. run,
      dy: rise,
    }),
    L_({
      dx: +. run,
      dy: rise,
    }),
  ];

let chevron = (direction: option(Direction.t), drawing_from: Direction.t) =>
  chevronf(
    caret_run(direction),
    drawing_from == Left ? -. tip_height : tip_height,
  );

let chonky_path_base =
    ((l, r), x_offset, length: float, height: float): list(SvgUtil.Path.cmd) => {
  List.flatten(
    SvgUtil.Path.[
      [
        M({
          x: -. x_offset,
          y: 0.,
        }),
        H_({dx: length}),
        V({y: height}),
      ],
      chevron(r, Right),
      [H_({dx: -. length}), v(~y=1)],
      chevron(l, Left),
      [Z],
    ],
  );
};

let length_of = (length, d_l, d_r) =>
  float_of_int(length)
  +. shape_adjust(Left, d_l)
  -. shape_adjust(Right, d_r);

let offset_of = d_l => shape_adjust(Left, d_l);

let path = ((d_l, d_r), length: int, height: int): list(SvgUtil.Path.cmd) =>
  chonky_path_base(
    (d_l, d_r),
    offset_of(d_l),
    length_of(length, d_l, d_r),
    float_of_int(height),
  );

let simple =
    (
      {font_metrics, tips: (l, r), measurement}: shard_dims,
      ~absolute=true,
      ~attr=[],
      classes,
    )
    : Node.t =>
  DecUtil.code_svg_sized(
    ~font_metrics,
    ~measurement,
    ~base_cls=["shard"] @ classes,
    ~path_cls=[],
    ~absolute,
    ~attr,
    path(
      (
        Option.map(Nib.Shape.direction_of(Left), l),
        Option.map(Nib.Shape.direction_of(Right), r),
      ),
      measurement.last.col - measurement.origin.col,
      measurement.last.row - measurement.origin.row,
    ),
  );

let relative = (shard_dims: shard_dims) =>
  simple(~absolute=false, shard_dims, []);
