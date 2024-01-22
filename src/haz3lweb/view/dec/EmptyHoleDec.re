open Virtual_dom.Vdom;
open Haz3lcore;

module Profile = {
  type t = {
    measurement: Measured.measurement,
    mold: Mold.t,
  };
};

type hole_svg_style =
  | StandardHole
  | ErrorHole
  | PromptHole;

let path = (tip_l, tip_r, offset, s: float) => {
  let x_dilate = 1.5;
  List.concat(
    SvgUtil.Path.[
      [
        M({x: offset +. 0.5, y: 0.5 -. s /. 2.}),
        H_({dx: x_dilate *. s /. 2.}),
      ],
      Diag.right_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip_r, 0)),
      [H_({dx: -. s *. x_dilate})],
      Diag.left_tip_path(~scale_x=s *. x_dilate, ~scale_y=s, (tip_l, 0)),
      [Z],
    ],
  );
};

let view =
    (~font_metrics, hole_svg_style, {measurement, mold}: Profile.t): Node.t => {
  let sort = mold.out;
  let c_cls = Sort.to_string(sort);
  let (tip_l, tip_r): (Haz3lcore.Nib.Shape.t, Haz3lcore.Nib.Shape.t) =
    Util.TupleUtil.map2(Haz3lcore.Nib.shape, mold.nibs);
  let (tip_l, tip_r): (Haz3lcore.Nib.t, Haz3lcore.Nib.t) = (
    {sort, shape: tip_l},
    {sort, shape: tip_r},
  );

  switch (hole_svg_style) {
  | StandardHole =>
    DecUtil.code_svg_sized(
      ~font_metrics,
      ~measurement,
      ~base_cls=["empty-hole"],
      ~path_cls=["empty-hole-path", c_cls],
      path(tip_l, tip_r, 0., 0.28),
    )
  | ErrorHole =>
    DecUtil.code_svg_sized(
      ~font_metrics,
      ~measurement,
      ~base_cls=["empty-hole"],
      ~path_cls=["unsolved-empty-hole-path", c_cls],
      path(tip_l, tip_r, 0., 0.42),
    )
  | PromptHole =>
    DecUtil.code_svg_sized(
      ~font_metrics,
      ~measurement,
      ~base_cls=["empty-hole"],
      ~path_cls=["solved-empty-hole-with-ci-path", c_cls],
      path(tip_l, tip_r, 0., 0.42),
    )
  };
};

let relative_view =
    (~font_metrics, hole_svg_style: hole_svg_style, mold: Mold.t): Node.t => {
  let sort = mold.out;
  let c_cls = Sort.to_string(sort);
  let (tip_l, tip_r): (Haz3lcore.Nib.Shape.t, Haz3lcore.Nib.Shape.t) =
    Util.TupleUtil.map2(Haz3lcore.Nib.shape, mold.nibs);
  let (tip_l, tip_r): (Haz3lcore.Nib.t, Haz3lcore.Nib.t) = (
    {sort, shape: tip_l},
    {sort, shape: tip_r},
  );

  switch (hole_svg_style) {
  | StandardHole =>
    DecUtil.code_svg_sized_relative(
      ~font_metrics,
      ~base_cls=["empty-hole"],
      ~path_cls=["empty-hole-path", c_cls],
      path(tip_l, tip_r, 0., 0.28),
    )
  | ErrorHole =>
    DecUtil.code_svg_sized_relative(
      ~font_metrics,
      ~base_cls=["empty-hole"],
      ~path_cls=["unsolved-empty-hole-path", c_cls],
      path(tip_l, tip_r, 0., 0.42),
    )
  | PromptHole =>
    DecUtil.code_svg_sized_relative(
      ~font_metrics,
      ~base_cls=["empty-hole"],
      ~path_cls=["solved-empty-hole-with-ci-path", c_cls],
      path(tip_l, tip_r, 0., 0.42),
    )
  };
};
