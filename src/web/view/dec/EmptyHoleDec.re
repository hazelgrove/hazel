open Virtual_dom.Vdom;
open Core;

module Profile = {
  type t = {
    measurement: Measured.measurement,
    mold: Mold.t,
  };
};

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
    (~font_metrics, {measurement: {origin, _}, mold}: Profile.t): Node.t => {
  let sort = mold.out;
  let c_cls = Sort.to_string(sort);
  let (tip_l, tip_r): (Core.Nib.Shape.t, Core.Nib.Shape.t) =
    Util.TupleUtil.map2(Core.Nib.shape, mold.nibs);
  let (tip_l, tip_r): (Core.Nib.t, Core.Nib.t) = (
    {sort, shape: tip_l},
    {sort, shape: tip_r},
  );
  DecUtil.code_svg(
    ~font_metrics,
    ~origin,
    ~base_cls=["empty-hole"],
    ~path_cls=["empty-hole-path", c_cls],
    path(tip_l, tip_r, 0., 0.28),
  );
};
