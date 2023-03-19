open Virtual_dom.Vdom;
open Haz3lcore;

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
    (
      ~global_inference_info: InferenceResult.global_inference_info,
      ~font_metrics,
      id,
      {measurement, mold}: Profile.t,
    )
    : Node.t => {
  let sort = mold.out;
  let c_cls = Sort.to_string(sort);
  let (tip_l, tip_r): (Haz3lcore.Nib.Shape.t, Haz3lcore.Nib.Shape.t) =
    Util.TupleUtil.map2(Haz3lcore.Nib.shape, mold.nibs);
  let (tip_l, tip_r): (Haz3lcore.Nib.t, Haz3lcore.Nib.t) = (
    {sort, shape: tip_l},
    {sort, shape: tip_r},
  );
  let (svg_enabled, unsolved_path_class) =
    InferenceResult.svg_display_settings(~global_inference_info, id);
  let svg_path_class =
    unsolved_path_class ? "unsolved-empty-hole-path" : "empty-hole-path";
  svg_enabled
    ? unsolved_path_class
        ? DecUtil.code_svg_sized(
            ~font_metrics,
            ~measurement,
            ~base_cls=["empty-hole"],
            ~path_cls=[svg_path_class, c_cls],
            path(tip_l, tip_r, 0., 0.58),
          )
        : DecUtil.code_svg_sized(
            ~font_metrics,
            ~measurement,
            ~base_cls=["empty-hole"],
            ~path_cls=[svg_path_class, c_cls],
            path(tip_l, tip_r, 0., 0.28),
          )
    : Node.none;
};
