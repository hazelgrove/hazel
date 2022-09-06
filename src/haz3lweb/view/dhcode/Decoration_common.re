open Virtual_dom.Vdom;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

type container_type =
  | Svg
  | Div;

/**
 * A buffered container for SVG elements so that strokes along
 * the bounding box of the elements do not get clipped by the
 * viewBox boundaries
 */
let container =
    (
      ~container_type: container_type,
      ~font_metrics: FontMetrics.t,
      ~origin: MeasuredPosition.t,
      ~height: int,
      ~width: int,
      ~cls: string,
      contents: list(Node.t),
    )
    : Node.t => {
  let buffered_height = height + 1;
  let buffered_width = width + 1;

  let buffered_height_px =
    Float.of_int(buffered_height) *. font_metrics.row_height;
  let buffered_width_px =
    Float.of_int(buffered_width) *. font_metrics.col_width;

  let container_origin_x =
    (Float.of_int(origin.row) -. 0.5) *. font_metrics.row_height;
  let container_origin_y =
    (Float.of_int(origin.col) -. 0.5) *. font_metrics.col_width;

  let inner =
    switch (container_type) {
    | Div =>
      Node.div(
        ~attr=
          Attr.many([
            Attr.classes([
              "decoration-container",
              Printf.sprintf("%s-container", cls),
            ]),
            Attr.create(
              "style",
              Printf.sprintf(
                "width: %fpx; height: %fpx;",
                buffered_width_px,
                buffered_height_px,
              ),
            ),
          ]),
        contents,
      )
    | Svg =>
      Node.create_svg(
        "svg",
        ~attr=
          Attr.many([
            Attr.classes([cls]),
            Attr.create(
              "viewBox",
              Printf.sprintf(
                "-0.5 -0.5 %d %d",
                buffered_width,
                buffered_height,
              ),
            ),
            Attr.create("width", Printf.sprintf("%fpx", buffered_width_px)),
            Attr.create(
              "height",
              Printf.sprintf("%fpx", buffered_height_px),
            ),
            Attr.create("preserveAspectRatio", "none"),
          ]),
        contents,
      )
    };
  Node.div(
    ~attr=
      Attr.many([
        Attr.classes([
          "decoration-container",
          Printf.sprintf("%s-container", cls),
        ]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: calc(%fpx - 1px); left: %fpx;",
            container_origin_x,
            container_origin_y,
          ),
        ),
      ]),
    [inner],
  );
};

let corner_radii = (font_metrics: FontMetrics.t) => {
  let r = 2.5;
  (r /. font_metrics.col_width, r /. font_metrics.row_height);
};

let rects =
    (
      ~indent=0,
      ~vtrim=0.0,
      start: MeasuredPosition.t,
      m: MeasuredLayout.t(_),
    )
    : list(SvgUtil.Rect.t) => {
  let mk_rect =
      (
        ~is_first=false,
        ~is_last=false,
        start: MeasuredPosition.t,
        box: MeasuredLayout.box,
      ) =>
    SvgUtil.Rect.{
      min: {
        x: Float.of_int(start.col),
        y: Float.of_int(start.row) +. (is_first ? vtrim : 0.0),
      },
      width: Float.of_int(box.width),
      height:
        Float.of_int(box.height)
        -. (is_first ? vtrim : 0.0)
        -. (is_last ? vtrim : 0.0),
    };
  let n = List.length(m.metrics);
  m.metrics
  |> List.mapi((i, box) => (i, box))
  |> List.fold_left_map(
       (start: MeasuredPosition.t, (i, box: MeasuredLayout.box)) =>
         (
           {row: start.row + box.height, col: indent},
           mk_rect(~is_first=i == 0, ~is_last=i == n - 1, start, box),
         ),
       start,
     )
  |> snd;
};

module ErrHole = {
  let view =
      (
        ~vtrim=0.,
        ~corner_radii: (float, float),
        (offset, subject): MeasuredLayout.with_offset(_),
      )
      : Node.t =>
    subject
    |> rects(~vtrim, {row: 0, col: offset})
    |> SvgUtil.OrthogonalPolygon.mk(~corner_radii)
    |> SvgUtil.Path.view(
         ~attrs=
           Attr.[
             classes(["err-hole"]),
             create("vector-effect", "non-scaling-stroke"),
           ],
       );
};

module VarErrHole = {
  let view =
      (
        ~vtrim=0.,
        ~corner_radii: (float, float),
        (offset, subject): MeasuredLayout.with_offset(_),
      )
      : Node.t =>
    subject
    |> rects(~vtrim, {row: 0, col: offset})
    |> SvgUtil.OrthogonalPolygon.mk(~corner_radii)
    |> SvgUtil.Path.view(
         ~attrs=
           Attr.[
             classes(["var-err-hole"]),
             create("vector-effect", "non-scaling-stroke"),
           ],
       );
};
