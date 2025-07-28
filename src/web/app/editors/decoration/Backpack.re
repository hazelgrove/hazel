open Virtual_dom.Vdom;
open Node;
open Util;

let shard =
    (x_off: float, y_off: float, scale: float, opacity: float, s: string) =>
  div(
    ~attrs=[
      Attr.classes(["code-text", "code", "backpack-selection"]),
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; transform-origin: bottom left; transform: translate(%fpx, %fpx) scale(%f); opacity: %f%%;",
          x_off,
          y_off,
          scale,
          opacity,
        ),
      ),
    ],
    [text(s)],
  );

let genie = (~font_metrics, ~left, ~genie_top, ~genie_height, ~genie_width) =>
  div(
    ~attrs=[
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; left: %fpx; top: %fpx;",
          left,
          genie_top,
        ),
      ),
    ],
    [
      DecUtil.code_svg(
        ~font_metrics,
        ~origin=Point.zero,
        ~base_cls=["restructuring-genie"],
        ~path_cls=["backpack-genie"],
        SvgUtil.Path.[
          M({
            x: 0.,
            y: 0.,
          }),
          V({y: -. genie_height}),
          H_({dx: genie_width}),
          V_({dy: 0.0}),
          Z,
        ],
      ),
    ],
  );

let pole = (~left, ~pole_top, ~pole_height) =>
  div(
    ~attrs=[
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; left: %fpx; top: %fpx; height: %fpx;",
          left,
          pole_top,
          pole_height,
        ),
      ),
      Attr.classes(["backpack-joiner"]),
    ],
    [],
  );

let flag =
    (~font_metrics: Haz3lcorep.FontMetrics.t, ~contents, ~left, ~flag_top) => {
  let scale_fn = idx => float_of_int(100 - 12 * idx) /. 100.;
  let x_fn = idx => float_of_int(12 * idx);
  let init_opacity = 100.;
  let opacity_reduction = 20.; // reduction per line
  let dy_fn = idx => font_metrics.row_height *. scale_fn(idx) -. 4.;
  let (_, _, _, shards) =
    List.fold_left(
      ((idx, y_offset, opacity, vs), s: string) => {
        let new_y_offset = y_offset -. dy_fn(idx);
        let v = shard(x_fn(idx), new_y_offset, scale_fn(idx), opacity, s);
        (idx + 1, new_y_offset, opacity -. opacity_reduction, [v, ...vs]);
      },
      (0, dy_fn(0), init_opacity, []),
      contents,
    );
  div(
    ~attrs=[
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; left: %fpx; top: %fpx;",
          left,
          flag_top,
        ),
      ),
      Attr.classes(["backpack"]),
    ],
    shards,
  );
};

let complete_bullshit =
    (~caret_d: option(Direction.t), ~ind_d: option(Direction.t)) =>
  (-1.)
  +. (
    switch (caret_d) {
    | None => 0.
    | Some(Left) =>
      switch (ind_d) {
      | Some(Left) => ShardDec.shape_adjust(Left, Some(Left)) +. 3.0
      | Some(Right) => ShardDec.shape_adjust(Right, Some(Left)) +. 2.0
      | _ => 2.5
      }
    | Some(Right) =>
      switch (ind_d) {
      | Some(Left) => ShardDec.shape_adjust(Left, Some(Right)) -. 2.0
      | Some(Right) => ShardDec.shape_adjust(Right, Some(Right)) -. 3.0
      | _ => (-2.0)
      }
    }
  );

let view =
    (
      ~font_metrics: Haz3lcorep.FontMetrics.t,
      ~can_put_down,
      ~caret_d: option(Direction.t),
      ~ind_d: option(Direction.t),
      ~origin: Point.t,
      contents: list(string),
    )
    : Node.t => {
  let max_disp = 4; /* Maximum vertical backpack displacement */
  let height_offset = 0.33 /* Offset from regular line spacing */;
  let genie_height = 0.3; /* Height of triangle joining pole to flag */
  let genie_width =
    0.4
    +. Float.of_int(
         switch (contents) {
         | [] => 0
         | [hd, ..._] => String.length(hd)
         },
       );
  let left =
    Float.of_int(origin.col)
    *. font_metrics.col_width
    +. complete_bullshit(~caret_d, ~ind_d);
  let vertical_disp = origin.row <= max_disp ? origin.row : max_disp;
  let top_baseline =
    Float.of_int(origin.row - vertical_disp + (origin.row == 0 ? 0 : 1));
  let flag_top =
    (top_baseline -. height_offset -. 1.0) *. font_metrics.row_height;
  let genie_top =
    (top_baseline -. height_offset +. genie_height) *. font_metrics.row_height;
  let pole_top =
    (Float.of_int(origin.row - vertical_disp) +. 1.0 -. height_offset)
    *. font_metrics.row_height;
  let pole_height =
    (Float.of_int(vertical_disp - 1) +. height_offset)
    *. font_metrics.row_height;
  div(
    ~attrs=[
      Attr.classes(["backpack"] @ (can_put_down ? [] : ["cant-put-down"])),
    ],
    [
      flag(~font_metrics, ~left, ~contents, ~flag_top),
      genie(~font_metrics, ~left, ~genie_top, ~genie_height, ~genie_width),
    ]
    @ [pole(~left, ~pole_top, ~pole_height)],
  );
};
