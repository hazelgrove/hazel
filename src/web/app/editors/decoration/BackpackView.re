open Virtual_dom.Vdom;
open Node;
open Util;

let backpack_shard_view =
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

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~can_put_down,
      ~caret_d: option(Direction.t),
      ~ind_d: option(Direction.t),
      ~origin: Point.t,
      contents: list(string),
    )
    : Node.t => {
  // This function is a mess
  let caret_adj = {
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
    };
  };
  let caret_adj_px = (-1.) +. caret_adj;
  let max_disp = 4; /* Maximum vertical backpack displacement */
  let vertical_disp = origin.row <= max_disp ? origin.row : max_disp;
  print_endline(
    Printf.sprintf(
      "vertical_disp: %d, origin.row: %d, max_disp: %d",
      vertical_disp,
      origin.row,
      max_disp,
    ),
  );
  let selections_style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
      Float.of_int(origin.row - vertical_disp - 1 + (origin.row == 0 ? 0 : 1))
      *. font_metrics.row_height,
    );
  print_endline(selections_style);
  let scale_fn = idx => float_of_int(100 - 12 * idx) /. 100.;
  let x_fn = idx => float_of_int(12 * idx);
  let init_opacity = 100.;
  let opacity_reduction = 20.; // reduction per line
  let init_idx = 0;
  let dy_fn = idx => font_metrics.row_height *. scale_fn(idx) -. 4.;
  let init_y_offset = dy_fn(init_idx);
  let (_, _, _, selections) =
    List.fold_left(
      ((idx, y_offset, opacity, vs), s: string) => {
        let scale = scale_fn(idx);
        let x_offset = x_fn(idx);
        let new_y_offset = y_offset -. dy_fn(idx);
        let v =
          backpack_shard_view(x_offset, new_y_offset, scale, opacity, s);
        let new_idx = idx + 1;
        let new_opacity = opacity -. opacity_reduction;
        // Am i making this difficult by going backwards?
        (new_idx, new_y_offset, new_opacity, List.cons(v, vs));
      },
      (init_idx, init_y_offset, init_opacity, []),
      contents,
    );
  let selections_view =
    div(
      ~attrs=[
        Attr.create("style", selections_style),
        Attr.classes(["backpack"]),
      ],
      selections,
    );
  let length =
    switch (contents) {
    | [] => 0
    | [hd, ..._] => String.length(hd)
    };

  let joiner_style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx; height: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
      +. Float.of_int(origin.row - vertical_disp) *. font_metrics.row_height,
      Float.of_int(vertical_disp) *. font_metrics.row_height,
    );
  let joiner =
    div(
      ~attrs=[
        Attr.create("style", joiner_style),
        Attr.classes(["backpack-joiner"]),
      ],
      [],
    );
  let genie_height = 0.3;
  let genie_view =
    DecUtil.code_svg(
      ~font_metrics,
      ~origin={
        row: 0,
        col: 0,
      },
      ~base_cls=["restructuring-genie"],
      ~path_cls=["backpack-genie"],
      SvgUtil.Path.[
        M({
          x: 0.,
          y: 0.,
        }),
        V({y: -. genie_height}),
        H_({dx: Float.of_int(length)}),
        V_({dy: 0.0}),
        Z,
      ],
    );

  let genie_style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
      (
        Float.of_int(
          origin.row
          - vertical_disp
          + (
            switch (origin.row) {
            | 0 => 0
            | 1 => 1
            | _ => 2
            }
          ),
        )
        -. 1.0
        +. genie_height
      )
      *. font_metrics.row_height
      +. 1.,
    );
  div(
    ~attrs=[
      Attr.classes(["backpack"] @ (can_put_down ? [] : ["cant-put-down"])),
    ],
    [
      selections_view,
      div(~attrs=[Attr.create("style", genie_style)], [genie_view]),
    ]
    @ (contents != [] ? [joiner] : []),
  );
};
