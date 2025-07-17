open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;

let shape_map = ProjectorCore.Shape.Map.empty; /* Assume this doesn't contain projectors */

let measured_of = seg => Measured.of_segment(seg, shape_map); /* Assume this doesn't contain projectors */

let text_view = (font_metrics, seg: Segment.t): list(Node.t) => {
  module Text =
    Code.Text({
      let map = measured_of(seg);
      let settings = Settings.Model.init;
      let shape_map = shape_map;
      let font_metrics = font_metrics;
    });
  Text.of_segment([], true, Any, seg);
};

let segment_origin = (seg: Segment.t): option(Point.t) =>
  Option.map(
    first => Measured.find_p(first, measured_of(seg)).origin,
    ListUtil.hd_opt(seg),
  );

let segment_last = (seg: Segment.t): option(Point.t) =>
  Option.map(
    last => Measured.find_p(last, measured_of(seg)).last,
    ListUtil.last_opt(seg),
  );

let segment_height = (seg: Segment.t) =>
  switch (segment_last(seg), segment_origin(seg)) {
  | (Some(last), Some(first)) => 1 + last.row - first.row
  | _ => 0
  };

let segment_width = (seg: Segment.t): int =>
  IntMap.fold(
    (_, {max_col, _}: Measured.Rows.shape, acc) => max(max_col, acc),
    measured_of(seg).rows,
    0,
  );

let backpack_sel_view =
    (
      x_off: float,
      y_off: float,
      scale: float,
      opacity: float,
      font_metrics,
      {focus: _, content, _}: Selection.t,
    ) => {
  // Maybe use init sort at caret to prime this
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
    // zwsp necessary for containing box to stretch to contain trailing newline
    text_view(font_metrics, content) @ [text(Unicode.zwsp)],
  );
};

let view =
    (~font_metrics: FontMetrics.t, ~origin: Point.t, z: Zipper.t): Node.t => {
  // This function is a mess
  let backpack =
    Zipper.local_wanted_shards'(z)
    @ (
      Ancestors.non_local_incomplete_tiles(z.relatives.ancestors)
      |> List.map(incomplete =>
           Tile.right_missing_shards(incomplete)
           @ Tile.left_missing_shards(incomplete)
         )
      |> List.concat
    )
    |> List.map(t => Selection.mk(~focus=Right, [Tile(t)]));
  let height_head =
    switch (backpack) {
    | [] => 0
    | [hd, ..._] => segment_height(hd.content)
    };
  let can_put_down =
    //TODO(andrew): update with new logic
    switch (Zipper.local_wanted_shards'(z)) {
    | [] => false
    | _ => z.caret == Outer
    };
  let ind_p_d =
    switch (Indicated.piece(z)) {
    | Some((_, d, _)) => Some(d)
    | None => None
    };
  let caret_d = Zipper.caret_direction(z);
  let caret_adj = {
    switch (caret_d) {
    | None => 0.
    | Some(Left) =>
      switch (ind_p_d) {
      | Some(Left) => ShardDec.shape_adjust(Left, Some(Left)) +. 3.0
      | Some(Right) => ShardDec.shape_adjust(Right, Some(Left)) +. 2.0
      | _ => 2.5
      }
    | Some(Right) =>
      switch (ind_p_d) {
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
      Float.of_int(
        origin.row - vertical_disp - height_head + (origin.row == 0 ? 0 : 1),
      )
      *. font_metrics.row_height,
    );
  print_endline(selections_style);
  let scale_fn = idx => float_of_int(100 - 12 * idx) /. 100.;
  let x_fn = idx => float_of_int(12 * idx);
  let init_opacity = 100.;
  let opacity_reduction = 20.; // reduction per line
  let init_idx = 0;
  let dy_fn = (idx, base_height) =>
    font_metrics.row_height
    *. float_of_int(base_height)
    *. scale_fn(idx)
    -. 4.;
  let init_y_offset = dy_fn(init_idx, height_head);
  let (_, _, _, selections) =
    List.fold_left(
      ((idx, y_offset, opacity, vs), s: Selection.t) => {
        let base_height = segment_height(s.content);
        let scale = scale_fn(idx);
        let x_offset = x_fn(idx);
        let new_y_offset = y_offset -. dy_fn(idx, base_height);
        let v =
          backpack_sel_view(
            x_offset,
            new_y_offset,
            scale,
            opacity,
            font_metrics,
            s,
          );
        let new_idx = idx + 1;
        let new_opacity = opacity -. opacity_reduction;
        // Am i making this difficult by going backwards?
        (new_idx, new_y_offset, new_opacity, List.cons(v, vs));
      },
      (init_idx, init_y_offset, init_opacity, []),
      backpack,
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
    switch (backpack) {
    | [] => 0
    | [hd, ..._] => segment_width(hd.content)
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
    @ (backpack != [] ? [joiner] : []),
  );
};
