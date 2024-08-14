open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;

/* Assume this doesn't contain projectors */
let measured_of = seg => Measured.of_segment(seg, Id.Map.empty);

let text_view = (seg: Segment.t(Id.t)): list(Node.t) => {
  module Text =
    Code.Text({
      let map = measured_of(seg);
      let settings = Init.startup.settings;
      let info_map = Id.Map.empty; /* Assume this doesn't contain projectors */
    });
  Text.of_segment([], true, Any, seg);
};

let segment_origin = (seg: Segment.t(Id.t)): option(Point.t) =>
  Option.map(
    first => Measured.find_p(first, measured_of(seg)).origin,
    ListUtil.hd_opt(seg),
  );

let segment_last = (seg: Segment.t(Id.t)): option(Point.t) =>
  Option.map(
    last => Measured.find_p(last, measured_of(seg)).last,
    ListUtil.last_opt(seg),
  );

let segment_height = (seg: Segment.t(Id.t)) =>
  switch (segment_last(seg), segment_origin(seg)) {
  | (Some(last), Some(first)) => 1 + last.row - first.row
  | _ => 0
  };

let segment_width = (seg: Segment.t(Id.t)): int =>
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
      {focus: _, content, _}: Selection.t,
    ) => {
  // TODO(andrew): Maybe use init sort at caret to prime this
  div(
    ~attrs=[
      Attr.classes(["code-text", "backpack-selection"]),
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
    text_view(content) @ [text(Unicode.zwsp)],
  );
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: Point.t,
      {backpack, _} as z: Zipper.t,
    )
    : Node.t => {
  //TODO(andrew): clean up this dumpster fire of a function
  let height_head =
    switch (backpack) {
    | [] => 0
    | [hd, ..._] => segment_height(hd.content)
    };
  let can_put_down =
    switch (Zipper.pop_backpack(z)) {
    // caret thing is hack; i don't know why pop_backpack
    // gives us what we want here
    | Some(_) => z.caret == Outer
    | None => false
    };
  let caret_adj = {
    let shape = Zipper.caret_direction(z);
    let side =
      switch (Indicated.piece(z)) {
      | Some((_, side, _)) => side
      | _ => Right
      };
    DecUtil.caret_adjust(side, shape);
  };
  let caret_adj_px =
    //TODO(andrew): figure out why we need this mystery pixel below
    (-1.) +. caret_adj *. font_metrics.col_width;
  let max_disp = 3; /* Maximum vertical backpack displacement */
  let vertical_disp = origin.row <= max_disp ? origin.row : max_disp;
  let selections_style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
      Float.of_int(origin.row - vertical_disp - height_head - 1)
      *. font_metrics.row_height,
    );
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
        let v = backpack_sel_view(x_offset, new_y_offset, scale, opacity, s);
        let new_idx = idx + 1;
        let new_opacity = opacity -. opacity_reduction;
        //TODO(andrew): am i making this difficult by going backwards?
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
      (-3.)
      +. Float.of_int(origin.row - vertical_disp)
      *. font_metrics.row_height,
      3. +. Float.of_int(vertical_disp) *. font_metrics.row_height,
    );
  let joiner =
    div(
      ~attrs=[
        Attr.create("style", joiner_style),
        Attr.classes(["backpack-joiner"]),
      ],
      [],
    );
  //TODO(andrew): break out backpack decoration into its own module
  let genie_view =
    DecUtil.code_svg(
      ~font_metrics,
      ~origin={row: 0, col: 0},
      ~base_cls=["restructuring-genie"],
      ~path_cls=["restructuring-genie-path"],
      SvgUtil.Path.[
        M({x: 0., y: 0.}),
        V({y: (-1.0)}),
        H_({dx: Float.of_int(length)}),
        V_({dy: 0.0}),
        Z,
      ],
    );
  let genie_style =
    Printf.sprintf(
      "position: absolute; left: %fpx; top: %fpx;",
      Float.of_int(origin.col) *. font_metrics.col_width +. caret_adj_px,
      Float.of_int(origin.row - vertical_disp)
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
