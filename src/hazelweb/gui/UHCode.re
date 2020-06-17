module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;
open Sexplib.Std;

module Decoration = {
  [@deriving sexp]
  type shape = {
    // start col of first line (relative to indent)
    start_col: int,
    // box: (height, width)
    boxes: list((int, int)),
  };

  type path_segment =
    | VLine(float)
    | HLine(float)
    | Corner(corner, direction, (float, float))
  and corner =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
  and direction =
    | CW
    | CCW;

  let corner_radius = 2.5;

  let outline_path = (~corner_radii, shape) => {
    let (rx, ry) = corner_radii;

    // TODO fix confusing api, returns half of passed len
    let vline_down = len => VLine(0.5 *. Float.of_int(len) -. ry);
    let vline_up = len => VLine(Float.neg(0.5 *. Float.of_int(len) -. ry));

    let hline_left = len =>
      HLine(Float.neg(Float.of_int(len) -. 2.0 *. rx));
    let hline_right = len => HLine(float_of_int(len) -. 2.0 *. rx);

    let corner_TopLeft = d => Corner(TopLeft, d, corner_radii);
    let corner_TopRight = d => Corner(TopRight, d, corner_radii);
    let corner_BottomLeft = d => Corner(BottomLeft, d, corner_radii);
    let corner_BottomRight = d => Corner(BottomRight, d, corner_radii);

    let top_cap = ((height, width)) => [
      vline_up(height),
      corner_TopLeft(CW),
      hline_right(width),
      corner_TopRight(CW),
      vline_down(height),
    ];
    let bottom_cap = ((height, width)) => [
      vline_down(height),
      corner_BottomRight(CW),
      hline_left(width),
      corner_BottomLeft(CW),
      vline_up(height),
    ];

    let left_edge =
        // list of cols from last line up
        (dims: list((int, int))) => {
      dims
      |> ListUtil.pairs
      |> List.map((((height1, col1), (height2, col2))) =>
           if (col1 == col2) {
             [
               VLine(
                 (-1.0)
                 *. (
                   Float.of_int(height1)
                   /. 2.0
                   +. Float.of_int(height2)
                   /. 2.0
                 ),
               ),
             ];
           } else if (col1 > col2) {
             [
               vline_up(height1),
               corner_TopRight(CCW),
               hline_left(col1 - col2),
               corner_BottomLeft(CW),
               vline_up(height2),
             ];
           } else {
             [
               vline_up(height1),
               corner_TopLeft(CW),
               hline_right(col2 - col1),
               corner_BottomRight(CCW),
               vline_up(height2),
             ];
           }
         )
      |> List.flatten;
    };
    let right_edge =
        // list of cols from first line down
        (dims: list((int, int))) => {
      dims
      |> ListUtil.pairs
      |> List.map((((height1, col1), (height2, col2))) =>
           if (col1 == col2) {
             [
               VLine(
                 Float.of_int(height1) /. 2.0 +. Float.of_int(height2) /. 2.0,
               ),
             ];
           } else if (col1 > col2) {
             [
               vline_down(height1),
               corner_BottomRight(CW),
               hline_left(col1 - col2),
               corner_TopLeft(CCW),
               vline_down(height2),
             ];
           } else {
             [
               vline_down(height1),
               corner_BottomLeft(CCW),
               hline_right(col2 - col1),
               corner_TopRight(CW),
               vline_down(height2),
             ];
           }
         )
      |> List.flatten;
    };

    let (leading, last) =
      shape.boxes
      |> ListUtil.split_last
      |> OptUtil.get(() => failwith("expected at least one box"));
    switch (leading) {
    | [] => top_cap(last) @ bottom_cap(last)
    | [(hd_height, _) as hd, ..._] =>
      top_cap(hd)
      @ right_edge(shape.boxes)
      @ bottom_cap(last)
      @ left_edge(
          List.rev([
            (hd_height, shape.start_col),
            ...List.map(
                 ((height, _)) => (height, 0),
                 List.tl(shape.boxes),
               ),
          ]),
        )
    };
  };

  let path_view = (~rel_hd_start, path: list(path_segment)) => {
    let d_path_segments =
      path
      |> List.map(
           fun
           | VLine(len) => "v " ++ string_of_float(len) ++ "0"
           | HLine(len) => "h " ++ string_of_float(len) ++ "0"
           | Corner(corner, direction, (rx, ry)) => {
               let direction_scalar =
                 switch (direction) {
                 | CW => 1.0
                 | CCW => (-1.0)
                 };
               let (dx, dy) =
                 switch (corner) {
                 | TopLeft => (rx, Float.neg(ry))
                 | TopRight => (rx, ry)
                 | BottomRight => (Float.neg(rx), ry)
                 | BottomLeft => (Float.neg(rx), Float.neg(ry))
                 };
               let (dx, dy) = (
                 dx *. direction_scalar,
                 dy *. direction_scalar,
               );
               StringUtil.sep([
                 "a",
                 string_of_float(rx) ++ "0",
                 string_of_float(ry) ++ "0",
                 "0",
                 "0",
                 switch (direction) {
                 | CW => "1"
                 | CCW => "0"
                 },
                 string_of_float(dx) ++ "0",
                 string_of_float(dy) ++ "0",
               ]);
             },
         );
    Vdom.(
      Node.create_svg(
        "path",
        [
          Attr.create("fill", "none"),
          Attr.create(
            "d",
            StringUtil.sep([
              "M 0 0",
              "m "
              ++ string_of_int(snd(rel_hd_start))
              ++ " "
              ++ string_of_float(0.5 *. Float.of_int(fst(rel_hd_start)))
              ++ "0",
              ...d_path_segments,
            ]),
          ),
          Attr.create("stroke-width", "1px"),
          Attr.create("vector-effect", "non-scaling-stroke"),
        ],
        [],
      )
    );
  };

  let err_hole_view = (~font_metrics, shape) => {
    let FontMetrics.{row_height, col_width} = font_metrics;

    let num_rows = shape.boxes |> List.map(fst) |> List.fold_left((+), 0);
    let buffered_height = float_of_int(num_rows + 1) *. row_height;

    let num_cols =
      shape.boxes |> List.map(snd) |> List.fold_left(max, Int.min_int);
    let buffered_width = float_of_int(num_cols + 1) *. col_width;

    let path_view = {
      let corner_radii = (
        corner_radius /. col_width,
        corner_radius /. row_height,
      );
      shape
      |> outline_path(~corner_radii)
      |> path_view(
           ~rel_hd_start=(fst(List.hd(shape.boxes)), shape.start_col),
         );
    };

    Vdom.(
      Node.div(
        [
          Attr.classes(["code-err-hole"]),
          Attr.create(
            "style",
            StringUtil.sep([
              "top:"
              ++ string_of_float(Float.neg(0.5 *. row_height))
              ++ "0px;",
              "left:"
              ++ string_of_float(Float.neg(0.5 *. col_width))
              ++ "0px;",
            ]),
          ),
        ],
        [
          Node.create_svg(
            "svg",
            [
              Attr.create(
                "viewBox",
                Printf.sprintf("-0.5 -0.5 %d %d", num_cols + 1, num_rows + 1),
              ),
              Attr.create("width", string_of_float(buffered_width) ++ "0px"),
              Attr.create(
                "height",
                string_of_float(buffered_height) ++ "0px",
              ),
              Attr.create("stroke", "red"),
              Attr.create("stroke-dasharray", "3 2"),
              Attr.create("preserveAspectRatio", "none"),
            ],
            [path_view],
          ),
        ],
      )
    );
  };
};

let clss_of_err: ErrStatus.t => list(cls) =
  fun
  | NotInHole => []
  | InHole(_) => ["InHole"];

let clss_of_verr: VarErrStatus.t => list(cls) =
  fun
  | NotInVarHole => []
  | InVarHole(_) => ["InVarHole"];

let clss_of_case_err: CaseErrStatus.t => list(cls) =
  fun
  | StandardErrStatus(err) => clss_of_err(err)
  | InconsistentBranches(_) => ["InconsistentBranches"];

let cursor_clss = (has_cursor: bool): list(cls) =>
  has_cursor ? ["Cursor"] : [];

let sort_clss: TermSort.t => list(cls) =
  fun
  | Typ => ["Typ"]
  | Pat => ["Pat"]
  | Exp => ["Exp"];

let shape_clss: TermShape.t => list(cls) =
  fun
  | Rule => ["Rule"]
  | Case({err}) => ["Case", ...clss_of_case_err(err)]
  | Var({err, verr, show_use}) =>
    ["Operand", "Var", ...clss_of_err(err)]
    @ clss_of_verr(verr)
    @ (show_use ? ["show-use"] : [])
  | Operand({err}) => ["Operand", ...clss_of_err(err)]
  | BinOp({err, op_index: _}) => ["BinOp", ...clss_of_err(err)]
  | NTuple({err, comma_indices: _}) => ["NTuple", ...clss_of_err(err)]
  | SubBlock(_) => ["SubBlock"];

let open_child_clss = (has_inline_OpenChild: bool, has_para_OpenChild: bool) =>
  List.concat([
    has_inline_OpenChild ? ["has-Inline-OpenChild"] : [],
    has_para_OpenChild ? ["has-Para-OpenChild"] : [],
  ]);

let has_child_clss = (has_child: bool) =>
  has_child ? ["has-child"] : ["no-children"];

let caret_from_pos = (x: float, y: float): Vdom.Node.t => {
  let pos_attr =
    Vdom.Attr.style(
      Css_gen.combine(
        Css_gen.left(`Px(int_of_float(Float.round(x)))),
        Css_gen.top(`Px(int_of_float(Float.round(y)))),
      ),
    );
  Vdom.Node.span(
    [Vdom.Attr.id("caret"), pos_attr, Vdom.Attr.classes(["blink"])],
    [],
  );
};

type box = {
  height: int,
  // Max number of columns used. For the first box, add to offset to get last
  // column used (relative to nearest containing Align).  For any other box,
  // `width` is the last column used (again relative to nearest containing
  // Align).
  width: int,
};
type metrics = {
  // column offset of the first box relative to nearest containing Align
  offset: int,
  boxes: list(box),
  // TODO: add last_offset so we can support aligns
  // that are not padded on the right
};

let table: WeakMap.t(UHLayout.t, IntMap.t((metrics, list(Vdom.Node.t)))) =
  WeakMap.mk();

let rec mem_view =
        (~offset: int, l: UHLayout.t): (metrics, list(Vdom.Node.t)) => {
  open Vdom;
  let f = (~offset, l: UHLayout.t): (metrics, list(Vdom.Node.t)) =>
    switch (l) {
    | Linebreak =>
      let metrics = {
        let box = {height: 1, width: 0};
        {offset, boxes: [box, box]};
      };
      (metrics, [Node.br([])]);
    | Text(s) =>
      let metrics = {
        offset,
        boxes: [{height: 1, width: StringUtil.utf8_length(s)}],
      };
      let vs = StringUtil.is_empty(s) ? [] : [Node.text(s)];
      (metrics, vs);
    | Align(l) =>
      let (metrics, vs) = mem_view(~offset=0, l);
      let bounding_metrics = {
        let bounding_box =
          metrics.boxes
          |> List.fold_left(
               ({height: bh, width: bw}, {height, width}) =>
                 {height: bh + height, width: max(bw, width)},
               {height: 0, width: 0},
             );
        {offset, boxes: [bounding_box]};
      };
      let vs = [Node.div([Attr.classes(["Align"])], vs)];
      (bounding_metrics, vs);
    | Cat(l1, l2) =>
      let (metrics1, vs1) = mem_view(~offset, l1);
      let (leading, last) = ListUtil.split_last(metrics1.boxes);
      let (metrics2, vs2) = {
        // If metrics1 is only one box, then width of the last box is added
        // to offset, otherwise it is from offset 0.
        // TODO: This assumes all aligns are padded on the right.
        let start_offset = leading == [] ? offset : 0;
        let offset = last.width + start_offset;
        mem_view(~offset, l2);
      };
      let (first, trailing) = ListUtil.split_first(metrics2.boxes);
      let mid_box = {
        height: max(last.height, first.height),
        width: last.width + first.width,
      };
      let metrics = {offset, boxes: leading @ [mid_box, ...trailing]};
      (metrics, vs1 @ vs2);
    | Annot(annot, l) =>
      let (metrics, vs) = mem_view(~offset, l);
      let vs =
        switch (annot) {
        | Step(_)
        | EmptyLine
        | SpaceOp => vs
        | Token({shape, _}) =>
          let clss =
            switch (shape) {
            | Text => ["code-text"]
            | Op => ["code-op"]
            | Delim(_) => ["code-delim"]
            };
          [Node.span([Attr.classes(clss)], vs)];
        | DelimGroup => [Node.span([Attr.classes(["DelimGroup"])], vs)]
        | LetLine => [Node.span([Attr.classes(["LetLine"])], vs)]

        | Padding => [Node.span([Attr.classes(["Padding"])], vs)]
        | Indent => [Node.span([Attr.classes(["Indent"])], vs)]

        | HoleLabel({len}) =>
          // TODO use metrics and remove len
          let width = Css_gen.width(`Ch(float_of_int(len)));
          [
            Node.span(
              [Vdom.Attr.style(width), Attr.classes(["HoleLabel"])],
              [Node.span([Attr.classes(["HoleNumber"])], vs)],
            ),
          ];
        | UserNewline => [Node.span([Attr.classes(["UserNewline"])], vs)]

        | OpenChild({is_inline}) => [
            Node.span(
              [Attr.classes(["OpenChild", is_inline ? "Inline" : "Para"])],
              vs,
            ),
          ]
        | ClosedChild({is_inline}) => [
            Node.span(
              [Attr.classes(["ClosedChild", is_inline ? "Inline" : "Para"])],
              vs,
            ),
          ]

        | Term({has_cursor, shape: term_shape, sort}) => [
            Node.span(
              [
                Attr.classes(
                  List.concat([
                    ["Term"],
                    cursor_clss(has_cursor),
                    sort_clss(sort),
                    shape_clss(term_shape),
                    open_child_clss(
                      l |> UHLayout.has_inline_OpenChild,
                      l |> UHLayout.has_para_OpenChild,
                    ),
                    has_child_clss(l |> UHLayout.has_child),
                  ]),
                ),
              ],
              vs,
            ),
          ]
        };

      (metrics, vs);
    };

  switch (WeakMap.get(table, l)) {
  | Some(offset_table) =>
    switch (IntMap.find_opt(offset, offset_table)) {
    | Some(result) => result
    | None =>
      let result = f(~offset, l);
      ignore(
        WeakMap.set(table, l, IntMap.add(offset, result, offset_table)),
      );
      result;
    }
  | None =>
    let result = f(~offset, l);
    ignore(WeakMap.set(table, l, IntMap.singleton(offset, result)));
    result;
  };
};

let view =
    (
      ~model: Model.t,
      ~inject: Update.Action.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~caret_pos: option((int, int)),
      l: UHLayout.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    model.measurements.measurements && model.measurements.uhcode_view,
    () => {
      open Vdom;

      let (_, vs) = mem_view(~offset=0, l);
      let children =
        switch (caret_pos) {
        | None => vs
        | Some((row, col)) =>
          let x = float_of_int(col) *. model.font_metrics.col_width;
          let y = float_of_int(row) *. model.font_metrics.row_height;
          let caret = caret_from_pos(x, y);
          [caret, ...vs];
        };
      let id = "code-root";
      Node.div(
        [
          Attr.id(id),
          Attr.classes(["code", "presentation"]),
          // need to use mousedown instead of click to fire
          // (and move caret) before cell focus event handler
          Attr.on_mousedown(evt => {
            let container_rect =
              JSUtil.force_get_elem_by_id(id)##getBoundingClientRect;
            let (target_x, target_y) = (
              float_of_int(evt##.clientX),
              float_of_int(evt##.clientY),
            );
            let row_col = (
              Float.to_int(
                (target_y -. container_rect##.top) /. font_metrics.row_height,
              ),
              Float.to_int(
                Float.round(
                  (target_x -. container_rect##.left) /. font_metrics.col_width,
                ),
              ),
            );
            inject(Update.Action.MoveAction(Click(row_col)));
          }),
        ],
        children,
      );
    },
  );
};
