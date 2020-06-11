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

let view =
    (
      ~model: Model.t,
      ~inject: Update.Action.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~caret_pos: option((int, int)),
      l: UHLayout.t,
    ) =>
  TimeUtil.measure_time(
    "UHCode.view",
    model.measurements.measurements && model.measurements.uhcode_view,
    () => {
      open Vdom;
      let (_, vs, _): (UHLayout.t, list(Vdom.Node.t), Decoration.shape) =
        l
        |> UHLayout.pos_fold(
             ~linebreak=
               pos =>
                 (
                   Pretty.Layout.Linebreak,
                   [Node.br([])],
                   {
                     let start_col = pos.col - pos.indent;
                     Decoration.{start_col, boxes: [(1, 0), (1, 0)]};
                   },
                 ),
             ~text=
               (pos, s) =>
                 (
                   Text(s),
                   StringUtil.is_empty(s) ? [] : [Node.text(s)],
                   {
                     start_col: pos.col - pos.indent,
                     boxes: [(1, StringUtil.utf8_length(s))],
                   },
                 ),
             ~align=
               (pos, (l, vs, shape)) =>
                 (
                   Align(l),
                   [Node.div([Attr.classes(["Align"])], vs)],
                   {
                     let (total_height, max_width) =
                       shape.boxes
                       |> List.fold_left(
                            ((total_height, max_width), (height, width)) =>
                              (total_height + height, max(max_width, width)),
                            (0, Int.min_int),
                          );
                     {
                       start_col: pos.col - pos.indent,
                       boxes: [(total_height, max_width)],
                     };
                   },
                 ),
             ~cat=
               (_, (l1, vs1, shape1), (l2, vs2, shape2)) =>
                 (
                   Cat(l1, l2),
                   vs1 @ vs2,
                   {
                     let (leading, last) =
                       shape1.boxes
                       |> ListUtil.split_last
                       |> OptUtil.get(() =>
                            failwith("expected at least one box")
                          );
                     let mid_height =
                       max(fst(last), fst(List.hd(shape2.boxes)));
                     let mid_width = snd(last) + snd(List.hd(shape2.boxes));
                     {
                       ...shape1,
                       boxes:
                         leading
                         @ [
                           (mid_height, mid_width),
                           ...List.tl(shape2.boxes),
                         ],
                     };
                   },
                 ),
             ~annot=
               (_, annot, (l, vs, shape)) =>
                 (
                   Annot(annot, l),
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
                   | DelimGroup => [
                       Node.span([Attr.classes(["DelimGroup"])], vs),
                     ]
                   | LetLine => [
                       Node.span([Attr.classes(["LetLine"])], vs),
                     ]

                   | Padding => [
                       Node.span([Attr.classes(["Padding"])], vs),
                     ]
                   | Indent => [Node.span([Attr.classes(["Indent"])], vs)]

                   | HoleLabel({len}) =>
                     let font_width = font_metrics.col_width;
                     let full_space = font_width *. float_of_int(len);
                     let width =
                       Css_gen.width(
                         `Px(int_of_float(Float.round(full_space))),
                       );
                     let styling = Vdom.Attr.style(width);
                     [
                       Node.span([styling, Attr.classes(["HoleLabel"])], vs),
                     ];
                   | UserNewline => [
                       Node.span([Attr.classes(["UserNewline"])], vs),
                     ]

                   | OpenChild({is_inline}) => [
                       Node.span(
                         [
                           Attr.classes([
                             "OpenChild",
                             is_inline ? "Inline" : "Para",
                           ]),
                         ],
                         vs,
                       ),
                     ]
                   | ClosedChild({is_inline}) => [
                       Node.span(
                         [
                           Attr.classes([
                             "ClosedChild",
                             is_inline ? "Inline" : "Para",
                           ]),
                         ],
                         vs,
                       ),
                     ]

                   | Term({has_cursor, shape: term_shape, sort}) =>
                     let err_hole_decoration =
                       switch (term_shape) {
                       | Rule
                       | SubBlock(_) => []
                       | Var({err, _})
                       | Operand({err})
                       | BinOp({err, _})
                       | NTuple({err, _})
                       | Case({err: StandardErrStatus(err)}) =>
                         switch (err) {
                         | NotInHole => []
                         | InHole(_) => [
                             Decoration.err_hole_view(~font_metrics, shape),
                           ]
                         }
                       | Case({err: InconsistentBranches(_)}) => [
                           Decoration.err_hole_view(~font_metrics, shape),
                         ]
                       };
                     [
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
                         err_hole_decoration @ vs,
                       ),
                     ];
                   },
                   shape,
                 ),
           );

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
