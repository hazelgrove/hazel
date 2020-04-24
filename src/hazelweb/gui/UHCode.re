module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

module Decoration = {
  type shape = {
    // start col of first line
    // (where n means n colums from
    // start of current indentation)
    hd_start: int,
    // width of first line
    hd_width: int,
    // widths of all other lines
    tl_widths: list(int),
  };

  type path_segment =
    | VLine(float)
    | HLine(float)
    | Corner(corner, (float, float))
  and corner =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight;

  let corner_radius = 4.0;

  let outline_path = (~corner_radii, shape) => {
    let (rx, ry) = corner_radii;

    let vline_down = VLine(0.5 -. ry);
    let vline_up = VLine(Float.neg(0.5 -. ry));

    let hline_left = len =>
      HLine(Float.neg(Float.of_int(len) -. 2.0 *. rx));
    let hline_right = len => HLine(float_of_int(len) -. 2.0 *. rx);

    let corner_TopLeft = Corner(TopLeft, corner_radii);
    let corner_TopRight = Corner(TopRight, corner_radii);
    let corner_BottomLeft = Corner(BottomLeft, corner_radii);
    let corner_BottomRight = Corner(BottomRight, corner_radii);

    let top_cap = width => [
      vline_up,
      corner_TopLeft,
      hline_right(width),
      corner_TopRight,
      vline_down,
    ];
    let bottom_cap = width => [
      vline_down,
      corner_BottomRight,
      hline_left(width),
      corner_BottomLeft,
      vline_up,
    ];

    let left_edge =
        // list of cols from last line up
        (cols: list(int)) =>
      cols
      |> ListUtil.pairs
      |> List.map(((col1, col2)) =>
           if (col1 == col2) {
             [VLine(-1.0)];
           } else if (col1 > col2) {
             [
               vline_up,
               corner_TopLeft,
               hline_right(col1 - col2),
               corner_BottomRight,
               vline_up,
             ];
           } else {
             [
               vline_up,
               corner_TopRight,
               hline_left(col2 - col1),
               corner_BottomLeft,
               vline_up,
             ];
           }
         )
      |> List.flatten;
    let right_edge =
        // list of cols from first line down
        (cols: list(int)) =>
      cols
      |> ListUtil.pairs
      |> List.map(((col1, col2)) =>
           if (col1 == col2) {
             [VLine(1.0)];
           } else if (col1 > col2) {
             [
               vline_down,
               corner_BottomRight,
               hline_left(col1 - col2),
               corner_TopLeft,
               vline_down,
             ];
           } else {
             [
               vline_down,
               corner_BottomLeft,
               hline_right(col2 - col1),
               corner_TopRight,
               vline_down,
             ];
           }
         )
      |> List.flatten;

    switch (ListUtil.split_last(shape.tl_widths)) {
    | None => top_cap(shape.hd_width) @ bottom_cap(shape.hd_width)
    | Some((_, last_width)) =>
      top_cap(shape.hd_width)
      @ right_edge([shape.hd_width, ...shape.tl_widths])
      @ bottom_cap(last_width)
      @ left_edge(
          ListUtil.replicate(List.length(shape.tl_widths), 0)
          @ [shape.hd_start],
        )
    };
  };

  let path_view = (~hd_start, path: list(path_segment)) => {
    let d_path_segments =
      path
      |> List.map(
           fun
           | VLine(len) => "v " ++ string_of_float(len)
           | HLine(len) => "h " ++ string_of_float(len)
           | Corner(corner, (rx, ry)) => {
               let (dx, dy) =
                 switch (corner) {
                 | TopLeft => (rx, Float.neg(ry))
                 | TopRight => (rx, ry)
                 | BottomRight => (Float.neg(rx), ry)
                 | BottomLeft => (Float.neg(rx), Float.neg(ry))
                 };
               StringUtil.sep([
                 "a",
                 string_of_float(rx),
                 string_of_float(ry),
                 "0",
                 "0",
                 "1",
                 string_of_float(dx),
                 string_of_float(dy),
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
              "m " ++ string_of_int(hd_start) ++ " 0",
              ...d_path_segments,
            ]),
          ),
        ],
        [],
      )
    );
  };

  let err_hole_view = (~font_metrics, shape) => {
    let FontMetrics.{row_height, col_width} = font_metrics;

    let num_rows = 1 + List.length(shape.tl_widths);
    let height = float_of_int(num_rows) *. row_height;

    let num_cols = shape.tl_widths |> List.fold_left(max, shape.hd_width);
    let width = float_of_int(num_cols) *. col_width;

    let path_view = {
      let corner_radii = (
        corner_radius /. col_width,
        corner_radius /. row_height,
      );
      shape
      |> outline_path(~corner_radii)
      |> path_view(~hd_start=shape.hd_start);
    };

    Vdom.(
      Node.create_svg(
        "svg",
        [
          Attr.create(
            "viewBox",
            Printf.sprintf("0 0 %d %d", num_cols, num_rows),
          ),
          Attr.create("width", string_of_float(width)),
          Attr.create("height", string_of_float(height)),
          Attr.create("stroke", "green"),
        ],
        [path_view],
      )
    );
  };
};

let contenteditable_false = Vdom.Attr.create("contenteditable", "false");

let clss_of_err: ErrStatus.t => list(cls) =
  fun
  | NotInHole => []
  | InHole(_) => ["InHole"];

let clss_of_verr: VarErrStatus.t => list(cls) =
  fun
  | NotInVarHole => []
  | InVarHole(_) => ["InVarHole"];

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
  | Case({err}) => ["Case", ...clss_of_err(err)]
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

let caret_from_left = (from_left: float): Vdom.Node.t => {
  assert(0.0 <= from_left && from_left <= 100.0);
  let left_attr =
    Vdom.Attr.create(
      "style",
      "left: " ++ string_of_float(from_left) ++ "0%;",
    );
  Vdom.Node.span(
    [Vdom.Attr.id("caret"), left_attr, Vdom.Attr.classes(["blink"])],
    [],
  );
};

let view = (~font_metrics: FontMetrics.t, l: UHLayout.t) => {
  open Vdom;
  let (_, vs, _): (UHLayout.t, list(Vdom.Node.t), Decoration.shape) =
    l
    |> UHLayout.pos_fold(
         ~linebreak=
           pos =>
             (
               Pretty.Layout.Linebreak,
               [Node.br([])],
               Decoration.{
                 hd_start: pos.col - pos.indent,
                 hd_width: 0,
                 tl_widths: [0],
               },
             ),
         ~text=
           (pos, s) =>
             (
               Text(s),
               StringUtil.is_empty(s) ? [] : [Node.text(s)],
               {
                 hd_start: pos.col - pos.indent,
                 hd_width: StringUtil.utf8_length(s),
                 tl_widths: [],
               },
             ),
         ~align=
           (_, (l, vs, shape)) =>
             (
               Align(l),
               [Node.div([Attr.classes(["Align"])], vs)],
               shape,
             ),
         ~cat=
           (_, (l1, vs1, shape1), (l2, vs2, shape2)) =>
             (
               Cat(l1, l2),
               vs1 @ vs2,
               switch (shape1.tl_widths) {
               | [] => {
                   hd_start: shape1.hd_start,
                   hd_width: shape1.hd_width + shape2.hd_width,
                   tl_widths: shape2.tl_widths,
                 }
               | [_, ..._] => {
                   hd_start: shape1.hd_start,
                   hd_width: shape1.hd_width,
                   tl_widths:
                     shape1.tl_widths @ [shape2.hd_width, ...shape2.tl_widths],
                 }
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
               | Token({shape, len, has_cursor}) =>
                 let clss =
                   switch (shape) {
                   | Text => ["code-text"]
                   | Op => ["code-op"]
                   | Delim(_) => ["code-delim"]
                   };
                 let vs =
                   switch (has_cursor) {
                   | None => vs
                   | Some(j) => [
                       caret_from_left(
                         len == 0
                           ? 0.0
                           : float_of_int(j) /. float_of_int(len) *. 100.0,
                       ),
                       ...vs,
                     ]
                   };
                 [Node.span([Attr.classes(clss)], vs)];
               | DelimGroup => [
                   Node.span([Attr.classes(["DelimGroup"])], vs),
                 ]
               | LetLine => [Node.span([Attr.classes(["LetLine"])], vs)]

               | Padding => [Node.span([Attr.classes(["Padding"])], vs)]
               | Indent => [Node.span([Attr.classes(["Indent"])], vs)]

               | HoleLabel(_) => [
                   Node.span([Attr.classes(["HoleLabel"])], vs),
                 ]
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
                   switch (TermShape.err_status(term_shape)) {
                   | NotInHole => []
                   | InHole(_) => [
                       Vdom.Node.div(
                         [Vdom.Attr.classes(["code-err-hole"])],
                         [Decoration.err_hole_view(~font_metrics, shape)],
                       ),
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
  Node.div([Attr.classes(["code", "presentation"])], vs);
};
