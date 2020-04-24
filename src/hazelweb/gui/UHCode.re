module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;
open Sexplib.Std;

module Decoration = {
  [@deriving sexp]
  type shape = {
    indent: int,
    // start col of first line
    hd_start: int,
    // end of first line
    hd_end: int,
    // ends of all other lines
    tl_ends: list(int),
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

  let corner_radius = 4.0;

  let outline_path = (~corner_radii, shape) => {
    let (rx, ry) = corner_radii;

    let vline_down = VLine(0.5 -. ry);
    let vline_up = VLine(Float.neg(0.5 -. ry));

    let hline_left = len =>
      HLine(Float.neg(Float.of_int(len) -. 2.0 *. rx));
    let hline_right = len => HLine(float_of_int(len) -. 2.0 *. rx);

    let corner_TopLeft = d => Corner(TopLeft, d, corner_radii);
    let corner_TopRight = d => Corner(TopRight, d, corner_radii);
    let corner_BottomLeft = d => Corner(BottomLeft, d, corner_radii);
    let corner_BottomRight = d => Corner(BottomRight, d, corner_radii);

    let top_cap = width => [
      vline_up,
      corner_TopLeft(CW),
      hline_right(width),
      corner_TopRight(CW),
      vline_down,
    ];
    let bottom_cap = width => [
      vline_down,
      corner_BottomRight(CW),
      hline_left(width),
      corner_BottomLeft(CW),
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
               corner_TopRight(CCW),
               hline_left(col1 - col2),
               corner_BottomLeft(CW),
               vline_up,
             ];
           } else {
             [
               vline_up,
               corner_TopLeft(CW),
               hline_right(col2 - col1),
               corner_BottomRight(CCW),
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
               corner_BottomRight(CW),
               hline_left(col1 - col2),
               corner_TopLeft(CCW),
               vline_down,
             ];
           } else {
             [
               vline_down,
               corner_BottomLeft(CCW),
               hline_right(col2 - col1),
               corner_TopRight(CW),
               vline_down,
             ];
           }
         )
      |> List.flatten;

    let hd_width = shape.hd_end - shape.hd_start;
    let tl_widths = shape.tl_ends |> List.map(tl_end => tl_end - shape.indent);
    switch (ListUtil.split_last(tl_widths)) {
    | None => top_cap(hd_width) @ bottom_cap(hd_width)
    | Some((_, last_width)) =>
      top_cap(hd_width)
      @ right_edge([hd_width, ...tl_widths])
      @ bottom_cap(last_width)
      @ left_edge(
          ListUtil.replicate(List.length(tl_widths), 0)
          @ [shape.hd_start - shape.indent],
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
                 "1",
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
              "m " ++ string_of_int(rel_hd_start) ++ " 0",
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

    let hd_width = shape.hd_end - shape.hd_start;
    let tl_widths = shape.tl_ends |> List.map(tl_end => tl_end - shape.indent);

    let num_rows = 1 + List.length(tl_widths);
    let height = float_of_int(num_rows) *. row_height;

    let num_cols = tl_widths |> List.fold_left(max, hd_width);
    let width = float_of_int(num_cols) *. col_width;

    let path_view = {
      let corner_radii = (
        corner_radius /. col_width,
        corner_radius /. row_height,
      );
      shape
      |> outline_path(~corner_radii)
      |> path_view(~rel_hd_start=shape.hd_start - shape.indent);
    };

    Vdom.(
      Node.create_svg(
        "svg",
        [
          Attr.create(
            "viewBox",
            Printf.sprintf("0 0 %d %d", num_cols, num_rows),
          ),
          Attr.create("width", string_of_float(width) ++ "0px"),
          Attr.create("height", string_of_float(height) ++ "0px"),
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
                 indent: pos.indent,
                 hd_start: pos.col,
                 hd_end: pos.col,
                 tl_ends: [pos.indent],
               },
             ),
         ~text=
           (pos, s) =>
             (
               Text(s),
               StringUtil.is_empty(s) ? [] : [Node.text(s)],
               {
                 indent: pos.indent,
                 hd_start: pos.col,
                 hd_end: pos.col + StringUtil.utf8_length(s),
                 tl_ends: [],
               },
             ),
         ~align=
           (pos, (l, vs, shape)) =>
             (
               Align(l),
               [Node.div([Attr.classes(["Align"])], vs)],
               {...shape, indent: pos.indent},
             ),
         ~cat=
           (_, (l1, vs1, shape1), (l2, vs2, shape2)) =>
             (
               Cat(l1, l2),
               vs1 @ vs2,
               switch (ListUtil.split_last(shape1.tl_ends)) {
               | None => {...shape2, hd_start: shape1.hd_start}
               | Some((tl_leading, _)) => {
                   ...shape1,
                   tl_ends: tl_leading @ [shape2.hd_end, ...shape2.tl_ends],
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
