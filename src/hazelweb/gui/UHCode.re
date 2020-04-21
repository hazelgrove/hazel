module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

module Decoration = {
  type shape = {
    // start and end of first line
    // (where n means n columns from
    // start of current indentation)
    hd: (int, int),
    // ends of all other lines
    tl: list(int),
  };

  let shape_of_layout = (~start: int) =>
    UHLayout.pos_fold(
      ~col_start=start,
      ~linebreak=pos => {hd: (pos.col, pos.col), tl: [pos.indent]},
      ~text=
        (pos, s) =>
          {hd: (pos.col, pos.col + StringUtil.utf8_length(s)), tl: []},
      ~align=(_, shape) => shape,
      ~cat=
        (_, shape1, shape2) =>
          switch (shape1.tl) {
          | [] => {hd: (fst(shape1.hd), snd(shape2.hd)), tl: shape2.tl}
          | [_, ..._] => {
              hd: shape1.hd,
              tl: shape1.tl @ [snd(shape2.hd), ...shape2.tl],
            }
          },
      ~annot=(_, _, shape) => shape,
    );

  type path_segment =
    | VLine(float)
    | HLine(float)
    | Corner(corner, float)
  and corner =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight;

  let outline_path = (~corner_radius, shape) => {
    let vline_down = VLine(0.5 -. corner_radius);
    let vline_up = VLine(Float.neg(0.5 -. corner_radius));

    let hline_left = len =>
      HLine(Float.neg(Float.of_int(len) -. 2.0 *. corner_radius));
    let hline_right = len =>
      HLine(float_of_int(len) -. 2.0 *. corner_radius);

    let corner_TopLeft = Corner(TopLeft, corner_radius);
    let corner_TopRight = Corner(TopRight, corner_radius);
    let corner_BottomLeft = Corner(BottomLeft, corner_radius);
    let corner_BottomRight = Corner(BottomRight, corner_radius);

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

    let hd_width = snd(shape.hd) - fst(shape.hd);
    switch (ListUtil.split_last(shape.tl)) {
    | None => top_cap(hd_width) @ bottom_cap(hd_width)
    | Some((_, last)) =>
      top_cap(hd_width)
      @ right_edge([snd(shape.hd), ...shape.tl])
      @ bottom_cap(last)
      @ left_edge(
          ListUtil.replicate(List.length(shape.tl), 0) @ [fst(shape.hd)],
        )
    };
  };

  let path_view = (~start, ~width, path: list(path_segment)) => {
    let d_path_segments =
      path
      |> List.map(
           fun
           | VLine(len) => "v " ++ string_of_float(len)
           | HLine(len) => "h " ++ string_of_float(len)
           | Corner(corner, radius) => {
               let (dx, dy) =
                 switch (corner) {
                 | TopLeft => (radius, Float.neg(radius))
                 | TopRight => (radius, radius)
                 | BottomRight => (Float.neg(radius), radius)
                 | BottomLeft => (Float.neg(radius), Float.neg(radius))
                 };
               StringUtil.sep([
                 "a",
                 string_of_float(radius),
                 string_of_float(radius),
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
        "svg",
        [Attr.create("viewBox", "")],
        [
          Node.create_svg(
            "path",
            [
              Attr.create("fill", "none"),
              Attr.create(
                "d",
                StringUtil.sep([
                  "m " ++ string_of_int(start) ++ " 0",
                  ...d_path_segments,
                ]),
              ),
            ],
            [],
          ),
        ],
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

let view =
    (~inject as _: Update.Action.t => Vdom.Event.t, l: UHLayout.t)
    : Vdom.Node.t => {
  open Vdom;
  let rec go: UHLayout.t => _ =
    fun
    | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
    | Linebreak => [Node.br([])]
    | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
    | Cat(l1, l2) => go(l1) @ go(l2)

    | Annot(Step(_) | EmptyLine | SpaceOp, l) => go(l)

    | Annot(Token({shape, len, has_cursor}), l) => {
        let clss =
          switch (shape) {
          | Text => ["code-text"]
          | Op => ["code-op"]
          | Delim(_) => ["code-delim"]
          };
        let children =
          switch (has_cursor) {
          | None => go(l)
          | Some(j) => [
              caret_from_left(
                len == 0
                  ? 0.0 : float_of_int(j) /. float_of_int(len) *. 100.0,
              ),
              ...go(l),
            ]
          };
        [Node.span([Attr.classes(clss)], children)];
      }

    | Annot(DelimGroup, l) => [
        Node.span([Attr.classes(["DelimGroup"])], go(l)),
      ]
    | Annot(LetLine, l) => [
        Node.span([Attr.classes(["LetLine"])], go(l)),
      ]

    | Annot(Padding, l) => [
        Node.span([Attr.classes(["Padding"])], go(l)),
      ]
    | Annot(Indent, l) => [Node.span([Attr.classes(["Indent"])], go(l))]

    | Annot(HoleLabel(_), l) => [
        Node.span([Attr.classes(["HoleLabel"])], go(l)),
      ]
    | Annot(UserNewline, l) => [
        Node.span([Attr.classes(["UserNewline"])], go(l)),
      ]

    | Annot(OpenChild({is_inline}), l) => [
        Node.span(
          [Attr.classes(["OpenChild", is_inline ? "Inline" : "Para"])],
          go(l),
        ),
      ]
    | Annot(ClosedChild({is_inline}), l) => [
        Node.span(
          [Attr.classes(["ClosedChild", is_inline ? "Inline" : "Para"])],
          go(l),
        ),
      ]

    | Annot(Term({has_cursor, shape, sort}), l) => [
        Node.span(
          [
            Attr.classes(
              List.concat([
                ["Term"],
                cursor_clss(has_cursor),
                sort_clss(sort),
                shape_clss(shape),
                open_child_clss(
                  l |> UHLayout.has_inline_OpenChild,
                  l |> UHLayout.has_para_OpenChild,
                ),
                has_child_clss(l |> UHLayout.has_child),
              ]),
            ),
          ],
          go(l),
        ),
      ];

  Node.div([Attr.classes(["code", "presentation"])], go(l));
};
