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

  let shape_of_layout = (~start: int, l: UHLayout.t): shape => {
    let col = ref(start);
    let rec go = (~indent, l: UHLayout.t) => {
      let go' = go(~indent);
      switch (l) {
      | Linebreak =>
        let eol = col^;
        col := indent;
        {hd: (eol, eol), tl: [indent]};
      | Text(s) =>
        let col_before = col^;
        col := col^ + StringUtil.utf8_length(s);
        let col_after = col^;
        {hd: (col_before - indent, col_after - indent), tl: []};
      | Align(l) => go(~indent=col^, l)
      | Cat(l1, l2) =>
        let s1 = go'(l1);
        let s2 = go'(l2);
        switch (s1.tl) {
        | [] => {hd: (fst(s1.hd), snd(s2.hd)), tl: s2.tl}
        | [_, ..._] => {hd: s1.hd, tl: s1.tl @ [snd(s2.hd), ...s2.tl]}
        };
      | Annot(_, l) => go'(l)
      };
    };
    go(~indent=0, l);
  };

  type outline_segment =
    | VLine(float)
    | HLine(float)
    | Corner(corner, float)
  and corner =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight;

  let outline_path = (~font_metrics: FontMetrics.t, ~corner_radius, shape) => {
    let row_height = font_metrics.row_height;
    let col_width = font_metrics.col_width;
    let half_row_height = row_height /. 2.0;

    let vline_down = VLine(half_row_height -. corner_radius);
    let vline_up = VLine((-1.0) *. (half_row_height -. corner_radius));

    let hline_left = len =>
      HLine(
        (-1.0) *. (float_of_int(len) *. col_width -. 2.0 *. corner_radius),
      );
    let hline_right = len =>
      HLine(float_of_int(len) *. col_width -. 2.0 *. corner_radius);

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
             [VLine((-1.0) *. row_height)];
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
             [VLine(row_height)];
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
