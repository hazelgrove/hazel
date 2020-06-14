module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
open ViewUtil;

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
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    model.measurements.measurements && model.measurements.uhcode_view,
    () => {
      open Vdom;

      let rec go: UHLayout.t => _ =
        fun
        | Text(s) =>
          if (List.length(String.split_on_char('\\', s)) <= 1) {
            [Node.text(s)];
          } else if (StringUtil.is_empty(s)) {
            [];
          } else if (String.length(s) == 1) {
            if (s == "\\") {
              [
                Node.span([Attr.classes(["InvalidSeq"])], [Node.text(s)]),
              ];
            } else {
              [Node.text(s)];
            };
          } else {
            switch (String.sub(s, 0, 1)) {
            | "\\" =>
              switch (String.sub(s, 1, 1)) {
              | "b"
              | "t"
              | "r"
              | "n"
              | " "
              | "\\"
              | "\""
              | "\'" => [
                  Node.span(
                    [Attr.classes(["ValidSeq"])],
                    [Node.text(String.sub(s, 0, 2))],
                  ),
                  ...go(
                       Pretty.Layout.Text(
                         String.sub(s, 2, String.length(s) - 2),
                       ),
                     ),
                ]
              | "o" =>
                if (String.length(s) >= 5) {
                  let ch1 = s.[2];
                  let ch2 = s.[3];
                  let ch3 = s.[4];
                  if ((ch1 >= '0' && ch1 <= '7')
                      && (ch2 >= '0' && ch2 <= '7')
                      && ch3 >= '0'
                      && ch3 <= '7') {
                    if (ch1 <= '3') {
                      [
                        Node.span(
                          [Attr.classes(["ValidSeq"])],
                          [Node.text(String.sub(s, 0, 5))],
                        ),
                        ...go(
                             Text(String.sub(s, 5, String.length(s) - 5)),
                           ),
                      ];
                    } else {
                      [
                        Node.span(
                          [Attr.classes(["InvalidSeq"])],
                          [Node.text(String.sub(s, 0, 5))],
                        ),
                        ...go(
                             Text(String.sub(s, 5, String.length(s) - 5)),
                           ),
                      ];
                    };
                  } else {
                    [
                      Node.span(
                        [Attr.classes(["InvalidSeq"])],
                        [Node.text(String.sub(s, 0, 2))],
                      ),
                      ...go(Text(String.sub(s, 2, String.length(s) - 2))),
                    ];
                  };
                } else {
                  [
                    Node.span(
                      [Attr.classes(["InvalidSeq"])],
                      [Node.text(String.sub(s, 0, 2))],
                    ),
                    ...go(Text(String.sub(s, 2, String.length(s) - 2))),
                  ];
                }
              | "x" =>
                if (String.length(s) >= 4) {
                  let ch1 = Char.lowercase_ascii(s.[2]);
                  let ch2 = Char.lowercase_ascii(s.[3]);
                  if ((ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f')
                      && (ch2 >= '0' && ch2 <= '9' || ch2 >= 'a' && ch2 <= 'f')) {
                    [
                      Node.span(
                        [Attr.classes(["ValidSeq"])],
                        [Node.text(String.sub(s, 0, 4))],
                      ),
                      ...go(Text(String.sub(s, 4, String.length(s) - 4))),
                    ];
                  } else {
                    [
                      Node.span(
                        [Attr.classes(["InvalidSeq"])],
                        [Node.text(String.sub(s, 0, 2))],
                      ),
                      ...go(Text(String.sub(s, 2, String.length(s) - 2))),
                    ];
                  };
                } else {
                  [
                    Node.span(
                      [Attr.classes(["InvalidSeq"])],
                      [Node.text(String.sub(s, 0, 2))],
                    ),
                    ...go(Text(String.sub(s, 2, String.length(s) - 2))),
                  ];
                }
              | _ =>
                let ch1 = s.[1];
                if (String.length(s) >= 4) {
                  let ch2 = s.[2];
                  let ch3 = s.[3];
                  if ((ch1 >= '0' && ch1 <= '9')
                      && (ch2 >= '0' && ch2 <= '9')
                      && ch3 >= '0'
                      && ch3 <= '9') {
                    if (int_of_string(String.sub(s, 1, 3)) < 256) {
                      [
                        Node.span(
                          [Attr.classes(["ValidSeq"])],
                          [Node.text(String.sub(s, 0, 4))],
                        ),
                        ...go(
                             Text(String.sub(s, 4, String.length(s) - 4)),
                           ),
                      ];
                    } else {
                      [
                        Node.span(
                          [Attr.classes(["InvalidSeq"])],
                          [Node.text(String.sub(s, 0, 4))],
                        ),
                        ...go(
                             Text(String.sub(s, 4, String.length(s) - 4)),
                           ),
                      ];
                    };
                  } else {
                    [
                      Node.span(
                        [Attr.classes(["InvalidSeq"])],
                        [Node.text(String.sub(s, 0, 2))],
                      ),
                      ...go(Text(String.sub(s, 2, String.length(s) - 2))),
                    ];
                  };
                } else {
                  [
                    Node.span(
                      [Attr.classes(["InvalidSeq"])],
                      [Node.text(String.sub(s, 0, 2))],
                    ),
                    ...go(Text(String.sub(s, 2, String.length(s) - 2))),
                  ];
                };
              }
            | _ => [
                Node.text(String.sub(s, 0, 1)),
                ...go(Text(String.sub(s, 1, String.length(s) - 1))),
              ]
            };
          }
        | Linebreak => [Node.br([])]
        | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
        | Cat(l1, l2) => go(l1) @ go(l2)

        | Annot(Step(_) | EmptyLine | SpaceOp, l) => go(l)

        | Annot(Token({shape, _}), l) => {
            let clss =
              switch (shape) {
              | Text(_) => ["code-text"]
              | Op => ["code-op"]
              | Delim(_) => ["code-delim"]
              };
            [Node.span([Attr.classes(clss)], go(l))];
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
        | Annot(Indent, l) => [
            Node.span([Attr.classes(["Indent"])], go(l)),
          ]

        | Annot(HoleLabel({len}), l) => {
            let width = Css_gen.width(`Ch(float_of_int(len)));
            [
              Node.span(
                [Vdom.Attr.style(width), Attr.classes(["HoleLabel"])],
                [Node.span([Attr.classes(["HoleNumber"])], go(l))],
              ),
            ];
          }
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

      let children =
        switch (caret_pos) {
        | None => go(l)
        | Some((row, col)) =>
          let x = float_of_int(col) *. model.font_metrics.col_width;
          let y = float_of_int(row) *. model.font_metrics.row_height;
          let caret = caret_from_pos(x, y);
          [caret, ...go(l)];
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
