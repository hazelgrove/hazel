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

let clss_of_list_err: ListErrStatus.t => list(cls) =
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
  | Invalid => ["Invalid"]
  | Case({err}) => ["Case", ...clss_of_case_err(err)]
  | List({err}) => ["List", ...clss_of_list_err(err)]
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
      ~measure: bool,
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~caret_pos: option((int, int)),
      l: UHLayout.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    measure,
    () => {
      open Vdom;

      let rec go: UHLayout.t => _ =
        fun
        | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
        | Linebreak => [Node.br([])]
        | Align(l) => [Node.div([Attr.classes(["Align"])], go(l))]
        | Cat(l1, l2) => go(l1) @ go(l2)

        | Annot(Step(_) | EmptyLine | SpaceOp, l) => go(l)

        | Annot(Token({shape, _}), l) => {
            let clss =
              switch (shape) {
              | Text => ["code-text"]
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

        | Annot(CommentLine, l) => [
            Node.span([Attr.classes(["CommentLine"])], go(l)),
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
          let x = float_of_int(col) *. font_metrics.col_width;
          let y = float_of_int(row) *. font_metrics.row_height;
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
            inject(ModelAction.MoveAction(Click(row_col)));
          }),
        ],
        children,
      );
    },
  );
};
