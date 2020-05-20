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
    (
      ~model: Model.t,
      ~inject: Update.Action.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      l: UHLayout.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    model.measurements.measurements && model.measurements.uhcode_view,
    () => {
      open Vdom;

      let rec go = (layout: UHLayout.t, ci_shown: bool): list(Node.t) => {
        switch (layout) {
        | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
        | Linebreak => [Node.br([])]
        | Align(l) => [
            Node.div([Attr.classes(["Align"])], go(l, ci_shown)),
          ]
        | Cat(l1, l2) => go(l1, ci_shown) @ go(l2, ci_shown)

        | Annot(Step(_) | EmptyLine | SpaceOp, l) => go(l, ci_shown)

        | Annot(Token({shape, len, has_cursor}), l) =>
          let clss =
            switch (shape) {
            | Text => ["code-text"]
            | Op => ["code-op"]
            | Delim(_) => ["code-delim"]
            };
          let children =
            switch (has_cursor) {
            | None => go(l, ci_shown)
            | Some(j) => [
                caret_from_left(
                  len == 0
                    ? 0.0 : float_of_int(j) /. float_of_int(len) *. 100.0,
                ),
                ...go(l, true),
              ]
            };
          switch (has_cursor, ci_shown) {
          | (_, true)
          | (None, _) => [Node.span([Attr.classes(clss)], children)]
          | (Some(_), false) => [
              Node.span([Attr.classes(clss)], children),
              CursorInspector.view(model),
            ]
          };
        | Annot(DelimGroup, l) => [
            Node.span([Attr.classes(["DelimGroup"])], go(l, ci_shown)),
          ]
        | Annot(LetLine, l) => [
            Node.span([Attr.classes(["LetLine"])], go(l, ci_shown)),
          ]

        | Annot(Padding, l) => [
            Node.span([Attr.classes(["Padding"])], go(l, ci_shown)),
          ]
        | Annot(Indent, l) => [
            Node.span([Attr.classes(["Indent"])], go(l, ci_shown)),
          ]

        | Annot(HoleLabel(_), l) => [
            Node.span([Attr.classes(["HoleLabel"])], go(l, ci_shown)),
          ]
        | Annot(UserNewline, l) => [
            Node.span([Attr.classes(["UserNewline"])], go(l, ci_shown)),
          ]

        | Annot(OpenChild({is_inline}), l) => [
            Node.span(
              [Attr.classes(["OpenChild", is_inline ? "Inline" : "Para"])],
              go(l, ci_shown),
            ),
          ]
        | Annot(ClosedChild({is_inline}), l) => [
            Node.span(
              [Attr.classes(["ClosedChild", is_inline ? "Inline" : "Para"])],
              go(l, ci_shown),
            ),
          ]

        | Annot(Term({has_cursor, shape, sort}), l) =>
          let show_ci =
            switch (has_cursor, UHLayout.has_para_OpenChild(l), shape) {
            | (false, _, _) => ci_shown
            | (true, true, _)
            | (true, _, Case(_))
            | (true, _, SubBlock(_)) => true
            | (true, _, Rule)
            | (true, _, Var(_))
            | (true, _, Operand(_))
            | (true, _, BinOp(_))
            | (true, _, NTuple(_)) => false
            };
          let node =
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
              go(l, show_ci),
            );
          if (show_ci && !ci_shown) {
            [node, CursorInspector.view(model)];
          } else {
            [node];
          };
        };
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
        go(l, false),
      );
    },
  );
};
