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

type box = {
  height: int,
  // Max number of columns used. For the first box, add to offset to get last
  // column used (relative to innermost/nearest containing Align).  For any
  // other box, `width` is the last column used (again relative to
  // innermost/nearest containing Align).
  width: int,
};
type metrics = {
  // Column offset of the first box
  offset: int,
  boxes: list(box),
  // TODO: add last_offset so we can support aligns that are not padded on the right
};

let annot =
    (annot: UHAnnot.t, vs: list(Vdom.Node.t), l: UHLayout.t)
    : list(Vdom.Node.t) => {
  Vdom.(
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
    }
  );
};

let table: WeakMap.t(UHLayout.t, IntMap.t((metrics, list(Vdom.Node.t)))) =
  WeakMap.mk();

let rec go = (~offset: int, l: UHLayout.t): (metrics, list(Vdom.Node.t)) => {
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
      let (metrics, vs) = go(~offset=0, l);
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
      let (metrics1, vs1) = go(~offset, l1);
      let (leading, last) = ListUtil.split_last(metrics1.boxes);
      let (metrics2, vs2) = {
        // If metrics1 is only one box, then width of the last box is added
        // to offset, otherwise it is from offset 0.
        //
        // TODO: This assumes all aligns are padded on the right.
        let start_offset = leading == [] ? offset : 0;
        let offset = last.width + start_offset;
        go(~offset, l2);
      };
      let (first, trailing) = ListUtil.split_first(metrics2.boxes);
      let mid_box = {
        height: max(last.height, first.height),
        width: last.width + first.width,
      };
      let metrics = {offset, boxes: leading @ [mid_box, ...trailing]};
      (metrics, vs1 @ vs2);
    | Annot(ann, l) =>
      let (metrics, vs) = go(~offset, l);
      (metrics, annot(ann, vs, l));
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

      let (_, vs) = go(~offset=0, l);
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
