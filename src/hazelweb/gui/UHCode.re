module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

let decoration_cls: UHDecorationShape.t => string =
  fun
  | ErrHole => "err-hole"
  | VarErrHole => "var-err-hole"
  | VarUse => "var-use"
  | CurrentTerm => "current-term"
  | AssertStatus(_) => "assert-result";

let decoration_view =
    (
      dshape: UHDecorationShape.t,
      dpaths: UHDecorationPaths.t,
      corner_radii,
      shape,
      sort,
      font_metrics: FontMetrics.t,
    ) => {
  switch (dshape) {
  | AssertStatus(report) =>
    let view = UHDecoration.AssertStatus.view(report, font_metrics);
    (Decoration_common.Div, view);
  | ErrHole =>
    let contains_current_term = Option.is_some(dpaths.current_term);
    let view =
      UHDecoration.ErrHole.view(~contains_current_term, ~corner_radii);
    (Decoration_common.Svg, view);
  | VarErrHole =>
    let contains_current_term = Option.is_some(dpaths.current_term);
    let view =
      UHDecoration.VarErrHole.view(~contains_current_term, ~corner_radii);
    (Decoration_common.Svg, view);
  | VarUse =>
    let view = UHDecoration.VarUse.view(~corner_radii);
    (Decoration_common.Svg, view);
  | CurrentTerm =>
    let view = UHDecoration.CurrentTerm.view(~corner_radii, ~sort, ~shape);
    (Decoration_common.Svg, view);
  };
};

let decoration_views =
    (~font_metrics: FontMetrics.t, dpaths: UHDecorationPaths.t, l: UHLayout.t)
    : list(Vdom.Node.t) => {
  let rec go =
          (
            ~tl: list(Vdom.Node.t)=[], // tail-recursive
            ~indent=0, // indentation level of `m`
            ~start=MeasuredPosition.zero, // start position of `m`
            dpaths: UHDecorationPaths.t, // paths to decorations within `m`
            m: UHMeasuredLayout.t,
          )
          : list(Vdom.Node.t) => {
    let go' = go(~indent, ~start);
    switch (m.layout) {
    | Linebreak
    | Text(_) => tl
    | Cat(m1, m2) =>
      let mid_row = start.row + MeasuredLayout.height(m1) - 1;
      let mid_col = {
        let (leading, MeasuredLayout.{width: last_width, _}) =
          ListUtil.split_last(m1.metrics);
        let offset =
          switch (leading) {
          | [] => start.col
          | [_, ..._] => indent
          };
        offset + last_width;
      };
      let mid_tl =
        go(~tl, ~indent, ~start={row: mid_row, col: mid_col}, dpaths, m2);
      go'(~tl=mid_tl, dpaths, m1);
    | Align(m) => go(~tl, ~indent=start.col, ~start, dpaths, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) =>
        let stepped = UHDecorationPaths.take_step(step, dpaths);
        UHDecorationPaths.is_empty(stepped) ? tl : go'(~tl, stepped, m);
      | Term({shape, sort, _}) =>
        let offset = start.col - indent;
        let current_vs =
          UHDecorationPaths.current(shape, dpaths)
          |> List.map((dshape: UHDecorationShape.t) => {
               let cls = decoration_cls(dshape);
               let corner_radii =
                 Decoration_common.corner_radii(font_metrics);
               let height = MeasuredLayout.height(m);
               let width = MeasuredLayout.width(~offset, m);
               let origin = MeasuredPosition.{row: start.row, col: indent};
               let (container_type, view) =
                 decoration_view(
                   dshape,
                   dpaths,
                   corner_radii,
                   shape,
                   sort,
                   font_metrics,
                 );
               Decoration_common.container(
                 ~container_type,
                 ~font_metrics,
                 ~origin,
                 ~height,
                 ~width,
                 ~cls,
                 [view((offset, m))],
               );
             });
        go'(~tl=current_vs @ tl, dpaths, m);
      | _ => go'(~tl, dpaths, m)
      }
    };
  };

  go(dpaths, UHMeasuredLayout.mk(l));
};

let view_of_cursor_inspector =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~assert_inspector,
      (steps, cursor): CursorPath.t,
      cursor_inspector: CursorInspectorModel.t,
      cursor_info: CursorInfo.t,
      l: UHLayout.t,
    ) => {
  let cursor =
    switch (cursor) {
    | OnText(_) => CursorPosition.OnText(0)
    | OnDelim(index, _) => CursorPosition.OnDelim(index, Before)
    | OnOp(_) => CursorPosition.OnOp(Before)
    };
  let m = UHMeasuredLayout.mk(l);
  let cursor_pos =
    UHMeasuredLayout.caret_position_of_path((steps, cursor), m)
    |> OptUtil.get(() => failwith("could not find caret"));
  let cursor_x = float_of_int(cursor_pos.col) *. font_metrics.col_width;
  let cursor_y = float_of_int(cursor_pos.row) *. font_metrics.row_height;
  CursorInspector.view(
    ~assert_inspector,
    ~inject,
    ~loc=(cursor_x, cursor_y),
    cursor_inspector,
    cursor_info,
  );
};

let key_handlers = (~inject, ~cursor_info: CursorInfo.t): list(Vdom.Attr.t) => {
  open Vdom;
  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let model_action: option(ModelAction.t) =
        KeyComboAction.get_model_action(cursor_info, evt);
      switch (model_action) {
      | Some(model_action) => prevent_stop_inject(model_action)
      | None => Event.Ignore
      };
    }),
  ];
};

let box_table: WeakMap.t(UHBox.t, list(Vdom.Node.t)) = WeakMap.mk();
let rec view_of_box = (box: UHBox.t): list(Vdom.Node.t) => {
  Vdom.(
    switch (WeakMap.get(box_table, box)) {
    | Some(vs) => vs
    | None =>
      switch (box) {
      | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
      | HBox(boxes) => boxes |> List.map(view_of_box) |> List.flatten
      | VBox(boxes) =>
        let vs =
          boxes
          |> List.map(view_of_box)
          |> ListUtil.join([Node.br([])])
          |> List.flatten;
        [Node.div([Attr.classes(["VBox"])], vs)];
      | Annot(annot, box) =>
        let vs = view_of_box(box);
        switch (annot) {
        | Token({shape, _}) =>
          let clss =
            switch (shape) {
            | Text => ["code-text"]
            | Op => ["code-op"]
            | Delim(_) => ["code-delim"]
            };
          [Node.span([Attr.classes(clss)], vs)];
        | HoleLabel({len}) =>
          let width = Css_gen.width(`Ch(float_of_int(len)));
          [
            Node.span(
              [Attr.style(width), Attr.classes(["HoleLabel"])],
              [Node.span([Attr.classes(["HoleNumber"])], vs)],
            ),
          ];
        | UserNewline => [Node.span([Attr.classes(["UserNewline"])], vs)]
        | CommentLine => [Node.span([Attr.classes(["CommentLine"])], vs)]
        | _ => vs
        };
      }
    }
  );
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~cursor_inspector: CursorInspectorModel.t,
      ~program: Program.t,
      ~assert_inspector,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    settings.performance.measure && settings.performance.uhcode_view,
    () => {
      open Vdom;

      let l = Program.get_layout(~settings, program);
      let code_text = view_of_box(UHBox.mk(l));
      let decorations = {
        let dpaths = Program.get_decoration_paths(program);
        decoration_views(~font_metrics, dpaths, l);
      };
      let caret = {
        let caret_pos = Program.get_caret_position(~settings, program);
        program.is_focused
          ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];
      };
      let cursor_inspector =
        if (program.is_focused && cursor_inspector.visible) {
          let path = Program.get_path(program);
          let ci = Program.get_cursor_info(program);
          [
            view_of_cursor_inspector(
              ~assert_inspector,
              ~inject,
              ~font_metrics,
              path,
              cursor_inspector,
              ci,
              l,
            ),
          ];
        } else {
          [];
        };

      let key_handlers =
        program.is_focused
          ? key_handlers(
              ~inject,
              ~cursor_info=Program.get_cursor_info(program),
            )
          : [];

      let click_handler = evt => {
        let container_rect =
          JSUtil.force_get_elem_by_id(ViewUtil.code_root_id)##getBoundingClientRect;
        let (target_x, target_y) = (
          float_of_int(evt##.clientX),
          float_of_int(evt##.clientY),
        );
        let caret_pos =
          MeasuredPosition.{
            row:
              Float.to_int(
                (target_y -. container_rect##.top) /. font_metrics.row_height,
              ),
            col:
              Float.to_int(
                Float.round(
                  (target_x -. container_rect##.left) /. font_metrics.col_width,
                ),
              ),
          };
        inject(ModelAction.MoveAction(Click(caret_pos)));
      };

      Node.div(
        [
          Attr.id(ViewUtil.code_root_id),
          Attr.classes(["code", "presentation"]),
          // need to use mousedown instead of click to fire
          // (and move caret) before cell focus event handler
          Attr.on_mousedown(click_handler),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_focus(_ => inject(ModelAction.FocusCell)),
          Attr.on_blur(_ => inject(ModelAction.BlurCell)),
          ...key_handlers,
        ],
        caret
        @ cursor_inspector
        @ [Node.span([Attr.classes(["code"])], code_text), ...decorations],
      );
    },
  );
};
