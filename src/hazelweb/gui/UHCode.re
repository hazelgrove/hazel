module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

let decoration_views =
    (~font_metrics: FontMetrics.t, ds: Decorations.t, l: UHLayout.t)
    : list(Vdom.Node.t) => {
  let rec go =
          (
            ~tl: list(Vdom.Node.t),
            ~indent: int,
            ~start: MeasuredPosition.t,
            ds: Decorations.t,
            m: UHMeasuredLayout.t,
          )
          : list(Vdom.Node.t) => {
    let go' = go(~indent, ~start);
    switch (m.layout) {
    | Linebreak
    | Text(_) => tl
    | Cat(m1, m2) =>
      let mid_row = {
        let height1 =
          m1.metrics
          |> List.map((box: MeasuredLayout.box) => box.height)
          |> List.fold_left((+), 0);
        start.row + height1 - 1;
      };
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
        go(~tl, ~indent, ~start={row: mid_row, col: mid_col}, ds, m2);
      go'(~tl=mid_tl, ds, m1);
    | Align(m) => go(~tl, ~indent=start.col, ~start, ds, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) =>
        let stepped = Decorations.take_step(step, ds);
        Decorations.is_empty(stepped) ? tl : go'(~tl, stepped, m);
      | Term({shape, sort, _}) =>
        let current_vs =
          Decorations.current(sort, shape, ds)
          |> List.map(
               UHCodeDecoration.view(
                 ~font_metrics,
                 ~origin={row: start.row, col: indent},
                 ~offset=start.col - indent,
                 // ~shape,
                 ~subject=m,
               ),
             );
        go'(~tl=current_vs @ tl, ds, m);
      | _ => go'(~tl, ds, m)
      }
    };
  };
  go(
    ~tl=[],
    ~indent=0,
    ~start={row: 0, col: 0},
    ds,
    UHMeasuredLayout.mk(l),
  );
};

let key_handlers =
    (~inject, ~is_mac: bool, ~cursor_info: CursorInfo_common.t)
    : list(Vdom.Attr.t) => {
  open Vdom;
  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      switch (MoveKey.of_key(Key.get_key(evt))) {
      | Some(move_key) =>
        prevent_stop_inject(ModelAction.MoveAction(Key(move_key)))
      | None =>
        switch (HazelKeyCombos.of_evt(evt)) {
        | Some(Ctrl_Z) =>
          if (is_mac) {
            Event.Ignore;
          } else {
            prevent_stop_inject(ModelAction.Undo);
          }
        | Some(Meta_Z) =>
          if (is_mac) {
            prevent_stop_inject(ModelAction.Undo);
          } else {
            Event.Ignore;
          }
        | Some(Ctrl_Shift_Z) =>
          if (is_mac) {
            Event.Ignore;
          } else {
            prevent_stop_inject(ModelAction.Redo);
          }
        | Some(Meta_Shift_Z) =>
          if (is_mac) {
            prevent_stop_inject(ModelAction.Redo);
          } else {
            Event.Ignore;
          }
        | Some(kc) =>
          prevent_stop_inject(
            ModelAction.EditAction(KeyComboAction.get(cursor_info, kc)),
          )
        | None =>
          switch (HazelKeyCombos.of_evt(evt)) {
          | Some(Ctrl_Z) => prevent_stop_inject(ModelAction.Undo)
          | Some(Ctrl_Shift_Z) => prevent_stop_inject(ModelAction.Redo)
          | Some(kc) =>
            prevent_stop_inject(
              ModelAction.EditAction(KeyComboAction.get(cursor_info, kc)),
            )
          | None =>
            switch (JSUtil.is_single_key(evt)) {
            | None => Event.Ignore
            | Some(single_key) =>
              prevent_stop_inject(
                ModelAction.EditAction(
                  Construct(SChar(JSUtil.single_key_string(single_key))),
                ),
              )
            }
          }
        }
      }
    }),
  ];
};

let rec view_of_box = (box: UHBox.t): list(Vdom.Node.t) => {
  Vdom.(
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
            [Vdom.Attr.style(width), Attr.classes(["HoleLabel"])],
            [Node.span([Attr.classes(["HoleNumber"])], vs)],
          ),
        ];
      | UserNewline => [Node.span([Attr.classes(["UserNewline"])], vs)]
      | CommentLine => [Node.span([Attr.classes(["CommentLine"])], vs)]
      | _ => vs
      };
    }
  );
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~measure: bool,
      ~is_mac: bool,
      program: Program.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    measure,
    () => {
      open Vdom;

      let l =
        Program.get_layout(
          ~measure_program_get_doc=false,
          ~measure_layoutOfDoc_layout_of_doc=false,
          ~memoize_doc=false,
          program,
        );

      let code_text = view_of_box(Pretty.Box.mk(l));
      let decorations = {
        let ds = Program.get_decorations(program);
        decoration_views(~font_metrics, ds, l);
      };
      let caret = {
        let caret_pos =
          Program.get_caret_position(
            ~measure_program_get_doc=false,
            ~measure_layoutOfDoc_layout_of_doc=false,
            ~memoize_doc=true,
            program,
          );
        program.is_focused
          ? [UHCodeDecoration.caret_view(~font_metrics, caret_pos)] : [];
      };

      let key_handlers =
        program.is_focused
          ? key_handlers(
              ~inject,
              ~is_mac,
              ~cursor_info=Program.get_cursor_info(program),
            )
          : [];

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
            let caret_pos =
              MeasuredPosition.{
                row:
                  Float.to_int(
                    (target_y -. container_rect##.top)
                    /. font_metrics.row_height,
                  ),
                col:
                  Float.to_int(
                    Float.round(
                      (target_x -. container_rect##.left)
                      /. font_metrics.col_width,
                    ),
                  ),
              };
            inject(ModelAction.MoveAction(Click(caret_pos)));
          }),
          // necessary to make cell focusable
          Attr.create("tabindex", "0"),
          Attr.on_focus(_ => inject(ModelAction.FocusCell)),
          Attr.on_blur(_ => inject(ModelAction.BlurCell)),
          ...key_handlers,
        ],
        caret
        @ [Node.span([Attr.classes(["code"])], code_text), ...decorations],
      );
    },
  );
};
