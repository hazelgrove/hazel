module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

/**
 * A buffered container for SVG elements so that strokes along
 * the bounding box of the elements do not get clipped by the
 * viewBox boundaries
 */
let decoration_container =
    (
      ~font_metrics: FontMetrics.t,
      ~origin: MeasuredPosition.t,
      ~height: int,
      ~width: int,
      ~cls: string,
      svgs: list(Vdom.Node.t),
    )
    : Vdom.Node.t => {
  let buffered_height = height + 1;
  let buffered_width = width + 1;

  let buffered_height_px =
    Float.of_int(buffered_height) *. font_metrics.row_height;
  let buffered_width_px =
    Float.of_int(buffered_width) *. font_metrics.col_width;

  let container_origin_x =
    (Float.of_int(origin.row) -. 0.5) *. font_metrics.row_height;
  let container_origin_y =
    (Float.of_int(origin.col) -. 0.5) *. font_metrics.col_width;

  Vdom.(
    Node.div(
      [
        Attr.classes([
          "decoration-container",
          Printf.sprintf("%s-container", cls),
        ]),
        Attr.create(
          "style",
          Printf.sprintf(
            "top: calc(%fpx - 1px); left: %fpx;",
            container_origin_x,
            container_origin_y,
          ),
        ),
      ],
      [
        Node.create_svg(
          "svg",
          [
            Attr.classes([cls]),
            Attr.create(
              "viewBox",
              Printf.sprintf(
                "-0.5 -0.5 %d %d",
                buffered_width,
                buffered_height,
              ),
            ),
            Attr.create("width", Printf.sprintf("%fpx", buffered_width_px)),
            Attr.create(
              "height",
              Printf.sprintf("%fpx", buffered_height_px),
            ),
            Attr.create("preserveAspectRatio", "none"),
          ],
          svgs,
        ),
      ],
    )
  );
};

let box_table: WeakMap.t(UHBox.t, list(Vdom.Node.t)) = WeakMap.mk();
let rec view_of_box =
        (~assert_map: option(AssertMap.t), box: UHBox.t): list(Vdom.Node.t) => {
  Vdom.(
    switch (WeakMap.get(box_table, box)) {
    | Some(vs) => vs
    | None =>
      switch (box) {
      | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
      | HBox(boxes) =>
        boxes |> List.map(view_of_box(~assert_map)) |> List.flatten
      | VBox(boxes) =>
        let vs =
          boxes
          |> List.map(view_of_box(~assert_map))
          |> ListUtil.join([Node.br([])])
          |> List.flatten;
        [Node.div([Attr.classes(["VBox"])], vs)];
      | Annot(annot, box) =>
        let vs = view_of_box(~assert_map, box);
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
        | AssertNum({num}) =>
          switch (assert_map) {
          | None => vs
          | Some(assert_map) =>
            switch (AssertMap.lookup(num, assert_map)) {
            | None => [
                Vdom.Node.span([Vdom.Attr.classes(["AssertIndet"])], vs),
              ]
            | Some(a) =>
              switch (AssertMap.check(a)) {
              | Pass => [
                  Vdom.Node.span([Vdom.Attr.classes(["AssertPass"])], vs),
                ]

              | Fail => [
                  Vdom.Node.span([Vdom.Attr.classes(["AssertFail"])], vs),
                ]

              | Comp => [
                  Vdom.Node.span([Vdom.Attr.classes(["AssertComp"])], vs),
                ]

              | Indet => [
                  Vdom.Node.span([Vdom.Attr.classes(["AssertIndet"])], vs),
                ]
              }
            }
          }
        | _ => vs
        };
      }
    }
  );
};

// TODO refactor so that it doesn't depend on assert map
let view_of_text = (~assert_map: option(AssertMap.t)=?, l: UHLayout.t) => {
  view_of_box(~assert_map, UHBox.mk(l));
};

let decoration_cls: UHDecorationShape.t => string =
  fun
  | ErrHole => "err-hole"
  | VarErrHole => "var-err-hole"
  | VarUse => "var-use"
  | CurrentTerm => "current-term"
  | FilledHole(_) => "filled-hole"
  | FillingHole(_) => "filling-hole";

let decoration_view =
    // todo
    (
      ~decoration_views as
        _: (UHDecorationPaths.t, UHLayout.t) => list(Vdom.Node.t),
      ~corner_radii: (float, float),
      ~contains_current_term: bool,
      ~term_sort: TermSort.t,
      ~term_shape: TermShape.t,
      dshape: UHDecorationShape.t,
    ) => {
  open UHDecoration;
  let mk_layout = e =>
    e
    |> Lazy.force(UHDoc_Exp.mk, ~memoize=false, ~enforce_inline=false)
    |> Pretty.LayoutOfDoc.layout_of_doc(~width=25, ~pos=0)
    |> OptUtil.get(() => failwith("layout failure"));
  switch (dshape) {
  | ErrHole => ErrHole.view(~contains_current_term, ~corner_radii)
  | VarErrHole => VarErrHole.view(~contains_current_term, ~corner_radii)
  | VarUse => VarUse.view(~corner_radii)
  | CurrentTerm =>
    CurrentTerm.view(~corner_radii, ~sort=term_sort, ~shape=term_shape)
  | FilledHole(_synthesizing, _filled_holes) => failwith("todo")
  // let l = mk_layout(e);
  // let text = view_of_text(l);
  // let decorations = {
  //   let dpaths = UHDecorationPaths.mk(~synthesizing, ());
  //   decoration_views(dpaths, l);
  // };
  // FilledHole.view(~text, ~decorations);
  | FillingHole(filling) =>
    let options =
      filling
      |> ZList.map(mk_layout, mk_layout)
      |> ZList.map(view_of_text, view_of_text)
      |> ZList.map(Vdom.Node.span([]), Vdom.Node.span([]));
    FillingHole.view(~options);
  };
};

let rec decoration_views =
        (
          ~font_metrics: FontMetrics.t,
          dpaths: UHDecorationPaths.t,
          l: UHLayout.t,
        )
        : list(Vdom.Node.t) => {
  let decoration_views = decoration_views(~font_metrics);

  let corner_radius = 2.5;
  let corner_radii = (
    corner_radius /. font_metrics.col_width,
    corner_radius /. font_metrics.row_height,
  );
  let decoration_view = decoration_view(~corner_radii);
  let decoration_container = decoration_container(~font_metrics);

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
        let view = dshape => {
          let view =
            decoration_view(
              ~decoration_views,
              ~contains_current_term=Option.is_some(dpaths.current_term),
              ~term_sort=sort,
              ~term_shape=shape,
              dshape,
              (offset, m),
            );
          switch (dshape) {
          | ErrHole
          | VarErrHole
          | VarUse
          | CurrentTerm =>
            decoration_container(
              ~height=MeasuredLayout.height(m),
              ~width=MeasuredLayout.width(~offset, m),
              ~origin=MeasuredPosition.{row: start.row, col: indent},
              ~cls=decoration_cls(dshape),
              [view],
            )
          | FillingHole(_)
          | FilledHole(_) =>
            Vdom.Node.div(
              Vdom.[
                Attr.classes(["synthesizing-container"]),
                Attr.create(
                  "style",
                  Printf.sprintf(
                    "top: %fpx; left: %fpx",
                    Float.of_int(start.row) *. font_metrics.row_height,
                    Float.of_int(indent) *. font_metrics.col_width,
                  ),
                ),
              ],
              [view],
            )
          };
        };
        let current_vs =
          List.map(view, UHDecorationPaths.current(shape, dpaths));
        go'(~tl=current_vs @ tl, dpaths, m);
      | _ => go'(~tl, dpaths, m)
      }
    };
  };

  go(dpaths, UHMeasuredLayout.mk(l));
};

let root_id = "code-root";

let focus = () => {
  JSUtil.force_get_elem_by_id(root_id)##focus;
};

let view_of_cursor_inspector =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      (steps, cursor): CursorPath.t,
      cursor_inspector: Model.cursor_inspector,
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
    ~inject,
    ~view_of_text=l => view_of_text(l),
    (cursor_x, cursor_y),
    cursor_inspector,
    cursor_info,
  );
};

let key_handlers =
    (
      ~inject,
      ~is_mac: bool,
      ~cursor_info: CursorInfo.t,
      ~cursor_inspector: Model.cursor_inspector,
    )
    : list(Vdom.Attr.t) => {
  open Vdom;
  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      switch (MoveKey.of_key(Key.get_key(evt))) {
      | Some(move_key) =>
        switch (cursor_inspector.synthesizing, move_key) {
        | (Some((_, i, _)), ArrowUp) when i > 0 =>
          prevent_stop_inject(ModelAction.ScrollFilling(i - 1))
        | (Some((_, i, fillings)), ArrowDown)
            when i < List.length(fillings) - 1 =>
          prevent_stop_inject(ModelAction.ScrollFilling(i + 1))
        | _ => prevent_stop_inject(ModelAction.MoveAction(Key(move_key)))
        }
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
        | Some(Ctrl_Space) =>
          prevent_stop_inject(ModelAction.ToggleShowCursorInspector)
        | Some(kc) =>
          switch (cursor_inspector.synthesizing) {
          | Some(_) when kc == Enter =>
            prevent_stop_inject(ModelAction.AcceptFilling)
          | None when kc == Ctrl_Enter =>
            switch (cursor_info.cursor_term) {
            | Exp(_, EmptyHole(u)) =>
              prevent_stop_inject(ModelAction.SynthesizeHole(u))
            | _ => Event.Ignore
            }
          | _ =>
            prevent_stop_inject(
              ModelAction.EditAction(KeyComboAction.get(cursor_info, kc)),
            )
          }
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
    }),
  ];
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~measure: bool,
      ~is_mac: bool,
      ~cursor_inspector: Model.cursor_inspector,
      program: Program.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "UHCode.view",
    measure,
    () => {
      open Vdom;

      let key_handlers =
        program.is_focused
          ? key_handlers(
              ~inject,
              ~is_mac,
              ~cursor_inspector,
              ~cursor_info=Program.get_cursor_info(program),
            )
          : [];

      let l =
        Program.get_layout(
          ~measure_program_get_doc=false,
          ~measure_layoutOfDoc_layout_of_doc=false,
          ~memoize_doc=false,
          program,
        );

      let code_text = {
        let assert_map = snd(Program.get_result(program));
        view_of_text(~assert_map, l);
      };
      let decorations = {
        let dpaths = Program.get_decoration_paths(program);
        decoration_views(~font_metrics, dpaths, l);
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
          ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];
      };
      let cursor_inspector =
        if (program.is_focused && cursor_inspector.visible) {
          let path = Program.get_path(program);
          let ci = Program.get_cursor_info(program);
          [
            view_of_cursor_inspector(
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

      let click_handler = evt => {
        let container_rect =
          JSUtil.force_get_elem_by_id(root_id)##getBoundingClientRect;
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
          Attr.id(root_id),
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
