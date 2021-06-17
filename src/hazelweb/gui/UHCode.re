open OptUtil.Syntax;
module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
module Attr = Vdom.Attr;
module Event = Vdom.Event;
module Node = Vdom.Node;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

let decoration_cls: UHDecorationShape.t => string =
  fun
  | ErrHole => "err-hole"
  | VarErrHole => "var-err-hole"
  | VarUse => "var-use"
  | CurrentTerm => "current-term";

let decoration_view =
    (
      ~contains_current_term: bool,
      ~corner_radii: (float, float),
      ~term_sort: TermSort.t,
      ~term_shape: TermShape.t,
      shape: UHDecorationShape.t,
    ) =>
  UHDecoration.(
    switch (shape) {
    | ErrHole => ErrHole.view(~contains_current_term, ~corner_radii)
    | VarErrHole => VarErrHole.view(~contains_current_term, ~corner_radii)
    | VarUse => VarUse.view(~corner_radii)
    | CurrentTerm =>
      CurrentTerm.view(~corner_radii, ~sort=term_sort, ~shape=term_shape)
    }
  );

let decoration_views =
    (~font_metrics: FontMetrics.t, dpaths: UHDecorationPaths.t, l: UHLayout.t)
    : list(Vdom.Node.t) => {
  let corner_radii = Decoration_common.corner_radii(font_metrics);

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
               let view =
                 decoration_view(
                   ~contains_current_term=Option.is_some(dpaths.current_term),
                   ~corner_radii,
                   ~term_shape=shape,
                   ~term_sort=sort,
                   dshape,
                   (offset, m),
                 );
               Decoration_common.container(
                 ~font_metrics,
                 ~height=MeasuredLayout.height(m),
                 ~width=MeasuredLayout.width(~offset, m),
                 ~origin=MeasuredPosition.{row: start.row, col: indent},
                 ~cls,
                 [view],
               );
             });
        go'(~tl=current_vs @ tl, dpaths, m);
      | _ => go'(~tl, dpaths, m)
      }
    };
  };

  go(dpaths, UHMeasuredLayout.mk(l));
};

let click_handler =
    (
      root_id: string,
      font_metrics: FontMetrics.t,
      inject: ModelAction.t => Ui_event.t,
      evt,
    )
    : Ui_event.t => {
  let container_rect =
    JSUtil.force_get_elem_by_id(root_id)##getBoundingClientRect;
  let (target_x, target_y) = (
    float_of_int(evt##.clientX),
    float_of_int(evt##.clientY),
  );
  let caret_pos =
    Pretty.MeasuredPosition.{
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
  let {row, col}: MeasuredPosition.t = caret_pos;
  Printf.printf("r: %d, c: %d\n", row, col);

  inject(ModelAction.MoveAction(Click(caret_pos)));
};

let get_selected_action = (cursor_info, u_gen, settings: Settings.t) => {
  let* cursor = Assistant_common.promote_cursor_info(cursor_info, u_gen);
  let actions = Assistant.compute_actions(cursor);
  let selected_index =
    //TODO(andrew): unfuck below duplicated from AssistantView.re code
    switch (settings.cursor_inspector.assistant_selection) {
    | None => 0
    | Some(i) =>
      let z = List.length(actions) == 0 ? 0 : i mod List.length(actions);
      z + (z < 0 ? List.length(actions) : 0);
    };
  Some(List.nth(actions, selected_index).action);
};

let get_assistant_key_action =
    (
      ~settings: Settings.t,
      ~u_gen: MetaVarGen.t,
      ~cursor_info: CursorInfo.t,
      evt,
    )
    : option(ModelAction.t) => {
  switch (Key.get_key(evt)) {
  | "Escape" => Some(UpdateSettings(CursorInspector(Toggle_assistant)))
  | "ArrowDown" =>
    Some(UpdateSettings(CursorInspector(Increment_assistant_selection)))
  | "ArrowUp" =>
    Some(UpdateSettings(CursorInspector(Decrement_assistant_selection)))
  | "Tab" =>
    let+ action = get_selected_action(cursor_info, u_gen, settings);
    ModelAction.AcceptSuggestion(action);
  | _ => None
  };
};

let get_regular_key_action =
    (~is_mac: bool, ~cursor_info: CursorInfo.t, evt): option(ModelAction.t) => {
  switch (MoveKey.of_key(Key.get_key(evt))) {
  | Some(move_key) => Some(MoveAction(Key(move_key)))
  | None =>
    switch (HazelKeyCombos.of_evt(evt)) {
    | Some(keycombo) =>
      switch (keycombo) {
      | Ctrl_Z when !is_mac => Some(Undo)
      | Meta_Z when is_mac => Some(Undo)
      | Ctrl_Shift_Z when !is_mac => Some(Redo)
      | Meta_Shift_Z when is_mac => Some(Redo)
      | Ctrl_Space =>
        Some(UpdateSettings(CursorInspector(Toggle_type_assist)))
      | kc => Some(EditAction(KeyComboAction.get(cursor_info, kc)))
      }
    | None =>
      let+ key = JSUtil.is_single_key(evt);
      ModelAction.EditAction(
        Construct(SChar(JSUtil.single_key_string(key))),
      );
    }
  };
};

let key_handlers =
    (
      ~settings: Settings.t,
      ~u_gen: MetaVarGen.t,
      ~inject: ModelAction.t => Ui_event.t,
      ~is_mac: bool,
      ~cursor_info: CursorInfo.t,
      ~assistant_active: bool,
    )
    : list(Attr.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      let assistant_action =
        get_assistant_key_action(~settings, ~u_gen, ~cursor_info, evt);
      let regular_action = get_regular_key_action(~is_mac, ~cursor_info, evt);
      let reset_assistant =
        inject(UpdateSettings(CursorInspector(Reset_assistant_selection)));
      let ev =
        switch (assistant_action) {
        | Some(action) when assistant_active => inject(action)
        | _ =>
          Event.Many([
            reset_assistant,
            switch (regular_action) {
            | Some(action) => inject(action)
            | None => Event.Ignore
            },
          ])
        };
      Event.Many([Event.Prevent_default, Event.Stop_propagation, ev]);
    }),
  ];
};

let box_table: WeakMap.t(UHBox.t, list(Vdom.Node.t)) = WeakMap.mk();
let rec view_of_box = (box: UHBox.t): list(Vdom.Node.t) => {
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
  };
};

let focus = (editor: Model.editor) =>
  try(JSUtil.force_get_elem_by_id(Model.editor_id(editor))##focus) {
  | _ =>
    Printf.printf(
      "ERROR: tried to focus editor not present in the DOM: %s",
      Model.editor_id(editor),
    )
  };

//TODO(andrew): deprecate
let get_codebox_layout = program => {
  program
  |> Program.get_edit_state
  |> Program.EditState_Exp.get_uhstx
  |> Lazy.force(UHDoc_Exp.mk, ~memoize=false, ~enforce_inline=false)
  |> Pretty.LayoutOfDoc.layout_of_doc(~width=program.width, ~pos=0)
  |> OptUtil.get(() => failwith("unimplemented: layout failure"));
};

let codebox_view = (~font_metrics: FontMetrics.t, program: Program.exp) => {
  let l = get_codebox_layout(program);
  let code_text = view_of_box(UHBox.mk(l));
  let (err_holes, var_err_holes) =
    program |> Program.Exp.get_err_holes_decoration_paths;
  let dpaths: UHDecorationPaths.t = {
    current_term: None,
    err_holes,
    var_uses: [],
    var_err_holes,
  };
  let decorations = decoration_views(~font_metrics, dpaths, l);
  [Node.span([Attr.classes(["code"])], code_text), ...decorations];
};

let typebox_view =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~font_metrics: FontMetrics.t,
      ~is_mac: bool,
      ~settings: Settings.t,
      ~is_focused: bool,
      editor: Program.typ,
      u_gen,
    ) => {
  let layout = Program.Typ.get_layout(~settings, editor);
  let code_text = view_of_box(UHBox.mk(layout));
  let dpaths = Program.Typ.get_decoration_paths(editor);
  let dpaths = {
    ...dpaths,
    current_term: is_focused ? dpaths.current_term : None,
  };
  let decorations = decoration_views(~font_metrics, dpaths, layout);
  let caret_pos = Program.Typ.get_caret_position(~settings, editor);
  let caret =
    is_focused ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];

  let cursor_info = Program.Typ.get_cursor_info(editor);
  let assistant_active = false; // TODO(andrew): factor this out of key_handlers?
  let key_handlers =
    is_focused
      ? key_handlers(
          ~settings,
          ~u_gen,
          ~inject,
          ~is_mac,
          ~cursor_info,
          ~assistant_active,
        )
      : [];
  let this_editor = Model.AssistantTypeEditor;
  let editor_id = Model.editor_id(this_editor);

  let on_click = click_handler(editor_id, font_metrics, inject);

  [
    Node.div(
      [
        Attr.id(editor_id),
        Attr.classes(["code"]),
        Attr.on_click(on_click),
        Attr.create("tabindex", "0"), // necessary to make cell focusable
        Attr.on_focus(_ => inject(FocusCell(this_editor))),
        Attr.on_blur(_ => inject(BlurCell)),
        ...key_handlers,
      ],
      caret
      @ [Node.span([Attr.classes(["code"])], code_text), ...decorations],
    ),
  ];
};
