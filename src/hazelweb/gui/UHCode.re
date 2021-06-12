open OptUtil.Syntax;
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

let key_handlers =
    (
      ~settings: Settings.t,
      ~u_gen,
      ~inject,
      ~is_mac: bool,
      ~cursor_info: CursorInfo.t,
      ~assistant_active: bool,
    )
    : list(Vdom.Attr.t) => {
  open Vdom;
  let prevent_stop_inject = a =>
    Event.Many([Event.Prevent_default, Event.Stop_propagation, inject(a)]);
  let pre_event = event =>
    Event.Many([
      prevent_stop_inject(
        ModelAction.UpdateSettings(
          CursorInspector(Reset_assistant_selection),
        ),
      ),
      event,
    ]);
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      // TODO(andrew): do this mode stuff more better
      switch (Key.get_key(evt)) {
      | "Tab" when assistant_active =>
        switch (get_selected_action(cursor_info, u_gen, settings)) {
        | None => Event.Ignore
        | Some(action) =>
          prevent_stop_inject(ModelAction.AcceptSuggestion(action))
        }
      | "Escape" when assistant_active =>
        prevent_stop_inject(
          ModelAction.UpdateSettings(CursorInspector(Toggle_assistant)),
        )
      | "ArrowDown" when assistant_active =>
        prevent_stop_inject(
          ModelAction.UpdateSettings(
            CursorInspector(Increment_assistant_selection),
          ),
        )
      | "ArrowUp" when assistant_active =>
        prevent_stop_inject(
          ModelAction.UpdateSettings(
            CursorInspector(Decrement_assistant_selection),
          ),
        )
      | _ =>
        // TODO(andrew): slightly hacky way to preceed all fallthru
        // events with resetting the current assistant selection
        pre_event(
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
            | Some(Ctrl_Space) =>
              prevent_stop_inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Toggle_type_assist),
                ),
              )
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
          },
        )
      }
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

let root_id = "code-root";

let focus = () => {
  JSUtil.force_get_elem_by_id(root_id)##focus;
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      program: Program.exp,
    )
    : (Base.list(Vdom.Node.t), list(Vdom.Node.t)) => {
  let l = Program.Exp.get_layout(~settings, program);
  let code_text = view_of_box(UHBox.mk(l));
  let decorations = {
    let dpaths = Program.Exp.get_decoration_paths(program);
    decoration_views(~font_metrics, dpaths, l);
  };
  (code_text, decorations);
};

let get_codebox_layout = program => {
  program
  |> Program.get_edit_state
  |> Program.EditState_Exp.get_uhstx
  |> Lazy.force(UHDoc_Exp.mk, ~memoize=false, ~enforce_inline=false)
  |> Pretty.LayoutOfDoc.layout_of_doc(~width=program.width, ~pos=0)
  |> OptUtil.get(() => failwith("unimplemented: layout failure"));
};

let codebox_view = (~font_metrics: FontMetrics.t, program: Program.exp) => {
  open Vdom;
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

// TODO(andrew): below two fns are copied-pasted from above two
let get_typebox_layout = program => {
  program
  |> Program.get_edit_state
  |> Program.EditState_Typ.get_uhstx
  |> Lazy.force(UHDoc_Typ.mk, ~memoize=false, ~enforce_inline=false)
  |> Pretty.LayoutOfDoc.layout_of_doc(~width=program.width, ~pos=0)
  |> OptUtil.get(() => failwith("unimplemented: layout failure"));
};

let typebox_view =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~is_focused=true,
      program: Program.typ,
      cursor_info,
      u_gen,
    ) => {
  open Vdom;
  let l = get_typebox_layout(program);
  let code_text = view_of_box(UHBox.mk(l));
  let (err_holes, var_err_holes) =
    program |> Program.Typ.get_err_holes_decoration_paths;
  let dpaths: UHDecorationPaths.t = {
    current_term: None,
    err_holes,
    var_uses: [],
    var_err_holes,
  };
  let caret = {
    let caret_pos = Program.Typ.get_caret_position(~settings, program);
    is_focused ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];
  };
  let key_handlers =
    is_focused
      ? key_handlers(
          ~settings,
          ~u_gen,
          ~inject,
          ~is_mac=true, //TODO(andrew): unhack this
          ~cursor_info,
          //TODO(andrew): clean up below
          ~assistant_active=false,
        )
      : [];
  let decorations = decoration_views(~font_metrics, dpaths, l);
  [
    Node.span(
      [
        Attr.classes(["code"]),
        Attr.on_mousedown(_ => Event.Many([Event.Stop_propagation])),
        Attr.on_click(_ => Event.Many([Event.Stop_propagation])),
        // necessary to make cell focusable
        Attr.create("tabindex", "0"),
        //Attr.on_click(_ =>
        //  Event.Many([Event.Prevent_default, Event.Stop_propagation])
        //),
        Attr.on_focus(_ => {
          print_endline("ASSISTANT taking focus");
          Event.Many([
            Event.Stop_propagation,
            Event.Prevent_default,
            inject(ModelAction.FocusCell(ModelAction.assistant_editor_id)),
          ]);
        }),
        Attr.on_blur(_ => inject(ModelAction.BlurCell)),
        ...key_handlers,
      ],
      caret @ code_text,
    ),
    ...decorations,
  ];
};
