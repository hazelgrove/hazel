module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;

open ViewUtil;

let view_of_cursor_inspector =
    (~inject, {font_metrics, settings, _} as model: Model.t) => {
  let program = Model.get_program(model);
  let (steps, cursor) = Editor.Exp.get_path(program);
  let cursor =
    switch (cursor) {
    | OnText(_) => CursorPosition.OnText(0)
    | OnDelim(index, _) => CursorPosition.OnDelim(index, Before)
    | OnOp(_) => CursorPosition.OnOp(Before)
    };
  let m = program |> Editor.Exp.get_layout(~settings) |> UHMeasuredLayout.mk;
  let cursor_pos =
    UHMeasuredLayout.caret_position_of_path((steps, cursor), m)
    |> OptUtil.get(() => failwith("could not find caret"));
  let cursor_x = float_of_int(cursor_pos.col) *. font_metrics.col_width;
  let cursor_y = float_of_int(cursor_pos.row) *. font_metrics.row_height;
  CursorInspector.view(~inject, ~loc=(cursor_x, cursor_y), model);
};

let code_view =
    (
      ~inject,
      {settings, focal_editor, font_metrics, assistant, cursor_inspector, _} as model: Model.t,
    ) => {
  TimeUtil.measure_time(
    "Cell.view_internal",
    settings.performance.measure && settings.performance.uhcode_view,
    () => {
      open Vdom;

      let main_editor_is_focused = focal_editor == Model.MainProgram;
      let program = Model.get_program(model);

      let codebox =
        UHCode.codebox_view(
          ~settings,
          ~font_metrics,
          ~is_focused=main_editor_is_focused,
          program,
        );

      let cursor_info = Editor.Exp.get_cursor_info(program);
      let u_gen = Editor.EditState_Exp.get_ugen(program.edit_state);
      let assistant_action =
        AssistantModel.get_action(~u_gen, assistant, cursor_info);

      let cursor_inspector =
        if (cursor_inspector.visible) {
          [view_of_cursor_inspector(~inject, model)];
        } else {
          [];
        };

      let this_editor = Model.MainProgram;
      let editor_id = Model.editor_id(this_editor);

      let on_click = evt =>
        inject(UHCode.click_to_move(editor_id, font_metrics, evt));

      let key_handlers =
        main_editor_is_focused
          ? UHCode.key_handlers(
              ~inject,
              ~cursor_info,
              ~assistant_action,
              ~assistant_active=assistant.active,
            )
          : [];

      /*
       let on_contextmenu = evt => {
         let ty =
           switch (Assistant_common.get_type(cursor_info)) {
           | None => UHTyp.contract(HTyp.Hole)
           | Some(ty) => UHTyp.contract(ty)
           };
         Event.Many([
           Event.Prevent_default,
           inject(
             Chain([
               UpdateAssistant(Set_type_editor(ty)),
               UpdateAssistant(Turn_on),
               UpdateSettings(CursorInspector(Set_visible(true))),
               UHCode.click_to_move(editor_id, font_metrics, evt),
             ]),
           ),
         ]);
       };
       */

      Node.div(
        [
          Attr.id(editor_id),
          Attr.classes(["code", "presentation"]),
          Attr.on_click(on_click),
          //Attr.on_contextmenu(on_contextmenu),
          Attr.create("tabindex", "0"), // necessary to make cell focusable
          Attr.on_focus(_ => inject(FocusCell(this_editor))),
          Attr.on_blur(_ => inject(BlurCell)),
          ...key_handlers,
        ],
        cursor_inspector @ codebox,
      );
    },
  );
};

let view = (~inject, model: Model.t) => {
  let settings = model.settings;
  let performance = settings.performance;
  TimeUtil.measure_time(
    "Cell.view", performance.measure && performance.cell_view, () => {
    Vdom.(
      Node.div(
        [Attr.id(cell_id)],
        [
          /* font-specimen used to gather font metrics for caret positioning and other things */
          Node.div([Attr.id("font-specimen")], [Node.text("X")]),
          Node.div(
            [Attr.id("code-container")],
            [code_view(~inject, model)],
          ),
        ],
      )
    )
  });
};
