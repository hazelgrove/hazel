module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;

open ViewUtil;

let view_of_cursor_inspector =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~is_mac: bool,
      ~settings: Settings.t,
      type_editor_is_focused,
      assistant_model,
      (steps, cursor): CursorPath.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
      program: Program.exp,
      u_gen: MetaVarGen.t,
    ) => {
  let cursor =
    switch (cursor) {
    | OnText(_) => CursorPosition.OnText(0)
    | OnDelim(index, _) => CursorPosition.OnDelim(index, Before)
    | OnOp(_) => CursorPosition.OnOp(Before)
    };
  let m = program |> Program.Exp.get_layout(~settings) |> UHMeasuredLayout.mk;
  let cursor_pos =
    UHMeasuredLayout.caret_position_of_path((steps, cursor), m)
    |> OptUtil.get(() => failwith("could not find caret"));
  let cursor_x = float_of_int(cursor_pos.col) *. font_metrics.col_width;
  let cursor_y = float_of_int(cursor_pos.row) *. font_metrics.row_height;
  CursorInspector.view(
    ~inject,
    ~font_metrics,
    ~is_mac,
    ~settings,
    type_editor_is_focused,
    assistant_model,
    (cursor_x, cursor_y),
    cursor_inspector,
    cursor_info,
    u_gen,
  );
};

let code_view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~is_mac: bool,
      ~settings: Settings.t,
      program: Program.exp,
      focal_editor: Model.editor,
      assistant_model: AssistantModel.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "Cell.view_internal",
    settings.performance.measure && settings.performance.uhcode_view,
    () => {
      open Vdom;

      let main_editor_is_focused = focal_editor == Model.MainProgram;
      let type_editor_is_focused = focal_editor == Model.AssistantTypeEditor;
      let u_gen = Program.EditState_Exp.get_ugen(program.edit_state);

      let codebox =
        UHCode.codebox_view(
          ~settings,
          ~font_metrics,
          ~is_focused=main_editor_is_focused,
          program,
        );

      let cursor_info = Program.Exp.get_cursor_info(program);
      let ci_settings = settings.cursor_inspector;
      let assistant_action =
        switch (Assistant_common.promote_cursor_info(cursor_info, u_gen)) {
        | None => None
        | Some(ci) => AssistantView.select_action(assistant_model, ci)
        };
      let key_handlers =
        main_editor_is_focused
          ? UHCode.key_handlers(
              ~inject,
              ~is_mac,
              ~cursor_info,
              ~assistant_action,
              ~assistant_active=assistant_model.active,
            )
          : [];

      let cursor_inspector =
        if (ci_settings.visible) {
          [
            view_of_cursor_inspector(
              ~inject,
              ~font_metrics,
              ~is_mac,
              ~settings,
              type_editor_is_focused,
              assistant_model,
              Program.Exp.get_path(program),
              ci_settings,
              cursor_info,
              program,
              u_gen,
            ),
          ];
        } else {
          [];
        };

      let this_editor = Model.MainProgram;
      let editor_id = Model.editor_id(this_editor);

      let on_click = evt =>
        inject(
          Chain([
            UpdateAssistant(Turn_off),
            UHCode.click_to_move(editor_id, font_metrics, evt),
          ]),
        );

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

      Node.div(
        [
          Attr.id(editor_id),
          Attr.classes(["code", "presentation"]),
          Attr.on_click(on_click),
          Attr.on_contextmenu(on_contextmenu),
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
    "Cell.view",
    performance.measure && performance.cell_view,
    () => {
      open Vdom;
      let program = Model.get_program(model);
      Node.div(
        [Attr.id(cell_id)],
        [
          /* font-specimen used to gather font metrics for caret positioning and other things */
          Node.div([Attr.id("font-specimen")], [Node.text("X")]),
          Node.div(
            [Attr.id("code-container")],
            [
              code_view(
                ~inject,
                ~font_metrics=model.font_metrics,
                ~is_mac=model.is_mac,
                ~settings,
                program,
                model.focal_editor,
                model.assistant,
              ),
            ],
          ),
        ],
      );
    },
  );
};
