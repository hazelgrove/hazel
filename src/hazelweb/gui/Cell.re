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
      (steps, cursor): CursorPath.t,
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
      l: UHLayout.t,
      u_gen: MetaVarGen.t,
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
    ~font_metrics,
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
      program: Program.t,
    )
    : Vdom.Node.t => {
  TimeUtil.measure_time(
    "Cell.view_internal",
    settings.performance.measure && settings.performance.uhcode_view,
    () => {
      open Vdom;

      let l = Program.get_layout(~settings, program);

      let (code_text, decorations) =
        UHCode.view(~font_metrics, ~settings, program);
      let caret = {
        let caret_pos = Program.get_caret_position(~settings, program);
        program.is_focused
          ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];
      };
      let cursor_inspector =
        // TODO(andrew): uncomment below (commented for testing purposes)
        if (/*program.is_focused &&*/ settings.cursor_inspector.visible) {
          let path = Program.get_path(program);
          let ci = Program.get_cursor_info(program);
          [
            view_of_cursor_inspector(
              ~inject,
              ~font_metrics,
              path,
              settings.cursor_inspector,
              ci,
              l,
              Program.get_ugen(program),
            ),
          ];
        } else {
          [];
        };

      let key_handlers =
        program.is_focused
          ? UHCode.key_handlers(
              ~settings,
              ~u_gen=Program.get_ugen(program),
              ~inject,
              ~is_mac,
              ~cursor_info=Program.get_cursor_info(program),
              //TODO(andrew): clean up below
              ~assistant_active=
                settings.cursor_inspector.assistant
                && Assistant_common.valid_assistant_term(
                     Program.get_cursor_info(program).cursor_term,
                   ),
            )
          : [];

      let click_handler = evt => {
        let container_rect =
          JSUtil.force_get_elem_by_id(UHCode.root_id)##getBoundingClientRect;
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
        inject(ModelAction.MoveAction(Click(caret_pos)));
      };

      Node.div(
        [
          Attr.id(UHCode.root_id),
          Attr.classes(["code", "presentation"]),
          // need to use mousedown instead of click to fire
          // (and move caret) before cell focus event handler
          Attr.on_mousedown(evt =>
            Event.Many([
              click_handler(evt),
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Set_visible(false)),
                ),
              ),
            ])
          ),
          Attr.on_contextmenu(evt => {
            // TODO(andrew): make this sane
            Event.Many([
              Event.Prevent_default,
              //Event.Stop_propagation,
              click_handler(evt),
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Set_visible(true)),
                ),
              ),
              inject(
                ModelAction.UpdateSettings(
                  CursorInspector(Set_assistant(true)),
                ),
              ),
            ])
          }),
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
              ),
            ],
          ),
        ],
      );
    },
  );
};
