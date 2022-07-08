module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;

open ViewUtil;

let view = (~inject, model: Model.t) => {
  let settings = model.settings;
  let cursor_inspector = model.cursor_inspector;
  let performance = settings.performance;
  TimeUtil.measure_time(
    "ProgramArea.view",
    performance.measure && performance.cell_view,
    () => {
      open Vdom;
      let program = model |> Model.get_program |> Program.extract_zcells;

      let wrap_cell = (num_of_cell, code_text) =>
        Node.div(
          [Attr.id(cell_id)],
          [
            /* font-specimen used to gather font metrics for caret positioning and other things */
            Node.div([Attr.id("font-specimen")], [Node.text("X")]),
            Node.div(
              [Attr.id("code-container")],
              [
                UHCode.view(
                  ~inject,
                  ~font_metrics=model.font_metrics,
                  ~settings,
                  ~cursor_inspector,
                  program,
                  num_of_cell,
                  code_text,
                ),
              ],
            ),
          ],
        );

      Node.div(
        [],
        UHCode.get_code_text_cells(~settings, program)
        |> List.mapi(wrap_cell),
      );
    },
  );
};
