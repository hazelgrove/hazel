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
      let wrap_cell = (num_of_cell, cell) =>
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
                  cell,
                  num_of_cell,
                ),
              ],
            ),
          ],
        );
      let cells =
        model
        |> Model.get_program
        |> Program.extract_zcells
        |> List.mapi(wrap_cell);
      Node.div([], cells);
    },
  );
};
