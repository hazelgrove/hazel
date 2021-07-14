module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;

open ViewUtil;

let view = (~inject, ~sync_livelit, model: Model.t) => {
  let settings = model.settings;
  let performance = settings.performance;
  TimeUtil.measure_time(
    "Cell.view",
    performance.measure && performance.cell_view,
    () => {
      open Vdom;
      let program = Model.get_program(model);
      let e = Program.get_uhexp(program);
      let llview_ctx = Statics_Exp.build_ll_view_ctx(e);
      Node.div(
        [Attr.id(cell_id), Attr.classes(["the-context-scroll-container"])],
        [
          /* font-specimen used to gather font metrics for caret positioning and other things */
          Node.div([Attr.id("font-specimen")], [Node.text("X")]),
          Node.div(
            [Attr.id("code-container")],
            [
              UHCode.view(
                ~inject,
                ~font_metrics=model.font_metrics,
                ~is_mac=model.is_mac,
                ~selected_instances=model.selected_instances,
                ~sync_livelit,
                ~settings,
                ~llview_ctx,
                program,
              ),
            ],
          ),
        ],
      );
    },
  );
};
