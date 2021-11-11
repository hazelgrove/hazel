open ViewUtil;
open Virtual_dom.Vdom;
open Node;

let view = (~inject, model: Model.t) => {
  let settings = model.settings;
  let cursor_inspector = model.cursor_inspector;
  let performance = settings.performance;
  TimeUtil.measure_time(
    "Cell.view",
    performance.measure && performance.cell_view,
    () => {
      let program = Model.get_program(model);
      div(
        [Attr.id(cell_id)],
        [
          /* font-specimen used to gather font metrics for caret positioning and other things */
          div([Attr.id(ViewUtil.font_specimen_id)], [text("X")]),
          div(
            [Attr.id("code-container")],
            [
              UHCode.view(
                ~inject,
                ~font_metrics=model.font_metrics,
                ~settings,
                ~cursor_inspector,
                program,
              ),
            ],
          ),
        ],
      );
    },
  );
};
