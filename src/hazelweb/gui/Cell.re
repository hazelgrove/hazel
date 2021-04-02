module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;

open ViewUtil;

let view = (~inject, model: Model.t) => {
  let settings = model.settings;
  let performance = settings.performance;
  TimeUtil.measure_time(
    "Cell.view",
    performance.measure && performance.cell_view,
    () => {
      open Vdom;
      let program = Model.get_program(model);
      let mini_buffer =
        switch (model.mini_buffer) {
        | None => []
        | Some(_) => [
            Node.input(
              [
                Attr.id("mini-buffer"),
                Attr.on_change((_, contents) =>
                  inject(ModelAction.SubmitMiniBuffer(contents))
                ),
                Attr.on_keydown(evt => {
                  switch (HazelKeyCombos.of_evt(evt)) {
                  | Some(Escape) =>
                    Event.Many([
                      Event.Prevent_default,
                      Event.Stop_propagation,
                      inject(ModelAction.CloseMiniBuffer),
                      inject(ModelAction.Focus(Cell)),
                    ])
                  | _ => Event.Many([])
                  }
                }),
                Attr.on_focus(_ => inject(ModelAction.Focus(MiniBuffer))),
                Attr.on_blur(_ => inject(ModelAction.Blur)),
              ],
              [],
            ),
          ]
        };
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
                ~is_mac=model.is_mac,
                ~is_focused=model.focused == Some(Cell),
                ~settings,
                program,
              ),
            ],
          ),
        ]
        @ mini_buffer,
      );
    },
  );
};
