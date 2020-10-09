module Vdom = Virtual_dom.Vdom;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;
module Sexp = Sexplib.Sexp;

open ViewUtil;

let view = (~inject, model: Model.t) => {
  TimeUtil.measure_time(
    "Cell.view",
    model.measurements.measurements && model.measurements.cell_view,
    () => {
      open Vdom;

      let program = Model.get_program(model);

      let ci_opt =
        if (Model.is_cell_focused(model) && model.cursor_inspector.visible) {
          let (steps, cursor) = Program.get_path(program);
          let cursor =
            switch (cursor) {
            | OnText(_) => CursorPosition.OnText(0)
            | OnDelim(index, _) => CursorPosition.OnDelim(index, Before)
            | OnOp(_) => CursorPosition.OnOp(Before)
            };
          let m =
            Program.get_measured_layout(
              ~measure_program_get_doc=model.measurements.program_get_doc,
              ~measure_layoutOfDoc_layout_of_doc=
                model.measurements.layoutOfDoc_layout_of_doc,
              ~memoize_doc=model.memoize_doc,
              program,
            );
          let cursor_pos =
            UHMeasuredLayout.caret_position_of_path((steps, cursor), m)
            |> OptUtil.get(() => failwith("could not find caret"));
          let cursor_x =
            float_of_int(cursor_pos.col) *. model.font_metrics.col_width;
          let cursor_y =
            float_of_int(cursor_pos.row) *. model.font_metrics.row_height;
          let ci =
            CursorInspector.view(~inject, model, (cursor_x, cursor_y));

          Some(ci);
        } else {
          None;
        };

      let code_view =
        UHCode.view(
          ~inject,
          ~measure=
            model.measurements.measurements && model.measurements.uhcode_view,
          ~font_metrics=model.font_metrics,
          ~is_mac=model.is_mac,
          program,
        );
      let child_view =
        switch (ci_opt) {
        | None => [code_view]
        | Some(ci) => [code_view, ci]
        };

      Node.div(
        [Attr.id(cell_id)],
        [
          /* font-specimen used to gather font metrics for caret positioning and other things */
          Node.div([Attr.id("font-specimen")], [Node.text("X")]),
          Node.div([Attr.id("code-container")], child_view),
        ],
      );
    },
  );
};
