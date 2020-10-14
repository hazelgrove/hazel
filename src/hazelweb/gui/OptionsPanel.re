module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;

let labeled_checkbox =
    (
      ~id: string,
      ~classes: List.t(string)=[],
      ~label: string,
      ~on_change: unit => Vdom.Event.t,
      ~disabled=false,
      checked: bool,
    )
    : Vdom.Node.t => {
  let checkbox_id = id ++ "_checkbox";
  Vdom.(
    Node.div(
      [Attr.id(id), Attr.classes(["labeled-checkbox", ...classes])],
      [
        Node.input(
          [
            [
              Attr.id(checkbox_id),
              Attr.type_("checkbox"),
              Attr.on_change((_, _) => on_change()),
            ],
            checked ? [Attr.checked] : [],
            disabled ? [Attr.disabled] : [],
          ]
          |> List.concat,
          [],
        ),
        Node.label(
          [
            Attr.for_(id),
            Attr.on_click(_ => on_change()),
            ...disabled ? [Attr.disabled] : [],
          ],
          [Node.text(label)],
        ),
      ],
    )
  );
};

let details = Vdom.Node.create("details");
let summary = Vdom.Node.create("summary");

let view =
    (~inject: ModelAction.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let compute_results_checkbox =
    Vdom.(
      Node.div(
        [Attr.id("OptionsPanel")],
        [
          labeled_checkbox(
            ~id="compute_results",
            ~label="Compute results",
            ~on_change=() => inject(ToggleComputeResults),
            model.compute_results.compute_results,
          ),
          labeled_checkbox(
            ~id="show_case_clauses",
            ~classes=["indented-option"],
            ~label="Show case clauses",
            ~on_change=() => inject(ToggleShowCaseClauses),
            ~disabled=!model.compute_results.compute_results,
            model.compute_results.show_case_clauses,
          ),
          labeled_checkbox(
            ~id="show_fn_bodies",
            ~classes=["indented-option"],
            ~label="Show function bodies",
            ~on_change=() => inject(ToggleShowFnBodies),
            ~disabled=!model.compute_results.compute_results,
            model.compute_results.show_fn_bodies,
          ),
          labeled_checkbox(
            ~id="show_casts",
            ~classes=["indented-option"],
            ~label="Show casts",
            ~on_change=() => inject(ToggleShowCasts),
            ~disabled=!model.compute_results.compute_results,
            model.compute_results.show_casts,
          ),
          labeled_checkbox(
            ~id="show_unevaluated_expansion",
            ~classes=["indented-option"],
            ~label="Show unevaluated expansion",
            ~on_change=() => inject(ToggleShowUnevaluatedExpansion),
            ~disabled=!model.compute_results.compute_results,
            model.compute_results.show_unevaluated_expansion,
          ),
          //
          labeled_checkbox(
            ~id="measure_times",
            ~label="Measure times",
            ~on_change=() => inject(ToggleMeasureTimes),
            model.measurements.measurements,
          ),
          labeled_checkbox(
            ~id="measure_model_perform_edit_action",
            ~classes=["indented-option"],
            ~label="model_perform_edit_action",
            ~on_change=() => inject(ToggleMeasureModel_perform_edit_action),
            ~disabled=!model.measurements.measurements,
            model.measurements.model_perform_edit_action,
          ),
          labeled_checkbox(
            ~id="measure_program_get_doc",
            ~classes=["indented-option"],
            ~label="program_get_doc",
            ~on_change=() => inject(ToggleMeasureProgram_get_doc),
            ~disabled=!model.measurements.measurements,
            model.measurements.program_get_doc,
          ),
          labeled_checkbox(
            ~id="measure_layoutOfDoc_layout_of_doc",
            ~classes=["indented-option"],
            ~label="layoutOfDoc_layout_of_doc",
            ~on_change=() => inject(ToggleMeasureLayoutOfDoc_layout_of_doc),
            ~disabled=!model.measurements.measurements,
            model.measurements.layoutOfDoc_layout_of_doc,
          ),
          labeled_checkbox(
            ~id="measure_uhcode_view",
            ~classes=["indented-option"],
            ~label="uhcode_view",
            ~on_change=() => inject(ToggleMeasureUHCode_view),
            ~disabled=!model.measurements.measurements,
            model.measurements.uhcode_view,
          ),
          labeled_checkbox(
            ~id="measure_cell_view",
            ~classes=["indented-option"],
            ~label="cell_view",
            ~on_change=() => inject(ToggleMeasureCell_view),
            ~disabled=!model.measurements.measurements,
            model.measurements.cell_view,
          ),
          labeled_checkbox(
            ~id="measure_page_view",
            ~classes=["indented-option"],
            ~label="page_view",
            ~on_change=() => inject(ToggleMeasurePage_view),
            ~disabled=!model.measurements.measurements,
            model.measurements.page_view,
          ),
          labeled_checkbox(
            ~id="measure_hazel_create",
            ~classes=["indented-option"],
            ~label="hazel_create",
            ~on_change=() => inject(ToggleMeasureHazel_create),
            ~disabled=!model.measurements.measurements,
            model.measurements.hazel_create,
          ),
          labeled_checkbox(
            ~id="measure_update_apply_action",
            ~classes=["indented-option"],
            ~label="update_apply_action",
            ~on_change=() => inject(ToggleMeasureUpdate_apply_action),
            ~disabled=!model.measurements.measurements,
            model.measurements.update_apply_action,
          ),
          //
          labeled_checkbox(
            ~id="memoize_doc",
            ~label="Memoize doc generation",
            ~on_change=() => inject(ToggleMemoizeDoc),
            model.memoize_doc,
          ),
          labeled_checkbox(
            ~id="novice_mode",
            ~label="Novice mode",
            ~on_change=() => inject(ToggleNoviceMode),
            Model.get_novice_mode(model),
          ),
        ],
      )
    );
  details(
    [],
    [summary([], [Vdom.Node.text("Options")]), compute_results_checkbox],
  );
};
