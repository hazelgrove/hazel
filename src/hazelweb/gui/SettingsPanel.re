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
    (~inject: ModelAction.t => Vdom.Event.t, settings: Settings.t)
    : Vdom.Node.t => {
  let Settings.{memoize_doc, performance, evaluation} = settings;
  let evaluation_checkboxes =
    Vdom.(
      Node.div(
        [Attr.id("SettingsPanel")],
        [
          labeled_checkbox(
            ~id="evaluate",
            ~label="Evaluate",
            ~on_change=
              () => inject(UpdateSettings(Evaluation(Toggle_evaluate))),
            evaluation.evaluate,
          ),
          labeled_checkbox(
            ~id="show_kinds",
            ~classes=["indented-option"],
            ~label="Show kinds",
            ~on_change=
              () => inject(UpdateSettings(Evaluation(Toggle_show_kinds))),
            ~disabled=!evaluation.evaluate,
            evaluation.show_kinds,
          ),
          labeled_checkbox(
            ~id="show_case_clauses",
            ~classes=["indented-option"],
            ~label="Show case clauses",
            ~on_change=
              () =>
                inject(
                  UpdateSettings(Evaluation(Toggle_show_case_clauses)),
                ),
            ~disabled=!evaluation.evaluate,
            evaluation.show_case_clauses,
          ),
          labeled_checkbox(
            ~id="show_fn_bodies",
            ~classes=["indented-option"],
            ~label="Show function bodies",
            ~on_change=
              () =>
                inject(UpdateSettings(Evaluation(Toggle_show_fn_bodies))),
            ~disabled=!evaluation.evaluate,
            evaluation.show_fn_bodies,
          ),
          labeled_checkbox(
            ~id="show_casts",
            ~classes=["indented-option"],
            ~label="Show casts",
            ~on_change=
              () => inject(UpdateSettings(Evaluation(Toggle_show_casts))),
            ~disabled=!evaluation.evaluate,
            evaluation.show_casts,
          ),
          labeled_checkbox(
            ~id="show_unevaluated_elaboration",
            ~classes=["indented-option"],
            ~label="Show unevaluated elaboration",
            ~on_change=
              () =>
                inject(
                  UpdateSettings(
                    Evaluation(Toggle_show_unevaluated_elaboration),
                  ),
                ),
            ~disabled=!evaluation.evaluate,
            evaluation.show_unevaluated_elaboration,
          ),
          //
          labeled_checkbox(
            ~id="measure_times",
            ~label="Measure times",
            ~on_change=
              () => inject(UpdateSettings(Performance(Toggle_measure))),
            performance.measure,
          ),
          labeled_checkbox(
            ~id="measure_model_perform_edit_action",
            ~classes=["indented-option"],
            ~label="model_perform_edit_action",
            ~on_change=
              () =>
                inject(
                  UpdateSettings(
                    Performance(Toggle_model_perform_edit_action),
                  ),
                ),
            ~disabled=!performance.measure,
            performance.model_perform_edit_action,
          ),
          labeled_checkbox(
            ~id="measure_program_get_doc",
            ~classes=["indented-option"],
            ~label="program_get_doc",
            ~on_change=
              () =>
                inject(UpdateSettings(Performance(Toggle_program_get_doc))),
            ~disabled=!performance.measure,
            performance.program_get_doc,
          ),
          labeled_checkbox(
            ~id="measure_layoutOfDoc_layout_of_doc",
            ~classes=["indented-option"],
            ~label="layoutOfDoc_layout_of_doc",
            ~on_change=
              () =>
                inject(
                  UpdateSettings(
                    Performance(Toggle_layoutOfDoc_layout_of_doc),
                  ),
                ),
            ~disabled=!performance.measure,
            performance.layoutOfDoc_layout_of_doc,
          ),
          labeled_checkbox(
            ~id="measure_uhcode_view",
            ~classes=["indented-option"],
            ~label="uhcode_view",
            ~on_change=
              () => inject(UpdateSettings(Performance(Toggle_uhcode_view))),
            ~disabled=!performance.measure,
            performance.uhcode_view,
          ),
          labeled_checkbox(
            ~id="measure_cell_view",
            ~classes=["indented-option"],
            ~label="cell_view",
            ~on_change=
              () => inject(UpdateSettings(Performance(Toggle_cell_view))),
            ~disabled=!performance.measure,
            performance.cell_view,
          ),
          labeled_checkbox(
            ~id="measure_page_view",
            ~classes=["indented-option"],
            ~label="page_view",
            ~on_change=
              () => inject(UpdateSettings(Performance(Toggle_page_view))),
            ~disabled=!performance.measure,
            performance.page_view,
          ),
          labeled_checkbox(
            ~id="measure_hazel_create",
            ~classes=["indented-option"],
            ~label="hazel_create",
            ~on_change=
              () =>
                inject(UpdateSettings(Performance(Toggle_hazel_create))),
            ~disabled=!performance.measure,
            performance.hazel_create,
          ),
          labeled_checkbox(
            ~id="measure_update_apply_action",
            ~classes=["indented-option"],
            ~label="update_apply_action",
            ~on_change=
              () =>
                inject(
                  UpdateSettings(Performance(Toggle_update_apply_action)),
                ),
            ~disabled=!performance.measure,
            performance.update_apply_action,
          ),
          //
          labeled_checkbox(
            ~id="memoize_doc",
            ~label="Memoize doc generation",
            ~on_change=() => inject(UpdateSettings(Toggle_memoize_doc)),
            memoize_doc,
          ),
        ],
      )
    );
  details(
    [],
    [summary([], [Vdom.Node.text("Options")]), evaluation_checkboxes],
  );
};
