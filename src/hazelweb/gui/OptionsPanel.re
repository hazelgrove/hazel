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

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
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
          labeled_checkbox(
            ~id="memoize_doc",
            ~label="Memoize doc generation",
            ~on_change=() => inject(ToggleMemoizeDoc),
            model.memoize_doc,
          ),
        ],
      )
    );
  compute_results_checkbox;
};
