module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;

let checkbox =
    (
      ~id: string,
      ~label: string,
      ~on_change: unit => Vdom.Event.t,
      ~disabled=false,
      checked: bool,
    )
    : Vdom.Node.t => {
  Vdom.(
    Node.div(
      [],
      [
        Node.input(
          [
            [
              Attr.id(id),
              Attr.type_("checkbox"),
              Attr.on_change((_, _) => on_change()),
            ],
            checked ? [Attr.checked] : [],
            disabled ? [Attr.disabled] : [],
          ]
          |> List.concat,
          [],
        ),
        Node.label([Attr.for_(id)], [Node.text(label)]),
      ],
    )
  );
};

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let compute_results_checkbox =
    Vdom.(
      Node.div(
        [],
        [
          checkbox(
            ~id="compute_results",
            ~label="Compute expansion",
            ~on_change=() => inject(ToggleComputeResults),
            model.compute_results,
          ),
          checkbox(
            ~id="evaluate_expansion",
            ~label="Evaluate expansion",
            ~on_change=() => inject(ToggleEvaluateExpansion),
            ~disabled=!model.compute_results,
            model.evaluate_expansion,
          ),
          checkbox(
            ~id="show_contenteditable",
            ~label="Show contenteditable (debugging)",
            ~on_change=() => inject(ToggleEvaluateExpansion),
            model.show_contenteditable,
          ),
          checkbox(
            ~id="show_presentation",
            ~label="Show presentation (debugging)",
            ~on_change=() => inject(ToggleShowPresentation),
            model.show_presentation,
          ),
        ],
      )
    );
  compute_results_checkbox;
};
