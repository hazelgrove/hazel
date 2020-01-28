module Vdom = Virtual_dom.Vdom;

let checkbox =
    (
      ~inject: Update.Action.t => Vdom.Event.t,
      id: string,
      text: string,
      default: bool,
      action: bool => Update.Action.t,
    )
    : Vdom.Node.t => {
  Vdom.(
    Node.div(
      [],
      [
        Node.input(
          (
            if (default) {
              [Attr.checked];
            } else {
              [];
            }
          )
          @ [
            Attr.type_("checkbox"),
            Attr.id(id),
            Attr.on_change((evt, _) => {
              let target =
                JSUtil.force_opt(
                  Js_of_ocaml.Dom_html.CoerceTo.input(
                    JSUtil.force_opt(evt##.target),
                  ),
                );
              let checked = Js_of_ocaml.Js.to_bool(target##.checked);
              inject(action(checked));
            }),
          ],
          [],
        ),
        Node.label([Attr.for_(id)], [Node.text(text)]),
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
            ~inject,
            "compute_results",
            "Compute Results",
            model.compute_results,
            checked =>
            Update.Action.SetComputeResults(checked)
          ),
          checkbox(
            ~inject,
            "show_content_editable",
            "Show Content Editable (Debugging)",
            model.show_content_editable,
            checked =>
            Update.Action.SetShowContentEditable(checked)
          ),
          checkbox(
            ~inject,
            "show_presentation",
            "Show Presentation (Debugging)",
            model.show_presentation,
            checked =>
            Update.Action.SetShowPresentation(checked)
          ),
        ],
      )
    );
  compute_results_checkbox;
};
