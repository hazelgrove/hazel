module Vdom = Virtual_dom.Vdom;

let view = (~inject: Update.Action.t => Vdom.Event.t, _: Model.t): Vdom.Node.t => {
  let compute_results_checkbox =
    Vdom.(
      Node.div(
        [],
        [
          Node.input(
            [
              Attr.create("type", "checkbox"),
              Attr.id("compute_results_checkbox"),
              Attr.on_change((evt, _) => {
                let target =
                  JSUtil.force_opt(
                    Js_of_ocaml.Dom_html.CoerceTo.input(
                      JSUtil.force_opt(evt##.target),
                    ),
                  );
                let checked = Js_of_ocaml.Js.to_bool(target##.checked);
                inject(Update.Action.SetComputeResults(checked));
              }),
            ],
            [],
          ),
          Node.label(
            [Attr.create("for", "compute_results_checkbox")],
            [Node.text("Compute Results")],
          ),
        ],
      )
    );
  compute_results_checkbox;
};
