module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;

let view = (~inject: ModelAction.t => Vdom.Event.t, model: Model.t) => {
  let (display, msg) = model.extraction;
  if (display) {
    switch (msg) {
    | OCamlExp(s) =>
      // Vdom.(
      //   Node.div(
      //     [Attr.classes(["extraction-result"])],
      //     [
      //       // header and close button
      //       Node.div(
      //         [Attr.classes(["extraction-header"])],
      //         [
      //           Node.text("Extraction Result"),
      //           Node.button(
      //             [
      //               Attr.on_click(_
      //                 // Event.Ignore;
      //                 =>
      //                   Vdom.Event.Many([
      //                     Event.Prevent_default,
      //                     Event.Stop_propagation,
      //                     inject(ModelAction.CloseExtraction),
      //                     inject(FocusCell),
      //                   ])
      //                 ),
      //               Attr.classes(["extraction-close"]),
      //             ],
      //             [Node.text("X")],
      //           ),
      //         ],
      //       ),
      //       // extraction content
      //       Node.text(s),
      //     ],
      //   )
      // )
      Vdom.(
        Node.div(
          [Attr.classes(["extraction-header"])],
          [
            // header and close button
            Node.span(
              [Attr.classes(["extraction-header-text"])],
              [Node.text("Extraction Result")],
            ),
            Node.button(
              [
                Attr.on_click(_ =>
                  Vdom.Event.Many([
                    Event.Prevent_default,
                    Event.Stop_propagation,
                    inject(ModelAction.CloseExtraction),
                    inject(FocusCell),
                  ])
                ),
                Attr.classes(["extraction-close"]),
              ],
              [Node.text("X")],
            ),
            Node.div(
              [Attr.classes(["extraction-result"])],
              [
                // extraction content
                Node.text(s),
              ],
            ),
          ],
        )
      )
    | ExtractionFailed(s) =>
      Vdom.(
        Node.div(
          [Attr.classes(["extraction-header"])],
          [
            // header and close button
            Node.span(
              [Attr.classes(["extraction-header-text"])],
              [Node.text("Extraction Result")],
            ),
            Node.button(
              [
                Attr.on_click(_ =>
                  Vdom.Event.Many([
                    Event.Prevent_default,
                    Event.Stop_propagation,
                    inject(ModelAction.CloseExtraction),
                    inject(FocusCell),
                  ])
                ),
                Attr.classes(["extraction-close"]),
              ],
              [Node.text("X")],
            ),
            Node.div(
              [Attr.classes(["extraction-error"])],
              [
                // extraction content
                Node.text(s),
              ],
            ),
          ],
        )
      )
    };
  } else {
    Vdom.(Node.div([], []));
  };
};
