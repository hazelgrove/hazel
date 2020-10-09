module Js = Js_of_ocaml.Js;
module Vdom = Virtual_dom.Vdom;

let view = (model: Model.t) => {
  let (display, msg) = model.extraction;
  if (display) {
    switch (msg) {
    | OCamlExp(s) =>
      Vdom.(
        Node.div([Attr.classes(["extraction-result"])], [Node.text(s)])
      )
    | ExtractionFailed(s) =>
      Vdom.(
        Node.div([Attr.classes(["extraction-error"])], [Node.text(s)])
      )
    };
  } else {
    Vdom.(Node.div([], []));
  };
};
