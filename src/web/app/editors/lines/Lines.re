//open Haz3lcore;
//open Virtual_dom.Vdom;
open Util;
open WebUtil;

/*
 Used to display line numbering alongside cells
 */

// No need for a Model or Update, just compute from the model

module View = {
  let view = (code_view: Node.t) => {
    Node.div(
      ~attrs=[Attr.classes(["cell-item", "code-editor"])],
      [
        div_c(
          "code",
          [span_c("code-text", [Text("1\n2\n3\n4")]: list(Node.t))],
        ),
      ]
      @ [code_view],
    );
  };
};
