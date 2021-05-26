open Virtual_dom.Vdom;

let view = (): Node.t => {
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation"])],
      [
        Node.div(
          [Attr.classes(["context-is-empty-msg"])],
          [Node.text("No explanation")],
        ),
      ],
    );
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Explanation"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [explanation_view],
      ),
    ],
  );
};
