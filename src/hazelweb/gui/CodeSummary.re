open Virtual_dom.Vdom;

let view = (): Node.t => {
  let summary_view = {
    Node.div(
      [Attr.classes(["the-summary"])],
      [
        Node.div(
          [Attr.classes(["context-is-empty-msg"])],
          [Node.text("No summary")],
        ),
      ],
    );
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Summary"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [summary_view],
      ),
    ],
  );
};
