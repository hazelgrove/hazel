open Virtual_dom.Vdom;
open Prompt;

let rank_selection_handler = (x, id) => {
  // update_chosen_rank
  let printing: string = String.concat(" ", [id, x]);
  print_endline(printing);
  Event.Many([]);
};

let a_single_example_expression =
    (example_id: string, example_body: string, ranking_out_of: int) => {
  let (msg, _) = CodeSummary.build_msg(example_body, false);
  [
    Node.div(
      [Attr.name("question_wrapper"), Attr.class_("question_wrapper")],
      [
        Node.select(
          [
            Attr.name(example_id),
            Attr.on_change((_, xx) =>
              rank_selection_handler(xx, example_id)
            ),
          ],
          CodeExplanation_common.rank_list(1 + ranking_out_of),
        ),
        Node.div([], msg),
      ],
    ),
  ];
};

// Generate a ranked prompt for each explanation
let render_explanations = (explanations: list(Prompt.explain)): list(Node.t) => {
  List.flatten(
    List.map(
      i =>
        a_single_example_expression(
          i.id,
          i.expression,
          List.length(explanations),
        ),
      explanations,
    ),
  );
};

let view = (explanations: list(Prompt.explain)): Node.t => {
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation"])],
      [Node.div([], render_explanations(explanations))],
    );
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Code Explanation"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [
          Node.div(
            [],
            [
              Node.div(
                [Attr.classes(["right-panel-prompt"])],
                [Node.text("Rank the explanations below")],
              ),
              explanation_view,
              Node.div(
                [Attr.classes(["right-panel-textarea-div"])],
                [
                  Node.textarea(
                    [Attr.classes(["right-panel-textarea"])],
                    [Node.text("If none of the above please explain why")],
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
    ],
  );
};
