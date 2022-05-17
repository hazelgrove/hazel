open Virtual_dom.Vdom;
open Prompt;

let rank_selection_handler = (~inject, index, new_rank) => {
  // update_chosen_rank
  Event.Many([
    inject(
      ModelAction.UpdateDocumentationStudySettings(
        DocumentationStudySettings.Update_Prompt(
          Explanation,
          index,
          new_rank,
        ),
      ),
    ),
  ]);
};

let a_single_example_expression =
    (
      ~settings: DocumentationStudySettings.t,
      ~inject,
      example_id: string,
      example_body: string,
      ranking_out_of: int,
      index: int,
    ) => {
  let (msg, _) =
    CodeExplanation_common.build_msg(
      example_body,
      settings.hovered_over == index,
    );
  [
    Node.div(
      [
        Attr.name("question_wrapper"),
        Attr.class_("question_wrapper"),
        Attr.on_mouseenter(_ => {
          inject(
            ModelAction.UpdateDocumentationStudySettings(
              DocumentationStudySettings.Toggle_Explanation_Hovered_over(
                index,
              ),
            ),
          )
        }),
        Attr.on_mouseleave(_ => {
          inject(
            ModelAction.UpdateDocumentationStudySettings(
              DocumentationStudySettings.Toggle_Explanation_Hovered_over(-1),
            ),
          )
        }),
      ],
      [
        Node.select(
          [
            Attr.name(example_id),
            Attr.on_change((_, new_rank) =>
              rank_selection_handler(~inject, index, int_of_string(new_rank))
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
let render_explanations =
    (
      ~settings: DocumentationStudySettings.t,
      ~inject,
      explanations: list(Prompt.explain),
    )
    : list(Node.t) => {
  List.flatten(
    List.mapi(
      (index, i) =>
        a_single_example_expression(
          ~settings,
          ~inject,
          i.id,
          i.expression,
          List.length(explanations),
          index,
        ),
      explanations,
    ),
  );
};

let get_mapping = (~settings: DocumentationStudySettings.t): ColorSteps.t => {
  switch (settings.hovered_over, settings.prompt) {
  | ((-1), _)
  | (_, None) => ColorSteps.empty
  | (index, Some(prompt)) =>
    let example_body =
      List.nth(List.nth(settings.prompts, prompt).explanation, index).
        expression;
    let (_, mapping) = CodeExplanation_common.build_msg(example_body, true);
    mapping;
  };
};

let view =
    (
      ~settings: DocumentationStudySettings.t,
      ~inject: ModelAction.t => Event.t,
      explanations: list(Prompt.explain),
    )
    : Node.t => {
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation"])],
      [Node.div([], render_explanations(~settings, ~inject, explanations))],
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
