open Virtual_dom.Vdom;
// open Virtual_dom.Vdom.Event;
open Prompt;

/*
 let ranking_select = ((name: string, e: int)): RankedOptionInfo.t => {
   name,
   caption: Virtual_dom.Vdom.Node.div([], []),
   init_value: e,
 };
 */
/*let rankings = [
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
  ];
  */
/*
 let ranking: RankedPromptInfo.t = {
   title: "rank this",
   cards: List.map(ranking_select, ranking),
 };
 */

let text_box_handler = (~inject, new_text) => {
  // update_chosen_rank
  Event.Many([
    inject(
      ModelAction.UpdateDocumentationStudySettings(
        DocumentationStudySettings.Update_Prompt_Text(Example, new_text),
      ),
    ),
  ]);
};

let rank_selection_handler = (inject, index, new_rank) => {
  // update_chosen_rank
  Event.Many([
    inject(
      ModelAction.UpdateDocumentationStudySettings(
        DocumentationStudySettings.Update_Prompt(Example, index, new_rank),
      ),
    ),
    inject(FocusCell),
  ]);
};

// let rank_selection_handler = (inject, x, id) => {
//   // update_chosen_rank
//   let printing: string = String.concat(" ", [id, x]);
//   print_endline(printing);
//   Event.Many([inject(ModelAction.FocusCell)]);
// };

// let update_chosen_rank = (u: update, settings: t) =>
//   switch (u) {
//   | Toggle_memoize_doc => {...settings, memoize_doc: !settings.memoize_doc}
//   | Evaluation(u) => {
//       ...settings,
//       evaluation: Evaluation.apply_update(u, settings.evaluation),
//     }
//   | Performance(u) => {
//       ...settings,
//       performance: Performance.apply_update(u, settings.performance),
//     }
//   | RightPanel(u) => {
//       ...settings,
//       right_panel: RightPanel.apply_update(u, settings.right_panel),
//     }
//   };

// let rank_list = [
//   Node.option([Attr.value("1")], [Node.text("1")]),
//   Node.option([Attr.value("2")], [Node.text("2")]),
// ];

// TODO make sure this is of type exampleExpression
let a_single_example_expression_ =
    (
      ~inject,
      example_id: string,
      caption: string,
      example_body: Node.t,
      ranking_out_of: int,
      result: Node.t,
      index: int,
      hovered_over_example: int,
      rank: int,
    ) => {
  let more_info =
    if (index == hovered_over_example) {
      [
        Node.div(
          [Attr.class_("example_result")],
          [Node.div([], [Node.text("Result: ")]), result],
        ),
        Node.div(
          [Attr.class_("example_explanation")],
          [Node.text("Explanation: "), Node.text(caption)],
        ),
      ];
    } else {
      [];
    };

  [
    Node.div(
      [
        Attr.name("question_wrapper"),
        Attr.class_("question_wrapper"),
        Attr.style(Css_gen.create(~field="float", ~value="left-block")),
      ],
      [
        Node.select(
          [
            Attr.name(example_id),
            Attr.style(Css_gen.create(~field="float", ~value="left-block")),
            Attr.on_change((_, new_rank) =>
              rank_selection_handler(inject, index, int_of_string(new_rank))
            ),
          ],
          CodeExplanation_common.rank_list(ranking_out_of, rank),
        ),
        Node.div(
          [
            Attr.on_click(_ => {
              Event.Many([
                inject(
                  ModelAction.UpdateDocumentationStudySettings(
                    DocumentationStudySettings.Toggle_Example_Hovered_over(
                      index,
                    ),
                  ),
                ),
                inject(FocusCell),
              ])
            }),
            Attr.on_mouseleave(_ => {
              Event.Many([
                inject(
                  ModelAction.UpdateDocumentationStudySettings(
                    DocumentationStudySettings.Toggle_Example_Hovered_over(
                      -1,
                    ),
                  ),
                ),
                inject(FocusCell),
              ])
            }),
          ],
          [example_body, ...more_info],
        ),
      ],
    ),
  ];
};

// One instance of a an example

// TODO need to call DHCode.view() to display this

// Create a list of examples
// TODO: this may have to be a fixed size of defined by a parameter
// let examples_with_id_list = ranking_out_of => {
//   [
//     question_with_id("quest_id_1", "quest_body 1", ranking_out_of),
//     question_with_id("quest_id_2", "quest_body 2", ranking_out_of),
//   ];
// };
// // make them into a list
// let examples_ = list_of_examples => {
//   List.flatten(List.map(i => i.expression, list_of_examples));
// };
// let displayed_examples = examples_(examples_with_id_list(6));

// Takes a list of Prompt.explain and returns a displayable list of Nodes
let display_examples =
    (
      ~inject,
      ~settings,
      selected_instance,
      font_metrics,
      width,
      pos,
      examples_list_: list(quest),
      hovered_over_example,
    ) => {
  List.flatten(
    List.mapi(
      (index, e) =>
        a_single_example_expression_(
          ~inject,
          e.idz,
          e.caption,
          UHCode.basic_view(~settings, ~width, e.expressionz),
          List.length(examples_list_),
          DHCode.view(
            ~inject,
            ~settings=settings.evaluation,
            ~selected_instance,
            ~font_metrics,
            ~width,
            ~pos,
            e.result,
          ),
          index,
          hovered_over_example,
          e.rankz,
        ),
      examples_list_,
    ),
  );
};

// TODO - Ensure correctness of the below.
// TODO - may have to fix width
let view =
    (
      ~inject: ModelAction.t => Virtual_dom.Vdom.Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      example_list: list(Prompt.quest),
      hovered_over_example: int,
      free_response: string,
    )
    : Node.t => {
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation"])],
      [
        Node.div(
          [],
          display_examples(
            ~inject,
            ~settings,
            None,
            font_metrics,
            100,
            0,
            example_list,
            hovered_over_example,
          ),
        ),
      ],
    );
  };

  // TODO implement this top level function to generate and display examples
  let free_response =
    free_response == ""
      ? "Please list any other options that you would have preferred"
      : free_response;
  print_endline("Free response: " ++ free_response);
  print_endline(string_of_bool(free_response == ""));
  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Code Example"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [
          Node.div(
            [],
            [
              Node.div(
                [Attr.classes(["right-panel-prompt"])],
                [Node.text("Rank w.r.t. the code and syntactic form")],
              ),
              Node.div(
                [Attr.classes(["right-panel-responses"])],
                [explanation_view],
              ),
            ],
          ),
        ],
      ),
      Node.div(
        [Attr.classes(["right-panel-textarea-div"])],
        [
          Node.textarea(
            [
              Attr.string_property("value", free_response),
              Attr.classes(["right-panel-textarea"]),
              Attr.on_input((_, new_rank) =>
                text_box_handler(~inject, new_rank)
              ),
            ],
            [],
          ),
        ],
      ),
    ],
  );
};
