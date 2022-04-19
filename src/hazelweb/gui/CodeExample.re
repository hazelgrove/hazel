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

let rank_selection_handler = (x, id) => {
  // update_chosen_rank
  let printing: string = String.concat(" ", [id, x]);
  print_endline(printing);
  Event.Many([]);
};

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
    (example_id: string, example_body: Node.t, ranking_out_of: int) => {
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
            Attr.on_change((_, xx) =>
              rank_selection_handler(xx, example_id)
            ),
          ],
          CodeExplanation_common.rank_list(1 + ranking_out_of),
        ),
        example_body,
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
let display_examples = (~settings, width, examples_list_: list(quest)) => {
  List.flatten(
    List.map(
      e =>
        a_single_example_expression_(
          e.idz,
          UHCode.basic_view(~settings, ~width, e.expressionz),
          List.length(examples_list_),
        ),
      examples_list_,
    ),
  );
};

// TODO - Ensure correctness of the below.
// TODO - may have to fix width
let view = (~settings: Settings.t, example_list: list(Prompt.quest)): Node.t => {
  let explanation_view = {
    Node.div(
      [Attr.classes(["the-explanation"])],
      [Node.div([], display_examples(~settings, 100, example_list))],
    );
  };

  // TODO implement this top level function to generate and display examples

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Code Example"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [explanation_view],
      ),
    ],
  );
};
