open Virtual_dom.Vdom;
open Node;

let string_of_operand_strategy: Suggestion.operand_strategy => string =
  fun
  | Delete => "del"
  | InsertLit => "lit"
  | InsertVar => "var"
  | InsertApp => "app"
  | InsertCase => "elm"
  | WrapApp => "wra"
  | WrapCase => "wra"
  | ConvertLit => "con";

let string_of_strategy: Suggestion.strategy => string =
  fun
  | ReplaceOperand(os, _) => string_of_operand_strategy(os);

let description_of_operand_strategy: Suggestion.operand_strategy => string =
  fun
  | Delete => "Delete the current form"
  | InsertLit => "Insert a literal"
  | InsertVar => "Insert a variable"
  | InsertApp => "Insert an application"
  | InsertCase => "Insert an eliminator"
  | WrapApp => "Wrap the current form in an application"
  | WrapCase => "Wrap the current form in a case"
  | ConvertLit => "Convert a literal to another data type";

let description_of_strategy: Suggestion.strategy => string =
  fun
  | ReplaceOperand(os, _) => description_of_operand_strategy(os);

let delta_errors_string = (delta_errors: float): string =>
  switch (delta_errors) {
  | 0. => "Same number of errors"
  | (-1.) => "One more error"
  | 1. => "One fewer error"
  | n when n < 0. =>
    "" ++ string_of_int(int_of_float(-. n)) ++ " more errors"
  | n => "" ++ string_of_int(int_of_float(n)) ++ " fewer errors"
  };

let type_specificity_string = (type_specificity: float) =>
  switch (type_specificity) {
  | 0. => "Same type specificity"
  | n when n < 0. => "Less granular type"
  | _ => "More granular type"
  };
let idiomaticity_string = (idiomaticity: float) =>
  switch (idiomaticity) {
  | 0. => "Same idiomaticity"
  | (-1.) => "Less idiomatic"
  | 1. => "More idiomatic"
  | n when n < 0. => "Much less idiomatic"
  | _ => "Much more idiomatic"
  };

let syntax_conserved_string = (ratio: float) =>
  switch (ratio) {
  | x when x < 0.6 => "Syntax partially conserved"
  | x when x < 0.9 => "Syntax mostly conserved"
  | _ => "Existing syntax conserved"
  };

let sign_label = (n: float): string =>
  switch (n) {
  | n when n < 0. => "minus"
  | n when n > 0. => "plus"
  | _ => ""
  };

let sign_string = (n: float): string =>
  switch (n) {
  | n when n < 0. => "-"
  | n when n > 0. => "+"
  | _ => ""
  };

let icon = (~sort: TermSort.t) => {
  let base = s => "imgs/assistant/boost-0000-" ++ s ++ ".png";
  let sort = TermSort.to_string(sort);
  Node.div(
    [Attr.classes(["clickable-help-icon", sort])],
    [create("img", [Attr.create("src", base(sort))], [])],
  );
};

let strategy_view = strategy => {
  let label = string_of_strategy(strategy);
  div([Attr.classes(["category", label])], [text(label)]);
};

let sign_view = n =>
  div([Attr.class_(sign_label(n))], [text(sign_string(n))]);

let subscore_view = ((subscore: float, score_string: float => string)) =>
  switch (subscore) {
  | 0. => []
  | _ => [
      div(
        [Attr.class_("subscore")],
        [sign_view(subscore), text(score_string(subscore))],
      ),
    ]
  };

let subscore_data_exp = (score: SuggestionReportExp.scores) => [
  (score.delta_errors, delta_errors_string),
  (score.idiomaticity, idiomaticity_string),
  (score.type_specificity, type_specificity_string),
  (score.syntax_conserved, syntax_conserved_string),
];

let suggestion_info_view = ({strategy, _} as s: Suggestion.t) =>
  switch (s.report) {
  | ExpOperand({scores, _}) =>
    div(
      [Attr.class_("suggestion-info")],
      [
        span([], [strategy_view(strategy)]),
        span(
          [Attr.class_("suggestion-description")],
          [text(description_of_strategy(strategy))],
        ),
      ]
      @ List.concat(List.map(subscore_view, subscore_data_exp(scores))),
    )
  };

/* Draws the matching characters overtop of suggestions */
let overlay_view =
    (
      {cursor_term, _}: CursorInfo.t,
      search_string: string,
      result_text: string,
    ) => {
  let index =
    switch (cursor_term) {
    | ExpOperand(OnText(i), _) => i
    | _ => String.length(search_string)
    };
  let (pre, suf) = StringUtil.split_string(index, search_string);
  let overlay = (n, s) => [
    text(String.make(n, ' ')),
    span([Attr.class_("overlay-text")], [text(s)]),
  ];
  let offset_overlay =
    switch (SuggestionReportExp.submatches_and_offsets(pre, suf, result_text)) {
    | (None, None) => []
    | (Some((s0, n0)), Some((s1, n1))) =>
      let n1' = n1 - (n0 + String.length(s0));
      overlay(n0, s0) @ overlay(n1', s1);
    | (Some((s, n)), _)
    | (_, Some((s, n))) => overlay(n, s)
    };
  div([Attr.class_("overlay")], offset_overlay);
};

let suggestion_view_exp_operand =
    (
      ~suggestion as {action, strategy, _} as suggestion: Suggestion.t,
      ~report as
        {show_text, operand, ty, _}: SuggestionReportExp.report_operand,
      ~index: int,
      ~is_hovered: bool,
      ~is_selected: bool,
      ~search_string: string,
      ~ci: CursorInfo.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~inject: ModelAction.t => Event.t,
    ) => {
  let result_view =
    UHCode.codebox_view(
      ~is_focused=false,
      ~settings,
      ~font_metrics,
      Editor.mk_exp_editor(UHExp.Block.wrap(operand)),
    );
  let overlay_view = overlay_view(ci, search_string, show_text);
  let perform_action = _ =>
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(FocusCell(MainProgram)), // prevent main editor from losing focus
      inject(ModelAction.AcceptSuggestion(action)),
    ]);
  let set_hover_index = (idx: option(int)) =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(idx)));
  let color_score = Suggestion.score(suggestion);
  let assistant_classes =
    ["choice"]
    @ (is_selected ? ["selected"] : [])
    @ (is_hovered ? ["hovered"] : [])
    @ (color_score > 0. ? ["errors-less"] : [])
    @ (color_score < 0. ? ["errors-more"] : []);
  div(
    [
      Attr.id(string_of_int(index)),
      Attr.classes(assistant_classes),
      Attr.create("tabindex", "0"), // necessary to make cell focusable
      Attr.on_click(perform_action),
      Attr.on_mouseenter(_ => set_hover_index(Some(index))),
      Attr.on_mouseleave(_ => set_hover_index(None)),
    ],
    [
      div(
        [Attr.class_("code-container")],
        [div([Attr.class_("code")], [overlay_view] @ result_view)],
      ),
      div([Attr.class_("type-ann")], [text(":")]),
      div([Attr.class_("type")], [HTypCode.view(ty)]),
      strategy_view(strategy),
    ],
  );
};

let suggestion_view =
    (
      ~suggestion: Suggestion.t,
      ~index: int,
      ~is_hovered: bool,
      ~is_selected: bool,
      ~search_string: string,
      ~ci: CursorInfo.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~inject: ModelAction.t => Event.t,
    ) =>
  switch (suggestion.report) {
  | ExpOperand(report) =>
    suggestion_view_exp_operand(
      ~report,
      ~ci,
      ~inject,
      ~settings,
      ~font_metrics,
      ~suggestion,
      ~is_selected,
      ~is_hovered,
      ~index,
      ~search_string,
    )
  };

let suggestions_view =
    (
      ~inject: ModelAction.t => Event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~u_gen: MetaVarGen.t,
      ~assistant: AssistantModel.t,
      ~ci: CursorInfo.t,
    )
    : Node.t => {
  let suggestions =
    AssistantModel.get_display_suggestions(~u_gen, ci, assistant);
  let suggestion_view = (index, suggestion) =>
    suggestion_view(
      ~ci,
      ~inject,
      ~settings,
      ~font_metrics,
      ~suggestion,
      ~is_selected=index == 0,
      ~is_hovered=AssistantModel.is_active_suggestion_index(assistant, index),
      ~index,
      ~search_string=CursorInfo_common.string_of_cursor_term(ci.cursor_term),
    );
  div([Attr.id("assistant")], List.mapi(suggestion_view, suggestions));
};

let view =
    (
      ~assistant: AssistantModel.t,
      ~ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~inject: ModelAction.t => Event.t,
    )
    : Node.t => {
  let suggestions_view =
    suggestions_view(
      ~inject,
      ~font_metrics,
      ~settings,
      ~u_gen,
      ~assistant,
      ~ci,
    );
  let suggestion_info_view =
    switch (AssistantModel.get_indicated_suggestion(~u_gen, assistant, ci)) {
    | Some(suggestion) when AssistantModel.is_hovering(assistant) => [
        suggestion_info_view(suggestion),
      ]
    | _ => []
    };
  div(
    [Attr.id("assistant-wrapper")],
    [suggestions_view] @ suggestion_info_view,
  );
};
