open Virtual_dom.Vdom;
open Node;

let icon = (~sort: TermSort.t) => {
  let base = s => "imgs/assistant/boost-0000-" ++ s ++ ".png";
  let sort = TermSort.to_string(sort);
  Node.div(
    [Attr.classes(["clickable-help-icon", sort])],
    [create("img", [Attr.create("src", base(sort))], [])],
  );
};

let category_view = category => {
  let label = Suggestion.string_of_category(category);
  div([Attr.classes(["category", label])], [text(label)]);
};

let sign_icon = n =>
  text(
    switch (n) {
    | n when n < 0 => "-"
    | n when n > 0 => "+"
    | _ => ""
    },
  );
let sign_label = n =>
  switch (n) {
  | n when n < 0 => "minus"
  | n when n > 0 => "plus"
  | _ => ""
  };

let delta_errors_string = (delta_errors: int): string =>
  switch (delta_errors) {
  | 0 => "Same number of errors"
  | (-1) => "One more error"
  | 1 => "One fewer error"
  | n when n < 0 => "" ++ string_of_int(- n) ++ " more errors"
  | n => "" ++ string_of_int(n) ++ " fewer errors"
  };

let type_specificity_string = (type_specificity: int) =>
  switch (type_specificity) {
  | 0 => "Same type specificity"
  | n when n < 0 => "Less granular type"
  | _ => "More granular type"
  };
let idiomaticity_string = (idiomaticity: int) =>
  switch (idiomaticity) {
  | 0 => "Same idiomaticity"
  | (-1) => "Less idiomatic"
  | 1 => "More idiomatic"
  | n when n < 0 => "Much less idiomatic"
  | _ => "Much more idiomatic"
  };

let text_match_string = (ratio: float) =>
  switch (ratio) {
  | x when x < 0.6 => "Syntax partially conserved"
  | x when x < 0.9 => "Syntax mostly conserved"
  | _ => "Existing syntax conserved"
  };

let sign_view = n => div([Attr.class_(sign_label(n))], [sign_icon(n)]);

let subscore_view = (score_string: int => string, subscore: int) => {
  switch (subscore) {
  | 0 => []
  | _ => [
      div(
        [Attr.class_("subscore")],
        [sign_view(subscore), text(score_string(subscore))],
      ),
    ]
  };
};

let subscore_view_float = (score_string: float => string, subscore: float) => {
  switch (subscore) {
  | 0. => []
  | _ => [
      div(
        [Attr.class_("subscore")],
        [sign_view(1), text(score_string(subscore))],
      ),
    ]
  };
};

let suggestion_info_view = ({category, score, _}: Suggestion.exp) => {
  div(
    [Attr.class_("suggestion-info")],
    [
      span([], [category_view(category)]),
      span(
        [Attr.class_("suggestion-description")],
        [text(Suggestion.description_of_category(category))],
      ),
    ]
    @ subscore_view(delta_errors_string, score.delta_errors)
    @ subscore_view(idiomaticity_string, score.idiomaticity)
    @ subscore_view(type_specificity_string, score.type_specificity)
    //@ [text(string_of_float(score.text_match))]
    @ subscore_view_float(text_match_string, score.text_match),
  );
};

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
    switch (SuggestionScore.submatches_and_offsets(pre, suf, result_text)) {
    | (None, None) => []
    | (Some((s0, n0)), Some((s1, n1))) =>
      let n1' = n1 - (n0 + String.length(s0));
      overlay(n0, s0) @ overlay(n1', s1);
    | (Some((s, n)), _)
    | (_, Some((s, n))) => overlay(n, s)
    };
  div([Attr.class_("overlay")], offset_overlay);
};

let suggestion_view =
    (
      ~ci: CursorInfo.t,
      ~inject: ModelAction.t => Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      {action, result, result_ty, category, result_text, score, _}: Suggestion.exp,
      is_selected: bool,
      is_hovered: bool,
      index: int,
      search_string: string,
    ) => {
  let result_view =
    UHCode.codebox_view(
      ~is_focused=false,
      ~settings,
      ~font_metrics,
      Editor.mk_exp_editor(result),
    );
  let overlay_view = overlay_view(ci, search_string, result_text);
  let perform_action = _ =>
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(FocusCell(MainProgram)), // prevent main editor from losing focus
      inject(ModelAction.AcceptSuggestion(action)),
    ]);
  let set_hover = _ =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(Some(index))));
  let unset_hover = _ =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(None)));
  let color_score =
    score.delta_errors + score.idiomaticity + score.type_specificity;
  let assistant_classes =
    ["choice"]
    @ (is_selected ? ["selected"] : [])
    @ (is_hovered ? ["hovered"] : [])
    @ (color_score > 0 ? ["errors-less"] : [])
    @ (color_score < 0 ? ["errors-more"] : []);
  div(
    [
      Attr.id(string_of_int(index)),
      Attr.classes(assistant_classes),
      Attr.create("tabindex", "0"), // necessary to make cell focusable
      Attr.on_click(perform_action),
      Attr.on_mouseenter(set_hover),
      Attr.on_mouseleave(unset_hover),
    ],
    [
      div(
        [Attr.class_("code-container")],
        [div([Attr.class_("code")], [overlay_view] @ result_view)],
      ),
      div([Attr.class_("type-ann")], [text(":")]),
      div([Attr.class_("type")], [HTypCode.view(result_ty)]),
      category_view(category),
    ],
  );
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
  let search_string = CursorInfo_common.string_of_cursor_term(ci.cursor_term);
  let suggestions =
    AssistantModel.get_display_suggestions(~u_gen, ci, assistant);
  let is_hovered = AssistantModel.is_active_suggestion_index(assistant);
  let suggestion_view = (index, suggestion) =>
    suggestion_view(
      ~ci,
      ~inject,
      ~settings,
      ~font_metrics,
      suggestion,
      index == 0,
      is_hovered(index),
      index,
      search_string,
    );
  div([Attr.id("assistant")], List.mapi(suggestion_view, suggestions));
};

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~u_gen: MetaVarGen.t,
      ~ci: CursorInfo.t,
      assistant: AssistantModel.t,
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
