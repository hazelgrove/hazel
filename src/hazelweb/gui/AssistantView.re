open Virtual_dom.Vdom;
open Node;

let category_view = category => {
  let label = Suggestion.string_of_category(category);
  div([Attr.classes(["category", label])], [text(label)]);
};

let delta_errors_info_view = (delta_errors: int) => {
  let str =
    switch (delta_errors) {
    | 0 => "Results in no change in the number of errors"
    | (-1) => "Results in one more error"
    | 1 => "Results in one fewer error"
    | n when n < 0 => "Results in " ++ string_of_int(- n) ++ " more errors"
    | n => "Results in " ++ string_of_int(n) ++ " fewer errors"
    };
  switch (delta_errors) {
  | 0 => []
  | _ => [div([], [text(str)])]
  };
};

let type_specificity_info_view = (type_specificity: int) => {
  let str =
    switch (type_specificity) {
    | 0 => "Results in no change in type specificity"
    | n when n < 0 => "Results in a less specific type"
    | _ => "Results in a more specific type"
    };
  switch (type_specificity) {
  | 0 => []
  | _ => [div([], [text(str)])]
  };
};

let idiomaticity_info_view = (idiomaticity: int) => {
  let str =
    switch (idiomaticity) {
    | 0 => "Results in no change in idiomaticity"
    | (-1) => "Result is less idiomatic"
    | 1 => "Results is more idiomatic"
    | n when n < 0 => "Results is much less idiomatic"
    | _ => "Results is much more idiomatic"
    };
  switch (idiomaticity) {
  | 0 => []
  | _ => [div([], [text(str)])]
  };
};

let suggestion_info_view = ({category, score, _}: Assistant_Exp.suggestion) => {
  div(
    [Attr.classes(["suggestion-info"])],
    [
      category_view(category),
      span([], [text(Suggestion.description_of_category(category))]),
    ]
    @ delta_errors_info_view(score.delta_errors)
    @ idiomaticity_info_view(score.idiomaticity)
    @ type_specificity_info_view(score.type_specificity),
  );
};

let suggestion_view =
    (
      ~ci as {cursor_term, _}: CursorInfo.t,
      ~inject: ModelAction.t => Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      {action, result, res_ty, category, result_text, score, _}: Assistant_Exp.suggestion,
      is_selected: bool,
      search_string: string,
    ) => {
  let perform_action = _ =>
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(ModelAction.AcceptSuggestion(action)),
    ]);

  let index =
    switch (cursor_term) {
    | ExpOperand(OnText(i), _) => i
    | _ => String.length(search_string)
    };
  // split string at caret, only use before caret portion to search
  let (before_caret, after_caret) =
    StringUtil.split_string(index, search_string);
  let match_string =
    StringUtil.match_prefix(before_caret, result_text) ? before_caret : "";
  let match_len = String.length(match_string);
  let unmatched_remainder =
    String.sub(
      result_text,
      match_len,
      String.length(result_text) - match_len,
    );
  let match_string2 =
    StringUtil.match_prefix(after_caret, unmatched_remainder)
      ? after_caret : "";
  let match_string = match_string ++ match_string2;
  let overlay_view =
    div([Attr.classes(["overlay"])], [text(match_string)]);
  let result_view =
    UHCode.codebox_view(
      ~is_focused=false,
      ~settings,
      ~font_metrics,
      Program.mk_exp_editor(result),
    );
  let error_str =
    switch (score.delta_errors) {
    | 1 => "+"
    | 2 => "++"
    | n when n > 2 => "+++"
    | (-1) => "-"
    | (-2) => "--"
    | n when n < (-2) => "---"
    | _ => ""
    };
  let color_score =
    score.delta_errors /*+ score.idiomaticity + score.type_specificity*/;
  div(
    [
      Attr.classes(
        ["choice"]
        @ (is_selected ? ["selected"] : [])
        @ (color_score > 0 ? ["errors-less"] : [])
        @ (color_score < 0 ? ["errors-more"] : []),
      ),
      Attr.on_click(perform_action),
    ],
    [
      div(
        [Attr.classes(["delta-errors"])],
        [
          text(error_str),
          text(string_of_int(score.idiomaticity + score.type_specificity)),
        ],
      ),
      div(
        [Attr.classes(["code-container"])],
        [div([Attr.classes(["code"])], [overlay_view] @ result_view)],
      ),
      div([Attr.classes(["type-ann"])], [text(" : ")]),
      div([Attr.classes(["type"])], [HTypCode.view(res_ty)]),
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
      ~assistant_model: AssistantModel.t,
      ~ci: CursorInfo.t,
    )
    : Node.t => {
  let (filter_string, _) =
    CursorInfo_common.string_and_index_of_cursor_term(ci.cursor_term);
  let suggestions =
    AssistantModel.get_display_suggestions(~u_gen, ci, assistant_model);
  let suggestion_view = (i, a) =>
    suggestion_view(
      ~ci,
      ~inject,
      ~settings,
      ~font_metrics,
      a,
      i == 0,
      filter_string,
    );
  div([Attr.id("assistant")], List.mapi(suggestion_view, suggestions));
};

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~u_gen: MetaVarGen.t,
      ~assistant_model: AssistantModel.t,
      ~ci: CursorInfo.t,
    )
    : Node.t => {
  let suggestions_view =
    suggestions_view(
      ~inject,
      ~font_metrics,
      ~settings,
      ~u_gen,
      ~assistant_model,
      ~ci,
    );
  let suggestion_info_view = {
    let s = AssistantModel.get_suggestion(~u_gen, assistant_model, ci);
    switch (s) {
    | Some(s) => suggestion_info_view(s)
    | None => Node.text("SDf")
    };
  };
  div(
    [Attr.id("assistant-wrapper")],
    [suggestions_view, suggestion_info_view],
  );
};
