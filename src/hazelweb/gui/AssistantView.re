open Virtual_dom.Vdom;
open Node;

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
  | n when n < 0 => "Less specific type"
  | _ => "More specific type"
  };
let idiomaticity_string = (idiomaticity: int) =>
  switch (idiomaticity) {
  | 0 => "Same idiomaticity"
  | (-1) => "Less idiomatic"
  | 1 => "More idiomatic"
  | n when n < 0 => "Much less idiomatic"
  | _ => "Much more idiomatic"
  };

let sign_view = n => {
  div([Attr.classes([sign_label(n)])], [sign_icon(n)]);
};

let subscore_view = (score_string: int => string, subscore: int) => {
  switch (subscore) {
  | 0 => []
  | _ => [
      div(
        [Attr.classes(["subscore"])],
        [sign_view(subscore), text(score_string(subscore))],
      ),
    ]
  };
};

let suggestion_info_view = ({category, score, _}: Assistant_Exp.suggestion) => {
  div(
    [Attr.classes(["suggestion-info"])],
    [
      span([], [category_view(category)]),
      span(
        [Attr.classes(["suggestion-description"])],
        [text(Suggestion.description_of_category(category))],
      ),
    ]
    @ subscore_view(delta_errors_string, score.delta_errors)
    @ subscore_view(idiomaticity_string, score.idiomaticity)
    @ subscore_view(type_specificity_string, score.type_specificity),
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
      is_hovered: bool,
      my_index: int,
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
  let match_string_after =
    StringUtil.match_prefix(after_caret, unmatched_remainder)
      ? after_caret : "";
  let match_string = match_string ++ match_string_after;
  let overlay_view =
    div([Attr.classes(["overlay"])], [text(match_string)]);
  let result_view =
    UHCode.codebox_view(
      ~is_focused=false,
      ~settings,
      ~font_metrics,
      Editor.mk_exp_editor(result),
    );
  let on_mouseenter = _x => {
    print_endline("setting focus to: Some(");
    print_endline(string_of_int(my_index));
    inject(ModelAction.UpdateAssistant(Set_hover_index(Some(my_index))));
  };
  let on_mouseleave = _x => {
    print_endline("setting focus to: None");
    inject(ModelAction.UpdateAssistant(Set_hover_index(None)));
  };
  /*
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
     */
  let color_score =
    score.delta_errors /*+ score.idiomaticity + score.type_specificity*/;
  div(
    [
      Attr.id(string_of_int(my_index)),
      Attr.classes(
        ["choice"]
        @ (is_selected ? ["selected"] : [])
        @ (is_hovered ? ["hovered"] : [])
        @ (color_score > 0 ? ["errors-less"] : [])
        @ (color_score < 0 ? ["errors-more"] : []),
      ),
      Attr.create("tabindex", "0"), // necessary to make cell focusable
      Attr.on_click(perform_action),
      Attr.on_mouseenter(on_mouseenter),
      Attr.on_mouseleave(on_mouseleave),
    ],
    [
      /*
       div(
         [Attr.classes(["delta-errors"])],
         [
           text(error_str),
           text(string_of_int(score.idiomaticity + score.type_specificity)),
         ],
       ),*/
      div(
        [Attr.classes(["code-container"])],
        [div([Attr.classes(["code"])], [overlay_view] @ result_view)],
      ),
      div([Attr.classes(["type-ann"])], [text(":")]),
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
      ~assistant: AssistantModel.t,
      ~ci: CursorInfo.t,
    )
    : Node.t => {
  let (filter_string, _) =
    CursorInfo_common.string_and_index_of_cursor_term(ci.cursor_term);
  let suggestions =
    AssistantModel.get_display_suggestions(~u_gen, ci, assistant);
  let is_hovered = AssistantModel.is_active_suggestion_index(assistant);
  let suggestion_view = (i, a) =>
    suggestion_view(
      ~ci,
      ~inject,
      ~settings,
      ~font_metrics,
      a,
      i == 0,
      is_hovered(i),
      i,
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
      ~ci: CursorInfo.t,
      //~cursor_inspector_mode: option(Model.cursor_inspector_mode),
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
  let suggestion_info_view = {
    let s = AssistantModel.get_indicated_suggestion(~u_gen, assistant, ci);
    switch (s) {
    | Some(s) => suggestion_info_view(s)
    | None => Node.text("")
    };
  };

  div(
    [Attr.id("assistant-wrapper")],
    [suggestions_view, suggestion_info_view],
  );
};
