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

let mk_offset_string = (pre: string, suf: string, target: string): string => {
  let (hit0, hit1) = AssistantModel.submatches_and_offsets(pre, suf, target);
  switch (hit0, hit1) {
  | (None, None) => ""
  | (Some((s0, n0)), Some((s1, n1))) =>
    let n1' = n1 - (n0 + String.length(s0));
    let m0 = String.make(n0, ' ') ++ s0;
    let m1 = String.make(n1', ' ') ++ s1;
    m0 ++ m1;
  | (Some((s, n)), _)
  | (_, Some((s, n))) => String.make(n, ' ') ++ s
  };
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
  let (before_caret, after_caret) =
    StringUtil.split_string(index, search_string);
  let overlay_string =
    mk_offset_string(before_caret, after_caret, result_text);
  div([Attr.classes(["overlay"])], [text(overlay_string)]);
};

let suggestion_view =
    (
      ~ci: CursorInfo.t,
      ~inject: ModelAction.t => Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      {action, result, res_ty, category, result_text, score, _}: Assistant_Exp.suggestion,
      is_selected: bool,
      is_hovered: bool,
      my_index: int,
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
      inject(ModelAction.AcceptSuggestion(action)),
    ]);
  let set_hover = _ =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(Some(my_index))));
  let unset_hover = _ =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(None)));
  let color_score =
    // TODO(andrew): figure out why i'm doing with this
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
      Attr.on_mouseenter(set_hover),
      Attr.on_mouseleave(unset_hover),
    ],
    [
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
    let suggestion =
      AssistantModel.get_indicated_suggestion(~u_gen, assistant, ci);
    switch (suggestion) {
    | Some(suggestion) when AssistantModel.is_hovering(assistant) =>
      suggestion_info_view(suggestion)
    | _ => Node.text("")
    };
  };

  div(
    [Attr.id("assistant-wrapper")],
    [suggestions_view, suggestion_info_view],
  );
};
