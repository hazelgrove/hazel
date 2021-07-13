open Virtual_dom.Vdom;
open Node;

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
  let label = Suggestion.string_of_category(category);
  let category_view =
    div([Attr.classes(["category", label])], [text(label)]);
  let index =
    switch (cursor_term) {
    | Exp(OnText(i), _) => i
    | _ => String.length(search_string)
    };
  // split string at caret, only use before caret portion to search
  let (search_string, _) = StringUtil.split_string(index, search_string);
  let match_string =
    StringUtil.match_prefix(search_string, result_text) ? search_string : "";
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
      category_view,
    ],
  );
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
