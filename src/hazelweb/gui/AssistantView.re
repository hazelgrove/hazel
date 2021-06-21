open Virtual_dom.Vdom;
open Node;
open Assistant_Exp;

let action_abbreviation: assistant_action_categories => string =
  fun
  | InsertVar => "var"
  | InsertApp => "app"
  | InsertLit => "lit"
  | InsertConstructor => "con"
  | InsertElim => "eli"
  | ReplaceOperator => "opr"
  | Wrap => "wra"
  | Delete => "del";

let mk_editor = (exp: UHExp.t): Program.exp =>
  exp
  |> ZExp.place_before
  |> Program.EditState_Exp.mk
  |> Program.Exp.mk(~width=80);

let action_view =
    (
      ~inject: ModelAction.t => Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      {action, result, res_ty, category, text: act_str, _}: assistant_action,
      is_selected: bool,
      search_string: string,
    ) => {
  let perform_action = _ =>
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(ModelAction.AcceptSuggestion(action)),
    ]);
  let label = action_abbreviation(category);
  let category_view =
    div([Attr.classes(["category", label])], [text(label)]);
  let match_string =
    StringUtil.match_prefix(search_string, act_str) ? search_string : "";
  let overlay_view =
    div([Attr.classes(["overlay"])], [text(match_string)]);
  let result_view =
    UHCode.codebox_view(
      ~is_focused=false,
      ~settings,
      ~font_metrics,
      mk_editor(result),
    );
  div(
    [
      Attr.classes(["choice"] @ (is_selected ? ["selected"] : [])),
      Attr.on_click(perform_action),
    ],
    [
      category_view,
      div(
        [Attr.classes(["code-container"])],
        [div([Attr.classes(["code"])], [overlay_view] @ result_view)],
      ),
      div([Attr.classes(["type-ann"])], [text(" : ")]),
      div([Attr.classes(["type"])], [HTypCode.view(res_ty)]),
    ],
  );
};

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      assistant: AssistantModel.t,
      ci: Assistant_common.cursor_info_pro,
    )
    : Node.t => {
  let filter_string = Assistant_common.term_to_str(ci.term);
  let actions = AssistantModel.get_display_actions(ci, assistant);
  let action_view = (i, a) =>
    action_view(~inject, ~settings, ~font_metrics, a, i == 0, filter_string);
  div([Attr.id("assistant")], List.mapi(action_view, actions));
};
