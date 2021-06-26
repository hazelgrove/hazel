open Virtual_dom.Vdom;
open Node;

let action_abbreviation: Assistant_Exp.assistant_action_categories => string =
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
      {
        action,
        result,
        res_ty,
        category,
        text: act_str,
        delta_errors,
        score,
        _,
      }: Assistant_Exp.assistant_action,
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
  let error_str =
    switch (delta_errors) {
    | 1 => "+"
    | 2 => "++"
    | 3 => "+++"
    | (-1) => "-"
    | (-2) => "--"
    | (-3) => "---"
    | _ => ""
    };
  div(
    [
      Attr.classes(
        ["choice"]
        @ (is_selected ? ["selected"] : [])
        @ (delta_errors > 0 ? ["errors-less"] : [])
        @ (delta_errors < 0 ? ["errors-more"] : []),
      ),
      Attr.on_click(perform_action),
    ],
    [
      category_view,
      div(
        [Attr.classes(["delta-errors"])],
        [text(error_str), text(string_of_int(score))],
      ),
      //div([], [context_consistent |> string_of_bool |> text]),
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
      ci: CursorInfo.pro,
    )
    : Node.t => {
  let filter_string = CursorInfo_common.string_of_cursor_term(ci.term);
  let actions = AssistantModel.get_display_actions(ci, assistant);
  let action_view = (i, a) =>
    action_view(~inject, ~settings, ~font_metrics, a, i == 0, filter_string);
  div([Attr.id("assistant")], List.mapi(action_view, actions));
};
