module Vdom = Virtual_dom.Vdom;
open Virtual_dom.Vdom;
open Node;
open Attr;
open Assistant;
open Assistant_Exp;

let action_abbreviation =
  fun
  | InsertVar => "var"
  | InsertApp => "app"
  | InsertLit => "lit"
  | InsertConstructor => "con"
  | ReplaceOperator => "opr";

let action_view =
    (
      inject,
      font_metrics,
      {action, result, res_ty, category, text: act_str, _}: assistant_action,
      is_selected: bool,
      search_string: string,
    ) => {
  let width = 80; //TODO: unhardcode?
  let abbr = action_abbreviation(category);
  let search_string =
    StringUtil.match_prefix(search_string, act_str) ? search_string : "";
  let perform_action = _ => {
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(ModelAction.AcceptSuggestion(action)),
    ]);
  };
  div(
    [
      classes(["choice"] @ (is_selected ? ["selected"] : [])),
      on_click(perform_action),
    ],
    [div([classes(["category", abbr])], [text(abbr)])]
    @ [div([classes(["overlay"])], [text(search_string)])]
    @ UHCode.codebox_view(~font_metrics, width, result)
    @ [
      span([classes(["type-ann"])], [text(" : ")]),
      span([classes(["type"])], [HTypCode.view(res_ty)]),
    ],
  );
};

let trim = (n, xs) => List.length(xs) < n ? xs : ListUtil.sublist(n, xs);

let get_action_index = (assistant_selection: option(int), actions): int => {
  let num_actions = List.length(actions);
  switch (assistant_selection) {
  | None => 0
  | Some(i) =>
    let z = num_actions == 0 ? 0 : i mod num_actions;
    z + (z < 0 ? num_actions : 0);
  };
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      {assistant_selection, assistant_choices_limit, _}: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
      u_gen: MetaVarGen.t,
    )
    : Vdom.Node.t => {
  switch (Assistant_common.promote_cursor_info(cursor_info, u_gen)) {
  | None => text("error")
  | Some(cursor) =>
    let actions = compute_actions(cursor);
    let action_index = get_action_index(assistant_selection, actions);
    let actions_visible =
      ListUtil.rotate_n(action_index, actions)
      |> trim(assistant_choices_limit);
    let search_string = Assistant_common.term_to_str(cursor.term);
    let action_views =
      List.mapi(
        (i, a) =>
          action_view(inject, font_metrics, a, i == 0, search_string),
        actions_visible,
      );
    div([id("assistant")], action_views);
  };
};
