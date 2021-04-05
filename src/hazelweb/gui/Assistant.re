module Vdom = Virtual_dom.Vdom;
open Vdom;
open Node;
open Attr;
//open OptUtil.Syntax;
open AssistantCore;

let action_abbrev =
  fun
  | InsertVar => "var"
  | InsertApp => "app";

let action_view =
    (inject, font_metrics, act: assistant_action, is_selected: bool) => {
  let {action, result, res_ty, category, _} = act;
  let width = 80; //TODO: unhardcode?
  let abbr = action_abbrev(category);
  div(
    [
      classes(["choice"] @ (is_selected ? ["selected"] : [])),
      on_click(_ => {
        Event.Many([
          Event.Prevent_default,
          Event.Stop_propagation,
          inject(ModelAction.AcceptSuggestion(action)),
        ])
      }),
    ],
    [div([classes(["category", abbr])], [text(abbr)])]
    @ UHCode.codebox_view(~font_metrics, width, result)
    @ [
      span([classes(["type-ann"])], [text(" : ")]),
      span([classes(["type"])], [HTypCode.view(res_ty)]),
    ],
  );
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      settings: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
      u_gen: MetaVarGen.t,
    )
    : Vdom.Node.t => {
  let cursor = AssistantCommon.promote_cursor_info(cursor_info, u_gen);
  let actions = compute_actions(cursor);
  let selected_index =
    switch (settings.assistant_selection) {
    | None => 0
    | Some(i) =>
      let z = i mod List.length(actions);
      z + (z < 0 ? List.length(actions) : 0);
    };
  let actions = ListUtil.rotate_n(selected_index, actions);
  let action_views =
    List.mapi(
      (i, a) => action_view(inject, font_metrics, a, i == 0),
      actions,
    );
  div([id("assistant")], action_views);
};
