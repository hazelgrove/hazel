//open OptUtil.Syntax;
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
  | ReplaceOperator => "opr"
  | Wrap => "wrp";

let action_view =
    (
      inject,
      settings,
      font_metrics,
      {action, result, res_ty, category, text: act_str, _}: assistant_action,
      is_selected: bool,
      search_string: string,
    ) => {
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
  //TODO(andrew): unhardcode width?
  //TODO(andrew): refactor to take uhexp instead of zexp?
  let edit_state = (ZExp.place_before(result), HTyp.Hole, MetaVarGen.init);
  let program = Program.Exp.mk(~width=80, edit_state);
  div(
    [
      classes(["choice"] @ (is_selected ? ["selected"] : [])),
      on_click(perform_action),
    ],
    [
      div([classes(["category", abbr])], [text(abbr)]),
      div(
        [Attr.classes(["code-container"])],
        [
          div(
            //NOTE(andrew): extra level of divs seems necessary for decoration formatting
            [Attr.classes(["code"])],
            [div([classes(["overlay"])], [text(search_string)])]
            @ UHCode.codebox_view(
                ~is_focused=false,
                ~settings,
                ~font_metrics,
                program,
              ),
          ),
        ],
      ),
      span([classes(["type-ann"])], [text(" : ")]),
      span([classes(["type"])], [HTypCode.view(res_ty)]),
    ],
  );
};

let trim = (n, xs) => List.length(xs) < n ? xs : ListUtil.sublist(n, xs);

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      type_filter_editor: Program.typ,
      {assistant_selection, assistant_choices_limit, _}: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
      u_gen: MetaVarGen.t,
    )
    : Vdom.Node.t => {
  switch (Assistant_common.promote_cursor_info(cursor_info, u_gen)) {
  | None => text("error")
  | Some(cursor) =>
    let filter_ty =
      type_filter_editor
      |> Program.get_edit_state
      |> Program.EditState_Typ.get_uhstx
      |> UHTyp.expand;
    let filter_string = Assistant_common.term_to_str(cursor.term);
    let actions =
      cursor
      |> compute_actions
      |> List.filter(a => HTyp.consistent(a.res_ty, filter_ty));
    let action_index =
      Assistant_common.get_action_index(assistant_selection, actions);
    let actions =
      actions
      |> ListUtil.rotate_n(action_index)
      |> trim(assistant_choices_limit);
    let action_views =
      List.mapi(
        (i, a) =>
          action_view(
            inject,
            settings,
            font_metrics,
            a,
            i == 0,
            filter_string,
          ),
        actions,
      );
    div([id("assistant")], action_views);
  };
};
