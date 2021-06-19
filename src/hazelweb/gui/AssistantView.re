open Virtual_dom.Vdom;
open Node;
open Assistant_Exp;

let action_abbreviation: assistant_action_categories => string =
  fun
  | InsertVar => "var"
  | InsertApp => "app"
  | InsertLit => "lit"
  | InsertConstructor => "con"
  | InsertElim => "elm"
  | ReplaceOperator => "opr"
  | Wrap => "wrp";

let get_editor = (exp: UHExp.t): Program.exp =>
  exp
  |> ZExp.place_before
  |> Program.EditState_Exp.mk
  |> Program.Exp.mk(~width=80);

let get_ty = (editor: Program.typ): HTyp.t =>
  editor
  |> Program.get_edit_state
  |> Program.EditState_Typ.get_uhstx
  |> UHTyp.expand;

let get_display_actions =
    (
      cursor: Assistant_common.cursor_info_pro,
      type_filter_editor: Program.typ,
      {assistant_selection, assistant_choices_limit, _}: Settings.CursorInspector.t,
    )
    : list(assistant_action) => {
  let filter_ty = get_ty(type_filter_editor);
  let actions = Assistant.get_actions_of_ty(cursor, filter_ty);
  let action_index = Assistant.wrap_index(assistant_selection, actions);
  actions
  |> ListUtil.rotate_n(action_index)
  |> ListUtil.trim(assistant_choices_limit);
};

let action_view =
    (
      ~inject: ModelAction.t => Event.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      {action, result, res_ty, category, text: act_str, _}: assistant_action,
      is_selected: bool,
      search_string: string,
    ) => {
  let perform_action = _ => {
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(ModelAction.AcceptSuggestion(action)),
    ]);
  };
  let cat_label = action_abbreviation(category);
  let category_view =
    div([Attr.classes(["category", cat_label])], [text(cat_label)]);
  let match_string =
    StringUtil.match_prefix(search_string, act_str) ? search_string : "";
  let overlay_view =
    div([Attr.classes(["overlay"])], [text(match_string)]);
  let result_view =
    UHCode.codebox_view(
      ~is_focused=false,
      ~settings,
      ~font_metrics,
      get_editor(result),
    );
  // NOTE(andrew): extra level of div nesting seems
  // necessary for correct decoration layout
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
      type_filter_editor: Program.typ,
      cursor_info: CursorInfo.t,
      u_gen: MetaVarGen.t,
    )
    : Node.t => {
  switch (Assistant_common.promote_cursor_info(cursor_info, u_gen)) {
  | None => text("error") // TODO(andrew)
  | Some(cursor) =>
    let filter_string = Assistant_common.term_to_str(cursor.term);
    let actions =
      get_display_actions(
        cursor,
        type_filter_editor,
        settings.cursor_inspector,
      );
    div(
      [Attr.id("assistant")],
      List.mapi(
        (i, a) =>
          action_view(
            ~inject,
            ~settings,
            ~font_metrics,
            a,
            i == 0,
            filter_string,
          ),
        actions,
      ),
    );
  };
};
