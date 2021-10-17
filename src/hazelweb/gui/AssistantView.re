open Virtual_dom.Vdom;
open Node;

let label_operand_strategy: Suggestion.operand_strategy => string =
  fun
  | Delete => "del"
  | InsertLit => "lit"
  | InsertVar => "var"
  | InsertApp => "app"
  | InsertCase => "cas"
  | WrapApp => "wra"
  | WrapCase => "wrc"
  | ConvertLit => "cnv";

let label_pat_operand_strategy: Suggestion.pat_operand_strategy => string =
  fun
  | Delete => "del"
  | InsertLit => "lit";

let label_typ_operand_strategy: Suggestion.typ_operand_strategy => string =
  fun
  | Delete => "del"
  | InsertLit => "lit"
  | AnalyzedType => "ana"
  | PatternType => "pat";

let label_strategy: Suggestion.t => string =
  fun
  | ReplaceExpOperand({operand_strategy, _}) =>
    label_operand_strategy(operand_strategy)
  | ReplacePatOperand({pat_operand_strategy, _}) =>
    label_pat_operand_strategy(pat_operand_strategy)
  | ReplaceTypOperand({typ_operand_strategy, _}) =>
    label_typ_operand_strategy(typ_operand_strategy);

let describe_operand_strategy: Suggestion.operand_strategy => string =
  fun
  | Delete => "Delete the current form"
  | InsertLit => "Insert a literal"
  | InsertVar => "Insert a variable"
  | InsertApp => "Insert an application"
  | InsertCase => "Insert an eliminator"
  | WrapApp => "Apply a functiong to the current form"
  | WrapCase => "Match on the current form"
  | ConvertLit => "Convert a literal to another type";

let describe_pat_operand_strategy: Suggestion.pat_operand_strategy => string =
  fun
  | Delete => "Delete the current pattern"
  | InsertLit => "Insert a literal";

let describe_typ_operand_strategy: Suggestion.typ_operand_strategy => string =
  fun
  | Delete => "Delete the current type"
  | InsertLit => "Insert a type"
  | AnalyzedType => "Insert the analytic type"
  | PatternType => "Insert the pattern type";

let describe_strategy: Suggestion.t => string =
  fun
  | ReplaceExpOperand({operand_strategy, _}) =>
    describe_operand_strategy(operand_strategy)
  | ReplacePatOperand({pat_operand_strategy, _}) =>
    describe_pat_operand_strategy(pat_operand_strategy)
  | ReplaceTypOperand({typ_operand_strategy, _}) =>
    describe_typ_operand_strategy(typ_operand_strategy);

let describe_delta_errors: float => string =
  fun
  | 0. => "Same number of errors"
  | (-1.) => "One more error"
  | 1. => "One fewer error"
  | n when n < 0. =>
    "" ++ string_of_int(int_of_float(-. n)) ++ " more errors"
  | n => "" ++ string_of_int(int_of_float(n)) ++ " fewer errors";

let describe_type_specificity: float => string =
  fun
  | 0. => "Same type specificity"
  | n when n < 0. => "Less granular type"
  | _ => "More granular type";

let describe_idiomaticity: float => string =
  fun
  | 0. => "Same idiomaticity"
  | (-1.) => "Less idiomatic"
  | 1. => "More idiomatic"
  | n when n < 0. => "Much less idiomatic"
  | _ => "Much more idiomatic";

let describe_syntax_conserved: float => string =
  fun
  | x when x < 0.6 => "Syntax partially conserved"
  | x when x < 0.9 => "Syntax mostly conserved"
  | _ => "Existing syntax conserved";

let describe_analysis_consistency: float => string =
  fun
  | 1.0 => "Consistent with analytic type"
  | 1.5 => "Equal to analytic type"
  | _ => "Not consistent with analytic type";

let describe_pattern_consistency: float => string =
  fun
  | 1.0 => "Consistent with pattern type"
  | 1.5 => "Equal to pattern type"
  | _ => "Not consistent with pattern type";

let sign_label: float => string =
  fun
  | n when n < 0. => "minus"
  | n when n > 0. => "plus"
  | _ => "";

let sign_symbol: float => string =
  fun
  | n when n < 0. => "-"
  | n when n > 0. => "+"
  | _ => "";

let icon = (~score: option(float)=None, sort: TermSort.t): Node.t => {
  let guy =
    switch (sort, score) {
    | (Pat, _) => "0000-pat"
    | (Typ, _) => "0000-typ"
    | (Exp, None) => "0000-exp"
    | (Exp, Some(x)) =>
      let pre =
        switch (x) {
        | _ when x > 2. => "0031"
        | _ when x > 0.5 => "0024"
        | _ when x > (-0.5) => "0000"
        | _ when x > (-2.0) => "0034"
        | _ => "0007"
        };
      pre ++ "-exp";
    };
  let path = "imgs/assistant/boost-" ++ guy ++ ".png";
  let sort = TermSort.to_string(sort);
  div(
    [Attr.classes(["clickable-help-icon", sort])],
    [create("img", [Attr.create("src", path)], [])],
  );
};

let strategy_view: Suggestion.t => Node.t =
  suggestion => {
    let label = label_strategy(suggestion);
    let sort = suggestion |> Suggestion.sort_of |> TermSort.to_string;
    div([Attr.classes(["category", label, sort])], [text(label)]);
  };

let sign_view: float => Node.t =
  n => div([Attr.class_(sign_label(n))], [text(sign_symbol(n))]);

let subscore_view = ((subscore: float, describe_subscore: float => string)) =>
  switch (subscore) {
  | 0. => []
  | _ => [
      div(
        [Attr.class_("subscore")],
        [sign_view(subscore), text(describe_subscore(subscore))],
      ),
    ]
  };

let subscore_data_exp = (score: SuggestionReportExp.scores) => [
  (score.delta_errors, describe_delta_errors),
  (score.idiomaticity, describe_idiomaticity),
  (score.type_specificity, describe_type_specificity),
  (score.syntax_conserved, describe_syntax_conserved),
];

let subscore_data_pat = (score: SuggestionReportPat.scores) => [
  (score.delta_errors, describe_delta_errors),
];

let subscore_data_typ = (score: SuggestionReportTyp.scores) => [
  (score.analysis_consistency, describe_analysis_consistency),
  (score.pattern_consistency, describe_pattern_consistency),
];

let subscorer: Suggestion.t => list((float, float => string)) =
  fun
  | ReplaceExpOperand({report: {scores, _}, _}) => subscore_data_exp(scores)
  | ReplacePatOperand({report: {scores, _}, _}) => subscore_data_pat(scores)
  | ReplaceTypOperand({report: {scores, _}, _}) =>
    subscore_data_typ(scores);

let suggestion_info_view = (s: Suggestion.t): Node.t =>
  div(
    [Attr.class_("suggestion-info")],
    [
      span([], [strategy_view(s)]),
      span(
        [Attr.class_("suggestion-description")],
        [text(describe_strategy(s))],
      ),
    ]
    @ List.concat(List.map(subscore_view, subscorer(s))),
  );

/* Draws the matching characters overtop of suggestions */
let overlay_view =
    (
      {cursor_term, _}: CursorInfo.t,
      search_string: string,
      result_text: string,
    )
    : Node.t => {
  let index =
    switch (cursor_term) {
    | ExpOperand(OnText(i), _) => i
    | _ => String.length(search_string)
    };
  let (pre, suf) = StringUtil.split_string(index, search_string);
  let overlay = (n, s) => [
    text(String.make(n, ' ')),
    span([Attr.class_("overlay-text")], [text(s)]),
  ];
  let offset_overlay =
    switch (SuggestionReportExp.submatches_and_offsets(pre, suf, result_text)) {
    | (None, None) => []
    | (Some((s0, n0)), Some((s1, n1))) =>
      let n1' = n1 - (n0 + String.length(s0));
      overlay(n0, s0) @ overlay(n1', s1);
    | (Some((s, n)), _)
    | (_, Some((s, n))) => overlay(n, s)
    };
  div([Attr.class_("overlay")], offset_overlay);
};

let result_view =
    (
      ~suggestion: Suggestion.t,
      ~search_string: string,
      ~ci: CursorInfo.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
    )
    : Node.t => {
  let syntax = Suggestion.show_syntax(suggestion);
  let result_view =
    UHCode.view_syntax(~is_focused=false, ~settings, ~font_metrics, syntax);
  let show_text = Suggestion.show_text(suggestion);
  let overlay_view = overlay_view(ci, search_string, show_text);
  div(
    [Attr.class_("code-container")],
    [div([Attr.class_("code")], [overlay_view] @ result_view)],
  );
};

let result_ty_view: Suggestion.t => list(Node.t) =
  fun
  | ReplaceExpOperand({report: {result_ty, _}, _})
  | ReplacePatOperand({report: {result_ty, _}, _}) => [
      div([Attr.class_("type-ann")], [text(":")]),
      div([Attr.class_("type")], [HTypCode.view(result_ty)]),
    ]
  | ReplaceTypOperand(_) => [];

let suggestion_view =
    (
      ~suggestion: Suggestion.t,
      ~index: int,
      ~is_hovered: bool,
      ~is_selected: bool,
      ~search_string: string,
      ~ci: CursorInfo.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~inject: ModelAction.t => Event.t,
    )
    : Node.t => {
  let perform_action = _ =>
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(FocusCell(MainProgram)), // prevent main editor from losing focus
      inject(ModelAction.AcceptSuggestion(Suggestion.action(suggestion))),
    ]);
  let set_hover_index = (idx: option(int)) =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(idx)));
  let color_score = Suggestion.score(suggestion);
  let assistant_classes =
    ["choice"]
    @ (is_selected ? ["selected"] : [])
    @ (is_hovered ? ["hovered"] : [])
    @ (color_score > 0. ? ["errors-less"] : [])
    @ (color_score < 0. ? ["errors-more"] : []);
  div(
    [
      Attr.id(string_of_int(index)),
      Attr.classes(assistant_classes),
      Attr.create("tabindex", "0"), // necessary to make cell focusable
      Attr.on_click(perform_action),
      Attr.on_mouseenter(_ => set_hover_index(Some(index))),
      Attr.on_mouseleave(_ => set_hover_index(None)),
    ],
    [result_view(~suggestion, ~search_string, ~ci, ~settings, ~font_metrics)]
    @ result_ty_view(suggestion)
    @ [strategy_view(suggestion)],
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
  let suggestions =
    AssistantModel.get_display_suggestions(~u_gen, ci, assistant);
  let suggestion_view = (index, suggestion) =>
    suggestion_view(
      ~ci,
      ~inject,
      ~settings,
      ~font_metrics,
      ~suggestion,
      ~is_selected=index == 0,
      ~is_hovered=AssistantModel.is_active_suggestion_index(assistant, index),
      ~index,
      ~search_string=CursorInfo_common.string_of_cursor_term(ci.cursor_term),
    );
  let sort = TermSort.to_string(CursorInfo.get_sort(ci));
  div(
    [Attr.id("assistant"), Attr.class_(sort)],
    List.mapi(suggestion_view, suggestions),
  );
};

let view =
    (
      ~assistant: AssistantModel.t,
      ~ci: CursorInfo.t,
      ~u_gen: MetaVarGen.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~inject: ModelAction.t => Event.t,
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
  let suggestion_info_view =
    switch (AssistantModel.get_indicated_suggestion(~u_gen, assistant, ci)) {
    | Some(suggestion) when AssistantModel.is_hovering(assistant) => [
        suggestion_info_view(suggestion),
      ]
    | _ => []
    };
  switch (ci.typed) {
  | OnType({analyzed_ty, pattern_ty, ann_ty}) =>
    print_endline("OnType: analyzed pattern ann");
    print_endline(Sexplib.Sexp.to_string_hum(HTyp.sexp_of_t(analyzed_ty)));
    print_endline(Sexplib.Sexp.to_string_hum(HTyp.sexp_of_t(pattern_ty)));
    print_endline(Sexplib.Sexp.to_string_hum(HTyp.sexp_of_t(ann_ty)));
  | _ => ()
  };
  let sort = TermSort.to_string(CursorInfo.get_sort(ci));
  div(
    [Attr.id("assistant-wrapper"), Attr.class_(sort)],
    [suggestions_view] @ suggestion_info_view,
  );
};
