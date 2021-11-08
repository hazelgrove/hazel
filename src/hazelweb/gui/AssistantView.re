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
  | WrapInj => "wri"
  | ConvertLit => "cnv";

let label_pat_operand_strategy: Suggestion.pat_operand_strategy => string =
  fun
  | Delete => "del"
  | InsertLit => "lit";

let label_typ_operand_strategy: Suggestion.typ_operand_strategy => string =
  fun
  | Delete => "del"
  | InsertLit => "lit"
  | InsertExpType => "ana"
  | InsertPatType => "pat"
  | InsertJoin => "joi";

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
  | InsertCase => "Insert a case"
  | WrapApp => "Apply a function to the current form"
  | WrapCase => "Case on the current form"
  | WrapInj => "Inject the current form"
  | ConvertLit => "Convert a literal to another type";

let describe_pat_operand_strategy: Suggestion.pat_operand_strategy => string =
  fun
  | Delete => "Delete the current pattern"
  | InsertLit => "Insert a literal";

let describe_typ_operand_strategy: Suggestion.typ_operand_strategy => string =
  fun
  | Delete => "Delete the current type"
  | InsertLit => "Insert a type"
  | InsertExpType => "Use the expression type"
  | InsertPatType => "Use the pattern type"
  | InsertJoin => "Join pattern and expression types";

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

let describe_expression_consistency: float => string =
  fun
  | (-1.0) => "Inconsistent with expression type"
  | 0.5 => "Consistent with expression type"
  | 1.0 => "Equal to expression type"
  | 2.0 => "More specific than expression type"
  | _ => "No info";

let describe_pattern_consistency: float => string =
  fun
  | (-1.0) => "Inconsistent with pattern type"
  | 0.5 => "Consistent with pattern type"
  | 1.0 => "Equal to pattern type"
  | 2.0 => "More specific than pattern type"
  | _ => "No info";

let sign_label: float => string =
  fun
  | n when n < 0. => "minus"
  | n when n > 0. => "plus"
  | _ => "zero";

let sign_symbol: float => string =
  fun
  | n when n < 0. => "-"
  | n when n > 0. => "+"
  | _ => " ";

let icon =
    (~score: option(float)=None, ~num_suggestions: int=0, sort: TermSort.t)
    : Node.t => {
  let blank_guy = "0001";
  let cool_guy = "0031";
  let eager_guy = "0024";
  let tense_guy = "0034";
  let baffled_guy = "0011";
  let neutral_guy: TermSort.t => string =
    fun
    | Exp => "0000"
    | Pat => "0032"
    | Typ => "0042";
  let emotive_guy: (TermSort.t, float) => string =
    (sort, score) =>
      switch (score) {
      | _ when score > 2. => cool_guy
      | _ when score > 0.5 => eager_guy
      | _ when score > (-0.5) => neutral_guy(sort)
      | _ when score > (-2.0) => tense_guy
      | _ => baffled_guy
      };
  let guy =
    switch (num_suggestions, score) {
    | (0, _) => blank_guy
    | (_, None) => neutral_guy(sort)
    | (_, Some(score)) => emotive_guy(sort, score)
    };
  let sort_str = TermSort.to_string(sort);
  let path = "imgs/assistant/boost-" ++ guy ++ "-" ++ sort_str ++ ".png";
  div(
    [Attr.classes(["clickable-help-icon", sort_str])],
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

let score_view = (subscore: float, describe_subscore: float => string) =>
  div(
    [Attr.class_("subscore")],
    switch (subscore) {
    | n when n < 0.01 && n > (-0.01) => []
    | _ => [sign_view(subscore), text(describe_subscore(subscore))]
    },
  );

let scores_view: Suggestion.t => list(Node.t) =
  fun
  | ReplaceExpOperand({report: {scores: s, _}, _}) => [
      score_view(s.delta_errors, describe_delta_errors),
      score_view(s.idiomaticity, describe_idiomaticity),
      score_view(s.type_specificity, describe_type_specificity),
      score_view(s.syntax_conserved, describe_syntax_conserved),
    ]
  | ReplacePatOperand({report: {scores: s, _}, _}) => [
      score_view(s.delta_errors, describe_delta_errors),
    ]
  | ReplaceTypOperand({report: {scores: s, _}, _}) => [
      score_view(s.expression_consistency, describe_expression_consistency),
      score_view(s.pattern_consistency, describe_pattern_consistency),
    ];

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
    @ scores_view(s),
  );

let overlay_view = (search_str: string, result_str: string): Node.t => {
  let overlay_str =
    SuggestionReportExp.syntax_conserved_overlay(search_str, result_str);
  div(
    [
      Attr.class_("overlay"),
      // TODO(andrew): reflect score in highlighting?
      // Attr.create("style", "opacity: " ++ "80" ++ "%;"),
    ],
    // Necessary to avoid underlining whitespace
    List.map(
      fun
      | ' ' => text(" ")
      | c =>
        span([Attr.class_("overlay-text")], [text(String.make(1, c))]),
      StringUtil.explode(overlay_str),
    ),
  );
};

let result_view =
    (
      ~suggestion: Suggestion.t,
      ~ci: CursorInfo.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
    )
    : Node.t => {
  let syntax = Suggestion.show_syntax(suggestion);
  let result_view =
    UHCode.view_syntax(~is_focused=false, ~settings, ~font_metrics, syntax);
  let result_str = Suggestion.show_text(suggestion);
  let search_str = CursorInfo_common.string_of_cursor_term(ci.cursor_term);
  let overlay_view = overlay_view(search_str, result_str);
  div(
    [Attr.class_("code-container")],
    [div([Attr.class_("code")], [overlay_view] @ result_view)],
  );
};

let result_ty_view: Suggestion.t => Node.t =
  s =>
    switch (Suggestion.result_ty(s)) {
    | None => div([], [])
    | Some(ty) =>
      div(
        [Attr.class_("type-ann")],
        [
          div([Attr.class_("type-ann-colon")], [text(":")]),
          div([Attr.class_("type-container")], [HTypCode.view(ty)]),
        ],
      )
    };

let suggestion_view_logic =
    (
      ~suggestion: Suggestion.t,
      ~index: int,
      ~is_hovered: bool,
      ~is_selected: bool,
      ~inject: ModelAction.t => Event.t,
    ) => {
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
  [
    Attr.id(string_of_int(index)),
    Attr.classes(assistant_classes),
    Attr.create("tabindex", "0"), // necessary to make cell focusable
    Attr.on_click(perform_action),
    Attr.on_mouseenter(_ => set_hover_index(Some(index))),
    Attr.on_mouseleave(_ => set_hover_index(None)),
  ];
};

let suggestion_view =
    (
      ~suggestion: Suggestion.t,
      ~display_mode: AssistantModel.display_mode,
      ~index: int,
      ~is_hovered: bool,
      ~is_selected: bool,
      ~ci: CursorInfo.t,
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~inject: ModelAction.t => Event.t,
    ) => {
  div(
    suggestion_view_logic(
      ~suggestion,
      ~index,
      ~is_hovered,
      ~is_selected,
      ~inject,
    ),
    [result_view(~suggestion, ~ci, ~settings, ~font_metrics)]
    @ (
      switch (display_mode) {
      | Minimal => []
      | Normal => [result_ty_view(suggestion), strategy_view(suggestion)]
      }
    ),
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
      ~display_mode=assistant.display_mode,
      ~is_selected=index == 0,
      ~is_hovered=AssistantModel.is_active_suggestion_index(assistant, index),
      ~index,
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
  let sort = TermSort.to_string(CursorInfo.get_sort(ci));
  div(
    [Attr.id("assistant-wrapper"), Attr.class_(sort)],
    [suggestions_view] @ suggestion_info_view,
  );
};
