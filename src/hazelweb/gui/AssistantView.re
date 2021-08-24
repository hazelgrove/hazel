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
  let (pre, suf) = StringUtil.split_string(index, search_string);
  let overlay = (n, s) => [
    text(String.make(n, ' ')),
    span([Attr.class_("overlay-text")], [text(s)]),
  ];
  let offset_overlay =
    switch (AssistantModel.submatches_and_offsets(pre, suf, result_text)) {
    | (None, None) => []
    | (Some((s0, n0)), Some((s1, n1))) =>
      let n1' = n1 - (n0 + String.length(s0));
      overlay(n0, s0) @ overlay(n1', s1);
    | (Some((s, n)), _)
    | (_, Some((s, n))) => overlay(n, s)
    };
  div([Attr.classes(["overlay"])], offset_overlay);
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
      /* NOTE: prevent main editor from losing focus */
      inject(FocusCell(MainProgram)),
      inject(ModelAction.AcceptSuggestion(action)),
    ]);
  let set_hover = _ =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(Some(my_index))));
  let unset_hover = _ =>
    inject(ModelAction.UpdateAssistant(Set_hover_index(None)));
  let color_score =
    // TODO(andrew): figure out what i'm doing with this
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

open Sexplib.Std;
let compareMetaData = (frame: CursorFrame.t, ci: CursorInfo.t) => {
  let ci_expected = Some(ci.expected_ty);
  let ci_actual = ci.actual_ty;
  let ci_enclosing_operand = ci.opParent;
  let (ci_enclosing_opseq_ty, ci_enclosing_seq, ci_enclosing_opseq_err) =
    switch (ci.syntactic_context) {
    | ExpSeq(ty, zseq, err) => (Some(ty), Some(zseq), Some(err))
    | NoSeq => (None, None, None)
    };
  let fr_expected = CursorFrame.get_expected_type_cursor_term(frame);
  let fr_actual = CursorFrame.get_actual_type_cursor_term(frame);
  let fr_enclosing_operand = CursorFrame.enclosing_operand(frame);
  let (fr_enclosing_opseq_ty, fr_enclosing_seq, fr_enclosing_opseq_err) =
    switch (CursorFrame.enclosing_zopseq_si(frame)) {
    | Some(
        {slice: ExpSeq(ZOpSeq(_, zseq)), ty_e, err, _}: CursorFrame.slice_info,
      ) => (
        //NOTE:
        switch (ty_e) {
        | None => Some(HTyp.Hole)
        | Some(ty) => Some(ty)
        },
        Some(zseq),
        err,
      )
    | _ => (None, None, None)
    };
  let p = (x, q) => Sexplib.Sexp.to_string_hum(sexp_of_option(x, q));
  Printf.printf(
    "1. expected: %b // CI: %s FR: %s \n",
    ci_expected == fr_expected,
    p(HTyp.sexp_of_t, ci_expected),
    p(HTyp.sexp_of_t, fr_expected),
  );
  Printf.printf(
    "2. actual: %b // CI: %s FR: %s \n",
    ci_actual == fr_actual,
    p(HTyp.sexp_of_t, ci_actual),
    p(HTyp.sexp_of_t, fr_actual),
  );
  Printf.printf(
    "3. zoperand: %b // CI: %s FR: %s \n",
    ci_enclosing_operand == fr_enclosing_operand,
    p(ZExp.sexp_of_zoperand, ci_enclosing_operand),
    p(ZExp.sexp_of_zoperand, fr_enclosing_operand),
  );
  Printf.printf(
    "4. zopseq %b // CI: %s FR: %s \n",
    ci_enclosing_seq == fr_enclosing_seq,
    p(ZExp.sexp_of_zseq, ci_enclosing_seq),
    p(ZExp.sexp_of_zseq, fr_enclosing_seq),
  );
  Printf.printf(
    "5. zopseq_ty: %b // CI: %s FR: %s \n",
    ci_enclosing_opseq_ty == fr_enclosing_opseq_ty,
    p(HTyp.sexp_of_t, ci_enclosing_opseq_ty),
    p(HTyp.sexp_of_t, fr_enclosing_opseq_ty),
  );
  Printf.printf(
    "6. zopseq_err: %b // CI: %s FR: %s \n",
    ci_enclosing_opseq_err == fr_enclosing_opseq_err,
    p(ErrStatus.sexp_of_t, ci_enclosing_opseq_err),
    p(ErrStatus.sexp_of_t, fr_enclosing_opseq_err),
  );
};

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~u_gen: MetaVarGen.t,
      ~ci: CursorInfo.t,
      ~frame: CursorFrame.t,
      assistant: AssistantModel.t,
    )
    : Node.t => {
  let _ = compareMetaData(frame, ci);
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
