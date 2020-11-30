module Vdom = Virtual_dom.Vdom;

type err_state_b =
  | TypeInconsistency
  | BindingError
  | OK;

let inconsistent_symbol =
  Vdom.Node.div(
    [
      Vdom.Attr.classes(["consistency-symbol", "inconsistent-symbol"]),
      Vdom.Attr.create("title", "Inconsistent"),
    ],
    [Vdom.Node.text(UnicodeConstants.inconsistent)],
  );

let consistent_symbol =
  Vdom.Node.div(
    [
      Vdom.Attr.classes(["consistent-symbol", "colon"]),
      Vdom.Attr.create("title", "Of Consistent Type"),
    ],
    [Vdom.Node.text(":"), Vdom.Node.text("~")],
  );

let emphasize_text = (~only_right=false, msg: string) => {
  let classes =
    only_right ? ["emphasize-text", "only-right"] : ["emphasize-text"];
  Vdom.Node.div([Vdom.Attr.classes(classes)], [Vdom.Node.text(msg)]);
};

let colon =
  Vdom.Node.div([Vdom.Attr.classes(["colon"])], [Vdom.Node.text(":")]);

let advanced_summary = (typed: CursorInfo.typed, tag_type: TermSort.t) => {
  let term_tag = TermTag.term_tag_view(tag_type, ~show_tooltip=true, []);
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty)
    | SynMatchingArrow(_, ty) => ([colon], [HTypCode.view(ty)])
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      switch (ty) {
      | HTyp.Hole => ([colon], [emphasize_text("Any Type")])
      | _ => ([colon], [HTypCode.view(ty)])
      }
    /* Use the got type if not just Hole */
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      switch (got_ty, HTyp.eq(expected_ty, got_ty)) {
      | (HTyp.Hole, true) => ([colon], [HTypCode.view(expected_ty)])
      | (HTyp.Hole, false) => (
          [consistent_symbol],
          [HTypCode.view(expected_ty)],
        )
      | (_, false) => ([consistent_symbol], [HTypCode.view(got_ty)])
      | _ => ([colon], [HTypCode.view(got_ty)])
      }
    | AnaTypeInconsistent(expected_ty, got_ty)
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk_diff(expected_ty, got_ty);
      (
        [colon],
        [
          TypDiffCode.view(expected_diff),
          inconsistent_symbol,
          TypDiffCode.view(got_diff),
        ],
      );
    | SynErrorArrow(_expected_ty, got_ty) => (
        [colon],
        [
          emphasize_text("Function Type"),
          inconsistent_symbol,
          HTypCode.view(got_ty),
        ],
      )
    | AnaWrongLength(expected_len, got_len, _expected_ty)
    | PatAnaWrongLength(expected_len, got_len, _expected_ty) => (
        [],
        [
          emphasize_text(string_of_int(expected_len) ++ "-tuple"),
          inconsistent_symbol,
          emphasize_text(string_of_int(got_len) ++ "-tuple"),
        ],
      )
    | AnaInvalid(expected_ty)
    | PatAnaInvalid(expected_ty) => (
        [colon],
        [
          HTypCode.view(expected_ty),
          inconsistent_symbol,
          emphasize_text("Invalid Text"),
        ],
      )
    | SynInvalid => ([], [emphasize_text("Invalid Text")])
    | SynInvalidArrow(_) => (
        [colon],
        [
          emphasize_text("Function Type"),
          inconsistent_symbol,
          emphasize_text("Invalid Text"),
        ],
      )
    | AnaFree(expected_ty) => (
        [colon],
        [
          HTypCode.view(expected_ty),
          inconsistent_symbol,
          emphasize_text("Free Variable"),
        ],
      )
    | SynFree => ([], [emphasize_text("Free Variable")])
    | SynFreeArrow(_) => (
        [colon],
        [
          emphasize_text("Function Type"),
          inconsistent_symbol,
          emphasize_text("Free Variable"),
        ],
      )
    | AnaKeyword(expected_ty, _)
    | PatAnaKeyword(expected_ty, _) => (
        [colon],
        [
          HTypCode.view(expected_ty),
          inconsistent_symbol,
          emphasize_text("Reserved Keyword"),
        ],
      )
    | SynKeyword(_)
    | PatSynKeyword(_) => ([], [emphasize_text("Reserved Keyword")])
    | SynKeywordArrow(_) => (
        [colon],
        [
          emphasize_text("Function Type"),
          inconsistent_symbol,
          emphasize_text("Reserved Keyword"),
        ],
      )
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        if (HTyp.consistent(ty, got_ty)) {
          ([colon], [HTypCode.view(ty)]);
        } else {
          let (ty_diff, got_diff) = TypDiff.mk_diff(ty, got_ty);
          (
            [colon],
            [
              TypDiffCode.view(ty_diff),
              inconsistent_symbol,
              TypDiffCode.view(got_diff),
            ],
          );
        }
      | (InconsistentBranchTys(_), _) => (
          [],
          [emphasize_text("Inconsistent Branch Types")],
        )
      | _ => message(typed)
      }
    | SynInconsistentBranches(_) => (
        [],
        [emphasize_text("Inconsistent Branch Types")],
      )
    | SynInconsistentBranchesArrow(_) => (
        [colon],
        [
          emphasize_text("Function Type"),
          inconsistent_symbol,
          emphasize_text("Inconsistent Branch Types"),
        ],
      )
    | OnType => ([], [])
    | OnLine => ([], [emphasize_text("Line")])
    | OnRule => ([], [emphasize_text("Rule")])
    };
  };
  let (colon_message, type_message) = message(typed);
  ([term_tag, ...colon_message], type_message);
};

let novice_summary = (typed: CursorInfo.typed, tag_typ: TermSort.t) => {
  let term_tag = TermTag.term_tag_view(tag_typ, ~show_tooltip=true, []);
  let article =
    switch (tag_typ) {
    | Exp => "an"
    | Pat
    | Typ => "a"
    };
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of type"),
        ],
        [HTypCode.view(ty)],
      )
    /* Use the got type if not just a Hole */
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      switch (got_ty, HTyp.eq(expected_ty, got_ty)) {
      | (HTyp.Hole, true) => (
          [
            Vdom.Node.text("Expecting " ++ article),
            term_tag,
            Vdom.Node.text("of type"),
          ],
          [HTypCode.view(expected_ty)],
        )
      | (HTyp.Hole, false) => (
          [
            Vdom.Node.text("Expecting " ++ article),
            term_tag,
            Vdom.Node.text("of type consistent with"),
          ],
          [HTypCode.view(expected_ty)],
        )
      | (_, false) => (
          [
            Vdom.Node.text("Got " ++ article),
            term_tag,
            Vdom.Node.text("of consistent type"),
          ],
          [HTypCode.view(got_ty)],
        )
      | _ => (
          [
            Vdom.Node.text("Got " ++ article),
            term_tag,
            Vdom.Node.text("of type"),
          ],
          [HTypCode.view(got_ty)],
        )
      }
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      switch (ty) {
      | HTyp.Hole => (
          [
            Vdom.Node.text("Expecting " ++ article),
            term_tag,
            Vdom.Node.text("of"),
          ],
          [emphasize_text("Any Type")],
        )
      | _ => (
          [
            Vdom.Node.text("Got " ++ article),
            term_tag,
            Vdom.Node.text("of type"),
          ],
          [HTypCode.view(ty)],
        )
      }
    | SynMatchingArrow(_, ty) => (
        [
          Vdom.Node.text("Got " ++ article),
          term_tag,
          Vdom.Node.text("of type"),
        ],
        [HTypCode.view(ty)],
      )
    | AnaTypeInconsistent(expected_ty, got_ty)
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk_diff(expected_ty, got_ty);
      (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of type"),
        ],
        [
          TypDiffCode.view(expected_diff),
          Vdom.Node.text("but got inconsistent type"),
          TypDiffCode.view(got_diff),
        ],
      );
    | SynErrorArrow(_expected_ty, got_ty) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of"),
        ],
        [
          emphasize_text("Function Type"),
          Vdom.Node.text("but got inconsistent type"),
          HTypCode.view(got_ty),
        ],
      )
    | AnaWrongLength(expected_len, got_len, _expected_ty)
    | PatAnaWrongLength(expected_len, got_len, _expected_ty) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of type"),
        ],
        [
          emphasize_text(string_of_int(expected_len) ++ "-tuple"),
          Vdom.Node.text("but got"),
          emphasize_text(string_of_int(got_len) ++ "-tuple"),
        ],
      )
    | AnaInvalid(expected_ty)
    | PatAnaInvalid(expected_ty) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of type"),
        ],
        [
          HTypCode.view(expected_ty),
          Vdom.Node.text("but got"),
          emphasize_text("Invalid Text"),
        ],
      )
    | SynInvalid => (
        [Vdom.Node.text("Got " ++ article), term_tag],
        [emphasize_text(~only_right=true, "Invalid Text")],
      )
    | SynInvalidArrow(_) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of"),
        ],
        [
          emphasize_text("Function Type"),
          Vdom.Node.text("but got"),
          emphasize_text("Invalid Text"),
        ],
      )
    | AnaFree(expected_ty) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of type"),
        ],
        [
          HTypCode.view(expected_ty),
          Vdom.Node.text("but got a"),
          emphasize_text("Free Variable"),
        ],
      )
    | SynFree => (
        [Vdom.Node.text("Got " ++ article), term_tag],
        [emphasize_text(~only_right=true, "Free Variable")],
      )
    | SynFreeArrow(_) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of"),
        ],
        [
          emphasize_text("Function Type"),
          Vdom.Node.text("but got a"),
          emphasize_text("Free Variable"),
        ],
      )
    | AnaKeyword(expected_ty, _)
    | PatAnaKeyword(expected_ty, _) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of type"),
        ],
        [
          HTypCode.view(expected_ty),
          Vdom.Node.text("but got a"),
          emphasize_text("Reserved Keyword"),
        ],
      )
    | SynKeyword(_)
    | PatSynKeyword(_) => (
        [Vdom.Node.text("Got " ++ article), term_tag],
        [emphasize_text("Reserved Keyword")],
      )
    | SynKeywordArrow(_) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of"),
        ],
        [
          emphasize_text("Function Type"),
          Vdom.Node.text("but got a"),
          emphasize_text("Reserved Keyword"),
        ],
      )
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        if (HTyp.consistent(ty, got_ty)) {
          (
            [
              Vdom.Node.text("Got " ++ article),
              term_tag,
              Vdom.Node.text("consistent with type"),
            ],
            [HTypCode.view(ty)],
          );
        } else {
          let (ty_diff, got_diff) = TypDiff.mk_diff(ty, got_ty);
          (
            [
              Vdom.Node.text("Expecting " ++ article),
              term_tag,
              Vdom.Node.text("of type"),
            ],
            [
              TypDiffCode.view(ty_diff),
              Vdom.Node.text("but got inconsistent type"),
              TypDiffCode.view(got_diff),
            ],
          );
        }
      | (InconsistentBranchTys(_), _) => (
          [Vdom.Node.text("Got " ++ article), term_tag],
          [emphasize_text("Inconsistent Branch Types")],
        )
      | _ => message(typed)
      }
    | SynInconsistentBranches(_) => (
        [Vdom.Node.text("Got " ++ article), term_tag],
        [emphasize_text("Inconsistent Branch Types")],
      )
    | SynInconsistentBranchesArrow(_) => (
        [
          Vdom.Node.text("Expecting " ++ article),
          term_tag,
          Vdom.Node.text("of"),
        ],
        [
          emphasize_text("Function Type"),
          Vdom.Node.text("but got"),
          emphasize_text("Inconsistent Branch Types"),
        ],
      )
    | OnType => ([Vdom.Node.text("Got " ++ article), term_tag], [])
    | OnLine => (
        [Vdom.Node.text("Got " ++ article), term_tag],
        [emphasize_text(~only_right=true, "Line")],
      )
    | OnRule => (
        [Vdom.Node.text("Got " ++ article), term_tag],
        [emphasize_text(~only_right=true, "Rule")],
      )
    };
  };
  message(typed);
};

let summary_bar =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ci: CursorInfo.t,
      err_state_b: err_state_b,
      show_expanded: bool,
      term_novice_message_mode: bool,
      type_novice_message_mode: bool,
    ) => {
  let arrow =
    if (show_expanded) {
      Icons.down_arrow(["cursor-inspector-arrow"]);
    } else {
      Icons.left_arrow(["cursor-inspector-arrow"]);
    };
  let err_icon =
    switch (err_state_b) {
    | TypeInconsistency
    | BindingError => Icons.x_circle
    | OK => Icons.check_circle
    };
  let tag_type = TermTag.get_cursor_term_sort(ci.cursor_term);
  let (term_novice, type_novice) = novice_summary(ci.typed, tag_type);
  let (term_advanced, type_advanced) = advanced_summary(ci.typed, tag_type);
  let summary =
    Vdom.(
      Node.div(
        [Attr.classes(["subsection"])],
        [
          Node.div(
            [
              Attr.classes(
                term_novice_message_mode
                  ? ["novice-mode", "subsection"] : ["subsection"],
              ),
              Attr.on_click(_ =>
                Vdom.Event.Many([
                  Event.Prevent_default,
                  Event.Stop_propagation,
                  inject(ModelAction.ToggleTermNoviceMessageMode),
                ])
              ),
            ],
            term_novice_message_mode ? term_novice : term_advanced,
          ),
          Node.div(
            [
              Attr.classes(
                term_novice_message_mode
                  ? ["novice-mode", "subsection"] : ["subsection"],
              ),
              Attr.on_click(_ =>
                Vdom.Event.Many([
                  Event.Prevent_default,
                  Event.Stop_propagation,
                  inject(ModelAction.ToggleTypeNoviceMessageMode),
                ])
              ),
            ],
            type_novice_message_mode ? type_novice : type_advanced,
          ),
        ],
      )
    );
  Vdom.(
    Node.div(
      [
        Attr.classes(["type-info-summary"]),
        Attr.on_click(_ =>
          Vdom.Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            inject(ModelAction.ToggleCursorInspectorExpansion),
          ])
        ),
      ],
      [summary, arrow, err_icon],
    )
  );
};

let view =
    (
      ~inject: ModelAction.t => Vdom.Event.t,
      ~view_of_text,
      loc: (float, float),
      cursor_inspector: Model.cursor_inspector,
      cursor_info: CursorInfo.t,
    )
    : Vdom.Node.t => {
  let typebar = ty =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "typebar"])],
        [HTypCode.view(ty)],
      )
    );
  let typebar_diff = ty =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "typebar"])],
        [TypDiffCode.view(ty)],
      )
    );
  let matched_ty_bar = (ty1, ty2) =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "matched-type-bar"])],
        [
          HTypCode.view(ty1),
          Node.span(
            [Attr.classes(["matched-connective"])],
            [Node.text(" ▶ ")],
          ),
          HTypCode.view(ty2),
        ],
      )
    );
  let inconsistent_branches_ty_bar =
      (branch_types, path_to_case, skipped_index) =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "inconsistent-branches-ty-bar"])],
        List.mapi(
          (index, ty) => {
            let shifted_index =
              switch (skipped_index) {
              | None => index
              | Some(skipped_index) =>
                if (index >= skipped_index) {
                  index + 1;
                } else {
                  index;
                }
              };
            Node.span(
              [
                Attr.on_click(_ => {
                  inject(SelectCaseBranch(path_to_case, shifted_index))
                }),
              ],
              [HTypCode.view(ty)],
            );
          },
          branch_types,
        ),
      )
    );

  let special_msg_bar = (msg: string) =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "special-msg-bar"])],
        [Node.text(msg)],
      )
    );

  let expected_indicator = (title_text, type_div) =>
    Vdom.(
      Node.div(
        [Attr.classes(["indicator", "expected-indicator"])],
        [Panel.view_of_main_title_bar(title_text), type_div],
      )
    );
  let expected_ty_title = "Expecting an expression of type";
  let expected_ty_title_pat = "Expecting a pattern of type";
  let expected_ty_title_consistent = "Expecting an expression consistent with type";
  let expected_ty_indicator = ty =>
    expected_indicator(expected_ty_title, typebar(ty));
  let expected_ty_indicator_diff = ty =>
    expected_indicator(expected_ty_title, typebar_diff(ty));
  let expected_ty_indicator_pat = ty =>
    expected_indicator(expected_ty_title_pat, typebar(ty));
  let expected_ty_indicator_pat_diff = ty =>
    expected_indicator(expected_ty_title_pat, typebar_diff(ty));
  let expected_ty_indicator_consistent = ty =>
    expected_indicator(expected_ty_title_consistent, typebar(ty));
  let expected_ty_indicator_consistent_diff = ty =>
    expected_indicator(expected_ty_title_consistent, typebar_diff(ty));
  let expected_msg_indicator = msg =>
    expected_indicator("Expecting an expression of ", special_msg_bar(msg));
  let expected_msg_indicator_pat = msg =>
    expected_indicator("Expecting a pattern of ", special_msg_bar(msg));
  let expected_any_indicator = expected_msg_indicator("any type");
  let expected_any_indicator_pat = expected_msg_indicator_pat("any type");
  let expected_a_type_indicator =
    expected_indicator("Expecting ", special_msg_bar("a type"));
  let expected_a_line_indicator =
    expected_indicator("Expecting ", special_msg_bar("a line item"));
  let expected_a_rule_indicator =
    expected_indicator("Expecting ", special_msg_bar("a case rule"));
  let expected_inconsistent_branches_indicator =
      (branch_types, path_to_case, skipped_index) =>
    expected_indicator(
      "No consistent expected type",
      inconsistent_branches_ty_bar(
        branch_types,
        path_to_case,
        Some(skipped_index),
      ),
    );

  let got_indicator = (title_text, type_div) =>
    Vdom.(
      Node.div(
        [Attr.classes(["indicator", "got-indicator"])],
        [Panel.view_of_other_title_bar(title_text), type_div],
      )
    );
  let got_ty_indicator = ty => got_indicator("Got type", typebar(ty));
  let got_as_expected_ty_indicator = ty =>
    got_indicator("Got as expected", typebar(ty));
  let got_inconsistent_indicator_diff = got_ty =>
    got_indicator("Got inconsistent type", typebar_diff(got_ty));
  let got_inconsistent_matched_indicator = (got_ty, matched_ty) =>
    got_indicator(
      "Got inconsistent type ▶ assumed ",
      matched_ty_bar(got_ty, matched_ty),
    );
  let got_inconsistent_branches_indicator = (branch_types, path_to_case) =>
    got_indicator(
      "Got inconsistent branch types",
      inconsistent_branches_ty_bar(branch_types, path_to_case, None),
    );

  let got_free_indicator =
    got_indicator("Got a free variable", typebar(HTyp.Hole));

  let got_invalid_indicator =
    got_indicator("Got invalid text", typebar(HTyp.Hole));

  let got_consistent_indicator = got_ty =>
    got_indicator("Got consistent type", typebar(got_ty));
  let got_a_type_indicator = got_indicator("Got", special_msg_bar("a type"));
  let got_a_line_indicator =
    got_indicator("Got", special_msg_bar("a line item"));
  let got_a_rule_indicator =
    got_indicator("Got", special_msg_bar("a case rule"));
  let got_keyword_indicator =
    got_indicator("Got a reserved keyword", typebar(HTyp.Hole));

  let rec get_indicator_info = (typed: CursorInfo.typed) =>
    switch (typed) {
    | Analyzed(ty) =>
      let ind1 = expected_ty_indicator(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      (ind1, ind2, OK);
    | AnaAnnotatedLambda(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK);
    | AnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk_diff(expected_ty, got_ty);
      let ind1 = expected_ty_indicator_diff(expected_diff);
      let ind2 = got_inconsistent_indicator_diff(got_diff);
      (ind1, ind2, TypeInconsistency);
    | AnaWrongLength(expected_len, got_len, _expected_ty) =>
      let expected_msg = string_of_int(expected_len) ++ "-tuple";
      let ind1 =
        expected_indicator(
          "Expecting an expression of type",
          special_msg_bar(expected_msg),
        );
      let got_msg = string_of_int(got_len) ++ "-tuple";
      let ind2 =
        got_indicator(
          "Got tuple of the wrong length",
          special_msg_bar(got_msg),
        );
      (ind1, ind2, TypeInconsistency);
    | AnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError);
    | AnaFree(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError);
    | AnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK);
    | AnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    | Synthesized(ty) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK);
    | SynInvalid =>
      let ind1 = expected_any_indicator;
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError);
    | SynFree =>
      let ind1 = expected_any_indicator;
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError);
    | SynKeyword(_keyword) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    | SynErrorArrow(expected_ty, got_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 = got_inconsistent_matched_indicator(got_ty, expected_ty);
      (ind1, ind2, TypeInconsistency);
    | SynMatchingArrow(syn_ty, matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        switch (syn_ty) {
        | HTyp.Hole =>
          got_indicator(
            "Got type ▶ matched to",
            matched_ty_bar(syn_ty, matched_ty),
          )
        | _ => got_indicator("Got", typebar(syn_ty))
        };
      (ind1, ind2, OK);
    | SynKeywordArrow(matched_ty, _k) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a keyword ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError);
    | SynInvalidArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got invalid text ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError);
    | SynFreeArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a free variable ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError);
    | SynBranchClause(join, typed, branch_index) =>
      let (ind1, ind2, err_state_b) = get_indicator_info(typed);
      let ind1 =
        switch (join) {
        | NoBranches => ind1
        | InconsistentBranchTys(rule_types, path_to_case) =>
          expected_inconsistent_branches_indicator(
            rule_types,
            path_to_case,
            branch_index,
          )
        | JoinTy(ty) => expected_ty_indicator_consistent(ty)
        };
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
        | (true, true) => (ind1, got_as_expected_ty_indicator(got_ty), OK)
        | (true, false) => (ind1, got_consistent_indicator(got_ty), OK)
        | (false, _) =>
          let (expected_diff, got_diff) = TypDiff.mk_diff(ty, got_ty);
          (
            expected_ty_indicator_consistent_diff(expected_diff),
            got_inconsistent_indicator_diff(got_diff),
            TypeInconsistency,
          );
        }
      | (InconsistentBranchTys(_), _) => (ind1, ind2, TypeInconsistency)
      | _ => (ind1, ind2, err_state_b)
      };
    | SynInconsistentBranches(rule_types, path_to_case) =>
      let ind1 = expected_any_indicator;
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      (ind1, ind2, TypeInconsistency);
    | SynInconsistentBranchesArrow(rule_types, path_to_case) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      (ind1, ind2, TypeInconsistency);
    | OnType =>
      let ind1 = expected_a_type_indicator;
      let ind2 = got_a_type_indicator;
      (ind1, ind2, OK);
    | PatAnalyzed(ty) =>
      let ind1 = expected_ty_indicator_pat(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      (ind1, ind2, OK);
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk_diff(expected_ty, got_ty);
      let ind1 = expected_ty_indicator_pat_diff(expected_diff);
      let ind2 = got_inconsistent_indicator_diff(got_diff);
      (ind1, ind2, TypeInconsistency);
    | PatAnaWrongLength(expected_len, got_len, _expected_ty) =>
      let expected_msg = string_of_int(expected_len) ++ "-tuple";
      let ind1 =
        expected_indicator(
          "Expecting a pattern of form",
          special_msg_bar(expected_msg),
        );
      let got_msg = string_of_int(got_len) ++ "-tuple";
      let ind2 =
        got_indicator(
          "Got tuple of the wrong length",
          special_msg_bar(got_msg),
        );
      (ind1, ind2, TypeInconsistency);
    | PatAnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError);
    | PatAnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK);
    | PatAnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    | PatSynthesized(ty) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK);
    | PatSynKeyword(_keyword) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    | OnLine =>
      /* TODO */
      let ind1 = expected_a_line_indicator;
      let ind2 = got_a_line_indicator;
      (ind1, ind2, OK);
    | OnRule =>
      /* TODO */
      let ind1 = expected_a_rule_indicator;
      let ind2 = got_a_rule_indicator;
      (ind1, ind2, OK);
    };

  let (ind1, ind2, err_state_b) = get_indicator_info(cursor_info.typed);

  // this determines the color
  let cls_of_err_state_b =
    switch (err_state_b) {
    | TypeInconsistency => "cursor-TypeInconsistency"
    | BindingError => "cursor-BindingError"
    | OK => "cursor-OK"
    };
  let (x, y) = loc;
  let pos_attr =
    Vdom.Attr.style(
      Css_gen.combine(
        Css_gen.left(`Px(int_of_float(x))),
        Css_gen.top(`Px(int_of_float(y))),
      ),
    );
  let above_or_below =
    switch (cursor_info.cursor_term) {
    | Exp(OnDelim(0, _), Case(_)) => "above"
    | _ => "below"
    };
  let summary =
    summary_bar(
      ~inject,
      cursor_info,
      err_state_b,
      cursor_inspector.show_expanded,
      cursor_inspector.term_novice_message_mode,
      cursor_inspector.type_novice_message_mode,
    );
  let content =
    if (cursor_inspector.show_expanded) {
      [summary, ind1, ind2];
    } else {
      [summary];
    };
  let content =
    switch (cursor_info.cursor_term, cursor_inspector.synthesizing) {
    | (Exp(_, EmptyHole(u)), Some((u', i, es, constraints))) when u == u' =>
      content
      @ [SynthPanel.view(~inject, ~view_of_text, i, es, u', constraints)]
    | _ => content
    };
  Vdom.(
    Node.div(
      [
        Attr.classes(["cursor-inspector-outer", above_or_below]),
        // stop propagation to code click handler
        Attr.on_mousedown(_ => Event.Stop_propagation),
        pos_attr,
      ],
      [
        Node.div(
          [Attr.classes(["panel", "cursor-inspector", cls_of_err_state_b])],
          content,
        ),
      ],
    )
  );
};
