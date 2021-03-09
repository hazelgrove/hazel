open Virtual_dom.Vdom;

type err_state_b =
  | TypeInconsistency
  | BindingError
  | OK;

let inconsistent_symbol =
  Node.div(
    [
      Attr.classes(["consistency-symbol", "inconsistent-symbol"]),
      Attr.create("title", "Inconsistent"),
    ],
    [Node.text(Unicode.inconsistent)],
  );

let emphasize_text = (~only_right=false, msg: string) => {
  let classes =
    only_right ? ["emphasize-text", "only-right"] : ["emphasize-text"];
  Node.div([Attr.classes(classes)], [Node.text(msg)]);
};

let colon = Node.div([Attr.classes(["colon"])], [Node.text(":")]);

let mk_expecting_of_type = (~article, ~term_tag) => [
  Node.text("Expecting " ++ article),
  term_tag,
  Node.text("of type"),
];

let advanced_summary = (typed: CursorInfo.typed, tag_typ: TermSort.t) => {
  let term_tag = TermTag.term_tag_view(tag_typ, ~show_tooltip=true, []);
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty)
    | SynMatchingArrow(_, ty) => [colon, HTypCode.view(ty)]
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      switch (ty) {
      | HTyp.Hole => [colon, emphasize_text("Any Type")]
      | _ => [colon, HTypCode.view(ty)]
      }
    /* Use the got type if not just Hole */
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      switch (got_ty) {
      | HTyp.Hole => [colon, HTypCode.view(expected_ty)]
      | _ => [colon, HTypCode.view(got_ty)]
      }
    | AnaTypeInconsistent(expected_ty, got_ty)
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk(expected_ty, got_ty);
      [
        colon,
        HTypCode.view(~diff_steps=expected_diff, expected_ty),
        inconsistent_symbol,
        HTypCode.view(~diff_steps=got_diff, got_ty),
      ];
    | SynErrorArrow(_expected_ty, got_ty) => [
        colon,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        HTypCode.view(got_ty),
      ]
    | AnaWrongLength(expected_len, got_len, _expected_ty)
    | PatAnaWrongLength(expected_len, got_len, _expected_ty) => [
        emphasize_text(string_of_int(expected_len) ++ "-tuple"),
        inconsistent_symbol,
        emphasize_text(string_of_int(got_len) ++ "-tuple"),
      ]
    | AnaInvalid(expected_ty)
    | PatAnaInvalid(expected_ty) => [
        colon,
        HTypCode.view(expected_ty),
        inconsistent_symbol,
        emphasize_text("Invalid Text"),
      ]
    | SynInvalid => [emphasize_text("Invalid Text")]
    | SynInvalidArrow(_) => [
        colon,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Invalid Text"),
      ]
    | AnaFree(expected_ty) => [
        colon,
        HTypCode.view(expected_ty),
        inconsistent_symbol,
        emphasize_text("Free Variable"),
      ]
    | SynFree => [emphasize_text("Free Variable")]
    | SynFreeArrow(_) => [
        colon,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Free Variable"),
      ]
    | AnaKeyword(expected_ty, _)
    | PatAnaKeyword(expected_ty, _) => [
        colon,
        HTypCode.view(expected_ty),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeyword(_)
    | PatSynKeyword(_) => [emphasize_text("Reserved Keyword")]
    | SynKeywordArrow(_) => [
        colon,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ]
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        if (HTyp.consistent(ty, got_ty)) {
          [colon, HTypCode.view(ty)];
        } else {
          let (ty_diff, got_diff) = TypDiff.mk(ty, got_ty);
          [
            colon,
            HTypCode.view(~diff_steps=ty_diff, ty),
            inconsistent_symbol,
            HTypCode.view(~diff_steps=got_diff, got_ty),
          ];
        }
      | (InconsistentBranchTys(_), _) => [
          emphasize_text("Inconsistent Branch Types"),
        ]
      | _ => message(typed)
      }
    | SynInconsistentBranches(_) => [
        emphasize_text("Inconsistent Branch Types"),
      ]
    | SynInconsistentBranchesArrow(_) => [
        colon,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | OnType => []
    | OnLine => [emphasize_text("Line")]
    | OnRule => [emphasize_text("Rule")]
    };
  };
  List.cons(term_tag, message(typed));
};

let novice_summary = (typed: CursorInfo.typed, tag_typ: TermSort.t) => {
  let term_tag = TermTag.term_tag_view(tag_typ, ~show_tooltip=true, []);
  let article =
    switch (tag_typ) {
    | Exp => "an"
    | Pat
    | Typ => "a"
    };
  let expecting_of_type = mk_expecting_of_type(~article, ~term_tag);
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty) => expecting_of_type @ [HTypCode.view(ty)]
    /* Use the got type if not just a Hole */
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      switch (got_ty) {
      | HTyp.Hole => expecting_of_type @ [HTypCode.view(expected_ty)]
      | _ => [
          Node.text("Got " ++ article),
          term_tag,
          Node.text("of type"),
          HTypCode.view(got_ty),
        ]
      }
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      switch (ty) {
      | HTyp.Hole => [
          Node.text("Expecting " ++ article),
          term_tag,
          Node.text("of"),
          emphasize_text("Any Type"),
        ]
      | _ => [
          Node.text("Got " ++ article),
          term_tag,
          Node.text("of type"),
          HTypCode.view(ty),
        ]
      }
    | SynMatchingArrow(_, ty) => [
        Node.text("Got " ++ article),
        term_tag,
        Node.text("of type"),
        HTypCode.view(ty),
      ]
    | AnaTypeInconsistent(expected_ty, got_ty)
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk(expected_ty, got_ty);

      expecting_of_type
      @ [
        HTypCode.view(~diff_steps=expected_diff, expected_ty),
        Node.text("but got inconsistent type"),
        HTypCode.view(~diff_steps=got_diff, got_ty),
      ];
    | SynErrorArrow(_expected_ty, got_ty) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("of"),
        emphasize_text("Function Type"),
        Node.text("but got inconsistent type"),
        HTypCode.view(got_ty),
      ]
    | AnaWrongLength(expected_len, got_len, _expected_ty)
    | PatAnaWrongLength(expected_len, got_len, _expected_ty) =>
      expecting_of_type
      @ [
        emphasize_text(string_of_int(expected_len) ++ "-tuple"),
        Node.text("but got"),
        emphasize_text(string_of_int(got_len) ++ "-tuple"),
      ]
    | AnaInvalid(expected_ty)
    | PatAnaInvalid(expected_ty) =>
      expecting_of_type
      @ [
        HTypCode.view(expected_ty),
        Node.text("but got"),
        emphasize_text("Invalid Text"),
      ]
    | SynInvalid => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text(~only_right=true, "Invalid Text"),
      ]
    | SynInvalidArrow(_) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("of"),
        emphasize_text("Function Type"),
        Node.text("but got"),
        emphasize_text("Invalid Text"),
      ]
    | AnaFree(expected_ty) =>
      expecting_of_type
      @ [
        HTypCode.view(expected_ty),
        Node.text("but got a"),
        emphasize_text("Free Variable"),
      ]
    | SynFree => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text(~only_right=true, "Free Variable"),
      ]
    | SynFreeArrow(_) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("of"),
        emphasize_text("Function Type"),
        Node.text("but got a"),
        emphasize_text("Free Variable"),
      ]
    | AnaKeyword(expected_ty, _)
    | PatAnaKeyword(expected_ty, _) =>
      expecting_of_type
      @ [
        HTypCode.view(expected_ty),
        Node.text("but got a"),
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeyword(_)
    | PatSynKeyword(_) => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeywordArrow(_) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("of"),
        emphasize_text("Function Type"),
        Node.text("but got a"),
        emphasize_text("Reserved Keyword"),
      ]
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        if (HTyp.consistent(ty, got_ty)) {
          [
            Node.text("Got " ++ article),
            term_tag,
            Node.text("of type"),
            HTypCode.view(ty),
          ];
        } else {
          let (ty_diff, got_diff) = TypDiff.mk(ty, got_ty);
          expecting_of_type
          @ [
            HTypCode.view(~diff_steps=ty_diff, ty),
            Node.text("but got inconsistent type"),
            HTypCode.view(~diff_steps=got_diff, got_ty),
          ];
        }
      | (InconsistentBranchTys(_), _) => [
          Node.text("Got " ++ article),
          term_tag,
          emphasize_text("Inconsistent Branch Types"),
        ]
      | _ => message(typed)
      }
    | SynInconsistentBranches(_) => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | SynInconsistentBranchesArrow(_) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("of"),
        emphasize_text("Function Type"),
        Node.text("but got"),
        emphasize_text("Inconsistent Branch Types"),
      ]
    | OnType => [Node.text("Got " ++ article), term_tag]
    | OnLine => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text(~only_right=true, "Line"),
      ]
    | OnRule => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text(~only_right=true, "Rule"),
      ]
    };
  };
  message(typed);
};

let summary_bar =
    (
      ~inject: ModelAction.t => Event.t,
      ci: CursorInfo.t,
      show: bool,
      show_expanded: bool,
      novice_mode: bool,
      show_strategy_guide: bool,
    ) => {
  let arrow_direction =
    if (show_expanded) {
      Icons.down_arrow(["cursor-inspector-arrow"]);
    } else {
      Icons.left_arrow(["cursor-inspector-arrow"]);
    };
  let arrow =
    Node.div(
      [
        Attr.classes(["clickable"]),
        Attr.on_click(_ =>
          Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            inject(
              ModelAction.UpdateSettings(
                CursorInspector(Toggle_show_expanded),
              ),
            ),
          ])
        ),
      ],
      [arrow_direction],
    );
  let tag_type = TermTag.get_cursor_term_sort(ci.cursor_term);
  let summary =
    Node.div(
      [
        Attr.create(
          "title",
          "Click to view expanded form of symbols in message",
        ),
        Attr.classes(
          novice_mode
            ? ["clickable", "summary-message", "novice-mode"]
            : ["clickable", "summary-message"],
        ),
        Attr.on_click(_ =>
          Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            inject(
              ModelAction.UpdateSettings(
                CursorInspector(Toggle_novice_mode),
              ),
            ),
          ])
        ),
      ],
      novice_mode
        ? novice_summary(ci.typed, tag_type)
        : advanced_summary(ci.typed, tag_type),
    );
  let fill_icon =
    Node.div(
      [
        Attr.classes(["clickable"]),
        Attr.on_click(_ =>
          Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            inject(
              ModelAction.UpdateSettings(
                CursorInspector(Toggle_type_assist),
              ),
            ),
          ])
        ),
      ],
      [Node.text(Unicode.light_bulb)],
    );
  let fill_space = Node.span([Attr.classes(["filler"])], []);
  let body = show ? [summary, fill_space, arrow] : [summary];
  let body =
    show_strategy_guide ? List.append(body, [fill_space, fill_icon]) : body;
  Node.div([Attr.classes(["type-info-summary"])], body);
};

let view =
    (
      ~inject: ModelAction.t => Event.t,
      loc: (float, float),
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
    )
    : Node.t => {
  let typebar = ty =>
    Node.div([Attr.classes(["infobar", "typebar"])], [HTypCode.view(ty)]);
  let typebar_diff = (diff_steps, ty) =>
    Node.div(
      [Attr.classes(["infobar", "typebar"])],
      [HTypCode.view(~diff_steps, ty)],
    );
  let matched_ty_bar = (ty1, ty2) =>
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
    );
  let inconsistent_branches_ty_bar =
      (branch_types, path_to_case, skipped_index) =>
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
    );

  let special_msg_bar = (msg: string) =>
    Node.div(
      [Attr.classes(["infobar", "special-msg-bar"])],
      [Node.text(msg)],
    );

  let expected_indicator = (title_text, type_div) =>
    Node.div(
      [Attr.classes(["indicator", "expected-indicator"])],
      [Panel.view_of_main_title_bar(title_text), type_div],
    );
  let expected_ty_title = "Expecting an expression of type";
  let expected_ty_title_pat = "Expecting a pattern of type";
  let expected_ty_title_consistent = "Expecting an expression consistent with type";
  let expected_ty_indicator = ty =>
    expected_indicator(expected_ty_title, typebar(ty));
  let expected_ty_indicator_diff = (diff_steps, ty) =>
    expected_indicator(expected_ty_title, typebar_diff(diff_steps, ty));
  let expected_ty_indicator_pat = ty =>
    expected_indicator(expected_ty_title_pat, typebar(ty));
  let expected_ty_indicator_pat_diff = (diff_steps, ty) =>
    expected_indicator(expected_ty_title_pat, typebar_diff(diff_steps, ty));
  let expected_ty_indicator_consistent = ty =>
    expected_indicator(expected_ty_title_consistent, typebar(ty));
  let expected_ty_indicator_consistent_diff = (diff_steps, ty) =>
    expected_indicator(
      expected_ty_title_consistent,
      typebar_diff(diff_steps, ty),
    );
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
    Node.div(
      [Attr.classes(["indicator", "got-indicator"])],
      [Panel.view_of_other_title_bar(title_text), type_div],
    );
  let got_ty_indicator = ty => got_indicator("Got type", typebar(ty));
  let got_as_expected_ty_indicator = ty =>
    got_indicator("Got as expected", typebar(ty));
  let got_inconsistent_indicator_diff = (diff_steps, got_ty) =>
    got_indicator("Got inconsistent type", typebar_diff(diff_steps, got_ty));
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
      (ind1, ind2, OK, false);
    | AnaAnnotatedLambda(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK, false);
    | AnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk(expected_ty, got_ty);
      let ind1 = expected_ty_indicator_diff(expected_diff, expected_ty);
      let ind2 = got_inconsistent_indicator_diff(got_diff, got_ty);
      (ind1, ind2, TypeInconsistency, false);
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
      (ind1, ind2, TypeInconsistency, false);
    | AnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError, false);
    | AnaFree(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError, false);
    | AnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK, false);
    | AnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, false);
    | Synthesized(ty) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK, false);
    | SynInvalid =>
      let ind1 = expected_any_indicator;
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError, false);
    | SynFree =>
      let ind1 = expected_any_indicator;
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError, false);
    | SynKeyword(_keyword) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, false);
    | SynErrorArrow(expected_ty, got_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 = got_inconsistent_matched_indicator(got_ty, expected_ty);
      (ind1, ind2, TypeInconsistency, false);
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
      (ind1, ind2, OK, false);
    | SynKeywordArrow(matched_ty, _k) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a keyword ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError, false);
    | SynInvalidArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got invalid text ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError, false);
    | SynFreeArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a free variable ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError, false);
    | SynBranchClause(join, typed, branch_index) =>
      let (ind1, ind2, err_state_b, _) = get_indicator_info(typed);
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
        | (true, true) => (
            ind1,
            got_as_expected_ty_indicator(got_ty),
            OK,
            false,
          )
        | (true, false) => (
            ind1,
            got_consistent_indicator(got_ty),
            OK,
            false,
          )
        | (false, _) =>
          let (expected_diff, got_diff) = TypDiff.mk(ty, got_ty);
          (
            expected_ty_indicator_consistent_diff(expected_diff, ty),
            got_inconsistent_indicator_diff(got_diff, got_ty),
            TypeInconsistency,
            false,
          );
        }
      | (InconsistentBranchTys(_), _) => (
          ind1,
          ind2,
          TypeInconsistency,
          true,
        )
      | _ => (ind1, ind2, err_state_b, false)
      };
    | SynInconsistentBranches(rule_types, path_to_case) =>
      let ind1 = expected_any_indicator;
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      (ind1, ind2, TypeInconsistency, true);
    | SynInconsistentBranchesArrow(rule_types, path_to_case) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      (ind1, ind2, TypeInconsistency, false);
    | OnType =>
      let ind1 = expected_a_type_indicator;
      let ind2 = got_a_type_indicator;
      (ind1, ind2, OK, false);
    | PatAnalyzed(ty) =>
      let ind1 = expected_ty_indicator_pat(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      (ind1, ind2, OK, false);
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk(expected_ty, got_ty);
      let ind1 = expected_ty_indicator_pat_diff(expected_diff, expected_ty);
      let ind2 = got_inconsistent_indicator_diff(got_diff, got_ty);
      (ind1, ind2, TypeInconsistency, false);
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
      (ind1, ind2, TypeInconsistency, false);
    | PatAnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError, false);
    | PatAnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK, false);
    | PatAnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, false);
    | PatSynthesized(ty) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK, false);
    | PatSynKeyword(_keyword) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, false);
    | OnLine =>
      /* TODO */
      let ind1 = expected_a_line_indicator;
      let ind2 = got_a_line_indicator;
      (ind1, ind2, OK, false);
    | OnRule =>
      /* TODO */
      let ind1 = expected_a_rule_indicator;
      let ind2 = got_a_rule_indicator;
      (ind1, ind2, OK, false);
    };

  let (ind1, ind2, err_state_b, show) =
    get_indicator_info(cursor_info.typed);

  // this determines the color
  let cls_of_err_state_b =
    switch (err_state_b) {
    | TypeInconsistency => "cursor-TypeInconsistency"
    | BindingError => "cursor-BindingError"
    | OK => "cursor-OK"
    };
  let (x, y) = loc;
  let pos_attr =
    Attr.style(
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
  let on_empty_hole =
    switch (cursor_info.cursor_term) {
    | Exp(_, EmptyHole(_)) => true
    | Exp(_, _) => false
    | Pat(_, EmptyHole(_)) => true
    | Pat(_, _) => false
    | Typ(_, Hole) => false
    | Typ(_, _) => false
    | ExpOp(_, _)
    | PatOp(_, _)
    | TypOp(_, _)
    | Line(_, _)
    | Rule(_, _) => false
    };
  let summary =
    summary_bar(
      ~inject,
      cursor_info,
      show,
      cursor_inspector.show_expanded,
      cursor_inspector.novice_mode,
      on_empty_hole,
    );
  let content =
    if (cursor_inspector.show_expanded && show) {
      [summary, ind1, ind2];
    } else {
      [summary];
    };
  let content =
    if (cursor_inspector.type_assist && on_empty_hole) {
      List.append(
        content,
        [StrategyGuide.view(~inject, cursor_inspector, cursor_info)],
      );
    } else {
      content;
    };
  Node.div(
    [
      Attr.id("cursor-inspector"),
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
  );
};
