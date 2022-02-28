open Virtual_dom.Vdom;

type err_state_b =
  | TypeInconsistency
  | BindingError
  | OK;

let inconsistent_symbol =
  Node.div(
    [
      Attr.classes(["consistency-symbol", "inconsistent"]),
      Attr.create("title", "Inconsistent"),
    ],
    [Node.text(Unicode.inconsistent)],
  );

let consistent_symbol =
  Node.div(
    [
      Attr.classes(["consistency-symbol", "consistent"]),
      Attr.create("title", "Consistent"),
    ],
    [Node.text("~")],
  );

let emphasize_text = (~only_right=false, msg: string) => {
  let classes =
    only_right ? ["emphasize-text", "only-right"] : ["emphasize-text"];
  Node.div([Attr.classes(classes)], [Node.text(msg)]);
};

let syn =
  Node.div(
    [Attr.classes(["bidirectional"]), Attr.create("title", "Synthesize")],
    [Node.text(Unicode.synSym)],
  );
let ana =
  Node.div(
    [Attr.classes(["bidirectional"]), Attr.create("title", "Analyze")],
    [Node.text(Unicode.anaSym)],
  );

let mk_expecting_of_type = (~article, ~term_tag) => [
  Node.text("Expecting " ++ article),
  term_tag,
  Node.text("of type"),
];

let any_typ_msg =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
    ) =>
  Node.div(
    [Attr.classes(["compressed"])],
    [
      emphasize_text("Any Type ("),
      HTypCode.view(~inject, ~selected_tag_hole, HTyp.Hole),
      emphasize_text(")"),
    ],
  );

let exp_keyword_msg = (term, keyword, main_msg) =>
  if (CursorInfo_common.is_end_keyword(term, keyword)) {
    main_msg
    @ [
      Node.text("("),
      AssistantView_common.kc_shortcut_node(HazelKeyCombos.Space),
      Node.text(" to expand keyword)"),
    ];
  } else {
    main_msg;
  };

let pat_ana_subsumed_msg =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
      expected_ty,
      got_ty,
      expecting_msg,
      consistency_msg,
    ) =>
  if (HTyp.eq(expected_ty, got_ty) || HTyp.eq(got_ty, HTyp.Hole)) {
    expecting_msg @ [HTypCode.view(~inject, ~selected_tag_hole, expected_ty)];
  } else {
    expecting_msg
    @ [
      HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
      consistency_msg,
      HTypCode.view(~inject, ~selected_tag_hole, got_ty),
    ];
  };

let syn_branch_clause_msg =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
      join,
      typed,
      join_type_consistent,
      join_type_inconsistent_expecting,
      join_type_inconsistent_msg,
      other,
    ) => {
  switch (join, typed) {
  | (CursorInfo.JoinTy(ty), CursorInfo.Synthesized(got_ty)) =>
    if (HTyp.consistent(ty, got_ty)) {
      join_type_consistent @ [HTypCode.view(~inject, ~selected_tag_hole, ty)];
    } else {
      let (ty_diff, got_diff) = TypDiff.mk(ty, got_ty);
      join_type_inconsistent_expecting
      @ [
        HTypCode.view(~inject, ~selected_tag_hole, ~diff_steps=ty_diff, ty),
        join_type_inconsistent_msg,
        HTypCode.view(
          ~inject,
          ~selected_tag_hole,
          ~diff_steps=got_diff,
          got_ty,
        ),
      ];
    }
  | _ => other(typed)
  };
};

let advanced_summary =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
      typed: CursorInfo.typed,
      term: CursorInfo.cursor_term,
      tag_typ: TermSort.t,
    ) => {
  let term_tag = TermTag.term_tag_view(tag_typ, ~show_tooltip=true, []);
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, ty),
      ]
    | SynMatchingArrow(_, ty) => [
        syn,
        HTypCode.view(~inject, ~selected_tag_hole, ty),
      ]
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      switch (term) {
      | ExpOperand(_, EmptyHole(_)) => [
          syn,
          any_typ_msg(~inject, ~selected_tag_hole),
        ]
      | _ => [syn, HTypCode.view(~inject, ~selected_tag_hole, ty)]
      }
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      pat_ana_subsumed_msg(
        ~inject,
        ~selected_tag_hole,
        expected_ty,
        got_ty,
        [ana],
        consistent_symbol,
      )
    | AnaTypeInconsistent(expected_ty, got_ty)
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk(expected_ty, got_ty);
      [
        ana,
        HTypCode.view(
          ~inject,
          ~selected_tag_hole,
          ~diff_steps=expected_diff,
          expected_ty,
        ),
        inconsistent_symbol,
        HTypCode.view(
          ~inject,
          ~selected_tag_hole,
          ~diff_steps=got_diff,
          got_ty,
        ),
      ];
    | SynErrorArrow(_expected_ty, got_ty) => [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        HTypCode.view(~inject, ~selected_tag_hole, got_ty),
      ]
    | AnaWrongLength(expected_len, got_len, _expected_ty)
    | PatAnaWrongLength(expected_len, got_len, _expected_ty) => [
        ana,
        emphasize_text(string_of_int(expected_len) ++ "-tuple"),
        inconsistent_symbol,
        emphasize_text(string_of_int(got_len) ++ "-tuple"),
      ]
    | AnaInvalid(expected_ty)
    | PatAnaInvalid(expected_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Invalid Text"),
      ]
    | SynInvalid => [syn, emphasize_text("Invalid Text")]
    | SynInvalidArrow(_) => [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Invalid Text"),
      ]
    | AnaInjExpectedTypeNotConsistentWithSums(expected_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Inconsistent With Sum Type"),
      ]
    | PatAnaInjExpectedTypeNotConsistentWithSums(expected_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Inconsistent With Sum Type"),
      ]
    | AnaInjExpectedArg(expected_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Missing Argument"),
      ]
    | PatAnaInjExpectedArg(expected_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Missing Argument"),
      ]
    | AnaInjUnexpectedArg(got_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, got_ty),
        inconsistent_symbol,
        emphasize_text("Unexpected Argument"),
      ]
    | OnInvalidTag(_) => [emphasize_text("Invalid Tag Name")]
    | OnUnknownTag(_) => [emphasize_text("Unknown Tag")]
    | OnDuplicateTag(_) => [emphasize_text("Duplicate Tag")]
    | PatAnaInjUnexpectedArg(got_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, got_ty),
        inconsistent_symbol,
        emphasize_text("Unexpected Argument"),
      ]
    | AnaFree(expected_ty) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Free Variable"),
      ]
    | SynFree => [syn, emphasize_text("Free Variable")]
    | SynFreeArrow(_) => [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Free Variable"),
      ]
    | AnaKeyword(expected_ty, keyword) =>
      let main_msg = [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ];
      exp_keyword_msg(term, keyword, main_msg);
    | PatAnaKeyword(expected_ty, _) => [
        ana,
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeyword(keyword) =>
      let main_msg = [syn, emphasize_text("Reserved Keyword")];
      exp_keyword_msg(term, keyword, main_msg);
    | PatSynKeyword(_) => [syn, emphasize_text("Reserved Keyword")]
    | SynKeywordArrow(_, keyword) =>
      let main_msg = [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ];
      exp_keyword_msg(term, keyword, main_msg);
    | SynBranchClause(join, typed, _) =>
      syn_branch_clause_msg(
        ~inject,
        ~selected_tag_hole,
        join,
        typed,
        [syn],
        [syn],
        inconsistent_symbol,
        message,
      )
    | SynInconsistentBranches(_) => [
        syn,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | AnaInconsistentBranches(_) => [
        ana,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | SynInconsistentBranchesArrow(_) => [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | OnType => []
    | OnTag => [emphasize_text("Tag")]
    | OnTagHole => [emphasize_text("Tag Hole")]
    | OnSumBodyOperand => [emphasize_text("Sum body operand")]
    | OnSumBodyOperator => [emphasize_text("Sum body operator")]
    | OnNonLetLine => /* TODO */ [emphasize_text("Line")]
    | OnRule(NotRedundant) => /* TODO */ [emphasize_text("Rule")]
    | OnRule(Redundant(_)) => /* TODO */ [emphasize_text("Redundant Rule")]
    | CaseNotExhaustive(_) => [emphasize_text("Non-Exhaustive Case")]
    };
  };
  switch (typed) {
  | OnNonLetLine => message(typed) /* Don't display the term tag for empty and comment lines */
  | _ => List.cons(term_tag, message(typed))
  };
};

let novice_summary =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~selected_tag_hole: option(MetaVar.t),
      typed: CursorInfo.typed,
      term: CursorInfo.cursor_term,
      tag_typ: TermSort.t,
    ) => {
  let term_tag = TermTag.term_tag_view(tag_typ, ~show_tooltip=true, []);
  let article = AssistantView_common.article(tag_typ);
  let expecting_of_type = mk_expecting_of_type(~article, ~term_tag);
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty) =>
      expecting_of_type @ [HTypCode.view(~inject, ~selected_tag_hole, ty)]
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      pat_ana_subsumed_msg(
        ~inject,
        ~selected_tag_hole,
        expected_ty,
        got_ty,
        expecting_of_type,
        Node.text("and got consistent type"),
      )
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      switch (term) {
      | ExpOperand(_, EmptyHole(_)) => [
          Node.text("Got " ++ article),
          term_tag,
          Node.text("of"),
          any_typ_msg(~inject, ~selected_tag_hole),
        ]
      | _ => [
          Node.text("Got " ++ article),
          term_tag,
          Node.text("of type"),
          HTypCode.view(~inject, ~selected_tag_hole, ty),
        ]
      }
    | SynMatchingArrow(_, ty) => [
        Node.text("Got " ++ article),
        term_tag,
        Node.text("of type"),
        HTypCode.view(~inject, ~selected_tag_hole, ty),
      ]
    | AnaTypeInconsistent(expected_ty, got_ty)
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk(expected_ty, got_ty);
      expecting_of_type
      @ [
        HTypCode.view(
          ~inject,
          ~selected_tag_hole,
          ~diff_steps=expected_diff,
          expected_ty,
        ),
        Node.text("but got inconsistent type"),
        HTypCode.view(
          ~inject,
          ~selected_tag_hole,
          ~diff_steps=got_diff,
          got_ty,
        ),
      ];
    | SynErrorArrow(_expected_ty, got_ty) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("of"),
        emphasize_text("Function Type"),
        Node.text("but got inconsistent type"),
        HTypCode.view(~inject, ~selected_tag_hole, got_ty),
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
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
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
    | AnaInjExpectedTypeNotConsistentWithSums(expected_ty) =>
      expecting_of_type
      @ [
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        Node.text("but got"),
        emphasize_text("An Injection"),
      ]
    | AnaInjExpectedArg(expected_ty) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("injection with argument of type"),
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        Node.text("but got"),
        emphasize_text("A Constant Injection"),
      ]
    | AnaInjUnexpectedArg(got_ty) => [
        Node.text("Got " ++ article),
        term_tag,
        Node.text("unary injection with argument type"),
        HTypCode.view(~inject, ~selected_tag_hole, got_ty),
        Node.text("but expected"),
        emphasize_text("A Constant Injection"),
      ]
    | PatAnaInjExpectedTypeNotConsistentWithSums(expected_ty) =>
      expecting_of_type
      @ [
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        Node.text("but got"),
        emphasize_text("An Injection"),
      ]
    | PatAnaInjExpectedArg(expected_ty) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("injection with argument of type"),
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        Node.text("but got"),
        emphasize_text("A Constant Injection"),
      ]
    | PatAnaInjUnexpectedArg(got_ty) => [
        Node.text("Got " ++ article),
        term_tag,
        Node.text("an injection with argument of type"),
        HTypCode.view(~inject, ~selected_tag_hole, got_ty),
        Node.text("but expected"),
        emphasize_text("A Constant Injection"),
      ]
    | AnaFree(expected_ty) =>
      expecting_of_type
      @ [
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
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
    | AnaKeyword(expected_ty, keyword) =>
      let main_msg =
        expecting_of_type
        @ [
          HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
          Node.text("but got a"),
          emphasize_text("Reserved Keyword"),
        ];
      exp_keyword_msg(term, keyword, main_msg);
    | PatAnaKeyword(expected_ty, _) =>
      expecting_of_type
      @ [
        HTypCode.view(~inject, ~selected_tag_hole, expected_ty),
        Node.text("but got a"),
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeyword(keyword) =>
      let main_msg = [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text("Reserved Keyword"),
      ];
      exp_keyword_msg(term, keyword, main_msg);
    | PatSynKeyword(_) => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeywordArrow(_, keyword) =>
      let main_msg = [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("of"),
        emphasize_text("Function Type"),
        Node.text("but got a"),
        emphasize_text("Reserved Keyword"),
      ];
      exp_keyword_msg(term, keyword, main_msg);
    | SynBranchClause(join, typed, _) =>
      syn_branch_clause_msg(
        ~inject,
        ~selected_tag_hole,
        join,
        typed,
        [Node.text("Got " ++ article), term_tag, Node.text("of type")],
        expecting_of_type,
        Node.text("but got inconsistent type"),
        message,
      )
    | SynInconsistentBranches(_) => [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | AnaInconsistentBranches(_) => [
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
    | OnType
    | OnTag
    | OnTagHole
    | OnSumBodyOperand
    | OnSumBodyOperator => [Node.text("Got " ++ article), term_tag]
    | OnInvalidTag(_) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("but got"),
        emphasize_text("Invalid Tag Name"),
      ]
    | OnUnknownTag(_) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("but got"),
        emphasize_text("Unknown Tag"),
      ]
    | OnDuplicateTag(_) => [
        Node.text("Expecting " ++ article),
        term_tag,
        Node.text("but got"),
        emphasize_text("Duplicate Tag"),
      ]
    | OnNonLetLine => /* TODO */ [
        Node.text("Got a "),
        /* Don't show the term tag for empty and comment lines */
        emphasize_text(~only_right=true, "Line"),
      ]
    | OnRule(Redundant(_)) => [
        Node.text("Got a"),
        emphasize_text("Redundant Rule"),
      ]
    | OnRule(NotRedundant) => [Node.text("Got a"), emphasize_text("Rule")]
    | CaseNotExhaustive(_) => [
        Node.text("Got a"),
        emphasize_text("Non-Exhaustive Case"),
      ]
    };
  };
  message(typed);
};

let summary_bar =
    (
      ~inject: ModelAction.t => Event.t,
      ~selected_tag_hole: option(MetaVar.t),
      ci: CursorInfo.t,
      show_expansion_arrow: bool,
      show_expanded: bool,
      novice_mode: bool,
      show_strategy_guide_icon: bool,
    ) => {
  let toggle_cursor_inspector_event = toggle =>
    Event.Many([
      Event.Prevent_default,
      Event.Stop_propagation,
      inject(ModelAction.UpdateCursorInspector(toggle)),
    ]);
  let arrow_direction =
    if (show_expanded) {
      Icons.down_arrow(["cursor-inspector-arrow"]);
    } else {
      Icons.left_arrow(["cursor-inspector-arrow"]);
    };
  let arrow =
    Node.div(
      [
        Attr.classes(["clickable-help"]),
        Attr.create("title", "Click to toggle expanded cursor inspector"),
        Attr.on_click(_ =>
          toggle_cursor_inspector_event(Toggle_show_expanded)
        ),
      ],
      [arrow_direction],
    );
  let tag_type = TermTag.get_cursor_term_sort(ci.cursor_term);
  let summary =
    Node.div(
      [
        Attr.classes(
          novice_mode
            ? ["summary-message", "novice-mode"] : ["summary-message"],
        ),
      ],
      novice_mode
        ? novice_summary(
            ~inject,
            ~selected_tag_hole,
            ci.typed,
            ci.cursor_term,
            tag_type,
          )
        : advanced_summary(
            ~inject,
            ~selected_tag_hole,
            ci.typed,
            ci.cursor_term,
            tag_type,
          ),
    );
  let fill_icon =
    Node.div(
      [
        Attr.classes(["clickable-help"]),
        Attr.create("title", "Click to toggle strategy guide"),
        Attr.on_click(_ =>
          toggle_cursor_inspector_event(Toggle_strategy_guide)
        ),
      ],
      [Node.text(Unicode.light_bulb)],
    );
  let fill_space = Node.span([Attr.classes(["filler"])], []);
  let body =
    switch (show_expansion_arrow, show_strategy_guide_icon) {
    | (true, true) => [summary, fill_space, arrow, fill_icon]
    | (true, false) => [summary, fill_space, arrow]
    | (false, true) => [summary, fill_space, fill_icon]
    | (false, false) => [summary]
    };
  Node.div(
    [
      Attr.create("title", "Click to toggle form of message"),
      Attr.classes(["type-info-summary", "clickable-help"]),
      Attr.on_click(_ => toggle_cursor_inspector_event(Toggle_novice_mode)),
    ],
    body,
  );
};

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~selected_tag_hole: option(MetaVar.t),
      ~loc: (float, float),
      cursor_inspector: CursorInspectorModel.t,
      cursor_info: CursorInfo.t,
    )
    : Node.t => {
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
            [HTypCode.view(~inject, ~selected_tag_hole, ty)],
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
  let expected_msg_indicator = msg =>
    expected_indicator("Expecting an expression of ", special_msg_bar(msg));
  let expected_any_indicator = expected_msg_indicator("any type");
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
  let got_inconsistent_branches_indicator = (branch_types, path_to_case) =>
    got_indicator(
      "Got inconsistent branch types",
      inconsistent_branches_ty_bar(branch_types, path_to_case, None),
    );

  let expanded_msg =
    switch (cursor_info.typed) {
    | SynBranchClause(
        InconsistentBranchTys(rule_types, path_to_case),
        _,
        branch_index,
      ) =>
      let ind =
        expected_inconsistent_branches_indicator(
          rule_types,
          path_to_case,
          branch_index,
        );
      Some([ind]);
    | SynInconsistentBranches(rule_types, path_to_case) =>
      let ind1 = expected_any_indicator;
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      Some([ind1, ind2]);
    | SynInconsistentBranchesArrow(rule_types, path_to_case) =>
      let ind = got_inconsistent_branches_indicator(rule_types, path_to_case);
      Some([ind]);
    | _ => None
    };

  let rec get_err_state_b = (typed: CursorInfo.typed) =>
    switch (typed) {
    | Analyzed(_)
    | AnaAnnotatedLambda(_)
    | AnaSubsumed(_)
    | Synthesized(_)
    | SynMatchingArrow(_)
    | OnType
    | OnTag
    | OnTagHole
    | OnSumBodyOperand
    | OnSumBodyOperator
    | PatAnalyzed(_)
    | PatAnaSubsumed(_)
    | PatSynthesized(_)
    | OnNonLetLine
    | OnRule(NotRedundant) => OK
    | AnaTypeInconsistent(_)
    | AnaWrongLength(_)
    | AnaInjExpectedTypeNotConsistentWithSums(_)
    | AnaInjExpectedArg(_)
    | AnaInjUnexpectedArg(_)
    | SynErrorArrow(_)
    | SynInconsistentBranches(_)
    | AnaInconsistentBranches(_)
    | SynInconsistentBranchesArrow(_)
    | PatAnaInjExpectedTypeNotConsistentWithSums(_)
    | PatAnaInjExpectedArg(_)
    | PatAnaInjUnexpectedArg(_)
    | PatAnaTypeInconsistent(_)
    | PatAnaWrongLength(_) => TypeInconsistency
    | AnaInvalid(_)
    | AnaFree(_)
    | AnaKeyword(_)
    | SynInvalid
    | SynFree
    | SynKeyword(_)
    | SynKeywordArrow(_)
    | SynInvalidArrow(_)
    | SynFreeArrow(_)
    | PatAnaInvalid(_)
    | PatAnaKeyword(_)
    | PatSynKeyword(_)
    | OnInvalidTag(_)
    | OnUnknownTag(_)
    | OnDuplicateTag(_)
    | OnRule(Redundant(_))
    | CaseNotExhaustive(_) => BindingError
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        if (HTyp.consistent(ty, got_ty)) {
          OK;
        } else {
          TypeInconsistency;
        }
      | (InconsistentBranchTys(_), _) => TypeInconsistency
      | _ => get_err_state_b(typed)
      }
    };

  let err_state_b = get_err_state_b(cursor_info.typed);

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
    | ExpOperand(OnDelim(0, _), Case(_)) => "above"
    | _ => "below"
    };
  let (show_strategy_guide_icon, strategy_guide) =
    switch (cursor_info.cursor_term, cursor_info.parent_info) {
    | (ExpOperand(_, EmptyHole(_)), _) => (
        true,
        Some(
          StrategyGuide.exp_hole_view(~inject, cursor_inspector, cursor_info),
        ),
      )
    | (Rule(_), _)
    | (ExpOperand(_, Case(_)), _)
    | (_, AfterBranchClause) =>
      switch (StrategyGuide.rules_view(cursor_info)) {
      | None => (false, None)
      | Some(sg_rules) => (
          true,
          Some((~selected_tag_hole as _) => sg_rules),
        )
      }
    | (Line(_, EmptyLine), _) => (
        true,
        Some((~selected_tag_hole as _) => StrategyGuide.lines_view(true)),
      )
    | (Line(_), _) => (
        true,
        Some((~selected_tag_hole as _) => StrategyGuide.lines_view(false)),
      )
    | _ => (false, None)
    };
  let show_expansion_arrow =
    switch (expanded_msg) {
    | Some(_) => true
    | None => false
    };
  let summary =
    summary_bar(
      ~inject,
      cursor_info,
      show_expansion_arrow,
      cursor_inspector.show_expanded,
      cursor_inspector.novice_mode,
      show_strategy_guide_icon,
    );
  let content =
    switch (cursor_inspector.show_expanded, expanded_msg) {
    | (true, Some(ind)) => [
        summary,
        ...List.map((node, ~selected_tag_hole as _) => node, ind),
      ]
    | _ => [summary]
    };
  let content =
    switch (cursor_inspector.strategy_guide, strategy_guide) {
    | (true, Some(strategy_guide)) =>
      List.append(content, [strategy_guide])
    | _ => content
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
        content |> List.map(f => f(~selected_tag_hole)),
      ),
    ],
  );
};
