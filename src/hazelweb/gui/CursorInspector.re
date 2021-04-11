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
  Node.div(
    [Attr.classes(["compressed"])],
    [
      emphasize_text("Any Type ("),
      HTypCode.view(HTyp.Hole),
      emphasize_text(")"),
    ],
  );

let is_end_keyword =
    (term: CursorInfo.cursor_term, keyword: ExpandingKeyword.t) =>
  switch (term) {
  | Exp(OnText(index), _) =>
    index == String.length(ExpandingKeyword.to_string(keyword))
  | _ => false
  };
/* TODO: don't just copy and paste functions like this from Strategy Guide*/
let shortcut_node = text =>
  Node.div([Attr.classes(["code-font", "shortcut"])], [Node.text(text)]);

let keyword_msg = (term, keyword, main_msg) =>
  if (is_end_keyword(term, keyword)) {
    main_msg
    @ [
      Node.text("("),
      shortcut_node("Space"),
      Node.text(" to expand keyword)"),
    ];
  } else {
    main_msg;
  };

let advanced_summary =
    (
      typed: CursorInfo.typed,
      term: CursorInfo.cursor_term,
      tag_typ: TermSort.t,
    ) => {
  let term_tag = TermTag.term_tag_view(tag_typ, ~show_tooltip=true, []);
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty) => [ana, HTypCode.view(ty)]
    | SynMatchingArrow(_, ty) => [syn, HTypCode.view(ty)]
    | Synthesized(ty)
    | PatSynthesized(ty)
    | OnLetLine(ty) =>
      switch (ty) {
      | HTyp.Hole => [syn, any_typ_msg]
      | _ => [syn, HTypCode.view(ty)]
      }
    /* Use the got type if not just Hole */
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      switch (got_ty) {
      | HTyp.Hole => [ana, HTypCode.view(expected_ty)]
      | _ => [ana, HTypCode.view(got_ty)]
      }
    | AnaTypeInconsistent(expected_ty, got_ty)
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let (expected_diff, got_diff) = TypDiff.mk(expected_ty, got_ty);
      [
        ana,
        HTypCode.view(~diff_steps=expected_diff, expected_ty),
        inconsistent_symbol,
        HTypCode.view(~diff_steps=got_diff, got_ty),
      ];
    | SynErrorArrow(_expected_ty, got_ty) => [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        HTypCode.view(got_ty),
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
        HTypCode.view(expected_ty),
        inconsistent_symbol,
        emphasize_text("Invalid Text"),
      ]
    | SynInvalid => [emphasize_text("Invalid Text")]
    | SynInvalidArrow(_) => [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Invalid Text"),
      ]
    | AnaFree(expected_ty) => [
        ana,
        HTypCode.view(expected_ty),
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
        HTypCode.view(expected_ty),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ];
      keyword_msg(term, keyword, main_msg);
    | PatAnaKeyword(expected_ty, _) => [
        ana,
        HTypCode.view(expected_ty),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeyword(keyword) =>
      let main_msg = [syn, emphasize_text("Reserved Keyword")];
      keyword_msg(term, keyword, main_msg);
    | PatSynKeyword(_) => [syn, emphasize_text("Reserved Keyword")]
    | SynKeywordArrow(_, keyword) =>
      let main_msg = [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Reserved Keyword"),
      ];
      keyword_msg(term, keyword, main_msg);
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        if (HTyp.consistent(ty, got_ty)) {
          [syn, HTypCode.view(ty)];
        } else {
          let (ty_diff, got_diff) = TypDiff.mk(ty, got_ty);
          [
            syn,
            HTypCode.view(~diff_steps=ty_diff, ty),
            inconsistent_symbol,
            HTypCode.view(~diff_steps=got_diff, got_ty),
          ];
        }
      /*| (InconsistentBranchTys(_), _) => [
          emphasize_text("Inconsistent Branch Types"),
        ]*/
      | _ => message(typed)
      }
    | SynInconsistentBranches(_) => [
        syn,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | SynInconsistentBranchesArrow(_) => [
        syn,
        emphasize_text("Function Type"),
        inconsistent_symbol,
        emphasize_text("Inconsistent Branch Types"),
      ]
    | OnType => []
    | OnLine => /* TODO */ [emphasize_text("Line")]
    | OnRule => /* TODO */ [emphasize_text("Rule")]
    };
  };
  List.cons(term_tag, message(typed));
};

let novice_summary =
    (
      typed: CursorInfo.typed,
      term: CursorInfo.cursor_term,
      tag_typ: TermSort.t,
    ) => {
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
    | PatSynthesized(ty)
    | OnLetLine(ty) =>
      switch (ty) {
      | HTyp.Hole => [
          Node.text("Expecting " ++ article),
          term_tag,
          Node.text("of"),
          any_typ_msg,
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
    | AnaKeyword(expected_ty, keyword) =>
      let main_msg =
        expecting_of_type
        @ [
          HTypCode.view(expected_ty),
          Node.text("but got a"),
          emphasize_text("Reserved Keyword"),
        ];
      keyword_msg(term, keyword, main_msg);
    | PatAnaKeyword(expected_ty, _) =>
      expecting_of_type
      @ [
        HTypCode.view(expected_ty),
        Node.text("but got a"),
        emphasize_text("Reserved Keyword"),
      ]
    | SynKeyword(keyword) =>
      let main_msg = [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text("Reserved Keyword"),
      ];
      keyword_msg(term, keyword, main_msg);
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
      keyword_msg(term, keyword, main_msg);
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
      /*| (InconsistentBranchTys(_), _) => [
          Node.text("Got " ++ article),
          term_tag,
          emphasize_text("Inconsistent Branch Types"),
        ]*/
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
    | OnLine => /* TODO */ [
        Node.text("Got " ++ article),
        term_tag,
        emphasize_text(~only_right=true, "Line"),
      ]
    | OnRule => /* TODO */ [
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
      assistant_enabled: bool,
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
        Attr.classes(["clickable-help"]),
        Attr.create("title", "Click to toggle expanded cursor inspector"),
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
        Attr.classes(
          novice_mode
            ? ["summary-message", "novice-mode"] : ["summary-message"],
        ),
      ],
      novice_mode
        ? novice_summary(ci.typed, ci.cursor_term, tag_type)
        : advanced_summary(ci.typed, ci.cursor_term, tag_type),
    );
  let fill_icon =
    Node.div(
      [
        Attr.classes(["clickable-help"]),
        Attr.create("title", "Click to toggle strategy guide"),
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
  let assistant_icon =
    Node.div(
      [
        Attr.classes(["clickable"]),
        Attr.on_click(_ =>
          Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            inject(
              ModelAction.UpdateSettings(CursorInspector(Toggle_assistant)),
            ),
          ])
        ),
      ],
      [Node.text(Unicode.robot_arm)],
    );
  let fill_space = Node.span([Attr.classes(["filler"])], []);
  let body =
    (show ? [summary, fill_space, arrow] : [summary])
    @ (show_strategy_guide ? [fill_space, fill_icon] : [])
    @ (assistant_enabled ? [assistant_icon] : []);
  Node.div(
    [
      Attr.create("title", "Click to toggle form of message"),
      Attr.classes(["type-info-summary", "clickable-help"]),
      Attr.on_click(_ =>
        Event.Many([
          Event.Prevent_default,
          Event.Stop_propagation,
          inject(
            ModelAction.UpdateSettings(CursorInspector(Toggle_novice_mode)),
          ),
        ])
      ),
    ],
    body,
  );
};

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~font_metrics: FontMetrics.t,
      loc: (float, float),
      cursor_inspector: Settings.CursorInspector.t,
      cursor_info: CursorInfo.t,
      u_gen: MetaVarGen.t,
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

  let expanded =
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
    | Analyzed(_) => OK
    | AnaAnnotatedLambda(_) => OK
    | AnaTypeInconsistent(_) => TypeInconsistency
    | AnaWrongLength(_) => TypeInconsistency
    | AnaInvalid(_) => BindingError
    | AnaFree(_) => BindingError
    | AnaSubsumed(_) => OK
    | AnaKeyword(_) => BindingError
    | Synthesized(_) => OK
    | SynInvalid => BindingError
    | SynFree => BindingError
    | SynKeyword(_) => BindingError
    | SynErrorArrow(_) => TypeInconsistency
    | SynMatchingArrow(_) => OK
    | SynKeywordArrow(_) => BindingError
    | SynInvalidArrow(_) => BindingError
    | SynFreeArrow(_) => BindingError
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
    | SynInconsistentBranches(_) => TypeInconsistency
    | SynInconsistentBranchesArrow(_) => TypeInconsistency
    | OnType => OK
    | PatAnalyzed(_) => OK
    | PatAnaTypeInconsistent(_) => TypeInconsistency
    | PatAnaWrongLength(_) => TypeInconsistency
    | PatAnaInvalid(_) => BindingError
    | PatAnaSubsumed(_) => OK
    | PatAnaKeyword(_) => BindingError
    | PatSynthesized(_) => OK
    | PatSynKeyword(_) => BindingError
    | OnLine => OK
    | OnLetLine(_) => OK
    | OnRule => OK
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
    | Exp(OnDelim(0, _), Case(_)) => "above"
    | _ => "below"
    };
  let on_empty_hole =
    AssistantCommon.on_empty_expr_hole(cursor_info.cursor_term);
  let assistant_enabled =
    AssistantCommon.valid_assistant_term(cursor_info.cursor_term);
  let assistant_on = assistant_enabled && cursor_inspector.assistant;
  let show =
    switch (expanded) {
    | Some(_) => true
    | None => false
    };
  let summary =
    summary_bar(
      ~inject,
      cursor_info,
      show,
      cursor_inspector.show_expanded,
      cursor_inspector.novice_mode,
      on_empty_hole,
      assistant_enabled,
    );
  let content =
    switch (cursor_inspector.show_expanded, expanded) {
    | (true, Some(ind)) => [summary, ...ind]
    | _ => [summary]
    };

  let content =
    if (assistant_on) {
      List.append(
        content,
        [
          AssistantView.view(
            ~inject,
            ~font_metrics,
            cursor_inspector,
            cursor_info,
            u_gen,
          ),
        ],
      );
    } else if (cursor_inspector.type_assist && on_empty_hole) {
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
      Attr.classes(
        ["cursor-inspector-outer", above_or_below]
        @ (assistant_on ? ["assistant-active"] : []),
      ),
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
