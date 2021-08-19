open Virtual_dom.Vdom;

let string_of_cursor_inspector_mode =
    (mode: option(Model.cursor_inspector_mode)) =>
  switch (mode) {
  | Some(Simple) => "inspector only"
  | Some(Assistant) => "assistant"
  | Some(Tutor) => "tutor"
  | None => "close"
  };

let radio = (str, checked) =>
  Node.input(
    [
      Attr.id(str ++ "-radio"),
      Attr.type_("radio"),
      Attr.name("mode"),
      Attr.value(str),
    ]
    @ (checked ? [Attr.checked] : []),
    [],
  );

let ci_mode_radio =
    (
      mode,
      current_mode,
      ~body: list(Node.t)=[],
      ~inject: ModelAction.t => Event.t,
    ) => {
  let mode_str = string_of_cursor_inspector_mode(mode);
  Node.div(
    [
      Attr.classes(["mode"]),
      Attr.on_click(_ => inject(SetCursorInspectorMode(mode))),
    ],
    [
      radio(mode_str, mode == current_mode),
      Node.label(
        [Attr.for_(mode_str ++ "-radio")],
        [Node.text(mode_str)],
      ),
    ]
    @ body,
  );
};

let ci_control_pane =
    (curent_mode: option(Model.cursor_inspector_mode), ~inject) => {
  let mode_radio = (~body=[], mode) =>
    ci_mode_radio(mode, curent_mode, ~body, ~inject);
  Node.div(
    [Attr.classes(["ci-control-pane-wrapper"])],
    [
      Node.div(
        [Attr.classes(["ci-control-pane"])],
        [
          Node.div(
            [Attr.id("ci-control-pane-mode-switch")],
            [
              Node.text("cycle inspector mode"),
              Node.div(
                [Attr.classes(["key"])],
                [Node.text("CTRL-SPACE")],
              ),
            ],
          ),
          mode_radio(Some(Assistant)),
          mode_radio(Some(Tutor)),
          mode_radio(Some(Simple)),
          mode_radio(
            None,
            ~body=[
              Node.div(
                [Attr.id("ci-control-pane-close"), Attr.classes(["key"])],
                [Node.text("ESC")],
              ),
            ],
          ),
        ],
      ),
    ],
  );
};

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
  Node.div(
    [Attr.classes(["compressed"])],
    [
      emphasize_text("Any Type ("),
      HTypCode.view(HTyp.Hole),
      emphasize_text(")"),
    ],
  );

let keyword_msg = (term, keyword, main_msg) =>
  if (CursorInfo_common.is_end_keyword(term, keyword)) {
    main_msg
    @ [
      Node.text("("),
      AssistantView_common.shortcut_node("Space"),
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
    | PatSynthesized(ty) =>
      switch (term) {
      | ExpOperand(_, EmptyHole(_)) => [syn, any_typ_msg]
      | _ => [syn, HTypCode.view(ty)]
      }
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      if (HTyp.eq(expected_ty, got_ty) || HTyp.eq(got_ty, HTyp.Hole)) {
        [ana, HTypCode.view(expected_ty)];
      } else {
        [
          ana,
          HTypCode.view(expected_ty),
          consistent_symbol,
          HTypCode.view(got_ty),
        ];
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
    | OnNonLetLine => /* TODO */ [emphasize_text("Line")]
    | OnRule => /* TODO */ [emphasize_text("Rule")]
    };
  };
  switch (typed) {
  | OnNonLetLine => message(typed) /* Don't display the term tag for empty and comment lines */
  | _ => List.cons(term_tag, message(typed))
  };
};

//TODO(andrew): this is mostly copy-pasted from advanced summary...
let assistant_summary =
    (
      typed: CursorInfo.typed,
      term: CursorInfo.cursor_term,
      tag_typ: TermSort.t,
      type_editor_is_focused: bool,
      assistant_model: AssistantModel.t,
      u_gen,
      ~inject,
      ~font_metrics,
      ~is_mac,
      ~settings,
    ) => {
  let term_tag =
    switch (term) {
    | ExpOperand(_, operand) =>
      let editor = operand |> UHExp.Block.wrap |> Editor.mk_exp_editor;
      UHCode.codebox_view(
        ~font_metrics,
        ~is_focused=false,
        ~settings,
        editor,
      );
    | _ => [TermTag.term_tag_view(tag_typ, ~show_tooltip=true, [])]
    };

  //TermTag.term_tag_view(tag_typ, ~show_tooltip=true, []);
  let rec message = (typed: CursorInfo.typed) => {
    switch (typed) {
    | Analyzed(ty)
    | PatAnalyzed(ty) => [ana, HTypCode.view(ty)]
    | SynMatchingArrow(_, ty) => [syn, HTypCode.view(ty)]
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      //| OnLetLine(ty) =>
      let any_typ_msg =
        Node.div(
          [Attr.classes(["compressed"])],
          [emphasize_text("Any Type (")]
          @ UHCode.typebox(
              ~inject,
              ~font_metrics,
              ~is_mac,
              ~settings,
              ~is_focused=type_editor_is_focused,
              assistant_model.filter_editor,
              u_gen,
            )
          //HTypCode.view(HTyp.Hole),
          @ [emphasize_text(")")],
        );
      switch (ty) {
      | HTyp.Hole => [syn, any_typ_msg]
      | _ => [syn, HTypCode.view(ty)]
      };
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
    | OnNonLetLine => /* TODO */ [emphasize_text("Line")]
    | OnRule => /* TODO */ [emphasize_text("Rule")]
    };
  };
  term_tag @ message(typed);
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
    | AnaAnnotatedLambda(expected_ty, got_ty)
    | AnaSubsumed(expected_ty, got_ty)
    | PatAnaSubsumed(expected_ty, got_ty) =>
      if (HTyp.eq(expected_ty, got_ty) || HTyp.eq(got_ty, HTyp.Hole)) {
        expecting_of_type @ [HTypCode.view(expected_ty)];
      } else {
        expecting_of_type
        @ [
          HTypCode.view(expected_ty),
          Node.text("and got consistent type"),
          HTypCode.view(got_ty),
        ];
      }
    | Synthesized(ty)
    | PatSynthesized(ty) =>
      switch (term) {
      | ExpOperand(_, EmptyHole(_)) => [
          Node.text("Got " ++ article),
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
    | OnNonLetLine => /* TODO */ [
        Node.text("Got a "),
        /* Don't show the term tag for empty and comment lines */
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
      show_expansion_arrow: bool,
      show_expanded: bool,
      novice_mode: bool,
      show_strategy_guide_icon: bool,
      _on_empty_hole: bool,
      assistant_enabled: bool,
      type_editor_is_focused,
      assistant_model,
      cursor_inspector_mode,
      u_gen,
      ~font_metrics,
      ~is_mac,
      ~settings,
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
          novice_mode && !assistant_enabled
            //TODO(andrew): dehackify. need this or formatting jumps around
            ? ["summary-message", "novice-mode"] : ["summary-message"],
        ),
      ],
      assistant_enabled
        ? assistant_summary(
            ci.typed,
            ci.cursor_term,
            tag_type,
            type_editor_is_focused,
            assistant_model,
            u_gen,
            ~inject,
            ~font_metrics,
            ~is_mac,
            ~settings,
          )
        : novice_mode
            ? novice_summary(ci.typed, ci.cursor_term, tag_type)
            : advanced_summary(ci.typed, ci.cursor_term, tag_type),
    );
  let images_dir = "imgs/";
  let images_name = "boost-pixel.png";
  let set_img = path =>
    Node.create(
      "img",
      [Attr.create("src", path), Attr.create("style", "height:1.1em")],
      [],
    );
  let fill_icon = symbol =>
    Node.div(
      [
        Attr.classes(["clickable-help-icon"]),
        Attr.create("title", "Click to toggle strategy guide"),
        Attr.on_click(_ =>
          Event.Many([
            Event.Prevent_default,
            Event.Stop_propagation,
            show_strategy_guide_icon
              ? inject(
                  UpdateSettings(CursorInspector(Toggle_strategy_guide)),
                )
              : inject(
                  Chain([
                    UpdateSettings(CursorInspector(Set_guide(false))),
                    UpdateAssistant(Toggle),
                  ]),
                ),
          ])
        ),
        /*
         Attr.on_contextmenu(_ =>
           Event.Many([
             Event.Prevent_default,
             Event.Stop_propagation,
             inject(
               Chain([
                 UpdateSettings(CursorInspector(Set_guide(false))),
                 UpdateAssistant(Toggle),
               ]),
             ),
           ])
         ),*/
      ],
      [
        if (symbol == Unicode.robot_arm) {
          set_img(images_dir ++ images_name);
        } else {
          Node.text(symbol);
        },
      ],
    );
  let fill_space = Node.span([Attr.classes(["filler"])], []);
  let body =
    (show_expansion_arrow ? [summary, fill_space, arrow] : [summary])
    @ (
      show_strategy_guide_icon || assistant_enabled
        ? [
          fill_space,
          fill_icon(
            show_strategy_guide_icon && !assistant_enabled
              ? Unicode.light_bulb : Unicode.robot_arm,
          ),
          ci_control_pane(cursor_inspector_mode, ~inject),
        ]
        : []
    );
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
      {settings, font_metrics, is_mac, focal_editor, assistant, _} as model: Model.t,
      loc: (float, float),
    )
    : Node.t => {
  let type_editor_is_focused = focal_editor == Model.AssistantTypeEditor;
  let cursor_inspector_mode = Model.get_cursor_inspector_mode(model);
  let program = Model.get_program(model);
  let cursor_info = Editor.Exp.get_cursor_info(program);
  let cursor_inspector = settings.cursor_inspector;
  let u_gen = Editor.EditState_Exp.get_ugen(program.edit_state);

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
    | OnNonLetLine => OK
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
    | ExpOperand(OnDelim(0, _), Case(_)) => "above"
    | _ => "below"
    };
  let on_empty_hole =
    CursorInfo_common.is_empty_hole(cursor_info.cursor_term);
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
    | (_, EndBranchClause) =>
      switch (StrategyGuide.rules_view(cursor_info)) {
      | None => (false, None)
      | Some(sg_rules) => (true, Some(sg_rules))
      }
    | (Line(_, EmptyLine), _) => (
        true,
        Some(StrategyGuide.lines_view(true)),
      )
    | (Line(_), _) => (true, Some(StrategyGuide.lines_view(false)))
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
      on_empty_hole,
      assistant.active,
      type_editor_is_focused,
      assistant,
      cursor_inspector_mode,
      u_gen,
      ~font_metrics,
      ~is_mac,
      ~settings,
    );
  let content =
    switch (cursor_inspector.strategy_guide, strategy_guide) {
    | (true, Some(strategy_guide)) =>
      List.append([summary], [strategy_guide])
    | _ => [summary]
    };

  let content =
    if (assistant.active) {
      [summary]
      @ [
        AssistantView.view(
          assistant,
          ~inject,
          ~font_metrics,
          ~settings,
          ~u_gen,
          ~ci=cursor_info,
          //~cursor_inspector_mode,
        ),
      ];
    } else {
      content;
    };

  Node.div(
    [
      Attr.id("cursor-inspector"),
      Attr.classes(
        ["cursor-inspector-outer", above_or_below]
        @ (assistant.active ? ["assistant-active"] : []),
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
