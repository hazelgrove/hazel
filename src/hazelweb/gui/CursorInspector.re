module Vdom = Virtual_dom.Vdom;

type err_state_b =
  | TypeInconsistency
  | BindingError
  | OK;

type warn_state_b =
  | BindingWarn
  | NoWarn;

type var_sort =
  | VarExp(int, int)
  | VarPat(bool, int, int)
  | VarNone;

let view =
    (~inject: ModelAction.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
  let typebar = ty =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "typebar"])],
        [HTypCode.view(ty)],
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

  let uses = num_of_uses => num_of_uses == 1 ? " use" : " uses";
  let var_pat_bar = (shadow, num_of_non_rec_uses, num_of_rec_uses) =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "var_pat_bar"])],
        (
          shadow
            ? [
              Node.span(
                [],
                [
                  Node.text(
                    UnicodeConstants.info ++ "shadowing previous definition, ",
                  ),
                ],
              ),
            ]
            : []
        )
        @ (
          num_of_non_rec_uses == 0
            ? [
              Node.span([], [Node.text(UnicodeConstants.warning ++ " ")]),
            ]
            : []
        )
        @ (
          num_of_rec_uses == 0
            ? [
              Node.span(
                [Attr.classes(["number"])],
                [Node.text(Int.to_string(num_of_non_rec_uses))],
              ),
              Node.span([], [Node.text(uses(num_of_non_rec_uses))]),
            ]
            : [
              Node.span(
                [Attr.classes(["number"])],
                [Node.text(Int.to_string(num_of_non_rec_uses))],
              ),
              Node.span([], [Node.text(" non-recursive")]),
              Node.span([], [Node.text(uses(num_of_non_rec_uses))]),
              Node.span([], [Node.text(" and ")]),
              Node.span(
                [Attr.classes(["number"])],
                [Node.text(Int.to_string(num_of_rec_uses))],
              ),
              Node.span([], [Node.text(" recursive")]),
              Node.span([], [Node.text(uses(num_of_rec_uses))]),
            ]
        ),
      )
    );

  let go_to_definition_button = disabled => {
    Vdom.(
      Node.div(
        disabled
          ? [
            Attr.classes(["goto-button"]),
            Attr.disabled,
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToDefinition)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Definition (Alt+[)"),
          ]
          : [
            Attr.classes(["goto-button"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToDefinition)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Definition (Alt+[)"),
          ],
        [Icons.up_arrow(["goto-icon"])],
      )
    );
  };

  let go_to_usage_button = disabled => {
    Vdom.(
      Node.div(
        disabled
          ? [
            Attr.classes(["goto-button"]),
            Attr.disabled,
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToFirstUsage)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Usage (Alt+])"),
          ]
          : [
            Attr.classes(["goto-button"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToFirstUsage)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Usage (Alt+])"),
          ],
        [Icons.down_arrow(["goto-icon"])],
      )
    );
  };

  let go_to_next_button = disabled => {
    Vdom.(
      Node.div(
        disabled
          ? [
            Attr.classes(["goto-button"]),
            Attr.disabled,
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToNextUsage)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Next Usage (Alt+N)"),
          ]
          : [
            Attr.classes(["goto-button"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToNextUsage)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Next Usage (Alt+N)"),
          ],
        [Icons.right_arrow(["goto-icon"])],
      )
    );
  };

  let go_to_prev_button = disabled => {
    Vdom.(
      Node.div(
        disabled
          ? [
            Attr.classes(["goto-button"]),
            Attr.disabled,
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToPrevUsage)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Previous Usage (Alt+P)"),
          ]
          : [
            Attr.classes(["goto-button"]),
            Attr.on_click(_ =>
              Vdom.Event.Many([
                inject(ModelAction.EditAction(GoToPrevUsage)),
                inject(FocusCell),
              ])
            ),
            Attr.create("title", "Go To Previous Usage (Alt+P)"),
          ],
        [Icons.left_arrow(["goto-icon"])],
      )
    );
  };

  let goto_button_bar = var_sort =>
    Vdom.(
      Node.div(
        [Attr.classes(["goto-button-bar"])],
        switch (var_sort) {
        | VarExp(_, num_of_other_uses) => [
            go_to_next_button(num_of_other_uses == 0),
            go_to_prev_button(num_of_other_uses == 0),
            go_to_usage_button(true),
            go_to_definition_button(false),
          ]
        | VarPat(_, num_of_non_rec_uses, num_of_rec_uses) => [
            go_to_next_button(true),
            go_to_prev_button(true),
            go_to_usage_button(num_of_non_rec_uses + num_of_rec_uses == 0),
            go_to_definition_button(true),
          ]
        | VarNone => [
            go_to_next_button(true),
            go_to_prev_button(true),
            go_to_usage_button(true),
            go_to_definition_button(true),
          ]
        },
      )
    );

  let var_exp_bar = (ordinal, total) =>
    Vdom.(
      Node.div(
        [Attr.classes(["infobar", "var_exp_bar"])],
        [
          Node.span(
            [Attr.classes(["number"])],
            [Node.text(Int.to_string(ordinal))],
          ),
          Node.span([], [Node.text(" of ")]),
          Node.span(
            [Attr.classes(["number"])],
            [Node.text(Int.to_string(total))],
          ),
          Node.span([], [Node.text(uses(total))]),
        ],
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
  let expected_ty_indicator_pat = ty =>
    expected_indicator(expected_ty_title_pat, typebar(ty));
  let expected_ty_indicator_consistent = ty =>
    expected_indicator(expected_ty_title_consistent, typebar(ty));
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
  let got_inconsistent_indicator = got_ty =>
    got_indicator("Got inconsistent type", typebar(got_ty));
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
  let got_duplicate_indicator =
    got_indicator("Got a duplicated variable", typebar(HTyp.Hole));

  let var_indicator = var_sort =>
    Vdom.(
      Node.div(
        [Attr.classes(["indicator", "got-indicator"])],
        switch (var_sort) {
        | VarExp(index_of_cur_use, num_of_other_uses) => [
            Panel.view_of_other_title_bar("Variable Expression"),
            goto_button_bar(var_sort),
            var_exp_bar(index_of_cur_use + 1, num_of_other_uses + 1),
          ]
        | VarPat(shadow, num_of_non_rec_uses, num_of_rec_uses) => [
            Panel.view_of_other_title_bar("Variable Pattern"),
            goto_button_bar(var_sort),
            var_pat_bar(shadow, num_of_non_rec_uses, num_of_rec_uses),
          ]
        | VarNone => [
            Panel.view_of_other_title_bar("Not a Variable"),
            goto_button_bar(var_sort),
            special_msg_bar(""),
          ]
        },
      )
    );

  let ci = model |> Model.get_program |> Program.get_cursor_info;
  let rec get_indicator_info = (typed: CursorInfo_common.typed) =>
    switch (typed) {
    | Analyzed(ty) =>
      let ind1 = expected_ty_indicator(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    | AnaAnnotatedLambda(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    | AnaTypeInconsistent(expected_ty, got_ty, varexp) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_inconsistent_indicator(got_ty);
      let ind3 =
        switch (varexp) {
        | None => var_indicator(VarNone)
        | Some((_, _, use_index, other_uses)) =>
          var_indicator(VarExp(use_index, List.length(other_uses)))
        };
      (ind1, ind2, ind3, TypeInconsistency, NoWarn);
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
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, TypeInconsistency, NoWarn);
    | AnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | AnaFree(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_free_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | AnaSubsumed(expected_ty, got_ty, varexp) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      let ind3 =
        switch (varexp) {
        | None => var_indicator(VarNone)
        | Some((_, _, use_index, other_uses)) =>
          var_indicator(VarExp(use_index, List.length(other_uses)))
        };
      (ind1, ind2, ind3, OK, NoWarn);
    | AnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_keyword_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | Synthesized(ty, varexp) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_ty_indicator(ty);
      let ind3 =
        switch (varexp) {
        | None => var_indicator(VarNone)
        | Some((_, _, use_index, other_uses)) =>
          var_indicator(VarExp(use_index, List.length(other_uses)))
        };
      (ind1, ind2, ind3, OK, NoWarn);
    | SynInvalid =>
      let ind1 = expected_any_indicator;
      let ind2 = got_invalid_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | SynFree =>
      let ind1 = expected_any_indicator;
      let ind2 = got_free_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | SynKeyword(_keyword) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_keyword_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | SynErrorArrow(expected_ty, got_ty, varexp) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 = got_inconsistent_matched_indicator(got_ty, expected_ty);
      let ind3 =
        switch (varexp) {
        | None => var_indicator(VarNone)
        | Some((_, _, use_index, other_uses)) =>
          var_indicator(VarExp(use_index, List.length(other_uses)))
        };
      (ind1, ind2, ind3, TypeInconsistency, NoWarn);
    | SynMatchingArrow(syn_ty, matched_ty, varexp) =>
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
      let ind3 =
        switch (varexp) {
        | None => var_indicator(VarNone)
        | Some((_, _, use_index, other_uses)) =>
          var_indicator(VarExp(use_index, List.length(other_uses)))
        };
      (ind1, ind2, ind3, OK, NoWarn);
    | SynKeywordArrow(matched_ty, _k) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a keyword ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | SynInvalidArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got invalid text ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | SynFreeArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a free variable ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | SynBranchClause(join, typed, branch_index) =>
      let (ind1, ind2, ind3, err_state_b, _) = get_indicator_info(typed);
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
      let (ind2, err_state_b) =
        switch (join, typed) {
        | (JoinTy(ty), Synthesized(got_ty, _)) =>
          switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
          | (true, true) => (got_as_expected_ty_indicator(got_ty), OK)
          | (true, false) => (got_consistent_indicator(got_ty), OK)
          | (false, _) => (
              got_inconsistent_indicator(got_ty),
              TypeInconsistency,
            )
          }
        | (InconsistentBranchTys(_), _) => (ind2, TypeInconsistency)
        | _ => (ind2, err_state_b)
        };
      (ind1, ind2, ind3, err_state_b, NoWarn);
    | SynInconsistentBranches(rule_types, path_to_case) =>
      let ind1 = expected_any_indicator;
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, TypeInconsistency, NoWarn);
    | SynInconsistentBranchesArrow(rule_types, path_to_case) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, TypeInconsistency, NoWarn);
    | OnType =>
      let ind1 = expected_a_type_indicator;
      let ind2 = got_a_type_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    | PatAnalyzed(ty) =>
      let ind1 = expected_ty_indicator_pat(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    | PatAnaVar(ty, _, shadow, var_warn, uses, rec_uses) =>
      let ind1 = expected_ty_indicator_pat(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      let ind3 =
        var_indicator(
          VarPat(
            shadow,
            List.length(uses) - List.length(rec_uses),
            List.length(rec_uses),
          ),
        );
      (
        ind1,
        ind2,
        ind3,
        OK,
        switch (var_warn) {
        | NoWarning => NoWarn
        | _ => BindingWarn
        },
      );
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_inconsistent_indicator(got_ty);
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, TypeInconsistency, NoWarn);
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
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, TypeInconsistency, NoWarn);
    | PatAnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | PatAnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    | PatAnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_keyword_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | PatAnaDuplicate(expected_ty, _var) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_duplicate_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | PatSynthesized(ty) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_ty_indicator(ty);
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    | PatSynVar(ty, _, shadow, var_warn, uses, rec_uses) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_ty_indicator(ty);
      let ind3 =
        var_indicator(
          VarPat(
            shadow,
            List.length(uses) - List.length(rec_uses),
            List.length(rec_uses),
          ),
        );
      (
        ind1,
        ind2,
        ind3,
        OK,
        switch (var_warn) {
        | NoWarning => NoWarn
        | _ => BindingWarn
        },
      );
    | PatSynKeyword(_keyword) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_keyword_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | PatSynDuplicate(_var) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_duplicate_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, BindingError, NoWarn);
    | OnLine =>
      /* TODO */
      let ind1 = expected_a_line_indicator;
      let ind2 = got_a_line_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    | OnRule =>
      /* TODO */
      let ind1 = expected_a_rule_indicator;
      let ind2 = got_a_rule_indicator;
      let ind3 = var_indicator(VarNone);
      (ind1, ind2, ind3, OK, NoWarn);
    };

  let (ind1, ind2, ind3, err_state_b, warn_state_b) =
    get_indicator_info(ci.typed);

  // this determines the color
  let cls_of_err_state_b =
    switch (err_state_b) {
    | TypeInconsistency => "cursor-TypeInconsistency"
    | BindingError => "cursor-BindingError"
    | OK => "cursor-OK"
    };

  let cls_of_warn_state_b =
    switch (warn_state_b) {
    | BindingWarn => "cursor-BindingWarn"
    | NoWarn => "cursor-NoWarn"
    };

  Vdom.(
    Node.div(
      [Attr.classes(["cursor-inspector-outer"])],
      [
        Node.div(
          [
            Attr.classes([
              "panel",
              "cursor-inspector",
              cls_of_err_state_b,
              cls_of_warn_state_b,
            ]),
          ],
          [ind1, ind2, ind3],
        ),
      ],
    )
  );
};
