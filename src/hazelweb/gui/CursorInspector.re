module Vdom = Virtual_dom.Vdom;

type err_state_b =
  | TypeInconsistency
  | BindingError
  | OK;

type warn_state_b =
  | BindingWarn
  | NoWarn;

let view =
    (~inject: Update.Action.t => Vdom.Event.t, model: Model.t): Vdom.Node.t => {
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

  let uses = num_of_uses => num_of_uses == 1 ? " use" : " uses";
  let got_var_pat_indicator = (shadow, num_of_non_rec_uses, num_of_rec_uses) => {
    got_indicator(
      "Got a variable pattern",
      special_msg_bar(
        (
          shadow
            ? UnicodeConstants.info ++ " shadowing previous variable, " : ""
        )
        ++ (num_of_non_rec_uses == 0 ? UnicodeConstants.warning ++ " " : "")
        ++ (
          num_of_rec_uses == 0
            ? Int.to_string(num_of_non_rec_uses) ++ uses(num_of_non_rec_uses)
            : Int.to_string(num_of_non_rec_uses)
              ++ " non-recursive"
              ++ uses(num_of_non_rec_uses)
              ++ " and "
              ++ Int.to_string(num_of_rec_uses)
              ++ " recursive"
              ++ uses(num_of_rec_uses)
        ),
      ),
    );
  };
  let got_var_indicator = (index_of_cur_use, num_of_other_uses) =>
    got_indicator(
      "Got a variable",
      special_msg_bar(
        Int.to_string(index_of_cur_use)
        ++ " / "
        ++ Int.to_string(num_of_other_uses + 1)
        ++ uses(num_of_other_uses + 1),
      ),
    );

  let ci = model |> Model.get_program |> Program.get_cursor_info;
  let rec get_indicator_info = (typed: CursorInfo_common.typed) =>
    switch (typed) {
    | Analyzed(ty, None) =>
      let ind1 = expected_ty_indicator(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      (ind1, ind2, OK, NoWarn);
    | Analyzed(ty, Some((_, _, use_index, other_uses))) =>
      let ind1 = expected_ty_indicator(ty);
      let ind2 = got_var_indicator(use_index, List.length(other_uses));
      (ind1, ind2, OK, NoWarn);
    | AnaAnnotatedLambda(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK, NoWarn);
    | AnaTypeInconsistent(expected_ty, got_ty, _) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_inconsistent_indicator(got_ty);
      (ind1, ind2, TypeInconsistency, NoWarn);
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
      (ind1, ind2, TypeInconsistency, NoWarn);
    | AnaFree(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | AnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK, NoWarn);
    | AnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | Synthesized(ty, None) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK, NoWarn);
    | Synthesized(_ty, Some((_, _, use_index, other_uses))) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_var_indicator(use_index, List.length(other_uses));
      (ind1, ind2, OK, NoWarn);
    | SynFree =>
      let ind1 = expected_any_indicator;
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | SynKeyword(_keyword) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | SynErrorArrow(expected_ty, got_ty, _varexp) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 = got_inconsistent_matched_indicator(got_ty, expected_ty);
      (ind1, ind2, TypeInconsistency, NoWarn);
    | SynMatchingArrow(syn_ty, matched_ty, _varexp) =>
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
      (ind1, ind2, OK, NoWarn);
    | SynKeywordArrow(matched_ty, _k) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a keyword ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError, NoWarn);
    | SynFreeArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a free variable ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError, NoWarn);
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
      (ind1, ind2, err_state_b, NoWarn);
    | SynInconsistentBranches(rule_types, path_to_case) =>
      let ind1 = expected_any_indicator;
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      (ind1, ind2, TypeInconsistency, NoWarn);
    | OnType =>
      let ind1 = expected_a_type_indicator;
      let ind2 = got_a_type_indicator;
      (ind1, ind2, OK, NoWarn);
    | PatAnalyzed(ty) =>
      let ind1 = expected_ty_indicator_pat(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      (ind1, ind2, OK, NoWarn);
    | PatAnaVar(ty, _, shadow, var_warn, uses, rec_uses) =>
      let ind1 = expected_ty_indicator_pat(ty);
      let ind2 =
        got_var_pat_indicator(
          shadow,
          List.length(uses) - List.length(rec_uses),
          List.length(rec_uses),
        );
      (
        ind1,
        ind2,
        OK,
        switch (var_warn) {
        | NoWarning => NoWarn
        | _ => BindingWarn
        },
      );
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_inconsistent_indicator(got_ty);
      (ind1, ind2, TypeInconsistency, NoWarn);
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
      (ind1, ind2, TypeInconsistency, NoWarn);
    | PatAnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK, NoWarn);
    | PatAnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | PatAnaDuplicate(expected_ty, _var) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_duplicate_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | PatSynthesized(ty) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK, NoWarn);
    | PatSynVar(_, _, shadow, var_warn, uses, rec_uses) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 =
        got_var_pat_indicator(
          shadow,
          List.length(uses) - List.length(rec_uses),
          List.length(rec_uses),
        );
      (
        ind1,
        ind2,
        OK,
        switch (var_warn) {
        | NoWarning => NoWarn
        | _ => BindingWarn
        },
      );
    | PatSynKeyword(_keyword) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | PatSynDuplicate(_var) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_duplicate_indicator;
      (ind1, ind2, BindingError, NoWarn);
    | OnLine =>
      /* TODO */
      let ind1 = expected_a_line_indicator;
      let ind2 = got_a_line_indicator;
      (ind1, ind2, OK, NoWarn);
    | OnRule =>
      /* TODO */
      let ind1 = expected_a_rule_indicator;
      let ind2 = got_a_rule_indicator;
      (ind1, ind2, OK, NoWarn);
    };

  let (ind1, ind2, err_state_b, warn_state_b) = get_indicator_info(ci.typed);

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
          [ind1, ind2],
        ),
      ],
    )
  );
};
