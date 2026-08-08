open Language;
open Util;
open WebUtil;
open Calc.Syntax;
open Haz3lcore;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type written_step_check_mode =
    | SingleEvalStep
    | CheckResult
    | ProofSearch;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type proof_search_verdict =
    | Ready
    | Planning
    | Checking
    | Cancelled
    | TimedOut
    | ProfileValid
    | EquivalentOutsideProfile
    | Invalid;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type open_box =
    | AxiomsOpen({
        axioms_model: AxiomsBox.Model.t,
        rewrite_selected_exp: option(Exp.t),
        rewrite_reparenthesized_exp: option(Exp.t),
        source_full_visible_exp: option(Exp.t),
      })
    | RewritesOpen({
        editor: CodeEditable.Model.t,
        rewrite_selected_exp: option(Exp.t),
        rewrite_reparenthesized_exp: option(Exp.t),
        source_full_visible_exp: option(Exp.t),
        cached_exp: Calc.saved(Exp.t),
        cached_result: Calc.saved(bool),
      })
    | WrittenStepOpen({
        editor: CodeEditable.Model.t,
        check_mode: written_step_check_mode,
        axioms_model: AxiomsBox.Model.t,
        rewrite_selected_exp: option(Exp.t),
        rewrite_reparenthesized_exp: option(Exp.t),
        source_full_visible_exp: option(Exp.t),
        proof_search_requested: bool,
        proof_search_verdict,
        proof_search_check_id: option(int),
        proof_search_message: option(string),
        proof_search_max_depth: int,
        proof_search_max_states: int,
        proof_search_source: option(string),
        calculated_rewrite_level: option(Axioms.rewrite_level),
        calculated_automation_stage: option(Axioms.automation_stage),
        cached_exp: Calc.saved(Exp.t),
        cached_result: Calc.saved(option(ProfileProofPlan.authorized_plan)),
      })
    | NoneOpen;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type assumptions = list(AssumptionBox.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    next_steps: Calc.saved(EvaluatorStep.status),
    refls: Calc.saved(list(Exp.t)),
    selected_id: Calc.saved(option(Id.t)),
    selected_exp: Calc.saved(option(Exp.t)),
    full_exp: Calc.saved(Exp.t),
    full_visible_exp: Calc.saved(Exp.t),
    assumptions: Calc.saved(option(assumptions)),
    open_box,
    cached_env: Calc.saved(Environment.t(Exp.t)) // TODO[Matt]: remove this later, just to get env into view for now.
  };

  let init = {
    next_steps: Calc.Pending,
    refls: Calc.Pending,
    selected_id: Calc.Pending,
    selected_exp: Calc.Pending,
    full_exp: Calc.Pending,
    full_visible_exp: Calc.Pending,
    assumptions: Calc.Pending,
    open_box: NoneOpen,
    cached_env: Calc.Pending,
  };
  let get_selected_exp = (m: t): Exp.t =>
    m.selected_exp
    |> Calc.saved_to_option
    |> Option.join
    |> OptUtil.get(() => EmptyHole |> Exp.fresh);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = unit;

  let persist = (_: t): persistent => ();

  let unpersist = (_: persistent): t => init;
};

let proof_search_verdict_label = (~has_candidate, verdict) =>
  switch (verdict) {
  | Model.Ready => has_candidate ? "Candidate ready" : "Ready"
  | Planning => "Planning with active profile..."
  | Checking => "Rocq checking..."
  | Cancelled => "Cancelled"
  | TimedOut => "Timed out"
  | ProfileValid => "Valid"
  | EquivalentOutsideProfile => "Equivalent, but blocked by profile"
  | Invalid => "Invalid"
  };

let effective_selection_for_editor = (editor: CodeSelectable.Model.t) =>
  SelectionEffective.effective_selection(
    ~info_map=CodeEditable.Model.get_statics(editor).info_map,
    ~measured=editor.editor.syntax.measured,
    ~term_data=editor.editor.syntax.term_data,
    editor.editor.state.zipper,
  );

let proof_search_can_replace =
  fun
  | Model.ProfileValid => true
  | Ready
  | Planning
  | Checking
  | Cancelled
  | TimedOut
  | EquivalentOutsideProfile
  | Invalid => false;

let proof_search_state_is_stale =
    (
      ~calculated_rewrite_level,
      ~rewrite_level,
      ~calculated_automation_stage,
      ~automation_stage,
      ~target_exp_changed,
      ~proof_search_source,
    ) =>
  calculated_rewrite_level != Some(rewrite_level)
  || calculated_automation_stage != Some(automation_stage)
  || target_exp_changed
  && proof_search_source
  |> Option.is_none;

let check_mode_for_automation_stage =
  fun
  | Axioms.Manual => Model.SingleEvalStep
  | MultiStepCheck
  | AutoEval => Model.ProofSearch;

let string_contains = (needle, haystack) => {
  let needle_length = String.length(needle);
  let haystack_length = String.length(haystack);
  let rec loop = offset =>
    offset
    + needle_length <= haystack_length
    && (
      String.sub(haystack, offset, needle_length) == needle
      || loop(offset + 1)
    );
  needle_length == 0 || loop(0);
};

let proof_search_failure_message = (~has_profile_trace, raw_message) => {
  let infrastructure_failure =
    [
      "Stack overflow",
      "timed out",
      "failed to start",
      "worker failed",
      "Persistent Rocq worker failed",
    ]
    |> List.exists(fragment => string_contains(fragment, raw_message));
  if (infrastructure_failure) {
    "Rocq checker failed unexpectedly. See the browser console for details.";
  } else if (has_profile_trace) {
    "Rocq could not verify the enabled Profile proof certificate. See the browser console for details.";
  } else {
    "No proof is available using the active Profile.";
  };
};

let proof_search_route_label = (summary: ProofTrace.trace_summary) => {
  let rule_names =
    summary.rule_ids
    |> List.filter_map(rule_id =>
         switch (Axioms.catalog_rule_by_id(rule_id)) {
         | Some(rule) => Some(rule.metadata.name)
         | None =>
           Axioms.cleanup_capability_for_id(rule_id)
           |> Option.map(capability =>
                Axioms.cleanup_capability_metadata(capability).name
              )
         }
       )
    |> RewriteChecker.dedup;
  rule_names == []
    ? ProofTrace.trace_summary_label(summary)
    : String.concat(" → ", rule_names);
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | ToggleAxioms(option(Exp.t), option(Exp.t))
    | ProposeRewrite(option(Exp.t), option(Exp.t))
    | ProposeWrittenStep(
        Model.written_step_check_mode,
        option(Exp.t),
        option(Exp.t),
      )
    | RunProofSearch(int, int, int, option(Exp.t), option(Exp.t))
    | RocqProofSearchStarted(int)
    | RocqProofSearchFinished(
        int,
        Model.proof_search_verdict,
        string,
        option(ProfileProofPlan.authorized_plan),
      )
    | RocqProofSearchCancelled(int)
    | AlgebriteSuggestionFinished(option(string), string, string)
    | AutoSimplifySuggestionFinished(Exp.t, option(string), string)
    | AutoProfileSimplifySuggestionFinished(Exp.t, option(Exp.t), string)
    | ProfileSuggestionFinished(option(Exp.t), string, string)
    | RewriteEditorAction(CodeEditable.Update.t)
    | WriteStepEditorAction(CodeEditable.Update.t)
    | AxiomBoxAction(AxiomsBox.Update.t);

  let rec update = (~settings, action, model: Model.t): Updated.t(Model.t) => {
    switch (action, model.open_box) {
    | (ToggleAxioms(rewrite_selected_exp, rewrite_reparenthesized_exp), _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | RewritesOpen(_)
        | WrittenStepOpen(_) =>
          Model.AxiomsOpen({
            axioms_model: AxiomsBox.Model.init,
            rewrite_selected_exp,
            rewrite_reparenthesized_exp,
            source_full_visible_exp: None,
          })
        | AxiomsOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~logged=true);
    | (ProposeRewrite(rewrite_selected_exp, rewrite_reparenthesized_exp), _) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | AxiomsOpen(_)
        | WrittenStepOpen(_) =>
          Model.RewritesOpen({
            editor:
              CodeEditable.Model.mk(
                Editor.Model.mk(Zipper.init(), ~root=Exp),
              ),
            rewrite_selected_exp,
            rewrite_reparenthesized_exp,
            source_full_visible_exp: None,
            cached_exp: Calc.Pending,
            cached_result: Calc.Pending,
          })
        | RewritesOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~recalculate=true, ~logged=true);
    | (
        ProposeWrittenStep(
          check_mode,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
        ),
        _,
      ) =>
      let open_box =
        switch (model.open_box) {
        | NoneOpen
        | AxiomsOpen(_)
        | RewritesOpen(_) =>
          Model.WrittenStepOpen({
            editor:
              CodeEditable.Model.mk(
                Editor.Model.mk(~root=Exp, Zipper.init()),
              ),
            check_mode,
            axioms_model: AxiomsBox.Model.init,
            rewrite_selected_exp,
            rewrite_reparenthesized_exp,
            source_full_visible_exp: None,
            proof_search_requested: false,
            proof_search_verdict: Ready,
            proof_search_check_id: None,
            proof_search_message: None,
            proof_search_max_depth: 4,
            proof_search_max_states: 80,
            proof_search_source: None,
            calculated_rewrite_level: None,
            calculated_automation_stage: None,
            cached_exp: Calc.Pending,
            cached_result: Calc.Pending,
          })
        | WrittenStepOpen(_) => Model.NoneOpen
        };
      Model.{
        ...model,
        open_box,
      }
      |> Updated.return_quiet(~recalculate=true, ~logged=true);
    | (RewriteEditorAction(action), RewritesOpen({editor, _} as r)) =>
      let* new_editor = CodeEditable.Update.update(~settings, action, editor);
      Model.{
        ...model,
        open_box:
          Model.RewritesOpen({
            ...r,
            editor: new_editor,
          }),
      };
    | (RewriteEditorAction(_), _) => model |> Updated.return_quiet
    | (
        RunProofSearch(
          check_id,
          max_depth,
          max_states,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
        ),
        WrittenStepOpen({check_mode: ProofSearch, _} as r),
      ) =>
      Model.{
        ...model,
        open_box:
          Model.WrittenStepOpen({
            ...r,
            rewrite_selected_exp,
            rewrite_reparenthesized_exp,
            proof_search_requested: true,
            proof_search_verdict: Planning,
            proof_search_check_id: Some(check_id),
            proof_search_message: Some("Planning with the active Profile..."),
            proof_search_max_depth: max_depth,
            proof_search_max_states: max_states,
            cached_result: Calc.Pending,
          }),
      }
      |> Updated.return_quiet(~recalculate=true, ~logged=true)
    | (RunProofSearch(_, _, _, _, _), _) => model |> Updated.return_quiet
    | (
        RocqProofSearchStarted(check_id),
        WrittenStepOpen(
          {check_mode: ProofSearch, proof_search_check_id, _} as r,
        ),
      ) =>
      proof_search_check_id == Some(check_id)
        ? Model.{
            ...model,
            open_box:
              Model.WrittenStepOpen({
                ...r,
                proof_search_verdict: Checking,
                proof_search_message: Some("Rocq checking..."),
              }),
          }
          |> Updated.return_quiet(~logged=true)
        : model |> Updated.return_quiet
    | (RocqProofSearchStarted(_), _) => model |> Updated.return_quiet
    | (
        RocqProofSearchFinished(check_id, verdict, message, trace_summary),
        WrittenStepOpen(
          {check_mode: ProofSearch, proof_search_check_id, _} as r,
        ),
      ) =>
      let matches =
        switch (proof_search_check_id) {
        | Some(active_id) => active_id == check_id
        | None => false
        };
      matches
        ? Model.{
            ...model,
            open_box:
              Model.WrittenStepOpen({
                ...r,
                proof_search_requested: false,
                proof_search_verdict: verdict,
                proof_search_check_id: None,
                proof_search_message: Some(message),
                cached_result:
                  Calc.Calculated(
                    verdict == ProfileValid ? trace_summary : None,
                  ),
              }),
          }
          |> Updated.return_quiet(~logged=true)
        : model |> Updated.return_quiet;
    | (RocqProofSearchFinished(_, _, _, _), _) =>
      model |> Updated.return_quiet
    | (
        RocqProofSearchCancelled(check_id),
        WrittenStepOpen(
          {check_mode: ProofSearch, proof_search_check_id, _} as r,
        ),
      ) =>
      proof_search_check_id == Some(check_id)
        ? Model.{
            ...model,
            open_box:
              Model.WrittenStepOpen({
                ...r,
                proof_search_requested: false,
                proof_search_verdict: Cancelled,
                proof_search_check_id: None,
                proof_search_message: Some("Proof search was cancelled."),
                cached_result: Calc.Pending,
              }),
          }
          |> Updated.return_quiet
        : model |> Updated.return_quiet
    | (RocqProofSearchCancelled(_), _) => model |> Updated.return_quiet
    | (
        AlgebriteSuggestionFinished(candidate, message, source),
        WrittenStepOpen({check_mode: ProofSearch, _} as r),
      ) =>
      switch (candidate) {
      | Some(candidate_text) =>
        switch (
          AlgebriteSuggestion.editor_of_hazel_text(
            ~settings=settings.core,
            candidate_text,
          )
        ) {
        | Some(editor) =>
          Model.{
            ...model,
            open_box:
              Model.WrittenStepOpen({
                ...r,
                editor,
                proof_search_requested: false,
                proof_search_verdict: Ready,
                proof_search_check_id: None,
                proof_search_message: Some(message),
                proof_search_source: Some(source),
                cached_exp: Calc.Pending,
                cached_result: Calc.Pending,
              }),
          }
          |> Updated.return_quiet(~recalculate=true, ~logged=true)
        | None =>
          Model.{
            ...model,
            open_box:
              Model.WrittenStepOpen({
                ...r,
                proof_search_requested: false,
                proof_search_verdict: Invalid,
                proof_search_check_id: None,
                proof_search_message:
                  Some(
                    "Algebrite suggestion could not be parsed: "
                    ++ candidate_text,
                  ),
                proof_search_source: None,
                cached_result: Calc.Pending,
              }),
          }
          |> Updated.return_quiet(~logged=true)
        }
      | None =>
        Model.{
          ...model,
          open_box:
            Model.WrittenStepOpen({
              ...r,
              proof_search_requested: false,
              proof_search_verdict: Invalid,
              proof_search_check_id: None,
              proof_search_message: Some(message),
              proof_search_source: None,
              cached_result: Calc.Pending,
            }),
        }
        |> Updated.return_quiet(~logged=true)
      }
    | (AlgebriteSuggestionFinished(_, _, _), _) =>
      model |> Updated.return_quiet
    | (
        AutoSimplifySuggestionFinished(
          expected_selected_exp,
          candidate,
          message,
        ),
        WrittenStepOpen({check_mode: ProofSearch, _} as r),
      ) =>
      let current_selected_exp =
        model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp");
      let selection_still_matches =
        switch (current_selected_exp) {
        | Some(current_selected_exp) =>
          Exp.fast_equal(current_selected_exp, expected_selected_exp)
        | None => false
        };
      if (selection_still_matches) {
        let model =
          Model.{
            ...model,
            open_box:
              Model.WrittenStepOpen({
                ...r,
                rewrite_selected_exp: Some(expected_selected_exp),
                rewrite_reparenthesized_exp: None,
              }),
          };
        update(
          ~settings,
          AlgebriteSuggestionFinished(
            candidate,
            message,
            "Algebrite auto simplify",
          ),
          model,
        );
      } else {
        model |> Updated.return_quiet;
      };
    | (AutoSimplifySuggestionFinished(_, _, _), _) =>
      model |> Updated.return_quiet
    | (
        AutoProfileSimplifySuggestionFinished(
          expected_selected_exp,
          candidate,
          message,
        ),
        WrittenStepOpen({check_mode: ProofSearch, _} as r),
      ) =>
      let current_selected_exp =
        model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp");
      let selection_still_matches =
        switch (current_selected_exp) {
        | Some(current_selected_exp) =>
          Exp.fast_equal(current_selected_exp, expected_selected_exp)
        | None => false
        };
      if (selection_still_matches) {
        let model =
          Model.{
            ...model,
            open_box:
              Model.WrittenStepOpen({
                ...r,
                rewrite_selected_exp: Some(expected_selected_exp),
                rewrite_reparenthesized_exp: None,
              }),
          };
        update(
          ~settings,
          ProfileSuggestionFinished(
            candidate,
            message,
            "profile-driven auto simplification",
          ),
          model,
        );
      } else {
        model |> Updated.return_quiet;
      };
    | (AutoProfileSimplifySuggestionFinished(_, _, _), _) =>
      model |> Updated.return_quiet
    | (
        ProfileSuggestionFinished(candidate, message, source),
        WrittenStepOpen({check_mode: ProofSearch, _} as r),
      ) =>
      switch (candidate) {
      | Some(candidate_exp) =>
        let editor =
          CodeWithStatics.Model.mk_from_exp(
            ~settings=settings.core,
            ~root=Exp,
            ~parenthesization=Haz3lcore.ExpToSegment.Settings.Defensive,
            candidate_exp,
          );
        Model.{
          ...model,
          open_box:
            Model.WrittenStepOpen({
              ...r,
              editor,
              proof_search_requested: false,
              proof_search_verdict: Ready,
              proof_search_check_id: None,
              proof_search_message: Some(message),
              proof_search_source: Some(source),
              cached_exp: Calc.Pending,
              cached_result: Calc.Pending,
            }),
        }
        |> Updated.return_quiet(~recalculate=true, ~logged=true);
      | None =>
        Model.{
          ...model,
          open_box:
            Model.WrittenStepOpen({
              ...r,
              proof_search_requested: false,
              proof_search_verdict: Invalid,
              proof_search_check_id: None,
              proof_search_message: Some(message),
              proof_search_source: None,
              cached_result: Calc.Pending,
            }),
        }
        |> Updated.return_quiet(~logged=true)
      }
    | (ProfileSuggestionFinished(_, _, _), _) =>
      model |> Updated.return_quiet
    | (WriteStepEditorAction(action), WrittenStepOpen({editor, _} as r)) =>
      let* new_editor = CodeEditable.Update.update(~settings, action, editor);
      let target_changed =
        CodeEditable.Model.to_string(editor)
        != CodeEditable.Model.to_string(new_editor);
      Model.{
        ...model,
        open_box:
          Model.WrittenStepOpen({
            ...r,
            editor: new_editor,
            proof_search_requested: false,
            proof_search_verdict:
              target_changed ? Ready : r.proof_search_verdict,
            proof_search_check_id: None,
            proof_search_message: None,
            proof_search_source: target_changed ? None : r.proof_search_source,
            cached_result:
              switch (r.check_mode) {
              | ProofSearch when target_changed => Calc.Pending
              | _ => r.cached_result
              },
          }),
      };
    | (WriteStepEditorAction(_), _) => model |> Updated.return_quiet
    | (AxiomBoxAction(action), WrittenStepOpen({axioms_model, _} as r)) =>
      let* updated = AxiomsBox.Update.update(~settings, action, axioms_model);
      Model.{
        ...model,
        open_box:
          Model.WrittenStepOpen({
            ...r,
            axioms_model: updated,
          }),
      };
    | (
        AxiomBoxAction(action),
        AxiomsOpen({
          axioms_model,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
          source_full_visible_exp,
        }),
      ) =>
      let* updated = AxiomsBox.Update.update(~settings, action, axioms_model);
      Model.{
        ...model,
        open_box:
          Model.AxiomsOpen({
            axioms_model: updated,
            rewrite_selected_exp,
            rewrite_reparenthesized_exp,
            source_full_visible_exp,
          }),
      };
    | (AxiomBoxAction(_), _) => model |> Updated.raise_invalid_action
    };
  };

  let can_undo = (action: t): bool => {
    switch (action) {
    | ToggleAxioms(_, _)
    | ProposeRewrite(_, _)
    | ProposeWrittenStep(_, _, _)
    | RunProofSearch(_, _, _, _, _)
    | RocqProofSearchStarted(_)
    | RocqProofSearchFinished(_, _, _, _)
    | RocqProofSearchCancelled(_)
    | AlgebriteSuggestionFinished(_, _, _)
    | AutoSimplifySuggestionFinished(_, _, _)
    | AutoProfileSimplifySuggestionFinished(_, _, _)
    | ProfileSuggestionFinished(_, _, _)
    | RewriteEditorAction(_)
    | WriteStepEditorAction(_)
    | AxiomBoxAction(_) => false
    };
  };

  let calculate =
      (
        ~rewrite_level: Axioms.rewrite_level,
        ~automation_stage: Axioms.automation_stage,
        ~active_profile: Axioms.math_profile,
        ~settings: CoreSettings.t,
        exp,
        info_map,
        ctx: Calc.t(SemanticCtx.t),
        new_next_steps,
        {
          next_steps: _,
          refls,
          assumptions,
          selected_exp,
          full_exp: _,
          full_visible_exp: _,
          selected_id,
          open_box,
          cached_env,
        }: Model.t,
        editor,
      )
      : Model.t => {
    let effective_profile = _level => active_profile;
    let editor: CodeSelectable.Model.t = editor |> Calc.get_value;
    let full_visible_exp = Calc.NewValue(editor.statics.term);
    let visible_terms = Calc.NewValue(editor.editor.syntax.terms);
    let effective_selection = effective_selection_for_editor(editor);
    let selected_id =
      SelectionEffective.root_id(effective_selection)
      |> Calc.set(_, selected_id);
    let selected_exp =
      selected_exp
      |> {
        let.calc selected_id = selected_id
        and.calc exp = exp
        and.calc full_visible_exp = full_visible_exp
        and.calc visible_terms = visible_terms
        and.calc info_map = info_map;
        OptUtil.Syntax.(
          switch (
            SelectionEffective.selected_exp(
              ~full_exp=full_visible_exp,
              effective_selection,
            )
          ) {
          | Some(exp') => Some(exp')
          | None =>
            let* id = selected_id;
            switch (ProofHacks.find_exp_id(id, full_visible_exp)) {
            | Some(exp') => Some(exp')
            | None =>
              switch (Id.Map.find_opt(id, visible_terms)) {
              | Some(Exp(exp')) => Some(exp')
              | _ =>
                switch (ProofHacks.find_exp_id(id, exp)) {
                | Some(exp') => Some(exp')
                | None =>
                  switch (Statics.Map.lookup(id, info_map)) {
                  | Some(Info.InfoExp({user_term, _})) => Some(user_term)
                  | _ =>
                    print_endline(
                      "[selected-exp-debug] missing id="
                      ++ Id.str8(id)
                      ++ " info_map_empty="
                      ++ (Id.Map.is_empty(info_map) ? "true" : "false"),
                    );
                    None;
                  }
                }
              }
            };
          }
        );
      };
    let assumptions =
      assumptions
      |> {
        let.calc _exp = selected_exp
        and.calc ctx = ctx;
        if (!settings.evaluation.enable_proof) {
          None;
        } else {
          let proof_ctx =
            ctx
            |> SemanticCtx.get_env
            |> Environment.to_list
            |> List.filter_map(((name, exp)) =>
                 switch (Exp.term_of(exp)) {
                 | Grammar.ProofObject(e) => Some((name, e))
                 | _ => None
                 }
               )
            |> List.fold_left(
                 (acc, (name, exp)) => ProofCtx.add_exp(name, exp, acc),
                 Axioms.v,
               )
            |> List.map(ctx_entry =>
                 AssumptionBox.Model.{ctx_entry: ctx_entry}
               );
          Some(proof_ctx);
        };
      };
    let selected_ctx =
      Calc.Pending
      |> {
        let.calc selected_id = selected_id
        and.calc info_map = info_map
        and.calc ctx = ctx;
        let fallback_ctx = ctx |> SemanticCtx.get_ctx;
        switch (selected_id) {
        | Some(id) =>
          switch (Statics.Map.lookup(id, info_map)) {
          | Some(info) => Info.ctx_of(info)
          | None => fallback_ctx
          }
        | None => fallback_ctx
        };
      };
    let selected_ana =
      Calc.Pending
      |> {
        let.calc selected_id = selected_id
        and.calc info_map = info_map;
        switch (selected_id) {
        | Some(id) =>
          switch (Statics.Map.lookup(id, info_map)) {
          | Some(Info.InfoExp(info)) => Some(info.ty)
          | _ => None
          }
        | None => None
        };
      };
    let refls =
      refls
      |> {
        let.calc exp = exp
        and.calc ctx = ctx
        and.calc new_next_steps = new_next_steps
        and.calc info_map = info_map;
        if (!settings.evaluation.enable_proof) {
          [];
        } else {
          let next_steps =
            new_next_steps
            |> (
              fun
              | EvaluatorStep.AutoStep(_) => []
              | EvaluatorStep.AvailableSteps(steps) => steps
            );
          ProofHacks.find_refls(
            ~info_map,
            ~env=SemanticCtx.get_env(ctx),
            exp,
          )
          |> List.filter(e =>
               !
                 List.exists(
                   s =>
                     e
                     |> Exp.rep_id
                     == (
                          EvaluatorStep.get_step_id_in(s, exp)
                          |> Option.value(
                               ~default=EvaluatorStep.get_step_id(s),
                             )
                        ),
                   next_steps,
                 )
               || settings.evaluation.write_out_steps
             );
        };
      };
    let open_box = {
      let current_full_visible_exp = editor.statics.term;
      let source_is_current = source_full_visible_exp =>
        switch (source_full_visible_exp) {
        | None => true
        | Some(source_full_visible_exp) =>
          Equality.ignoring_ascriptions.exp(
            source_full_visible_exp,
            current_full_visible_exp,
          )
        };
      switch (open_box) {
      | RewritesOpen({source_full_visible_exp, _})
          when !source_is_current(source_full_visible_exp) => Model.NoneOpen
      | RewritesOpen({
          editor,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
          source_full_visible_exp: _,
          cached_exp,
          cached_result,
        }) =>
        // Calculate syntax, holes, types, etc for the editor
        let ana = Calc.get_value(selected_ana);
        let editor =
          CodeEditable.Update.calculate(
            ~settings,
            ~is_edited=true,
            ~is_dynamic_term=true,
            ~dynamics=Dynamics.Map.empty,
            ~stitch=x => x,
            ~ctx=Calc.get_value(selected_ctx),
            ~ana?,
            editor,
          );
        // Extract an exp from the editor
        let cached_exp =
          Calc.set(
            ~eq=Exp.fast_equal,
            CodeEditable.Model.get_statics(editor).elaborated,
            cached_exp,
          );
        // Reset result if editor changes
        let cached_result =
          cached_result
          |> {
            let.calc sctx = ctx
            and.calc to_exp = cached_exp
            and.calc from_exp = selected_exp;
            let env = SemanticCtx.get_env(sctx);
            let from_exp =
              switch (rewrite_selected_exp) {
              | Some(rewrite_selected_exp) => Some(rewrite_selected_exp)
              | None => from_exp
              };
            let from_exp =
              Substitution.in_exp(
                env,
                from_exp
                |> Option.value(
                     ~default=IdTagged.FreshGrammar.Exp.empty_hole(),
                   ),
              );
            let to_exp = Substitution.in_exp(env, to_exp);
            RewriteChecker.check_rewrite_at_level(
              ~level=rewrite_level,
              ~settings,
              ~env,
              from_exp,
              to_exp,
            );
          };
        Model.RewritesOpen({
          editor,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
          source_full_visible_exp: Some(current_full_visible_exp),
          cached_exp: cached_exp |> Calc.save,
          cached_result: cached_result |> Calc.save,
        });
      | WrittenStepOpen({source_full_visible_exp, _})
          when !source_is_current(source_full_visible_exp) => Model.NoneOpen
      | WrittenStepOpen({
          editor,
          check_mode,
          axioms_model,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
          source_full_visible_exp: _,
          proof_search_requested,
          proof_search_verdict,
          proof_search_check_id,
          proof_search_message,
          proof_search_max_depth,
          proof_search_max_states,
          proof_search_source,
          calculated_rewrite_level,
          calculated_automation_stage,
          cached_exp,
          cached_result,
        }) =>
        let automation_stage_changed =
          calculated_automation_stage != Some(automation_stage);
        let check_mode =
          automation_stage_changed
            ? check_mode_for_automation_stage(automation_stage) : check_mode;
        // Calculate syntax, holes, types, etc for the editor
        let ana = Calc.get_value(selected_ana);
        let editor =
          CodeEditable.Update.calculate(
            ~settings,
            ~is_edited=true,
            ~is_dynamic_term=true,
            ~dynamics=Dynamics.Map.empty,
            ~stitch=x => x,
            ~ctx=Calc.get_value(selected_ctx),
            ~ana?,
            editor,
          );
        // Extract an exp from the editor
        let target_exp_changed =
          switch (
            check_mode,
            Calc.saved_to_option(cached_exp),
            CodeEditable.Model.get_statics(editor).elaborated,
          ) {
          | (ProofSearch, Some(old_exp), new_exp) =>
            !Exp.fast_equal(old_exp, new_exp)
          | (ProofSearch, None, _) => true
          | _ => false
          };
        let cached_exp =
          Calc.set(
            ~eq=Exp.fast_equal,
            CodeEditable.Model.get_statics(editor).elaborated,
            cached_exp,
          );
        let rewrite_level_changed =
          calculated_rewrite_level != Some(rewrite_level);
        let reset_proof_search =
          proof_search_state_is_stale(
            ~calculated_rewrite_level,
            ~rewrite_level,
            ~calculated_automation_stage,
            ~automation_stage,
            ~target_exp_changed,
            ~proof_search_source,
          );
        let cached_result =
          rewrite_level_changed ? Calc.Pending : cached_result;
        // Reset result if editor changes
        let cached_result =
          switch (check_mode) {
          | ProofSearch => reset_proof_search ? Calc.Pending : cached_result
          | _ =>
            cached_result
            |> {
              let.calc sctx = ctx
              and.calc to_exp = cached_exp
              and.calc from_exp = selected_exp;
              let env = SemanticCtx.get_env(sctx);
              let from_exp =
                switch (rewrite_selected_exp) {
                | Some(rewrite_selected_exp) => Some(rewrite_selected_exp)
                | None => from_exp
                };
              let from_exp =
                Substitution.in_exp(
                  env,
                  from_exp
                  |> Option.value(
                       ~default=IdTagged.FreshGrammar.Exp.empty_hole(),
                     ),
                );
              let to_exp = Substitution.in_exp(env, to_exp);
              switch (check_mode) {
              | SingleEvalStep =>
                let profile = effective_profile(rewrite_level);
                ProfileProofPlan.authorize({
                  profile,
                  stage: Axioms.Manual,
                  candidate_origin: ProfileProofPlan.UserEntered,
                  settings,
                  env,
                  source: from_exp,
                  target: to_exp,
                  max_depth: 1,
                  max_states: 80,
                })
                |> ProfileProofPlan.authorized_plan;
              | CheckResult =>
                let profile = effective_profile(rewrite_level);
                ProfileProofPlan.authorize({
                  profile,
                  stage: Axioms.MultiStepCheck,
                  candidate_origin: ProfileProofPlan.UserEntered,
                  settings,
                  env,
                  source: from_exp,
                  target: to_exp,
                  max_depth: 4,
                  max_states: 80,
                })
                |> ProfileProofPlan.authorized_plan;
              | ProofSearch => None
              };
            }
            |> Calc.save
          };
        let selected_exp_for_axioms =
          switch (rewrite_selected_exp) {
          | Some(_) => Calc.NewValue(rewrite_selected_exp)
          | None => selected_exp
          };
        let axioms_model =
          AxiomsBox.Update.calculate(
            ~info_map,
            ~ctx,
            ~selected_exp=selected_exp_for_axioms,
            axioms_model,
          );
        Model.WrittenStepOpen({
          editor,
          check_mode,
          axioms_model,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
          source_full_visible_exp: Some(current_full_visible_exp),
          proof_search_requested:
            reset_proof_search ? false : proof_search_requested,
          proof_search_verdict:
            reset_proof_search ? Ready : proof_search_verdict,
          proof_search_check_id:
            reset_proof_search ? None : proof_search_check_id,
          proof_search_message:
            reset_proof_search ? None : proof_search_message,
          proof_search_max_depth,
          proof_search_max_states,
          proof_search_source: reset_proof_search ? None : proof_search_source,
          calculated_rewrite_level: Some(rewrite_level),
          calculated_automation_stage: Some(automation_stage),
          cached_exp: cached_exp |> Calc.save,
          cached_result,
        });
      | AxiomsOpen({
          axioms_model,
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
          source_full_visible_exp: _,
        }) =>
        let selected_exp_for_axioms =
          switch (rewrite_selected_exp) {
          | Some(_) => Calc.NewValue(rewrite_selected_exp)
          | None => selected_exp
          };
        AxiomsOpen({
          axioms_model:
            AxiomsBox.Update.calculate(
              ~info_map,
              ~ctx,
              ~selected_exp=selected_exp_for_axioms,
              axioms_model,
            ),
          rewrite_selected_exp,
          rewrite_reparenthesized_exp,
          source_full_visible_exp: Some(current_full_visible_exp),
        });
      | NoneOpen
          when
            !settings.evaluation.enable_proof
            && settings.evaluation.write_out_steps =>
        Model.WrittenStepOpen({
          editor:
            CodeEditable.Model.mk(Editor.Model.mk(~root=Exp, Zipper.init())),
          check_mode: CheckResult,
          axioms_model: AxiomsBox.Model.init,
          rewrite_selected_exp: None,
          rewrite_reparenthesized_exp: None,
          source_full_visible_exp: Some(current_full_visible_exp),
          proof_search_requested: false,
          proof_search_verdict: Ready,
          proof_search_check_id: None,
          proof_search_message: None,
          proof_search_max_depth: 4,
          proof_search_max_states: 80,
          proof_search_source: None,
          calculated_rewrite_level: Some(rewrite_level),
          calculated_automation_stage: Some(automation_stage),
          cached_exp: Calc.Pending,
          cached_result: Calc.Pending,
        })
      | NoneOpen => open_box
      };
    };
    let cached_env =
      cached_env
      |> {
        let.calc ctx = ctx;
        SemanticCtx.get_env(ctx);
      };
    {
      next_steps: new_next_steps |> Calc.save,
      refls: refls |> Calc.save,
      assumptions: assumptions |> Calc.save,
      full_exp: exp |> Calc.save,
      full_visible_exp: full_visible_exp |> Calc.save,
      selected_exp: selected_exp |> Calc.save,
      selected_id: selected_id |> Calc.save,
      open_box,
      cached_env: cached_env |> Calc.save,
    };
  };
};

module Selection = {
  open Cursor;
  // Selection handles focus

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | RewriteEditor(CodeEditable.Selection.t)
    | WriteStepEditor(CodeEditable.Selection.t)
    | AxiomBoxSelection(AxiomsBox.Selection.t);

  let get_cursor_info =
      (~inject, ~selection: t, model: Model.t): cursor(Update.t) => {
    switch (selection, model.open_box) {
    | (RewriteEditor(selection), RewritesOpen({editor, _})) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(
          ~inject=a => inject(Update.RewriteEditorAction(a)),
          ~selection,
          editor,
        );
      Update.RewriteEditorAction(ci);
    | (RewriteEditor(_), _) => empty
    | (WriteStepEditor(selection), WrittenStepOpen({editor, _})) =>
      let+ ci =
        CodeEditable.Selection.get_cursor_info(
          ~inject=a => inject(Update.WriteStepEditorAction(a)),
          ~selection,
          editor,
        );
      Update.WriteStepEditorAction(ci);
    | (WriteStepEditor(_), _) => empty
    | (AxiomBoxSelection(selection), AxiomsOpen({axioms_model, _})) =>
      let+ ci = AxiomsBox.Selection.get_cursor_info(~selection, axioms_model);
      Update.AxiomBoxAction(ci);
    | (AxiomBoxSelection(selection), WrittenStepOpen({axioms_model, _})) =>
      let+ ci = AxiomsBox.Selection.get_cursor_info(~selection, axioms_model);
      Update.AxiomBoxAction(ci);
    | (AxiomBoxSelection(_), _) => empty
    };
  };
};

module AutoSimplifyDebounce = {
  /* This follows source-selection changes while the Auto simplify Search pane
     is open. It intentionally does not debounce target-editor typing. */
  let debounce_ms = 400.0;
  let timer_id: ref(option(Js_of_ocaml.Dom_html.timeout_id)) = ref(None);
  let scheduled_key: ref(option(string)) = ref(None);
  let generation = ref(0);

  let cancel = (~reset_key=false, ()) => {
    switch (timer_id^) {
    | Some(id) => Js_of_ocaml.Dom_html.window##clearTimeout(id)
    | None => ()
    };
    timer_id := None;
    generation := generation^ + 1;
    if (reset_key) {
      scheduled_key := None;
    };
  };

  let schedule = (~key: string, ~run: unit => unit) =>
    if (scheduled_key^ != Some(key)) {
      cancel();
      scheduled_key := Some(key);
      let this_generation = generation^;
      timer_id :=
        Some(
          Js_of_ocaml.Dom_html.window##setTimeout(
            Js_of_ocaml.Js.wrap_callback(() => {
              timer_id := None;
              if (generation^ == this_generation) {
                run();
              };
            }),
            debounce_ms,
          ),
        );
    };
};

let auto_simplify_uses_profile = (level, exp) =>
  level == Axioms.Calculus && DifferentiationRewrite.contains_diff(exp);

module View = {
  open OptUtil.Syntax;

  let warmup_rocq_search_in_browser = (): unit =>
    try(
      Js_of_ocaml.Js.Unsafe.fun_call(
        Js_of_ocaml.Js.Unsafe.js_expr("window.HazelJSCoq.warmupSearch"),
        [||],
      )
      |> ignore
    ) {
    | _ => ()
    };

  let check_rocq_search_in_browser =
      (
        ~check_id: int,
        ~coq_data: string,
        ~on_result: (bool, string) => unit,
        ~on_cancel: unit => unit,
      )
      : unit => {
    let callback =
      Js_of_ocaml.Js.wrap_callback((status_js, message_js) => {
        let status =
          status_js |> Js_of_ocaml.Js.Unsafe.coerce |> Js_of_ocaml.Js.to_string;
        let message =
          message_js
          |> Js_of_ocaml.Js.Unsafe.coerce
          |> Js_of_ocaml.Js.to_string;
        status == "cancelled"
          ? on_cancel() : on_result(status == "ok", message);
      });
    let opts =
      Js_of_ocaml.Js.Unsafe.obj([|
        ("requestId", Js_of_ocaml.Js.Unsafe.inject(check_id)),
      |]);
    try(
      Js_of_ocaml.Js.Unsafe.fun_call(
        Js_of_ocaml.Js.Unsafe.js_expr("window.HazelJSCoq.searchAndReport"),
        [|
          Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(coq_data)),
          Js_of_ocaml.Js.Unsafe.inject(callback),
          Js_of_ocaml.Js.Unsafe.inject(opts),
        |],
      )
      |> ignore
    ) {
    | _ =>
      on_result(
        false,
        "JSCoq/Rocq tactic search failed to start. See the browser console.",
      )
    };
  };

  let cancel_rocq_searches_except_in_browser = check_id => {
    let request_id =
      check_id |> Option.map(string_of_int) |> Option.value(~default="");
    try(
      Js_of_ocaml.Js.Unsafe.fun_call(
        Js_of_ocaml.Js.Unsafe.js_expr(
          "(function(requestId) { if (window.HazelJSCoq && window.HazelJSCoq.cancelSearchesExcept) { window.HazelJSCoq.cancelSearchesExcept(requestId === '' ? null : Number(requestId)); } })",
        ),
        [|Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(request_id))|],
      )
      |> ignore
    ) {
    | _ => ()
    };
  };

  let cancel_proof_searches_except_in_browser = check_id => {
    ProofSearchBackend.cancel_local_profile_plans_except(check_id);
    cancel_rocq_searches_except_in_browser(check_id);
  };

  let algebrite_suggestion_effect =
      (~inject, ~selected_exp, ~method_name, ~source, ~auto_selected_exp=None) => {
    let finish = (candidate, message) =>
      switch (auto_selected_exp) {
      | Some(expected_selected_exp) =>
        inject(
          Update.AutoSimplifySuggestionFinished(
            expected_selected_exp,
            candidate,
            message,
          ),
        )
      | None =>
        inject(
          Update.AlgebriteSuggestionFinished(candidate, message, source),
        )
      };
    let fail = message => finish(None, message);
    switch (AlgebriteSuggestion.serialize_for_algebrite(selected_exp)) {
    | None => fail("Algebrite suggestion is unavailable for this expression.")
    | Some(input) =>
      try({
        let simplify =
          Js_of_ocaml.Js.Unsafe.js_expr(
            "(function(methodName, input) { if (!window.HazelAlgebrite || !window.HazelAlgebrite[methodName]) { throw new Error('Algebrite operation is not available.'); } return window.HazelAlgebrite[methodName](input); })",
          );
        let raw_result =
          Js_of_ocaml.Js.Unsafe.fun_call(
            simplify,
            [|
              Js_of_ocaml.Js.Unsafe.inject(
                Js_of_ocaml.Js.string(method_name),
              ),
              Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js.string(input)),
            |],
          );
        let candidate =
          raw_result
          |> Js_of_ocaml.Js.Unsafe.coerce
          |> Js_of_ocaml.Js.to_string
          |> AlgebriteSuggestion.hazel_syntax_of_algebrite;
        candidate == ""
          ? fail("Algebrite returned an empty suggestion.")
          : finish(Some(candidate), "Algebrite suggested: " ++ candidate);
      }) {
      | Failure(message) => fail("Algebrite suggestion failed: " ++ message)
      | exn =>
        fail("Algebrite suggestion failed: " ++ Printexc.to_string(exn))
      }
    };
  };

  let profile_suggestion_effect =
      (
        ~inject,
        ~selected_exp,
        ~profile,
        ~settings,
        ~env,
        ~auto_selected_exp=None,
      ) => {
    let candidate =
      selected_exp
      |> Substitution.in_exp(env)
      |> RewriteChecker.simplify_for_profile(~profile, ~settings, ~env);
    let message =
      switch (candidate) {
      | Some(_) => "Active profile suggested a derivative result."
      | None => "The active profile could not simplify this expression."
      };
    switch (auto_selected_exp) {
    | Some(expected_selected_exp) =>
      inject(
        Update.AutoProfileSimplifySuggestionFinished(
          expected_selected_exp,
          candidate,
          message,
        ),
      )
    | None =>
      inject(
        Update.ProfileSuggestionFinished(
          candidate,
          message,
          "profile-driven differentiation",
        ),
      )
    };
  };

  type event =
    | AddInduction(option(Exp.t))
    | AddForall
    | HideStepper
    | AddAxiomStep(string, int, Exp.t, Direction.t, string)
    | AddReparenthesizedAxiomStep(Exp.t, string, Exp.t, Direction.t, string)
    | AddAlgebriteStep(int, Exp.t, Exp.t)
    | AddReparenthesizeStep(Exp.t)
    | AddReparenthesizedAlgebriteStep(Exp.t, Exp.t, Exp.t)
    | AddReparenthesizedWrittenStep(
        ProofTrace.trace_summary,
        Exp.t,
        Exp.t,
        Exp.t,
      )
    | AddWrittenStep(ProofTrace.trace_summary, int, Exp.t, Exp.t)
    | AutoSimplify(Exp.t, Exp.t)
    | MakeActive(Selection.t)
    | TakeStep(int)
    | Refl(int)
    | StepHere(list(Language.Id.t), bool);

  let get_segment_bounds = (~measured: Measured.t, segment: Segment.t) => {
    let* first_piece = ListUtil.hd_opt(segment);
    let Point.{row: start_y, col: start_x} =
      Measured.find_p(~msg="get_segment_bounds", first_piece, measured)
      |> (m => m.origin);
    let* last_piece = ListUtil.last_opt(segment);
    let Point.{row: end_y, col: end_x} =
      Measured.find_p(~msg="get_segment_bounds", last_piece, measured)
      |> (m => m.last);
    let rec get_left = (current_left: int, row: int, final_row: int) =>
      if (row > final_row) {
        current_left;
      } else {
        get_left(
          Int.min(
            current_left,
            Measured.Rows.find(row, measured.rows).indent,
          ),
          row + 1,
          final_row,
        );
      };
    let left = get_left(start_x, start_y, end_y);
    let rec get_right = (current_right: int, row: int, final_row: int) =>
      if (row == final_row) {
        current_right;
      } else {
        get_right(
          Int.max(
            current_right,
            Measured.Rows.find(row, measured.rows).max_col,
          ),
          row + 1,
          final_row,
        );
      };
    let right = get_right(end_x, start_y, end_y);
    Some((left, right, start_y, end_y + 1));
  };

  let view_overlay =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~editor: CodeSelectable.Model.t,
        ~selected: option(Selection.t),
        ~rewrite_level: Axioms.rewrite_level,
        ~automation_stage: Axioms.automation_stage,
        ~active_profile: Axioms.math_profile,
        ~info_map,
        model: Model.t,
      ) =>
    {
      let active_profile_for_model = (_model, _rewrite_level) => active_profile;
      let segment_bounds =
        get_segment_bounds(
          ~measured=editor.editor.syntax.measured,
          editor.editor.state.zipper.selection.content,
        );
      /* Proof-search ownership is changed only by explicit lifecycle events.
       * Cancelling from here is unsafe: every theorem on an exercise page
       * renders its own overlay, so an idle theorem would cancel another
       * theorem's active Rocq request. */
      let+ (left, right, top, bottom) = segment_bounds;
      let effective_selection = effective_selection_for_editor(editor);
      let selection_override =
        SelectionEffective.virtual_target(effective_selection);
      let selection_term_data = editor.editor.syntax.term_data;

      let proof_button = (~callback: Ui_effect.t(unit), label: string) => {
        Node.div(
          ~attrs=[
            Attr.classes(["proof-button"]),
            Attr.on_pointerdown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
            Attr.on_pointerup(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
            Attr.on_mousemove(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
            Attr.on_click(_ =>
              Ui_effect.Many([
                callback,
                Virtual_dom.Vdom.Effect.Stop_propagation,
              ])
            ),
          ],
          [Node.text(label)],
        );
      };

      let selected_id =
        model.selected_id |> Calc.get_saved_exc(~print="Selected Id");

      let show_step_button =
        if (globals.settings.core.evaluation.write_out_steps) {
          None;
        } else {
          switch (selected_id) {
          | Some(selected_id) =>
            let visible_exp = editor.statics.term;
            List.find_index(
              step_id => step_id == selected_id,
              model.next_steps
              |> Calc.get_saved_exc(~print="next_steps")
              |> (
                fun
                | AutoStep(_) => []
                | AvailableSteps(steps) => steps
              )
              |> List.map(step =>
                   EvaluatorStep.get_step_id_in(step, visible_exp)
                   |> Option.value(~default=EvaluatorStep.get_step_id(step))
                 ),
            );
          | None => None
          };
        };

      let show_refl_button =
        switch (
          model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp")
        ) {
        | Some(selected_exp) =>
          List.find_index(
            x => x == (selected_exp |> Exp.rep_id),
            model.refls
            |> Calc.get_saved_exc(~print="refls")
            |> List.map(refl => refl |> Exp.rep_id),
          )
        | None => None
        };

      let show_function_body_button = {
        Calc.get_saved_exc(model.selected_exp)
        == Some(Calc.get_saved_exc(model.full_visible_exp))
        && Exp.is_fun(Calc.get_saved_exc(model.full_visible_exp));
      };

      /* Written-step actions from both the explicit Replace button and the
       * suggested-rule arrow must resolve virtual associative selections the
       * same way.  Such a selection has no subtree index in [full_visible_exp],
       * so rewrite its concrete range inside the containing associative node. */
      let add_written_step_for_selection =
          (
            trace_summary,
            rewrite_reparenthesized_exp,
            at_idx,
            at_exp,
            with_exp,
          ) => {
        let full_visible_exp =
          model.full_visible_exp
          |> Calc.get_saved_exc(~print="full_visible_exp");
        let direct_override =
          selection_override
          |> Option.bind(_, override =>
               SelectionEffective.replacement_for_virtual(
                 ~virtual_=override,
                 ~with_exp,
                 ~full_exp=full_visible_exp,
                 ~term_data=selection_term_data,
               )
             );
        switch (direct_override, rewrite_reparenthesized_exp) {
        | (Some({at_exp, with_exp}), _) =>
          signal(
            AddWrittenStep(
              trace_summary,
              ProofHacks.exp_idx(at_exp, full_visible_exp),
              at_exp,
              with_exp,
            ),
          )
        | (None, Some(reparenthesized_exp)) =>
          /* The main editor can lose its virtual associative range when the
           * target mini-editor takes focus.  Keep the concrete source captured
           * when Search opened so Replace still has a real subtree to target. */
          signal(
            AddReparenthesizedWrittenStep(
              trace_summary,
              reparenthesized_exp,
              at_exp,
              with_exp,
            ),
          )
        | (None, None) =>
          signal(AddWrittenStep(trace_summary, at_idx, at_exp, with_exp))
        };
      };

      let add_axiom_step_for_selection =
          (
            rewrite_reparenthesized_exp,
            name,
            at_idx,
            at_exp,
            direction,
            equality,
          ) =>
        switch (rewrite_reparenthesized_exp) {
        | Some(reparenthesized_exp) =>
          signal(
            AddReparenthesizedAxiomStep(
              reparenthesized_exp,
              name,
              at_exp,
              direction,
              equality,
            ),
          )
        | None =>
          let full_visible_exp =
            model.full_visible_exp
            |> Calc.get_saved_exc(~print="full_visible_exp");
          switch (selection_override) {
          | Some(override) =>
            switch (
              SelectionEffective.reparenthesize_virtual(
                ~virtual_=override,
                ~full_exp=full_visible_exp,
              )
            ) {
            | Some(reparenthesized) =>
              switch (Language.Reparenthesize.selected_exp(reparenthesized)) {
              | Some(selected_exp) =>
                signal(
                  AddReparenthesizedAxiomStep(
                    reparenthesized.exp,
                    name,
                    selected_exp,
                    direction,
                    equality,
                  ),
                )
              | None => Ui_effect.Ignore
              }
            | None => Ui_effect.Ignore
            }
          | None =>
            signal(AddAxiomStep(name, at_idx, at_exp, direction, equality))
          };
        };

      let view_rewrites_box =
          (
            editor,
            rewrite_selected_exp,
            rewrite_reparenthesized_exp,
            cached_exp,
            cached_result,
          ) => {
        let unboxed_cached_exp =
          Calc.get_saved_exc(~print="cached exp not calculated", cached_exp);
        let unboxed_selected_exp =
          Option.value(~default=EmptyHole |> Exp.fresh, rewrite_selected_exp);
        [
          // one element list with a div
          // with a list containing two elements
          // an Editor for user to propose their rewrite
          // a button to submit the rewrite
          div_c(
            "rewrite-box",
            [
              Node.text("Replace: "),
              CodeViewable.view_any(
                ~globals,
                ~settings=
                  ExpToSegment.Settings.of_core(
                    ~inline=false,
                    ~fold_fn_bodies=`Text,
                    globals.settings.core,
                  ),
                Exp(unboxed_selected_exp),
              ),
              Node.text("With: "),
              div_c(
                "inline-editor-wrapper",
                [
                  CodeEditable.View.view(
                    ~globals,
                    ~signal=
                      fun
                      | MakeActive => signal(MakeActive(RewriteEditor())),
                    ~edit_mode=
                      EditMode.Editable({
                        inject: x => inject(RewriteEditorAction(x)),
                        escape: _ => Ui_effect.Ignore,
                        take_focus: _ => Ui_effect.Ignore,
                        focus:
                          switch (selected) {
                          | Some(RewriteEditor ()) => Some()
                          | _ => None
                          },
                        highlight: false,
                      }),
                    ~dynamics=Dynamics.Map.empty,
                    editor,
                  ),
                ],
              ),
            ]
            @ {
              switch (cached_result) {
              | Some(true) => [
                  Node.text("Valid"),
                  Widgets.button(
                    ~clss=["proof-button"],
                    Node.text("Replace"),
                    ~tooltip="replace",
                    _ => {
                      let substituted_cached_exp =
                        unboxed_cached_exp
                        |> Substitution.in_exp(
                             model.cached_env
                             |> Calc.get_saved_exc(~print="env not cached"),
                           );
                      switch (rewrite_reparenthesized_exp) {
                      | Some(reparenthesized_exp) =>
                        signal(
                          AddReparenthesizedAlgebriteStep(
                            reparenthesized_exp,
                            unboxed_selected_exp,
                            substituted_cached_exp,
                          ),
                        )
                      | None =>
                        let full_visible_exp =
                          model.full_visible_exp
                          |> Calc.get_saved_exc(~print="full_visible_exp");
                        switch (selection_override) {
                        | Some(override) =>
                          switch (
                            SelectionEffective.replacement_for_virtual(
                              ~virtual_=override,
                              ~with_exp=substituted_cached_exp,
                              ~full_exp=full_visible_exp,
                              ~term_data=selection_term_data,
                            )
                          ) {
                          | Some({at_exp, with_exp}) =>
                            signal(
                              AddAlgebriteStep(
                                ProofHacks.exp_idx(at_exp, full_visible_exp),
                                at_exp,
                                with_exp,
                              ),
                            )
                          | None => Ui_effect.Ignore
                          }
                        | None =>
                          let at_idx =
                            try(
                              ProofHacks.exp_idx(
                                unboxed_selected_exp,
                                full_visible_exp,
                              )
                            ) {
                            | _ => 0
                            };
                          signal(
                            AddAlgebriteStep(
                              at_idx,
                              unboxed_selected_exp,
                              substituted_cached_exp,
                            ),
                          );
                        };
                      };
                    },
                  ),
                ]
              | Some(false) => [Node.text("Invalid")]
              | None => [Node.text("...")]
              };
            },
          ),
        ];
      };

      let view_written_step_box =
          (
            editor: CodeEditable.Model.t,
            check_mode,
            rewrite_selected_exp,
            rewrite_reparenthesized_exp,
            proof_search_verdict,
            proof_search_check_id,
            proof_search_message,
            proof_search_source,
            cached_exp,
            cached_result,
            suggestions,
          ) => {
        warmup_rocq_search_in_browser();
        let unboxed_cached_exp =
          Calc.get_saved_exc(~print="cached exp not calculated", cached_exp);
        let unboxed_selected_exp =
          switch (rewrite_selected_exp) {
          | Some(rewrite_selected_exp) => rewrite_selected_exp
          | None =>
            Option.value(
              ~default=EmptyHole |> Exp.fresh,
              Calc.get_saved_exc(
                ~print="selected exp not calculated",
                model.selected_exp,
              ),
            )
          };
        let proof_plan_matches_current =
            (plan: ProfileProofPlan.authorized_plan) => {
          let summary = plan.summary;
          let active_profile = active_profile_for_model(model, rewrite_level);
          let active_fingerprint =
            ProfileProofPlan.profile_fingerprint(
              active_profile,
              automation_stage,
            );
          switch (summary.ProofTrace.prover_steps) {
          | [] => false
          | [first_step, ...rest_steps] =>
            let last_step =
              switch (rest_steps) {
              | [] => first_step
              | _ => ListUtil.last(rest_steps)
              };
            plan.profile_fingerprint == active_fingerprint
            && Equality.ignoring_ascriptions.exp(
                 first_step.before_full_exp,
                 unboxed_selected_exp,
               )
            && Equality.ignoring_ascriptions.exp(
                 last_step.after_full_exp,
                 unboxed_cached_exp,
               );
          };
        };
        let cached_result =
          switch (check_mode, cached_result) {
          | (Model.ProofSearch, Some(Some(plan)))
              when !proof_plan_matches_current(plan) =>
            None
          | _ => cached_result
          };
        /* A successful Rocq callback can outlive the calculated trace cache by
         * one recalculation.  Reconstruct only the exact current profile trace
         * so ProfileValid never loses its Replace control, while stale or
         * outside-profile candidates remain non-committable. */
        let cached_result =
          switch (check_mode, proof_search_verdict, cached_result) {
          | (Model.ProofSearch, Model.ProfileValid, None | Some(None)) =>
            let request =
              ProofSearchBackend.{
                backend: JSCoqTacticSearch,
                level: rewrite_level,
                max_depth: 4,
                max_states: 80,
                source: unboxed_selected_exp,
                target: unboxed_cached_exp,
              };
            let active_profile =
              active_profile_for_model(model, rewrite_level);
            let env =
              model.cached_env |> Calc.get_saved_exc(~print="env not cached");
            ProofSearchBackend.local_profile_plan(
              ~profile=active_profile,
              ~settings=globals.settings.core,
              ~env,
              request,
            )
            |> Option.map(plan => Some(plan));
          | _ => cached_result
          };
        let replace_button = (plan: ProfileProofPlan.authorized_plan) =>
          Widgets.button(
            ~clss=["proof-button"],
            Node.text("Replace"),
            ~tooltip="replace",
            _ => {
              let substituted_cached_exp =
                unboxed_cached_exp
                |> Substitution.in_exp(
                     model.cached_env
                     |> Calc.get_saved_exc(~print="env not cached"),
                   );
              let full_visible_exp =
                model.full_visible_exp
                |> Calc.get_saved_exc(~print="full_visible_exp");
              let at_idx =
                try(
                  ProofHacks.exp_idx(unboxed_selected_exp, full_visible_exp)
                ) {
                | _ => 0
                };
              add_written_step_for_selection(
                plan.summary,
                rewrite_reparenthesized_exp,
                at_idx,
                unboxed_selected_exp,
                substituted_cached_exp,
              );
            },
          );
        let cancel_proof_search_button =
          proof_search_check_id
          |> Option.map(check_id =>
               Widgets.button(
                 ~clss=["proof-button"],
                 Node.text("Cancel"),
                 ~tooltip="cancel profile planning and Rocq checking",
                 _ => {
                   cancel_proof_searches_except_in_browser(None);
                   inject(RocqProofSearchCancelled(check_id));
                 },
               )
             );
        let run_proof_search_button =
          Widgets.button(
            ~clss=["proof-button"],
            Node.text(
              proof_search_source |> Option.is_some
                ? "Validate Candidate" : "Run Rocq Search",
            ),
            ~tooltip=
              proof_search_source |> Option.is_some
                ? "validate candidate against the active profile"
                : "run JSCoq/Rocq tactic search",
            _ => {
              let check_id = JsUtil.date_now()##getTime |> int_of_float;
              /* Starting a request is the ownership handoff point.  Retain
               * this request while cancelling only genuinely older local and
               * Rocq work. */
              cancel_proof_searches_except_in_browser(Some(check_id));
              let request =
                ProofSearchBackend.{
                  backend: JSCoqTacticSearch,
                  level: rewrite_level,
                  max_depth: 4,
                  max_states: 80,
                  source: unboxed_selected_exp,
                  target: unboxed_cached_exp,
                };
              let active_profile =
                active_profile_for_model(model, rewrite_level);
              let env =
                model.cached_env
                |> Calc.get_saved_exc(~print="env not cached");
              let start_search =
                inject(
                  RunProofSearch(
                    check_id,
                    4,
                    80,
                    rewrite_selected_exp,
                    rewrite_reparenthesized_exp,
                  ),
                );
              let start_rocq = local_plan =>
                try({
                  let local_trace =
                    local_plan
                    |> Option.map((plan: ProfileProofPlan.authorized_plan) =>
                         plan.summary
                       );
                  let coq_data =
                    switch (local_plan) {
                    | Some(plan) =>
                      ProofSearchBackend.rocq_program_for_authorized_plan(
                        ~profile=active_profile,
                        request,
                        plan,
                      )
                    | None =>
                      ProofSearchBackend.rocq_equivalence_program_for_profile(
                        ~profile=active_profile,
                        request,
                      )
                    };
                  let finish = (verdict, message) =>
                    Ui_effect.Expert.handle(
                      inject(
                        RocqProofSearchFinished(
                          check_id,
                          verdict,
                          message,
                          local_plan,
                        ),
                      ),
                    );
                  Ui_effect.Expert.handle(
                    inject(RocqProofSearchStarted(check_id)),
                  );
                  check_rocq_search_in_browser(
                    ~check_id,
                    ~coq_data,
                    ~on_result=
                      (ok, message) =>
                        if (ok) {
                          switch (local_trace) {
                          | Some(_) => finish(ProfileValid, message)
                          | None =>
                            finish(
                              EquivalentOutsideProfile,
                              "Equivalent, but the active profile has no enabled trace for this result. Enable the required operation or cleanup capability.",
                            )
                          };
                        } else {
                          finish(
                            string_contains("timed out", message)
                              ? TimedOut : Invalid,
                            proof_search_failure_message(
                              ~has_profile_trace=local_trace |> Option.is_some,
                              message,
                            ),
                          );
                        },
                    ~on_cancel=
                      () =>
                        Ui_effect.Expert.handle(
                          inject(RocqProofSearchCancelled(check_id)),
                        ),
                  );
                }) {
                | Failure(message) =>
                  Ui_effect.Expert.handle(
                    inject(
                      RocqProofSearchFinished(
                        check_id,
                        Invalid,
                        "Rocq export failed: " ++ message,
                        None,
                      ),
                    ),
                  )
                | _ =>
                  Ui_effect.Expert.handle(
                    inject(
                      RocqProofSearchFinished(
                        check_id,
                        Invalid,
                        "Rocq export failed: unexpected exception while generating Coq",
                        None,
                      ),
                    ),
                  )
                };
              ProofSearchBackend.local_profile_plan_incremental(
                ~check_id,
                ~candidate_origin=
                  proof_search_source |> Option.is_some
                    ? ProfileProofPlan.AutomaticSimplify
                    : ProfileProofPlan.UserEntered,
                ~profile=active_profile,
                ~settings=globals.settings.core,
                ~env,
                ~on_finish=
                  fun
                  | ProofSearchBackend.LocalPlanningFinished(local_plan) =>
                    start_rocq(local_plan)
                  | LocalPlanningCancelled =>
                    Ui_effect.Expert.handle(
                      inject(RocqProofSearchCancelled(check_id)),
                    )
                  | LocalPlanningTimedOut =>
                    Ui_effect.Expert.handle(
                      inject(
                        RocqProofSearchFinished(
                          check_id,
                          TimedOut,
                          "Profile planning timed out before Rocq checking began.",
                          None,
                        ),
                      ),
                    )
                  | LocalPlanningFailed(message) =>
                    Ui_effect.Expert.handle(
                      inject(
                        RocqProofSearchFinished(
                          check_id,
                          Invalid,
                          "Profile planning failed: " ++ message,
                          None,
                        ),
                      ),
                    ),
                request,
              );
              start_search;
            },
          );
        let algebrite_suggestion_button =
            (~label, ~tooltip, ~method_name, ~source) =>
          Widgets.button(
            ~clss=["proof-button"], Node.text(label), ~tooltip, _ =>
            algebrite_suggestion_effect(
              ~inject,
              ~selected_exp=unboxed_selected_exp,
              ~method_name,
              ~source,
              ~auto_selected_exp=None,
            )
          );
        let simplify_with_algebrite_button =
          algebrite_suggestion_button(
            ~label="Simplify",
            ~tooltip="Simplify expression",
            ~method_name="simplifyToString",
            ~source="Algebrite simplify candidate",
          );
        let simplify_with_profile_button =
          Widgets.button(
            ~clss=["proof-button"],
            Node.text("Simplify"),
            ~tooltip="Simplify using the active math profile",
            _ => {
              let env =
                model.cached_env
                |> Calc.get_saved_exc(~print="env not cached");
              let profile = active_profile_for_model(model, rewrite_level);
              profile_suggestion_effect(
                ~inject,
                ~selected_exp=unboxed_selected_exp,
                ~profile,
                ~settings=globals.settings.core,
                ~env,
                ~auto_selected_exp=None,
              );
            },
          );
        let factor_with_algebrite_button =
          algebrite_suggestion_button(
            ~label="Factor",
            ~tooltip="Factor expression",
            ~method_name="factorToString",
            ~source="Algebrite factor candidate",
          );
        let factor_suggestion_available =
          AlgebriteSuggestion.factor_suggestion_enabled_for_profile(
            active_profile_for_model(model, rewrite_level),
          )
          && AlgebriteSuggestion.is_factor_candidate_shape(
               unboxed_selected_exp,
             )
          && (
            switch (
              AlgebriteSuggestion.serialize_for_algebrite(
                unboxed_selected_exp,
              )
            ) {
            | Some(input) when String.length(input) <= 512 =>
              try(
                Js_of_ocaml.Js.Unsafe.fun_call(
                  Js_of_ocaml.Js.Unsafe.js_expr(
                    "(function(input) { return !!(window.HazelAlgebrite && window.HazelAlgebrite.hasNontrivialFactorization && window.HazelAlgebrite.hasNontrivialFactorization(input)); })",
                  ),
                  [|
                    Js_of_ocaml.Js.Unsafe.inject(
                      Js_of_ocaml.Js.string(input),
                    ),
                  |],
                )
                |> Js_of_ocaml.Js.Unsafe.coerce
                |> Js_of_ocaml.Js.to_bool
              ) {
              | _ => false
              }
            | _ => false
            }
          );
        let algebrite_suggestion_controls =
          switch (check_mode) {
          | Model.ProofSearch =>
            let simplify_controls =
              switch (rewrite_level) {
              | Arithmetic
              | Algebra
              | Trigonometry
              | FunctionsAndLists => [simplify_with_algebrite_button]
              | Calculus => [
                  auto_simplify_uses_profile(
                    rewrite_level,
                    unboxed_selected_exp,
                  )
                    ? simplify_with_profile_button
                    : simplify_with_algebrite_button,
                ]
              };
            simplify_controls
            @ (
              factor_suggestion_available
                ? [factor_with_algebrite_button] : []
            );
          | _ => []
          };
        let mode_issue =
          AxiomSearch.unsupported_constructs_message_for_rewrite(
            ~level=rewrite_level,
            ~source=unboxed_selected_exp,
            ~target=unboxed_cached_exp,
          );
        let inactive_session_warning =
          check_mode == Model.ProofSearch
          && Axioms.session_rewrites_for_profile(
               active_profile_for_model(model, rewrite_level),
             )
          != []
            ? [
              div_c(
                "proof-mode-warning session-rewrite-warning",
                [
                  Node.text(
                    "Untrusted session rewrites are One Step-only. Rocq validation and proof export mark them as UNSOUND and use Admitted.",
                  ),
                ],
              ),
            ]
            : [];
        let mode_issue_view = message =>
          div_c("proof-mode-warning", [Node.text(message)]);
        let mode_issue_ids =
          AxiomSearch.unsupported_construct_ids_for_rewrite(
            ~level=rewrite_level,
            ~source=unboxed_selected_exp,
            ~target=unboxed_cached_exp,
          );
        let mode_issue_overlay = (editor: CodeWithStatics.Model.t) =>
          switch (mode_issue_ids) {
          | [] => []
          | unsupported_ids => [
              Arms.Errors.of_ids(
                ~font_metrics=globals.font_metrics,
                ~syntax=editor.editor.syntax,
                unsupported_ids,
              ),
            ]
          };
        let source_editor =
          CodeWithStatics.Model.mk_from_exp(
            ~settings=globals.settings.core,
            ~root=Exp,
            ~parenthesization=Haz3lcore.ExpToSegment.Settings.Defensive,
            unboxed_selected_exp,
          );
        let source_mode_issue_overlay = mode_issue_overlay(source_editor);
        let target_mode_issue_overlay = mode_issue_overlay(editor);
        [
          // one element list with a div
          // with a list containing two elements
          // an Editor for user to propose their rewrite
          // a button to submit the rewrite
          div_c(
            "rewrite-box",
            [
              Node.text("From: "),
              CodeWithStatics.View.view(
                ~globals,
                ~overlays=source_mode_issue_overlay,
                source_editor,
              ),
              Node.text("Take a step to: "),
            ]
            @ algebrite_suggestion_controls
            @ [
              div_c(
                "inline-editor-wrapper",
                [
                  CodeEditable.View.view(
                    ~globals,
                    ~overlays=target_mode_issue_overlay,
                    ~signal=
                      fun
                      | MakeActive => signal(MakeActive(WriteStepEditor())),
                    ~edit_mode=
                      EditMode.Editable({
                        inject: x => {
                          inject(WriteStepEditorAction(x));
                        },
                        escape: _ => Ui_effect.Ignore,
                        take_focus: _ => Ui_effect.Ignore,
                        focus:
                          switch (selected) {
                          | Some(WriteStepEditor ()) => Some()
                          | _ => None
                          },
                        highlight: false,
                      }),
                    ~dynamics=Dynamics.Map.empty,
                    editor,
                  ),
                ],
              ),
            ]
            @ suggestions
            @ inactive_session_warning
            @ [
              div_c(
                "proof-search-status",
                switch (mode_issue) {
                | Some(message) => [mode_issue_view(message)]
                | None =>
                  switch (check_mode, proof_search_verdict, cached_result) {
                  | (Model.ProofSearch, Model.Planning | Model.Checking, _) => [
                      Node.text(
                        proof_search_message
                        |> Option.value(
                             ~default=
                               proof_search_verdict_label(
                                 ~has_candidate=false,
                                 proof_search_verdict,
                               ),
                           ),
                      ),
                      ...cancel_proof_search_button |> Option.to_list,
                    ]
                  | (Model.ProofSearch, Model.Ready, _) => [
                      Node.text(
                        proof_search_verdict_label(
                          ~has_candidate=proof_search_source |> Option.is_some,
                          proof_search_verdict,
                        ),
                      ),
                      run_proof_search_button,
                    ]
                  | (Model.ProofSearch, Model.Cancelled | Model.TimedOut, _) =>
                    [
                      Node.text(
                        proof_search_verdict_label(
                          ~has_candidate=false,
                          proof_search_verdict,
                        ),
                      ),
                    ]
                    @ (
                      proof_search_message
                      |> Option.map(message =>
                           div_c("proof-search-error", [Node.text(message)])
                         )
                      |> Option.to_list
                    )
                    @ [run_proof_search_button]
                  | (Model.ProofSearch, Model.EquivalentOutsideProfile, _) => [
                      Node.text(
                        proof_search_verdict_label(
                          ~has_candidate=false,
                          proof_search_verdict,
                        ),
                      ),
                      run_proof_search_button,
                    ]
                  | (
                      Model.ProofSearch,
                      Model.ProfileValid,
                      Some(Some(plan)),
                    ) => [
                      Node.text("Valid"),
                      div_c(
                        "proof-search-route",
                        [
                          Node.text(
                            "Profile route: "
                            ++ proof_search_route_label(plan.summary),
                          ),
                        ],
                      ),
                      replace_button(plan),
                    ]
                  | (Model.ProofSearch, Model.ProfileValid, _) => [
                      Node.text("Valid"),
                    ]
                  | (Model.ProofSearch, Model.Invalid, _) =>
                    [Node.text("Invalid")]
                    @ (
                      switch (proof_search_message) {
                      | Some(message) => [
                          div_c("proof-search-error", [Node.text(message)]),
                        ]
                      | None => []
                      }
                    )
                    @ [run_proof_search_button]
                  | (_, _, Some(Some(plan))) => [
                      Node.text("Valid"),
                      div_c(
                        "proof-search-route",
                        [
                          Node.text(
                            "Profile route: "
                            ++ proof_search_route_label(plan.summary),
                          ),
                        ],
                      ),
                      replace_button(plan),
                    ]
                  | (_, _, Some(None)) => [Node.text("Invalid")]
                  | (_, _, None) => [Node.text("...")]
                  }
                },
              ),
            ],
          ),
        ];
      };

      let unparenthesize_exp =
        switch (
          selected_id,
          model.selected_exp |> Calc.get_saved_exc(~print="Selected Exp"),
        ) {
        | (Some(id), Some({term: Parens(_), _})) =>
          Language.Reparenthesize.unparenthesize(
            ~selected_id=id,
            editor.statics.term,
          )
        | _ => None
        };
      let step_here_ids = [];
      let step_here_button: option((string, bool)) = None;
      let selected_exp_for_rewrite =
        Calc.get_saved_exc(
          ~print="selected exp not calculated",
          model.selected_exp,
        );
      let reparenthesize_result_for_rewrite:
        option(Language.Reparenthesize.result) =
        selection_override
        |> Option.bind(_, override =>
             SelectionEffective.reparenthesize_virtual(
               ~virtual_=override,
               ~full_exp=
                 model.full_visible_exp
                 |> Calc.get_saved_exc(~print="full_visible_exp"),
             )
           );
      let text_arrow = (b: bool) => if (b) {"▲"} else {"▼"};

      let show_manual_actions = automation_stage == Axioms.Manual;
      let show_check_actions =
        false
        && (
          automation_stage == Axioms.MultiStepCheck
          || globals.settings.core.evaluation.write_out_steps
        );
      let show_rewrite_actions =
        false && automation_stage == Axioms.MultiStepCheck;
      let show_auto_actions = automation_stage == Axioms.AutoEval;
      let show_proof_search_actions = automation_stage != Axioms.Manual;
      if (show_proof_search_actions) {
        warmup_rocq_search_in_browser();
      };
      if (show_auto_actions) {
        switch (model.open_box, selected_exp_for_rewrite) {
        | (
            Model.WrittenStepOpen({check_mode: Model.ProofSearch, _}),
            Some(selected_exp),
          ) =>
          let key =
            Axioms.rewrite_level_label(rewrite_level)
            ++ "|"
            ++ ProfileProofPlan.profile_fingerprint(
                 active_profile_for_model(model, rewrite_level),
                 automation_stage,
               )
            ++ "|"
            ++ Exp.show(selected_exp);
          AutoSimplifyDebounce.schedule(
            ~key,
            ~run=() => {
              let effect =
                if (auto_simplify_uses_profile(rewrite_level, selected_exp)) {
                  let env =
                    model.cached_env
                    |> Calc.get_saved_exc(~print="env not cached");
                  let profile =
                    active_profile_for_model(model, rewrite_level);
                  profile_suggestion_effect(
                    ~inject,
                    ~selected_exp,
                    ~profile,
                    ~settings=globals.settings.core,
                    ~env,
                    ~auto_selected_exp=Some(selected_exp),
                  );
                } else {
                  algebrite_suggestion_effect(
                    ~inject,
                    ~selected_exp,
                    ~method_name="simplifyToString",
                    ~source="Algebrite auto simplify candidate",
                    ~auto_selected_exp=Some(selected_exp),
                  );
                };
              Ui_effect.Expert.handle(effect);
            },
          );
        | _ => AutoSimplifyDebounce.cancel(~reset_key=true, ())
        };
      } else {
        AutoSimplifyDebounce.cancel(~reset_key=true, ());
      };

      let unparenthesize_action_buttons =
        switch (unparenthesize_exp) {
        | None => []
        | Some(exp) => [
            proof_button(
              ~callback=signal(AddReparenthesizeStep(exp)),
              "Unparenthesize",
            ),
          ]
        };

      let manual_action_buttons =
        show_manual_actions
          ? [
              proof_button(
                ~callback=
                  inject(
                    ProposeWrittenStep(
                      Model.SingleEvalStep,
                      selected_exp_for_rewrite,
                      switch (reparenthesize_result_for_rewrite) {
                      | Some(result) => Some(result.exp)
                      | None => None
                      },
                    ),
                  ),
                "One Step "
                ++ text_arrow(
                     switch (model.open_box) {
                     | Model.WrittenStepOpen({
                         check_mode: Model.SingleEvalStep,
                         _,
                       }) =>
                       true
                     | _ => false
                     },
                   ),
              ),
            ]
            @ (
              switch (step_here_button) {
              | None => []
              | Some((label, evaluate_after_parenthesize)) => [
                  proof_button(
                    ~callback=
                      signal(
                        StepHere(step_here_ids, evaluate_after_parenthesize),
                      ),
                    label,
                  ),
                ]
              }
            )
            @ (
              switch (show_step_button) {
              | None => []
              | Some(idx) => [
                  proof_button(
                    ~callback=Ui_effect.Many([signal(TakeStep(idx))]),
                    "Step",
                  ),
                ]
              }
            )
            @ (
              switch (show_refl_button) {
              | None => []
              | Some(idx) => [
                  proof_button(
                    ~callback=
                      Ui_effect.Many([
                        globals.inject_global(
                          Set(Evaluation(ForceShowRecord)),
                        ),
                        signal(Refl(idx)),
                      ]),
                    "Reflexivity",
                  ),
                ]
              }
            )
            @ (
              show_function_body_button
                ? [
                  proof_button(
                    ~callback=
                      Ui_effect.Many([
                        globals.inject_global(
                          Set(Evaluation(ForceShowRecord)),
                        ),
                        signal(AddForall),
                      ]),
                    "Function Body",
                  ),
                ]
                : []
            )
          : [];

      let check_action_buttons =
        show_check_actions
          ? [
            proof_button(
              ~callback=
                inject(
                  ProposeWrittenStep(
                    Model.CheckResult,
                    selected_exp_for_rewrite,
                    switch (reparenthesize_result_for_rewrite) {
                    | Some(result) => Some(result.exp)
                    | None => None
                    },
                  ),
                ),
              "Check Result "
              ++ text_arrow(
                   switch (model.open_box) {
                   | Model.WrittenStepOpen({check_mode: Model.CheckResult, _}) =>
                     true
                   | _ => false
                   },
                 ),
            ),
          ]
          : [];

      let proof_search_action_buttons =
        show_proof_search_actions
          ? [
            proof_button(
              ~callback=
                inject(
                  ProposeWrittenStep(
                    Model.ProofSearch,
                    selected_exp_for_rewrite,
                    switch (reparenthesize_result_for_rewrite) {
                    | Some(result) => Some(result.exp)
                    | None => None
                    },
                  ),
                ),
              "Search "
              ++ text_arrow(
                   switch (model.open_box) {
                   | Model.WrittenStepOpen({check_mode: Model.ProofSearch, _}) =>
                     true
                   | _ => false
                   },
                 ),
            ),
          ]
          : [];

      let rewrite_action_buttons =
        show_rewrite_actions
          ? [
            proof_button(
              ~callback=
                inject(
                  ProposeRewrite(
                    selected_exp_for_rewrite,
                    switch (reparenthesize_result_for_rewrite) {
                    | Some(result) => Some(result.exp)
                    | None => None
                    },
                  ),
                ),
              "Rewrite "
              ++ text_arrow(
                   switch (model.open_box) {
                   | Model.RewritesOpen(_) => true
                   | _ => false
                   },
                 ),
            ),
          ]
          : [];

      let general_proof_buttons = [
        proof_button(
          ~callback=
            Ui_effect.Many([
              globals.inject_global(Set(Evaluation(ForceShowRecord))),
              signal(
                AddInduction(
                  model.selected_exp
                  |> Calc.get_saved_exc(~print="Selected Exp"),
                ),
              ),
            ]),
          "Cases/Induction",
        ),
      ];

      let buttons =
        Node.div(
          ~attrs=[Attr.classes(["proof-selection-buttons"])],
          unparenthesize_action_buttons
          @ manual_action_buttons
          @ check_action_buttons
          @ proof_search_action_buttons
          @ rewrite_action_buttons
          @ general_proof_buttons,
        );

      [
        Node.div(
          ~attrs=[
            Attr.classes(["missing-step-overlay-align"]),
            DecUtil.position(
              ~width=right - left,
              ~height=bottom - top,
              ~font_metrics=globals.font_metrics,
              Point.{
                col: left,
                row: top,
              },
            ),
          ],
          [
            Node.div(
              ~attrs=[
                Attr.class_("proof-context-box"),
                Attr.on_pointerdown(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
                Attr.on_pointerup(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
                Attr.on_mousemove(_ =>
                  Virtual_dom.Vdom.Effect.Stop_propagation
                ),
              ],
              (
                !globals.settings.core.evaluation.enable_proof
                && globals.settings.core.evaluation.write_out_steps
                  ? [] : [buttons]
              )
              @ {
                switch (model.open_box) {
                | NoneOpen => []
                | AxiomsOpen({
                    axioms_model: m,
                    rewrite_selected_exp,
                    rewrite_reparenthesized_exp,
                    source_full_visible_exp: _,
                  }) =>
                  let selected_exp_for_axioms =
                    switch (rewrite_selected_exp) {
                    | Some(exp) => exp
                    | None =>
                      model.selected_exp
                      |> Calc.get_saved_exc(~print="Selected Exp")
                      |> Option.value(~default=EmptyHole |> Exp.fresh, _)
                    };
                  let full_exp_for_axioms =
                    switch (rewrite_reparenthesized_exp) {
                    | Some(exp) => exp
                    | None =>
                      model.full_visible_exp
                      |> Calc.get_saved_exc(
                           ~print="full_visible_exp not cached",
                         )
                    };
                  [
                    div_c(
                      "axiom-box",
                      AxiomsBox.View.view(
                        ~globals,
                        ~info_map,
                        ~env=
                          model.cached_env
                          |> Calc.get_saved_exc(~print="env not cached"),
                        ~inject=
                          (a: AxiomsBox.Update.t) =>
                            inject(AxiomBoxAction(a)),
                        ~take_focus=
                          (s: AxiomsBox.Selection.t) =>
                            signal(MakeActive(AxiomBoxSelection(s))),
                        ~add_axiom_step=
                          (a, b, c, d, e) =>
                            add_axiom_step_for_selection(
                              rewrite_reparenthesized_exp,
                              a,
                              b,
                              c,
                              d,
                              e,
                            ),
                        ~add_written_step=
                          (summary, at_idx, at_exp, with_exp) =>
                            add_written_step_for_selection(
                              summary,
                              rewrite_reparenthesized_exp,
                              at_idx,
                              at_exp,
                              with_exp,
                            ),
                        ~profile=
                          active_profile_for_model(model, rewrite_level),
                        ~rewrite_level,
                        ~show_mode_warning=true,
                        ~full_exp=full_exp_for_axioms,
                        ~selected_exp=selected_exp_for_axioms,
                        m,
                      ),
                    ),
                  ];
                | RewritesOpen({
                    editor,
                    rewrite_selected_exp,
                    rewrite_reparenthesized_exp,
                    source_full_visible_exp: _,
                    cached_exp,
                    cached_result,
                  }) =>
                  view_rewrites_box(
                    editor,
                    rewrite_selected_exp,
                    rewrite_reparenthesized_exp,
                    cached_exp,
                    cached_result |> Calc.saved_to_option,
                  )
                | WrittenStepOpen({
                    editor,
                    check_mode,
                    axioms_model,
                    rewrite_selected_exp,
                    rewrite_reparenthesized_exp,
                    source_full_visible_exp: _,
                    proof_search_requested: _,
                    proof_search_verdict,
                    proof_search_check_id,
                    proof_search_message,
                    proof_search_max_depth: _,
                    proof_search_max_states: _,
                    proof_search_source,
                    calculated_rewrite_level: _,
                    calculated_automation_stage: _,
                    cached_exp,
                    cached_result,
                  }) =>
                  let live_rewrite_reparenthesized_exp =
                    switch (reparenthesize_result_for_rewrite) {
                    | Some(result) => Some(result.exp)
                    | None => None
                    };
                  let option_exp_equal = (a, b) =>
                    switch (a, b) {
                    | (None, None) => true
                    | (Some(a), Some(b)) =>
                      Equality.ignoring_ascriptions.exp(a, b)
                    | _ => false
                    };
                  let reparenthesized_source_changed =
                    switch (
                      rewrite_reparenthesized_exp,
                      live_rewrite_reparenthesized_exp,
                    ) {
                    /* Losing the range because the mini-editor took focus is
                     * not a source edit.  The open box also tracks the full
                     * source expression and is closed separately if it changes. */
                    | (Some(_), None) => false
                    | (stored, live) => !option_exp_equal(stored, live)
                    };
                  let source_selection_changed =
                    !
                      option_exp_equal(
                        rewrite_selected_exp,
                        selected_exp_for_rewrite,
                      )
                    || reparenthesized_source_changed;
                  let suggestions =
                    switch (
                      check_mode,
                      globals.settings.core.evaluation.suggest_rewrites,
                    ) {
                    | (Model.ProofSearch, true) =>
                      let selected_exp_for_axioms =
                        switch (selected_exp_for_rewrite) {
                        | Some(exp) => exp
                        | None =>
                          model.selected_exp
                          |> Calc.get_saved_exc(~print="Selected Exp")
                          |> Option.value(~default=EmptyHole |> Exp.fresh, _)
                        };
                      let full_exp_for_axioms =
                        switch (live_rewrite_reparenthesized_exp) {
                        | Some(exp) => exp
                        | None =>
                          model.full_visible_exp
                          |> Calc.get_saved_exc(
                               ~print="full_visible_exp not cached",
                             )
                        };
                      [
                        div_c(
                          "proof-search-suggestions",
                          AxiomsBox.View.view(
                            ~globals,
                            ~info_map,
                            ~env=
                              model.cached_env
                              |> Calc.get_saved_exc(~print="env not cached"),
                            ~inject=
                              (a: AxiomsBox.Update.t) =>
                                inject(AxiomBoxAction(a)),
                            ~take_focus=
                              (s: AxiomsBox.Selection.t) =>
                                signal(MakeActive(AxiomBoxSelection(s))),
                            ~add_axiom_step=
                              (a, b, c, d, e) =>
                                add_axiom_step_for_selection(
                                  live_rewrite_reparenthesized_exp,
                                  a,
                                  b,
                                  c,
                                  d,
                                  e,
                                ),
                            ~add_written_step=
                              (summary, at_idx, at_exp, with_exp) =>
                                add_written_step_for_selection(
                                  summary,
                                  rewrite_reparenthesized_exp,
                                  at_idx,
                                  at_exp,
                                  with_exp,
                                ),
                            ~profile=
                              active_profile_for_model(model, rewrite_level),
                            ~rewrite_level,
                            ~show_mode_warning=false,
                            ~full_exp=full_exp_for_axioms,
                            ~selected_exp=selected_exp_for_axioms,
                            axioms_model,
                          ),
                        ),
                      ];
                    | (_, _) => []
                    };
                  view_written_step_box(
                    editor,
                    check_mode,
                    selected_exp_for_rewrite,
                    rewrite_reparenthesized_exp,
                    source_selection_changed
                      ? Model.Ready : proof_search_verdict,
                    source_selection_changed ? None : proof_search_check_id,
                    source_selection_changed ? None : proof_search_message,
                    source_selection_changed ? None : proof_search_source,
                    cached_exp,
                    source_selection_changed
                      ? None : cached_result |> Calc.saved_to_option,
                    suggestions,
                  );
                };
              },
            ),
          ],
        ),
      ];
    }
    |> Option.value(~default=[]);

  let view_justification =
      (
        ~globals: Globals.t,
        ~hide_stepper: Ui_effect.t(unit),
        ~undo: option(Ui_effect.t(unit)),
        ~is_toplevel: bool,
        _model: Model.t,
      ) => {
    let button_back =
      Widgets.button_d(
        Icons.undo,
        switch (undo) {
        | Some(u) => u
        | None => Ui_effect.Ignore
        },
        ~disabled=Option.is_none(undo),
        ~tooltip="Step Backwards",
      );
    let button_hide_stepper =
      Widgets.toggle(
        ~tooltip="Show Stepper",
        "s",
        true,
        _ => {
          cancel_proof_searches_except_in_browser(None);
          hide_stepper;
        },
      );
    let toggle_show_history =
      Widgets.toggle(
        ~tooltip="Show History",
        "h",
        globals.settings.core.evaluation.stepper_history,
        _ =>
        globals.inject_global(Set(Evaluation(ShowRecord)))
      );
    let eval_settings =
      Widgets.button(Icons.gear, _ =>
        globals.inject_global(Set(Evaluation(ShowSettings)))
      );
    Node.div(
      ~attrs=[Attr.classes(["stepper-controls"])],
      [button_back]
      @ (
        is_toplevel
          ? [eval_settings, toggle_show_history, button_hide_stepper] : []
      ),
    );
  };
};
