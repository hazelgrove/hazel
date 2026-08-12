open Language;
open Util;

type candidate_origin =
  | UserEntered
  | AutomaticSimplify
  | DisplayedSuggestion;

[@deriving (show({with_path: false}), sexp, yojson)]
type certificate_strategy =
  | LemmaReplay
  | AffineCertificate
  | PolynomialCertificate
  | TrigonometricCertificate
  | DerivativeCertificate
  | EvaluationEvidence
  | UntrustedSessionRewrite;

type request = {
  profile: Axioms.math_profile,
  stage: Axioms.automation_stage,
  candidate_origin,
  settings: CoreSettings.t,
  env: Environment.t(Exp.t),
  source: Exp.t,
  target: Exp.t,
  max_depth: int,
  max_states: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type authorized_plan = {
  stage: Axioms.automation_stage,
  source: Exp.t,
  target: Exp.t,
  summary: ProofTrace.trace_summary,
  capability_ids: list(string),
  capability_use_counts: list((string, int)),
  certificate_strategy,
  exportable: bool,
  profile_fingerprint: string,
};

type profile_rejection =
  | UnsupportedSyntax(string)
  | InvalidProfileConfiguration(Axioms.profile_configuration_error)
  | NoSemanticRoute
  | DisabledCapability(string)
  | SearchBudgetExhausted
  | InvalidEvidence(string)
  | NonExportableEvaluation;

type result =
  | Authorized(authorized_plan)
  | Rejected(profile_rejection);

let rejection_message =
  fun
  | UnsupportedSyntax(message) => "unsupported syntax: " ++ message
  | InvalidProfileConfiguration(error) =>
    Axioms.profile_configuration_error_message(error)
  | NoSemanticRoute => "no Profile-authorized semantic route"
  | DisabledCapability(message) => message
  | SearchBudgetExhausted => "bounded proof search exhausted its budget"
  | InvalidEvidence(message) => "invalid proof evidence: " ++ message
  | NonExportableEvaluation => "the evaluator step is not exportable";

let authorization_stage =
  fun
  | Axioms.AutoEval => Axioms.MultiStepCheck
  | stage => stage;

let usage_fingerprint =
  fun
  | Axioms.Disabled => "0"
  | AtMostOne => "1"
  | BoundedClosure({max_uses, max_states, cost}) =>
    "n:"
    ++ string_of_int(max_uses)
    ++ ":"
    ++ string_of_int(max_states)
    ++ ":"
    ++ string_of_int(cost);

let profile_fingerprint = (profile, stage) => {
  let plan =
    Axioms.stage_plan_for_profile(profile, authorization_stage(stage));
  let capabilities =
    plan.capabilities
    |> List.map((capability: Axioms.compiled_capability) =>
         capability.id ++ "=" ++ usage_fingerprint(capability.usage)
       )
    |> List.sort(String.compare)
    |> String.concat(",");
  let directions =
    plan.visible_rules
    |> List.map((planned: Axioms.planned_visible_rule) =>
         planned.rule.id
         ++ "="
         ++ Axioms.show_math_rule_direction(planned.direction)
       )
    |> List.sort(String.compare)
    |> String.concat(",");
  let cleanup =
    profile.step_policy.default_cleanup
    |> List.map(Axioms.cleanup_capability_label)
    |> List.sort(String.compare)
    |> String.concat(",");
  let session_rewrites =
    Axioms.session_rewrites_for_profile(profile)
    |> List.map((rewrite: Axioms.session_rewrite) =>
         rewrite.id
         ++ "="
         ++ rewrite.source_pattern
         ++ "->"
         ++ rewrite.target_pattern
         ++ ":"
         ++ Axioms.show_math_rule_direction(rewrite.direction)
       )
    |> List.sort(String.compare)
    |> String.concat(",");
  Axioms.rewrite_level_label(profile.level)
  ++ "|"
  ++ Axioms.automation_stage_label(plan.stage)
  ++ "|"
  ++ capabilities
  ++ "|"
  ++ directions
  ++ "|"
  ++ cleanup
  ++ "|"
  ++ session_rewrites;
};

let increment_count = (counts, id) => {
  let previous =
    counts
    |> List.find_opt(((candidate, _count)) => candidate == id)
    |> Option.map(((_id, count)) => count)
    |> Option.value(~default=0);
  [(id, previous + 1), ...counts |> List.remove_assoc(id)];
};

/* Some visible catalog rules also have cleanup aliases (for example,
 * [alg.collect_like_terms] versus [collect.like_terms]).  Preserve the
 * catalog identity when the evidence records an explicit visible rule; only
 * canonicalize actual cleanup aliases.  Otherwise disabling automatic cleanup
 * would incorrectly disable the corresponding student-visible One Step rule. */
let canonical_capability_id = (~visible_rule_ids, id) =>
  if (List.mem(id, visible_rule_ids)) {
    id;
  } else {
    switch (Axioms.cleanup_capability_for_id(id)) {
    | Some(capability) => Axioms.cleanup_capability_label(capability)
    | None => id
    };
  };

let capability_use_counts =
    (~visible_rule_ids, summary: ProofTrace.trace_summary) => {
  let from_steps =
    summary.prover_steps
    |> List.fold_left(
         (counts, step: ProofTrace.prover_step) =>
           increment_count(
             counts,
             canonical_capability_id(~visible_rule_ids, step.rule_id),
           ),
         [],
       );
  summary.rule_ids
  |> List.fold_left(
       (counts, raw_id) => {
         let id = canonical_capability_id(~visible_rule_ids, raw_id);
         List.mem_assoc(id, counts) ? counts : [(id, 1), ...counts];
       },
       from_steps,
     )
  |> List.rev;
};

let exp_same = (left, right) => TrigRewrite.exp_same(left, right);

let root_product_has_sum_factor = exp =>
  switch (RewriteChecker.strip_math_wrappers(exp).term) {
  | BinOp(op, _, _) when RewriteChecker.is_times_op(op) =>
    RewriteChecker.product_has_sum_factor(exp)
  | _ => false
  };

let rec steps_are_contiguous =
  fun
  | []
  | [_] => true
  | [left, right, ...rest] =>
    exp_same(left.ProofTrace.after_full_exp, right.ProofTrace.before_full_exp)
    && steps_are_contiguous([right, ...rest]);

let endpoints_match = (request: request, summary) =>
  switch (summary.ProofTrace.prover_steps) {
  | [] => false
  | steps =>
    let first: ProofTrace.prover_step = List.hd(steps);
    let last: ProofTrace.prover_step = List.hd(List.rev(steps));
    exp_same(first.before_full_exp, request.source)
    && exp_same(last.after_full_exp, request.target);
  };

/* Older polynomial checkers store two normalization legs (source -> normal
   form and target -> normal form), which are useful diagnostics but are not a
   contiguous replay plan.  During the compatibility phase, collapse only
   catalogued macro operations with an enabled, precise semantic contract.
   This conversion does not consult Rocq and therefore cannot turn tactic
   success into authorization. */
let contiguous_legacy_summary =
    (request: request, summary: ProofTrace.trace_summary) =>
  if (endpoints_match(request, summary)
      && steps_are_contiguous(summary.prover_steps)) {
    summary;
  } else {
    let macro_rule_id =
      if (summary.justification == "algebra"
          && (
            List.mem("alg.factor_common", summary.to_rule_ids)
            || !root_product_has_sum_factor(request.source)
            && root_product_has_sum_factor(request.target)
          )) {
        Some("alg.factor_polynomial_normalize");
      } else if ((
                   summary.justification == "algebra"
                   || summary.justification == "algebra one step"
                 )
                 && (
                   List.mem("alg.expand_polynomial", summary.from_rule_ids)
                   || List.mem("alg.expand_polynomial", summary.to_rule_ids)
                   || List.mem("alg.expand_polynomial", summary.rule_ids)
                 )) {
        Some("alg.expand_polynomial");
      } else {
        None;
      };
    switch (macro_rule_id) {
    | None => summary
    | Some(rule_id) =>
      let recorded_rule_ids =
        rule_id == "alg.factor_polynomial_normalize"
          ? [rule_id] : RewriteChecker.dedup([rule_id, ...summary.rule_ids]);
      /* The compact witness represents the complete polynomial transition,
         even when collecting terms was the final normalization capability.
         Recording only that final cleanup incorrectly selects a linear
         certificate for transitions that expand a power or product. */
      let evidence_rule_id = rule_id;
      {
        ...summary,
        justification:
          rule_id == "alg.factor_polynomial_normalize"
            ? "factor polynomial normalization" : "polynomial expansion",
        from_normal_exp: request.target,
        to_normal_exp: request.target,
        from_rule_ids: recorded_rule_ids,
        to_rule_ids: [],
        rule_ids: recorded_rule_ids,
        prover_steps: [
          ProofTrace.prover_step(
            ~origin=ProofTrace.Normalization,
            ~rule_id=evidence_rule_id,
            ~before_full_exp=request.source,
            ~after_full_exp=request.target,
            ~before_exp=request.source,
            ~after_exp=request.target,
            ~detail="catalog-authorized compact polynomial witness",
          ),
        ],
      };
    };
  };

let certificate_strategy = (summary: ProofTrace.trace_summary) => {
  let has_prefix = prefix =>
    summary.rule_ids
    |> List.exists(id => {
         let length = String.length(prefix);
         String.length(id) >= length && String.sub(id, 0, length) == prefix;
       });
  summary.rule_ids |> List.exists(SessionRewrite.is_session_rule_id)
    ? UntrustedSessionRewrite
    : summary.prover_steps
      |> List.exists((step: ProofTrace.prover_step) =>
           step.origin == ProofTrace.AutoEvaluation
         )
        ? EvaluationEvidence
        : has_prefix("calc.")
            ? DerivativeCertificate
            : has_prefix("trig.")
                ? TrigonometricCertificate
                : List.mem("arith.affine_normalize", summary.rule_ids)
                    ? AffineCertificate
                    : has_prefix("alg.") ? PolynomialCertificate : LemmaReplay;
};

let validate_summary = (request: request, summary) => {
  let summary = contiguous_legacy_summary(request, summary);
  let stage = authorization_stage(request.stage);
  let stage_plan = Axioms.stage_plan_for_profile(request.profile, stage);
  let visible_rule_ids =
    stage_plan.visible_rules
    |> List.map((planned: Axioms.planned_visible_rule) => planned.rule.id);
  let counts = capability_use_counts(~visible_rule_ids, summary);
  let direction_allows = (allowed, actual) =>
    allowed == Axioms.BothDirections || allowed == actual;
  let invalid_direction =
    summary.ProofTrace.prover_steps
    |> List.find_map((step: ProofTrace.prover_step) =>
         if (!TrigRewrite.is_trig_rule_id(step.rule_id)) {
           None;
         } else {
           let planned =
             stage_plan.visible_rules
             |> List.find_opt((candidate: Axioms.planned_visible_rule) =>
                  candidate.rule.id == step.rule_id
                );
           switch (
             planned,
             TrigRewrite.transition_direction(
               step.rule_id,
               step.before_exp,
               step.after_exp,
             ),
           ) {
           | (Some(planned), Some(actual))
               when direction_allows(planned.direction, actual) =>
             None
           | _ => Some(step.rule_id)
           };
         }
       );
  let session_rewrite_for_id = id =>
    Axioms.session_rewrites_for_profile(request.profile)
    |> List.find_opt((rewrite: Axioms.session_rewrite) => rewrite.id == id);
  let invalid_session_direction =
    summary.ProofTrace.prover_steps
    |> List.find_map((step: ProofTrace.prover_step) =>
         switch (session_rewrite_for_id(step.rule_id)) {
         | None =>
           SessionRewrite.is_session_rule_id(step.rule_id)
             ? Some(step.rule_id) : None
         | Some(definition) =>
           switch (
             SessionRewrite.transition_direction(
               definition,
               step.before_exp,
               step.after_exp,
             )
           ) {
           | Some(actual) when direction_allows(definition.direction, actual) =>
             None
           | _ => Some(step.rule_id)
           }
         }
       );
  let (session_counts, trusted_counts) =
    counts
    |> List.partition(((id, _count)) => session_rewrite_for_id(id) != None);
  let foreground_rule_ids =
    summary.ProofTrace.prover_steps
    |> List.map((step: ProofTrace.prover_step) => step.rule_id);
  if (summary.ProofTrace.prover_steps == []) {
    Error(InvalidEvidence("an authorized plan must contain evidence steps"));
  } else if (!endpoints_match(request, summary)) {
    Error(InvalidEvidence("proof-plan endpoints do not match the request"));
  } else if (!steps_are_contiguous(summary.prover_steps)) {
    Error(InvalidEvidence("proof-plan steps are not contiguous"));
  } else if (invalid_direction != None) {
    Error(
      InvalidEvidence(
        "proof-plan evidence uses a disallowed rewrite direction: "
        ++ Option.get(invalid_direction),
      ),
    );
  } else if (invalid_session_direction != None) {
    Error(
      InvalidEvidence(
        "proof-plan evidence uses an unavailable or disallowed session rewrite: "
        ++ Option.get(invalid_session_direction),
      ),
    );
  } else if (session_counts != []) {
    let valid_session_step =
      stage == Axioms.Manual
      && trusted_counts == []
      && List.length(session_counts) == 1
      && session_counts
      |> List.for_all(((_id, count)) => count == 1);
    valid_session_step
      ? Ok({
          stage,
          source: request.source,
          target: request.target,
          summary: {
            ...summary,
            exportable: false,
          },
          capability_ids: session_counts |> List.map(((id, _count)) => id),
          capability_use_counts: session_counts,
          certificate_strategy: UntrustedSessionRewrite,
          exportable: false,
          profile_fingerprint: profile_fingerprint(request.profile, stage),
        })
      : Error(
          DisabledCapability(
            "untrusted session rewrites are limited to one manual One Step transition",
          ),
        );
  } else {
    switch (
      Axioms.validate_foreground_rule_uses(stage_plan, foreground_rule_ids),
      Axioms.validate_capability_use_counts(stage_plan, counts),
    ) {
    | (Some(message), _) => Error(DisabledCapability(message))
    | (None, Some(message)) => Error(DisabledCapability(message))
    | (None, None) =>
      Ok({
        stage,
        source: request.source,
        target: request.target,
        summary,
        capability_ids: counts |> List.map(((id, _count)) => id),
        capability_use_counts: counts,
        certificate_strategy: certificate_strategy(summary),
        exportable: summary.exportable,
        profile_fingerprint: profile_fingerprint(request.profile, stage),
      })
    };
  };
};

let direct_candidate_trace = (request: request) => {
  let stage = authorization_stage(request.stage);
  switch (
    Axioms.unsupported_constructs_message_for_rewrite(
      ~level=request.profile.level,
      ~source=request.source,
      ~target=request.target,
    )
  ) {
  | Some(message) => Error(UnsupportedSyntax(message))
  | None =>
    let direct =
      switch (stage) {
      | Axioms.Manual =>
        RewriteChecker.check_single_step_result_for_profile(
          ~profile=request.profile,
          ~settings=request.settings,
          ~env=request.env,
          request.source,
          request.target,
        )
        |> Option.map(RewriteChecker.trace_summary_of_result)
      | MultiStepCheck
      | AutoEval =>
        switch (
          RewriteChecker.calculus_check_result_trace_for_profile(
            ~profile=request.profile,
            request.source,
            request.target,
          )
        ) {
        | Some(summary) => Some(summary)
        | None =>
          RewriteChecker.check_written_step_trace_for_profile(
            ~stage,
            ~profile=request.profile,
            ~settings=request.settings,
            ~env=request.env,
            request.source,
            request.target,
          )
        }
      };
    Ok(direct);
  };
};

let authorize_summary = (request, summary) =>
  switch (validate_summary(request, summary)) {
  | Ok(plan) => Authorized(plan)
  | Error(rejection) => Rejected(rejection)
  };

let axiom_search_progress = (request: request) => {
  let stage = authorization_stage(request.stage);
  let stage_plan = Axioms.stage_plan_for_profile(request.profile, stage);
  let search_rule_id = capability_id =>
    switch (Axioms.cleanup_capability_for_id(capability_id)) {
    | Some(cleanup)
        when capability_id == Axioms.cleanup_capability_label(cleanup) =>
      Axioms.cleanup_capability_label(cleanup)
    | Some(_)
    | None => capability_id
    };
  /* A cleanup-backed visible rule remains available as an explicit student
     step when automatic cleanup is disabled.  It must not, however, re-enter
     through background search and silently restore the disabled cleanup. */
  let capability_available_to_search =
      (capability: Axioms.compiled_capability) => {
    let cleanup_enabled =
      switch (Axioms.cleanup_capability_for_id(capability.id)) {
      | Some(cleanup)
          when capability.id != Axioms.cleanup_capability_label(cleanup) =>
        List.mem(cleanup, stage_plan.pre_cleanup)
      | Some(_)
      | None => true
      };
    cleanup_enabled
    && capability.usage != Axioms.Disabled
    && Axioms.compiled_capability_requirements_satisfied(
         stage_plan,
         capability,
       );
  };
  let allowed_rule_ids =
    stage_plan.capabilities
    |> List.filter(capability_available_to_search)
    |> List.map((capability: Axioms.compiled_capability) =>
         search_rule_id(capability.id)
       );
  let rule_use_limits =
    stage_plan.capabilities
    |> List.filter_map((capability: Axioms.compiled_capability) =>
         if (!capability_available_to_search(capability)) {
           None;
         } else {
           switch (capability.usage) {
           | Axioms.Disabled => None
           | AtMostOne => Some((search_rule_id(capability.id), 1))
           | BoundedClosure({max_uses, _}) =>
             let max_uses =
               switch (
                 Axioms.repeated_rule_composite_capability(capability.id)
               ) {
               | Some(composite_id)
                   when
                     !
                       Axioms.compiled_capability_enabled(
                         stage_plan,
                         composite_id,
                       ) =>
                 min(1, max_uses)
               | Some(_)
               | None => max_uses
               };
             Some((search_rule_id(capability.id), max_uses));
           };
         }
       );
  AxiomSearch.start_search(
    ~level=request.profile.level,
    ~max_depth=request.max_depth,
    ~max_states=
      Axioms.compiled_search_state_limit(
        ~requested=request.max_states,
        stage_plan,
      ),
    ~allowed_rule_ids,
    ~rule_use_limits,
    ~foreground_rule_ids=stage_plan.foreground_rule_ids,
    ~max_foreground_uses=stage == Axioms.Manual ? 1 : (-1),
    request.source,
    request.target,
  );
};

type planning_progress =
  | PlanningSearch(request, AxiomSearch.search_progress)
  | PlanningComplete(result);

let finish_axiom_progress = (request, progress) =>
  switch (progress) {
  | AxiomSearch.SearchComplete(Some(search_result)) =>
    PlanningComplete(
      authorize_summary(request, AxiomSearch.trace_summary(search_result)),
    )
  | SearchComplete(None) => PlanningComplete(Rejected(NoSemanticRoute))
  | SearchPending(_) => PlanningSearch(request, progress)
  };

let start_authorize = (request: request) =>
  switch (Axioms.validate_profile_configuration(request.profile)) {
  | Some(error) =>
    PlanningComplete(Rejected(InvalidProfileConfiguration(error)))
  | None =>
    switch (direct_candidate_trace(request)) {
    | Error(rejection) => PlanningComplete(Rejected(rejection))
    | Ok(Some(summary)) =>
      PlanningComplete(authorize_summary(request, summary))
    | Ok(None) =>
      finish_axiom_progress(request, axiom_search_progress(request))
    }
  };

let continue_authorize = (~work_budget=1, progress) =>
  switch (progress) {
  | PlanningComplete(_) => progress
  | PlanningSearch(request, search_progress) =>
    AxiomSearch.continue_search(~work_budget, search_progress)
    |> finish_axiom_progress(request)
  };

let authorize = request => {
  let rec finish = progress =>
    switch (continue_authorize(~work_budget=request.max_states, progress)) {
    | PlanningComplete(result) => result
    | PlanningSearch(_, _) as progress => finish(progress)
    };
  start_authorize(request) |> finish;
};

let authorized_plan =
  fun
  | Authorized(plan) => Some(plan)
  | Rejected(_) => None;
