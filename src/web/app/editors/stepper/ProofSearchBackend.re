open Language;

type backend =
  | LocalAxiomSearch
  | JSCoqTacticSearch;

type request = {
  backend,
  level: Axioms.rewrite_level,
  max_depth: int,
  max_states: int,
  source: Exp.t,
  target: Exp.t,
};

type outcome =
  | PrimitiveTrace(RewriteChecker.trace_summary)
  | CollapsedMacro(RewriteChecker.trace_summary)
  | Rejected(string);

let local_axiom_search = request =>
  AxiomSearch.search(
    ~level=request.level,
    ~max_depth=request.max_depth,
    ~max_states=request.max_states,
    request.source,
    request.target,
  )
  |> Option.map(AxiomSearch.trace_summary)
  |> Option.map(summary => PrimitiveTrace(summary))
  |> Option.value(~default=Rejected("local axiom search found no proof"));

let effective_profile_for_request = request =>
  Axioms.effective_profile_for_rewrite(
    ~requested_level=request.level,
    request.source,
    request.target,
  );

let domain_for_request = request => {
  let profile = effective_profile_for_request(request);
  switch (profile.rocq_domain_policy) {
  | Axioms.RealsByDefault => CoqExport.Reals
  | IntegersByDefault =>
    CoqExport.requires_reals(request.source)
    || CoqExport.requires_reals(request.target)
      ? CoqExport.Reals : CoqExport.Integers
  };
};

let vars_for_request = request =>
  CoqExport.unique_vars_in_ast(request.source)
  @ CoqExport.unique_vars_in_ast(request.target)
  |> RewriteChecker.dedup;

let forall_string = (~domain, vars) =>
  switch (vars) {
  | [] => ""
  | vars =>
    let typ =
      switch (domain) {
      | CoqExport.Reals => "R"
      | Integers => "Z"
      };
    "forall " ++ String.concat(" ", vars) ++ " : " ++ typ ++ ",";
  };

let tactic_plan_purpose_for_automation_stage = Axioms.tactic_plan_purpose_for_stage;

let effective_level_for_request = request =>
  effective_profile_for_request(request).level;

let rocq_tactic_step_script = (step: Axioms.rocq_tactic_step) =>
  switch (step.mode) {
  | Axioms.Once => step.tactic
  | TryOnce => "try " ++ step.tactic
  | RepeatUntilStuck => "repeat progress " ++ step.tactic
  | RepeatFuel(fuel) =>
    "hazel_repeat_fuel " ++ string_of_int(fuel) ++ "%nat " ++ step.tactic
  | FinishOnly => "try solve [" ++ step.tactic ++ "]"
  };

let rocq_tactic_plan_script = (plan: Axioms.rocq_tactic_plan) =>
  plan.steps
  |> List.map(rocq_tactic_step_script)
  |> (scripts => scripts @ ["reflexivity"])
  |> String.concat("; ");

let rocq_plan_for_profile_and_purpose = (profile, purpose) =>
  switch (purpose) {
  | Axioms.ValidatePrimitiveStep =>
    Axioms.stage_plan_for_profile(profile, Manual).rocq_plan
  | CheckResult =>
    Axioms.stage_plan_for_profile(profile, MultiStepCheck).rocq_plan
  | AutoSimplify => Axioms.stage_plan_for_profile(profile, AutoEval).rocq_plan
  | ValidateMacroStep => Axioms.rocq_tactic_plan_for_profile(profile, purpose)
  };

let catalog_domain =
  fun
  | CoqExport.Integers => Axioms.RocqIntegers
  | Reals => RocqReals;

let rocq_backend_tactics = (~domain, backend: Axioms.rocq_rule_backend) =>
  Axioms.rocq_tactics_for_domain(
    ~domain=catalog_domain(domain),
    backend.search_tactics,
  );

let tactic_alternatives = (~name, tactics) =>
  switch (tactics |> RewriteChecker.dedup) {
  | [] => "Ltac " ++ name ++ " := fail 0 \"no enabled rules\".\n"
  | tactics =>
    "Ltac "
    ++ name
    ++ " :=\n  first [\n    "
    ++ String.concat("\n  | ", tactics)
    ++ "\n  ].\n"
  };

let goal_directed_finishers = (~domain, ~profile, request) =>
  RewriteChecker.rational_affine_equivalent(request.source, request.target)
    ? Axioms.guarded_normalization_backend_for_profile(
        profile,
        "arith.affine_normalize",
      )
      |> Option.map(rocq_backend_tactics(~domain))
      |> Option.value(~default=[])
    : [];

let direct_certificate_prelude = domain =>
  switch (domain) {
  | CoqExport.Integers => "From Stdlib Require Import ZArith Lia.\nOpen Scope Z_scope.\n"
  | Reals =>
    "From Stdlib Require Import Rbase Rfunctions Rtrigo1 Lra.\n"
    ++ "Open Scope R_scope.\n"
  };

let direct_certificate_definitions = directed_finishers =>
  switch (directed_finishers |> RewriteChecker.dedup) {
  | [] => "Ltac hazel_profile_search := fail 0 \"no enabled rules\".\n"
  | tactics =>
    "Ltac hazel_profile_search :=\n  solve [\n    "
    ++ String.concat("\n  | ", tactics)
    ++ "\n  ].\n"
  };

let profile_search_definitions = (~domain, ~profile, ~max_depth, request) => {
  let plan = Axioms.stage_plan_for_profile(profile, MultiStepCheck);
  let directed_finishers =
    goal_directed_finishers(~domain, ~profile, request);
  let visible_tactics =
    plan.visible_rules
    |> List.concat_map((planned: Axioms.planned_visible_rule) =>
         planned.rule.rocq_backend
         |> Option.map(rocq_backend_tactics(~domain))
         |> Option.value(~default=[])
       );
  let cleanup_tactics =
    plan.pre_cleanup
    @ plan.post_cleanup
    @ (
      plan.visible_rules
      |> List.concat_map((planned: Axioms.planned_visible_rule) =>
           planned.allowed_cleanup
         )
    )
    |> List.sort_uniq(compare)
    |> List.concat_map(cleanup =>
         Axioms.rocq_cleanup_tactics(~domain=catalog_domain(domain), cleanup)
       );
  let normalization_finishers =
    plan.normalization_backends
    |> List.filter((backend: Axioms.rocq_rule_backend) =>
         backend.mode == Axioms.FinishOnly
       )
    |> List.concat_map(rocq_backend_tactics(~domain));
  let normalization_steps =
    plan.normalization_backends
    |> List.filter((backend: Axioms.rocq_rule_backend) =>
         backend.mode != Axioms.FinishOnly
       )
    |> List.concat_map(rocq_backend_tactics(~domain));
  let max_depth = max(0, min(12, max_depth));
  let recursive_search_branches =
    visible_tactics
    @ normalization_steps
    @ cleanup_tactics
    |> RewriteChecker.dedup
    |> List.map(tactic =>
         "progress (" ++ tactic ++ "); hazel_profile_search_exact n'"
       );
  tactic_alternatives(~name="hazel_profile_direct_finish", directed_finishers)
  ++ "\n"
  ++ tactic_alternatives(~name="hazel_profile_visible_step", visible_tactics)
  ++ "\n"
  ++ tactic_alternatives(
       ~name="hazel_profile_normalization_step",
       normalization_finishers @ normalization_steps,
     )
  ++ "\n"
  ++ tactic_alternatives(~name="hazel_profile_cleanup_step", cleanup_tactics)
  ++ "\nLtac hazel_profile_search_exact n :=\n"
  ++ "  lazymatch n with\n"
  ++ "  | O => reflexivity\n"
  ++ "  | S ?n' => first [\n"
  ++ "      reflexivity\n"
  ++ "    | "
  ++ (
    switch (recursive_search_branches) {
    | [] => "fail 0 \"no enabled search branches\""
    | branches => String.concat("\n    | ", branches)
    }
  )
  ++ "\n"
  ++ "    ]\n"
  ++ "  end.\n\n"
  ++ "Ltac hazel_profile_search :=\n  first [\n    "
  ++ (
    (directed_finishers == [] ? [] : ["solve [hazel_profile_direct_finish]"])
    @ (
      normalization_finishers
      |> RewriteChecker.dedup
      |> List.map(tactic => "solve [" ++ tactic ++ "]")
    )
    @ ["hazel_profile_search_exact " ++ string_of_int(max_depth) ++ "%nat"]
    |> String.concat("\n  | ")
  )
  ++ "\n  ].\n";
};

let check_result_finisher = (~domain, level) =>
  switch (domain, level) {
  | (CoqExport.Integers, Axioms.Arithmetic) => "lia"
  | (Integers, Algebra)
  | (Integers, Trigonometry) => "hazel_integer_polynomial"
  | (Reals, Arithmetic) => "lra"
  | (Reals, Algebra)
  | (Reals, Trigonometry) => "hazel_real_algebra"
  | (_, FunctionsAndLists) => "hazel_functions"
  | (_, Calculus) => "hazel_calculus"
  };

let profile_check_result_script = "hazel_profile_search";

let equivalence_check_result_script = (~domain, profile) =>
  "first ["
  ++ check_result_finisher(~domain, profile.Axioms.level)
  ++ " | reflexivity]";

let rocq_search_program_for_profile_and_purpose_internal =
    (~profile, ~purpose, ~equivalence_fallback, request) => {
  let domain = domain_for_request(request);
  let plan = rocq_plan_for_profile_and_purpose(profile, purpose);
  let directed_finishers =
    switch (purpose, equivalence_fallback) {
    | (Axioms.CheckResult, false) =>
      goal_directed_finishers(~domain, ~profile, request)
    | _ => []
    };
  let prelude =
    directed_finishers != []
      ? direct_certificate_prelude(domain)
      : (
        switch (domain) {
        | CoqExport.Reals => CoqProofExport.real_prelude
        | Integers => CoqProofExport.prelude
        }
      );
  let source = CoqExport.string_of_d_for_domain(~domain, request.source);
  let target = CoqExport.string_of_d_for_domain(~domain, request.target);
  let forall_str = forall_string(~domain, vars_for_request(request));
  let (search_definitions, tactic_script) =
    switch (purpose, equivalence_fallback) {
    | (Axioms.CheckResult, false) when directed_finishers != [] => (
        direct_certificate_definitions(directed_finishers),
        profile_check_result_script,
      )
    | (Axioms.CheckResult, false) => (
        profile_search_definitions(
          ~domain,
          ~profile,
          ~max_depth=request.max_depth,
          request,
        ),
        profile_check_result_script,
      )
    | (CheckResult, true) => (
        "",
        equivalence_check_result_script(~domain, profile),
      )
    | _ => ("", rocq_tactic_plan_script(plan))
    };
  Printf.sprintf(
    "%s\n%s\n(* Hazel Rocq tactic-search candidate. *)\nTheorem hazel_rocq_search:%s%s=%s.\nProof.\nintros.\n%s.\nQed.",
    prelude,
    search_definitions,
    forall_str,
    source,
    target,
    tactic_script,
  );
};

let rocq_search_program_for_profile_and_purpose =
    (~profile, ~purpose, request) =>
  rocq_search_program_for_profile_and_purpose_internal(
    ~profile,
    ~purpose,
    ~equivalence_fallback=false,
    request,
  );

let rocq_equivalence_program_for_profile = (~profile, request) =>
  rocq_search_program_for_profile_and_purpose_internal(
    ~profile,
    ~purpose=CheckResult,
    ~equivalence_fallback=true,
    request,
  );

let rocq_search_program_for_purpose = (~purpose, request) =>
  rocq_search_program_for_profile_and_purpose(
    ~profile=effective_profile_for_request(request),
    ~purpose,
    request,
  );

let rocq_search_program = request =>
  rocq_search_program_for_purpose(~purpose=Axioms.CheckResult, request);

let macro_detail_for_plan = (plan: Axioms.rocq_tactic_plan) =>
  "JSCoq/Rocq tactic-search plan: " ++ plan.id;

let collapsed_macro_summary_for_purpose = (~purpose, request) => {
  let profile = effective_profile_for_request(request);
  let plan = rocq_plan_for_profile_and_purpose(profile, purpose);
  let group_name =
    switch (List.rev(profile.groups)) {
    | [group, ..._] => Some(group.name)
    | [] => None
    };
  let rule_id = profile.rocq_macro_rule_id;
  let step =
    RewriteChecker.{
      origin: Normalization,
      rule_id,
      before_full_exp: request.source,
      after_full_exp: request.target,
      before_exp: request.source,
      after_exp: request.target,
      occurrence: 1,
      detail: Some(macro_detail_for_plan(plan)),
    };
  RewriteChecker.{
    justification: "Rocq tactic search",
    group_name,
    from_normal_exp: request.target,
    to_normal_exp: request.target,
    from_rule_ids: [rule_id],
    to_rule_ids: [],
    rule_ids: [rule_id],
    prover_steps: [step],
    exportable: true,
  };
};

let collapsed_macro_summary = request =>
  collapsed_macro_summary_for_purpose(~purpose=Axioms.CheckResult, request);

let search = request =>
  switch (request.backend) {
  | LocalAxiomSearch => local_axiom_search(request)
  | JSCoqTacticSearch =>
    Rejected("JSCoq tactic search backend is not implemented yet")
  };

let trace_summary = outcome =>
  switch (outcome) {
  | PrimitiveTrace(summary)
  | CollapsedMacro(summary) => Some(summary)
  | Rejected(_) => None
  };

let search_trace = request => search(request) |> trace_summary;
