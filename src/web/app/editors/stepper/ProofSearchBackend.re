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

let macro_detail_for_profile = profile =>
  "JSCoq/Rocq tactic-search macro: " ++ profile.Axioms.rocq_tactic_group;

let effective_level_for_request = request =>
  effective_profile_for_request(request).level;

let rocq_search_program = request => {
  let domain = domain_for_request(request);
  let profile = effective_profile_for_request(request);
  let prelude =
    switch (domain) {
    | CoqExport.Reals => CoqProofExport.real_prelude
    | Integers => CoqProofExport.prelude
    };
  let source = CoqExport.string_of_d_for_domain(~domain, request.source);
  let target = CoqExport.string_of_d_for_domain(~domain, request.target);
  let forall_str = forall_string(~domain, vars_for_request(request));
  Printf.sprintf(
    "%s\n(* Hazel Rocq tactic-search candidate. *)\nTheorem hazel_rocq_search:%s%s=%s.\nProof.\nintros.\n%s.\nQed.",
    prelude,
    forall_str,
    source,
    target,
    profile.rocq_tactic_group,
  );
};

let collapsed_macro_summary = request => {
  let profile = effective_profile_for_request(request);
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
      detail: Some(macro_detail_for_profile(profile)),
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
