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

let domain_for_request = request =>
  switch (request.level) {
  | Axioms.Trigonometry
  | Calculus => CoqExport.Reals
  | _ =>
    CoqExport.requires_reals(request.source)
    || CoqExport.requires_reals(request.target)
      ? CoqExport.Reals : CoqExport.Integers
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

let tactic_for_level =
  fun
  | Axioms.Arithmetic => "hazel_arithmetic"
  | Algebra => "hazel_algebra"
  | Trigonometry => "hazel_trigonometry"
  | FunctionsAndLists
  | Calculus => "hazel_trigonometry";

let macro_rule_id_for_level =
  fun
  | Axioms.Arithmetic => "rocq.arithmetic_tactic_search"
  | Algebra => "rocq.algebra_tactic_search"
  | Trigonometry => "rocq.trigonometry_tactic_search"
  | FunctionsAndLists => "rocq.functions_tactic_search"
  | Calculus => "rocq.calculus_tactic_search";

let macro_detail_for_level = level =>
  "JSCoq/Rocq tactic-search macro: " ++ tactic_for_level(level);

let effective_level_for_request = request =>
  Axioms.export_level_for_rewrite(
    ~requested_level=request.level,
    request.source,
    request.target,
  );

let rocq_search_program = request => {
  let domain = domain_for_request(request);
  let effective_level = effective_level_for_request(request);
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
    tactic_for_level(effective_level),
  );
};

let collapsed_macro_summary = request => {
  let effective_level = effective_level_for_request(request);
  let group_name =
    switch (effective_level) {
    | Axioms.Arithmetic => Some("arithmetic")
    | Algebra => Some("algebra")
    | FunctionsAndLists => Some("functions/lists")
    | Trigonometry => Some("trigonometry")
    | Calculus => Some("calculus")
    };
  let rule_id = macro_rule_id_for_level(effective_level);
  let step =
    RewriteChecker.{
      origin: Normalization,
      rule_id,
      before_full_exp: request.source,
      after_full_exp: request.target,
      before_exp: request.source,
      after_exp: request.target,
      occurrence: 1,
      detail: Some(macro_detail_for_level(effective_level)),
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
