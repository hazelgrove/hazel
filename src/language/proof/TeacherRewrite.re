open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type definition = {
  id: string,
  display_name: string,
  category: string,
  description: string,
  source_pattern: string,
  target_pattern: string,
  metavariables: list(string),
  type_restrictions: list(string),
  side_conditions: list(string),
  direction: Axioms.math_rule_direction,
  stages: list(Axioms.automation_stage),
  default_usage: Axioms.rewrite_usage,
  certificate_ref: string,
};

type validation_error =
  | UnknownApprovedSchema(string)
  | MismatchedApprovedSchema(string)
  | MalformedPatterns(string)
  | InvalidStages(string)
  | InvalidUsage(string)
  | MismatchedCertificate(string);

let validation_error_message =
  fun
  | UnknownApprovedSchema(id) => "No approved rewrite schema: " ++ id
  | MismatchedApprovedSchema(id) =>
    "Rewrite metadata does not match the approved schema: " ++ id
  | MalformedPatterns(id) =>
    "Rewrite patterns or metavariables are malformed: " ++ id
  | InvalidStages(id) => "Rewrite has invalid stage availability: " ++ id
  | InvalidUsage(id) => "Rewrite has an invalid usage policy: " ++ id
  | MismatchedCertificate(id) =>
    "Rewrite certificate does not prove the approved schema: " ++ id;

let schema =
    (
      ~id,
      ~display_name,
      ~description,
      ~source_pattern,
      ~target_pattern,
      ~certificate_ref,
    ) => {
  id,
  display_name,
  category: "Trigonometry / Sum and difference identities",
  description,
  source_pattern,
  target_pattern,
  metavariables: ["a", "b"],
  type_restrictions: ["a: real", "b: real"],
  side_conditions: [],
  direction: Axioms.BothDirections,
  stages: Axioms.automation_stages,
  default_usage: Axioms.AtMostOne,
  certificate_ref,
};

/* This is the initial trust boundary: schemas and certificate adapters are
   approved here; imported definitions may select/configure them but cannot
   supply tactic source. */
let approved_schemas = [
  schema(
    ~id="trig.sin_sum",
    ~display_name="Sine of a sum",
    ~description="Expand or contract the sine sum identity.",
    ~source_pattern="sin($a + $b)",
    ~target_pattern="sin($a)*cos($b) + cos($a)*sin($b)",
    ~certificate_ref="rocq.rewrite.sin_plus",
  ),
  schema(
    ~id="trig.sin_diff",
    ~display_name="Sine of a difference",
    ~description="Expand or contract the sine difference identity.",
    ~source_pattern="sin($a - $b)",
    ~target_pattern="sin($a)*cos($b) - cos($a)*sin($b)",
    ~certificate_ref="rocq.rewrite.sin_minus",
  ),
  schema(
    ~id="trig.cos_sum",
    ~display_name="Cosine of a sum",
    ~description="Expand or contract the cosine sum identity.",
    ~source_pattern="cos($a + $b)",
    ~target_pattern="cos($a)*cos($b) - sin($a)*sin($b)",
    ~certificate_ref="rocq.rewrite.cos_plus",
  ),
  schema(
    ~id="trig.cos_diff",
    ~display_name="Cosine of a difference",
    ~description="Expand or contract the cosine difference identity.",
    ~source_pattern="cos($a - $b)",
    ~target_pattern="cos($a)*cos($b) + sin($a)*sin($b)",
    ~certificate_ref="rocq.rewrite.cos_minus",
  ),
];

let approved_schema = id =>
  approved_schemas
  |> List.find_opt((definition: definition) => definition.id == id);

let unique = items =>
  List.length(items) == List.length(List.sort_uniq(compare, items));

let stage_rank =
  fun
  | Axioms.Manual => 0
  | MultiStepCheck => 1
  | AutoEval => 2;

let stages_with_higher = (stage, stages) =>
  Axioms.automation_stages
  |> List.filter(candidate =>
       stage_rank(candidate) >= stage_rank(stage)
       || List.mem(candidate, stages)
     );

let stages_without_lower = (stage, stages) =>
  stages
  |> List.filter(candidate => stage_rank(candidate) > stage_rank(stage));

let stages_are_upward_closed = stages =>
  stages
  |> List.for_all(stage =>
       Axioms.automation_stages
       |> List.for_all(candidate =>
            stage_rank(candidate) < stage_rank(stage)
            || List.mem(candidate, stages)
          )
     );

let validate = (definition: definition) =>
  switch (approved_schema(definition.id)) {
  | None => Error(UnknownApprovedSchema(definition.id))
  | Some(approved) =>
    if (String.trim(definition.source_pattern) == ""
        || String.trim(definition.target_pattern) == ""
        || !unique(definition.metavariables)
        || definition.metavariables != approved.metavariables) {
      Error(MalformedPatterns(definition.id));
    } else if (definition.source_pattern != approved.source_pattern
               || definition.target_pattern != approved.target_pattern
               || definition.type_restrictions != approved.type_restrictions
               || definition.side_conditions != approved.side_conditions) {
      Error(MismatchedApprovedSchema(definition.id));
    } else if (definition.certificate_ref != approved.certificate_ref) {
      Error(MismatchedCertificate(definition.id));
    } else if (definition.stages == []
               || !unique(definition.stages)
               || !stages_are_upward_closed(definition.stages)
               || !
                    List.for_all(
                      stage => List.mem(stage, approved.stages),
                      definition.stages,
                    )) {
      Error(InvalidStages(definition.id));
    } else if (!Axioms.rewrite_usage_is_well_formed(definition.default_usage)) {
      Error(InvalidUsage(definition.id));
    } else if (definition.display_name != approved.display_name
               || definition.category != approved.category
               || definition.description != approved.description) {
      Error(MismatchedApprovedSchema(definition.id));
    } else {
      Ok(definition);
    }
  };
