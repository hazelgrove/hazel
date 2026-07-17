open Language;
open Util;
open WebUtil;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cleanup_override = {
    capability_id: string,
    enabled: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type rule_override = {
    rule_id: string,
    enabled: option(bool),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type section_override = {
    level_id: string,
    expanded: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    rule_overrides: list(rule_override),
    cleanup_overrides: list(cleanup_override),
    section_overrides: list(section_override),
  };

  let init = {
    rule_overrides: [],
    cleanup_overrides: [],
    section_overrides: [],
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetRuleEnabled(string, bool)
    | SetCleanupEnabled(string, bool)
    | SetSectionExpanded(string, bool);

  let default_rule_override = rule_id =>
    Model.{
      rule_id,
      enabled: None,
    };

  let upsert_rule_override = (~rule_id, ~f, model: Model.t): Model.t => {
    let found = ref(false);
    let rule_overrides =
      model.rule_overrides
      |> List.map((rule_override: Model.rule_override) =>
           if (rule_override.rule_id == rule_id) {
             found := true;
             f(rule_override);
           } else {
             rule_override;
           }
         );
    Model.{
      ...model,
      rule_overrides:
        found^
          ? rule_overrides
          : [f(default_rule_override(rule_id)), ...rule_overrides],
    };
  };

  let upsert_cleanup_override = (~capability_id, ~enabled, model: Model.t) => {
    let found = ref(false);
    let cleanup_overrides =
      model.cleanup_overrides
      |> List.map((cleanup_override: Model.cleanup_override) =>
           if (cleanup_override.capability_id == capability_id) {
             found := true;
             Model.{
               capability_id,
               enabled,
             };
           } else {
             cleanup_override;
           }
         );
    Model.{
      ...model,
      cleanup_overrides:
        found^
          ? cleanup_overrides
          : [
            Model.{
              capability_id,
              enabled,
            },
            ...cleanup_overrides,
          ],
    };
  };

  let update = (action, model: Model.t): Model.t =>
    switch (action) {
    | SetRuleEnabled(rule_id, enabled) =>
      upsert_rule_override(
        ~rule_id,
        ~f=
          rule_override =>
            Model.{
              ...rule_override,
              enabled: Some(enabled),
            },
        model,
      )
    | SetCleanupEnabled(capability_id, enabled) =>
      upsert_cleanup_override(~capability_id, ~enabled, model)
    | SetSectionExpanded(level_id, expanded) =>
      let section_overrides =
        model.section_overrides
        |> List.filter((override: Model.section_override) =>
             override.level_id != level_id
           );
      Model.{
        ...model,
        section_overrides: [
          Model.{
            level_id,
            expanded,
          },
          ...section_overrides,
        ],
      };
    };
};

type example = {
  id: string,
  label: string,
  level: Axioms.rewrite_level,
  source: Exp.t,
  target: Exp.t,
  expected_valid: bool,
};

type example_result = {
  example,
  accepted: bool,
  justification: option(string),
  rule_ids: list(string),
  cleanup_labels: list(string),
  exportable: bool,
};

type visible_rule_summary = {
  rule_id: string,
  name: string,
  short_name: string,
  example: string,
  mode_label: string,
  cleanup_labels: list(string),
  cleanup_metadata: list(Axioms.operation_metadata),
};

type profile_summary = {
  level_label: string,
  detail: string,
  visible_rules: list(visible_rule_summary),
  default_cleanup_labels: list(string),
  default_cleanup_metadata: list(Axioms.operation_metadata),
};

let plus = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Plus), left, right));

let times = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Times), left, right));

let power = (left, right) =>
  Exp.fresh(BinOp(Operators.Int(Operators.Power), left, right));

let builtin_app = (name, arg) =>
  Exp.fresh(Ap(Operators.Forward, Exp.fresh(BuiltinFun(name)), arg));

let sin = arg => builtin_app("sin", arg);
let cos = arg => builtin_app("cos", arg);
let var = name => Exp.fresh(Var(name));
let int = value => Exp.fresh(Atom(Int(Bigint.of_int(value))));

let cleanup_labels_for_rule = (profile, rule_id) =>
  Axioms.cleanup_for_visible_rule(profile.Axioms.step_policy, rule_id)
  |> List.map(Axioms.cleanup_capability_label);

let cleanup_labels_for_rules = (profile, rule_ids) =>
  rule_ids
  |> List.concat_map(rule_id => cleanup_labels_for_rule(profile, rule_id))
  |> List.fold_left(
       (seen, label) => List.mem(label, seen) ? seen : [label, ...seen],
       [],
     )
  |> List.rev;

let visible_rule_summary = (rule: Axioms.visible_rule_policy) => {
  rule_id: rule.rule_id,
  name: rule.metadata.name,
  short_name: rule.metadata.short_name,
  example: rule.metadata.example,
  mode_label: Axioms.visible_step_mode_display_label(rule.mode),
  cleanup_labels:
    rule.allowed_cleanup |> List.map(Axioms.cleanup_capability_label),
  cleanup_metadata:
    rule.allowed_cleanup |> List.map(Axioms.cleanup_capability_metadata),
};

let cleanup_capability_id = Axioms.cleanup_capability_label;

let rule_override = (model: Model.t, rule_id) =>
  model.rule_overrides
  |> List.find_opt((rule_override: Model.rule_override) =>
       rule_override.rule_id == rule_id
     );

let rule_enabled = (model: Model.t, rule_id) =>
  switch (rule_override(model, rule_id)) {
  | Some({enabled: Some(enabled), _}) => enabled
  | Some({enabled: None, _})
  | None => true
  };

let cleanup_capability_enabled = (model: Model.t, capability) => {
  let capability_id = cleanup_capability_id(capability);
  switch (
    model.cleanup_overrides
    |> List.find_opt((override: Model.cleanup_override) =>
         override.capability_id == capability_id
       )
  ) {
  | Some({enabled, _}) => enabled
  | None => true
  };
};

let section_expanded = (~active_level, model: Model.t, level) => {
  let level_id = Axioms.rewrite_level_label(level);
  switch (
    model.section_overrides
    |> List.find_opt((override: Model.section_override) =>
         override.level_id == level_id
       )
  ) {
  | Some({expanded, _}) => expanded
  | None => level == active_level
  };
};

let rule_policy_with_cleanup = (cleanup, rule: Axioms.visible_rule_policy) =>
  Axioms.{
    ...rule,
    allowed_cleanup: cleanup,
  };

let profile_with_cleanup = (~cleanup, profile) => {
  let current_policy: Axioms.step_policy = profile.Axioms.step_policy;
  let cleanup_enabled = capability =>
    cleanup
    |> List.exists((candidate: Axioms.cleanup_capability) =>
         candidate == capability
       );
  Axioms.{
    ...profile,
    step_policy: {
      default_cleanup:
        current_policy.default_cleanup |> List.filter(cleanup_enabled),
      visible_rules:
        current_policy.visible_rules
        |> List.map((rule: Axioms.visible_rule_policy) =>
             rule_policy_with_cleanup(
               rule.allowed_cleanup |> List.filter(cleanup_enabled),
               rule,
             )
           ),
    },
  };
};

let profile_without_visible_rule = (~rule_id, profile) => {
  let current_policy: Axioms.step_policy = profile.Axioms.step_policy;
  Axioms.{
    ...profile,
    step_policy: {
      ...current_policy,
      visible_rules:
        current_policy.visible_rules
        |> List.filter((rule: Axioms.visible_rule_policy) =>
             rule.rule_id != rule_id
           ),
    },
  };
};

let apply_model_to_profile = (model: Model.t, profile) => {
  let current_policy: Axioms.step_policy = profile.Axioms.step_policy;
  let cleanup_enabled = cleanup_capability_enabled(model);
  Axioms.{
    ...profile,
    step_policy: {
      default_cleanup:
        current_policy.default_cleanup |> List.filter(cleanup_enabled),
      visible_rules:
        current_policy.visible_rules
        |> List.filter((rule: Axioms.visible_rule_policy) =>
             rule_enabled(model, rule.rule_id)
           )
        |> List.map((rule: Axioms.visible_rule_policy) =>
             rule_policy_with_cleanup(
               rule.allowed_cleanup |> List.filter(cleanup_enabled),
               rule,
             )
           ),
    },
  };
};

let profile_summary = (profile: Axioms.math_profile) => {
  level_label: profile.label,
  detail: profile.detail,
  visible_rules:
    profile.step_policy.visible_rules |> List.map(visible_rule_summary),
  default_cleanup_labels:
    profile.step_policy.default_cleanup
    |> List.map(Axioms.cleanup_capability_label),
  default_cleanup_metadata:
    profile.step_policy.default_cleanup
    |> List.map(Axioms.cleanup_capability_metadata),
};

let default_examples = {
  let x = var("x");
  [
    {
      id: "arith.distribute.strict",
      label: "Arithmetic distribution without folding",
      level: Axioms.Arithmetic,
      source: times(int(2), plus(int(1), int(2))),
      target: plus(times(int(2), int(1)), times(int(2), int(2))),
      expected_valid: true,
    },
    {
      id: "arith.distribute.folded",
      label: "Arithmetic distribution plus folding",
      level: Axioms.Arithmetic,
      source: times(int(2), plus(int(1), int(2))),
      target: int(6),
      expected_valid: false,
    },
    {
      id: "alg.distribute.strict",
      label: "Algebra distribution without simplification",
      level: Axioms.Algebra,
      source: times(x, plus(int(1), x)),
      target: plus(times(x, int(1)), times(x, x)),
      expected_valid: true,
    },
    {
      id: "alg.distribute.simplified",
      label: "Algebra distribution plus identity/power cleanup",
      level: Axioms.Algebra,
      source: times(x, plus(int(1), x)),
      target: plus(x, power(x, int(2))),
      expected_valid: false,
    },
    {
      id: "alg.distribute.ac",
      label: "Algebra distribution with AC cleanup",
      level: Axioms.Algebra,
      source: times(x, plus(plus(int(1), int(2)), x)),
      target:
        plus(plus(times(int(1), x), times(int(2), x)), times(x, x)),
      expected_valid: true,
    },
    {
      id: "alg.distribute.folded",
      label: "Algebra distribution plus constant folding",
      level: Axioms.Algebra,
      source: times(x, plus(plus(int(1), int(2)), x)),
      target: plus(times(x, int(3)), times(x, x)),
      expected_valid: false,
    },
    {
      id: "trig.pythagorean",
      label: "Pythagorean trigonometry identity",
      level: Axioms.Trigonometry,
      source: plus(power(sin(x), int(2)), power(cos(x), int(2))),
      target: int(1),
      expected_valid: true,
    },
  ];
};

let run_example_with_profile = (~settings, ~env, ~profile, example) => {
  let result =
    RewriteChecker.check_single_step_result_for_profile(
      ~profile,
      ~settings,
      ~env,
      example.source,
      example.target,
    );
  switch (result) {
  | Some(result) =>
    let rule_ids =
      result.RewriteChecker.trace
      |> List.map((rule: Axioms.rewrite_rule) => rule.id);
    {
      example,
      accepted: true,
      justification: Some(result.justification),
      rule_ids,
      cleanup_labels: cleanup_labels_for_rules(profile, rule_ids),
      exportable: result.exportable,
    };
  | None => {
      example,
      accepted: false,
      justification: None,
      rule_ids: [],
      cleanup_labels: [],
      exportable: false,
    }
  };
};

let run_example = (~settings, ~env, example) =>
  run_example_with_profile(
    ~settings,
    ~env,
    ~profile=Axioms.math_profile(example.level),
    example,
  );

let run_default_examples = (~settings, ~env) =>
  default_examples
  |> List.map(example => run_example(~settings, ~env, example));

module View = {
  let text_row = (label, value) =>
    div_c(
      "profile-board-row",
      [
        span_c("profile-board-label", [Node.text(label)]),
        Node.text(value),
      ],
    );

  let operation_chip = (metadata: Axioms.operation_metadata) =>
    Node.span(
      ~attrs=[
        Attr.class_("profile-board-cleanup"),
        Attr.title(metadata.name ++ ": " ++ metadata.example),
      ],
      [
        span_c(
          "profile-board-cleanup-name",
          [Node.text(metadata.short_name)],
        ),
        metadata.example == ""
          ? span_c("profile-board-cleanup-example", [Node.text("")])
          : span_c(
              "profile-board-cleanup-example",
              [Node.text(metadata.example)],
            ),
      ],
    );

  let cleanup_list = cleanup_metadata =>
    cleanup_metadata == []
      ? Node.text("none")
      : span_c(
          "profile-board-cleanup-list",
          cleanup_metadata |> List.map(operation_chip),
        );

  let compact_cleanup_list = cleanup_metadata =>
    cleanup_metadata == []
      ? span_c("profile-board-cleanup-none", [Node.text("none")])
      : span_c(
          "profile-board-cleanup-compact-list",
          cleanup_metadata
          |> List.map((metadata: Axioms.operation_metadata) =>
               Node.span(
                 ~attrs=[
                   Attr.class_("profile-board-cleanup-compact"),
                   Attr.title(metadata.name ++ ": " ++ metadata.example),
                 ],
                 [Node.text(metadata.short_name)],
               )
             ),
        );

  let visible_rule = (rule: visible_rule_summary) =>
    div_c(
      "profile-board-rule",
      [
        text_row("Rule", rule.name),
        text_row("Example", rule.example),
        text_row("Step behavior", rule.mode_label),
        div_c(
          "profile-board-row",
          [
            span_c("profile-board-label", [Node.text("Cleanup")]),
            cleanup_list(rule.cleanup_metadata),
          ],
        ),
      ],
    );

  let cleanup_catalog = (~default_cleanup, rules) =>
    rules
    |> List.fold_left(
         (seen, rule: Axioms.visible_rule_policy) =>
           rule.allowed_cleanup
           |> List.fold_left(
                (seen, capability) =>
                  seen
                  |> List.exists((candidate: Axioms.cleanup_capability) =>
                       candidate == capability
                     )
                    ? seen : [capability, ...seen],
                seen,
              ),
         default_cleanup,
       )
    |> List.fold_left(
         (ordered, capability) =>
           List.mem(capability, ordered) ? ordered : ordered @ [capability],
         [],
       );

  let example_result = (result: example_result) =>
    div_c(
      result.accepted
        ? "profile-board-example accepted" : "profile-board-example rejected",
      [
        span_c(
          "profile-board-example-status",
          [Node.text(result.accepted ? "Valid" : "Invalid")],
        ),
        span_c(
          "profile-board-example-label",
          [Node.text(result.example.label)],
        ),
        result.rule_ids == []
          ? span_c("profile-board-example-rules", [Node.text("")])
          : span_c(
              "profile-board-example-rules",
              [Node.text(String.concat(", ", result.rule_ids))],
            ),
      ],
    );

  let examples_section = results =>
    div_c(
      "profile-board-section",
      [
        div_c("profile-board-section-title", [Node.text("Examples")]),
        ...List.map(example_result, results),
      ],
    );

  let checkbox = (~checked, ~disabled, ~label, ~detail, ~on_change) =>
    Node.label(
      ~attrs=[
        Attr.classes(
          ["profile-board-toggle"] @ (disabled ? ["disabled"] : []),
        ),
      ],
      [
        Node.input(
          ~attrs=[
            Attr.create("type", "checkbox"),
            Attr.bool_property("checked", checked),
            Attr.bool_property("disabled", disabled),
            Attr.on_click(_ =>
              disabled ? Virtual_dom.Vdom.Effect.Ignore : on_change(!checked)
            ),
          ],
          (),
        ),
        span_c(
          "profile-board-toggle-text",
          [
            span_c("profile-board-toggle-label", [Node.text(label)]),
            detail == ""
              ? span_c("profile-board-toggle-detail", [Node.text("")])
              : span_c("profile-board-toggle-detail", [Node.text(detail)]),
          ],
        ),
      ],
    );

  let rule_controls = (~model, ~inject, rule: Axioms.visible_rule_policy) => {
    let enabled = rule_enabled(model, rule.rule_id);
    div_c(
      "profile-board-control-rule",
      [
        div_c(
          "profile-board-control-rule-top",
          [
            checkbox(
              ~checked=enabled,
              ~disabled=false,
              ~label=rule.metadata.name,
              ~detail=rule.metadata.example,
              ~on_change=value =>
              inject(Update.SetRuleEnabled(rule.rule_id, value))
            ),
            span_c(
              "profile-board-control-mode",
              [
                Node.text(Axioms.visible_step_mode_display_label(rule.mode)),
              ],
            ),
          ],
        ),
        div_c(
          "profile-board-control-cleanups",
          [
            span_c(
              "profile-board-control-cleanups-label",
              [Node.text("Cleanup")],
            ),
            compact_cleanup_list(
              rule.allowed_cleanup
              |> List.filter(cleanup_capability_enabled(model))
              |> List.map(Axioms.cleanup_capability_metadata),
            ),
          ],
        ),
      ],
    );
  };

  let cleanup_policy_controls =
      (~model, ~inject, capability: Axioms.cleanup_capability) => {
    let metadata = Axioms.cleanup_capability_metadata(capability);
    let capability_id = cleanup_capability_id(capability);
    div_c(
      "profile-board-cleanup-policy",
      [
        checkbox(
          ~checked=cleanup_capability_enabled(model, capability),
          ~disabled=false,
          ~label=metadata.name,
          ~detail=metadata.example,
          ~on_change=value =>
          inject(Update.SetCleanupEnabled(capability_id, value))
        ),
      ],
    );
  };

  let level_section =
      (~model, ~inject, ~active_level, group: Axioms.rewrite_group, rules) => {
    let expanded = section_expanded(~active_level, model, group.level);
    let level_id = Axioms.rewrite_level_label(group.level);
    div_c(
      "profile-board-level",
      [
        Node.button(
          ~attrs=[
            Attr.class_("profile-board-level-toggle"),
            Attr.create("type", "button"),
            Attr.create("aria-expanded", expanded ? "true" : "false"),
            Attr.on_click(_ =>
              inject(Update.SetSectionExpanded(level_id, !expanded))
            ),
          ],
          [
            span_c(
              "profile-board-level-chevron",
              [Node.text(expanded ? "▼" : "▶")],
            ),
            span_c(
              "profile-board-level-name",
              [Node.text(Axioms.rewrite_level_label(group.level))],
            ),
            span_c(
              "profile-board-level-count",
              [
                Node.text(
                  string_of_int(List.length(rules))
                  ++ (List.length(rules) == 1 ? " operation" : " operations"),
                ),
              ],
            ),
          ],
        ),
        ...expanded
             ? [
               div_c(
                 "profile-board-level-rules",
                 rules |> List.map(rule_controls(~model, ~inject)),
               ),
             ]
             : [],
      ],
    );
  };

  let view = (~summary, ~results) => {
    ignore(results);
    div_c(
      "profile-board",
      [
        div_c(
          "profile-board-header",
          [Node.text(summary.level_label ++ " profile")],
        ),
        div_c("profile-board-detail", [Node.text(summary.detail)]),
        div_c(
          "profile-board-section",
          [
            div_c(
              "profile-board-section-title",
              [Node.text("Visible rules")],
            ),
            ...List.map(visible_rule, summary.visible_rules),
          ],
        ),
        div_c(
          "profile-board-section",
          [
            div_c(
              "profile-board-section-title",
              [Node.text("Default cleanup")],
            ),
            cleanup_list(summary.default_cleanup_metadata),
          ],
        ),
      ],
    );
  };

  let editable =
      (~model, ~inject, ~on_close, ~base_profile, ~summary, ~results) => {
    ignore(results);
    Node.div(
      ~attrs=[Attr.class_("profile-board-layer")],
      [
        Node.div(
          ~attrs=[
            Attr.class_("profile-board-backdrop"),
            Attr.on_mousedown(_ => on_close),
          ],
          [],
        ),
        Node.div(
          ~attrs=[
            Attr.class_("profile-board-modal"),
            Attr.on_mousedown(_ => Virtual_dom.Vdom.Effect.Stop_propagation),
          ],
          [
            div_c(
              "profile-board-modal-top",
              [
                div_c(
                  "profile-board-modal-title",
                  [Node.text(summary.level_label ++ " Math Profile")],
                ),
                Node.button(
                  ~attrs=[
                    Attr.class_("profile-board-close"),
                    Attr.title("Close profile"),
                    Attr.on_click(_ => on_close),
                  ],
                  [Node.text("x")],
                ),
              ],
            ),
            div_c(
              "profile-board-wrap",
              [
                div_c(
                  "profile-board-controls",
                  [
                    div_c(
                      "profile-board-section-title",
                      [Node.text("Visible operations")],
                    ),
                    ...base_profile.groups
                       |> List.filter_map((group: Axioms.rewrite_group) => {
                            let group_rule_ids =
                              group.rules
                              |> List.map((rule: Axioms.rewrite_rule) =>
                                   rule.id
                                 );
                            let rules =
                              base_profile.Axioms.step_policy.visible_rules
                              |> List.filter(
                                   (rule: Axioms.visible_rule_policy) =>
                                   List.mem(rule.rule_id, group_rule_ids)
                                 );
                            rules == []
                              ? None
                              : Some(
                                  level_section(
                                    ~model,
                                    ~inject,
                                    ~active_level=base_profile.level,
                                    group,
                                    rules,
                                  ),
                                );
                          }),
                  ],
                ),
                div_c(
                  "profile-board-cleanup-policies",
                  [
                    div_c(
                      "profile-board-section-title",
                      [Node.text("Cleanup policies")],
                    ),
                    ...cleanup_catalog(
                         ~default_cleanup=
                           base_profile.Axioms.step_policy.default_cleanup,
                         base_profile.Axioms.step_policy.visible_rules,
                       )
                       |> List.map(cleanup_policy_controls(~model, ~inject)),
                  ],
                ),
              ],
            ),
          ],
        ),
      ],
    );
  };
};
