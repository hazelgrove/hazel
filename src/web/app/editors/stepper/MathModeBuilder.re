open Language;
open Util;
open WebUtil;

let library_storage_key = "HAZEL_CUSTOM_MATH_MODE_LIBRARY_V1";

let load_library = (): CustomMathMode.library =>
  try({
    let local_store =
      Js_of_ocaml.Dom_html.window##.localStorage
      |> Js_of_ocaml.Js.Optdef.get(_, () => assert(false));
    let raw =
      local_store##getItem(Js_of_ocaml.Js.string(library_storage_key))
      |> Js_of_ocaml.Js.Opt.to_option
      |> Option.map(Js_of_ocaml.Js.to_string);
    switch (raw) {
    | None => CustomMathMode.empty_library
    | Some(raw) =>
      let library =
        raw |> Yojson.Safe.from_string |> CustomMathMode.library_of_yojson;
      switch (CustomMathMode.validate_library(library)) {
      | Ok () => library
      | Error(_) => CustomMathMode.empty_library
      };
    };
  }) {
  | _ => CustomMathMode.empty_library
  };

let save_library = (library: CustomMathMode.library): unit =>
  switch (CustomMathMode.validate_library(library)) {
  | Error(_) => ()
  | Ok () =>
    try({
      let local_store =
        Js_of_ocaml.Dom_html.window##.localStorage
        |> Js_of_ocaml.Js.Optdef.get(_, () => assert(false));
      let json =
        library
        |> CustomMathMode.yojson_of_library
        |> Yojson.Safe.pretty_to_string;
      local_store##setItem(
        Js_of_ocaml.Js.string(library_storage_key),
        Js_of_ocaml.Js.string(json),
      );
    }) {
    | _ => ()
    }
  };

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: string,
    label: string,
    detail: string,
    parent_level: Axioms.rewrite_level,
    additional_parent_levels: list(Axioms.rewrite_level),
    custom_parent_ids: list(string),
    active: bool,
    rule_overrides: list(CustomMathMode.rule_override),
    cleanup_overrides: list(CustomMathMode.cleanup_override),
    usage_overrides: list(Axioms.capability_usage_override),
    teacher_rewrites: list(TeacherRewrite.definition),
    session_rewrites: list(Axioms.session_rewrite),
    rewrite_draft_source: string,
    rewrite_draft_target: string,
    saved_definitions: list(CustomMathMode.definition),
    import_json: string,
    import_status: option(string),
    expanded_operation_groups: list(string),
  };

  let blank = {
    id: "my-math-mode",
    label: "My math mode",
    detail: "A custom mode composed from catalog capabilities.",
    parent_level: Axioms.Algebra,
    additional_parent_levels: [],
    custom_parent_ids: [],
    active: false,
    rule_overrides: [],
    cleanup_overrides: [],
    usage_overrides: [],
    teacher_rewrites: [],
    session_rewrites: [],
    rewrite_draft_source: "",
    rewrite_draft_target: "",
    saved_definitions: [],
    import_json: "",
    import_status: None,
    expanded_operation_groups: [],
  };

  let of_definition = (~library, ~active, definition) => {
    let built_in_parents =
      definition.CustomMathMode.parents
      |> List.filter_map(parent =>
           switch (parent) {
           | CustomMathMode.BuiltInParent(level) => Some(level)
           | CustomParent(_) => None
           }
         );
    let custom_parent_ids =
      definition.parents
      |> List.filter_map(parent =>
           switch (parent) {
           | CustomMathMode.CustomParent(id) => Some(id)
           | BuiltInParent(_) => None
           }
         );
    let parent_level =
      built_in_parents
      |> ListUtil.hd_opt
      |> Option.value(~default=Axioms.Arithmetic);
    let additional_parent_levels =
      switch (built_in_parents) {
      | []
      | [_] => []
      | [_, ...rest] => rest
      };
    {
      ...blank,
      id: definition.id,
      label: definition.label,
      detail: definition.detail,
      parent_level,
      additional_parent_levels,
      custom_parent_ids,
      active,
      rule_overrides: definition.rule_overrides,
      cleanup_overrides: definition.cleanup_overrides,
      usage_overrides: definition.usage_overrides,
      teacher_rewrites: definition.teacher_rewrites,
      saved_definitions: library.CustomMathMode.definitions,
    };
  };

  let init = {
    let library = load_library();
    let active_definition =
      switch (library.active_id) {
      | None => None
      | Some(active_id) =>
        library.definitions
        |> List.find_opt((definition: CustomMathMode.definition) =>
             definition.id == active_id
           )
      };
    switch (active_definition) {
    | Some(definition) => of_definition(~library, ~active=true, definition)
    | None => {
        ...blank,
        saved_definitions: library.definitions,
      }
    };
  };
};

let definition_of_model = (model: Model.t): CustomMathMode.definition => {
  id: model.id,
  label: model.label,
  detail: model.detail,
  parents:
    [CustomMathMode.BuiltInParent(model.parent_level)]
    @ (
      model.additional_parent_levels
      |> List.filter(level => level != model.parent_level)
      |> List.map(level => CustomMathMode.BuiltInParent(level))
    )
    @ (
      model.custom_parent_ids
      |> List.filter(id => id != model.id)
      |> List.map(id => CustomMathMode.CustomParent(id))
    ),
  rule_overrides: model.rule_overrides,
  cleanup_overrides: model.cleanup_overrides,
  usage_overrides: model.usage_overrides,
  teacher_rewrites: model.teacher_rewrites,
};

let library_of_model = (model: Model.t): CustomMathMode.library => {
  schema_version: CustomMathMode.current_schema_version,
  definitions: model.saved_definitions,
  active_id: model.active ? Some(model.id) : None,
};

let persist_model_library = model => save_library(library_of_model(model));

let saved_with_definition = (model, definition: CustomMathMode.definition) => [
  definition,
  ...model.Model.saved_definitions
     |> List.filter((saved: CustomMathMode.definition) =>
          saved.id != definition.id
        ),
];

let rec fresh_copy_id = (saved_definitions, base_id, suffix) => {
  let id =
    base_id ++ "-copy" ++ (suffix == 1 ? "" : "-" ++ string_of_int(suffix));
  saved_definitions
  |> List.exists((definition: CustomMathMode.definition) =>
       definition.id == id
     )
    ? fresh_copy_id(saved_definitions, base_id, suffix + 1) : id;
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetLabel(string)
    | SetDetail(string)
    | SetParent(Axioms.rewrite_level)
    | SetAdditionalParent(Axioms.rewrite_level, bool)
    | SetCustomParent(string, bool)
    | SetActive(bool)
    | SetRuleEnabled(string, bool)
    | SetCleanupEnabled(string, bool)
    | SetUsage(string, Axioms.automation_stage, Axioms.rewrite_usage)
    | SetTeacherRewriteEnabled(string, bool)
    | SetTeacherRewriteDirection(string, Axioms.math_rule_direction)
    | SetTeacherRewriteStage(string, Axioms.automation_stage, bool)
    | SetTeacherRewriteUsage(string, Axioms.rewrite_usage)
    | SetSessionRewriteDirection(string, Axioms.math_rule_direction)
    | RemoveSessionRewrite(string)
    | SetRewriteDraftSource(string)
    | SetRewriteDraftTarget(string)
    | AddRewriteDraft
    | SaveDefinition
    | NewDefinition
    | DuplicateDefinition
    | DeleteDefinition
    | ExportLibrary
    | ImportLibrary(bool)
    | SetImportJson(string)
    | ImportDefinition
    | LoadSavedDefinition(string)
    | SetOperationGroupExpanded(string, bool);

  let load_definition = (model: Model.t, definition) => {
    let built_in_parents =
      definition.CustomMathMode.parents
      |> List.filter_map(parent =>
           switch (parent) {
           | CustomMathMode.BuiltInParent(level) => Some(level)
           | CustomParent(_) => None
           }
         );
    let custom_parent_ids =
      definition.CustomMathMode.parents
      |> List.filter_map(parent =>
           switch (parent) {
           | CustomMathMode.CustomParent(id) => Some(id)
           | BuiltInParent(_) => None
           }
         );
    switch (built_in_parents) {
    | [parent_level, ...additional_parent_levels] => {
        ...model,
        id: definition.id,
        label: definition.label,
        detail: definition.detail,
        parent_level,
        additional_parent_levels,
        custom_parent_ids,
        rule_overrides: definition.rule_overrides,
        cleanup_overrides: definition.cleanup_overrides,
        usage_overrides: definition.usage_overrides,
        teacher_rewrites: definition.teacher_rewrites,
        import_status: Some("Imported " ++ definition.label),
      }
    | [] => {
        ...model,
        parent_level: Axioms.Arithmetic,
        additional_parent_levels: [],
        custom_parent_ids,
        id: definition.id,
        label: definition.label,
        detail: definition.detail,
        rule_overrides: definition.rule_overrides,
        cleanup_overrides: definition.cleanup_overrides,
        usage_overrides: definition.usage_overrides,
        teacher_rewrites: definition.teacher_rewrites,
        import_status: Some("Imported " ++ definition.label),
      }
    };
  };

  let update_teacher_rewrite = (model: Model.t, id, update) => {
    ...model,
    teacher_rewrites:
      model.teacher_rewrites
      |> List.map((definition: TeacherRewrite.definition) =>
           definition.id == id ? update(definition) : definition
         ),
  };

  let update_session_rewrite = (model: Model.t, id, update) => {
    ...model,
    session_rewrites:
      model.session_rewrites
      |> List.map((definition: Axioms.session_rewrite) =>
           definition.id == id ? update(definition) : definition
         ),
  };

  let fresh_session_rewrite_id = rewrites => {
    let rec loop = suffix => {
      let id = SessionRewrite.id_prefix ++ string_of_int(suffix);
      rewrites
      |> List.exists((rewrite: Axioms.session_rewrite) => rewrite.id == id)
        ? loop(suffix + 1) : id;
    };
    loop(1);
  };

  let approved_schema_for_patterns = (source_pattern, target_pattern) =>
    TeacherRewrite.approved_schemas
    |> List.find_opt((schema: TeacherRewrite.definition) =>
         schema.source_pattern == String.trim(source_pattern)
         && schema.target_pattern == String.trim(target_pattern)
       );

  let update = (action, model: Model.t): Model.t =>
    switch (action) {
    | SetLabel(label) => {
        ...model,
        label,
      }
    | SetDetail(detail) => {
        ...model,
        detail,
      }
    | SetParent(parent_level) => {
        ...model,
        parent_level,
        additional_parent_levels:
          model.additional_parent_levels
          |> List.filter(level => level != parent_level),
      }
    | SetAdditionalParent(level, enabled) => {
        ...model,
        additional_parent_levels:
          enabled
            ? List.sort_uniq(
                compare,
                [
                  level,
                  ...model.additional_parent_levels
                     |> List.filter(candidate =>
                          candidate != model.parent_level
                        ),
                ],
              )
            : List.filter(
                candidate => candidate != level,
                model.additional_parent_levels,
              ),
      }
    | SetCustomParent(id, enabled) => {
        ...model,
        custom_parent_ids:
          enabled
            ? [
              id,
              ...model.custom_parent_ids
                 |> List.filter(candidate => candidate != id),
            ]
            : model.custom_parent_ids
              |> List.filter(candidate => candidate != id),
      }
    | SetActive(false) =>
      let updated = {
        ...model,
        active: false,
      };
      persist_model_library(updated);
      updated;
    | SetActive(true) =>
      let definition = definition_of_model(model);
      let updated = {
        ...model,
        active: true,
        saved_definitions: saved_with_definition(model, definition),
      };
      switch (CustomMathMode.validate_library(library_of_model(updated))) {
      | Ok () =>
        persist_model_library(updated);
        updated;
      | Error(error) => {
          ...model,
          active: false,
          import_status: Some(CustomMathMode.library_error_message(error)),
        }
      };
    | SetRuleEnabled(rule_id, enabled) => {
        ...model,
        rule_overrides: [
          CustomMathMode.{
            rule_id,
            enabled,
          },
          ...model.rule_overrides
             |> List.filter((override: CustomMathMode.rule_override) =>
                  override.rule_id != rule_id
                ),
        ],
      }
    | SetCleanupEnabled(capability_id, enabled) => {
        ...model,
        cleanup_overrides: [
          CustomMathMode.{
            capability_id,
            enabled,
          },
          ...model.cleanup_overrides
             |> List.filter((override: CustomMathMode.cleanup_override) =>
                  override.capability_id != capability_id
                ),
        ],
      }
    | SetUsage(capability_id, stage, usage) => {
        ...model,
        usage_overrides: [
          Axioms.{
            capability_id,
            stage,
            usage,
          },
          ...model.usage_overrides
             |> List.filter((override: Axioms.capability_usage_override) =>
                  if (override.capability_id != capability_id) {
                    true;
                  } else if (override.stage == stage) {
                    false;
                  } else {
                    usage == Axioms.Disabled
                    || TeacherRewrite.stage_rank(override.stage)
                    < TeacherRewrite.stage_rank(stage)
                    || override.usage != Axioms.Disabled;
                  }
                ),
        ],
      }
    | SetOperationGroupExpanded(group, expanded) => {
        ...model,
        expanded_operation_groups:
          expanded
            ? [
              group,
              ...model.expanded_operation_groups
                 |> List.filter(candidate => candidate != group),
            ]
            : model.expanded_operation_groups
              |> List.filter(candidate => candidate != group),
      }
    | SetRewriteDraftSource(rewrite_draft_source) => {
        ...model,
        rewrite_draft_source,
        import_status: None,
      }
    | SetRewriteDraftTarget(rewrite_draft_target) => {
        ...model,
        rewrite_draft_target,
        import_status: None,
      }
    | AddRewriteDraft =>
      switch (
        approved_schema_for_patterns(
          model.rewrite_draft_source,
          model.rewrite_draft_target,
        )
      ) {
      | None =>
        let id = fresh_session_rewrite_id(model.session_rewrites);
        switch (
          SessionRewrite.make(
            ~id,
            ~source_pattern=model.rewrite_draft_source,
            ~target_pattern=model.rewrite_draft_target,
          )
        ) {
        | Error(error) => {
            ...model,
            import_status:
              Some(
                "Rewrite not added: "
                ++ SessionRewrite.validation_error_message(error),
              ),
          }
        | Ok(definition) => {
            ...model,
            rewrite_draft_source: "",
            rewrite_draft_target: "",
            session_rewrites: [definition, ...model.session_rewrites],
            import_status:
              Some(
                "Added an untrusted rewrite for this session. It is available only in One Step; Rocq export admits its reusable schema once and replays each use from that lemma.",
              ),
          }
        };
      | Some(schema) =>
        let candidate: TeacherRewrite.definition = {
          ...schema,
          source_pattern: String.trim(model.rewrite_draft_source),
          target_pattern: String.trim(model.rewrite_draft_target),
        };
        switch (TeacherRewrite.validate(candidate)) {
        | Error(error) => {
            ...model,
            import_status:
              Some(
                "Rewrite not added: "
                ++ TeacherRewrite.validation_error_message(error),
              ),
          }
        | Ok(candidate) => {
            ...model,
            rewrite_draft_source: "",
            rewrite_draft_target: "",
            teacher_rewrites: [
              candidate,
              ...model.teacher_rewrites
                 |> List.filter((item: TeacherRewrite.definition) =>
                      item.id != candidate.id
                    ),
            ],
            import_status:
              Some(
                "Added reviewed rewrite " ++ candidate.display_name ++ ".",
              ),
          }
        };
      }
    | SetTeacherRewriteEnabled(id, enabled) =>
      if (enabled) {
        switch (TeacherRewrite.approved_schema(id)) {
        | Some(definition) => {
            ...model,
            teacher_rewrites: [
              definition,
              ...model.teacher_rewrites
                 |> List.filter((item: TeacherRewrite.definition) =>
                      item.id != id
                    ),
            ],
          }
        | None => model
        };
      } else {
        {
          ...model,
          teacher_rewrites:
            model.teacher_rewrites
            |> List.filter((item: TeacherRewrite.definition) => item.id != id),
        };
      }
    | SetTeacherRewriteDirection(id, direction) =>
      update_teacher_rewrite(model, id, definition =>
        {
          ...definition,
          direction,
        }
      )
    | SetTeacherRewriteStage(id, stage, enabled) =>
      update_teacher_rewrite(model, id, definition =>
        {
          ...definition,
          stages:
            enabled
              ? TeacherRewrite.stages_with_higher(stage, definition.stages)
              : TeacherRewrite.stages_without_lower(stage, definition.stages),
        }
      )
    | SetTeacherRewriteUsage(id, default_usage) =>
      update_teacher_rewrite(model, id, definition =>
        {
          ...definition,
          default_usage,
        }
      )
    | SetSessionRewriteDirection(id, direction) =>
      update_session_rewrite(model, id, definition =>
        {
          ...definition,
          direction,
        }
      )
    | RemoveSessionRewrite(id) => {
        ...model,
        session_rewrites:
          model.session_rewrites
          |> List.filter((definition: Axioms.session_rewrite) =>
               definition.id != id
             ),
        import_status: Some("Removed the session rewrite."),
      }
    | SaveDefinition =>
      let definition = definition_of_model(model);
      let updated = {
        ...model,
        saved_definitions: saved_with_definition(model, definition),
        import_status: Some("Saved " ++ definition.label),
      };
      switch (CustomMathMode.validate_library(library_of_model(updated))) {
      | Ok () =>
        persist_model_library(updated);
        updated;
      | Error(error) => {
          ...model,
          import_status: Some(CustomMathMode.library_error_message(error)),
        }
      };
    | NewDefinition => {
        ...Model.blank,
        saved_definitions: model.saved_definitions,
        import_status: Some("Started a new inactive draft."),
      }
    | DuplicateDefinition =>
      let definition = definition_of_model(model);
      let id = fresh_copy_id(model.saved_definitions, definition.id, 1);
      let duplicate: CustomMathMode.definition = {
        ...definition,
        id,
        label: definition.label ++ " copy",
        parents:
          definition.parents
          |> List.filter(parent => parent != CustomMathMode.CustomParent(id)),
      };
      let updated =
        load_definition(
          {
            ...model,
            active: false,
            saved_definitions: [duplicate, ...model.saved_definitions],
          },
          duplicate,
        );
      persist_model_library(updated);
      {
        ...updated,
        import_status: Some("Duplicated as " ++ duplicate.label),
      };
    | DeleteDefinition =>
      let remaining =
        model.saved_definitions
        |> List.filter((definition: CustomMathMode.definition) =>
             definition.id != model.id
           );
      let updated = {
        ...Model.blank,
        saved_definitions: remaining,
        import_status: Some("Deleted " ++ model.label),
      };
      persist_model_library(updated);
      updated;
    | ExportLibrary => {
        ...model,
        import_json:
          model
          |> library_of_model
          |> CustomMathMode.yojson_of_library
          |> Yojson.Safe.pretty_to_string,
        import_status: Some("Exported the versioned mode library as JSON."),
      }
    | ImportLibrary(replace_conflicts) =>
      try({
        let imported =
          model.import_json
          |> Yojson.Safe.from_string
          |> CustomMathMode.library_of_yojson;
        switch (CustomMathMode.validate_library(imported)) {
        | Error(error) => {
            ...model,
            import_status: Some(CustomMathMode.library_error_message(error)),
          }
        | Ok () =>
          let conflicts =
            imported.definitions
            |> List.filter((definition: CustomMathMode.definition) =>
                 model.saved_definitions
                 |> List.exists((existing: CustomMathMode.definition) =>
                      existing.id == definition.id
                    )
               );
          if (conflicts != [] && !replace_conflicts) {
            {
              ...model,
              import_status:
                Some(
                  "Import conflict: "
                  ++ (
                    conflicts
                    |> List.map((definition: CustomMathMode.definition) =>
                         definition.id
                       )
                    |> String.concat(", ")
                  )
                  ++ ". Choose Replace conflicts to continue.",
                ),
            };
          } else {
            let imported_ids =
              imported.definitions
              |> List.map((definition: CustomMathMode.definition) =>
                   definition.id
                 );
            let merged =
              imported.definitions
              @ (
                model.saved_definitions
                |> List.filter((definition: CustomMathMode.definition) =>
                     !List.mem(definition.id, imported_ids)
                   )
              );
            let library: CustomMathMode.library = {
              ...imported,
              definitions: merged,
            };
            switch (CustomMathMode.validate_library(library)) {
            | Error(error) => {
                ...model,
                import_status:
                  Some(CustomMathMode.library_error_message(error)),
              }
            | Ok () =>
              save_library(library);
              let active_definition =
                imported.active_id
                |> Option.map(active_id =>
                     merged
                     |> List.find_opt((definition: CustomMathMode.definition) =>
                          definition.id == active_id
                        )
                   )
                |> Option.join;
              switch (active_definition) {
              | Some(definition) =>
                load_definition(
                  {
                    ...model,
                    saved_definitions: merged,
                    active: true,
                  },
                  definition,
                )
              | None => {
                  ...model,
                  saved_definitions: merged,
                  active: false,
                  import_status:
                    Some("Imported the versioned math-mode library."),
                }
              };
            };
          };
        };
      }) {
      | _ => {
          ...model,
          import_status: Some("Import failed: invalid library JSON."),
        }
      }
    | SetImportJson(import_json) => {
        ...model,
        import_json,
        import_status: None,
      }
    | ImportDefinition =>
      try(
        model.import_json
        |> Yojson.Safe.from_string
        |> CustomMathMode.definition_of_yojson
        |> load_definition({
             ...model,
             active: false,
           })
      ) {
      | _ => {
          ...model,
          import_status: Some("Import failed: invalid math-mode JSON."),
        }
      }
    | LoadSavedDefinition(id) =>
      switch (
        model.saved_definitions
        |> List.find_opt((saved: CustomMathMode.definition) => saved.id == id)
      ) {
      | Some(definition) =>
        load_definition(
          {
            ...model,
            active: model.active && model.id == definition.id,
          },
          definition,
        )
      | None => model
      }
    };
};

let definition = definition_of_model;

let resolved_profile = model => {
  let definition = definition(model);
  CustomMathMode.resolve(
    ~definitions=[
      definition,
      ...model.saved_definitions
         |> List.filter((saved: CustomMathMode.definition) =>
              saved.id != definition.id
            ),
    ],
    definition.id,
  );
};

let effective_profile = (~fallback, model: Model.t) =>
  if (!model.active) {
    fallback;
  } else {
    switch (resolved_profile(model)) {
    | Ok(profile) => {
        ...profile,
        step_policy: {
          ...profile.Axioms.step_policy,
          visible_rules:
            profile.step_policy.visible_rules
            @ (
              model.session_rewrites
              |> List.map((definition: Axioms.session_rewrite) =>
                   Axioms.{
                     rule_id: definition.id,
                     metadata: {
                       id: definition.id,
                       name: definition.label,
                       short_name: "Untrusted",
                       example:
                         definition.source_pattern
                         ++ " → "
                         ++ definition.target_pattern,
                       profile_group: Some("Session rewrites"),
                     },
                     allowed_cleanup: [],
                     session_rewrite: Some(definition),
                   }
                 )
            ),
        },
      }
    | Error(_) => fallback
    };
  };

let explicit_rule_enabled = (model: Model.t, rule_id) =>
  model.rule_overrides
  |> List.find_opt((override: CustomMathMode.rule_override) =>
       override.rule_id == rule_id
     )
  |> Option.map((override: CustomMathMode.rule_override) => override.enabled)
  |> Option.value(~default=true);

let rule_override_state = (model: Model.t, rule_id) =>
  model.rule_overrides
  |> List.find_opt((override: CustomMathMode.rule_override) =>
       override.rule_id == rule_id
     )
  |> Option.map((override: CustomMathMode.rule_override) => override.enabled);

let explicit_cleanup_enabled = (model: Model.t, capability_id) =>
  model.cleanup_overrides
  |> List.find_opt((override: CustomMathMode.cleanup_override) =>
       override.capability_id == capability_id
     )
  |> Option.map((override: CustomMathMode.cleanup_override) =>
       override.enabled
     )
  |> Option.value(~default=true);

let usage_for = (~profile, model: Model.t, capability_id, stage) =>
  switch (
    model.usage_overrides
    |> List.find_opt((override: Axioms.capability_usage_override) =>
         override.capability_id == capability_id && override.stage == stage
       )
  ) {
  | Some(override) => override.usage
  | None =>
    Axioms.stage_plan_for_profile(profile, stage).capabilities
    |> List.find_opt((capability: Axioms.compiled_capability) =>
         capability.id == capability_id
       )
    |> Option.map((capability: Axioms.compiled_capability) =>
         capability.usage
       )
    |> Option.value(~default=Axioms.Disabled)
  };

let usage_of_value =
  fun
  | "never" => Axioms.Disabled
  | "many" =>
    BoundedClosure({
      max_uses: 8,
      max_states: 128,
      cost: 1,
    })
  | _ => AtMostOne;

let direction_of_value =
  fun
  | "backward" => Axioms.Backward
  | "both" => Axioms.BothDirections
  | _ => Axioms.Forward;

let check_result_normalizers_for_level = level =>
  Axioms.math_rule_catalog
  |> List.filter((rule: Axioms.math_rule) =>
       (
         rule.kind == Axioms.NormalizationRule
         || rule.kind == Axioms.GuardedNormalizationRule
       )
       && List.mem(Axioms.MultiStepCheck, rule.supported_stages)
       && Axioms.rewrite_level_inherits(~current_level=level, rule.level)
       && !Axioms.rule_visible_at_level(rule, level)
     );

module View = {
  let select_option = (~selected, ~value, label) =>
    Node.option(
      ~attrs=[
        Attr.value(value),
        ...selected ? [Attr.create("selected", "selected")] : [],
      ],
      [Node.text(label)],
    );

  let usage_select = (~usage, ~on_change) =>
    Node.select(
      ~attrs=[
        Attr.class_("proof-select"),
        Attr.title("Choose Check Result repetition"),
        Attr.on_change((_, value) => on_change(usage_of_value(value))),
      ],
      [
        select_option(
          ~selected=usage == Axioms.Disabled,
          ~value="never",
          "Never",
        ),
        select_option(
          ~selected=usage == Axioms.AtMostOne,
          ~value="once",
          "Once",
        ),
        select_option(
          ~selected=
            switch (usage) {
            | Axioms.BoundedClosure(_) => true
            | _ => false
            },
          ~value="many",
          "Many",
        ),
      ],
    );

  let enabled_usage_for_stage =
    fun
    | Axioms.Manual => Axioms.AtMostOne
    | MultiStepCheck => AtMostOne
    | AutoEval => usage_of_value("many");

  let binary_stage_usage_select = (~stage, ~usage, ~on_change) => {
    let enabled = usage != Axioms.Disabled;
    let (off_label, on_label) =
      switch (stage) {
      | Axioms.Manual => ("Hidden", "Available")
      | AutoEval => ("Off", "Use for suggestions")
      | MultiStepCheck => ("Never", "Once")
      };
    Node.select(
      ~attrs=[
        Attr.class_("proof-select"),
        Attr.title(Axioms.automation_stage_detail(stage)),
        Attr.on_change((_, value) =>
          on_change(
            value == "enabled"
              ? enabled_usage_for_stage(stage) : Axioms.Disabled,
          )
        ),
      ],
      [
        select_option(~selected=!enabled, ~value="disabled", off_label),
        select_option(~selected=enabled, ~value="enabled", on_label),
      ],
    );
  };

  let stage_usage_select = (~stage, ~usage, ~on_change) =>
    switch (stage) {
    | Axioms.MultiStepCheck => usage_select(~usage, ~on_change)
    | Manual
    | AutoEval => binary_stage_usage_select(~stage, ~usage, ~on_change)
    };

  let direction_select = (~direction, ~on_change) =>
    Node.select(
      ~attrs=[
        Attr.class_("proof-select"),
        Attr.title("Choose allowed rewrite direction"),
        Attr.on_change((_, value) => on_change(direction_of_value(value))),
      ],
      [
        select_option(
          ~selected=direction == Axioms.Forward,
          ~value="forward",
          "Forward",
        ),
        select_option(
          ~selected=direction == Axioms.Backward,
          ~value="backward",
          "Backward",
        ),
        select_option(
          ~selected=direction == Axioms.BothDirections,
          ~value="both",
          "Both ways",
        ),
      ],
    );

  let checkbox = (~checked, ~label, ~detail, ~on_change) =>
    ProfileBoard.View.checkbox(
      ~checked,
      ~disabled=false,
      ~label,
      ~detail,
      ~on_change,
    );

  let builder_field = (label, input) =>
    div_c(
      "math-mode-builder-field",
      [span_c("math-mode-builder-field-label", [Node.text(label)]), input],
    );

  let editable =
      (~model: Model.t, ~inject, ~on_close, ~settings as _, ~env as _) => {
    let parent_profile = Axioms.math_profile(model.parent_level);
    let compiled_profile =
      switch (resolved_profile(model)) {
      | Ok(profile) => profile
      | Error(_) => parent_profile
      };
    let status =
      switch (resolved_profile(model)) {
      | Ok(profile) =>
        "Ready: "
        ++ string_of_int(List.length(profile.step_policy.visible_rules))
        ++ " operations"
      | Error(error) => CustomMathMode.resolution_error_message(error)
      };
    let level_button = level =>
      Widgets.button(
        ~clss=
          level == model.parent_level
            ? ["proof-button", "math-mode-builder-choice", "selected"]
            : ["proof-button", "math-mode-builder-choice"],
        Node.text(Axioms.rewrite_level_label(level)),
        ~tooltip="inherit this built-in mode",
        _ =>
        inject(Update.SetParent(level))
      );
    let custom_parent_controls =
      model.saved_definitions
      |> List.filter((saved: CustomMathMode.definition) =>
           saved.id != model.id
         )
      |> List.map((saved: CustomMathMode.definition) =>
           checkbox(
             ~checked=List.mem(saved.id, model.custom_parent_ids),
             ~label=saved.label,
             ~detail="inherit this saved custom mode",
             ~on_change=enabled =>
             inject(Update.SetCustomParent(saved.id, enabled))
           )
         );
    let rule_control = (rule: Axioms.visible_rule_policy) => {
      let state =
        switch (rule_override_state(model, rule.rule_id)) {
        | None => "Inherited"
        | Some(true) => "Locally enabled"
        | Some(false) => "Locally disabled"
        };
      let contract =
        switch (Axioms.catalog_rule_by_id(rule.rule_id)) {
        | Some(catalog_rule) =>
          let dependencies =
            catalog_rule.required_rule_ids
            @ (
              catalog_rule.required_cleanup
              |> List.map(Axioms.cleanup_capability_label)
            );
          state
          ++ (
            catalog_rule.rocq_backend == None
              ? " · no Rocq certificate" : " · Rocq certified"
          )
          ++ (
            dependencies == []
              ? "" : " · requires " ++ String.concat(", ", dependencies)
          );
        | None => state
        };
      div_c(
        "profile-board-control-rule",
        [
          checkbox(
            ~checked=explicit_rule_enabled(model, rule.rule_id),
            ~label=rule.metadata.name,
            ~detail=rule.metadata.example ++ " · " ++ contract,
            ~on_change=enabled =>
            inject(Update.SetRuleEnabled(rule.rule_id, enabled))
          ),
          ...Axioms.automation_stages
             |> List.map(stage => {
                  let usage =
                    usage_for(
                      ~profile=compiled_profile,
                      model,
                      rule.rule_id,
                      stage,
                    );
                  let local =
                    model.usage_overrides
                    |> List.exists(
                         (override: Axioms.capability_usage_override) =>
                         override.capability_id == rule.rule_id
                         && override.stage == stage
                       );
                  div_c(
                    "profile-board-row",
                    [
                      Node.text(
                        Axioms.automation_stage_label(stage)
                        ++ (local ? " (local): " : " (inherited): "),
                      ),
                      stage_usage_select(~stage, ~usage, ~on_change=usage =>
                        inject(Update.SetUsage(rule.rule_id, stage, usage))
                      ),
                    ],
                  );
                }),
        ],
      );
    };
    let operation_group_name = (rule: Axioms.visible_rule_policy) =>
      rule.metadata.profile_group |> Option.value(~default="Core operations");
    let rec rule_subgroups = (rules: list(Axioms.visible_rule_policy)) =>
      switch (rules) {
      | [] => []
      | [first, ..._] =>
        let subgroup = operation_group_name(first);
        let (members, remaining) =
          rules
          |> List.partition((rule: Axioms.visible_rule_policy) =>
               operation_group_name(rule) == subgroup
             );
        [(subgroup, members), ...rule_subgroups(remaining)];
      };
    let operation_subgroup_nodes = (~level_id, rules) =>
      rule_subgroups(rules)
      |> List.map(((group, members)) => {
           let expansion_id =
             "operation-subgroup:" ++ level_id ++ ":" ++ group;
           let expanded =
             List.mem(expansion_id, model.expanded_operation_groups);
           div_c(
             "profile-board-subgroup",
             [
               Node.button(
                 ~attrs=[
                   Attr.class_("profile-board-subgroup-toggle"),
                   Attr.create("type", "button"),
                   Attr.create("aria-expanded", expanded ? "true" : "false"),
                   Attr.on_click(_ =>
                     inject(
                       Update.SetOperationGroupExpanded(
                         expansion_id,
                         !expanded,
                       ),
                     )
                   ),
                 ],
                 [
                   span_c(
                     "profile-board-level-chevron",
                     [Node.text(expanded ? "▼" : "▶")],
                   ),
                   span_c(
                     "profile-board-subgroup-name",
                     [Node.text(group)],
                   ),
                   span_c(
                     "profile-board-level-count",
                     [
                       Node.text(
                         string_of_int(List.length(members))
                         ++ (
                           List.length(members) == 1
                             ? " operation" : " operations"
                         ),
                       ),
                     ],
                   ),
                 ],
               ),
               ...expanded
                    ? [
                      div_c(
                        "profile-board-subgroup-rules",
                        members |> List.map(rule_control),
                      ),
                    ]
                    : [],
             ],
           );
         });
    let operation_group_nodes =
      [Axioms.Arithmetic, Algebra, Trigonometry, Calculus]
      |> List.filter_map(level => {
           let members =
             parent_profile.step_policy.visible_rules
             |> List.filter((rule: Axioms.visible_rule_policy) =>
                  Axioms.level_for_rule_id(rule.rule_id) == Some(level)
                );
           members == [] ? None : Some((level, members));
         })
      |> List.map(((level, members)) => {
           let level_id = Axioms.rewrite_level_label(level);
           let expansion_id = "operation-level:" ++ level_id;
           let expanded =
             List.mem(expansion_id, model.expanded_operation_groups);
           div_c(
             "profile-board-level",
             [
               Node.button(
                 ~attrs=[
                   Attr.class_("profile-board-level-toggle"),
                   Attr.create("type", "button"),
                   Attr.create("aria-expanded", expanded ? "true" : "false"),
                   Attr.on_click(_ =>
                     inject(
                       Update.SetOperationGroupExpanded(
                         expansion_id,
                         !expanded,
                       ),
                     )
                   ),
                 ],
                 [
                   span_c(
                     "profile-board-level-chevron",
                     [Node.text(expanded ? "▼" : "▶")],
                   ),
                   span_c(
                     "profile-board-level-name",
                     [Node.text(level_id)],
                   ),
                   span_c(
                     "profile-board-level-count",
                     [
                       Node.text(
                         string_of_int(List.length(members))
                         ++ (
                           List.length(members) == 1
                             ? " operation" : " operations"
                         ),
                       ),
                     ],
                   ),
                 ],
               ),
               ...expanded
                    ? [
                      div_c(
                        "profile-board-level-rules",
                        operation_subgroup_nodes(~level_id, members),
                      ),
                    ]
                    : [],
             ],
           );
         });
    let normalizer_control = (rule: Axioms.math_rule) => {
      let stage = Axioms.MultiStepCheck;
      let usage = usage_for(~profile=compiled_profile, model, rule.id, stage);
      let local =
        model.usage_overrides
        |> List.exists((override: Axioms.capability_usage_override) =>
             override.capability_id == rule.id && override.stage == stage
           );
      let dependencies =
        rule.required_rule_ids
        @ (rule.required_cleanup |> List.map(Axioms.cleanup_capability_label));
      div_c(
        "profile-board-control-rule",
        [
          div_c(
            "profile-board-control-copy",
            [
              span_c(
                "profile-board-level-name",
                [Node.text(rule.metadata.name)],
              ),
              div_c(
                "profile-board-detail",
                [
                  Node.text(
                    rule.metadata.example
                    ++ " · hidden result-checking helper"
                    ++ (
                      dependencies == []
                        ? ""
                        : " · requires " ++ String.concat(", ", dependencies)
                    ),
                  ),
                ],
              ),
            ],
          ),
          div_c(
            "profile-board-row",
            [
              Node.text(
                "Check Result" ++ (local ? " (local): " : " (inherited): "),
              ),
              binary_stage_usage_select(~stage, ~usage, ~on_change=usage =>
                inject(Update.SetUsage(rule.id, stage, usage))
              ),
            ],
          ),
        ],
      );
    };
    let normalizer_rules =
      check_result_normalizers_for_level(model.parent_level);
    let normalizer_group_node =
      if (normalizer_rules == []) {
        [];
      } else {
        let expansion_id = "operation-check-result-helpers";
        let expanded =
          List.mem(expansion_id, model.expanded_operation_groups);
        [
          div_c(
            "profile-board-level",
            [
              Node.button(
                ~attrs=[
                  Attr.class_("profile-board-level-toggle"),
                  Attr.create("type", "button"),
                  Attr.create("aria-expanded", expanded ? "true" : "false"),
                  Attr.on_click(_ =>
                    inject(
                      Update.SetOperationGroupExpanded(
                        expansion_id,
                        !expanded,
                      ),
                    )
                  ),
                ],
                [
                  span_c(
                    "profile-board-level-chevron",
                    [Node.text(expanded ? "▼" : "▶")],
                  ),
                  span_c(
                    "profile-board-level-name",
                    [Node.text("Result-checking helpers")],
                  ),
                  span_c(
                    "profile-board-level-count",
                    [
                      Node.text(
                        string_of_int(List.length(normalizer_rules))
                        ++ (
                          List.length(normalizer_rules) == 1
                            ? " helper" : " helpers"
                        ),
                      ),
                    ],
                  ),
                ],
              ),
              ...expanded
                   ? [
                     div_c(
                       "profile-board-level-rules",
                       normalizer_rules |> List.map(normalizer_control),
                     ),
                   ]
                   : [],
            ],
          ),
        ];
      };
    let teacher_rewrite_control = (approved: TeacherRewrite.definition) => {
      let installed =
        model.teacher_rewrites
        |> List.find_opt((definition: TeacherRewrite.definition) =>
             definition.id == approved.id
           );
      div_c(
        "profile-board-control-rule",
        [
          checkbox(
            ~checked=installed != None,
            ~label=approved.display_name,
            ~detail=approved.description,
            ~on_change=enabled =>
            inject(Update.SetTeacherRewriteEnabled(approved.id, enabled))
          ),
          ...switch (installed) {
             | None => []
             | Some(definition) =>
               [
                 Node.pre([
                   Node.text(
                     definition.source_pattern
                     ++ "  ↔  "
                     ++ definition.target_pattern,
                   ),
                 ]),
                 direction_select(
                   ~direction=definition.direction, ~on_change=direction =>
                   inject(
                     Update.SetTeacherRewriteDirection(
                       definition.id,
                       direction,
                     ),
                   )
                 ),
                 div_c(
                   "profile-board-row",
                   [
                     Node.text("Check Result repetition:"),
                     usage_select(
                       ~usage=definition.default_usage, ~on_change=usage =>
                       inject(
                         Update.SetTeacherRewriteUsage(definition.id, usage),
                       )
                     ),
                   ],
                 ),
               ]
               @ (
                 Axioms.automation_stages
                 |> List.map(stage =>
                      checkbox(
                        ~checked=List.mem(stage, definition.stages),
                        ~label=Axioms.automation_stage_label(stage),
                        ~detail=Axioms.automation_stage_detail(stage),
                        ~on_change=enabled =>
                        inject(
                          Update.SetTeacherRewriteStage(
                            definition.id,
                            stage,
                            enabled,
                          ),
                        )
                      )
                    )
               )
               @ [Node.text("Certificate: " ++ definition.certificate_ref)]
             },
        ],
      );
    };
    let session_rewrite_control = (definition: Axioms.session_rewrite) =>
      div_c(
        "profile-board-control-rule math-mode-builder-untrusted-rule",
        [
          div_c(
            "math-mode-builder-untrusted-heading",
            [
              span_c(
                "profile-board-level-name",
                [Node.text("Untrusted session rewrite")],
              ),
              span_c(
                "math-mode-builder-untrusted-badge",
                [Node.text("One Step only")],
              ),
            ],
          ),
          Node.pre([
            Node.text(
              definition.source_pattern
              ++ "  ↔  "
              ++ definition.target_pattern,
            ),
          ]),
          direction_select(
            ~direction=definition.direction, ~on_change=direction =>
            inject(
              Update.SetSessionRewriteDirection(definition.id, direction),
            )
          ),
          div_c(
            "profile-board-detail math-mode-builder-untrusted-warning",
            [
              Node.text(
                "One Step only. Rocq proof export admits this reusable schema once in an UNSOUND section and replays each use from that lemma.",
              ),
            ],
          ),
          Widgets.button(
            ~clss=["proof-button"],
            Node.text("Remove"),
            ~tooltip="remove this session rewrite",
            _ =>
            inject(Update.RemoveSessionRewrite(definition.id))
          ),
        ],
      );
    Node.div(
      ~attrs=[Attr.class_("profile-board-layer")],
      [
        Node.div(
          ~attrs=[
            Attr.class_("profile-board-backdrop"),
            Attr.on_click(_ => on_close),
          ],
          [],
        ),
        div_c(
          "profile-board-modal math-mode-builder-modal",
          [
            div_c(
              "profile-board-modal-top",
              [
                div_c(
                  "math-mode-builder-heading",
                  [
                    div_c(
                      "profile-board-modal-title",
                      [Node.text("Math Mode Builder")],
                    ),
                    div_c(
                      "profile-board-detail",
                      [
                        Node.text(
                          "Compose a focused, reusable proof profile.",
                        ),
                      ],
                    ),
                  ],
                ),
                Widgets.button(
                  ~clss=["profile-board-close"],
                  Node.text("Close"),
                  ~tooltip="close Math Mode Builder",
                  _ =>
                  on_close
                ),
              ],
            ),
            div_c(
              "profile-board-wrap math-mode-builder-wrap",
              [
                div_c(
                  "profile-board-controls math-mode-builder-section",
                  [
                    div_c(
                      "profile-board-section-title",
                      [Node.text("Mode details")],
                    ),
                    builder_field(
                      "Name",
                      Node.input(
                        ~attrs=[
                          Attr.class_("math-mode-builder-input"),
                          Attr.value(model.label),
                          Attr.placeholder("Mode name"),
                          Attr.on_input((_, value) =>
                            inject(Update.SetLabel(value))
                          ),
                        ],
                        (),
                      ),
                    ),
                    builder_field(
                      "Description",
                      Node.input(
                        ~attrs=[
                          Attr.class_("math-mode-builder-input"),
                          Attr.value(model.detail),
                          Attr.placeholder(
                            "What should learners be able to do?",
                          ),
                          Attr.on_input((_, value) =>
                            inject(Update.SetDetail(value))
                          ),
                        ],
                        (),
                      ),
                    ),
                    div_c(
                      "math-mode-builder-parent-row",
                      [
                        span_c(
                          "math-mode-builder-field-label",
                          [Node.text("Start from")],
                        ),
                        div_c(
                          "math-mode-builder-choice-row",
                          [Axioms.Arithmetic, Algebra, Trigonometry, Calculus]
                          |> List.map(level_button),
                        ),
                      ],
                    ),
                    div_c(
                      custom_parent_controls == []
                        ? "math-mode-builder-parent-row empty"
                        : "math-mode-builder-parent-row",
                      [
                        span_c(
                          "math-mode-builder-field-label",
                          [Node.text("Custom parents")],
                        ),
                        div_c(
                          "math-mode-builder-parent-options",
                          custom_parent_controls,
                        ),
                      ],
                    ),
                    checkbox(
                      ~checked=model.active,
                      ~label="Use this mode",
                      ~detail=status,
                      ~on_change=active =>
                      inject(Update.SetActive(active))
                    ),
                  ],
                ),
                div_c(
                  "profile-board-controls math-mode-builder-section",
                  [
                    div_c(
                      "profile-board-section-title",
                      [Node.text("Operations")],
                    ),
                    div_c(
                      "profile-board-detail",
                      [
                        Node.text(
                          "One Step controls visibility; Check Result limits one submitted candidate; Suggest Result only prefills the search field.",
                        ),
                      ],
                    ),
                    ...operation_group_nodes @ normalizer_group_node,
                  ],
                ),
                div_c(
                  "profile-board-cleanup-policies math-mode-builder-section",
                  [
                    div_c(
                      "profile-board-section-title",
                      [Node.text("Cleanup capabilities")],
                    ),
                    div_c(
                      "profile-board-detail",
                      [
                        Node.text(
                          "Cleanup capabilities may be combined while checking, so their total scope can be broader than one visible operation.",
                        ),
                      ],
                    ),
                    ...parent_profile.step_policy.default_cleanup
                       |> List.map(capability => {
                            let id =
                              Axioms.cleanup_capability_label(capability);
                            let metadata =
                              Axioms.cleanup_capability_metadata(capability);
                            checkbox(
                              ~checked=explicit_cleanup_enabled(model, id),
                              ~label=metadata.name,
                              ~detail=metadata.example,
                              ~on_change=enabled =>
                              inject(Update.SetCleanupEnabled(id, enabled))
                            );
                          }),
                  ],
                ),
                div_c(
                  "profile-board-controls math-mode-builder-section",
                  [
                    div_c(
                      "profile-board-section-title",
                      [Node.text("Custom rewrites")],
                    ),
                    div_c(
                      "profile-board-detail",
                      [
                        Node.text(
                          "Enter source and target patterns using $names for metavariables. A reviewed match stays Rocq-certified; any other supported math pattern becomes an untrusted, session-only One Step rule.",
                        ),
                      ],
                    ),
                    div_c(
                      "math-mode-builder-syntax-hint",
                      [
                        Node.text(
                          "Example syntax: sin($a + $b)  →  sin($a)*cos($b) + cos($a)*sin($b)",
                        ),
                      ],
                    ),
                    builder_field(
                      "From",
                      Node.input(
                        ~attrs=[
                          Attr.class_("math-mode-builder-input math-pattern"),
                          Attr.value(model.rewrite_draft_source),
                          Attr.placeholder("Source pattern"),
                          Attr.on_input((_, value) =>
                            inject(Update.SetRewriteDraftSource(value))
                          ),
                        ],
                        (),
                      ),
                    ),
                    builder_field(
                      "To",
                      Node.input(
                        ~attrs=[
                          Attr.class_("math-mode-builder-input math-pattern"),
                          Attr.value(model.rewrite_draft_target),
                          Attr.placeholder("Target pattern"),
                          Attr.on_input((_, value) =>
                            inject(Update.SetRewriteDraftTarget(value))
                          ),
                        ],
                        (),
                      ),
                    ),
                    Widgets.button(
                      ~clss=["proof-button", "math-mode-builder-primary"],
                      Node.text("Add rewrite"),
                      ~tooltip=
                        "add a reviewed rewrite or an untrusted session-only rewrite",
                      _ =>
                      inject(Update.AddRewriteDraft)
                    ),
                  ]
                  @ List.map(teacher_rewrite_control, model.teacher_rewrites)
                  @ List.map(session_rewrite_control, model.session_rewrites),
                ),
                div_c(
                  "profile-board-controls math-mode-builder-section",
                  [
                    div_c(
                      "profile-board-section-title",
                      [Node.text("Save, duplicate, import, or export")],
                    ),
                    div_c(
                      "math-mode-builder-actions",
                      [
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("New"),
                          ~tooltip="start a new inactive mode",
                          _ =>
                          inject(Update.NewDefinition)
                        ),
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("Save mode"),
                          ~tooltip=
                            "create or rename this mode in the local library",
                          _ =>
                          inject(Update.SaveDefinition)
                        ),
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("Duplicate"),
                          ~tooltip="make an inactive copy",
                          _ =>
                          inject(Update.DuplicateDefinition)
                        ),
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("Delete"),
                          ~tooltip="delete this custom mode",
                          _ =>
                          inject(Update.DeleteDefinition)
                        ),
                        ...model.saved_definitions
                           |> List.map((saved: CustomMathMode.definition) =>
                                Widgets.button(
                                  ~clss=["proof-button"],
                                  Node.text("Load " ++ saved.label),
                                  ~tooltip="load this saved definition",
                                  _ =>
                                  inject(
                                    Update.LoadSavedDefinition(saved.id),
                                  )
                                )
                              ),
                      ],
                    ),
                    Node.input(
                      ~attrs=[
                        Attr.class_("math-mode-builder-input"),
                        Attr.value(model.import_json),
                        Attr.placeholder("Paste exported math-mode JSON"),
                        Attr.on_input((_, value) =>
                          inject(Update.SetImportJson(value))
                        ),
                      ],
                      (),
                    ),
                    div_c(
                      "math-mode-builder-actions",
                      [
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("Export library JSON"),
                          ~tooltip=
                            "write the versioned library JSON into the field",
                          _ =>
                          inject(Update.ExportLibrary)
                        ),
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("Import library"),
                          ~tooltip=
                            "validate and merge a versioned mode library",
                          _ =>
                          inject(Update.ImportLibrary(false))
                        ),
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("Replace conflicts"),
                          ~tooltip=
                            "replace same-id modes after validating the library",
                          _ =>
                          inject(Update.ImportLibrary(true))
                        ),
                        Widgets.button(
                          ~clss=["proof-button"],
                          Node.text("Import legacy definition"),
                          ~tooltip=
                            "validate and load one unversioned definition",
                          _ =>
                          inject(Update.ImportDefinition)
                        ),
                      ],
                    ),
                    ...model.import_status
                       |> Option.map(status =>
                            [
                              div_c(
                                "math-mode-builder-status",
                                [Node.text(status)],
                              ),
                            ]
                          )
                       |> Option.value(~default=[]),
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
