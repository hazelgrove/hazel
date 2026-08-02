open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type parent_ref =
  | BuiltInParent(Axioms.rewrite_level)
  | CustomParent(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type rule_override = {
  rule_id: string,
  enabled: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type cleanup_override = {
  capability_id: string,
  enabled: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type definition = {
  id: string,
  label: string,
  detail: string,
  parents: list(parent_ref),
  rule_overrides: list(rule_override),
  cleanup_overrides: list(cleanup_override),
  usage_overrides: list(Axioms.capability_usage_override),
  teacher_rewrites: list(TeacherRewrite.definition),
};

let current_schema_version = 1;

[@deriving (show({with_path: false}), sexp, yojson)]
type library = {
  schema_version: int,
  definitions: list(definition),
  active_id: option(string),
};

type resolution_error =
  | UnknownParent(string)
  | InheritanceCycle(list(string))
  | UnknownRule(string)
  | UnknownCleanup(string)
  | DependencyFailure(string)
  | InvalidTeacherRewrite(TeacherRewrite.validation_error)
  | InvalidProfile(Axioms.profile_configuration_error);

type library_error =
  | UnsupportedSchemaVersion(int)
  | DuplicateDefinitionId(string)
  | MissingActiveDefinition(string)
  | InvalidLibraryDefinition(string, resolution_error);

let resolution_error_message =
  fun
  | UnknownParent(id) => "Unknown custom math-mode parent: " ++ id
  | InheritanceCycle(ids) =>
    "Custom math-mode inheritance cycle: " ++ String.concat(" -> ", ids)
  | UnknownRule(id) => "Unknown math rule override: " ++ id
  | UnknownCleanup(id) => "Unknown cleanup override: " ++ id
  | DependencyFailure(id) => "Enabled rule has a disabled prerequisite: " ++ id
  | InvalidTeacherRewrite(error) =>
    TeacherRewrite.validation_error_message(error)
  | InvalidProfile(error) =>
    Axioms.profile_configuration_error_message(error);

let library_error_message =
  fun
  | UnsupportedSchemaVersion(version) =>
    "Unsupported custom math-mode schema version: " ++ string_of_int(version)
  | DuplicateDefinitionId(id) => "Duplicate custom math-mode id: " ++ id
  | MissingActiveDefinition(id) =>
    "Active custom math mode is missing from the library: " ++ id
  | InvalidLibraryDefinition(id, error) =>
    "Invalid custom math mode "
    ++ id
    ++ ": "
    ++ resolution_error_message(error);

let replace_by = (~key, item, items) => [
  item,
  ...items |> List.filter(existing => key(existing) != key(item)),
];

let merge_unique = (~key, left, right) =>
  right
  |> List.fold_left(
       (items, item) =>
         items |> List.exists(existing => key(existing) == key(item))
           ? items : items @ [item],
       left,
     );

let merge_profiles = (left: Axioms.math_profile, right) => {
  let shell = left.rank >= right.Axioms.rank ? left : right;
  {
    ...shell,
    groups:
      merge_unique(
        ~key=(group: Axioms.rewrite_group) => group.name,
        left.groups,
        right.groups,
      ),
    step_policy: {
      visible_rules:
        merge_unique(
          ~key=(rule: Axioms.visible_rule_policy) => rule.rule_id,
          left.step_policy.visible_rules,
          right.step_policy.visible_rules,
        ),
      default_cleanup:
        merge_unique(
          ~key=Axioms.cleanup_capability_label,
          left.step_policy.default_cleanup,
          right.step_policy.default_cleanup,
        ),
    },
    capability_usage_overrides:
      right.capability_usage_overrides
      |> List.fold_left(
           (overrides, override: Axioms.capability_usage_override) =>
             replace_by(
               ~key=
                 (candidate: Axioms.capability_usage_override) =>
                   candidate.capability_id
                   ++ ":"
                   ++ Axioms.automation_stage_label(candidate.stage),
               override,
               overrides,
             ),
           left.capability_usage_overrides,
         ),
    capability_direction_overrides:
      right.capability_direction_overrides
      |> List.fold_left(
           (overrides, override: Axioms.capability_direction_override) =>
             replace_by(
               ~key=
                 (candidate: Axioms.capability_direction_override) =>
                   candidate.capability_id,
               override,
               overrides,
             ),
           left.capability_direction_overrides,
         ),
  };
};

let apply_rule_override = (profile, override: rule_override) =>
  switch (Axioms.catalog_rule_by_id(override.rule_id)) {
  | None => Error(UnknownRule(override.rule_id))
  | Some(rule) =>
    let visible_rules =
      profile.Axioms.step_policy.visible_rules
      |> List.filter((policy: Axioms.visible_rule_policy) =>
           policy.rule_id != override.rule_id
         );
    let visible_rules =
      override.enabled
        ? visible_rules
          @ [
            {
              rule_id: rule.id,
              metadata: rule.metadata,
              allowed_cleanup: rule.allowed_cleanup,
              session_rewrite: None,
            },
          ]
        : visible_rules;
    Ok({
      ...profile,
      step_policy: {
        ...profile.step_policy,
        visible_rules,
      },
    });
  };

let apply_cleanup_override = (profile, override: cleanup_override) =>
  switch (Axioms.cleanup_capability_for_id(override.capability_id)) {
  | None => Error(UnknownCleanup(override.capability_id))
  | Some(cleanup) =>
    let default_cleanup =
      profile.Axioms.step_policy.default_cleanup
      |> List.filter(candidate => candidate != cleanup);
    let default_cleanup =
      override.enabled ? default_cleanup @ [cleanup] : default_cleanup;
    Ok({
      ...profile,
      step_policy: {
        ...profile.step_policy,
        default_cleanup,
      },
    });
  };

let install_teacher_rewrite = (profile, definition) =>
  switch (TeacherRewrite.validate(definition)) {
  | Error(error) => Error(InvalidTeacherRewrite(error))
  | Ok(definition) =>
    switch (
      apply_rule_override(
        profile,
        {
          rule_id: definition.id,
          enabled: true,
        },
      )
    ) {
    | Error(_) as error => error
    | Ok(profile) =>
      let capability_direction_overrides =
        replace_by(
          ~key=
            (candidate: Axioms.capability_direction_override) =>
              candidate.capability_id,
          Axioms.{
            capability_id: definition.id,
            direction: definition.direction,
          },
          profile.capability_direction_overrides,
        );
      let capability_usage_overrides =
        Axioms.automation_stages
        |> List.fold_left(
             (overrides, stage) =>
               replace_by(
                 ~key=
                   (candidate: Axioms.capability_usage_override) =>
                     candidate.capability_id
                     ++ ":"
                     ++ Axioms.automation_stage_label(candidate.stage),
                 Axioms.{
                   capability_id: definition.id,
                   stage,
                   usage:
                     List.mem(stage, definition.stages)
                       ? definition.default_usage : Disabled,
                 },
                 overrides,
               ),
             profile.capability_usage_overrides,
           );
      Ok({
        ...profile,
        capability_direction_overrides,
        capability_usage_overrides,
      });
    }
  };

/* A teacher-facing custom mode describes the capabilities that remain
   available, not an invalid intermediate dependency graph.  Removing a
   prerequisite therefore removes every visible operation that depends on it.
   Iterate to a fixed point so catalog dependencies can form chains without
   requiring the builder to know about individual rule IDs. */
let rec prune_rules_with_disabled_prerequisites =
        (profile: Axioms.math_profile) => {
  let visible_rules =
    profile.step_policy.visible_rules
    |> List.filter((policy: Axioms.visible_rule_policy) =>
         switch (Axioms.catalog_rule_by_id(policy.rule_id)) {
         | Some(rule) => Axioms.rule_prerequisites_satisfied(profile, rule)
         | None => true
         }
       );
  if (List.length(visible_rules)
      == List.length(profile.step_policy.visible_rules)) {
    profile;
  } else {
    prune_rules_with_disabled_prerequisites({
      ...profile,
      step_policy: {
        ...profile.step_policy,
        visible_rules,
      },
    });
  };
};

let apply_overrides = (profile, definition) => {
  let rules =
    definition.rule_overrides
    |> List.fold_left(
         (result, override) =>
           switch (result) {
           | Error(_) => result
           | Ok(profile) => apply_rule_override(profile, override)
           },
         Ok(profile),
       );
  let cleanup =
    definition.cleanup_overrides
    |> List.fold_left(
         (result, override) =>
           switch (result) {
           | Error(_) => result
           | Ok(profile) => apply_cleanup_override(profile, override)
           },
         rules,
       );
  let rewrites =
    definition.teacher_rewrites
    |> List.fold_left(
         (result, teacher_rewrite) =>
           switch (result) {
           | Error(_) => result
           | Ok(profile) => install_teacher_rewrite(profile, teacher_rewrite)
           },
         cleanup,
       );
  switch (rewrites) {
  | Error(_) as error => error
  | Ok(profile) =>
    let profile = {
      ...profile,
      label: definition.label,
      detail: definition.detail,
      capability_usage_overrides:
        definition.usage_overrides
        |> List.fold_left(
             (overrides, override: Axioms.capability_usage_override) =>
               replace_by(
                 ~key=
                   (candidate: Axioms.capability_usage_override) =>
                     candidate.capability_id
                     ++ ":"
                     ++ Axioms.automation_stage_label(candidate.stage),
                 override,
                 overrides,
               ),
             profile.capability_usage_overrides,
           ),
    };
    let profile = prune_rules_with_disabled_prerequisites(profile);
    switch (Axioms.validate_profile_configuration(profile)) {
    | Some(error) => Error(InvalidProfile(error))
    | None =>
      switch (
        profile.step_policy.visible_rules
        |> List.find_opt((policy: Axioms.visible_rule_policy) =>
             switch (Axioms.catalog_rule_by_id(policy.rule_id)) {
             | Some(rule) =>
               !Axioms.rule_prerequisites_satisfied(profile, rule)
             | None => true
             }
           )
      ) {
      | Some(policy) => Error(DependencyFailure(policy.rule_id))
      | None => Ok(profile)
      }
    };
  };
};

let resolve = (~definitions, id) => {
  let definition_for_id = id =>
    definitions
    |> List.find_opt((definition: definition) => definition.id == id);
  let rec resolve_definition = (visiting, definition) =>
    if (List.mem(definition.id, visiting)) {
      Error(InheritanceCycle(List.rev([definition.id, ...visiting])));
    } else {
      let visiting = [definition.id, ...visiting];
      let parents =
        definition.parents
        |> List.fold_left(
             (result, parent) =>
               switch (result) {
               | Error(_) => result
               | Ok(profiles) =>
                 switch (parent) {
                 | BuiltInParent(level) =>
                   Ok(profiles @ [Axioms.math_profile(level)])
                 | CustomParent(id) =>
                   switch (definition_for_id(id)) {
                   | None => Error(UnknownParent(id))
                   | Some(parent) =>
                     switch (resolve_definition(visiting, parent)) {
                     | Error(_) as error => error
                     | Ok(profile) => Ok(profiles @ [profile])
                     }
                   }
                 }
               },
             Ok([]),
           );
      switch (parents) {
      | Error(_) as error => error
      | Ok([]) =>
        apply_overrides(Axioms.math_profile(Axioms.Arithmetic), definition)
      | Ok([first, ...rest]) =>
        rest
        |> List.fold_left(merge_profiles, first)
        |> (profile => apply_overrides(profile, definition))
      };
    };
  switch (definition_for_id(id)) {
  | None => Error(UnknownParent(id))
  | Some(definition) => resolve_definition([], definition)
  };
};

let validate_library = (library: library) =>
  if (library.schema_version != current_schema_version) {
    Error(UnsupportedSchemaVersion(library.schema_version));
  } else {
    let duplicate_id =
      library.definitions
      |> List.find_map((definition: definition) =>
           library.definitions
           |> List.filter(candidate => candidate.id == definition.id)
           |> List.length
           |> (count => count > 1 ? Some(definition.id) : None)
         );
    switch (duplicate_id, library.active_id) {
    | (Some(id), _) => Error(DuplicateDefinitionId(id))
    | (None, Some(id))
        when
          !
            List.exists(
              (definition: definition) => definition.id == id,
              library.definitions,
            ) =>
      Error(MissingActiveDefinition(id))
    | (None, _) =>
      library.definitions
      |> List.fold_left(
           (result, definition: definition) =>
             switch (result) {
             | Error(_) => result
             | Ok () =>
               switch (
                 resolve(~definitions=library.definitions, definition.id)
               ) {
               | Ok(_) => Ok()
               | Error(error) =>
                 Error(InvalidLibraryDefinition(definition.id, error))
               }
             },
           Ok(),
         )
    };
  };

let empty_library = {
  schema_version: current_schema_version,
  definitions: [],
  active_id: None,
};
