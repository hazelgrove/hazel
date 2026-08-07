open Language;
open Util;

/* A reproducible math environment owned by an exercise.  Unlike the Math Mode
   Builder library, this definition travels with the exercise and does not
   depend on browser-local state. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  profile: CustomMathMode.definition,
  automation_stage: Axioms.automation_stage,
  lock_profile: bool,
  lock_automation_stage: bool,
  show_profile_summary: bool,
  show_next_step_hints: bool,
};

let make =
    (
      ~id,
      ~label,
      ~detail,
      ~parent_level,
      ~automation_stage,
      ~lock_profile=true,
      ~lock_automation_stage=true,
      ~show_profile_summary=true,
      ~show_next_step_hints=true,
      ~rule_overrides=[],
      ~cleanup_overrides=[],
      ~usage_overrides=[],
      (),
    )
    : t => {
  profile: {
    id,
    label,
    detail,
    parents: [CustomMathMode.BuiltInParent(parent_level)],
    rule_overrides,
    cleanup_overrides,
    usage_overrides,
    teacher_rewrites: [],
  },
  automation_stage,
  lock_profile,
  lock_automation_stage,
  show_profile_summary,
  show_next_step_hints,
};

let resolve = (policy: t) =>
  CustomMathMode.resolve(~definitions=[policy.profile], policy.profile.id);

let resolved_profile = (policy: t): Axioms.math_profile =>
  switch (resolve(policy)) {
  | Ok(profile) => profile
  | Error(error) =>
    failwith(
      "Invalid exercise math policy "
      ++ policy.profile.id
      ++ ": "
      ++ CustomMathMode.resolution_error_message(error),
    )
  };
