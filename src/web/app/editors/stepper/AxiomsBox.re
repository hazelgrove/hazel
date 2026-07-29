open Util;
open WebUtil;
open Calc.Syntax;
open Language;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    filter: Calc.t(string),
    all_rules: Calc.saved(ProofCtx.t),
    filtered_rewrites: Calc.saved(list(AssumptionBox.Model.t)),
  };

  let init = {
    filter: Calc.NewValue(""),
    all_rules: Calc.Pending,
    filtered_rewrites: Calc.Pending,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetFilter(string);

  let update = (~settings as _, action, model): Updated.t(Model.t) => {
    switch (action) {
    | SetFilter(filter) =>
      Model.{
        ...model,
        filter: Calc.NewValue(filter),
      }
      |> Updated.return_quiet
    };
  };

  let calculate =
      (
        ~info_map: Calc.t(Statics.Map.t),
        ~ctx: Calc.t(SemanticCtx.t),
        ~selected_exp: Calc.t(option(Exp.t)),
        model: Model.t,
      )
      : Model.t => {
    let all_rules =
      model.all_rules
      |> {
        let.calc ctx = ctx;
        let env = SemanticCtx.get_env(ctx);
        let ctx = SemanticCtx.get_ctx(ctx);
        ProofCtx.of_env(~builtins=Axioms.v, ~ctx, env);
      };

    let filtered_rewrites =
      model.filtered_rewrites
      |> {
        let.calc all_rules = all_rules
        and.calc ctx = ctx
        and.calc filter = model.filter
        and.calc selected_exp = selected_exp
        and.calc info_map = info_map;

        let all_assumption_boxes =
          all_rules
          |> (
            filter == ""
              ? x => x
              : List.filter(({name, _}: ProofCtx.entry) =>
                  StringUtil.subseq_search(name, filter)
                )
          )
          |> List.map(ctx_entry =>
               AssumptionBox.Model.{ctx_entry: ctx_entry}
             )
          |> (
            filter == ""
              ? List.filter((ab: AssumptionBox.Model.t) =>
                  switch (selected_exp) {
                  | Some(selected_exp) =>
                    ProofRule.is_active(
                      ~info_map,
                      ~env=SemanticCtx.get_env(ctx),
                      ab.ctx_entry.rule,
                      selected_exp |> DHExp.strip_ascriptions,
                    )
                  | None => false
                  }
                )
              : (x => x)
          );

        all_assumption_boxes;
      };
    {
      filter: model.filter |> Calc.make_old,
      all_rules: all_rules |> Calc.save,
      filtered_rewrites: filtered_rewrites |> Calc.save,
    };
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = unit;

  let get_cursor_info =
      (~selection as (): t, _model: Model.t): cursor(Update.t) => {
    empty;
  };
};

let rewrite_enabled_for_profile =
    (profile: Axioms.math_profile, rewrite: TrigRewrite.rewrite) =>
  Axioms.visible_rule_enabled(profile.step_policy, rewrite.rule_id);

type calculus_cleanup_step = {
  before_exp: Exp.t,
  after_exp: Exp.t,
  capability: Axioms.cleanup_capability,
};

type calculus_action_details = {
  rewrite: TrigRewrite.rewrite,
  rule_after_exp: Exp.t,
  cleanup_steps: list(calculus_cleanup_step),
};

let calculus_cleanup_steps = (~cleanup_enabled, exp) => {
  let rec loop = (steps, before_exp) =>
    switch (DifferentiationRewrite.cleanup_once(~cleanup_enabled, before_exp)) {
    | Some((after_exp, capability))
        when !Exp.fast_equal(before_exp, after_exp) =>
      loop(
        [
          {
            before_exp,
            after_exp,
            capability,
          },
          ...steps,
        ],
        after_exp,
      )
    | Some(_)
    | None => List.rev(steps)
    };
  loop([], exp);
};

let calculus_action_details_for_profile =
    (~profile: Axioms.math_profile, selected_exp) =>
  DifferentiationRewrite.applicable_at_root(
    ~rule_enabled=
      rule_id => Axioms.visible_rule_enabled(profile.step_policy, rule_id),
    selected_exp,
  )
  |> List.map((rewrite: TrigRewrite.rewrite) => {
       let cleanup =
         Axioms.cleanup_for_visible_rule(
           profile.step_policy,
           rewrite.rule_id,
         );
       let cleanup_enabled = capability => List.mem(capability, cleanup);
       let cleanup_steps =
         calculus_cleanup_steps(~cleanup_enabled, rewrite.after_exp);
       let after_exp =
         cleanup_steps
         |> ListUtil.last_opt
         |> Option.map((step: calculus_cleanup_step) => step.after_exp)
         |> Option.value(~default=rewrite.after_exp);
       {
         rewrite: {
           ...rewrite,
           after_exp,
         },
         rule_after_exp: rewrite.after_exp,
         cleanup_steps,
       };
     });

let calculus_actions_for_profile =
    (~profile: Axioms.math_profile, selected_exp) =>
  calculus_action_details_for_profile(~profile, selected_exp)
  |> List.map((details: calculus_action_details) => details.rewrite);

let calculus_cleanup_actions_for_profile =
    (~profile: Axioms.math_profile, selected_exp) => {
  let cleanup_enabled = capability =>
    List.mem(capability, profile.step_policy.default_cleanup);
  switch (DifferentiationRewrite.cleanup_once(~cleanup_enabled, selected_exp)) {
  | None => []
  | Some((after_exp, capability)) =>
    let metadata = Axioms.cleanup_capability_metadata(capability);
    [
      TrigRewrite.{
        rule_id: metadata.id,
        label: metadata.name,
        before_exp: selected_exp,
        after_exp,
      },
    ];
  };
};

let algebra_shape_label = rule_ids =>
  if (List.mem("alg.expand_polynomial", rule_ids)) {
    "expand polynomial";
  } else if (List.mem("alg.factor_common", rule_ids)) {
    "factor expression";
  } else if (List.mem("alg.distribute_mul_add", rule_ids)) {
    "distribute multiplication";
  } else {
    "algebra rewrite";
  };

module View = {
  let view =
      (
        ~globals: Globals.t,
        ~info_map,
        ~env,
        ~full_exp,
        ~selected_exp,
        ~inject: Update.t => Ui_effect.t(unit),
        ~take_focus: Selection.t => Ui_effect.t(unit),
        ~add_axiom_step:
           (string, int, Exp.t, Direction.t, string) => Ui_effect.t(unit),
        ~add_written_step:
           (ProofTrace.trace_summary, int, Exp.t, Exp.t) => Ui_effect.t(unit),
        ~profile: Axioms.math_profile,
        ~rewrite_level: Axioms.rewrite_level,
        ~show_mode_warning: bool,
        model: Model.t,
      ) => {
    let unpacked_rewrites =
      model.filtered_rewrites
      |> Calc.get_saved_exc(~print="view_step_rewrites");
    let filter = model.filter |> Calc.get_value;
    let allowed_trig_rule_ids =
      profile.step_policy.visible_rules
      |> List.map((rule: Axioms.visible_rule_policy) => rule.rule_id)
      |> List.filter(TrigRewrite.is_trig_rule_id);
    let rewrite_allowed = (rewrite: TrigRewrite.rewrite) =>
      AxiomSearch.unsupported_constructs_for_rewrite(
        ~level=rewrite_level,
        ~source=selected_exp,
        ~target=rewrite.after_exp,
      )
      == [];
    let trig_actions =
      TrigRewrite.applicable_at_root(selected_exp)
      |> List.filter((rewrite: TrigRewrite.rewrite) =>
           List.mem(rewrite.rule_id, allowed_trig_rule_ids)
         )
      |> List.filter(rewrite_allowed)
      |> (
        filter == ""
          ? x => x
          : List.filter((rewrite: TrigRewrite.rewrite) =>
              StringUtil.subseq_search(rewrite.label, filter)
              || StringUtil.subseq_search(rewrite.rule_id, filter)
            )
      );
    let session_actions =
      Axioms.session_rewrites_for_profile(profile)
      |> List.concat_map((definition: Axioms.session_rewrite) =>
           SessionRewrite.rewrites_at_root(definition, selected_exp)
         )
      |> List.filter(rewrite_allowed)
      |> (
        filter == ""
          ? x => x
          : List.filter((rewrite: TrigRewrite.rewrite) =>
              StringUtil.subseq_search(rewrite.label, filter)
              || StringUtil.subseq_search(rewrite.rule_id, filter)
            )
      );
    let calculus_actions =
      calculus_actions_for_profile(~profile, selected_exp)
      |> List.filter(rewrite_allowed)
      |> (
        filter == ""
          ? x => x
          : List.filter((rewrite: TrigRewrite.rewrite) =>
              StringUtil.subseq_search(rewrite.label, filter)
              || StringUtil.subseq_search(rewrite.rule_id, filter)
            )
      );
    let calculus_cleanup_actions =
      calculus_cleanup_actions_for_profile(~profile, selected_exp)
      |> List.filter((cleanup: TrigRewrite.rewrite) =>
           calculus_actions
           |> List.for_all((visible: TrigRewrite.rewrite) =>
                !TrigRewrite.exp_same(visible.after_exp, cleanup.after_exp)
              )
         )
      |> List.filter(rewrite_allowed)
      |> (
        filter == ""
          ? x => x
          : List.filter((rewrite: TrigRewrite.rewrite) =>
              StringUtil.subseq_search(rewrite.label, filter)
              || StringUtil.subseq_search(rewrite.rule_id, filter)
            )
      );
    let filter_rewrites = rewrites =>
      filter == ""
        ? rewrites
        : List.filter(
            (rewrite: TrigRewrite.rewrite) =>
              StringUtil.subseq_search(rewrite.label, filter)
              || StringUtil.subseq_search(rewrite.rule_id, filter),
            rewrites,
          );
    let algebra_identity_actions =
      AlgebraIdentityRewrite.applicable_at_root(selected_exp)
      |> List.filter((rewrite: TrigRewrite.rewrite) =>
           rewrite_enabled_for_profile(profile, rewrite)
         )
      |> List.filter(rewrite_allowed)
      |> filter_rewrites;
    let algebra_enabled =
      profile.step_policy.visible_rules
      |> List.exists((rule: Axioms.visible_rule_policy) =>
           String.starts_with(~prefix="alg.", rule.rule_id)
         );
    let simplification_actions =
      (
        Axioms.normalization_rule_id_enabled_for_profile(
          profile,
          Axioms.MultiStepCheck,
          "arith.affine_normalize",
        )
          ? TrigRewrite.scalar_product_simplifications_at_root(selected_exp)
          : []
      )
      |> List.filter(rewrite_allowed)
      |> filter_rewrites;
    let algebra_shape_actions =
      (
        algebra_enabled
          ? switch (RewriteChecker.normalize_algebra_shape(selected_exp)) {
            | Some((after_exp, rule_ids)) => [
                TrigRewrite.{
                  rule_id:
                    rule_ids
                    |> ListUtil.hd_opt
                    |> Option.value(~default="alg.distribute_mul_add"),
                  label: algebra_shape_label(rule_ids),
                  before_exp: selected_exp,
                  after_exp,
                },
              ]
            | None => []
            }
          : []
      )
      |> List.filter(rewrite_allowed)
      |> List.filter((rewrite: TrigRewrite.rewrite) =>
           rewrite_enabled_for_profile(profile, rewrite)
         )
      |> filter_rewrites;
    let selected_exp_idx =
      try(ProofHacks.exp_idx(selected_exp, full_exp)) {
      | _ => 0
      };
    let mode_warning =
      algebra_shape_actions != []
      || simplification_actions != []
      || trig_actions != []
      || session_actions != []
      || calculus_actions != []
      || calculus_cleanup_actions != []
        ? []
        : (
          switch (
            AxiomSearch.unsupported_constructs_message(
              ~level=rewrite_level,
              [selected_exp],
            )
          ) {
          | Some(message) => [
              div_c("proof-mode-warning", [Node.text(message)]),
            ]
          | None => []
          }
        );
    let authorized_plan_for_rewrite = (rewrite: TrigRewrite.rewrite) =>
      ProfileProofPlan.authorize({
        profile,
        stage: Axioms.Manual,
        candidate_origin: ProfileProofPlan.DisplayedSuggestion,
        settings: globals.settings.core,
        env,
        source: selected_exp,
        target: rewrite.after_exp,
        max_depth: 1,
        max_states: 80,
      })
      |> ProfileProofPlan.authorized_plan;
    let trig_action_view = (rewrite: TrigRewrite.rewrite) =>
      authorized_plan_for_rewrite(rewrite)
      |> Option.map((plan: ProfileProofPlan.authorized_plan) =>
           div_c(
             "assumption-box",
             [
               Widgets.button_d(
                 Node.text("==>"),
                 add_written_step(
                   plan.summary,
                   selected_exp_idx,
                   selected_exp,
                   rewrite.after_exp,
                 ),
                 ~disabled=false,
               ),
               Node.text(" " ++ rewrite.label ++ ": "),
               CodeViewable.view_any(
                 ~globals,
                 ~settings=
                   Haz3lcore.ExpToSegment.Settings.of_core(
                     ~inline=true,
                     ~fold_fn_bodies=`Text,
                     globals.settings.core,
                   ),
                 Exp(rewrite.after_exp),
               ),
             ],
           )
         );
    let calculus_action_view = (rewrite: TrigRewrite.rewrite) =>
      trig_action_view(rewrite);
    let calculus_cleanup_action_view = (rewrite: TrigRewrite.rewrite) =>
      trig_action_view(rewrite);
    let trace_summary_for_simplification = (rewrite: TrigRewrite.rewrite) =>
      authorized_plan_for_rewrite(rewrite)
      |> Option.map((plan: ProfileProofPlan.authorized_plan) => plan.summary);
    let simplification_action_view = (rewrite: TrigRewrite.rewrite) =>
      trace_summary_for_simplification(rewrite)
      |> Option.map(summary =>
           div_c(
             "assumption-box",
             [
               Widgets.button_d(
                 Node.text("==>"),
                 add_written_step(
                   summary,
                   selected_exp_idx,
                   selected_exp,
                   rewrite.after_exp,
                 ),
                 ~disabled=false,
               ),
               Node.text(" " ++ rewrite.label ++ ": "),
               CodeViewable.view_any(
                 ~globals,
                 ~settings=
                   Haz3lcore.ExpToSegment.Settings.of_core(
                     ~inline=true,
                     ~fold_fn_bodies=`Text,
                     globals.settings.core,
                   ),
                 Exp(rewrite.after_exp),
               ),
             ],
           )
         );
    let trace_summary_for_algebra_shape = (rewrite: TrigRewrite.rewrite) =>
      authorized_plan_for_rewrite(rewrite)
      |> Option.map((plan: ProfileProofPlan.authorized_plan) => plan.summary);
    let algebra_shape_action_view = (rewrite: TrigRewrite.rewrite) =>
      trace_summary_for_algebra_shape(rewrite)
      |> Option.map(summary =>
           div_c(
             "assumption-box",
             [
               Widgets.button_d(
                 Node.text("==>"),
                 add_written_step(
                   summary,
                   selected_exp_idx,
                   selected_exp,
                   rewrite.after_exp,
                 ),
                 ~disabled=false,
               ),
               Node.text(" " ++ rewrite.label ++ ": "),
               CodeViewable.view_any(
                 ~globals,
                 ~settings=
                   Haz3lcore.ExpToSegment.Settings.of_core(
                     ~inline=true,
                     ~fold_fn_bodies=`Text,
                     globals.settings.core,
                   ),
                 Exp(rewrite.after_exp),
               ),
             ],
           )
         );
    let algebra_shape_section =
      switch (
        List.filter_map(
          algebra_shape_action_view,
          algebra_identity_actions @ algebra_shape_actions,
        )
      ) {
      | [] => []
      | action_views => [
          div_c("assumption-box", [Node.text("Algebra")]),
          ...action_views,
        ]
      };
    let simplification_section =
      switch (
        List.filter_map(simplification_action_view, simplification_actions)
      ) {
      | [] => []
      | action_views => [
          div_c("assumption-box", [Node.text("Simplifications")]),
          ...action_views,
        ]
      };
    let trig_section =
      switch (List.filter_map(trig_action_view, trig_actions)) {
      | [] => []
      | action_views => [
          div_c("assumption-box", [Node.text("Trig identities")]),
          ...action_views,
        ]
      };
    let session_section =
      switch (List.filter_map(trig_action_view, session_actions)) {
      | [] => []
      | action_views => [
          div_c(
            "assumption-box session-rewrite-heading",
            [Node.text("Session rewrites (unproved)")],
          ),
          ...action_views,
        ]
      };
    let calculus_section =
      switch (List.filter_map(calculus_action_view, calculus_actions)) {
      | [] => []
      | action_views => [
          div_c("assumption-box", [Node.text("Differentiation")]),
          ...action_views,
        ]
      };
    let calculus_cleanup_section =
      switch (
        List.filter_map(
          calculus_cleanup_action_view,
          calculus_cleanup_actions,
        )
      ) {
      | [] => []
      | action_views => [
          div_c("assumption-box", [Node.text("Calculus cleanup")]),
          ...action_views,
        ]
      };
    [
      Node.input(
        ~attrs=[
          Attr.value(model.filter |> Calc.get_value),
          Attr.placeholder("search assumptions..."),
          Attr.on_focus(_ => take_focus()),
          Attr.on_input((_, s) => inject(SetFilter(s))),
        ],
        (),
      ),
    ]
    @ (show_mode_warning ? mode_warning : [])
    @ algebra_shape_section
    @ simplification_section
    @ calculus_section
    @ calculus_cleanup_section
    @ session_section
    @ trig_section
    @ List.map(
        (am: AssumptionBox.Model.t) =>
          AssumptionBox.View.view(
            ~globals,
            ~info_map,
            ~env,
            ~active_selection=
              Some((
                selected_exp,
                [],
                fun
                | AssumptionBox.EqualityLeft(e) => {
                    add_axiom_step(
                      am.ctx_entry.name,
                      try(ProofHacks.exp_idx(selected_exp, full_exp)) {
                      | _ => 0
                      },
                      selected_exp,
                      Left,
                      e,
                    );
                  }
                | AssumptionBox.EqualityRight(e) =>
                  add_axiom_step(
                    am.ctx_entry.name,
                    try(ProofHacks.exp_idx(selected_exp, full_exp)) {
                    | _ => 0
                    },
                    selected_exp,
                    Right,
                    e,
                  ),
              )),
            am,
          ),
        unpacked_rewrites,
      );
  };
};
