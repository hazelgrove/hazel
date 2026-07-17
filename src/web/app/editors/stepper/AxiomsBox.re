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

let calculus_actions_for_profile =
    (~profile: Axioms.math_profile, selected_exp) =>
  DifferentiationRewrite.applicable_at_root(
    ~rule_enabled=
      rule_id => Axioms.visible_rule_enabled(profile.step_policy, rule_id),
    selected_exp,
  );

module View = {
  let view =
      (
        ~globals,
        ~info_map,
        ~env,
        ~full_exp,
        ~selected_exp,
        ~inject: Update.t => Ui_effect.t(unit),
        ~take_focus: Selection.t => Ui_effect.t(unit),
        ~add_axiom_step:
           (string, int, Exp.t, Direction.t, string) => Ui_effect.t(unit),
        ~add_written_step:
           (RewriteChecker.trace_summary, int, Exp.t, Exp.t) =>
           Ui_effect.t(unit),
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
        algebra_enabled
          ? TrigRewrite.scalar_product_simplifications_at_root(selected_exp)
          : []
      )
      |> List.filter(rewrite_allowed)
      |> filter_rewrites;
    let algebra_shape_label = rule_ids =>
      if (List.mem("alg.distribute_mul_add", rule_ids)) {
        "distribute multiplication";
      } else if (List.mem("alg.expand_polynomial", rule_ids)) {
        "expand polynomial";
      } else if (List.mem("alg.factor_common", rule_ids)) {
        "factor expression";
      } else {
        "algebra rewrite";
      };
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
      || calculus_actions != []
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
    let trace_summary_for_trig = (rewrite: TrigRewrite.rewrite) =>
      RewriteChecker.{
        justification: "trigonometry one step",
        group_name: Some("trigonometry"),
        from_normal_exp: selected_exp,
        to_normal_exp: rewrite.after_exp,
        from_rule_ids: [rewrite.rule_id],
        to_rule_ids: [],
        rule_ids: [rewrite.rule_id],
        prover_steps: [
          prover_step(
            ~origin=ManualRewrite,
            ~rule_id=rewrite.rule_id,
            ~before_full_exp=selected_exp,
            ~after_full_exp=rewrite.after_exp,
            ~before_exp=rewrite.before_exp,
            ~after_exp=rewrite.after_exp,
            ~detail="selected trig identity",
          ),
        ],
        exportable: false,
      };
    let trig_action_view = (rewrite: TrigRewrite.rewrite) =>
      div_c(
        "assumption-box",
        [
          Widgets.button_d(
            Node.text("==>"),
            add_written_step(
              trace_summary_for_trig(rewrite),
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
      );
    let trace_summary_for_calculus = (rewrite: TrigRewrite.rewrite) =>
      RewriteChecker.{
        justification: "calculus one step",
        group_name: Some("calculus"),
        from_normal_exp: selected_exp,
        to_normal_exp: rewrite.after_exp,
        from_rule_ids: [rewrite.rule_id],
        to_rule_ids: [],
        rule_ids: [rewrite.rule_id],
        prover_steps: [
          prover_step(
            ~origin=ManualRewrite,
            ~rule_id=rewrite.rule_id,
            ~before_full_exp=selected_exp,
            ~after_full_exp=rewrite.after_exp,
            ~before_exp=rewrite.before_exp,
            ~after_exp=rewrite.after_exp,
            ~detail="selected differentiation rule",
          ),
        ],
        exportable: true,
      };
    let calculus_action_view = (rewrite: TrigRewrite.rewrite) =>
      div_c(
        "assumption-box",
        [
          Widgets.button_d(
            Node.text("==>"),
            add_written_step(
              trace_summary_for_calculus(rewrite),
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
      );
    let trace_summary_for_simplification = (rewrite: TrigRewrite.rewrite) => {
      let normalization_summary = () =>
        RewriteChecker.{
          justification: "trigonometry argument normalization",
          group_name: Some("trigonometry"),
          from_normal_exp: rewrite.after_exp,
          to_normal_exp: rewrite.after_exp,
          from_rule_ids: [rewrite.rule_id],
          to_rule_ids: [],
          rule_ids: [rewrite.rule_id],
          prover_steps: [
            prover_step(
              ~origin=Normalization,
              ~rule_id=rewrite.rule_id,
              ~before_full_exp=selected_exp,
              ~after_full_exp=rewrite.after_exp,
              ~before_exp=rewrite.before_exp,
              ~after_exp=rewrite.after_exp,
              ~detail="normalize scalar products in trig argument",
            ),
          ],
          exportable: true,
        };
      switch (
        RewriteChecker.check_single_step_trace_at_level(
          ~level=rewrite_level,
          ~settings=globals.settings.core,
          ~env,
          selected_exp,
          rewrite.after_exp,
        )
      ) {
      | Some({prover_steps: [_, ..._], _} as summary) => summary
      | Some(_)
      | None => normalization_summary()
      };
    };
    let simplification_action_view = (rewrite: TrigRewrite.rewrite) =>
      div_c(
        "assumption-box",
        [
          Widgets.button_d(
            Node.text("==>"),
            add_written_step(
              trace_summary_for_simplification(rewrite),
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
      );
    let trace_summary_for_algebra_shape = (rewrite: TrigRewrite.rewrite) =>
      RewriteChecker.check_single_step_result_for_profile(
        ~profile,
        ~settings=globals.settings.core,
        ~env,
        selected_exp,
        rewrite.after_exp,
      )
      |> Option.map(RewriteChecker.trace_summary_of_result);
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
      switch (simplification_actions) {
      | [] => []
      | actions => [
          div_c("assumption-box", [Node.text("Simplifications")]),
          ...List.map(simplification_action_view, actions),
        ]
      };
    let trig_section =
      switch (trig_actions) {
      | [] => []
      | actions => [
          div_c("assumption-box", [Node.text("Trig identities")]),
          ...List.map(trig_action_view, actions),
        ]
      };
    let calculus_section =
      switch (calculus_actions) {
      | [] => []
      | actions => [
          div_c("assumption-box", [Node.text("Differentiation")]),
          ...List.map(calculus_action_view, actions),
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
