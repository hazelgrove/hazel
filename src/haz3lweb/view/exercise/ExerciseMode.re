open Util;
open Virtual_dom.Vdom;
open Node;

module GradingReport = Haz3lschool.GradingReport.F(Exercise.ExerciseEnv);

let score_view = ((earned: float, max: float)) => {
  div(
    ~attrs=[
      Attr.classes([
        "test-percent",
        Float.equal(earned, max) ? "all-pass" : "some-fail",
      ]),
    ],
    [text(Printf.sprintf("%.1f / %.1f pts", earned, max))],
  );
};

let view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~exercise: Exercise.state,
      ~stitched_dynamics: Exercise.stitched(Exercise.DynamicsItem.t),
      ~highlights,
      ~explainThisModel,
      ~cursor_info,
    ) => {
  let grading_report = GradingReport.mk(exercise.model, ~stitched_dynamics);
  let score_view = grading_report |> GradingReport.overall_score |> score_view;
  let title_view = Cell.title_cell(exercise.header.title);
  let prompt_view =
    Cell.narrative_cell(
      div(~attrs=[Attr.class_("cell-prompt")], [exercise.header.prompt]),
    );
  let (view, cursor_info) =
    switch (exercise, stitched_dynamics, grading_report) {
    | (
        {pos: Programming(pos), model: Programming(eds), _},
        Programming(stitched_dynamics),
        Programming(grading_report),
      ) => (
        ProgrammingView.programming_view(
          ~inject,
          ~ui_state,
          ~settings,
          ~pos,
          ~eds,
          ~grading_report,
          ~stitched_dynamics,
          ~highlights,
        ),
        cursor_info,
      )
    | (
        {pos: Proof(pos), model: Proof(eds), _},
        Proof(stitched_dynamics),
        Proof(grading_report),
      ) =>
      let trees = grading_report.proof_report.verified_tree;
      let info =
        switch (NinjaKeysRules.staged^, NinjaKeysRules.pos^) {
        | (true, Trees(i, pos)) =>
          ExplainThis.Deduction(
            try({
              let tree = List.nth(trees, i);
              let res = Tree.nth(tree, pos);
              let tree = List.nth(eds.trees, i);
              let ed = Tree.nth(tree, pos);
              switch (ed, res) {
              | (Just({rule: Some(rule), _}), {rule: None, _}) =>
                Some({
                  ...res,
                  rule:
                    Some(
                      {
                        open Haz3lcore;
                        print_endline("Uncaught Rule: " ++ Rule.show(rule));
                        let spec = RuleSpec.of_spec(rule);
                        let tests = RuleTest.of_tests(rule);
                        let (spec, tests) =
                          RuleVerify.fill_eq_tests(spec, tests);
                        let tests = RuleVerify.test_remove_eq_test(tests);
                        {rule, spec, tests};
                      },
                    ),
                })
              | _ => Some(res)
              };
            }) {
            | _ => None
            },
          )
        | _ => cursor_info
        };
      let highlights =
        ExplainThis.get_color_map(~settings, ~explainThisModel, info);
      (
        ProofView.proof_view(
          ~inject,
          ~ui_state,
          ~settings,
          ~pos,
          ~eds,
          ~grading_report,
          ~stitched_dynamics,
          ~highlights,
        ),
        info,
      );
    | _ => failwith("Impossible")
    };
  ([score_view, title_view, prompt_view] @ view, cursor_info);
};

let reset_button = inject =>
  Widgets.button_named(
    Icons.trash,
    _ => {
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to reset this exercise? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        inject(UpdateAction.ResetCurrentEditor);
      } else {
        Virtual_dom.Vdom.Effect.Ignore;
      };
    },
    ~tooltip="Reset Exercise",
  );

let instructor_export = (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.star,
    _ => inject(Export(ExerciseModule)),
    ~tooltip="Export Exercise Module",
  );

let instructor_transitionary_export =
    (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.star,
    _ => {inject(Export(TransitionaryExerciseModule))},
    ~tooltip="Export Transitionary Exercise Module",
  );

let instructor_grading_export = (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.star,
    _ => {inject(Export(GradingExerciseModule))},
    ~tooltip="Export Grading Exercise Module",
  );
let export_submission = (inject: UpdateAction.t => Ui_effect.t(unit)) =>
  Widgets.button_named(
    Icons.star,
    _ => inject(Export(Submission)),
    ~tooltip="Export Submission",
  );

let import_submission = (~inject) =>
  Widgets.file_select_button_named(
    "import-submission",
    Icons.star,
    file => {
      switch (file) {
      | None => Virtual_dom.Vdom.Effect.Ignore
      | Some(file) => inject(UpdateAction.InitImportAll(file))
      }
    },
    ~tooltip="Import Submission",
  );
