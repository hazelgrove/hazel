open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

open Exercise.Programming;
open ProgrammingGradingView;

type vis_marked('a) =
  | InstructorOnly(unit => 'a)
  | Always('a);

let render_cells = (settings: Settings.t, v: list(vis_marked(Node.t))) => {
  List.filter_map(
    vis =>
      switch (vis) {
      | InstructorOnly(f) => settings.instructor_mode ? Some(f()) : None
      | Always(node) => Some(node)
      },
    v,
  );
};

let programming_view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~grading_report: GradingReport.t,
      ~pos: pos,
      ~eds: model(Editor.t),
      ~stitched_dynamics: stitched(Exercise.DynamicsItem.t),
      ~highlights,
    ) => {
  let {
    test_validation,
    user_impl,
    user_tests,
    prelude,
    instructor,
    hidden_bugs,
    hidden_tests: _,
  } = stitched_dynamics;
  let editor_view =
      (
        ~editor: Editor.t,
        ~caption: string,
        ~subcaption: option(string)=?,
        ~footer=?,
        ~di: Exercise.DynamicsItem.t,
        this_pos,
      ) => {
    Cell.editor_view(
      ~selected=(Programming(pos): Exercise.pos) == this_pos,
      ~override_statics=di.statics,
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchEditor(this_pos)],
      ~settings,
      ~highlights,
      ~caption=Cell.caption(caption, ~rest=?subcaption),
      ~target_id=Exercise.show_pos(this_pos),
      ~test_results=ModelResult.test_results(di.result),
      ~footer?,
      ~sort=Exp,
      editor,
    );
  };

  let prelude_view =
    Always(
      editor_view(
        Programming(Prelude),
        ~caption="Prelude",
        ~subcaption=settings.instructor_mode ? "" : " (Read-Only)",
        ~editor=eds.prelude,
        ~di=prelude,
      ),
    );

  let correct_impl_view =
    InstructorOnly(
      () =>
        editor_view(
          Programming(CorrectImpl),
          ~caption="Correct Implementation",
          ~editor=eds.correct_impl,
          ~di=instructor,
        ),
    );
  // determine trailing hole
  // TODO: module
  let correct_impl_ctx_view =
    Always(
      {
        let exp_ctx_view = {
          let correct_impl_trailing_hole_ctx =
            Haz3lcore.Editor.trailing_hole_ctx(
              eds.correct_impl,
              instructor.statics.info_map,
            );
          let prelude_trailing_hole_ctx =
            Haz3lcore.Editor.trailing_hole_ctx(
              eds.prelude,
              prelude.statics.info_map,
            );
          switch (correct_impl_trailing_hole_ctx, prelude_trailing_hole_ctx) {
          | (None, _) => Node.div([text("No context available (1)")])
          | (_, None) => Node.div([text("No context available (2)")]) // TODO show exercise configuration error
          | (
              Some(correct_impl_trailing_hole_ctx),
              Some(prelude_trailing_hole_ctx),
            ) =>
            let specific_ctx =
              Haz3lcore.Ctx.subtract_prefix(
                correct_impl_trailing_hole_ctx,
                prelude_trailing_hole_ctx,
              );
            switch (specific_ctx) {
            | None => Node.div([text("No context available")]) // TODO show exercise configuration error
            | Some(specific_ctx) =>
              ContextInspector.ctx_view(~inject, specific_ctx)
            };
          };
        };
        Cell.simple_cell_view([
          Cell.simple_cell_item([
            Cell.caption(
              "Correct Implementation",
              ~rest=" (Type Signatures Only)",
            ),
            exp_ctx_view,
          ]),
        ]);
      },
    );
  let your_tests_view =
    Always(
      editor_view(
        Programming(YourTests(Validation)),
        ~caption="Test Validation",
        ~subcaption=": Your Tests vs. Correct Implementation",
        ~editor=eds.your_tests.tests,
        ~di=test_validation,
        ~footer=[
          TestValidationReport.view(
            ~inject,
            grading_report.test_validation_report,
            grading_report.point_distribution.test_validation,
          ),
        ],
      ),
    );
  let wrong_impl_views =
    List.mapi(
      (i, ({impl, _}, di)) => {
        InstructorOnly(
          () =>
            editor_view(
              Programming(HiddenBugs(i)),
              ~caption="Wrong Implementation " ++ string_of_int(i + 1),
              ~editor=impl,
              ~di,
            ),
        )
      },
      List.combine(eds.hidden_bugs, hidden_bugs),
    );
  let mutation_testing_view =
    Always(
      MutationTestingReport.view(
        ~inject,
        grading_report.mutation_testing_report,
        grading_report.point_distribution.mutation_testing,
      ),
    );
  let your_impl_view = {
    Always(
      editor_view(
        Programming(YourImpl),
        ~caption="Your Implementation",
        ~editor=eds.your_impl,
        ~di=user_impl,
        ~footer=
          Cell.footer(
            ~locked=false,
            ~settings,
            ~inject,
            ~ui_state,
            ~result=user_impl.result,
            ~result_key=StitchUtil.key(YourImpl),
          ),
      ),
    );
  };
  let syntax_grading_view =
    Always(SyntaxReport.view(grading_report.syntax_report));

  let impl_validation_view =
    Always(
      editor_view(
        Programming(YourTests(Testing)),
        ~caption="Implementation Validation",
        ~subcaption=
          ": Your Tests (synchronized with Test Validation above) vs. Your Implementation",
        ~editor=eds.your_tests.tests,
        ~di=user_tests,
        ~footer=[
          Cell.test_report_footer_view(
            ~inject,
            ~test_results=ModelResult.test_results(user_tests.result),
          ),
        ],
      ),
    );

  let hidden_tests_view =
    InstructorOnly(
      () =>
        editor_view(
          Programming(HiddenTests),
          ~caption="Hidden Tests",
          ~editor=eds.hidden_tests.tests,
          ~di=instructor,
        ),
    );

  let impl_grading_view =
    Always(
      ImplGradingReport.view(
        ~inject,
        ~report=grading_report.impl_grading_report,
        ~syntax_report=grading_report.syntax_report,
        ~max_points=grading_report.point_distribution.impl_grading,
      ),
    );

  render_cells(
    settings,
    [prelude_view, correct_impl_view, correct_impl_ctx_view, your_tests_view]
    @ wrong_impl_views
    @ [
      mutation_testing_view,
      your_impl_view,
      syntax_grading_view,
      impl_validation_view,
      hidden_tests_view,
      impl_grading_view,
    ],
  );
};
