open Haz3lcore;
open Virtual_dom.Vdom;
open Node;

type t = {
  exercise: Exercise.state,
  results: option(ModelResults.t),
  settings: Settings.t,
  explainThisModel: ExplainThisModel.t,
  stitched_dynamics: Exercise.stitched(Exercise.DynamicsItem.t),
  grading_report: Grading.GradingReport.t,
};

let mk =
    (
      ~exercise: Exercise.state,
      ~results: option(ModelResults.t),
      ~settings: Settings.t,
      ~explainThisModel,
    )
    : t => {
  let Exercise.{eds, _} = exercise;
  let stitched_dynamics =
    Util.TimeUtil.measure_time("stitch_dynamics", true, () =>
      Exercise.stitch_dynamic(settings.core, exercise, results)
    );

  let grading_report = Grading.GradingReport.mk(eds, ~stitched_dynamics);

  {
    exercise,
    results,
    settings,
    explainThisModel,
    stitched_dynamics,
    grading_report,
  };
};

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

let view =
    (~inject, ~font_metrics, ~show_backpack_targets, ~mousedown, self: t) => {
  let {
    exercise,
    results: _,
    settings,
    stitched_dynamics,
    grading_report,
    explainThisModel,
  } = self;
  let Exercise.{pos, eds} = exercise;
  let Exercise.{
        test_validation,
        user_impl,
        user_tests,
        prelude,
        instructor,
        hidden_bugs,
        hidden_tests: _,
      } = stitched_dynamics;
  let (focal_zipper, focal_info_map) =
    Exercise.focus(exercise, stitched_dynamics);
  let score_view = Grading.GradingReport.view_overall_score(grading_report);
  let color_highlighting: option(ColorSteps.colorMap) =
    if (explainThisModel.highlight && explainThisModel.show) {
      //TODO(andrew): is indicated index appropriate below?
      Some(
        ExplainThis.get_color_map(
          ~doc=explainThisModel,
          None,
          focal_info_map,
        ),
      );
    } else {
      None;
    };

  // partially apply for convenience below
  let editor_view = pos => {
    Cell.editor_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~mousedown,
      ~mousedown_updates=[Update.SwitchEditor(pos)],
      ~settings,
      ~color_highlighting,
    );
  };

  let title_view = Cell.title_cell(eds.title);

  let prompt_view =
    Cell.narrative_cell(
      div(~attr=Attr.class_("cell-prompt"), [eds.prompt]),
    );

  let prelude_view =
    Always(
      editor_view(
        Prelude,
        ~selected=pos == Prelude,
        ~caption=
          Cell.bolded_caption(
            "Prelude",
            ~rest=?settings.instructor_mode ? None : Some(" (Read-Only)"),
          ),
        ~code_id="prelude",
        ~info_map=prelude.info_map,
        ~test_results=
          TestResults.unwrap_test_results(
            ModelResult.get_simple(prelude.result),
          ),
        ~footer=[],
        eds.prelude,
      ),
    );

  let correct_impl_view =
    InstructorOnly(
      () =>
        editor_view(
          CorrectImpl,
          ~selected=pos == CorrectImpl,
          ~caption=Cell.bolded_caption("Correct Implementation"),
          ~code_id="correct-impl",
          ~info_map=instructor.info_map,
          ~test_results=
            TestResults.unwrap_test_results(
              ModelResult.get_simple(instructor.result),
            ),
          ~footer=[],
          eds.correct_impl,
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
              instructor.info_map,
            );
          let prelude_trailing_hole_ctx =
            Haz3lcore.Editor.trailing_hole_ctx(eds.prelude, prelude.info_map);
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
              CtxInspector.ctx_view(~inject, specific_ctx)
            };
          };
        };
        Cell.simple_cell_view([
          Cell.simple_cell_item([
            Cell.bolded_caption(
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
        YourTestsValidation,
        ~selected=pos == YourTestsValidation,
        ~caption=
          Cell.bolded_caption(
            "Test Validation",
            ~rest=": Your Tests vs. Correct Implementation",
          ),
        ~code_id="your-tests",
        ~info_map=test_validation.info_map,
        ~test_results=
          TestResults.unwrap_test_results(
            ModelResult.get_simple(test_validation.result),
          ),
        ~footer=[
          Grading.TestValidationReport.view(
            ~inject,
            grading_report.test_validation_report,
            grading_report.point_distribution.test_validation,
          ),
        ],
        eds.your_tests.tests,
      ),
    );

  let wrong_impl_views =
    List.mapi(
      (
        i,
        (Exercise.{impl, _}, Exercise.DynamicsItem.{info_map, result, _}),
      ) => {
        InstructorOnly(
          () =>
            editor_view(
              HiddenBugs(i),
              ~selected=pos == HiddenBugs(i),
              ~caption=
                Cell.bolded_caption(
                  "Wrong Implementation " ++ string_of_int(i + 1),
                ),
              ~code_id="wrong-implementation-" ++ string_of_int(i + 1),
              ~info_map,
              ~test_results=
                TestResults.unwrap_test_results(
                  ModelResult.get_simple(result),
                ),
              ~footer=[],
              impl,
            ),
        )
      },
      List.combine(eds.hidden_bugs, hidden_bugs),
    );

  let mutation_testing_view =
    Always(
      Grading.MutationTestingReport.view(
        ~inject,
        grading_report.mutation_testing_report,
        grading_report.point_distribution.mutation_testing,
      ),
    );

  let your_impl_view = {
    let simple = ModelResult.get_simple(user_impl.result);
    Always(
      editor_view(
        YourImpl,
        ~selected=pos == YourImpl,
        ~caption=Cell.bolded_caption("Your Implementation"),
        ~code_id="your-impl",
        ~info_map=user_impl.info_map,
        ~test_results=TestResults.unwrap_test_results(simple),
        ~footer=
          Cell.footer(
            ~settings,
            ~inject,
            ~font_metrics,
            ~result_key=Exercise.user_impl_key,
            ~result=user_impl.result,
          ),
        eds.your_impl,
      ),
    );
  };

  let testing_results =
    TestResults.unwrap_test_results(
      ModelResult.get_simple(user_tests.result),
    );

  let syntax_grading_view =
    Always(Grading.SyntaxReport.view(grading_report.syntax_report));

  let impl_validation_view =
    Always(
      editor_view(
        YourTestsTesting,
        ~selected=pos == YourTestsTesting,
        ~caption=
          Cell.bolded_caption(
            "Implementation Validation",
            ~rest=
              ": Your Tests (code synchronized with Test Validation cell above) vs. Your Implementation",
          ),
        ~code_id="your-tests-testing-view",
        ~info_map=user_tests.info_map,
        ~test_results=testing_results,
        ~footer=[
          Cell.test_report_footer_view(
            ~inject,
            ~test_results=testing_results,
          ),
        ],
        eds.your_tests.tests,
      ),
    );

  let hidden_tests_view =
    InstructorOnly(
      () =>
        editor_view(
          HiddenTests,
          ~selected=pos == HiddenTests,
          ~caption=Cell.bolded_caption("Hidden Tests"),
          ~code_id="hidden-tests",
          ~info_map=instructor.info_map,
          ~test_results=
            TestResults.unwrap_test_results(
              ModelResult.get_simple(instructor.result),
            ),
          ~footer=[],
          eds.hidden_tests.tests,
        ),
    );

  let impl_grading_view =
    Always(
      Grading.ImplGradingReport.view(
        ~inject,
        ~report=grading_report.impl_grading_report,
        ~syntax_report=grading_report.syntax_report,
        ~max_points=grading_report.point_distribution.impl_grading,
      ),
    );

  let bottom_bar =
    settings.core.statics
      ? [
        CursorInspector.view(
          ~inject,
          ~settings,
          ~show_explain_this=explainThisModel.show,
          focal_zipper,
          focal_info_map,
        ),
      ]
      : [];
  let sidebar =
    explainThisModel.show && settings.core.statics
      ? ExplainThis.view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=explainThisModel,
          Indicated.ci_of(focal_zipper, focal_info_map),
        )
      : div([]);
  [
    div(
      ~attr=
        Attr.many([
          Attr.id("main"),
          Attr.classes([Settings.show_mode(settings.mode)]),
        ]),
      [
        div(
          ~attr=Attr.classes(["editor", "column"]),
          [score_view, title_view, prompt_view]
          @ render_cells(
              settings,
              [
                prelude_view,
                correct_impl_view,
                correct_impl_ctx_view,
                your_tests_view,
              ]
              @ wrong_impl_views
              @ [
                mutation_testing_view,
                your_impl_view,
                syntax_grading_view,
                impl_validation_view,
                hidden_tests_view,
                impl_grading_view,
              ],
            ),
        ),
      ],
    ),
    sidebar,
  ]
  @ bottom_bar;
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

let instructor_export = (exercise: Exercise.state) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      // .ml files because show uses OCaml syntax (dune handles seamlessly)
      let module_name = exercise.eds.module_name;
      let filename = exercise.eds.module_name ++ ".ml";
      let content_type = "text/plain";
      let contents = Exercise.export_module(module_name, exercise);
      JsUtil.download_string_file(~filename, ~content_type, ~contents);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Exercise Module",
  );

let instructor_transitionary_export = (exercise: Exercise.state) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      // .ml files because show uses OCaml syntax (dune handles seamlessly)
      let module_name = exercise.eds.module_name;
      let filename = exercise.eds.module_name ++ ".ml";
      let content_type = "text/plain";
      let contents =
        Exercise.export_transitionary_module(module_name, exercise);
      JsUtil.download_string_file(~filename, ~content_type, ~contents);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Transitionary Exercise Module",
  );

let instructor_grading_export = (exercise: Exercise.state) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      // .ml files because show uses OCaml syntax (dune handles seamlessly)
      let module_name = exercise.eds.module_name;
      let filename = exercise.eds.module_name ++ "_grading.ml";
      let content_type = "text/plain";
      let contents = Exercise.export_grading_module(module_name, exercise);
      JsUtil.download_string_file(~filename, ~content_type, ~contents);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Grading Exercise Module",
  );

let download_editor_state = (~instructor_mode) =>
  Log.get_and(log => {
    let data = Export.export_all(~instructor_mode, ~log);
    JsUtil.download_json(ExerciseSettings.filename, data);
  });

let export_submission = (~settings: Settings.t) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      download_editor_state(~instructor_mode=settings.instructor_mode);
      Virtual_dom.Vdom.Effect.Ignore;
    },
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
