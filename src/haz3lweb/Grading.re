open Virtual_dom.Vdom;
open Node;

include Haz3lschool.Grading.F(SchoolExercise.ExerciseEnv);

let score_view = ((earned: points, max: points)) => {
  div(
    ~attr=
      Attr.classes([
        "test-percent",
        Float.equal(earned, max) ? "all-pass" : "some-fail",
      ]),
    [text(Printf.sprintf("%.1f / %.1f pts", earned, max))],
  );
};

module TestValidationReport = {
  include TestValidationReport;
  let textual_summary = (report: t) => {
    switch (report.test_results) {
    | None => [Node.text("No test results")]
    | Some(test_results) => [
        {
          let total_tests = test_results.total;
          let required = report.required;
          let num_tests_message =
            total_tests >= required
              ? "at least " ++ string_of_int(required)
              : string_of_int(test_results.total)
                ++ " of "
                ++ string_of_int(report.required);
          text(
            "Entered "
            ++ num_tests_message
            ++ " tests. "
            ++ test_summary_str(test_results),
          );
        },
      ]
    };
  };

  let view = (~inject, report: t, max_points: int) => {
    Cell.report_footer_view([
      div(
        ~attr=Attr.classes(["test-summary"]),
        [
          div(
            ~attr=Attr.class_("test-text"),
            [score_view(score_of_percent(percentage(report), max_points))]
            @ textual_summary(report),
          ),
        ]
        @ Option.to_list(
            report.test_results
            |> Option.map(test_results =>
                 TestView.test_bar(~inject, ~test_results)
               ),
          ),
      ),
    ]);
  };
};

module MutationTestingReport = {
  include MutationTestingReport;
  open Haz3lcore;

  let summary_message = (~score, ~total, ~found): Node.t =>
    div(
      ~attr=Attr.classes(["test-text"]),
      [score_view(score), text(summary_str(~total, ~found))],
    );

  let bar = (~inject as _, instances) =>
    div(
      ~attr=Attr.classes(["test-bar"]),
      List.map(
        ((status, _)) =>
          div(
            ~attr=Attr.classes(["segment", TestStatus.to_string(status)]),
            [],
          ),
        instances,
      ),
    );

  let summary = (~inject, ~report, ~max_points) => {
    let total = List.length(report.results);
    let found =
      List.length(
        List.filter(((x: TestStatus.t, _)) => x == Pass, report.results),
      );
    let status_class = total == found ? "Pass" : "Fail";
    div(
      ~attr=
        Attr.classes([
          "cell-item",
          "test-summary",
          "cell-report",
          status_class,
        ]),
      [
        summary_message(
          ~score=score_of_percent(percentage(report), max_points),
          ~total,
          ~found,
        ),
        bar(~inject, report.results),
      ],
    );
  };

  let individual_report = (i, ~inject, ~hint: string, ~status) =>
    div(
      ~attr=
        Attr.many([
          Attr.classes(["test-report"]),
          Attr.on_click(TestView.jump_to_test(~inject)),
        ]),
      [
        div(
          ~attr=
            Attr.classes([
              "test-id",
              "Test" ++ TestStatus.to_string(status),
            ]),
          /* NOTE: prints lexical index, not unique id */
          [text(string_of_int(i + 1))],
        ),
        // TestView.test_instance_view(~font_metrics, instance),
      ]
      @ [
        div(
          ~attr=
            Attr.classes([
              "test-hint",
              "test-instance",
              TestStatus.to_string(status),
            ]),
          [text(hint)],
        ),
      ],
    );

  let individual_reports = (~inject, coverage_results) =>
    div(
      coverage_results
      |> List.mapi((i, (status, hint)) =>
           individual_report(i, ~inject, ~hint, ~status)
         ),
    );

  // let passing_test_ids = test_map =>
  //   test_map
  //   |> List.filter(((_id, reports)) =>
  //        List.for_all(
  //          ((_, status)) => status == Haz3lcore.TestStatus.Pass,
  //          reports,
  //        )
  //      )
  //   |> List.split
  //   |> fst;

  // let failing_test_ids = test_map =>
  //   test_map
  //   |> List.filter(((_id, reports)) =>
  //        List.for_all(
  //          ((_, status)) => status == Haz3lcore.TestStatus.Fail,
  //          reports,
  //        )
  //      )
  //   |> List.split
  //   |> fst;

  // let get_test_map = (editors: list(Haz3lcore.Editor.t)) => {
  //   let (reference_term, reference_map) = spliced_statics(editors);
  //   let result_reference =
  //     Interface.test_results(reference_map, reference_term);
  //   switch (result_reference) {
  //   | None => []
  //   | Some(test_results) => test_results.test_map
  //   };
  // };
  // let show_term = (editor: Editor.t, _) =>
  //   editor.state.zipper
  //   |> Zipper.zip
  //   |> MakeTerm.go
  //   |> fst
  //   |> Term.UExp.show
  //   |> print_endline
  //   |> (_ => Virtual_dom.Vdom.Effect.Ignore);

  // let get_first_common =
  //     (reference_passing, wrong): (TestStatus.t, option('a)) => {
  //   let wrong_test_map = wrong |> get_test_map;
  //   let wrong_failing = wrong_test_map |> failing_test_ids;
  //   let common =
  //     List.filter(x => List.mem(x, reference_passing), wrong_failing);
  //   let instance: option(list('a)) =
  //     switch (common) {
  //     | [] => None
  //     | [x, ..._] => List.assoc_opt(x, wrong_test_map)
  //     };
  //   switch (instance) {
  //   | Some([instance, ..._]) => (TestStatus.Pass, Some(instance))
  //   | _ => (TestStatus.Fail, None)
  //   };
  // };

  let view = (~inject, report: t, max_points: int) =>
    if (max_points == 0) {
      Node.div([]);
    } else {
      Cell.panel(
        ~classes=["test-panel"],
        [
          Cell.bolded_caption(
            "Mutation Testing",
            ~rest=": Your Tests vs. Buggy Implementations (hidden)",
          ),
          individual_reports(~inject, report.results),
        ],
        ~footer=Some(summary(~inject, ~report, ~max_points)),
      );
    };
};

module ImplGradingReport = {
  open Haz3lcore;
  include ImplGradingReport;
  let textual_summary = (report: t) => {
    switch (report.test_results) {
    | None => [Node.text("No test results")]
    | Some(test_results) => [
        {
          text(test_summary_str(test_results));
        },
      ]
    };
  };

  // let summary = (~inject, ~report, ~max_points) => {
  //   let percentage = percentage(report);
  //   let score = score_of_percent(percentage);
  //   let total = total(report);
  //   let num_passed = num_passed(report);
  //   let status_class = total == num_passed ? "Pass" : "Fail";
  //   div(
  //     ~attr=
  //       Attr.classes([
  //         "cell-item",
  //         "test-summary",
  //         "cell-report",
  //         status_class,
  //       ]),
  //     [
  //       summary_message(
  //         ~score,
  //         ~total,
  //         ~found=num_passed,
  //       ),
  //       bar(~inject, report.results),
  //     ],
  //   );
  // };

  let individual_report = (i, ~inject, ~hint: string, ~status) =>
    div(
      ~attr=
        Attr.many([
          Attr.classes(["test-report"]),
          Attr.on_click(TestView.jump_to_test(~inject)),
        ]),
      [
        div(
          ~attr=
            Attr.classes([
              "test-id",
              "Test" ++ TestStatus.to_string(status),
            ]),
          /* NOTE: prints lexical index, not unique id */
          [text(string_of_int(i + 1))],
        ),
        // TestView.test_instance_view(~font_metrics, instance),
      ]
      @ [
        div(
          ~attr=
            Attr.classes([
              "test-hint",
              "test-instance",
              TestStatus.to_string(status),
            ]),
          [text(hint)],
        ),
      ],
    );

  let individual_reports = (~inject, ~hinted_results) =>
    div(
      hinted_results
      |> List.mapi((i, (status, hint)) =>
           individual_report(i, ~inject, ~hint, ~status)
         ),
    );

  let view = (~inject, ~report: t, ~max_points: int) => {
    Cell.panel(
      ~classes=["cell-item", "panel", "test-panel"],
      [
        Cell.bolded_caption(
          "Implementation Grading",
          ~rest=": Hidden Tests vs. Your Implementation",
        ),
        individual_reports(~inject, ~hinted_results=report.hinted_results),
      ],
      ~footer=
        Some(
          Cell.report_footer_view([
            div(
              ~attr=Attr.classes(["test-summary"]),
              [
                div(
                  ~attr=Attr.class_("test-text"),
                  [
                    score_view(
                      score_of_percent(percentage(report), max_points),
                    ),
                  ]
                  @ textual_summary(report),
                ),
              ]
              @ Option.to_list(
                  report.test_results
                  |> Option.map(test_results =>
                       TestView.test_bar(~inject, ~test_results)
                     ),
                ),
            ),
          ]),
        ),
    );
  };
};

module GradingReport = {
  type t = {
    point_distribution: SchoolExercise.point_distribution,
    test_validation_report: TestValidationReport.t,
    mutation_testing_report: MutationTestingReport.t,
    impl_grading_report: ImplGradingReport.t,
  };

  let mk =
      (
        eds: SchoolExercise.eds,
        ~stitched_dynamics:
           SchoolExercise.stitched(SchoolExercise.DynamicsItem.t),
      ) => {
    point_distribution: eds.point_distribution,
    test_validation_report:
      TestValidationReport.mk(
        eds,
        ModelResult.unwrap_test_results(
          stitched_dynamics.test_validation.simple_result,
        ),
      ),
    mutation_testing_report:
      MutationTestingReport.mk(
        ~test_validation=stitched_dynamics.test_validation,
        ~hidden_bugs_state=eds.hidden_bugs,
        ~hidden_bugs=stitched_dynamics.hidden_bugs,
      ),
    impl_grading_report:
      ImplGradingReport.mk(
        ~hints=eds.hidden_tests.hints,
        ~test_results=
          ModelResult.unwrap_test_results(
            stitched_dynamics.hidden_tests.simple_result,
          ),
      ),
  };

  let overall_score =
      (
        {
          point_distribution,
          test_validation_report,
          mutation_testing_report,
          impl_grading_report,
        }: t,
      )
      : score => {
    let (tv_points, tv_max) =
      score_of_percent(
        TestValidationReport.percentage(test_validation_report),
        point_distribution.test_validation,
      );
    let (mt_points, mt_max) =
      score_of_percent(
        MutationTestingReport.percentage(mutation_testing_report),
        point_distribution.mutation_testing,
      );
    let (ig_points, ig_max) =
      score_of_percent(
        ImplGradingReport.percentage(impl_grading_report),
        point_distribution.impl_grading,
      );
    let total_points = tv_points +. mt_points +. ig_points;
    let max_points = tv_max +. mt_max +. ig_max;
    (total_points, max_points);
  };

  let view_overall_score = (report: t) => {
    score_view(overall_score(report));
  };
};
