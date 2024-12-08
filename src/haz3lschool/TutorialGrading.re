open Haz3lcore;
open Util;

module D = (TutorialEnv: Tutorial.TutorialEnv) => {
  open Tutorial.D(TutorialEnv);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type percentage = float;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type points = float;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type score = (points, points);

  let score_of_percent = (percent, max_points) => {
    let max_points = float_of_int(max_points);
    (percent *. max_points, max_points);
  };

  module ImplGradingReport = {
    type t = {
      hints: list(string),
      test_results: option(TestResults.t),
      hinted_results: list((TestStatus.t, string)),
    };

    let mk = (~hints: list(string), ~test_results: option(TestResults.t)): t => {
      let hinted_results =
        switch (test_results) {
        | Some(test_results) =>
          let statuses = test_results.statuses;
          Util.ListUtil.zip_defaults(
            statuses,
            hints,
            Haz3lcore.TestStatus.Indet,
            "No hint available.",
          );

        | None =>
          Util.ListUtil.zip_defaults(
            [],
            hints,
            Haz3lcore.TestStatus.Indet,
            "Exercise configuration error: Hint without a test.",
          )
        };
      {hints, test_results, hinted_results};
    };

    let total = (report: t) => List.length(report.hinted_results);
    let num_passed = (report: t) => {
      report.hinted_results
      |> List.find_all(((status, _)) => status == TestStatus.Pass)
      |> List.length;
    };

    let percentage = (report: t): float => {
      let passed = float_of_int(num_passed(report));
      let total = float_of_int(total(report));
      if (total == 0.0) {
        0.0; // Avoid division by zero
      } else {
        passed /. total; // Return percentage as a float
      };
    };

    let test_summary_str = (test_results: TestResults.t) => {
      TestResults.result_summary_str(
        ~n=test_results.total,
        ~p=test_results.failing,
        ~q=test_results.unfinished,
        ~n_str="test",
        ~ns_str="tests",
        ~p_str="failing",
        ~q_str="indeterminate",
        ~r_str="valid",
      );
    };
  };

  module GradingReport = {
    type t = {
      // point_distribution,
      // test_validation_report: TestValidationReport.t,
      // mutation_testing_report: MutationTestingReport.t,
      // syntax_report: SyntaxReport.t,
      impl_grading_report: ImplGradingReport.t,
    };

    let mk = (eds: eds, ~stitched_dynamics: stitched(DynamicsItem.t)) => {
      // point_distribution: eds.point_distribution,
      // test_validation_report:
      //   TestValidationReport.mk(
      //     eds,
      //     ModelResult.test_results(stitched_dynamics.test_validation.result),
      //   ),
      // mutation_testing_report:
      //   MutationTestingReport.mk(
      //     ~test_validation=stitched_dynamics.test_validation,
      //     ~hidden_bugs_state=eds.hidden_bugs,
      //     ~hidden_bugs=stitched_dynamics.hidden_bugs,
      //   ),
      // syntax_report:
      //   SyntaxReport.mk(~your_impl=eds.your_impl, ~tests=eds.syntax_tests),
      impl_grading_report:
        ImplGradingReport.mk(
          ~hints=eds.hidden_tests.hints,
          ~test_results=
            ModelResult.test_results(stitched_dynamics.hidden_tests.result),
        ),
    };

    let overall_score =
        (
          {
            // point_distribution,
            // test_validation_report,
            // mutation_testing_report,
            // syntax_report,
            impl_grading_report,
            _,
          }: t,
        )
        : score => {
      // let (tv_points, tv_max) =
      //   score_of_percent(
      //     TestValidationReport.percentage(test_validation_report),
      //     point_distribution.test_validation,
      //   );
      // let (mt_points, mt_max) =
      //   score_of_percent(
      //     MutationTestingReport.percentage(mutation_testing_report),
      //     point_distribution.mutation_testing,
      //   );
      let (ig_points, ig_max) =
        score_of_percent(
          ImplGradingReport.percentage(impl_grading_report),
          1,
        );
      let total_points = ig_points;
      let max_points = ig_max;
      (total_points, max_points);
    };
  };
};
