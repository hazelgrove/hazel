open Haz3lcore;
open Sexplib.Std;

module F = (ExerciseEnv: SchoolExercise.ExerciseEnv) => {
  open SchoolExercise.F(ExerciseEnv);

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

  module TestValidationReport = {
    type t = {
      test_results: option(TestResults.test_results),
      required: int,
      provided: int,
    };

    let mk = (eds: eds, test_results: option(TestResults.test_results)) => {
      {
        test_results,
        required: eds.your_tests.required,
        provided: eds.your_tests.provided,
      };
    };

    let percentage = (report: t): percentage => {
      switch (report.test_results) {
      | None => 0.0
      | Some(test_results) =>
        let num_tests = float_of_int(test_results.total);
        let required = float_of_int(report.required);
        let provided = float_of_int(report.provided);
        let num_passing = float_of_int(test_results.passing);

        required -. provided <= 0.0 || num_tests <= 0.0
          ? 0.0
          : num_passing
            /. num_tests
            *. (
              Float.max(
                0.,
                Float.min(num_tests -. provided, required -. provided),
              )
              /. (required -. provided)
            );
      };
    };

    let test_summary_str = (test_results: TestResults.test_results) => {
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

  module MutationTestingReport = {
    type t = {results: list((TestStatus.t, string))};

    let hidden_bug_status =
        (
          test_validation_data: DynamicsItem.t,
          hidden_bug_data: DynamicsItem.t,
        )
        : TestStatus.t => {
      switch (
        test_validation_data.simple_result,
        hidden_bug_data.simple_result,
      ) {
      | (None, _)
      | (_, None) => Indet
      | (Some(test_validation_data), Some(hidden_bug_data)) =>
        let validation_test_map = test_validation_data.test_results.test_map;
        let hidden_bug_test_map = hidden_bug_data.test_results.test_map;

        let found =
          hidden_bug_test_map
          |> List.find_opt(((id, instance_reports)) => {
               let status = TestMap.joint_status(instance_reports);
               switch (status) {
               | TestStatus.Pass
               | TestStatus.Indet => false
               | TestStatus.Fail =>
                 let validation_test_reports =
                   validation_test_map |> TestMap.lookup(id);
                 switch (validation_test_reports) {
                 | None => false
                 | Some(reports) =>
                   let status = TestMap.joint_status(reports);
                   switch (status) {
                   | TestStatus.Pass => true
                   | TestStatus.Fail
                   | TestStatus.Indet => false
                   };
                 };
               };
             });
        switch (found) {
        | None => Fail
        | Some(_) => Pass
        };
      };
    }; // for each hidden bug
    //   in the test results data, find a test ID that passes test validation but fails against

    let mk =
        (
          ~test_validation: DynamicsItem.t,
          ~hidden_bugs_state: list(wrong_impl(Editor.t)),
          ~hidden_bugs: list(DynamicsItem.t),
        )
        : t => {
      let results =
        List.map(hidden_bug_status(test_validation), hidden_bugs);
      let hints =
        List.map(
          (wrong_impl: wrong_impl(Editor.t)) => wrong_impl.hint,
          hidden_bugs_state,
        );
      let results = List.combine(results, hints);
      {results: results};
    };

    let percentage = (report: t): percentage => {
      let results = report.results;
      let num_wrong_impls = List.length(results);
      let num_passed =
        results
        |> List.find_all(((status, _)) => status == TestStatus.Pass)
        |> List.length;
      switch (num_wrong_impls) {
      | 0 => 1.0
      | _ => float_of_int(num_passed) /. float_of_int(num_wrong_impls)
      };
    };

    // TODO move to separate module

    let summary_str = (~total, ~found): string => {
      TestResults.result_summary_str(
        ~n=total,
        ~p=found,
        ~q=0,
        ~n_str="bug",
        ~ns_str="bugs",
        ~p_str="exposed",
        ~q_str="",
        ~r_str="unrevealed",
      );
    };
  };

  module ImplGradingReport = {
    type t = {
      hints: list(string),
      test_results: option(TestResults.test_results),
      hinted_results: list((TestStatus.t, string)),
      sr_percentage: float,
    };

    let mk =
        (
          ~hints: list(string),
          ~test_results: option(TestResults.test_results),
          ~sr_percentage: float,
        )
        : t => {
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
      {hints, test_results, hinted_results, sr_percentage};
    };

    let total = (report: t) => List.length(report.hinted_results);
    let num_passed = (report: t) => {
      report.hinted_results
      |> List.find_all(((status, _)) => status == TestStatus.Pass)
      |> List.length;
    };

    let percentage = (report: t): percentage => {
      report.sr_percentage
      *. (float_of_int(num_passed(report)) /. float_of_int(total(report)));
    };

    let test_summary_str = (test_results: TestResults.test_results) => {
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

  module SyntaxReport = {
    type t = {
      hinted_results: list((bool, string)),
      percentage: float,
    };

    let mk = (your_impl: Editor.t, tests: SyntaxTest.syntax_tests): t => {
      let user_impl_term =
        Util.TimeUtil.measure_time("user_impl_term_syntax", true, () =>
          EditorUtil.stitch([your_impl])
        );

      let syntax_results = SyntaxTest.check(user_impl_term, tests);
      {
        hinted_results: syntax_results.hinted_results,
        percentage: syntax_results.percentage,
      };
    };
  };

  module GradingReport = {
    type t = {
      point_distribution,
      syntax_report: SyntaxReport.t,
      test_validation_report: TestValidationReport.t,
      mutation_testing_report: MutationTestingReport.t,
      impl_grading_report: ImplGradingReport.t,
    };

    let mk = (eds: eds, ~stitched_dynamics: stitched(DynamicsItem.t)) => {
      let syntax_report =
        SyntaxReport.mk(eds.your_impl, eds.hidden_tests.syntax_tests);
      {
        point_distribution: eds.point_distribution,
        syntax_report,
        test_validation_report:
          TestValidationReport.mk(
            eds,
            TestResults.unwrap_test_results(
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
              TestResults.unwrap_test_results(
                stitched_dynamics.hidden_tests.simple_result,
              ),
            ~sr_percentage=syntax_report.percentage,
          ),
      };
    };

    let overall_score =
        (
          {
            point_distribution,
            test_validation_report,
            mutation_testing_report,
            impl_grading_report,
            _,
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
  };
};
