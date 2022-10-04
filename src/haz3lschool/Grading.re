open Haz3lcore;

module F = (ExerciseEnv: SchoolExercise.ExerciseEnv) => {
  open SchoolExercise.F(ExerciseEnv);

  type percentage = float;
  type points = float;
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
};
