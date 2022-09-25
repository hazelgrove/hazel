open Virtual_dom.Vdom;
open Node;

type percentage = float;
type points = float;
type score = (points, points);

let score_of_percent = (percent, max_points) => {
  let max_points = float_of_int(max_points);
  (percent *. max_points, max_points);
};

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
  type t = {
    test_results: option(Interface.test_results),
    num_required: int,
    minimum: int,
  };

  let mk =
      (eds: SchoolExercise.eds, test_results: option(Interface.test_results)) => {
    {
      test_results,
      num_required: eds.your_tests.num_required,
      minimum: eds.your_tests.minimum,
    };
  };

  let percentage = (report: t): percentage => {
    switch (report.test_results) {
    | None => 0.0
    | Some(test_results) =>
      let num_tests = float_of_int(test_results.total);
      let num_required = float_of_int(report.num_required);
      let minimum = float_of_int(report.minimum);
      let num_passing = float_of_int(test_results.passing);

      num_required -. minimum <= 0.0 || num_tests <= 0.0
        ? 0.0
        : num_passing
          /. num_tests
          *. (
            Float.max(
              0.,
              Float.min(num_tests -. minimum, num_required -. minimum),
            )
            /. (num_required -. minimum)
          );
    };
  };

  let test_summary_str = (test_results: Interface.test_results) => {
    TestView.result_summary_str(
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

  let textual_summary = (report: t) => {
    switch (report.test_results) {
    | None => [Node.text("No test results")]
    | Some(test_results) => [
        {
          let total_tests = test_results.total;
          let num_required = report.num_required;
          let num_tests_message =
            total_tests >= num_required
              ? "at least " ++ string_of_int(num_required)
              : string_of_int(test_results.total)
                ++ " of "
                ++ string_of_int(report.num_required);
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
  open Haz3lcore;
  open SchoolExercise;
  type t = {results: list((TestStatus.t, string))};
  module DynamicsItem = SchoolExercise.DynamicsItem;

  let hidden_bug_status =
      (test_validation_data: DynamicsItem.t, hidden_bug_data: DynamicsItem.t)
      : TestStatus.t => {
    switch (test_validation_data.simple_result, hidden_bug_data.simple_result) {
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
        ~test_validation: SchoolExercise.DynamicsItem.t,
        ~hidden_bugs_state: list(SchoolExercise.wrong_impl(Editor.t)),
        ~hidden_bugs: list(SchoolExercise.DynamicsItem.t),
      )
      : t => {
    let results = List.map(hidden_bug_status(test_validation), hidden_bugs);
    let hints =
      List.map(
        (wrong_impl: SchoolExercise.wrong_impl(Editor.t)) => wrong_impl.hint,
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
    TestView.result_summary_str(
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

  type t = {
    hints: list(string),
    test_results: option(Interface.test_results),
    hinted_results: list((TestView.TestStatus.t, string)),
  };

  let mk =
      (~hints: list(string), ~test_results: option(Interface.test_results))
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
    {hints, test_results, hinted_results};
  };

  let total = (report: t) => List.length(report.hinted_results);
  let num_passed = (report: t) => {
    report.hinted_results
    |> List.find_all(((status, _)) => status == TestStatus.Pass)
    |> List.length;
  };

  let percentage = (report: t): percentage => {
    float_of_int(num_passed(report)) /. float_of_int(total(report));
  };

  let test_summary_str = (test_results: Interface.test_results) => {
    TestView.result_summary_str(
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
