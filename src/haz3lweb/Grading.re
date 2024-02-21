open Virtual_dom.Vdom;
open Node;

include Haz3lschool.Grading.F(Exercise.ExerciseEnv);

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

let percentage_view = (p: percentage) => {
  div(
    ~attr=
      Attr.classes([
        "test-percent",
        Float.equal(p, 1.) ? "all-pass" : "some-fail",
      ]),
    [text(Printf.sprintf("%.0f%%", 100. *. p))],
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
                 TestView.test_bar(
                   ~inject,
                   ~test_results,
                   YourTestsValidation,
                 )
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

  let bar = (~inject, instances) =>
    div(
      ~attr=Attr.classes(["test-bar"]),
      List.mapi(
        (id, (status, _)) =>
          div(
            ~attr=
              Attr.many([
                Attr.classes(["segment", TestStatus.to_string(status)]),
                Attr.on_click(
                  //TODO: wire up test ids
                  TestView.jump_to_test(~inject, HiddenBugs(id), Id.invalid),
                ),
              ]),
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

  let individual_report = (id, ~inject, ~hint: string, ~status: TestStatus.t) =>
    div(
      ~attr=
        Attr.many([
          Attr.classes(["test-report"]),
          //TODO: wire up test ids
          Attr.on_click(
            TestView.jump_to_test(~inject, HiddenBugs(id), Id.invalid),
          ),
        ]),
      [
        div(
          ~attr=
            Attr.classes([
              "test-id",
              "Test" ++ TestStatus.to_string(status),
            ]),
          /* NOTE: prints lexical index, not unique id */
          [text(string_of_int(id + 1))],
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
          Cell.caption(
            "Mutation Testing",
            ~rest=": Your Tests vs. Buggy Implementations (hidden)",
          ),
          individual_reports(~inject, report.results),
        ],
        ~footer=Some(summary(~inject, ~report, ~max_points)),
      );
    };
};

module SyntaxReport = {
  include SyntaxReport;
  let individual_report = (i: int, hint: string, status: bool) => {
    let result_string = status ? "Pass" : "Indet";

    div(
      ~attr=Attr.classes(["test-report"]),
      [
        div(
          ~attr=Attr.classes(["test-id", "Test" ++ result_string]),
          [text(string_of_int(i + 1))],
        ),
      ]
      @ [
        div(
          ~attr=Attr.classes(["test-hint", "test-instance", result_string]),
          [text(hint)],
        ),
      ],
    );
  };

  let individual_reports = (hinted_results: list((bool, string))) => {
    div(
      hinted_results
      |> List.mapi((i, (status, hint)) =>
           individual_report(i, hint, status)
         ),
    );
  };

  let view = (syntax_report: t) => {
    Cell.panel(
      ~classes=["test-panel"],
      [
        Cell.caption(
          "Syntax Validation",
          ~rest=
            ": Does your implementation satisfy the syntactic requirements?",
        ),
        individual_reports(syntax_report.hinted_results),
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
                    percentage_view(syntax_report.percentage),
                    text(
                      " of the Implementation Validation points will be earned",
                    ),
                  ],
                ),
              ],
            ),
          ]),
        ),
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

  let individual_report = (i, ~inject, ~hint: string, ~status, (id, _)) =>
    div(
      ~attr=
        Attr.many([
          Attr.classes(["test-report"]),
          Attr.on_click(TestView.jump_to_test(~inject, HiddenTests, id)),
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

  let individual_reports = (~inject, ~report) => {
    switch (report.test_results) {
    | Some(test_results)
        when
          List.length(test_results.test_map)
          == List.length(report.hinted_results) =>
      /* NOTE: This condition will be false when evaluation crashes,
       * for example due to a stack overflow, which may occur in normal operation  */
      div(
        report.hinted_results
        |> List.mapi((i, (status, hint)) =>
             individual_report(
               i,
               ~inject,
               ~hint,
               ~status,
               List.nth(test_results.test_map, i),
             )
           ),
      )
    | _ => div([])
    };
  };

  let view =
      (~inject, ~report: t, ~syntax_report: SyntaxReport.t, ~max_points: int) => {
    Cell.panel(
      ~classes=["cell-item", "panel", "test-panel"],
      [
        Cell.caption(
          "Implementation Grading",
          ~rest=": Hidden Tests vs. Your Implementation",
        ),
        individual_reports(~inject, ~report),
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
                      score_of_percent(
                        percentage(report, syntax_report),
                        max_points,
                      ),
                    ),
                  ]
                  @ textual_summary(report),
                ),
              ]
              @ Option.to_list(
                  report.test_results
                  |> Option.map(test_results =>
                       TestView.test_bar(~inject, ~test_results, HiddenTests)
                     ),
                ),
            ),
          ]),
        ),
    );
  };
};

module GradingReport = {
  include GradingReport;

  let view_overall_score = (report: t) => {
    score_view(overall_score(report));
  };
};
