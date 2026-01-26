open Language;
open Util;
open Virtual_dom.Vdom;
open Node;
open Tutorial;

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

let score_view = ((earned: points, max: points)) => {
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

let percentage_view = (p: percentage) => {
  div(
    ~attrs=[
      Attr.classes([
        "test-percent",
        Float.equal(p, 1.) ? "all-pass" : "some-fail",
      ]),
    ],
    [text(Printf.sprintf("%.0f%%", 100. *. p))],
  );
};

module TestValidationReport = {
  type t = {
    test_results: option(TestResults.t),
    required: int,
    provided: int,
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

  let view = (/*~signal_jump,*/ report: t, max_points: int) => {
    CellCommon.report_footer_view([
      div(
        ~attrs=[Attr.classes(["test-summary"])],
        [
          div(
            ~attrs=[Attr.class_("test-text")],
            [score_view(score_of_percent(percentage(report), max_points))]
            @ textual_summary(report),
          ),
        ]
        @ Option.to_list(
            report.test_results
            |> Option.map(test_results =>
                 TestView.test_bar(
                   /*~inject_jump=signal_jump,*/
                   ~test_results,
                 )
               ),
          ),
      ),
    ]);
  };
};

module MutationTestingReport = {
  type t = {results: list((TestStatus.t, string))};

  let hidden_bug_status =
      (
        test_validation_data: option(TestResults.t),
        hidden_bug_data: option(TestResults.t),
      )
      : TestStatus.t => {
    switch (test_validation_data, hidden_bug_data) {
    | (None, _)
    | (_, None) => Indet
    | (Some(test_validation_data), Some(hidden_bug_data)) =>
      let validation_test_map = test_validation_data.test_map;
      let hidden_bug_test_map = hidden_bug_data.test_map;

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
        ~test_validation,
        ~hidden_bugs_state: list(wrong_impl(Haz3lcore.Editor.t)),
        ~hidden_bugs,
      )
      : t => {
    let results = List.map(hidden_bug_status(test_validation), hidden_bugs);
    let hints =
      List.map(
        (wrong_impl: wrong_impl(Haz3lcore.Editor.t)) => wrong_impl.hint,
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

  let summary_message = (~score, ~total, ~found): Node.t =>
    div(
      ~attrs=[Attr.classes(["test-text"])],
      [score_view(score), text(summary_str(~total, ~found))],
    );

  let bar = (~inject as _, instances) =>
    div(
      ~attrs=[Attr.classes(["test-bar"])],
      List.mapi(
        (_id, (status, _)) =>
          div(
            ~attrs=[
              Attr.classes(["segment", TestStatus.to_string(status)]),
              // TODO: Wire up test ids.
            ],
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
      ~attrs=[
        Attr.classes([
          "cell-item",
          "test-summary",
          "cell-report",
          status_class,
        ]),
      ],
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

  let individual_report =
      (id, ~inject as _, ~hint: string, ~status: TestStatus.t) =>
    div(
      ~attrs=[
        Attr.classes(["test-report"]),
        //TODO: wire up test ids
      ],
      [
        div(
          ~attrs=[
            Attr.classes([
              "test-id",
              "Test" ++ TestStatus.to_string(status),
            ]),
          ],
          /* NOTE: prints lexical index, not unique id */
          [text(string_of_int(id + 1))],
        ),
      ]
      @ [
        div(
          ~attrs=[
            Attr.classes([
              "test-hint",
              "test-instance",
              TestStatus.to_string(status),
            ]),
          ],
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

  let view = (~inject, report: t, max_points: int) =>
    if (max_points == 0) {
      Node.div([]);
    } else {
      CellCommon.panel(
        ~classes=["test-panel"],
        [
          CellCommon.caption(
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
  type t = {
    hinted_results: list((bool, hint)),
    percentage,
  };

  let mk = (~your_impl: Haz3lcore.Editor.t, ~tests: syntax_tests): t => {
    let user_impl_term =
      Haz3lcore.MakeTerm.from_zip_for_sem(your_impl.state.zipper).term;

    let predicates =
      List.map(((_, p)) => SyntaxTest.predicate_fn(p), tests);
    let hints = List.map(((h, _)) => h, tests);
    let syntax_results = SyntaxTest.check(user_impl_term, predicates);

    {
      hinted_results:
        List.map2((r, h) => (r, h), syntax_results.results, hints),
      percentage: syntax_results.percentage,
    };
  };

  let individual_report = (i: int, hint: string, status: bool) => {
    let result_string = status ? "Pass" : "Indet";

    div(
      ~attrs=[Attr.classes(["test-report"])],
      [
        div(
          ~attrs=[Attr.classes(["test-id", "Test" ++ result_string])],
          [text(string_of_int(i + 1))],
        ),
      ]
      @ [
        div(
          ~attrs=[
            Attr.classes(["test-hint", "test-instance", result_string]),
          ],
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
    CellCommon.panel(
      ~classes=["test-panel"],
      [
        CellCommon.caption(
          "Syntax Validation",
          ~rest=
            ": Does your implementation satisfy the syntactic requirements?",
        ),
        individual_reports(syntax_report.hinted_results),
      ],
      ~footer=
        Some(
          CellCommon.report_footer_view([
            div(
              ~attrs=[Attr.classes(["test-summary"])],
              [
                div(
                  ~attrs=[Attr.class_("test-text")],
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
          Language.TestStatus.Indet,
          "No hint available.",
        );

      | None =>
        Util.ListUtil.zip_defaults(
          [],
          hints,
          Language.TestStatus.Indet,
          "Tutorial configuration error: Hint without a test.",
        )
      };
    {
      hints,
      test_results,
      hinted_results,
    };
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

  let individual_report =
      (i /*~signal_jump,*/, ~hint: string, ~status, (_id, _)) =>
    div(
      ~attrs=[
        Attr.classes(["test-report"]),
        // Attr.on_click(_ => signal_jump(id)),
      ],
      [
        div(
          ~attrs=[
            Attr.classes([
              "test-id",
              "Test" ++ TestStatus.to_string(status),
            ]),
          ],
          /* NOTE: prints lexical index, not unique id */
          [text(string_of_int(i + 1))],
        ),
      ]
      @ [
        div(
          ~attrs=[
            Attr.classes([
              "test-hint",
              "test-instance",
              TestStatus.to_string(status),
            ]),
          ],
          [text(hint)],
        ),
      ],
    );

  let individual_reports = /*~signal_jump,*/ (~report) => {
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
               /*~signal_jump,*/
               ~hint,
               ~status,
               List.nth(test_results.test_map, i),
             )
           ),
      )
    | _ => div([])
    };
  };

  // HiddenTests
  let view = /*~signal_jump,*/ (~report: t, ~max_points: int) => {
    CellCommon.panel(
      ~classes=["cell-item", "panel", "test-panel"],
      [
        CellCommon.caption(
          "Implementation Grading",
          ~rest=": Hidden Tests vs. Your Implementation",
        ),
        individual_reports(
          /*~signal_jump,*/
          ~report,
        ),
      ],
      ~footer=
        Some(
          CellCommon.report_footer_view([
            div(
              ~attrs=[Attr.classes(["test-summary"])],
              [
                div(
                  ~attrs=[Attr.class_("test-text")],
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
                       TestView.test_bar(
                         /*~inject_jump=signal_jump,*/
                         ~test_results,
                       )
                     ),
                ),
            ),
          ]),
        ),
    );
  };
};

module GradingReport = {
  type t = {impl_grading_report: ImplGradingReport.t};

  let mk = (eds: eds, ~stitched_tests: stitched(option(TestResults.t))) => {
    impl_grading_report:
      ImplGradingReport.mk(
        ~hints=eds.hidden_tests.hints,
        ~test_results=stitched_tests.hidden_tests,
      ),
  };

  let overall_score = ({impl_grading_report, _}: t): score => {
    let (ig_points, ig_max) =
      score_of_percent(ImplGradingReport.percentage(impl_grading_report), 1);
    let total_points = ig_points;
    let max_points = ig_max;
    (total_points, max_points);
  };

  let view_overall_score = (report: t) => {
    score_view(overall_score(report));
  };
};
