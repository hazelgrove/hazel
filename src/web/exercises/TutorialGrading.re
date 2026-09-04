open Language;
open Util_web;
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
};

module MutationTestingReport = {
  type t = {results: list((TestStatus.t, string))};
  // TODO move to separate module
};

module SyntaxReport = {
  type t = {
    hinted_results: list((bool, hint)),
    percentage,
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
        Util_web.ListUtil.zip_defaults(
          statuses,
          hints,
          Language.TestStatus.Indet,
          "No hint available.",
        );

      | None =>
        Util_web.ListUtil.zip_defaults(
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

  let individual_report = (i, ~signal_jump, ~hint: string, ~status, (id, _)) =>
    div(
      ~attrs=[
        Attr.classes(["test-report"]),
        Attr.on_click(_ => signal_jump(id)),
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

  let individual_reports = (~signal_jump, ~report) => {
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
               ~signal_jump,
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
  let view = (~signal_jump, ~report: t, ~max_points: int) => {
    CellCommon.panel(
      ~classes=["cell-item", "panel", "test-panel"],
      [
        CellCommon.caption(
          "Implementation Grading",
          ~rest=": Hidden Tests vs. Your Implementation",
        ),
        individual_reports(~signal_jump, ~report),
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
                         ~inject_jump=signal_jump,
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
};
