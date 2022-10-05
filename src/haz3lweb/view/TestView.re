open Virtual_dom.Vdom;
open Node;
open Util.Web;

module TestStatus = Haz3lcore.TestStatus;
module TestMap = Haz3lcore.TestMap;
module TestResults = Haz3lcore.TestResults;
module Interface = Haz3lcore.Interface;

let test_instance_view =
    (~font_metrics, (d, status): TestMap.instance_report) =>
  div(
    ~attr=
      Attr.many([clss(["test-instance", TestStatus.to_string(status)])]),
    [
      DHCode.view_tylr(
        ~settings=Settings.Evaluation.init,
        ~selected_hole_instance=None,
        ~font_metrics,
        ~width=40,
        d,
      ),
    ],
  );

let jump_to_test = (~inject as _, _) => Effect.Ignore;

let test_report_view =
    (
      ~inject,
      ~font_metrics,
      ~description: option(string)=None,
      i: int,
      (_id, instance_reports): TestMap.report,
    ) => {
  let status =
    instance_reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    ~attr=
      Attr.many([
        Attr.class_("test-report"),
        Attr.on_click(jump_to_test(~inject)),
      ]),
    [
      div(
        ~attr=clss(["test-id", "Test" ++ status]),
        // note: prints lexical index, not id
        [text(string_of_int(i + 1))],
      ),
      div(
        ~attr=Attr.class_("test-instances"),
        List.map(test_instance_view(~font_metrics), instance_reports),
      ),
    ]
    @ (
      switch (description) {
      | None => []
      | Some(d) => [div(~attr=clss(["test-description"]), [text(d)])]
      }
    ),
  );
};

let test_reports_view =
    (~inject, ~font_metrics, ~test_results: option(Interface.test_results)) =>
  div(
    ~attr=clss(["panel-body", "test-reports"]),
    switch (test_results) {
    | None => [Node.text("No test report available.")]
    | Some(test_results) =>
      List.mapi(
        (i, r) =>
          test_report_view(
            ~inject,
            ~font_metrics,
            ~description=List.nth_opt(test_results.descriptions, i),
            i,
            r,
          ),
        test_results.test_map,
      )
    },
  );

let test_bar_segment = (~inject, (_id, reports)) => {
  let status = reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    ~attr=
      Attr.many([
        clss(["segment", status]),
        Attr.on_click(jump_to_test(~inject)),
      ]),
    [],
  );
};

let test_bar = (~inject, ~test_results: Interface.test_results) =>
  div(
    ~attr=Attr.class_("test-bar"),
    List.map(test_bar_segment(~inject), test_results.test_map),
  );

// result_summary_str and test_summary_str have been moved to haz3lcore/TestResults.re

let percent_view = (n: int, p: int): Node.t => {
  let percentage =
    n == 0 ? 100. : 100. *. float_of_int(p) /. float_of_int(n);
  div(
    ~attr=clss(["test-percent", n == p ? "all-pass" : "some-fail"]),
    [text(Printf.sprintf("%.0f%%", percentage))],
  );
};

let test_percentage = (test_results: Interface.test_results): Node.t =>
  percent_view(test_results.total, test_results.passing);

let test_text = (test_results: Interface.test_results): Node.t =>
  div(
    ~attr=Attr.class_("test-text"),
    [
      test_percentage(test_results),
      div([text(":")]),
      text(TestResults.test_summary_str(test_results)),
    ],
  );

let test_summary = (~inject, ~test_results: option(Interface.test_results)) => {
  div(
    ~attr=clss(["test-summary"]),
    {
      switch (test_results) {
      | None => [Node.text("No test results available.")]
      | Some(test_results) => [
          test_text(test_results),
          test_bar(~inject, ~test_results),
        ]
      };
    },
  );
};

let view_of_main_title_bar = (title_text: string) =>
  div(
    ~attr=Attr.many([clss(["title-bar", "panel-title-bar"])]),
    [Node.text(title_text)],
  );

let inspector_view =
    (~inject as _, ~font_metrics, ~test_map: TestMap.t, id: int): option(t) => {
  switch (TestMap.lookup(id, test_map)) {
  | Some(instances) when TestMap.joint_status(instances) != Indet =>
    Some(
      div(
        ~attr=Attr.class_("test-inspector"),
        [
          div(
            ~attr=Attr.class_("test-instances"),
            List.map(test_instance_view(~font_metrics), instances),
          ),
        ],
      ),
    )
  | _ => None
  };
};
