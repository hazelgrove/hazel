open Virtual_dom.Vdom;
open Node;
open Util.Web;

module TestStatus = Haz3lcore.TestStatus;
module TestMap = Haz3lcore.TestMap;
module TestResults = Haz3lcore.TestResults;
module Interface = Haz3lcore.Interface;

let test_bar_segment = (~inject_jump, (id, reports)) => {
  let status = reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    ~attr=
      Attr.many([
        clss(["segment", status]),
        Attr.on_click(_ => inject_jump(id)),
      ]),
    [],
  );
};

let test_bar = (~inject_jump, ~test_results: TestResults.t) =>
  div(
    ~attr=Attr.class_("test-bar"),
    List.map(test_bar_segment(~inject_jump), test_results.test_map),
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

let test_percentage = (test_results: TestResults.t): Node.t =>
  percent_view(test_results.total, test_results.passing);

let test_text = (test_results: TestResults.t): Node.t =>
  div(
    ~attr=Attr.class_("test-text"),
    [
      test_percentage(test_results),
      div([text(":")]),
      text(TestResults.test_summary_str(test_results)),
    ],
  );

let test_summary = (~inject_jump, ~test_results: option(TestResults.t)) => {
  div(
    ~attr=clss(["test-summary"]),
    {
      switch (test_results) {
      | None => [Node.text("No test results available.")]
      | Some(test_results) => [
          test_text(test_results),
          test_bar(~inject_jump, ~test_results),
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
