open Virtual_dom.Vdom;
open Node;
open Util.Web;

module TestStatus = Haz3lcore.TestStatus;
module TestMap = Haz3lcore.TestMap;
module TestResults = Haz3lcore.TestResults;
module Interface = Haz3lcore.Interface;

let test_instance_view =
    (
      ~settings,
      ~inject,
      ~font_metrics,
      ~infomap,
      {exp: d, status, hint: _hint}: TestMap.instance_report,
    ) =>
  div(
    ~attrs=[clss(["test-instance", TestStatus.to_string(status)])],
    [
      DHCode.view(
        ~inject,
        ~settings,
        ~selected_hole_instance=None,
        ~font_metrics,
        ~width=40,
        ~result_key="",
        ~infomap,
        d,
      ),
    ],
  );

let jump_to_test = (~inject, pos, id, _) => {
  let effect1 = inject(Update.SwitchEditor(pos));
  let effect2 = inject(Update.PerformAction(Jump(TileId(id))));
  Effect.bind(effect1, ~f=_result1 => effect2);
};

let test_report_view =
    (
      ~settings,
      ~inject,
      ~font_metrics,
      ~description: option(string)=None,
      ~infomap,
      i: int,
      (id, instance_reports): TestMap.report,
    ) => {
  let status =
    instance_reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    ~attrs=[
      Attr.class_("test-report"),
      Attr.on_click(jump_to_test(~inject, YourTestsTesting, id)),
    ],
    [
      div(
        ~attrs=[clss(["test-id", "Test" ++ status])],
        // note: prints lexical index, not id
        [text(string_of_int(i + 1))],
      ),
      div(
        ~attrs=[Attr.class_("test-instances")],
        List.map(
          test_instance_view(~infomap, ~settings, ~inject, ~font_metrics),
          instance_reports,
        ),
      ),
    ]
    @ (
      switch (description) {
      | None => []
      | Some(d) => [div(~attrs=[clss(["test-description"])], [text(d)])]
      }
    ),
  );
};

let test_reports_view =
    (
      ~settings,
      ~inject,
      ~font_metrics,
      ~infomap,
      ~test_results: option(TestResults.t),
    ) =>
  div(
    ~attrs=[clss(["panel-body", "test-reports"])],
    switch (test_results) {
    | None => [Node.text("No test report available.")]
    | Some(test_results) =>
      List.mapi(
        (i, r) =>
          test_report_view(
            ~settings,
            ~inject,
            ~font_metrics,
            ~infomap,
            ~description=List.nth_opt(test_results.descriptions, i),
            i,
            r,
          ),
        test_results.test_map,
      )
    },
  );

let test_bar_segment = (~inject, pos, (id, reports)) => {
  let status = reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    ~attrs=[
      clss(["segment", status]),
      Attr.on_click(jump_to_test(~inject, pos, id)),
    ],
    [],
  );
};

let test_bar = (~inject, ~test_results: TestResults.t, pos) =>
  div(
    ~attrs=[Attr.class_("test-bar")],
    List.map(test_bar_segment(~inject, pos), test_results.test_map),
  );

// result_summary_str and test_summary_str have been moved to haz3lcore/TestResults.re

let percent_view = (n: int, p: int): Node.t => {
  let percentage =
    n == 0 ? 100. : 100. *. float_of_int(p) /. float_of_int(n);
  div(
    ~attrs=[clss(["test-percent", n == p ? "all-pass" : "some-fail"])],
    [text(Printf.sprintf("%.0f%%", percentage))],
  );
};

let test_percentage = (test_results: TestResults.t): Node.t =>
  percent_view(test_results.total, test_results.passing);

let test_text = (test_results: TestResults.t): Node.t =>
  div(
    ~attrs=[Attr.class_("test-text")],
    [
      test_percentage(test_results),
      div([text(":")]),
      text(TestResults.test_summary_str(test_results)),
    ],
  );

let test_summary = (~inject, ~test_results: option(TestResults.t)) => {
  div(
    ~attrs=[clss(["test-summary"])],
    {
      switch (test_results) {
      | None => [Node.text("No test results available.")]
      | Some(test_results) => [
          test_text(test_results),
          test_bar(~inject, ~test_results, YourTestsTesting),
        ]
      };
    },
  );
};

let view_of_main_title_bar = (title_text: string) =>
  div(
    ~attrs=[clss(["title-bar", "panel-title-bar"])],
    [Node.text(title_text)],
  );

let inspector_view =
    (
      ~settings,
      ~inject,
      ~font_metrics,
      ~test_map: TestMap.t,
      ~infomap,
      id: Haz3lcore.Id.t,
    )
    : option(t) => {
  switch (TestMap.lookup(id, test_map)) {
  | Some(instances) when TestMap.joint_status(instances) != Indet =>
    Some(
      div(
        ~attrs=[Attr.class_("test-inspector")],
        [
          div(
            ~attrs=[Attr.class_("test-instances")],
            List.map(
              test_instance_view(~settings, ~inject, ~font_metrics, ~infomap),
              instances,
            ),
          ),
        ],
      ),
    )
  | _ => None
  };
};
