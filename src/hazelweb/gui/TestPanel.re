open Virtual_dom.Vdom;
open Node;
open ViewUtil;

let test_instance_view =
    (dhcode_view, (d, status): TestMap.test_instance_report) => {
  let status = TestStatus.to_string(status);
  div([Attr.classes(["test-instance", status])], [dhcode_view(d)]);
};

let jump_to_test = (~inject, path, _) =>
  switch (path) {
  | Some(path) =>
    Event.Many([
      inject(ModelAction.FocusCell),
      inject(ModelAction.EditAction(MoveTo(path))),
    ])
  | None => Event.Ignore
  };

let test_report_view =
    (
      ~inject,
      ~dhcode_view,
      ~test_path,
      i: int,
      (id, instance_reports): TestMap.test_report,
    ) => {
  let status =
    instance_reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    [
      Attr.class_("test-report"),
      Attr.on_click(jump_to_test(~inject, test_path(id))),
    ],
    [
      div(
        [Attr.classes(["test-id", "Test" ++ status])],
        // note: prints lexical index, not id
        [text(string_of_int(i + 1))],
      ),
      div(
        [Attr.class_("test-instances")],
        List.map(test_instance_view(dhcode_view), instance_reports),
      ),
    ],
  );
};

let test_reports_view =
    (
      ~inject,
      ~test_path,
      ~dhcode_view: DHExp.t => Node.t,
      test_map: TestMap.t,
    ) =>
  div(
    [Attr.classes(["panel-body", "test-reports"])],
    List.mapi(test_report_view(~inject, ~test_path, ~dhcode_view), test_map),
  );

let test_bar = (~inject, ~test_path, ~test_map: TestMap.t) =>
  div(
    [Attr.class_("test-bar")],
    List.map(
      ((id, instance_reports)) => {
        let status =
          instance_reports |> TestMap.joint_status |> TestStatus.to_string;
        div(
          [
            Attr.classes(["segment", status]),
            Attr.on_click(jump_to_test(~inject, test_path(id))),
          ],
          [],
        );
      },
      test_map,
    ),
  );

let test_summary_str = (~test_map: TestMap.t): string => {
  let total = TestMap.count(test_map);
  let failing = TestMap.count_status(Fail, test_map);
  let unfinished = TestMap.count_status(Indet, test_map);
  let one_failing = "one is failing ";
  let one_unfinished = "one is unfinished ";
  let mny_failing = Printf.sprintf("%d are failing ", failing);
  let mny_unfinished = Printf.sprintf("%d are unfinished ", unfinished);
  let of_n_tests = Printf.sprintf("Out of %d tests, ", total);
  switch (total, failing, unfinished) {
  | (_, 0, 0) => "All tests passing "
  | (n, _, c) when n == c => "All tests unfinished "
  | (n, f, _) when n == f => "All tests failing "
  | (1, 1, 0) => "One failing test "
  | (1, 0, 1) => "One unfinished test "
  | (2, 1, 1) => "One failing and one unfinished test "
  | (_, 0, 1) => of_n_tests ++ one_unfinished
  | (_, 1, 0) => of_n_tests ++ one_failing
  | (_, 1, 1) => of_n_tests ++ one_failing ++ "and " ++ one_unfinished
  | (_, _, 1) => of_n_tests ++ mny_failing ++ "and " ++ one_unfinished
  | (_, 1, _) => of_n_tests ++ one_failing ++ "and " ++ mny_unfinished
  | (_, 0, _) => of_n_tests ++ mny_unfinished
  | (_, _, 0) => of_n_tests ++ mny_failing
  | _ => of_n_tests ++ mny_failing ++ ", and " ++ mny_unfinished
  };
};

let test_percentage = (test_map: TestMap.t): t => {
  let total = TestMap.count(test_map);
  let passing = TestMap.count_status(Pass, test_map);
  let percentage = 100. *. float_of_int(passing) /. float_of_int(total);
  div(
    [
      Attr.classes([
        "test-percent",
        total == passing ? "all-pass" : "some-fail",
      ]),
    ],
    [text(Printf.sprintf("%.0f%%", percentage))],
  );
};

let test_text = (test_map: TestMap.t): Node.t =>
  div(
    [Attr.class_("test-text")],
    [
      test_percentage(test_map),
      div([], [text(":")]),
      text(test_summary_str(~test_map)),
    ],
  );

let test_summary = (~inject, ~test_path, ~test_map) => {
  let failing = TestMap.count_status(Fail, test_map);
  let unfinished = TestMap.count_status(Indet, test_map);
  let status_class =
    switch (failing, unfinished) {
    | (0, 0) => "Pass"
    | (0, _) => "Indet"
    | _ => "Fail"
    };
  div(
    [Attr.classes(["test-summary", "instructional-msg", status_class])],
    [test_text(test_map), test_bar(~inject, ~test_path, ~test_map)],
  );
};

let dhcode_view = (~inject: ModelAction.t => Event.t, ~model: Model.t) =>
  DHCode.view(
    ~inject,
    ~settings=model.settings.evaluation,
    ~selected_instance=Model.get_selected_hole_instance(model),
    ~font_metrics=model.font_metrics,
    ~width=30,
  );

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~model: Model.t,
      ~test_map: TestMap.t,
      ~test_path: KeywordID.t => option(CursorPath.t),
    )
    : t => {
  let dhcode_view = dhcode_view(~inject, ~model);
  div_if(
    test_map != [],
    [Attr.classes(["panel", "test-panel"])],
    [
      Panel.view_of_main_title_bar("Tests"),
      test_reports_view(~inject, ~test_path, ~dhcode_view, test_map),
      test_summary(~inject, ~test_path, ~test_map),
    ],
  );
};

let inspector_view =
    (
      ~inject: ModelAction.t => Event.t,
      ~model: Model.t,
      ~test_map: TestMap.t,
      id: KeywordID.t,
    )
    : option(t) => {
  let dhcode_view = dhcode_view(~inject, ~model);
  switch (TestMap.lookup(id, test_map)) {
  | Some(instances) when TestMap.joint_status(instances) != Indet =>
    Some(
      div(
        [Attr.class_("test-inspector")],
        [
          div(
            [Attr.class_("test-instances")],
            List.map(test_instance_view(dhcode_view), instances),
          ),
        ],
      ),
    )
  | _ => None
  };
};
