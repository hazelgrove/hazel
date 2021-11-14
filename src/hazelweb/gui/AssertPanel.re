open Virtual_dom.Vdom;
open Node;
open ViewUtil;

let test_instance_view =
    (dhcode_view, (d, status): AssertMap.assert_instance_report) => {
  let status = AssertStatus.to_string(status);
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
      ~assert_path,
      i: int,
      (id, instance_reports): AssertMap.assert_report,
    ) => {
  let status =
    instance_reports |> AssertMap.joint_status |> AssertStatus.to_string;
  div(
    [
      Attr.class_("test-report"),
      Attr.on_click(jump_to_test(~inject, assert_path(id))),
    ],
    [
      div(
        [Attr.classes(["test-id", "Assert" ++ status])],
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
      ~assert_path,
      ~dhcode_view: DHExp.t => Node.t,
      assert_map: AssertMap.t,
    ) =>
  div(
    [Attr.classes(["panel-body", "test-reports"])],
    List.mapi(
      test_report_view(~inject, ~assert_path, ~dhcode_view),
      assert_map,
    ),
  );

let test_bar = (~inject, ~assert_path, ~assert_map: AssertMap.t) =>
  div(
    [Attr.class_("test-bar")],
    List.map(
      ((id, instance_reports)) => {
        let status =
          instance_reports |> AssertMap.joint_status |> AssertStatus.to_string;
        div(
          [
            Attr.classes(["segment", status]),
            Attr.on_click(jump_to_test(~inject, assert_path(id))),
          ],
          [],
        );
      },
      assert_map,
    ),
  );

let testing_summary = (~assert_map: AssertMap.t): string => {
  let total = AssertMap.count(assert_map);
  let failing = AssertMap.count_status(Fail, assert_map);
  let unfinished = AssertMap.count_status(Indet, assert_map);
  let one_failing = "one is failing ";
  let one_unfinished = "one is unfinished ";
  let mny_failing = Printf.sprintf("%d are failing ", failing);
  let mny_unfinished = Printf.sprintf("%d are unfinished ", unfinished);
  let of_n_tests = Printf.sprintf("Of %d tests, ", total);
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

let test_summary = (~inject, ~assert_path, ~assert_map) => {
  let failing = AssertMap.count_status(Fail, assert_map);
  let unfinished = AssertMap.count_status(Indet, assert_map);
  let status_class =
    switch (failing, unfinished) {
    | (0, 0) => "Pass"
    | (0, _) => "Indet"
    | _ => "Fail"
    };
  let testing_summary = testing_summary(~assert_map);
  div(
    [Attr.classes(["test-summary", "instructional-msg", status_class])],
    [text(testing_summary), test_bar(~inject, ~assert_path, ~assert_map)],
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
      ~assert_map: AssertMap.t,
      ~assert_path: KeywordID.t => option(CursorPath.t),
    )
    : t => {
  let dhcode_view = dhcode_view(~inject, ~model);
  div_if(
    assert_map != [],
    [Attr.classes(["panel", "test-panel"])],
    [
      Panel.view_of_main_title_bar("Tests"),
      test_reports_view(~inject, ~assert_path, ~dhcode_view, assert_map),
      test_summary(~inject, ~assert_path, ~assert_map),
    ],
  );
};

let inspector_view =
    (
      ~inject: ModelAction.t => Event.t,
      ~model: Model.t,
      ~assert_map: AssertMap.t,
      id: KeywordID.t,
    )
    : option(t) => {
  let dhcode_view = dhcode_view(~inject, ~model);
  switch (AssertMap.lookup(id, assert_map)) {
  | Some(instances) when AssertMap.joint_status(instances) != Indet =>
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
