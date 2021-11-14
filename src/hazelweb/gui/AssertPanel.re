open Virtual_dom.Vdom;
open Node;
open ViewUtil;

let assert_instance_view =
    (dhcode_view, (d, status): AssertMap.assert_instance_report) => {
  let status = AssertStatus.to_string(status);
  div(
    [Attr.classes(["test-instance", "Assert" ++ status])],
    [dhcode_view(d)],
  );
};

let assert_report_view =
    (
      ~inject,
      ~dhcode_view,
      ~assert_path,
      (id, instance_reports): AssertMap.assert_report,
    ) => {
  let status =
    instance_reports |> AssertMap.joint_status |> AssertStatus.to_string;
  div(
    [Attr.class_("test-report")],
    [
      div(
        [
          Attr.classes(["test-id", "Assert" ++ status]),
          Attr.on_click(_ =>
            switch (assert_path(id)) {
            | Some(path) =>
              Event.Many([
                inject(ModelAction.FocusCell),
                inject(ModelAction.EditAction(MoveTo(path))),
              ])

            | None => Event.Ignore
            }
          ),
        ],
        [text(string_of_int(id))],
      ),
      div(
        [Attr.class_("test-instances")],
        List.map(assert_instance_view(dhcode_view), instance_reports),
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
    List.map(
      assert_report_view(~inject, ~assert_path, ~dhcode_view),
      assert_map,
    ),
  );

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~model: Model.t,
      ~assert_map: AssertMap.t,
      ~assert_path: KeywordID.t => option(CursorPath.t),
    )
    : t => {
  let dhcode_view =
    DHCode.view(
      ~inject,
      ~settings=model.settings.evaluation,
      ~selected_instance=Model.get_selected_hole_instance(model),
      ~font_metrics=model.font_metrics,
      ~width=30,
    );
  div_if(
    assert_map != [],
    [Attr.classes(["panel", "test-panel"])],
    [
      Panel.view_of_main_title_bar("Tests"),
      test_reports_view(~inject, ~assert_path, ~dhcode_view, assert_map),
    ],
  );
};
