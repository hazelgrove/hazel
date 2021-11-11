module Vdom = Virtual_dom.Vdom;
open Vdom;
open Node;

let div_if = (p, ats, ns) => p ? div(ats, ns) : div([], []);

let assert_instance_view =
    (dhcode_view, (d, status): AssertMap.assert_instance_report) => {
  let status = AssertStatus.to_string(status);
  div(
    [Attr.classes(["test-instance", "Assert" ++ status])],
    [dhcode_view(d)],
  );
};

let assert_report_view =
    (dhcode_view, (id, instance_reports): AssertMap.assert_report) => {
  let status =
    instance_reports |> AssertMap.joint_status |> AssertStatus.to_string;
  div(
    [Attr.class_("test-report")],
    [div([Attr.class_("Assert" ++ status)], [text(string_of_int(id))])]
    @ List.map(assert_instance_view(dhcode_view), instance_reports),
  );
};

let test_reports_view = (dhcode_view, assert_map) =>
  div(
    [Attr.classes(["panel-body", "test-reports"])],
    List.map(assert_report_view(dhcode_view), assert_map),
  );

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~model: Model.t,
      ~assert_map: AssertMap.t,
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
      test_reports_view(dhcode_view, assert_map),
    ],
  );
};
