open Virtual_dom.Vdom;
open Node;
open Util.Web;

let test_instance_view =
    (dhcode_view, (d, status): TestMap.test_instance_report) => {
  let status = TestStatus.to_string(status);
  div([clss(["test-instance", status])], [dhcode_view(d)]);
};

let jump_to_test = (~inject as _, _) => Event.Ignore;

let test_report_view =
    (
      ~inject,
      ~dhcode_view,
      i: int,
      (_id, instance_reports): TestMap.test_report,
    ) => {
  let status =
    instance_reports |> TestMap.joint_status |> TestStatus.to_string;
  div(
    [Attr.class_("test-report"), Attr.on_click(jump_to_test(~inject))],
    [
      div(
        [clss(["test-id", "Test" ++ status])],
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
    (~inject, ~dhcode_view: DHExp.t => Node.t, test_map: TestMap.t) =>
  div(
    [clss(["panel-body", "test-reports"])],
    List.mapi(test_report_view(~inject, ~dhcode_view), test_map),
  );

let test_bar = (~inject, ~test_map: TestMap.t) =>
  div(
    [Attr.class_("test-bar")],
    List.map(
      ((_id, instance_reports)) => {
        let status =
          instance_reports |> TestMap.joint_status |> TestStatus.to_string;
        div(
          [
            clss(["segment", status]),
            Attr.on_click(jump_to_test(~inject)),
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
  | (1, 0, 1) => "One test unfinished "
  | (1, 1, 0) => "One test failing "
  | (2, 1, 1) => "One test failing and one unfinished "
  | (_, 0, 1) => of_n_tests ++ one_unfinished
  | (_, 1, 0) => of_n_tests ++ one_failing
  | (_, 1, 1) => of_n_tests ++ one_failing ++ "and " ++ one_unfinished
  | (_, 1, _) => of_n_tests ++ one_failing ++ "and " ++ mny_unfinished
  | (_, _, 1) => of_n_tests ++ mny_failing ++ "and " ++ one_unfinished
  | (_, 0, _) => of_n_tests ++ mny_unfinished
  | (_, _, 0) => of_n_tests ++ mny_failing
  | (_, _, _) => of_n_tests ++ mny_failing ++ "and " ++ mny_unfinished
  };
};

let test_percentage = (test_map: TestMap.t): t => {
  let total = TestMap.count(test_map);
  let passing = TestMap.count_status(Pass, test_map);
  let percentage = 100. *. float_of_int(passing) /. float_of_int(total);
  div(
    [clss(["test-percent", total == passing ? "all-pass" : "some-fail"])],
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

let test_summary = (~inject, ~test_map) => {
  let failing = TestMap.count_status(Fail, test_map);
  let unfinished = TestMap.count_status(Indet, test_map);
  let status_class =
    switch (failing, unfinished) {
    | (0, 0) => "Pass"
    | (0, _) => "Indet"
    | _ => "Fail"
    };
  div(
    [clss(["test-summary", "instructional-msg", status_class])],
    [test_text(test_map), test_bar(~inject, ~test_map)],
  );
};

let dhcode_view = (~font_metrics) =>
  Interface.dhcode_view(~font_metrics, ~width=40);

let view_of_main_title_bar = (title_text: string) =>
  div([clss(["title-bar", "panel-title-bar"])], [Node.text(title_text)]);

let view =
    (
      ~title: string,
      ~inject=(),
      ~font_metrics,
      d: Elaborator_Exp.ElaborationResult.t,
    )
    : t => {
  let dhcode_view = dhcode_view(~font_metrics);
  let result = Interface.get_result(d);
  switch (result) {
  | None =>
    //print_endline("WARNING: TESTVIREW: no result");
    div([], [])
  | Some((_, test_map)) =>
    /*print_endline("TESTVIEW: some result");
      if (test_map == []) {
        print_endline("TESTMAP EMPTY");
      };*/
    div_if(
      test_map != [],
      [clss(["panel", "test-panel"])],
      [
        view_of_main_title_bar(title),
        test_reports_view(~inject, ~dhcode_view, test_map),
        test_summary(~inject, ~test_map),
      ],
    )
  };
};

let inspector_view =
    (~inject as _, ~font_metrics, ~test_map: TestMap.t, id: int): option(t) => {
  let dhcode_view = dhcode_view(~font_metrics);
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

open Core;

let join_tile = (id): Tile.t => {
  id,
  label: [";"],
  mold: Mold.mk_bin(10, Exp, []),
  shards: [0],
  children: [],
};

let splice_editors = (editors: list(Model.editor)): Segment.t =>
  editors
  |> List.map((ed: Model.editor) => Zipper.unselect_and_zip(ed.zipper))
  |> (
    xs =>
      Util.ListUtil.interleave(
        xs,
        List.init(List.length(editors) - 1, i =>
          [Piece.Tile(join_tile(i + 100000))]
        ) //TODO(andrew): id_gen hack
      )
  )
  |> List.flatten;

let spliced_statics = (eds: list(Model.editor)) => {
  let term = eds |> splice_editors |> Term.uexp_of_seg;
  let (_, _, info_map) = term |> Statics.uexp_to_info_map;
  (term, info_map);
};

let school_panel = (~font_metrics, editors) => {
  switch (editors) {
  | [student_impl, student_tests, teacher_tests] =>
    let (implement_term, implement_map) = spliced_statics([student_impl]);
    let (teacher_term, teacher_map) =
      spliced_statics([student_impl, teacher_tests]);
    let (student_term, student_map) =
      spliced_statics([student_impl, student_tests]);
    div(
      [clss(["test-multi-panel"])],
      [
        view(
          ~title="Student Tests",
          ~font_metrics,
          Elaborator.uexp_elab(student_map, student_term),
        ),
        view(
          ~title="Teacher Tests",
          ~font_metrics,
          Elaborator.uexp_elab(teacher_map, teacher_term),
        ),
        Interface.res_view(~font_metrics, implement_term, implement_map),
      ],
    );
  | _ => div([], [])
  };
};
