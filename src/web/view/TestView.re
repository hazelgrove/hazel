open Virtual_dom.Vdom;
open Node;
open Util.Web;

//TODO: cleanup

let dhcode_view = (~font_metrics) =>
  Interface.dhcode_view(~font_metrics, ~width=40);

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

let meta_summary_str =
    (~n, ~p, ~q, ~n_str, ~ns_str, ~p_str, ~q_str, ~r_str): string => {
  let one_p = "one is " ++ p_str ++ " ";
  let one_q = "one is " ++ q_str ++ " ";
  let mny_p = Printf.sprintf("%d are %s ", p, p_str);
  let mny_q = Printf.sprintf("%d are %s ", q, q_str);
  let of_n = Printf.sprintf("Out of %d %s, ", n, ns_str);
  switch (n, p, q) {
  | (_, 0, 0) => "All " ++ ns_str ++ " " ++ r_str ++ "! "
  | (n, _, c) when n == c => "All " ++ ns_str ++ " " ++ q_str ++ " "
  | (n, f, _) when n == f => "All " ++ ns_str ++ " " ++ p_str ++ " "
  | (1, 0, 1) => "One " ++ n_str ++ " " ++ q_str ++ " "
  | (1, 1, 0) => "One " ++ n_str ++ " " ++ p_str ++ " "
  | (2, 1, 1) =>
    "One " ++ n_str ++ " " ++ p_str ++ " and one " ++ q_str ++ " "
  | (_, 0, 1) => of_n ++ one_q
  | (_, 1, 0) => of_n ++ one_p
  | (_, 1, 1) => of_n ++ one_p ++ "and " ++ one_q
  | (_, 1, _) => of_n ++ one_p ++ "and " ++ mny_q
  | (_, _, 1) => of_n ++ mny_p ++ "and " ++ one_q
  | (_, 0, _) => of_n ++ mny_q
  | (_, _, 0) => of_n ++ mny_p
  | (_, _, _) => of_n ++ mny_p ++ "and " ++ mny_q
  };
};

let _test_summary_str = (~test_map: TestMap.t): string => {
  let total = TestMap.count(test_map);
  let failing = TestMap.count_status(Fail, test_map);
  let unfinished = TestMap.count_status(Indet, test_map);
  meta_summary_str(
    ~n=total,
    ~p=failing,
    ~q=unfinished,
    ~n_str="test",
    ~ns_str="tests",
    ~p_str="failing",
    ~q_str="unfinished",
    ~r_str="passing",
  );
};

let buggy_summary_str = (~total, ~found): string => {
  meta_summary_str(
    ~n=total,
    ~p=found,
    ~q=0,
    ~n_str="bug",
    ~ns_str="bugs",
    ~p_str="found",
    ~q_str="",
    ~r_str="unrevealed",
  );
};

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
  | (_, 0, 0) => "All tests passing! "
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

let percent_view = (n, p) => {
  let percentage = 100. *. float_of_int(p) /. float_of_int(n);
  div(
    [clss(["test-percent", n == p ? "all-pass" : "some-fail"])],
    [text(Printf.sprintf("%.0f%%", percentage))],
  );
};

let test_percentage = (test_map: TestMap.t): t => {
  let total = TestMap.count(test_map);
  let passing = TestMap.count_status(Pass, test_map);
  percent_view(total, passing);
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
  | None => div([], [])
  | Some((_, test_map)) =>
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
  let term = eds |> splice_editors |> MakeTerm.go;
  let (_, _, info_map) = term |> Statics.mk_map;
  (term, info_map);
};

let get_test_map = eds_to_splice => {
  let (reference_term, reference_map) = spliced_statics(eds_to_splice);
  let d_reference = Elaborator.uexp_elab(reference_map, reference_term);
  let result_reference = Interface.get_result(d_reference);
  switch (result_reference) {
  | None => []
  | Some((_, test_map)) => test_map
  };
};

[@deriving show({with_path: false})]
type blah = list(int);

let buggy_text = (~total, ~found): Node.t =>
  div(
    [Attr.class_("test-text")],
    [
      percent_view(total, found),
      div([], [text(":")]),
      text(buggy_summary_str(~total, ~found)),
    ],
  );

let buggy_bar = (~inject as _, ~total, ~found) =>
  //TODO(andrew)
  div(
    [Attr.class_("test-bar")],
    List.map(
      status => {
        div([clss(["segment", TestStatus.to_string(status)])], [])
      },
      List.init(total, _ =>
        total == found ? TestStatus.Pass : TestStatus.Fail
      ),
    ),
  );

let buggy_summary = (~inject, ~total, ~found) => {
  let status_class = total == found ? "Pass" : "Fail";
  div(
    [clss(["test-summary", "instructional-msg", status_class])],
    [buggy_text(~total, ~found), buggy_bar(~inject, ~total, ~found)],
  );
};

let buggy_view = (~font_metrics, ~inject, reference, wrongs) => {
  ///TODO: clean up this dogshit function
  let reference_test_map = get_test_map(reference);
  let reference_passing: list(int) =
    reference_test_map
    |> List.filter(((_id, reports)) =>
         List.for_all(((_, status)) => status == TestStatus.Pass, reports)
       )
    |> List.split
    |> fst;
  let instances =
    List.map(
      wrong => {
        let wrong_test_map = get_test_map(wrong);
        let wrong_failing: list(int) =
          wrong_test_map
          |> List.filter(((_id, reports)) =>
               List.for_all(
                 ((_, status)) => status == TestStatus.Fail,
                 reports,
               )
             )
          |> List.split
          |> fst;
        let common =
          List.filter(x => List.mem(x, reference_passing), wrong_failing);
        let instance: option(list('a)) =
          switch (common) {
          | [] => None
          | [x, ..._] => List.assoc_opt(x, wrong_test_map)
          };
        switch (instance) {
        | Some([instance, ..._]) => Some(instance)
        | _ => None
        };
      },
      wrongs,
    );
  let instances_view =
    List.map(
      instance => {
        let instance_view =
          switch (instance) {
          | Some(instance) =>
            test_instance_view(dhcode_view(~font_metrics), instance)
          | None => div([], [])
          };
        instance_view;
      },
      instances,
    );

  let total = List.length(instances);
  let found =
    List.length(List.filter((x: option('a)) => x != None, instances));

  div(
    [clss(["panel", "test-panel"])],
    [
      view_of_main_title_bar("Test Coverage:"),
      div(
        [clss(["panel-body", "test-reports"])],
        List.mapi(
          (i, view) =>
            div(
              [
                Attr.class_("test-report"),
                Attr.on_click(jump_to_test(~inject)),
              ],
              [
                div(
                  [clss(["test-id", "Test" ++ "Pass"])], //TODO unhardcode Pass
                  // note: prints lexical index, not id
                  [text(string_of_int(i + 1))],
                ),
                view,
              ],
            ),
          instances_view,
        ),
      ),
      buggy_summary(~inject, ~total, ~found),
    ],
  );
};

let test_section_view = (~font_metrics, ~title, eds) => {
  let (term, map) = spliced_statics(eds);
  view(~title, ~font_metrics, Elaborator.uexp_elab(map, term));
};

let school_panel = (~inject, ~font_metrics, editors) => {
  switch (editors) {
  | [
      student_impl,
      student_tests,
      hidden_tests,
      reference_impl,
      wrong_impl_1,
      wrong_impl_2,
      wrong_impl_3,
    ] =>
    let (implement_term, implement_map) = spliced_statics([student_impl]);
    div(
      [clss(["test-multi-panel"])],
      [
        test_section_view(
          ~title="Your Tests:",
          ~font_metrics,
          [student_impl, student_tests],
        ),
        test_section_view(
          ~title="Our Tests:",
          ~font_metrics,
          [student_impl, hidden_tests],
        ),
        buggy_view(
          ~inject,
          ~font_metrics,
          [reference_impl, student_tests],
          [
            [wrong_impl_1, student_tests],
            [wrong_impl_2, student_tests],
            [wrong_impl_3, student_tests],
          ],
        ),
        Interface.res_view(~font_metrics, implement_term, implement_map),
      ],
    );
  | _ => div([], [])
  };
};
