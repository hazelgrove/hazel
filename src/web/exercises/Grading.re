open Language;
open Util;
open Virtual_dom.Vdom;
open Node;
open Exercise;

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

  let mk = (eds: eds, test_results: option(TestResults.t)) => {
    {
      test_results,
      required: eds.your_tests.required,
      provided: eds.your_tests.provided,
    };
  };

  let percentage = (report: t): percentage => {
    switch (report.test_results) {
    | None => 0.0
    | Some(test_results) =>
      let num_tests = float_of_int(test_results.total);
      let required = float_of_int(report.required);
      let provided = float_of_int(report.provided);
      let num_passing = float_of_int(test_results.passing);

      required -. provided <= 0.0 || num_tests <= 0.0
        ? 0.0
        : num_passing
          /. num_tests
          *. (
            Float.max(
              0.,
              Float.min(num_tests -. provided, required -. provided),
            )
            /. (required -. provided)
          );
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
          let total_tests = test_results.total;
          let required = report.required;
          let num_tests_message =
            total_tests >= required
              ? "at least " ++ string_of_int(required)
              : string_of_int(test_results.total)
                ++ " of "
                ++ string_of_int(report.required);
          text(
            "Entered "
            ++ num_tests_message
            ++ " tests. "
            ++ test_summary_str(test_results),
          );
        },
      ]
    };
  };

  // YourTestsValidation
  let view =
      (
        ~globals: Globals.t,
        ~signal_jump,
        ~signal_editing_test_val_rep,
        ~signal_update_test_val,
        ~signal_textbox_active,
        ~editing_test_val_rep,
        report: t,
        max_points: int,
        max_tests: int,
      ) => {
    CellCommon.report_footer_view([
      div(
        ~attrs=[Attr.classes(["test-summary"])],
        [
          globals.settings.instructor_mode
            ? editing_test_val_rep
                ? div(
                    ~attrs=[Attr.class_("test-text")],
                    [
                      div(
                        ~attrs=[Attr.class_("input-field")],
                        [
                          label([text("Points:")]),
                          input(
                            ~attrs=[
                              Attr.type_("number"),
                              Attr.class_("point-num-input"),
                              Attr.id("point-max-input"),
                              Attr.create("min", "0"),
                              Attr.value(string_of_int(max_points)),
                              Attr.on_focus(_ => signal_textbox_active),
                            ],
                            (),
                          ),
                        ],
                      ),
                      div(
                        ~attrs=[Attr.class_("input-field")],
                        [
                          label([text("Tests required:")]),
                          input(
                            ~attrs=[
                              Attr.type_("number"),
                              Attr.class_("point-num-input"),
                              Attr.id("test-required-input"),
                              Attr.create("min", "0"),
                              Attr.value(string_of_int(max_tests)),
                              Attr.on_focus(_ => signal_textbox_active),
                            ],
                            (),
                          ),
                        ],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(
                            Icons.confirm,
                            _ => {
                              let new_dist =
                                Obj.magic(
                                  Js_of_ocaml.Js.some(
                                    JsUtil.get_elem_by_id("point-max-input"),
                                  ),
                                )##.value;
                              let new_test_num =
                                Obj.magic(
                                  Js_of_ocaml.Js.some(
                                    JsUtil.get_elem_by_id(
                                      "test-required-input",
                                    ),
                                  ),
                                )##.value;

                              let update_events = [
                                signal_editing_test_val_rep,
                                signal_update_test_val(
                                  int_of_string(new_test_num),
                                  int_of_string(new_dist),
                                ),
                              ];
                              Virtual_dom.Vdom.Effect.Many(update_events);
                            },
                          ),
                        ],
                      ),
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.cancel, _ =>
                            signal_editing_test_val_rep
                          ),
                        ],
                      ),
                    ],
                  )
                : div(
                    ~attrs=[Attr.class_("test-text")],
                    [
                      score_view(
                        score_of_percent(percentage(report), max_points),
                      ),
                    ]
                    @ textual_summary(report)
                    @ [
                      div(
                        ~attrs=[Attr.class_("edit-icon")],
                        [
                          Widgets.button(Icons.pencil, _ =>
                            signal_editing_test_val_rep
                          ),
                        ],
                      ),
                    ],
                  )
            : div(
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
                 TestView.test_bar(~inject_jump=signal_jump, ~test_results)
               ),
          ),
      ),
    ]);
  };
};

module MutationTestingReport = {
  type t = {results: list((TestStatus.t, string))};

  let hidden_bug_status =
      (
        test_validation_data: option(TestResults.t),
        hidden_bug_data: option(TestResults.t),
      )
      : TestStatus.t => {
    switch (test_validation_data, hidden_bug_data) {
    | (None, _)
    | (_, None) => Indet
    | (Some(test_validation_data), Some(hidden_bug_data)) =>
      let validation_test_map = test_validation_data.test_map;
      let hidden_bug_test_map = hidden_bug_data.test_map;

      let found =
        hidden_bug_test_map
        |> List.find_opt(((id, instance_reports)) => {
             let status = TestMap.joint_status(instance_reports);
             switch (status) {
             | TestStatus.Pass
             | TestStatus.Indet => false
             | TestStatus.Fail =>
               let validation_test_reports =
                 validation_test_map |> TestMap.lookup(id);
               switch (validation_test_reports) {
               | None => false
               | Some(reports) =>
                 let status = TestMap.joint_status(reports);
                 switch (status) {
                 | TestStatus.Pass => true
                 | TestStatus.Fail
                 | TestStatus.Indet => false
                 };
               };
             };
           });
      switch (found) {
      | None => Fail
      | Some(_) => Pass
      };
    };
  }; // for each hidden bug
  //   in the test results data, find a test ID that passes test validation but fails against

  let mk =
      (
        ~test_validation,
        ~hidden_bugs_state: list(wrong_impl(Haz3lcore.Editor.t)),
        ~hidden_bugs,
      )
      : t => {
    let results = List.map(hidden_bug_status(test_validation), hidden_bugs);
    let hints =
      List.map(
        (wrong_impl: wrong_impl(Haz3lcore.Editor.t)) => wrong_impl.hint,
        hidden_bugs_state,
      );
    let results = List.combine(results, hints);
    {results: results};
  };

  let percentage = (report: t): percentage => {
    let results = report.results;
    let num_wrong_impls = List.length(results);
    let num_passed =
      results
      |> List.find_all(((status, _)) => status == TestStatus.Pass)
      |> List.length;
    switch (num_wrong_impls) {
    | 0 => 1.0
    | _ => float_of_int(num_passed) /. float_of_int(num_wrong_impls)
    };
  };

  // TODO move to separate module

  let summary_str = (~total, ~found): string => {
    TestResults.result_summary_str(
      ~n=total,
      ~p=found,
      ~q=0,
      ~n_str="bug",
      ~ns_str="bugs",
      ~p_str="exposed",
      ~q_str="",
      ~r_str="unrevealed",
    );
  };

  let summary_message =
      (
        ~globals: Globals.t,
        ~editing_mut_test_rep,
        ~select_textbox,
        ~inject_editing_mut_test_rep,
        ~inject_update_mut_test_rep,
        ~score,
        ~total,
        ~found,
        ~max_points,
      )
      : Node.t =>
    div(
      ~attrs=[Attr.class_("test-text")],
      globals.settings.instructor_mode
        ? editing_mut_test_rep
            ? [
              div(
                ~attrs=[Attr.class_("input-field")],
                [
                  label([text("Points:")]),
                  input(
                    ~attrs=[
                      Attr.type_("number"),
                      Attr.class_("point-num-input"),
                      Attr.id("point-max-input"),
                      Attr.value(string_of_int(max_points)),
                      Attr.create("min", "0"),
                      Attr.on_focus(_ => select_textbox),
                    ],
                    (),
                  ),
                ],
              ),
              div(
                ~attrs=[Attr.class_("edit-icon")],
                [
                  Widgets.button(
                    Icons.confirm,
                    _ => {
                      let new_dist =
                        Obj.magic(
                          Js_of_ocaml.Js.some(
                            JsUtil.get_elem_by_id("point-max-input"),
                          ),
                        )##.value;

                      let new_hints =
                        List.init(total, i =>
                          Obj.magic(
                            Js_of_ocaml.Js.some(
                              JsUtil.get_elem_by_id(
                                "hint-input-" ++ string_of_int(i),
                              ),
                            ),
                          )##.value
                        );

                      let update_events = [
                        inject_editing_mut_test_rep,
                        inject_update_mut_test_rep(
                          int_of_string(new_dist),
                          new_hints,
                        ),
                      ];
                      Virtual_dom.Vdom.Effect.Many(update_events);
                    },
                  ),
                ],
              ),
              div(
                ~attrs=[Attr.class_("edit-icon")],
                [
                  Widgets.button(Icons.cancel, _ =>
                    inject_editing_mut_test_rep
                  ),
                ],
              ),
            ]
            : [
              score_view(score),
              text(summary_str(~total, ~found)),
              div(
                ~attrs=[Attr.class_("edit-icon")],
                [
                  Widgets.button(Icons.pencil, _ =>
                    inject_editing_mut_test_rep
                  ),
                ],
              ),
            ]
        : [score_view(score), text(summary_str(~total, ~found))],
    );

  let bar = (~inject as _, instances) =>
    div(
      ~attrs=[Attr.classes(["test-bar"])],
      List.mapi(
        (_id, (status, _)) =>
          div(
            ~attrs=[
              Attr.classes(["segment", TestStatus.to_string(status)]),
              // TODO: Wire up test ids.
            ],
            [],
          ),
        instances,
      ),
    );

  let summary =
      (
        ~globals: Globals.t,
        ~editing_mut_test_rep,
        ~inject_editing_mut_test_rep,
        ~inject_update_mut_test_rep,
        ~select_textbox,
        ~report,
        ~max_points,
      ) => {
    let total = List.length(report.results);
    let found =
      List.length(
        List.filter(((x: TestStatus.t, _)) => x == Pass, report.results),
      );
    let status_class = total == found ? "Pass" : "Fail";
    div(
      ~attrs=[
        Attr.classes([
          "cell-item",
          "test-summary",
          "cell-report",
          status_class,
        ]),
      ],
      [
        summary_message(
          ~globals,
          ~editing_mut_test_rep,
          ~inject_editing_mut_test_rep,
          ~inject_update_mut_test_rep,
          ~select_textbox,
          ~score=score_of_percent(percentage(report), max_points),
          ~total,
          ~found,
          ~max_points,
        ),
        bar(~inject=(), report.results),
      ],
    );
  };

  let individual_report =
      (
        ~i: int,
        ~hint: string,
        ~status,
        ~editing_mut_test_rep,
        ~globals: Globals.t,
        ~select_textbox,
      ) =>
    if (globals.settings.instructor_mode && editing_mut_test_rep) {
      div(
        ~attrs=[
          Attr.classes(["test-report"]),
          //TODO: wire up test ids
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
          // TestView.test_instance_view(~font_metrics, instance),
        ]
        @ [
          label([text("Hint: ")]),
          input(
            ~attrs=[
              //Attr.type_("string"),
              Attr.classes(["test-hint", "test-instance"]),
              Attr.id("hint-input-" ++ string_of_int(i)),
              Attr.value(hint),
              Attr.create("min", "0"),
              Attr.on_focus(_ => select_textbox),
            ],
            (),
          ),
        ],
      );
    } else {
      div(
        ~attrs=[
          Attr.classes(["test-report"]),
          //TODO: wire up test ids
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
          // TestView.test_instance_view(~font_metrics, instance),
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
    };

  let individual_reports =
      (
        coverage_results,
        ~editing_mut_test_rep,
        ~globals: Globals.t,
        ~select_textbox,
      ) =>
    div(
      coverage_results
      |> List.mapi((i, (status, hint)) =>
           individual_report(
             ~i,
             ~hint,
             ~status,
             ~editing_mut_test_rep,
             ~globals,
             ~select_textbox,
           )
         ),
    );

  // let passing_test_ids = test_map =>
  //   test_map
  //   |> List.filter(((_id, reports)) =>
  //        List.for_all(
  //          ((_, status)) => status == Haz3lcore.TestStatus.Pass,
  //          reports,
  //        )
  //      )
  //   |> List.split
  //   |> fst;

  // let failing_test_ids = test_map =>
  //   test_map
  //   |> List.filter(((_id, reports)) =>
  //        List.for_all(
  //          ((_, status)) => status == Haz3lcore.TestStatus.Fail,
  //          reports,
  //        )
  //      )
  //   |> List.split
  //   |> fst;

  // let get_test_map = (editors: list(Haz3lcore.Editor.t)) => {
  //   let (reference_term, reference_map) = spliced_statics(editors);
  //   let result_reference =
  //     Interface.test_results(reference_map, reference_term);
  //   switch (result_reference) {
  //   | None => []
  //   | Some(test_results) => test_results.test_map
  //   };
  // };
  // let show_term = (editor: Editor.t, _) =>
  //   editor.state.zipper
  //   |> Zipper.zip
  //   |> MakeTerm.go
  //   |> fst
  //   |> Exp.show
  //   |> print_endline
  //   |> (_ => Virtual_dom.Vdom.Effect.Ignore);

  // let get_first_common =
  //     (reference_passing, wrong): (TestStatus.t, option('a)) => {
  //   let wrong_test_map = wrong |> get_test_map;
  //   let wrong_failing = wrong_test_map |> failing_test_ids;
  //   let common =
  //     List.filter(x => List.mem(x, reference_passing), wrong_failing);
  //   let instance: option(list('a)) =
  //     switch (common) {
  //     | [] => None
  //     | [x, ..._] => List.assoc_opt(x, wrong_test_map)
  //     };
  //   switch (instance) {
  //   | Some([instance, ..._]) => (TestStatus.Pass, Some(instance))
  //   | _ => (TestStatus.Fail, None)
  //   };
  // };

  let view =
      (
        ~globals: Globals.t,
        ~editing_mut_test_rep,
        ~inject_editing_mut_test_rep,
        ~inject_update_mut_test_rep,
        ~select_textbox,
        report: t,
        max_points: int,
      ) => {
    let subcaption =
      globals.settings.instructor_mode
        ? ": Student Tests vs. Mutant Implementations"
        : ": Your Tests vs. Mutant Implementations (hidden)";
    if (max_points < 0) {
      Node.div([]);
    } else {
      CellCommon.panel(
        ~classes=["test-panel"],
        [
          CellCommon.caption("Mutation Testing", ~rest=subcaption),
          individual_reports(
            report.results,
            ~editing_mut_test_rep,
            ~globals,
            ~select_textbox,
          ),
        ],
        ~footer=
          Some(
            summary(
              ~globals,
              ~editing_mut_test_rep,
              ~inject_editing_mut_test_rep,
              ~inject_update_mut_test_rep,
              ~select_textbox,
              ~report,
              ~max_points,
            ),
          ),
      );
    };
  };
};

module SyntaxReport = {
  type t = {
    hinted_results: list((bool, hint)),
    percentage,
  };

  let mk = (~your_impl: Haz3lcore.Editor.t, ~tests: syntax_tests): t => {
    let user_impl_term =
      Haz3lcore.MakeTerm.from_zip_for_sem(
        your_impl.state.zipper,
        Haz3lcore.Editor.ids_of_refractors(your_impl.state.refractors),
      ).
        term;
    let predicates =
      List.map(((_, p)) => SyntaxTest.predicate_fn(p), tests);
    let hints = List.map(((h, _)) => h, tests);
    let syntax_results = SyntaxTest.check(user_impl_term, predicates);

    {
      hinted_results:
        List.map2((r, h) => (r, h), syntax_results.results, hints),
      percentage: syntax_results.percentage,
    };
  };

  let individual_report =
      (
        i: int,
        hint: string,
        status: bool,
        ~editing_syntax_rep,
        ~globals: Globals.t,
        ~select_textbox,
      ) => {
    let result_string = status ? "Pass" : "Indet";

    if (globals.settings.instructor_mode && editing_syntax_rep) {
      div(
        ~attrs=[Attr.classes(["test-report"])],
        [
          div(
            ~attrs=[Attr.classes(["test-id", "Test" ++ result_string])],
            [text(string_of_int(i + 1))],
          ),
        ]
        @ [
          input(
            ~attrs=[
              Attr.classes(["test-hint", "test-instance"]),
              Attr.id("syntax-hint-input-" ++ string_of_int(i)),
              Attr.value(hint),
              Attr.create("min", "0"),
              Attr.on_focus(_ => select_textbox),
            ],
            (),
          ),
        ],
      );
    } else {
      div(
        ~attrs=[Attr.classes(["test-report"])],
        [
          div(
            ~attrs=[Attr.classes(["test-id", "Test" ++ result_string])],
            [text(string_of_int(i + 1))],
          ),
        ]
        @ [
          div(
            ~attrs=[
              Attr.classes(["test-hint", "test-instance", result_string]),
            ],
            [text(hint)],
          ),
        ],
      );
    };
  };

  let individual_reports =
      (
        hinted_results: list((bool, string)),
        ~editing_syntax_rep,
        ~globals,
        ~select_textbox,
      ) => {
    div(
      hinted_results
      |> List.mapi((i, (status, hint)) =>
           individual_report(
             i,
             hint,
             status,
             ~editing_syntax_rep,
             ~globals,
             ~select_textbox,
           )
         ),
    );
  };

  let view =
      (
        ~globals: Globals.t,
        ~editing_syntax_rep,
        ~inject_set_editing_syntax_rep,
        ~inject_update_syntax_rep,
        ~select_textbox,
        syntax_report: t,
      ) => {
    let subcaption =
      globals.settings.instructor_mode
        ? ": Does student's implementation satisfy the syntactic requirements?"
        : ": Does your implementation satisfy the syntactic requirements?";
    CellCommon.panel(
      ~classes=["test-panel"],
      [
        CellCommon.caption("Syntax Validation", ~rest=subcaption),
        individual_reports(
          syntax_report.hinted_results,
          ~editing_syntax_rep,
          ~globals,
          ~select_textbox,
        ),
      ],
      ~footer=
        Some(
          CellCommon.report_footer_view([
            div(
              ~attrs=[Attr.classes(["test-summary"])],
              [
                div(
                  ~attrs=[Attr.class_("test-text")],
                  globals.settings.instructor_mode
                    ? editing_syntax_rep
                        ? [
                          div(
                            ~attrs=[Attr.class_("edit-icon")],
                            [
                              Widgets.button(
                                Icons.confirm,
                                _ => {
                                  let new_hints =
                                    List.init(
                                      List.length(
                                        syntax_report.hinted_results,
                                      ),
                                      i =>
                                      Obj.magic(
                                        Js_of_ocaml.Js.some(
                                          JsUtil.get_elem_by_id(
                                            "syntax-hint-input-"
                                            ++ string_of_int(i),
                                          ),
                                        ),
                                      )##.value
                                    );

                                  let update_events = [
                                    inject_set_editing_syntax_rep,
                                    inject_update_syntax_rep(new_hints),
                                  ];
                                  Virtual_dom.Vdom.Effect.Many(update_events);
                                },
                              ),
                            ],
                          ),
                          div(
                            ~attrs=[Attr.class_("edit-icon")],
                            [
                              Widgets.button(Icons.cancel, _ =>
                                inject_set_editing_syntax_rep
                              ),
                            ],
                          ),
                        ]
                        : [
                          percentage_view(syntax_report.percentage),
                          text(
                            " of the Implementation Validation points will be earned",
                          ),
                          div(
                            ~attrs=[Attr.class_("edit-icon")],
                            [
                              Widgets.button(Icons.pencil, _ =>
                                inject_set_editing_syntax_rep
                              ),
                            ],
                          ),
                        ]
                    : [
                      percentage_view(syntax_report.percentage),
                      text(
                        " of the Implementation Validation points will be earned",
                      ),
                    ],
                ),
              ],
            ),
          ]),
        ),
    );
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
        Util.ListUtil.zip_defaults(
          statuses,
          hints,
          Language.TestStatus.Indet,
          "No Hint available.",
        );

      | None =>
        Util.ListUtil.zip_defaults(
          [],
          hints,
          Language.TestStatus.Indet,
          "Exercise configuration error: Hint without a test.",
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

  let percentage = (report: t, syntax_report: SyntaxReport.t): percentage => {
    syntax_report.percentage
    *. (float_of_int(num_passed(report)) /. float_of_int(total(report)));
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

  // let summary = (~inject, ~report, ~max_points) => {
  //   let percentage = percentage(report);
  //   let score = score_of_percent(percentage);
  //   let total = total(report);
  //   let num_passed = num_passed(report);
  //   let status_class = total == num_passed ? "Pass" : "Fail";
  //   div(
  //     ~attrs=
  //       Attr.classes([
  //         "cell-item",
  //         "test-summary",
  //         "cell-report",
  //         status_class,
  //       ]),
  //     [
  //       summary_message(
  //         ~score,
  //         ~total,
  //         ~found=num_passed,
  //       ),
  //       bar(~inject, report.results),
  //     ],
  //   );
  // };

  let individual_report =
      (
        i,
        ~signal_jump,
        ~hint: string,
        ~status,
        (id, _),
        ~editing_impl_grd_rep,
        ~globals: Globals.t,
        ~select_textbox,
      ) =>
    if (globals.settings.instructor_mode && editing_impl_grd_rep) {
      print_endline("We are here, trying to edit");
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
          // TestView.test_instance_view(~font_metrics, instance),
        ]
        @ [
          label([text("Hint: ")]),
          input(
            ~attrs=[
              //Attr.type_("string"),
              Attr.classes(["test-hint", "test-instance"]),
              Attr.id("impl-hint-input-" ++ string_of_int(i)),
              Attr.value(hint),
              Attr.create("min", "0"),
              Attr.on_focus(_ => select_textbox),
            ],
            (),
          ),
        ],
      );
    } else {
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
          // TestView.test_instance_view(~font_metrics, instance),
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
    };

  let individual_reports =
      (
        ~signal_jump,
        ~report,
        ~editing_impl_grd_rep,
        ~globals,
        ~select_textbox,
      ) => {
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
               ~editing_impl_grd_rep,
               ~globals,
               ~select_textbox,
             )
           ),
      )
    | _ => div([])
    };
  };

  // HiddenTests
  let view =
      (
        ~globals: Globals.t,
        ~editing_impl_grd_rep,
        ~signal_jump,
        ~inject_set_editing_impl_grd_rep,
        ~inject_update_impl_grd_rep,
        ~select_textbox,
        ~report: t,
        ~syntax_report: SyntaxReport.t,
        ~max_points: int,
      ) => {
    let subcaption =
      globals.settings.instructor_mode
        ? ": Hidden Tests vs. Student's Implementation"
        : ": Hidden Tests vs. Your Implementation";
    CellCommon.panel(
      ~classes=["test-panel"],
      [
        CellCommon.caption("Implementation Grading", ~rest=subcaption),
        individual_reports(
          ~signal_jump,
          ~report,
          ~editing_impl_grd_rep,
          ~globals,
          ~select_textbox,
        ),
      ],
      ~footer=
        Some(
          CellCommon.report_footer_view([
            div(
              ~attrs=[Attr.classes(["test-summary"])],
              [
                globals.settings.instructor_mode
                  ? editing_impl_grd_rep
                      ? div(
                          ~attrs=[Attr.class_("test-text")],
                          [
                            div(
                              ~attrs=[Attr.class_("input-field")],
                              [
                                label([text("Points:")]),
                                input(
                                  ~attrs=[
                                    Attr.type_("number"),
                                    Attr.class_("point-num-input"),
                                    Attr.id("point-max-input"),
                                    Attr.value(string_of_int(max_points)),
                                    Attr.create("min", "0"),
                                    Attr.on_focus(_ => select_textbox),
                                  ],
                                  (),
                                ),
                              ],
                            ),
                            div(
                              ~attrs=[Attr.class_("edit-icon")],
                              [
                                Widgets.button(
                                  Icons.confirm,
                                  _ => {
                                    let new_dist =
                                      Obj.magic(
                                        Js_of_ocaml.Js.some(
                                          JsUtil.get_elem_by_id(
                                            "point-max-input",
                                          ),
                                        ),
                                      )##.value;

                                    let new_hints =
                                      List.init(
                                        List.length(report.hinted_results), i =>
                                        Obj.magic(
                                          Js_of_ocaml.Js.some(
                                            JsUtil.get_elem_by_id(
                                              "impl-hint-input-"
                                              ++ string_of_int(i),
                                            ),
                                          ),
                                        )##.value
                                      );

                                    let update_events = [
                                      inject_set_editing_impl_grd_rep,
                                      inject_update_impl_grd_rep(
                                        int_of_string(new_dist),
                                        new_hints,
                                      ),
                                    ];
                                    Virtual_dom.Vdom.Effect.Many(
                                      update_events,
                                    );
                                  },
                                ),
                              ],
                            ),
                            div(
                              ~attrs=[Attr.class_("edit-icon")],
                              [
                                Widgets.button(Icons.cancel, _ =>
                                  inject_set_editing_impl_grd_rep
                                ),
                              ],
                            ),
                          ],
                        )
                      : div(
                          ~attrs=[Attr.class_("test-text")],
                          [
                            score_view(
                              score_of_percent(
                                percentage(report, syntax_report),
                                max_points,
                              ),
                            ),
                          ]
                          @ textual_summary(report)
                          @ [
                            div(
                              ~attrs=[Attr.class_("edit-icon")],
                              [
                                Widgets.button(Icons.pencil, _ =>
                                  inject_set_editing_impl_grd_rep
                                ),
                              ],
                            ),
                          ],
                        )
                  : div(
                      ~attrs=[Attr.class_("test-text")],
                      [
                        score_view(
                          score_of_percent(
                            percentage(report, syntax_report),
                            max_points,
                          ),
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
  type t = {
    point_distribution,
    test_validation_report: TestValidationReport.t,
    mutation_testing_report: MutationTestingReport.t,
    syntax_report: SyntaxReport.t,
    impl_grading_report: ImplGradingReport.t,
  };

  let mk = (eds: eds, ~stitched_tests: stitched(option(TestResults.t))) => {
    point_distribution: eds.point_distribution,
    test_validation_report:
      TestValidationReport.mk(eds, stitched_tests.test_validation),
    mutation_testing_report:
      MutationTestingReport.mk(
        ~test_validation=stitched_tests.test_validation,
        ~hidden_bugs_state=eds.hidden_bugs,
        ~hidden_bugs=stitched_tests.hidden_bugs,
      ),
    syntax_report:
      SyntaxReport.mk(~your_impl=eds.your_impl, ~tests=eds.syntax_tests),
    impl_grading_report:
      ImplGradingReport.mk(
        ~hints=eds.hidden_tests.hints,
        ~test_results=stitched_tests.hidden_tests,
      ),
  };

  let overall_score =
      (
        {
          point_distribution,
          test_validation_report,
          mutation_testing_report,
          syntax_report,
          impl_grading_report,
          _,
        }: t,
      )
      : score => {
    let (tv_points, tv_max) =
      score_of_percent(
        TestValidationReport.percentage(test_validation_report),
        point_distribution.test_validation,
      );
    let (mt_points, mt_max) =
      score_of_percent(
        MutationTestingReport.percentage(mutation_testing_report),
        point_distribution.mutation_testing,
      );
    let (ig_points, ig_max) =
      score_of_percent(
        ImplGradingReport.percentage(impl_grading_report, syntax_report),
        point_distribution.impl_grading,
      );
    let total_points = tv_points +. mt_points +. ig_points;
    let max_points = tv_max +. mt_max +. ig_max;
    (total_points, max_points);
  };

  let view_overall_score = (report: t) => {
    score_view(overall_score(report));
  };
};
