open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
// open Node;

type vis_marked('a) =
  | InstructorOnly(unit => 'a)
  | Always('a);

// type validation_result =
//   | ValidationSuccess(test_case)
//   | ValidationFailure(test_case, string, string);

// type validation_result =
//   | ValidationSuccess(test_case)
//   | ValidationFailure(test_case, string, string);

let render_cells = (settings: Settings.t, v: list(vis_marked(Node.t))) => {
  List.filter_map(
    vis =>
      switch (vis) {
      | InstructorOnly(f) => settings.instructor_mode ? Some(f()) : None
      | Always(node) => Some(node)
      },
    v,
  );
};

let get_code = (z: Zipper.t): string => {
  let segment = Zipper.unselect_and_zip(z);
  let term = MakeTerm.go(segment).term;
  UExp.show(term); // Convert the term into a string representation
};
let zipper_to_string = (zipper: Haz3lcore.Zipper.t): string => {
  Haz3lcore.Zipper.show(
    zipper /* Automatically derived show function */
  );
};

let editor_to_zipper = (editor: Editor.t): ZipperBase.t => {
  editor.state.
    zipper; /* Access zipper field */
};

let hidden_tests_to_zipper =
    (hidden_tests: Tutorial.hidden_tests(Editor.t)): ZipperBase.t => {
  hidden_tests.tests.state.
    zipper; /* Access zipper field */
};

// let validate_user_impl = (~user_impl: string, ~hidden_tests: test_case) => {
//   let func = test_case => {
//     let expected_output = TestCase.get_expected_output(test_case);
//     let actual_output =
//       if (user_impl == "fib") {
//         expected_output;
//       } else {
//         "error";
//       };

//     if (actual_output == expected_output) {
//       ValidationSuccess(test_case);
//     } else {
//       ValidationFailure(test_case, actual_output, expected_output);
//     };
//   };
//   func(hidden_tests);
// };

let get_code = (z: Zipper.t): string => {
  let segment = Zipper.unselect_and_zip(z);
  let term = MakeTerm.go(segment).term;
  UExp.show(term); // Convert the term into a string representation
};
let zipper_to_string = (zipper: Haz3lcore.Zipper.t): string => {
  Haz3lcore.Zipper.show(
    zipper /* Automatically derived show function */
  );
};

let editor_to_zipper = (editor: Editor.t): ZipperBase.t => {
  editor.state.
    zipper; /* Access zipper field */
};

let hidden_tests_to_zipper =
    (hidden_tests: Tutorial.hidden_tests(Editor.t)): ZipperBase.t => {
  hidden_tests.tests.state.
    zipper; /* Access zipper field */
};

// let convert_editors_to_test_cases =
//     (editors: Tutorial.hidden_tests(Editor.t)): test_case => {
//   let zipper = hidden_tests_to_zipper(editors);
//   let func = editor => {
//     let input_code = get_code(editor); /* Replace with actual extraction logic */
//     TestCase.create(
//       ~input=[input_code],
//       ~expected_output="4", /* Example expected output */
//       ~description=Some("Sample Test Case"),
//     );
//   };
//   func(zipper);
// };

// let validate_user_impl = (~user_impl: string, ~hidden_tests: test_case) => {
//   let func = test_case => {
//     let expected_output = TestCase.get_expected_output(test_case);
//     let actual_output =
//       if (user_impl == "fib") {
//         expected_output;
//       } else {
//         "error";
//       };

//     if (actual_output == expected_output) {
//       ValidationSuccess(test_case);
//     } else {
//       ValidationFailure(test_case, actual_output, expected_output);
//     };
//   };
//   func(hidden_tests);
// };

let view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~tutorial,
      // ~results,
      ~stitched_dynamics,
      // ~results,
      // ~stitched_dynamics,
      ~highlights,
    ) => {
  // editor : Editor.t,

  // let result = ModelResults.lookup(results, result_key);
  let Tutorial.{eds, pos} = tutorial;
  // let stitched_dynamics =
  // Tutorial.stitch_dynamic(
  //   settings.core,
  //   tutorial,
  //   settings.core.dynamics ? Some(results) : None,
  // );
  // let stitched_dynamics =
  //   Tutorial.stitch_dynamic(
  //     settings.core,
  //     tutorial,
  //     settings.core.dynamics ? Some(results) : None,
  //   );
  let {
    test_validation,
    user_impl,
    user_tests,
    // prelude,
    instructor,
    // hidden_bugs,
    hidden_tests,
    // hidden_tests,
  }:
    Tutorial.stitched(Tutorial.DynamicsItem.t) = stitched_dynamics;
  let grading_report =
    TutorialGrading.GradingReport.mk(eds, ~stitched_dynamics);
  // let score_view =
  //   TutorialGrading.GradingReport.view_overall_score(grading_report);

  let editor_view =
      (
        ~editor: Editor.t,
        ~caption: string,
        ~subcaption: option(string)=?,
        ~footer=?,
        ~di: Tutorial.DynamicsItem.t,
        this_pos,
      ) => {
    Cell.editor_view(
      ~selected=pos == this_pos,
      ~override_statics=di.statics,
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchTutEditor(this_pos)],
      ~settings,
      ~highlights,
      ~caption=Cell.caption(caption, ~rest=?subcaption),
      ~target_id=Tutorial.show_pos(this_pos),
      ~test_results=ModelResult.test_results(di.result),
      ~footer?,
      editor,
    );
  };

  let title_view = Cell.title_cell(eds.title);

  let your_impl_view = {
    Always(
      editor_view(
        YourImpl,
        ~caption="Your Implementation",
        ~editor=eds.your_impl,
        ~di=user_impl,
        ~footer=
          Cell.footer(
            ~locked=true,
            ~settings,
            ~inject,
            ~ui_state,
            ~result=user_impl.result,
            ~result_key=Tutorial.user_impl_key,
          ),
      ),
    );
  };

  let your_tests_view =
    Always(
      editor_view(
        YourTestsValidation,
        ~caption="Test Validation",
        ~subcaption=": Your Tests vs. Correct Implementation",
        ~editor=eds.your_tests.tests,
        ~di=test_validation,
        ~footer=[] // TutorialGrading.TestValidationReport.view(
        //   ~inject,
        //   grading_report.test_validation_report,
        //   grading_report.point_distribution.test_validation,
        // ),
      ),
    );

  let hidden_tests_view =
    InstructorOnly(
      () =>
        editor_view(
          HiddenTests,
          ~caption="Hidden Tests",
          ~editor=eds.hidden_tests.tests,
          ~di=instructor,
        ),
    );

  let impl_validation_view =
    Always(
      editor_view(
        YourTestsTesting,
        ~caption="Implementation Validation",
        ~subcaption=
          ": Your Tests (synchronized with Test Validation above) vs. Your Implementation",
        ~editor=eds.your_tests.tests,
        ~di=user_tests,
        ~footer=[
          Cell.test_report_footer_view(
            ~inject,
            ~test_results=ModelResult.test_results(user_tests.result),
          ),
        ],
      ),
    );

  // let impl_validation_view =
  //   Always(
  //     TutorialGrading.ImplGradingReport.view(
  //       ~inject,
  //       ~report=grading_report.impl_grading_report,
  //       ~max_points=4,
  //     ),
  //   );

  [title_view]
  @ render_cells(
      settings,
      [
        your_impl_view,
        hidden_tests_view,
        your_tests_view,
        impl_validation_view,
      ],
    );
};

let reset_button = inject =>
  Widgets.button_named(
    Icons.trash,
    _ => {
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to reset this exercise? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        inject(UpdateAction.ResetCurrentEditor);
      } else {
        Virtual_dom.Vdom.Effect.Ignore;
      };
    },
    ~tooltip="Reset Exercise",
  );

let instructor_export = (exercise: Tutorial.state) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      // .ml files because show uses OCaml syntax (dune handles seamlessly)
      let module_name = exercise.eds.title;
      let filename = exercise.eds.title ++ ".ml";
      let content_type = "text/plain";
      let contents = Tutorial.export_module(module_name, exercise);
      JsUtil.download_string_file(~filename, ~content_type, ~contents);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Exercise Module",
  );

let instructor_transitionary_export = (exercise: Tutorial.state) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      // .ml files because show uses OCaml syntax (dune handles seamlessly)
      let module_name = exercise.eds.title;
      let filename = exercise.eds.title ++ ".ml";
      let content_type = "text/plain";
      let contents =
        Tutorial.export_transitionary_module(module_name, exercise);
      JsUtil.download_string_file(~filename, ~content_type, ~contents);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Transitionary Exercise Module",
  );

let download_editor_state = (~instructor_mode) =>
  Log.get_and(log => {
    let data = Export.export_all(~instructor_mode, ~log);
    JsUtil.download_json(ExerciseSettings.filename, data);
  });

let export_submission = (~settings: Settings.t) =>
  Widgets.button_named(
    Icons.star,
    _ => {
      download_editor_state(~instructor_mode=settings.instructor_mode);
      Virtual_dom.Vdom.Effect.Ignore;
    },
    ~tooltip="Export Submission",
  );

let import_submission = (~inject) =>
  Widgets.file_select_button_named(
    "import-submission",
    Icons.star,
    file => {
      switch (file) {
      | None => Virtual_dom.Vdom.Effect.Ignore
      | Some(file) => inject(UpdateAction.InitImportAll(file))
      }
    },
    ~tooltip="Import Submission",
  );
