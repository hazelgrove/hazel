open Virtual_dom.Vdom;
open Haz3lcore;

module ExerciseEnv = {
  type node = Node.t;
  let default = Node.text("TODO: prompt");
  let output_header = module_name =>
    "let prompt = " ++ module_name ++ "_prompt.prompt\n";
};

include Haz3lschool.SchoolExercise.F(ExerciseEnv);

let spliced_elabs: state => list((ModelResults.key, DHExp.t)) =
  state => {
    let {
      test_validation,
      user_impl,
      user_tests,
      instructor,
      hidden_bugs,
      hidden_tests,
    } =
      stitch_static(state);
    [
      (
        test_validation_key,
        Interface.elaborate(test_validation.info_map, test_validation.term),
      ),
      (
        user_impl_key,
        Interface.elaborate(user_impl.info_map, user_impl.term),
      ),
      (
        user_tests_key,
        Interface.elaborate(user_tests.info_map, user_tests.term),
      ),
      (
        instructor_key,
        Interface.elaborate(instructor.info_map, instructor.term),
      ),
      (
        hidden_tests_key,
        Interface.elaborate(hidden_tests.info_map, hidden_tests.term),
      ),
    ]
    @ (
      hidden_bugs
      |> List.mapi((n, hidden_bug: StaticsItem.t) =>
           (
             hidden_bugs_key(n),
             Interface.elaborate(hidden_bug.info_map, hidden_bug.term),
           )
         )
    );
  };

// DynamicsItem

let stitch_dynamic = (state: state, results: option(ModelResults.t)) => {
  let simple_result_of = key =>
    switch (results) {
    | None => None
    | Some(results) =>
      ModelResult.get_simple(ModelResults.lookup(results, key))
    };
  stitch_dynamic(state, simple_result_of);
};

let focus = (state: state, stitched_dynamics: stitched(DynamicsItem.t)) => {
  let {pos, eds} = state;
  let {
    test_validation,
    user_impl,
    user_tests,
    instructor,
    hidden_bugs,
    hidden_tests,
  } = stitched_dynamics;

  let (focal_zipper, focal_info_map) =
    switch (pos) {
    | Prelude => (eds.prelude.state.zipper, instructor.info_map)
    | CorrectImpl => (eds.correct_impl.state.zipper, instructor.info_map)
    | YourTestsValidation => (
        eds.your_tests.tests.state.zipper,
        test_validation.info_map,
      )
    | YourTestsTesting => (
        eds.your_tests.tests.state.zipper,
        user_tests.info_map,
      )
    | YourImpl => (eds.your_impl.state.zipper, user_impl.info_map)
    | HiddenBugs(idx) =>
      let editor = List.nth(eds.hidden_bugs, idx).impl;
      let info_map = List.nth(hidden_bugs, idx).info_map;
      (editor.state.zipper, info_map);
    | HiddenTests => (
        eds.hidden_tests.tests.state.zipper,
        hidden_tests.info_map,
      )
    };
  (focal_zipper, focal_info_map);
};
