open Virtual_dom.Vdom;
open Haz3lcore;

include Haz3lschool.SchoolData.SchoolExercise({
  type node = Node.t;
  let default = Node.text("TODO: prompt");
  let output_header = _ => "";
});

// # Stitching

module StaticsItem = {
  type t = {
    term: TermBase.UExp.t,
    info_map: Statics.map,
  };
};

type stitched('a) = {
  test_validation: 'a, // prelude + correct_impl + your_tests
  user_impl: 'a, // prelude + your_impl
  user_tests: 'a, // prelude + your_impl + your_tests
  instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
  hidden_bugs: list('a), // prelude + hidden_bugs[i].impl + your_tests,
  hidden_tests: 'a,
};

type stitched_statics = stitched(StaticsItem.t);

let stitch_static = ({eds, _}: state): stitched_statics => {
  let (test_validation_term, _) =
    EditorUtil.stitch([eds.prelude, eds.correct_impl, eds.your_tests.tests]);
  let test_validation_map = Statics.mk_map(test_validation_term);
  let test_validation =
    StaticsItem.{term: test_validation_term, info_map: test_validation_map};

  let (user_impl_term, _) = EditorUtil.stitch([eds.prelude, eds.your_impl]);
  let user_impl_map = Statics.mk_map(user_impl_term);
  let user_impl = StaticsItem.{term: user_impl_term, info_map: user_impl_map};

  let (user_tests_term, _) =
    EditorUtil.stitch([eds.prelude, eds.your_impl, eds.your_tests.tests]);
  let user_tests_map = Statics.mk_map(user_tests_term);
  let user_tests =
    StaticsItem.{term: user_tests_term, info_map: user_tests_map};

  let (instructor_term, _) =
    EditorUtil.stitch([
      eds.prelude,
      eds.correct_impl,
      eds.hidden_tests.tests,
    ]);
  let instructor_info_map = Statics.mk_map(instructor_term);
  let instructor =
    StaticsItem.{term: instructor_term, info_map: instructor_info_map};

  let hidden_bugs =
    List.map(
      ({impl, _}) => {
        let (term, _) =
          EditorUtil.stitch([eds.prelude, impl, eds.your_tests.tests]);
        let info_map = Statics.mk_map(term);
        StaticsItem.{term, info_map};
      },
      eds.hidden_bugs,
    );

  let (hidden_tests_term, _) =
    EditorUtil.stitch([eds.prelude, eds.your_impl, eds.hidden_tests.tests]);
  let hidden_tests_map = Statics.mk_map(hidden_tests_term);
  let hidden_tests =
    StaticsItem.{term: hidden_tests_term, info_map: hidden_tests_map};

  {
    test_validation,
    user_impl,
    user_tests,
    instructor,
    hidden_bugs,
    hidden_tests,
  };
};

let test_validation_key = "test_validation";
let user_impl_key = "user_impl";
let user_tests_key = "user_tests";
let instructor_key = "instructor";
let hidden_bugs_key = n => "hidden_bugs_" ++ string_of_int(n);
let hidden_tests_key = "hidden_tests";

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

module DynamicsItem = {
  type t = {
    term: TermBase.UExp.t,
    info_map: Statics.map,
    simple_result: ModelResult.simple,
  };
};
let stitch_dynamic = (state: state, results: option(ModelResults.t)) => {
  let {
    test_validation,
    user_impl,
    user_tests,
    instructor,
    hidden_bugs,
    hidden_tests,
  } =
    stitch_static(state);
  let simple_result_of = key =>
    switch (results) {
    | None => None
    | Some(results) =>
      ModelResult.get_simple(ModelResults.lookup(results, key))
    };
  let test_validation =
    DynamicsItem.{
      term: test_validation.term,
      info_map: test_validation.info_map,
      simple_result: simple_result_of(test_validation_key),
    };
  let user_impl =
    DynamicsItem.{
      term: user_impl.term,
      info_map: user_impl.info_map,
      simple_result: simple_result_of(user_impl_key),
    };
  let user_tests =
    DynamicsItem.{
      term: user_tests.term,
      info_map: user_tests.info_map,
      simple_result: simple_result_of(user_tests_key),
    };
  let instructor =
    DynamicsItem.{
      term: instructor.term,
      info_map: instructor.info_map,
      simple_result: simple_result_of(instructor_key),
    };
  let hidden_bugs =
    List.mapi(
      (n, statics_item: StaticsItem.t) =>
        DynamicsItem.{
          term: statics_item.term,
          info_map: statics_item.info_map,
          simple_result: simple_result_of(hidden_bugs_key(n)),
        },
      hidden_bugs,
    );
  let hidden_tests =
    DynamicsItem.{
      term: hidden_tests.term,
      info_map: hidden_tests.info_map,
      simple_result: simple_result_of(hidden_tests_key),
    };

  {
    test_validation,
    user_impl,
    user_tests,
    instructor,
    hidden_bugs,
    hidden_tests,
  };
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
