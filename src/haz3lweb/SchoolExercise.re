open Virtual_dom.Vdom;
open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type wrong_impl('code) = {
  impl: 'code,
  hint: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type hidden_tests('code) = {
  tests: 'code,
  hints: list(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type your_tests('code) = {
  tests: 'code,
  num_required: int,
  minimum: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type point_distribution = {
  test_validation: int,
  mutation_testing: int,
  impl_grading: int,
};

let validate_point_distribution =
    ({test_validation, mutation_testing, impl_grading}: point_distribution) =>
  test_validation + mutation_testing + impl_grading == 100
    ? () : failwith("Invalid point distribution in exercise.");

[@deriving (show({with_path: false}), sexp, yojson)]
type p('code) = {
  next_id: Id.t,
  title: string,
  version: int,
  prompt: [@opaque] Node.t,
  point_distribution,
  prelude: 'code,
  correct_impl: 'code,
  your_tests: your_tests('code),
  your_impl: 'code,
  hidden_bugs: list(wrong_impl('code)),
  hidden_tests: hidden_tests('code),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type key = (string, int);

let key_of = p => {
  (p.title, p.version);
};

let find_key_opt = (key, specs: list(p('code))) => {
  specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Prelude
  | CorrectImpl
  | YourTestsValidation
  | YourTestsTesting
  | YourImpl
  | HiddenBugs(int)
  | HiddenTests;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = p(CodeString.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type eds = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {
  pos,
  eds,
};

let key_of_state = ({eds, _}) => key_of(eds);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = (pos, Id.t, list((pos, Zipper.t)));

let editor_of_state: state => Editor.t =
  ({pos, eds, _}) =>
    switch (pos) {
    | Prelude => eds.prelude
    | CorrectImpl => eds.correct_impl
    | YourTestsValidation => eds.your_tests.tests
    | YourTestsTesting => eds.your_tests.tests
    | YourImpl => eds.your_impl
    | HiddenBugs(i) => List.nth(eds.hidden_bugs, i).impl
    | HiddenTests => eds.hidden_tests.tests
    };

let id_of_state = ({eds, _}: state): Id.t => {
  eds.next_id;
};

let put_editor_and_id =
    ({pos, eds, _} as state: state, next_id, editor: Editor.t) =>
  switch (pos) {
  | Prelude => {
      ...state,
      eds: {
        ...eds,
        next_id,
        prelude: editor,
      },
    }
  | CorrectImpl => {
      ...state,
      eds: {
        ...eds,
        next_id,
        correct_impl: editor,
      },
    }
  | YourTestsValidation
  | YourTestsTesting => {
      ...state,
      eds: {
        ...eds,
        next_id,
        your_tests: {
          ...eds.your_tests,
          tests: editor,
        },
      },
    }
  | YourImpl => {
      ...state,
      eds: {
        ...eds,
        next_id,
        your_impl: editor,
      },
    }
  | HiddenBugs(n) => {
      ...state,
      eds: {
        ...eds,
        next_id,
        hidden_bugs:
          Util.ListUtil.put_nth(
            n,
            {...List.nth(eds.hidden_bugs, n), impl: editor},
            eds.hidden_bugs,
          ),
      },
    }
  | HiddenTests => {
      ...state,
      eds: {
        ...eds,
        next_id,
        hidden_tests: {
          ...eds.hidden_tests,
          tests: editor,
        },
      },
    }
  };

let editors = ({eds, _}: state) =>
  [
    eds.prelude,
    eds.correct_impl,
    eds.your_tests.tests,
    eds.your_tests.tests,
    eds.your_impl,
  ]
  @ List.map(wrong_impl => wrong_impl.impl, eds.hidden_bugs)
  @ [eds.hidden_tests.tests];

let editor_positions = ({eds, _}: state) =>
  [Prelude, CorrectImpl, YourTestsTesting, YourTestsValidation, YourImpl]
  @ List.mapi((i, _) => HiddenBugs(i), eds.hidden_bugs)
  @ [HiddenTests];

let positioned_editors = state =>
  List.combine(editor_positions(state), editors(state));

let idx_of_pos = (pos, p: p('code)) =>
  switch (pos) {
  | Prelude => 0
  | CorrectImpl => 1
  | YourTestsTesting => 2
  | YourTestsValidation => 3
  | YourImpl => 4
  | HiddenBugs(i) =>
    if (i < List.length(p.hidden_bugs)) {
      5 + i;
    } else {
      failwith("invalid hidden bug index");
    }
  | HiddenTests => 5 + List.length(p.hidden_bugs)
  };

let pos_of_idx = (p: p('code), idx: int) =>
  switch (idx) {
  | 0 => Prelude
  | 1 => CorrectImpl
  | 2 => YourTestsTesting
  | 3 => YourTestsValidation
  | 4 => YourImpl
  | _ =>
    if (idx < 0) {
      failwith("negative idx");
    } else if (idx < 5 + List.length(p.hidden_bugs)) {
      HiddenBugs(idx - 5);
    } else if (idx == 5 + List.length(p.hidden_bugs)) {
      HiddenTests;
    } else {
      failwith("element idx");
    }
  };

let switch_editor = (idx: int, {eds, _}) => {
  pos: pos_of_idx(eds, idx),
  eds,
};

let editor_of_code = (init_id, code) =>
  switch (EditorUtil.editor_of_code(init_id, code)) {
  | None => failwith("Exercise error: invalid code")
  | Some(x) => x
  };
let eds_of_spec: spec => eds =
  (
    {
      next_id,
      title,
      version,
      prompt,
      point_distribution,
      prelude,
      correct_impl,
      your_tests,
      your_impl,
      hidden_bugs,
      hidden_tests,
    },
  ) => {
    let id = next_id;
    let (id, prelude) = editor_of_code(id, prelude);
    let (id, correct_impl) = editor_of_code(id, correct_impl);
    let (id, your_tests) = {
      let (id, tests) = editor_of_code(id, your_tests.tests);
      (
        id,
        {
          tests,
          num_required: your_tests.num_required,
          minimum: your_tests.minimum,
        },
      );
    };
    let (id, your_impl) = editor_of_code(id, your_impl);
    let (id, hidden_bugs) =
      List.fold_left(
        ((id, acc), {impl, hint}) => {
          let (id, impl) = editor_of_code(id, impl);
          (id, acc @ [{impl, hint}]);
        },
        (id, []),
        hidden_bugs,
      );
    let (id, hidden_tests) = {
      let {tests, hints} = hidden_tests;
      let (id, tests) = editor_of_code(id, tests);
      (id, {tests, hints});
    };
    {
      next_id: id,
      title,
      version,
      prompt,
      point_distribution,
      prelude,
      correct_impl,
      your_tests,
      your_impl,
      hidden_bugs,
      hidden_tests,
    };
  };

let set_instructor_mode = ({eds, _} as state: state, new_mode: bool) => {
  ...state,
  eds: {
    ...eds,
    prelude: Editor.set_read_only(eds.prelude, !new_mode),
  },
};

let visible_in = (pos, ~instructor_mode) => {
  switch (pos) {
  | Prelude => instructor_mode
  | CorrectImpl => instructor_mode
  | YourTestsValidation => true
  | YourTestsTesting => false
  | YourImpl => true
  | HiddenBugs(_) => instructor_mode
  | HiddenTests => instructor_mode
  };
};

let state_of_spec = (spec, ~instructor_mode: bool): state => {
  let eds = eds_of_spec(spec);
  set_instructor_mode({pos: YourImpl, eds}, instructor_mode);
};

let persistent_state_of_state =
    ({pos, eds} as state: state, ~instructor_mode: bool) => {
  let zippers =
    positioned_editors(state)
    |> List.filter(((pos, _)) => visible_in(pos, ~instructor_mode))
    |> List.map(((pos, editor)) => {(pos, Editor.(editor.state.zipper))});
  (pos, eds.next_id, zippers);
};

let unpersist_state =
    (
      (pos, next_id, positioned_zippers): persistent_state,
      ~spec: spec,
      ~instructor_mode: bool,
    )
    : state => {
  let lookup = (id, pos, default) =>
    if (visible_in(pos, ~instructor_mode)) {
      (id, Editor.init(List.assoc(pos, positioned_zippers)));
    } else {
      editor_of_code(id, default);
    };
  let id = next_id;
  let (id, prelude) = lookup(id, Prelude, spec.prelude);
  let (id, correct_impl) = lookup(id, CorrectImpl, spec.correct_impl);
  let (id, your_tests_tests) =
    lookup(id, YourTestsValidation, spec.your_tests.tests);
  let (id, your_impl) = lookup(id, YourImpl, spec.your_impl);
  let (_, id, hidden_bugs) =
    List.fold_left(
      ((i, id, hidden_bugs: list(wrong_impl(Editor.t))), {impl, hint}) => {
        let (id, impl) = lookup(id, HiddenBugs(i), impl);
        (i + 1, id, hidden_bugs @ [{impl, hint}]);
      },
      (0, id, []),
      spec.hidden_bugs,
    );
  let (id, hidden_tests_tests) =
    lookup(id, HiddenTests, spec.hidden_tests.tests);

  set_instructor_mode(
    {
      pos,
      eds: {
        next_id: id,
        title: spec.title,
        version: spec.version,
        prompt: spec.prompt,
        point_distribution: spec.point_distribution,
        prelude,
        correct_impl,
        your_tests: {
          tests: your_tests_tests,
          num_required: spec.your_tests.num_required,
          minimum: spec.your_tests.minimum,
        },
        your_impl,
        hidden_bugs,
        hidden_tests: {
          tests: hidden_tests_tests,
          hints: spec.hidden_tests.hints,
        },
      },
    },
    instructor_mode,
  );
};

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
