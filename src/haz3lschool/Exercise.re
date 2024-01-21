open Sexplib.Std;
open Haz3lcore;

module type ExerciseEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

let output_header_grading = _module_name =>
  "module Exercise = GradePrelude.Exercise\n" ++ "let prompt = ()\n";

module F = (ExerciseEnv: ExerciseEnv) => {
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
  type predicate = Term.UExp.t => bool;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type hint = string;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type syntax_test = (hint, predicate);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type syntax_tests = list(syntax_test);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type your_tests('code) = {
    tests: 'code,
    required: int,
    provided: int,
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
    title: string,
    version: int,
    module_name: string,
    prompt:
      [@printer (fmt, _) => Format.pp_print_string(fmt, "prompt")] [@opaque] ExerciseEnv.node,
    point_distribution,
    prelude: 'code,
    correct_impl: 'code,
    your_tests: your_tests('code),
    your_impl: 'code,
    hidden_bugs: list(wrong_impl('code)),
    hidden_tests: hidden_tests('code),
    syntax_tests,
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
  type spec = p(Zipper.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type transitionary_spec = p(CodeString.t);

  let map = (p: p('a), f: 'a => 'b): p('b) => {
    {
      title: p.title,
      version: p.version,
      module_name: p.module_name,
      prompt: p.prompt,
      point_distribution: p.point_distribution,
      prelude: f(p.prelude),
      correct_impl: f(p.correct_impl),
      your_tests: {
        tests: f(p.your_tests.tests),
        required: p.your_tests.required,
        provided: p.your_tests.provided,
      },
      your_impl: f(p.your_impl),
      hidden_bugs:
        p.hidden_bugs
        |> List.map(wrong_impl => {
             {
               impl: PersistentZipper.persist(wrong_impl.impl),
               hint: wrong_impl.hint,
             }
           }),
      hidden_tests: {
        tests: PersistentZipper.persist(p.hidden_tests.tests),
        hints: p.hidden_tests.hints,
      },
      syntax_tests: p.syntax_tests,
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type eds = p(Editor.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = {
    pos,
    eds,
  };

  let key_of_state = ({eds, _}) => key_of(eds);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_state = (pos, list((pos, PersistentZipper.t)));

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

  let put_editor = ({pos, eds, _} as state: state, editor: Editor.t) =>
    switch (pos) {
    | Prelude => {
        ...state,
        eds: {
          ...eds,
          prelude: editor,
        },
      }
    | CorrectImpl => {
        ...state,
        eds: {
          ...eds,
          correct_impl: editor,
        },
      }
    | YourTestsValidation
    | YourTestsTesting => {
        ...state,
        eds: {
          ...eds,
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
          your_impl: editor,
        },
      }
    | HiddenBugs(n) => {
        ...state,
        eds: {
          ...eds,
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

  let switch_editor = (~pos, instructor_mode, ~exercise) =>
    if (!instructor_mode) {
      switch (pos) {
      | HiddenTests
      | HiddenBugs(_) => exercise
      | _ => {eds: exercise.eds, pos}
      };
    } else {
      {eds: exercise.eds, pos};
    };

  let zipper_of_code = code => {
    switch (Printer.zipper_of_string(code)) {
    | None => failwith("Transition failed.")
    | Some(zipper) => zipper
    };
  };

  let transition: transitionary_spec => spec =
    (
      {
        title,
        version,
        module_name,
        prompt,
        point_distribution,
        prelude,
        correct_impl,
        your_tests,
        your_impl,
        hidden_bugs,
        hidden_tests,
        syntax_tests,
      },
    ) => {
      let prelude = zipper_of_code(prelude);
      let correct_impl = zipper_of_code(correct_impl);
      let your_tests = {
        let tests = zipper_of_code(your_tests.tests);
        {tests, required: your_tests.required, provided: your_tests.provided};
      };
      let your_impl = zipper_of_code(your_impl);
      let hidden_bugs =
        List.fold_left(
          (acc, {impl, hint}) => {
            let impl = zipper_of_code(impl);
            acc @ [{impl, hint}];
          },
          [],
          hidden_bugs,
        );
      let hidden_tests = {
        let {tests, hints} = hidden_tests;
        let tests = zipper_of_code(tests);
        {tests, hints};
      };
      {
        title,
        version,
        module_name,
        prompt,
        point_distribution,
        prelude,
        correct_impl,
        your_tests,
        your_impl,
        hidden_bugs,
        hidden_tests,
        syntax_tests,
      };
    };

  let editor_of_serialization = zipper => Editor.init(zipper);
  let eds_of_spec: spec => eds =
    (
      {
        title,
        version,
        module_name,
        prompt,
        point_distribution,
        prelude,
        correct_impl,
        your_tests,
        your_impl,
        hidden_bugs,
        hidden_tests,
        syntax_tests,
      },
    ) => {
      let prelude = editor_of_serialization(prelude);
      let correct_impl = editor_of_serialization(correct_impl);
      let your_tests = {
        let tests = editor_of_serialization(your_tests.tests);
        {tests, required: your_tests.required, provided: your_tests.provided};
      };
      let your_impl = editor_of_serialization(your_impl);
      let hidden_bugs =
        hidden_bugs
        |> List.map(({impl, hint}) => {
             let impl = editor_of_serialization(impl);
             {impl, hint};
           });
      let hidden_tests = {
        let {tests, hints} = hidden_tests;
        let tests = editor_of_serialization(tests);
        {tests, hints};
      };
      {
        title,
        version,
        module_name,
        prompt,
        point_distribution,
        prelude,
        correct_impl,
        your_tests,
        your_impl,
        hidden_bugs,
        hidden_tests,
        syntax_tests,
      };
    };

  //
  // Old version of above that did string-based parsing, may be useful
  // for transitions between zipper data structure versions (TODO)
  //
  // let editor_of_code = (init_id, code) =>
  //   switch (EditorUtil.editor_of_code(init_id, code)) {
  //   | None => failwith("Exercise error: invalid code")
  //   | Some(x) => x
  //   };
  // let eds_of_spec: spec => eds =
  //   (
  //     {
  //
  //       title,
  //       version,
  //       prompt,
  //       point_distribution,
  //       prelude,
  //       correct_impl,
  //       your_tests,
  //       your_impl,
  //       hidden_bugs,
  //       hidden_tests,
  //     },
  //   ) => {
  //     let id = next_id;
  //     let (id, prelude) = editor_of_code(id, prelude);
  //     let (id, correct_impl) = editor_of_code(id, correct_impl);
  //     let (id, your_tests) = {
  //       let (id, tests) = editor_of_code(id, your_tests.tests);
  //       (
  //         id,
  //         {
  //           tests,
  //           num_required: your_tests.num_required,
  //           minimum: your_tests.minimum,
  //         },
  //       );
  //     };
  //     let (id, your_impl) = editor_of_code(id, your_impl);
  //     let (id, hidden_bugs) =
  //       List.fold_left(
  //         ((id, acc), {impl, hint}) => {
  //           let (id, impl) = editor_of_code(id, impl);
  //           (id, acc @ [{impl, hint}]);
  //         },
  //         (id, []),
  //         hidden_bugs,
  //       );
  //     let (id, hidden_tests) = {
  //       let {tests, hints} = hidden_tests;
  //       let (id, tests) = editor_of_code(id, tests);
  //       (id, {tests, hints});
  //     };
  //     {
  //       next_id: id,
  //       title,
  //       version,
  //       prompt,
  //       point_distribution,
  //       prelude,
  //       correct_impl,
  //       your_tests,
  //       your_impl,
  //       hidden_bugs,
  //       hidden_tests,
  //     };
  //   };

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
      ({pos, _} as state: state, ~instructor_mode: bool) => {
    let zippers =
      positioned_editors(state)
      |> List.filter(((pos, _)) => visible_in(pos, ~instructor_mode))
      |> List.map(((pos, editor)) => {
           (pos, PersistentZipper.persist(Editor.(editor.state.zipper)))
         });
    (pos, zippers);
  };

  let unpersist_state =
      (
        (pos, positioned_zippers): persistent_state,
        ~spec: spec,
        ~instructor_mode: bool,
      )
      : state => {
    let lookup = (pos, default) =>
      if (visible_in(pos, ~instructor_mode)) {
        let persisted_zipper = List.assoc(pos, positioned_zippers);
        let zipper = PersistentZipper.unpersist(persisted_zipper);
        Editor.init(zipper);
      } else {
        editor_of_serialization(default);
      };
    let prelude = lookup(Prelude, spec.prelude);
    let correct_impl = lookup(CorrectImpl, spec.correct_impl);
    let your_tests_tests = lookup(YourTestsValidation, spec.your_tests.tests);
    let your_impl = lookup(YourImpl, spec.your_impl);
    let (_, hidden_bugs) =
      List.fold_left(
        ((i, hidden_bugs: list(wrong_impl(Editor.t))), {impl, hint}) => {
          let impl = lookup(HiddenBugs(i), impl);
          (i + 1, hidden_bugs @ [{impl, hint}]);
        },
        (0, []),
        spec.hidden_bugs,
      );
    let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);

    set_instructor_mode(
      {
        pos,
        eds: {
          title: spec.title,
          version: spec.version,
          module_name: spec.module_name,
          prompt: spec.prompt,
          point_distribution: spec.point_distribution,
          prelude,
          correct_impl,
          your_tests: {
            tests: your_tests_tests,
            required: spec.your_tests.required,
            provided: spec.your_tests.provided,
          },
          your_impl,
          hidden_bugs,
          hidden_tests: {
            tests: hidden_tests_tests,
            hints: spec.hidden_tests.hints,
          },
          syntax_tests: spec.syntax_tests,
        },
      },
      instructor_mode,
    );
  };

  // # Stitching

  type stitched('a) = {
    test_validation: 'a, // prelude + correct_impl + your_tests
    user_impl: 'a, // prelude + your_impl
    user_tests: 'a, // prelude + your_impl + your_tests
    prelude: 'a, // prelude
    instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
    hidden_bugs: list('a), // prelude + hidden_bugs[i].impl + your_tests,
    hidden_tests: 'a,
  };

  module TermItem = {
    type t = TermBase.UExp.t;
  };

  module StaticsItem = {
    type t = {
      term: TermBase.UExp.t,
      info_map: Statics.Map.t,
    };
  };

  let stitch_term = ({eds, _}: state): stitched(TermItem.t) => {
    let instructor =
      EditorUtil.stitch([
        eds.prelude,
        eds.correct_impl,
        eds.hidden_tests.tests,
      ]);
    {
      test_validation:
        EditorUtil.stitch([
          eds.prelude,
          eds.correct_impl,
          eds.your_tests.tests,
        ]),
      user_impl: EditorUtil.stitch([eds.prelude, eds.your_impl]),
      user_tests:
        EditorUtil.stitch([eds.prelude, eds.your_impl, eds.your_tests.tests]),
      prelude: instructor, // works as long as you don't shadow anything in the prelude
      instructor,
      hidden_bugs:
        List.map(
          ({impl, _}) => {
            EditorUtil.stitch([eds.prelude, impl, eds.your_tests.tests])
          },
          eds.hidden_bugs,
        ),
      hidden_tests:
        EditorUtil.stitch([
          eds.prelude,
          eds.your_impl,
          eds.hidden_tests.tests,
        ]),
    };
  };
  let stitch_term = Core.Memo.general(stitch_term);

  type stitched_statics = stitched(StaticsItem.t);

  /* Multiple stitchings are needed for each exercise
     (see comments in the stitched type above)

     Stitching is necessary to concatenate terms
     from different editors, which are then typechecked. */
  let stitch_static =
      (settings: CoreSettings.t, t: stitched(TermItem.t)): stitched_statics => {
    let mk = (term): StaticsItem.t => {
      term,
      info_map: Interface.Statics.mk_map(settings, term),
    };
    let instructor = mk(t.instructor);
    {
      test_validation: mk(t.test_validation),
      user_impl: mk(t.user_impl),
      user_tests: mk(t.user_tests),
      prelude: instructor, // works as long as you don't shadow anything in the prelude
      instructor,
      hidden_bugs: List.map(mk, t.hidden_bugs),
      hidden_tests: mk(t.hidden_tests),
    };
  };

  let stitch_static = Core.Memo.general(stitch_static);

  let statics_of_stiched =
      (state: state, s: stitched(StaticsItem.t)): StaticsItem.t =>
    switch (state.pos) {
    | Prelude => s.prelude
    | CorrectImpl => s.instructor
    | YourTestsValidation => s.test_validation
    | YourTestsTesting => s.user_tests
    | YourImpl => s.user_impl
    | HiddenBugs(idx) => List.nth(s.hidden_bugs, idx)
    | HiddenTests => s.hidden_tests
    };

  let statics_of = (~settings, exercise: state): StaticsItem.t =>
    exercise
    |> stitch_term
    |> stitch_static(settings)
    |> statics_of_stiched(exercise);

  let test_validation_key = "test_validation";
  let user_impl_key = "user_impl";
  let user_tests_key = "user_tests";
  let instructor_key = "instructor";
  let hidden_bugs_key = n => "hidden_bugs_" ++ string_of_int(n);
  let hidden_tests_key = "hidden_tests";

  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, DHExp.t)) => {
    let {
      test_validation,
      user_impl,
      user_tests,
      prelude: _,
      instructor,
      hidden_bugs,
      hidden_tests,
    } =
      stitch_static(settings, stitch_term(state));
    [
      (
        test_validation_key,
        Interface.elaborate(
          ~settings,
          test_validation.info_map,
          test_validation.term,
        ),
      ),
      (
        user_impl_key,
        Interface.elaborate(~settings, user_impl.info_map, user_impl.term),
      ),
      (
        user_tests_key,
        Interface.elaborate(~settings, user_tests.info_map, user_tests.term),
      ),
      (
        instructor_key,
        Interface.elaborate(~settings, instructor.info_map, instructor.term),
      ),
      (
        hidden_tests_key,
        Interface.elaborate(
          ~settings,
          hidden_tests.info_map,
          hidden_tests.term,
        ),
      ),
    ]
    @ (
      hidden_bugs
      |> List.mapi((n, hidden_bug: StaticsItem.t) =>
           (
             hidden_bugs_key(n),
             Interface.elaborate(
               ~settings,
               hidden_bug.info_map,
               hidden_bug.term,
             ),
           )
         )
    );
  };

  module DynamicsItem = {
    type t = {
      term: TermBase.UExp.t,
      info_map: Statics.Map.t,
      simple_result: ModelResult.simple,
    };
    let empty: t = {
      term: {
        term: Tuple([]),
        ids: [Id.mk()],
      },
      info_map: Id.Map.empty,
      simple_result: None,
    };
    let statics_only = ({term, info_map}: StaticsItem.t): t => {
      {term, info_map, simple_result: None};
    };
  };

  /* Given the evaluation results, collects the
     relevant information for producing dynamic
     feedback*/
  let stitch_dynamic =
      (
        settings: CoreSettings.t,
        state: state,
        results: option(ModelResults.t),
      )
      : stitched(DynamicsItem.t) => {
    let {
      test_validation,
      user_impl,
      user_tests,
      prelude,
      instructor,
      hidden_bugs,
      hidden_tests,
    } =
      stitch_static(settings, stitch_term(state));
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
    let prelude =
      DynamicsItem.{
        term: prelude.term,
        info_map: prelude.info_map,
        simple_result: None,
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
      prelude,
      hidden_bugs,
      hidden_tests,
    };
  };

  let stitch_dynamic =
      (
        settings: CoreSettings.t,
        state: state,
        results: option(ModelResults.t),
      )
      : stitched(DynamicsItem.t) =>
    if (settings.statics && settings.dynamics) {
      stitch_dynamic(settings, state, results);
    } else if (settings.statics) {
      let t = stitch_static(settings, stitch_term(state));
      {
        test_validation: DynamicsItem.statics_only(t.test_validation),
        user_impl: DynamicsItem.statics_only(t.user_impl),
        user_tests: DynamicsItem.statics_only(t.user_tests),
        instructor: DynamicsItem.statics_only(t.instructor),
        prelude: DynamicsItem.statics_only(t.prelude),
        hidden_bugs: List.map(DynamicsItem.statics_only, t.hidden_bugs),
        hidden_tests: DynamicsItem.statics_only(t.hidden_tests),
      };
    } else {
      {
        test_validation: DynamicsItem.empty,
        user_impl: DynamicsItem.empty,
        user_tests: DynamicsItem.empty,
        instructor: DynamicsItem.empty,
        prelude: DynamicsItem.empty,
        hidden_bugs:
          List.init(List.length(state.eds.hidden_bugs), _ =>
            DynamicsItem.empty
          ),
        hidden_tests: DynamicsItem.empty,
      };
    };
  let stitch_dynamic = Core.Memo.general(stitch_dynamic);

  // Module Export

  let editor_pp = (fmt, editor: Editor.t) => {
    let zipper = editor.state.zipper;
    let serialization = Zipper.show(zipper);
    // let string_literal = "\"" ++ String.escaped(serialization) ++ "\"";
    Format.pp_print_string(fmt, serialization);
  };

  let export_module = (module_name, {eds, _}: state) => {
    let prefix =
      "let prompt = "
      ++ module_name
      ++ "_prompt.prompt\n"
      ++ "let exercise: Exercise.spec = ";
    let record = show_p(editor_pp, eds);
    let data = prefix ++ record ++ "\n";
    data;
  };

  let transitionary_editor_pp = (fmt, editor: Editor.t) => {
    let zipper = editor.state.zipper;
    let code = Printer.to_string_basic(zipper);
    Format.pp_print_string(fmt, "\"" ++ String.escaped(code) ++ "\"");
  };

  let export_transitionary_module = (module_name, {eds, _}: state) => {
    let prefix =
      "let prompt = "
      ++ module_name
      ++ "_prompt.prompt\n"
      ++ "let exercise: Exercise.spec = Exercise.transition(";
    let record = show_p(transitionary_editor_pp, eds);
    let data = prefix ++ record ++ ")\n";
    data;
  };

  let export_grading_module = (module_name, {eds, _}: state) => {
    let header = output_header_grading(module_name);
    let prefix = "let exercise: Exercise.spec = ";
    let record = show_p(editor_pp, eds);
    let data = header ++ prefix ++ record ++ "\n";
    data;
  };

  let blank_spec =
      (
        ~title,
        ~module_name,
        ~point_distribution,
        ~required_tests,
        ~provided_tests,
        ~num_wrong_impls,
      ) => {
    let prelude = Zipper.next_blank();
    let correct_impl = Zipper.next_blank();
    let your_tests_tests = Zipper.next_blank();
    let your_impl = Zipper.next_blank();
    let hidden_bugs =
      List.init(
        num_wrong_impls,
        i => {
          let zipper = Zipper.next_blank();
          {impl: zipper, hint: "TODO: hint " ++ string_of_int(i)};
        },
      );
    let hidden_tests_tests = Zipper.next_blank();
    {
      title,
      version: 1,
      module_name,
      prompt: ExerciseEnv.default,
      point_distribution,
      prelude,
      correct_impl,
      your_tests: {
        tests: your_tests_tests,
        required: required_tests,
        provided: provided_tests,
      },
      your_impl,
      hidden_bugs,
      hidden_tests: {
        tests: hidden_tests_tests,
        hints: [],
      },
      syntax_tests: [],
    };
  };

  // From Store

  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise_export = {
    cur_exercise: key,
    exercise_data: list((key, persistent_state)),
  };

  let serialize_exercise = (exercise, ~instructor_mode) => {
    persistent_state_of_state(exercise, ~instructor_mode)
    |> sexp_of_persistent_state
    |> Sexplib.Sexp.to_string;
  };

  let deserialize_exercise = (data, ~spec, ~instructor_mode) => {
    data
    |> Sexplib.Sexp.of_string
    |> persistent_state_of_sexp
    |> unpersist_state(~spec, ~instructor_mode);
  };

  let deserialize_exercise_export = data => {
    data |> Sexplib.Sexp.of_string |> exercise_export_of_sexp;
  };
};
