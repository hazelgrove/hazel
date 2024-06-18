open Sexplib.Std;
open Haz3lcore;

module type ExerciseEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

module F = (ExerciseEnv: ExerciseEnv) => {
  open ExerciseBase.F(ExerciseEnv);
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
  type hint = string;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type syntax_test = (hint, SyntaxTest.predicate);

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
    header,
    point_distribution,
    prelude: 'code,
    correct_impl: 'code,
    your_tests: your_tests('code),
    your_impl: 'code,
    hidden_bugs: list(wrong_impl('code)),
    hidden_tests: hidden_tests('code),
    syntax_tests,
  };

  let key_of = ({header, _}) => (header.title, header.version);

  let find_key_opt = (key, specs: list(p('code))) =>
    specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | Prelude
    | CorrectImpl
    | YourTests(your_tests_usage)
    | YourImpl
    | HiddenBugs(int)
    | HiddenTests
  and your_tests_usage =
    | Validation
    | Testing;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type spec = p(Zipper.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type transitionary_spec = p(CodeString.t);

  // let map = (f: 'a => 'b, p: p('a)): p('b) => {
  //   {
  //     header: p.header,
  //     point_distribution: p.point_distribution,
  //     prelude: f(p.prelude),
  //     correct_impl: f(p.correct_impl),
  //     your_tests: {
  //       ...p.your_tests,
  //       tests: f(p.your_tests.tests),
  //     },
  //     your_impl: f(p.your_impl),
  //     hidden_bugs:
  //       p.hidden_bugs
  //       |> List.map(wrong_impl => {
  //            {
  //              impl: PersistentZipper.persist(wrong_impl.impl),
  //              hint: wrong_impl.hint,
  //            }
  //          }),
  //     hidden_tests: {
  //       tests: PersistentZipper.persist(p.hidden_tests.tests),
  //       hints: p.hidden_tests.hints,
  //     },
  //     syntax_tests: p.syntax_tests,
  //   };
  // };

  let map = (f: 'a => 'b, p: p('a)): p('b) => {
    {
      ...p,
      prelude: p.prelude |> f,
      correct_impl: p.correct_impl |> f,
      your_tests: {
        ...p.your_tests,
        tests: p.your_tests.tests |> f,
      },
      your_impl: p.your_impl |> f,
      hidden_bugs:
        p.hidden_bugs |> List.map(({impl, hint}) => {impl: f(impl), hint}),
      hidden_tests: {
        ...p.hidden_tests,
        tests: p.hidden_tests.tests |> f,
      },
    };
  };

  let mapi = (f, p: p('a)): p('b) => {
    {
      ...p,
      prelude: p.prelude |> f(Prelude),
      correct_impl: p.correct_impl |> f(CorrectImpl),
      your_tests: {
        ...p.your_tests,
        tests: p.your_tests.tests |> f(YourTests(Validation)),
      },
      your_impl: p.your_impl |> f(YourImpl),
      hidden_bugs:
        p.hidden_bugs
        |> List.mapi((i, {impl, hint}) =>
             {impl: impl |> f(HiddenBugs(i)), hint}
           ),
      hidden_tests: {
        ...p.hidden_tests,
        tests: p.hidden_tests.tests |> f(HiddenTests),
      },
    };
  };

  let nth = (p: p('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => p.prelude
    | CorrectImpl => p.correct_impl
    | YourTests(_) => p.your_tests.tests
    | YourImpl => p.your_impl
    | HiddenBugs(i) => List.nth(p.hidden_bugs, i).impl
    | HiddenTests => p.hidden_tests.tests
    };

  let map_nth = (f: 'a => 'b, p: p('a), pos: pos): p('b) =>
    switch (pos) {
    | Prelude => {...p, prelude: p.prelude |> f}
    | CorrectImpl => {...p, correct_impl: p.correct_impl |> f}
    | YourTests(_) => {
        ...p,
        your_tests: {
          ...p.your_tests,
          tests: p.your_tests.tests |> f,
        },
      }
    | YourImpl => {...p, your_impl: p.your_impl |> f}
    | HiddenBugs(i) => {
        ...p,
        hidden_bugs:
          Util.ListUtil.map_nth(
            i,
            ({impl, hint}) => {impl: impl |> f, hint},
            p.hidden_bugs,
          ),
      }
    | HiddenTests => {
        ...p,
        hidden_tests: {
          ...p.hidden_tests,
          tests: p.hidden_tests.tests |> f,
        },
      }
    };

  let put_nth = (x: 'a, p: p('a), pos: pos): p('a) =>
    map_nth(Fun.const(x), p, pos);

  let flatten = p =>
    [p.prelude, p.correct_impl, p.your_tests.tests, p.your_impl]
    @ (p.hidden_bugs |> List.map(({impl, _}) => impl))
    @ [p.hidden_tests.tests];

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

  let editor_of_state = ({pos, eds}: state): Editor.t => nth(eds, pos);

  let put_editor = ({pos, eds}: state, editor: Editor.t): state => {
    pos,
    eds: put_nth(editor, eds, pos),
  };

  let editors = ({eds, _}: state) => eds |> flatten;

  let editor_positions = ({eds, _}: state) =>
    eds |> mapi((i, _) => i) |> flatten;

  let positioned_editors = state =>
    List.combine(editor_positions(state), editors(state));

  // let idx_of_pos = (pos, p: p('code)) =>
  //   switch (pos) {
  //   | Prelude => 0
  //   | CorrectImpl => 1
  //   | YourTestsTesting => 2
  //   | YourTestsValidation => 3
  //   | YourImpl => 4
  //   | HiddenBugs(i) =>
  //     if (i < List.length(p.hidden_bugs)) {
  //       5 + i;
  //     } else {
  //       failwith("invalid hidden bug index");
  //     }
  //   | HiddenTests => 5 + List.length(p.hidden_bugs)
  //   };

  // let pos_of_idx = (p: p('code), idx: int) =>
  //   switch (idx) {
  //   | 0 => Prelude
  //   | 1 => CorrectImpl
  //   | 2 => YourTestsTesting
  //   | 3 => YourTestsValidation
  //   | 4 => YourImpl
  //   | _ =>
  //     if (idx < 0) {
  //       failwith("negative idx");
  //     } else if (idx < 5 + List.length(p.hidden_bugs)) {
  //       HiddenBugs(idx - 5);
  //     } else if (idx == 5 + List.length(p.hidden_bugs)) {
  //       HiddenTests;
  //                  // Node(zhiyao): This could be wrong
  //     } else {
  //       failwith("element idx");
  //     }
  //   };

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

  let transition: transitionary_spec => spec = map(zipper_of_code);

  let editor_of_serialization = zipper => Editor.init(zipper);
  let eds_of_spec: spec => eds = map(editor_of_serialization);

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

  let set_instructor_mode = ({eds, pos}: state, new_mode: bool) => {
    pos,
    eds: {
      ...eds,
      prelude: Editor.set_read_only(eds.prelude, !new_mode),
    },
  };

  let visible_in = (pos, ~instructor_mode) => {
    switch (pos) {
    | Prelude => instructor_mode
    | CorrectImpl => instructor_mode
    | YourTests(Validation) => true
    | YourTests(Testing) => false
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
    let eds = spec |> mapi(lookup);
    set_instructor_mode({pos, eds}, instructor_mode);
  };

  type stitched('a) = {
    test_validation: 'a, // prelude + correct_impl + your_tests
    user_impl: 'a, // prelude + your_impl
    user_tests: 'a, // prelude + your_impl + your_tests
    prelude: 'a, // prelude
    instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
    hidden_bugs: list('a), // prelude + hidden_bugs[i].impl + your_tests,
    hidden_tests: 'a,
  };

  let stitch_term = ({eds, _}: state): stitched(TermItem.t) => {
    let instructor =
      stitch3(eds.prelude, eds.correct_impl, eds.hidden_tests.tests);
    let user_impl_term = {
      let your_impl_term =
        eds.your_impl |> term_of |> wrap_filter(FilterAction.Step);
      let prelude_term =
        eds.prelude |> term_of |> wrap_filter(FilterAction.Eval);
      EditorUtil.append_exp(prelude_term, your_impl_term);
    };
    let test_validation_term =
      stitch3(eds.prelude, eds.correct_impl, eds.your_tests.tests);
    let user_tests_term =
      EditorUtil.append_exp(user_impl_term, term_of(eds.your_tests.tests));
    let hidden_tests_term =
      EditorUtil.append_exp(user_impl_term, term_of(eds.hidden_tests.tests));
    {
      test_validation: wrap(test_validation_term, eds.your_tests.tests),
      user_impl: wrap(user_impl_term, eds.your_impl),
      user_tests: wrap(user_tests_term, eds.your_tests.tests),
      // instructor works here as long as you don't shadow anything in the prelude
      prelude: wrap(instructor, eds.prelude),
      instructor: wrap(instructor, eds.correct_impl),
      hidden_bugs:
        List.map(
          (t): TermItem.t =>
            wrap(stitch3(eds.prelude, t.impl, eds.your_tests.tests), t.impl),
          eds.hidden_bugs,
        ),
      hidden_tests: wrap(hidden_tests_term, eds.hidden_tests.tests),
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
    let mk = ({term, term_ranges, _}: TermItem.t): StaticsItem.t => {
      let info_map = Interface.Statics.mk_map(settings, term);
      {
        term,
        error_ids: Statics.Map.error_ids(term_ranges, info_map),
        info_map,
      };
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
    | YourTests(Validation) => s.test_validation
    | YourTests(Testing) => s.user_tests
    | YourImpl => s.user_impl
    | HiddenBugs(idx) => List.nth(s.hidden_bugs, idx)
    | HiddenTests => s.hidden_tests
    };

  let statics_of = (~settings, exercise: state): StaticsItem.t =>
    exercise
    |> stitch_term
    |> stitch_static(settings)
    |> statics_of_stiched(exercise);

  let prelude_key = "prelude";
  let test_validation_key = "test_validation";
  let user_impl_key = "user_impl";
  let user_tests_key = "user_tests";
  let instructor_key = "instructor";
  let hidden_bugs_key = n => "hidden_bugs_" ++ string_of_int(n);
  let hidden_tests_key = "hidden_tests";

  let key_for_statics = (state: state): string =>
    switch (state.pos) {
    | Prelude => prelude_key
    | CorrectImpl => instructor_key
    | YourTests(Validation) => test_validation_key
    | YourTests(Testing) => user_tests_key
    | YourImpl => user_impl_key
    | HiddenBugs(idx) => hidden_bugs_key(idx)
    | HiddenTests => hidden_tests_key
    };

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
    let elab = (s: CachedStatics.statics) =>
      Interface.elaborate(~settings, s.info_map, s.term);
    [
      (test_validation_key, elab(test_validation)),
      (user_impl_key, elab(user_impl)),
      (user_tests_key, elab(user_tests)),
      (instructor_key, elab(instructor)),
      (hidden_tests_key, elab(hidden_tests)),
    ]
    @ (
      hidden_bugs
      |> List.mapi((n, hidden_bug: StaticsItem.t) =>
           (hidden_bugs_key(n), elab(hidden_bug))
         )
    );
  };

  let mk_statics =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, StaticsItem.t)) => {
    let stitched = stitch_static(settings, stitch_term(state));
    [
      (prelude_key, stitched.prelude),
      (test_validation_key, stitched.test_validation),
      (user_impl_key, stitched.user_impl),
      (user_tests_key, stitched.user_tests),
      (instructor_key, stitched.instructor),
      (hidden_tests_key, stitched.hidden_tests),
    ]
    @ List.mapi(
        (n, hidden_bug: StaticsItem.t) => (hidden_bugs_key(n), hidden_bug),
        stitched.hidden_bugs,
      );
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
    let result_of = key =>
      switch (results) {
      | None => ModelResult.NoElab
      | Some(results) =>
        ModelResults.lookup(results, key)
        |> Option.value(~default=ModelResult.NoElab)
      };

    let test_validation =
      DynamicsItem.{
        term: test_validation.term,
        info_map: test_validation.info_map,
        result: result_of(test_validation_key),
      };

    let user_impl =
      DynamicsItem.{
        term: user_impl.term,
        info_map: user_impl.info_map,
        result: result_of(user_impl_key),
      };

    let user_tests =
      DynamicsItem.{
        term: user_tests.term,
        info_map: user_tests.info_map,
        result: result_of(user_tests_key),
      };
    let prelude =
      DynamicsItem.{
        term: prelude.term,
        info_map: prelude.info_map,
        result: NoElab,
      };
    let instructor =
      DynamicsItem.{
        term: instructor.term,
        info_map: instructor.info_map,
        result: result_of(instructor_key),
      };
    let hidden_bugs =
      List.mapi(
        (n, statics_item: StaticsItem.t) =>
          DynamicsItem.{
            term: statics_item.term,
            info_map: statics_item.info_map,
            result: result_of(hidden_bugs_key(n)),
          },
        hidden_bugs,
      );
    let hidden_tests =
      DynamicsItem.{
        term: hidden_tests.term,
        info_map: hidden_tests.info_map,
        result: result_of(hidden_tests_key),
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
  // NOTE(zhiyao): This function may be unnecessary
  // let blank_spec =
  //     (
  //       ~title,
  //       ~module_name,
  //       ~point_distribution,
  //       ~required_tests,
  //       ~provided_tests,
  //       ~num_wrong_impls,
  //     ) => {
  //   let prelude = Zipper.next_blank();
  //   let correct_impl = Zipper.next_blank();
  //   let your_tests_tests = Zipper.next_blank();
  //   let your_impl = Zipper.next_blank();
  //   let hidden_bugs =
  //     List.init(
  //       num_wrong_impls,
  //       i => {
  //         let zipper = Zipper.next_blank();
  //         {impl: zipper, hint: "TODO: hint " ++ string_of_int(i)};
  //       },
  //     );
  //   let hidden_tests_tests = Zipper.next_blank();
  //   {
  //     title,
  //     version: 1,
  //     module_name,
  //     prompt: ExerciseEnv.default,
  //     point_distribution,
  //     prelude,
  //     correct_impl,
  //     your_tests: {
  //       tests: your_tests_tests,
  //       required: required_tests,
  //       provided: provided_tests,
  //     },
  //     your_impl,
  //     hidden_bugs,
  //     hidden_tests: {
  //       tests: hidden_tests_tests,
  //       hints: [],
  //     },
  //     syntax_tests: [],
  //   };
  // };
};
