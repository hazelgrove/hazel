open Util;
open Haz3lcore;

module type TutorialEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

let output_header_grading = _module_name =>
  "module Documentation = GradePrelude.Documentation\n" ++ "let prompt = ()\n";

module D = (TutorialEnv: TutorialEnv) => {
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
      ? () : failwith("Invalid point distribution in tutorial.");

  [@deriving (show({with_path: false}), sexp, yojson)]
  type p('code) = {
    title: string,
    description: string,
    your_impl: 'code,
    hidden_tests: hidden_tests('code),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type key = string;

  let key_of_string = (p): string => {
    p.title;
  };

  let key_of = p => {
    p.title;
  };

  // let find_key_opt = (key, specs: list(p('code))) => {
  //   specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);
  // };

  let find_key_opt =
      (key, specs: list(p('code))): option((string, p('code))) => {
    let rec loop = remaining_specs => {
      switch (remaining_specs) {
      | [] => None
      | [spec, ...rest] =>
        if (key_of(spec) == key) {
          Some
            ((key_of(spec), spec)); // Return as soon as we find a match
        } else {
          loop(
            rest // Continue searching in the remaining specs
          );
        }
      };
    };
    loop(specs); // Start looping over `specs`
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | YourImpl
    | YourTestsValidation
    | HiddenTests;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type spec = p(Zipper.t);

  // [@deriving (show({with_path: false}), sexp, yojson)]
  // type transitionary_spec = p(CodeString.t);

  let map = (p: p('a), f: 'a => 'b): p('b) => {
    {
      title: p.title,
      description: p.description,
      your_impl: f(p.your_impl),
      hidden_tests: {
        tests: PersistentZipper.persist(p.hidden_tests.tests),
        hints: p.hidden_tests.hints,
      },
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
  type persistent_state = {
    focus: pos,
    title: string,
    description: string,
    editors: list((pos, PersistentZipper.t)),
  };

  let editor_of_state: state => Editor.t =
    ({pos, eds, _}) =>
      switch (pos) {
      | YourImpl => eds.your_impl
      | YourTestsValidation => eds.hidden_tests.tests
      | HiddenTests => eds.hidden_tests.tests
      };

  let put_editor = ({pos, eds} as state: state, editor: Editor.t) =>
    switch (pos) {
    | YourImpl => {
        ...state,
        eds: {
          ...eds,
          your_impl: editor,
        },
      }
    | YourTestsValidation
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

  let editors = ({eds, _}: state) => [
    eds.your_impl,
    eds.hidden_tests.tests,
  ];

  let editor_positions = [YourImpl, HiddenTests, YourTestsValidation];

  let positioned_editors = state =>
    List.combine(editor_positions, editors(state));

  let idx_of_pos = (pos, p: p('code)) =>
    switch (pos) {
    | YourImpl => 0
    | YourTestsValidation => 1
    | HiddenTests => 1 + List.length(p.hidden_tests.tests)
    };

  let pos_of_idx = (p: p('code), idx: int) =>
    switch (idx) {
    | 0 => YourImpl
    | _ =>
      if (idx < 0) {
        failwith(
          "negative idx",
          // } else if (idx < 5 + List.length(p.hidden_bugs)) {
          //   HiddenBugs(idx - 5);
        );
      } else if (idx == 0 + (+ List.length(p.hidden_tests.tests))) {
        HiddenTests;
      } else {
        failwith("element idx");
      }
    };

  let switch_editor = (~pos, instructor_mode, ~documentation) =>
    if (!instructor_mode) {
      switch (pos) {
      | HiddenTests
      | _ => {eds: documentation.eds, pos}
      };
    } else {
      {eds: documentation.eds, pos};
    };

  let zipper_of_code = code => {
    switch (Printer.zipper_of_string(code)) {
    | None => failwith("Transition failed.")
    | Some(zipper) => zipper
    };
  };

  // let transition: transitionary_spec => spec =
  //   ({title, description, your_impl, hidden_tests}) => {
  //     let your_impl = zipper_of_code(your_impl);
  //     let hidden_tests = {
  //       let {tests, hints} = hidden_tests;
  //       let tests = zipper_of_code(tests);
  //       {
  //         tests,
  //         hints,
  //       };
  //     };
  //     {
  //       title,
  //       description,
  //       your_impl,
  //       hidden_tests,
  //     };
  //   };

  let eds_of_spec =
      (
        {title, description, your_impl, hidden_tests},
        ~settings: CoreSettings.t,
      )
      : eds => {
    let editor_of_serialization = Editor.init(~settings);
    let your_impl = editor_of_serialization(your_impl);
    let hidden_tests = {
      let {tests, hints} = hidden_tests;
      let tests = editor_of_serialization(tests);
      {tests, hints};
    };
    {title, description, your_impl, hidden_tests};
  };

  let set_instructor_mode = ({eds, _} as state: state, new_mode: bool) => {
    let updated_hidden_tests = {
      ...eds.hidden_tests,
      tests: Editor.set_read_only(eds.hidden_tests.tests, !new_mode),
    };

    {
      ...state,
      eds: {
        ...eds,
        hidden_tests: updated_hidden_tests,
      },
    };
  };

  let visible_in = (pos, ~instructor_mode) => {
    switch (pos) {
    | YourImpl => true
    | YourTestsValidation => true
    | HiddenTests => instructor_mode
    };
  };

  let state_of_spec =
      (spec, ~instructor_mode: bool, ~settings: CoreSettings.t): state => {
    let eds = eds_of_spec(~settings, spec);
    set_instructor_mode({pos: YourImpl, eds}, instructor_mode);
  };

  let persistent_state_of_state = (state: state, ~instructor_mode: bool) => {
    let zippers =
      positioned_editors(state)
      |> List.filter(((pos, _)) => visible_in(pos, ~instructor_mode))
      |> List.map(((pos, editor)) => {
           (pos, PersistentZipper.persist(Editor.(editor.state.zipper)))
         });
    {
      focus: state.pos,
      editors: zippers,
      title: state.eds.title,
      description: state.eds.description,
    };
  };

  let unpersist_state =
      (
        {focus, editors, _}: persistent_state,
        ~spec: spec,
        ~instructor_mode: bool,
        ~settings: CoreSettings.t,
      )
      // ~editing_title: bool,
      : state => {
    let lookup = (pos, default) =>
      if (visible_in(pos, ~instructor_mode)) {
        let persisted_zipper = List.assoc(pos, editors);
        let zipper = PersistentZipper.unpersist(persisted_zipper);
        // Ensure that zipper is the correct type for Editor.init
        Editor.init(zipper, ~read_only=false, ~settings); // Pass required settings if needed
      } else {
        Editor.init(
          default,
          ~read_only=false,
          ~settings // Use a default that matches
        );
      };
    let your_impl = lookup(YourImpl, spec.your_impl);
    let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);

    set_instructor_mode(
      {
        pos: focus,
        eds: {
          title: spec.title,
          description: spec.description,
          your_impl,
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

  // module TermItem = {
  //   type t = {
  //     term: TermBase.UExp.t,
  //     term_ranges: TermRanges.t,
  //   };
  // };

  // module StaticsItem = {
  //   type t = CachedStatics.statics;
  // };

  type stitched('a) = {
    test_validation: 'a, // prelude + correct_impl + your_tests
    user_impl: 'a, // prelude + your_impl
    instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
    hidden_tests: 'a,
  };

  let wrap_filter = (act: FilterAction.action, term: UExp.t): UExp.t =>
    Exp.{
      term:
        Exp.Filter(
          Filter({
            act: FilterAction.(act, One),
            pat: {
              term: Constructor("$e", Unknown(Internal) |> Typ.temp),
              copied: false,
              ids: [Id.mk()],
            },
          }),
          term,
        ),
      copied: false,
      ids: [Id.mk()],
    };

  // let wrap = (term, editor: Editor.t): TermItem.t => {
  //   term,
  //   term_ranges: editor.state.meta.term_ranges,
  // };

  let term_of = (editor: Editor.t): UExp.t =>
    MakeTerm.from_zip_for_sem(editor.state.zipper).term;

  let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
    EditorUtil.append_exp(
      EditorUtil.append_exp(term_of(ed1), term_of(ed2)),
      term_of(ed3),
    );

  let stitch1 = (ed1: Editor.t) =>
    // EditorUtil.append_exp(
    EditorUtil.append_exp(term_of(ed1));

  let stitch_term = ({eds, _}: state): stitched(UExp.t) => {
    let instructor = eds.hidden_tests.tests |> term_of;
    let user_impl_term = {
      eds.your_impl |> term_of |> wrap_filter(FilterAction.Step);
    };
    let test_validation_term = eds.hidden_tests.tests |> term_of;

    // No combining of your_impl_term with hidden_tests
    let hidden_tests_term =
      EditorUtil.append_exp(user_impl_term, term_of(eds.hidden_tests.tests));

    {
      user_impl: user_impl_term,
      instructor,
      test_validation: test_validation_term,
      hidden_tests: hidden_tests_term,
    };
  };

  let stitch_term = Core.Memo.general(stitch_term);

  type stitched_statics = stitched(Editor.CachedStatics.t);

  /* Multiple stitchings are needed for each exercise
     (see comments in the stitched type above)

     Stitching is necessary to concatenate terms
     from different editors, which are then typechecked. */

  let stitch_static =
      (settings: CoreSettings.t, t: stitched(UExp.t)): stitched_statics => {
    let mk = (term: UExp.t): Editor.CachedStatics.t => {
      let info_map = Statics.mk(settings, Builtins.ctx_init, term);
      {term, error_ids: Statics.Map.error_ids(info_map), info_map};
    };
    let instructor = mk(t.instructor);
    {
      test_validation: mk(t.test_validation),
      user_impl: mk(t.user_impl),
      instructor,
      hidden_tests: mk(t.hidden_tests),
    };
  };

  let stitch_static = Core.Memo.general(stitch_static);

  // let statics_of_stiched =
  //     (state: state, s: stitched(StaticsItem.t)): StaticsItem.t =>
  //   switch (state.pos) {
  //   | YourImpl => s.user_impl
  //   | HiddenTests => s.hidden_tests
  //   };

  // let statics_of = (~settings, documentation: state): StaticsItem.t =>
  //   documentation
  //   |> stitch_term
  //   |> stitch_static(settings)
  //   |> statics_of_stiched(documentation);

  let prelude_key = "prelude";
  let test_validation_key = "test_validation";
  let user_impl_key = "user_impl";
  let user_tests_key = "user_tests";
  let instructor_key = "instructor";
  let hidden_bugs_key = n => "hidden_bugs_" ++ string_of_int(n);
  let hidden_tests_key = "hidden_tests";

  let key_for_statics = (state: state): string =>
    switch (state.pos) {
    | YourImpl => user_impl_key
    | YourTestsValidation => test_validation_key
    | HiddenTests => hidden_tests_key
    };

  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, Elaborator.Elaboration.t)) => {
    let {test_validation, user_impl, instructor, hidden_tests} =
      stitch_static(settings, stitch_term(state));
    let elab = (s: Editor.CachedStatics.t): Elaborator.Elaboration.t => {
      d: Interface.elaborate(~settings, s.info_map, s.term),
    };
    [
      (test_validation_key, elab(test_validation)),
      (user_impl_key, elab(user_impl)),
      (instructor_key, elab(instructor)),
      (hidden_tests_key, elab(hidden_tests)),
    ];
    // @ (
    //   hidden_bugs
    //   |> List.mapi((n, hidden_bug: Editor.CachedStatics.t) =>
    //        (hidden_bugs_key(n), elab(hidden_bug))
    //      )
    // );
  };

  module DynamicsItem = {
    type t = {
      statics: Editor.CachedStatics.t,
      result: ModelResult.t,
    };
    let empty: t = {statics: Editor.CachedStatics.empty, result: NoElab};
    let statics_only = (statics: Editor.CachedStatics.t): t => {
      statics,
      result: NoElab,
    };
  };

  let statics_of_stiched_dynamics =
      (state: state, s: stitched(DynamicsItem.t)): Editor.CachedStatics.t =>
    switch (state.pos) {
    | YourImpl => s.user_impl.statics
    | YourTestsValidation => s.test_validation.statics
    | HiddenTests => s.hidden_tests.statics
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
      // user_tests,
      // prelude,
      instructor,
      // hidden_bugs,
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
        statics: test_validation,
        result: result_of(test_validation_key),
      };

    let user_impl =
      DynamicsItem.{statics: user_impl, result: result_of(user_impl_key)};

    let instructor =
      DynamicsItem.{statics: instructor, result: result_of(instructor_key)};

    let hidden_tests =
      DynamicsItem.{
        statics: hidden_tests,
        result: result_of(hidden_tests_key),
      };
    {
      test_validation,
      user_impl,
      // user_tests,
      instructor,
      // prelude,
      // hidden_bugs,
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
        // user_tests: DynamicsItem.statics_only(t.user_tests),
        instructor: DynamicsItem.statics_only(t.instructor),
        // prelude: DynamicsItem.statics_only(t.prelude),
        // hidden_bugs: List.map(DynamicsItem.statics_only, t.hidden_bugs),
        hidden_tests: DynamicsItem.statics_only(t.hidden_tests),
      };
    } else {
      {
        test_validation: DynamicsItem.empty,
        user_impl: DynamicsItem.empty,
        // user_tests: DynamicsItem.empty,
        instructor: DynamicsItem.empty,
        // prelude: DynamicsItem.empty,
        // hidden_bugs:
        //   List.init(List.length(state.eds.hidden_bugs), _ =>
        //     DynamicsItem.empty
        //   ),
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

  let blank_spec = (~title, ~description) => {
    // ~point_distribution,
    // ~required_tests,
    // ~provided_tests,
    // ~num_wrong_impls,

    // let prelude = Zipper.next_blank();
    // let correct_impl = Zipper.next_blank();
    // let your_tests_tests = Zipper.next_blank();
    let your_impl = Zipper.next_blank();
    // let hidden_bugs =
    //   List.init(
    //     num_wrong_impls,
    //     i => {
    //       let zipper = Zipper.next_blank();
    //       {impl: zipper, hint: "TODO: hint " ++ string_of_int(i)};
    //     },
    //   );
    let hidden_tests_tests = Zipper.next_blank();
    {
      title,
      description,
      // version: 1,
      // module_name,
      // prompt: ExerciseEnv.default,
      // point_distribution,
      // prelude,
      // correct_impl,
      // your_tests: {
      //   tests: your_tests_tests,
      //   required: required_tests,
      //   provided: provided_tests,
      // },
      your_impl,
      // hidden_bugs,
      hidden_tests: {
        tests: hidden_tests_tests,
        hints: [],
      },
      // syntax_tests: [],
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
