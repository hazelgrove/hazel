open Sexplib.Std;
open Haz3lcore;

module type DocEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

let output_header_grading = _module_name =>
  "module Documentation = GradePrelude.Documentation\n" ++ "let prompt = ()\n";

module D = (DocEnv: DocEnv) => {
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

  let find_key_opt = (key, specs: list(p('code))) => {
    specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | YourImpl
    | HiddenTests;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type spec = p(Zipper.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type transitionary_spec = p(CodeString.t);

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

  let editor_positions = [YourImpl, HiddenTests];

  let positioned_editors = state =>
    List.combine(editor_positions, editors(state));

  let idx_of_pos = (pos, p: p('code)) =>
    switch (pos) {
    | YourImpl => 0

    | HiddenTests => 0 + List.length(p.hidden_tests.tests) // NEED TO FIGURE OUT HOW TO ACTUALLY MAKE THIS WORK
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
      // =>
      // Update the `hidden_tests` editor when position is HiddenTests
      // print_endline("Updating HiddenTests editor");
      // let updated_hidden_tests = documentation.eds.hidden_tests.tests;
      // {
      //   // ...documentation,
      //   eds: {
      //     ...documentation.eds,
      //     hidden_tests: {
      //       ...documentation.eds.hidden_tests,
      //       tests: updated_hidden_tests,
      //     },
      //     your_impl: documentation.eds.your_impl,
      //   },
      //   pos: HiddenTests,
      // };
      // | YourImpl =>
      //   // Update the `your_impl` editor when position is YourImpl
      //   print_endline("Updating YourImpl editor");
      //   let updated_your_impl = documentation.eds.your_impl;
      //   {
      //     //   ...documentation,
      //     eds: {
      //       ...documentation.eds,
      //       your_impl: updated_your_impl,
      //       hidden_tests: documentation.eds.hidden_tests,
      //     },
      //     pos: YourImpl,
      //   };
      | _ => {
          // Handle other cases by just updating the position
          eds: documentation.eds,
          pos,
        }
      };
    } else {
      {
        // When instructor_mode is enabled, decide if further logic is needed
        // In this case, just return the documentation with updated position
        eds: documentation.eds,
        pos,
      };
    };

  let zipper_of_code = code => {
    switch (Printer.zipper_of_string(code)) {
    | None => failwith("Transition failed.")
    | Some(zipper) => zipper
    };
  };

  let transition: transitionary_spec => spec =
    ({title, description, your_impl, hidden_tests}) => {
      let your_impl = zipper_of_code(your_impl);
      let hidden_tests = {
        let {tests, hints} = hidden_tests;
        let tests = zipper_of_code(tests);
        {tests, hints};
      };
      {title, description, your_impl, hidden_tests};
    };

  let editor_of_serialization = zipper => Editor.init(zipper);
  let eds_of_spec: spec => eds =
    ({title, description, your_impl, hidden_tests}) => {
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
    | HiddenTests => instructor_mode
    };
  };

  let state_of_spec = (spec, ~instructor_mode: bool): state => {
    let eds = eds_of_spec(spec);
    set_instructor_mode({pos: HiddenTests, eds}, instructor_mode);
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
      )
      // ~editing_title: bool,
      : state => {
    let lookup = (pos, default) =>
      if (visible_in(pos, ~instructor_mode)) {
        let persisted_zipper = List.assoc(pos, editors);
        let zipper = PersistentZipper.unpersist(persisted_zipper);
        Editor.init(zipper);
      } else {
        editor_of_serialization(default);
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

  module TermItem = {
    type t = {
      term: TermBase.UExp.t,
      term_ranges: TermRanges.t,
    };
  };

  module StaticsItem = {
    type t = CachedStatics.statics;
  };

  type stitched('a) = {
    user_impl: 'a, // prelude + your_impl
    instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
    hidden_tests: 'a,
  };

  let wrap_filter = (act: FilterAction.action, term: Term.UExp.t): Term.UExp.t =>
    TermBase.UExp.{
      term:
        TermBase.UExp.Filter(
          FilterAction.(act, One),
          {term: Constructor("$e"), ids: [Id.mk()]},
          term,
        ),
      ids: [Id.mk()],
    };

  let wrap = (term, editor: Editor.t): TermItem.t => {
    term,
    term_ranges: editor.state.meta.term_ranges,
  };

  let term_of = (editor: Editor.t): Term.UExp.t =>
    editor.state.meta.view_term;

  let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
    EditorUtil.append_exp(
      EditorUtil.append_exp(term_of(ed1), term_of(ed2)),
      term_of(ed3),
    );

  let stitch1 = (ed1: Editor.t) =>
    // EditorUtil.append_exp(
    EditorUtil.append_exp(term_of(ed1));

  let stitch_term = ({eds, _}: state): stitched(TermItem.t) => {
    let instructor = eds.hidden_tests.tests |> term_of;
    let user_impl_term = {
      eds.your_impl |> term_of |> wrap_filter(FilterAction.Step);
    };

    // No combining of your_impl_term with hidden_tests
    let hidden_tests_term = term_of(eds.hidden_tests.tests);

    {
      user_impl: wrap(user_impl_term, eds.your_impl),
      instructor: wrap(instructor, eds.hidden_tests.tests),
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
      user_impl: mk(t.user_impl),
      instructor,
      hidden_tests: mk(t.hidden_tests),
    };
  };

  let stitch_static = Core.Memo.general(stitch_static);

  let statics_of_stiched =
      (state: state, s: stitched(StaticsItem.t)): StaticsItem.t =>
    switch (state.pos) {
    | YourImpl => s.user_impl
    | HiddenTests => s.hidden_tests
    };

  let statics_of = (~settings, documentation: state): StaticsItem.t =>
    documentation
    |> stitch_term
    |> stitch_static(settings)
    |> statics_of_stiched(documentation);

  let user_impl_key = "user_impl";
  let instructor_key = "instructor";
  let hidden_tests_key = "hidden_tests";

  let key_for_statics = (state: state): string =>
    switch (state.pos) {
    | YourImpl => user_impl_key
    | HiddenTests => hidden_tests_key
    };

  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, DHExp.t)) => {
    let {user_impl, instructor, hidden_tests} =
      stitch_static(settings, stitch_term(state));
    let elab = (s: CachedStatics.statics) =>
      Interface.elaborate(~settings, s.info_map, s.term);
    [
      (user_impl_key, elab(user_impl)),
      (instructor_key, elab(instructor)),
      (hidden_tests_key, elab(hidden_tests)),
    ];
  };

  let mk_statics =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, StaticsItem.t)) => {
    let stitched = stitch_static(settings, stitch_term(state));
    [
      (user_impl_key, stitched.user_impl),
      (instructor_key, stitched.instructor),
      (hidden_tests_key, stitched.hidden_tests),
    ];
  };

  module DynamicsItem = {
    type t = {
      term: TermBase.UExp.t,
      info_map: Statics.Map.t,
      result: ModelResult.t,
    };
    let empty: t = {
      term: {
        term: Tuple([]),
        ids: [Id.mk()],
      },
      info_map: Id.Map.empty,
      result: NoElab,
    };
    let statics_only = ({term, info_map, _}: StaticsItem.t): t => {
      {term, info_map, result: NoElab};
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
    let {user_impl, instructor, hidden_tests} =
      stitch_static(settings, stitch_term(state));
    let result_of = key =>
      switch (results) {
      | None => ModelResult.NoElab
      | Some(results) =>
        ModelResults.lookup(results, key)
        |> Option.value(~default=ModelResult.NoElab)
      };

    let user_impl =
      DynamicsItem.{
        term: user_impl.term,
        info_map: user_impl.info_map,
        result: result_of(user_impl_key),
      };
    let instructor =
      DynamicsItem.{
        term: instructor.term,
        info_map: instructor.info_map,
        result: result_of(instructor_key),
      };
    let hidden_tests =
      DynamicsItem.{
        term: hidden_tests.term,
        info_map: hidden_tests.info_map,
        result: result_of(hidden_tests_key),
      };
    {user_impl, instructor, hidden_tests};
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
        user_impl: DynamicsItem.statics_only(t.user_impl),
        instructor: DynamicsItem.statics_only(t.instructor),
        hidden_tests: DynamicsItem.statics_only(t.hidden_tests),
      };
    } else {
      {
        user_impl: DynamicsItem.empty,
        instructor: DynamicsItem.empty,
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
      ++ "let exercise: DocumentationEnv.spec = ";
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
      ++ "let exercise: DocumentationEnv.spec = DocumentationEnv.transition(";
    let record = show_p(transitionary_editor_pp, eds);
    let data = prefix ++ record ++ ")\n";
    data;
  };

  let export_grading_module = (module_name, {eds, _}: state) => {
    let header = output_header_grading(module_name);
    let prefix = "let exercise: DocumentationEnv.spec = ";
    let record = show_p(editor_pp, eds);
    let data = header ++ prefix ++ record ++ "\n";
    data;
  };

  let blank_spec = (~title, ~description) => {
    let your_impl_m = Zipper.next_blank();
    let hidden_tests_tests = Zipper.next_blank();
    {
      title,
      description,
      your_impl: your_impl_m,
      hidden_tests: {
        tests: hidden_tests_tests,
        hints: [],
      },
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
