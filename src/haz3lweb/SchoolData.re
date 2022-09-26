open Sexplib.Std;
open Haz3lcore;

module type Node = {
  type node;
  let default: node;
};

module SchoolExercise = (Node: Node) => {
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
  type pos =
    | Prelude
    | CorrectImpl
    | YourTestsValidation
    | YourTestsTesting
    | YourImpl
    | HiddenBugs(int)
    | HiddenTests;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type key = (string, int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type p('code) = {
    next_id: Id.t,
    title: string,
    version: int,
    module_name: string,
    prompt:
      [@printer (fmt, _) => Format.pp_print_string(fmt, "prompt")] [@opaque] Node.node,
    point_distribution,
    prelude: 'code,
    correct_impl: 'code,
    your_tests: your_tests('code),
    your_impl: 'code,
    hidden_bugs: list(wrong_impl('code)),
    hidden_tests: hidden_tests('code),
  };

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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type spec = p(Zipper.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type eds = p(Editor.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = {
    pos,
    eds,
  };

  let switch_editor = (idx: int, {eds, _}) => {
    pos: pos_of_idx(eds, idx),
    eds,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_state = (pos, Id.t, list((pos, Zipper.t)));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type school_export = {
    cur_exercise: key,
    exercise_data: list((key, persistent_state)),
  };

  let key_of = (p: p('code)) => {
    (p.title, p.version);
  };

  let find_key_opt = (key, specs: list(p('code))) => {
    specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);
  };

  let key_of_state = ({eds, _}) => key_of(eds);

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

  let editor_of_serialization = zipper => Editor.init(zipper);
  let eds_of_spec: spec => eds =
    (
      {
        next_id,
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
        next_id,
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
      };
    };

  let set_instructor_mode = ({eds, _} as state: state, new_mode: bool) => {
    ...state,
    eds: {
      ...eds,
      prelude: Editor.set_read_only(eds.prelude, !new_mode),
    },
  };

  let state_of_spec = (spec, ~instructor_mode: bool): state => {
    let eds = eds_of_spec(spec);
    set_instructor_mode({pos: YourImpl, eds}, instructor_mode);
  };

  let positioned_editors = state =>
    List.combine(editor_positions(state), editors(state));

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
      ++ "let exercise: SchoolExercise.spec = ";
    let record = show_p(editor_pp, eds);
    let data = prefix ++ record;
    print_endline(data);
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
      )
      : p('code) => {
    let id = 0;
    let (id, prelude) = Zipper.next_blank(id);
    let (id, correct_impl) = Zipper.next_blank(id);
    let (id, your_tests_tests) = Zipper.next_blank(id);
    let (id, your_impl) = Zipper.next_blank(id);
    let (id, hidden_bugs) =
      Util.ListUtil.init_fold(
        num_wrong_impls,
        id,
        (i, id) => {
          let (id, zipper) = Zipper.next_blank(id);
          (id, {impl: zipper, hint: "TODO: hint " ++ string_of_int(i)});
        },
      );
    let (id, hidden_tests_tests) = Zipper.next_blank(id);
    {
      next_id: id,
      title,
      version: 1,
      module_name,
      prompt: Node.default,
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
    };
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
        (next_id, editor_of_serialization(default));
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
        },
      },
      instructor_mode,
    );
  };

  let deserialize_school_export = data => {
    data |> Sexplib.Sexp.of_string |> school_export_of_sexp;
  };
};
