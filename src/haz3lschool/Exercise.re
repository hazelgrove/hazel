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
  type derivation('code) = {
    jdmt: 'code,
    rule: Derivation.Rule.t,
  };

  let map_jdmt = (f, {jdmt, rule}) => {jdmt: f(jdmt), rule};

  let put_jdmt = (jdmt, {rule, _}) => {jdmt, rule};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type derivation_tree('code) = Util.Tree.t(derivation('code));

  // let mk_derivation_tree = t => {
  //   let (flattened, tree) = Util.FlatTree.init_with(100, t);
  //   let (judgements, rules) = List.split(flattened);
  //   let judgements =
  //     judgements @ List.init(100 - List.length(judgements), Fun.const(""));
  //   let rules =
  //     rules
  //     @ List.init(
  //         100 - List.length(rules),
  //         Fun.const(Derivation.Rule.Assumption),
  //       );
  //   {judgements, rules, tree};
  // };

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
    derivation_tree: derivation_tree('code),
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
    | HiddenTests
    | Derive(Util.Tree.pos);

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
      derivation_tree: p.derivation_tree |> Util.Tree.map(f |> map_jdmt),
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
      | Derive(pos) => Util.Tree.nth(eds.derivation_tree, pos).jdmt
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
    | Derive(pos) => {
        ...state,
        eds: {
          ...eds,
          derivation_tree:
            Util.Tree.map_nth(put_jdmt(editor), eds.derivation_tree, pos),
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
    @ [eds.hidden_tests.tests]
    @ (eds.derivation_tree |> Util.Tree.map(d => d.jdmt) |> Util.Tree.flatten);

  let editor_positions = ({eds, _}: state) =>
    [Prelude, CorrectImpl, YourTestsTesting, YourTestsValidation, YourImpl]
    @ List.mapi((i, _) => HiddenBugs(i), eds.hidden_bugs)
    @ [HiddenTests]
    @ (
      eds.derivation_tree
      |> Util.Tree.flatten_pos
      |> List.map(pos => Derive(pos))
    );

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
    // Node(zhiyao): This could be wrong
    | Derive(_) => 5 + List.length(p.hidden_bugs) + 1
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
                   // Node(zhiyao): This could be wrong
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

  let switch_derivation_rule = (~pos, ~new_rule, ~exercise) =>
    switch (pos) {
    | Derive(pos) =>
      let tree = exercise.eds.derivation_tree;
      let Node({jdmt, rule}, children) = Util.Tree.nth_node(tree, pos);
      let old_prems_num = Derivation.Rule.prem_num(rule);
      let new_prems_num = Derivation.Rule.prem_num(new_rule);
      let delta = new_prems_num - old_prems_num;
      let rec iter = (f, n, x) => n == 0 ? x : iter(f, n - 1, f(x));
      let children =
        if (delta > 0) {
          iter(
            fun
            | c => [
                Util.Tree.init(_ =>
                  {jdmt: Editor.init(zipper_of_code("")), rule: Assumption}
                ),
                ...c,
              ],
            delta,
            children,
          );
        } else {
          iter(
            fun
            | c => List.tl(c),
            - delta,
            children,
          );
        };
      let tree =
        Util.Tree.put_nth_node(
          Node({jdmt, rule: new_rule}, children),
          tree,
          pos,
        );
      {
        ...exercise,
        eds: {
          ...exercise.eds,
          derivation_tree: tree,
        },
      };
    | _ => exercise
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
        derivation_tree,
      },
    ) => {
      let prelude = zipper_of_code(prelude);
      let correct_impl = zipper_of_code(correct_impl);
      let your_tests = {
        let tests = zipper_of_code(your_tests.tests);
        {tests, required: your_tests.required, provided: your_tests.provided};
      };
      let your_impl = zipper_of_code(your_impl);
      // Note(zhiyao): I think it's better to use List.map here
      // let hidden_bugs =
      //   List.fold_left(
      //     (acc, {impl, hint}) => {
      //       let impl = zipper_of_code(impl);
      //       acc @ [{impl, hint}];
      //     },
      //     [],
      //     hidden_bugs,
      //   );
      let hidden_bugs =
        hidden_bugs
        |> List.map(({impl, hint}) => {
             let impl = zipper_of_code(impl);
             {impl, hint};
           });
      let hidden_tests = {
        let {tests, hints} = hidden_tests;
        let tests = zipper_of_code(tests);
        {tests, hints};
      };
      let derivation_tree =
        derivation_tree |> Util.Tree.map(zipper_of_code |> map_jdmt);
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
        derivation_tree,
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
        derivation_tree,
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
      let derivation_tree =
        derivation_tree |> Util.Tree.map(editor_of_serialization |> map_jdmt);
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
        derivation_tree,
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
    | Derive(_) => true
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
    // Note(zhiyao): I think it's better to use List.mapi here
    // let (_, hidden_bugs) =
    //   List.fold_left(
    //     ((i, hidden_bugs: list(wrong_impl(Editor.t))), {impl, hint}) => {
    //       let impl = lookup(HiddenBugs(i), impl);
    //       (i + 1, hidden_bugs @ [{impl, hint}]);
    //     },
    //     (0, []),
    //     spec.hidden_bugs,
    //   );
    let hidden_bugs =
      spec.hidden_bugs
      |> List.mapi((i, {impl, hint}) => {
           let impl = lookup(HiddenBugs(i), impl);
           {impl, hint};
         });
    let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);
    let derivation_tree =
      spec.derivation_tree
      |> Util.Tree.mapi((pos, {jdmt, rule}) => {
           let jdmt = lookup(Derive(pos), jdmt);
           {jdmt, rule};
         });
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
          derivation_tree,
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
    test_validation: 'a, // prelude + correct_impl + your_tests
    user_impl: 'a, // prelude + your_impl
    user_tests: 'a, // prelude + your_impl + your_tests
    prelude: 'a, // prelude
    instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
    hidden_bugs: list('a), // prelude + hidden_bugs[i].impl + your_tests,
    hidden_tests: 'a,
    derivation_tree: Util.Tree.t('a) // prelude + your_impl + derivation
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
      derivation_tree:
        eds.derivation_tree
        |> Util.Tree.map(({jdmt, _}) =>
             wrap(
               jdmt |> term_of |> EditorUtil.append_exp(user_impl_term),
               jdmt,
             )
           ),
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
      derivation_tree: t.derivation_tree |> Util.Tree.map(mk),
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
    | Derive(pos) => Util.Tree.nth(s.derivation_tree, pos)
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

  let derivation_tree_key = {
    let rec aux = (acc, pos: Util.Tree.pos) =>
      switch (pos) {
      | Value => acc
      | Children(i, pos) => aux(acc ++ "_" ++ string_of_int(i), pos)
      };
    aux("derivation");
  };

  let key_for_statics = (state: state): string =>
    switch (state.pos) {
    | Prelude => prelude_key
    | CorrectImpl => instructor_key
    | YourTestsValidation => test_validation_key
    | YourTestsTesting => user_tests_key
    | YourImpl => user_impl_key
    | HiddenBugs(idx) => hidden_bugs_key(idx)
    | HiddenTests => hidden_tests_key
    | Derive(pos) => derivation_tree_key(pos)
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
      derivation_tree,
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
    )
    @ (
      derivation_tree
      |> Util.Tree.mapi((pos, si) => (derivation_tree_key(pos), elab(si)))
      |> Util.Tree.flatten
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
      )
    @ (
      stitched.derivation_tree
      |> Util.Tree.mapi((pos, si) => (derivation_tree_key(pos), si))
      |> Util.Tree.flatten
    );
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
    let {
      test_validation,
      user_impl,
      user_tests,
      prelude,
      instructor,
      hidden_bugs,
      hidden_tests,
      derivation_tree,
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
    let derivation_tree =
      derivation_tree
      |> Util.Tree.mapi((pos, statics_item: StaticsItem.t) =>
           DynamicsItem.{
             term: statics_item.term,
             info_map: statics_item.info_map,
             result: result_of(derivation_tree_key(pos)),
           }
         );
    {
      test_validation,
      user_impl,
      user_tests,
      instructor,
      prelude,
      hidden_bugs,
      hidden_tests,
      derivation_tree,
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
        derivation_tree:
          Util.Tree.map(DynamicsItem.statics_only, t.derivation_tree),
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
        derivation_tree:
          state.eds.derivation_tree |> Util.Tree.map(_ => DynamicsItem.empty),
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
    let derivation_tree = Util.Tree.init(Zipper.next_blank);
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
      derivation_tree:
        derivation_tree |> Util.Tree.map(jdmt => {jdmt, rule: Assumption}),
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
