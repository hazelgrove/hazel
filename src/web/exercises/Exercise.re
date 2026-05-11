/* The common interface for an exercise, across all kinds.

   Code-exercise-specific logic (points, hidden tests, stitching, etc.) lives
   in [CodeExercise.re]. Derivation- and Theorem-specific logic lives in
   [DerivationExercise.re] and [TheoremExercise.re] respectively. This module
   is just the thin dispatcher that unifies them. */

/* Sum type over all exercise kinds. An exercise file should produce a value
   of this type, tagged with the appropriate constructor, so
   [[ExerciseSettings_base.re]] can simply list them without additional
   wrapping. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Code(CodeExercise.spec)
  | Derivation(DerivationExercise.spec)
  | Theorem(TheoremExercise.spec);

let id_of = (e: t): Haz3lcore.Id.t =>
  switch (e) {
  | Code(s) => s.id
  | Derivation(s) => s.id
  | Theorem(s) => s.id
  };

let title_of = (e: t): string =>
  switch (e) {
  | Code(s) => s.title
  | Derivation(s) => s.title
  | Theorem(s) => s.title
  };

let module_name_of = (e: t): string =>
  switch (e) {
  | Code(s) => s.module_name
  | Derivation(s) => s.module_name
  | Theorem(s) => s.module_name
  };

let max_points_of = (e: t): int =>
  switch (e) {
  | Code(s) =>
    let {test_validation, mutation_testing, impl_grading}: CodeExercise.point_distribution =
      s.point_distribution;
    test_validation + mutation_testing + impl_grading;
  | Derivation(s) => s.max_points
  | Theorem(s) => s.max_points
  };

let zipper_of_code = code => {
  switch (Parser.to_zipper(code)) {
  | None => failwith("Transition failed.")
  | Some(zipper) => zipper
  };
};

let transition: transitionary_spec => spec =
  (
    {
      id,
      title,
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
      {
        tests,
        required: your_tests.required,
        provided: your_tests.provided,
      };
    };
    let your_impl = zipper_of_code(your_impl);
    let hidden_bugs =
      List.fold_left(
        (acc, {impl, hint}) => {
          let impl = zipper_of_code(impl);
          acc
          @ [
            {
              impl,
              hint,
            },
          ];
        },
        [],
        hidden_bugs,
      );
    let hidden_tests = {
      let {tests, hints} = hidden_tests;
      let tests = zipper_of_code(tests);
      {
        tests,
        hints,
      };
    };
    {
      id,
      title,
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

let eds_of_spec =
    (
      {
        id,
        title,
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
      ~settings as _: Language.CoreSettings.t,
    ) => {
  let editor_of_serialization = Editor.Model.mk;
  let prelude = editor_of_serialization(prelude);
  let correct_impl = editor_of_serialization(correct_impl);
  let your_tests = {
    let tests = editor_of_serialization(your_tests.tests);
    {
      tests,
      required: your_tests.required,
      provided: your_tests.provided,
    };
  };
  let your_impl = editor_of_serialization(your_impl);
  let hidden_bugs =
    hidden_bugs
    |> List.map(({impl, hint}) => {
         let impl = editor_of_serialization(impl);
         {
           impl,
           hint,
         };
       });
  let hidden_tests = {
    let {tests, hints} = hidden_tests;
    let tests = editor_of_serialization(tests);
    {
      tests,
      hints,
    };
  };
  {
    id,
    title,
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

let visible_in = (pos, ~instructor_mode) => {
  switch (pos) {
  | Prelude => instructor_mode
  | CorrectImpl => instructor_mode
  | YourTestsValidation => true
  | YourTestsTesting => true
  | YourImpl => true
  | HiddenBugs(_) => instructor_mode
  | HiddenTests => instructor_mode
  };
};

let update_exercise_title = ({eds, _}: state, new_title: string) => {
  eds: {
    ...eds,
    title: new_title,
  },
};

let add_buggy_impl = (state: state) => {
  let new_buggy_impl = {
    impl: Editor.Model.mk(Zipper.init()),
    hint: "No Hint Available",
  };
  {
    eds: {
      ...state.eds,
      hidden_bugs: state.eds.hidden_bugs @ [new_buggy_impl],
    },
  };
};

let delete_buggy_impl = (state: state, index: int) => {
  {
    eds: {
      ...state.eds,
      hidden_bugs: List.filteri((i, _) => i != index, state.eds.hidden_bugs),
    },
  };
};

let edit_buggy_impl = (state: state, idx: int, impl: Editor.t, new_hint: hint) => {
  let buggy_impl = {
    impl,
    hint: new_hint,
  };
  {
    eds: {
      ...state.eds,
      hidden_bugs:
        Util.ListUtil.put_nth(idx, buggy_impl, state.eds.hidden_bugs),
    },
  };
};

let update_exercise_prompt = ({eds}: state, new_prompt: string) => {
  eds: {
    ...eds,
    prompt: new_prompt,
  },
};

let update_test_val_rep = ({eds}: state, new_test_num: int, new_dist: int) => {
  eds: {
    ...eds,
    your_tests: {
      ...eds.your_tests,
      required: new_test_num < 0 ? 0 : new_test_num,
    },
    point_distribution: {
      ...eds.point_distribution,
      test_validation: new_dist < 0 ? 0 : new_dist,
    },
  },
};

let update_mut_test_rep =
    ({eds}: state, new_dist: int, new_hints: list(string)) => {
  let updated_bugs =
    List.mapi(
      (i, bug) => {
        let new_hint = List.nth_opt(new_hints, i);
        switch (new_hint) {
        | Some(hint) => {
            ...bug,
            hint,
          }
        | None => bug
        };
      },
      eds.hidden_bugs,
    );
  {
    eds: {
      ...eds,
      hidden_bugs: updated_bugs,
      point_distribution: {
        ...eds.point_distribution,
        mutation_testing: new_dist < 0 ? 0 : new_dist,
      },
    },
  };
};

let update_impl_grd_rep =
    ({eds}: state, new_dist: int, new_hints: list(string)) => {
  {
    eds: {
      ...eds,
      hidden_tests: {
        ...eds.hidden_tests,
        hints: new_hints,
      },
      point_distribution: {
        ...eds.point_distribution,
        impl_grading: new_dist < 0 ? 0 : new_dist,
      },
    },
  };
};

let update_syntax_rep = ({eds}: state, new_hints: list(string)) => {
  eds: {
    ...eds,
    syntax_tests:
      List.mapi(
        (i, (_, predicate)) => {
          let new_hint = List.nth_opt(new_hints, i);
          switch (new_hint) {
          | Some(hint) => (hint, predicate)
          | None => ("No Hint Provided", predicate)
          };
        },
        eds.syntax_tests,
      ),
  },
};

let update_module_name = ({eds}: state, new_module_name: string) => {
  eds: {
    ...eds,
    module_name: new_module_name,
  },
};

let update_prov_tests = ({eds}: state, new_prov_tests: int) => {
  eds: {
    ...eds,
    your_tests: {
      ...eds.your_tests,
      provided: new_prov_tests,
    },
  },
};

// # Stitching

module TermItem = {
  type t = {
    term: Language.Exp.t,
    editor: Editor.t,
  };
};

module StaticsItem = {
  type t = CachedStatics.t;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type stitched('a) = {
  test_validation: 'a, // prelude + correct_impl + your_tests
  user_impl: 'a, // prelude + your_impl
  user_tests: 'a, // prelude + your_impl + your_tests
  prelude: 'a, // prelude
  instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
  hidden_bugs: list('a), // prelude + hidden_bugs[i].impl + your_tests,
  hidden_tests: 'a,
};

let map_stitched = (f: (pos, 'a) => 'b, s: stitched('a)): stitched('b) => {
  test_validation: f(YourTestsValidation, s.test_validation),
  user_impl: f(YourImpl, s.user_impl),
  user_tests: f(YourTestsTesting, s.user_tests),
  prelude: f(Prelude, s.prelude),
  instructor: f(CorrectImpl, s.instructor),
  hidden_bugs: List.mapi((i, p) => f(HiddenBugs(i), p), s.hidden_bugs),
  hidden_tests: f(HiddenTests, s.hidden_tests),
};

let get_stitched = (pos, s: stitched('a)): 'a =>
  switch (pos) {
  | YourTestsValidation => s.test_validation
  | YourImpl => s.user_impl
  | YourTestsTesting => s.user_tests
  | Prelude => s.prelude
  | CorrectImpl => s.instructor
  | HiddenBugs(i) => List.nth(s.hidden_bugs, i)
  | HiddenTests => s.hidden_tests
  };

let map2_stitched =
    (f: (pos, 'a, 'b) => 'c, s1: stitched('a), s2: stitched('b))
    : stitched('c) =>
  map_stitched((pos, a) => f(pos, a, get_stitched(pos, s2)), s1);

let put_stitched = (pos, s: stitched('a), x: 'a): stitched('a) =>
  switch (pos) {
  | YourTestsValidation => {
      ...s,
      test_validation: x,
    }
  | YourImpl => {
      ...s,
      user_impl: x,
    }
  | YourTestsTesting => {
      ...s,
      user_tests: x,
    }
  | Prelude => {
      ...s,
      prelude: x,
    }
  | CorrectImpl => {
      ...s,
      instructor: x,
    }
  | HiddenBugs(i) => {
      ...s,
      hidden_bugs: Util.ListUtil.put_nth(i, x, s.hidden_bugs),
    }
  | HiddenTests => {
      ...s,
      hidden_tests: x,
    }
  };

let wrap_filter =
    (act: Language.FilterAction.action, term: Language.Exp.t): Language.Exp.t => {
  term:
    Filter(
      Filter({
        act: Language.FilterAction.(act, One),
        pat: {
          term:
            Constructor(
              "$e",
              Some(
                Some(
                  Unknown(Internal |> Language.Prov.fresh)
                  |> Language.Typ.fresh,
                ),
              ),
            ),
          annotation: {
            ids: [Id.mk()],
          },
        },
      }),
      term,
    ),
  annotation: {
    ids: [Id.mk()],
  },
};

let wrap = (term, editor: Editor.t): TermItem.t => {
  term,
  editor,
};

let term_of = (editor: Editor.t): Language.Exp.t =>
  MakeTerm.from_zip_for_sem(editor.state.zipper).term;

let rec append_exp = (e1: Language.Exp.t, e2: Language.Exp.t): Language.Exp.t => {
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Undefined
  | Deferral(_)
  | Atom(_)
  | ListLit(_)
  | TupleExtension(_)
  | Constructor(_)
  | Closure(_)
  | Fun(_)
  | TypFun(_)
  | FixF(_)
  | Forall(_)
  | Tuple(_)
  | TupLabel(_)
  | Label(_)
  | ExplicitNonlabel
  | Dot(_)
  | Var(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Test(_)
  | HintedTest(_)
  | Parens(_)
  | Probe(_)
  | Cons(_)
  | ListConcat(_)
  | LivelitName(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Asc(_)
  | ProofObject(_)
  | Match(_) => {
      term: Seq(e1, e2),
      annotation: {
        ids: [Id.mk()],
      },
    }
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    {
      term: Seq(e11, e12'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Filter(kind, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Filter(kind, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Let(p, edef, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Theorem(p, thm, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Theorem(p, thm, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: TyAlias(tp, tdef, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  | Use(t, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Use(t, ebody'),
      annotation: {
        ids: Language.IdTagged.ids(e1),
      },
    };
  };
};

let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
  append_exp(append_exp(term_of(ed1), term_of(ed2)), term_of(ed3));

let stitch_term = (eds: p('a)): stitched(TermItem.t) => {
  let instructor =
    stitch3(eds.prelude, eds.correct_impl, eds.hidden_tests.tests);
  let user_impl_term = {
    let your_impl_term =
      eds.your_impl |> term_of |> wrap_filter(Language.FilterAction.Step);
    let prelude_term =
      eds.prelude |> term_of |> wrap_filter(Language.FilterAction.Eval);
    append_exp(prelude_term, your_impl_term);
  };
  let test_validation_term =
    stitch3(eds.prelude, eds.correct_impl, eds.your_tests.tests);
  let user_tests_term =
    append_exp(user_impl_term, term_of(eds.your_tests.tests));
  let hidden_tests_term =
    append_exp(user_impl_term, term_of(eds.hidden_tests.tests));
  {
    test_validation: wrap(test_validation_term, eds.your_tests.tests),
    /* Passing tests term to user_impl so probes in impl reflect tests: */
    //user_impl: wrap(user_impl_term, eds.your_impl),
    user_impl: wrap(user_tests_term, eds.your_impl),
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

let prelude_key = "prelude";
let test_validation_key = "test_validation";
let user_impl_key = "user_impl";
let user_tests_key = "user_tests";
let instructor_key = "instructor";
let hidden_bugs_key = n => "hidden_bugs_" ++ string_of_int(n);
let hidden_tests_key = "hidden_tests";

let key_for_statics = (pos: pos): string =>
  switch (pos) {
  | Prelude => prelude_key
  | CorrectImpl => instructor_key
  | YourTestsValidation => test_validation_key
  | YourTestsTesting => user_tests_key
  | YourImpl => user_impl_key
  | HiddenBugs(idx) => hidden_bugs_key(idx)
  | HiddenTests => hidden_tests_key
  };

let pos_of_key = (key: string): pos =>
  switch () {
  | _ when key == prelude_key => Prelude
  | _ when key == test_validation_key => YourTestsValidation
  | _ when key == user_impl_key => YourImpl
  | _ when key == user_tests_key => YourTestsTesting
  | _ when key == instructor_key => CorrectImpl
  | _ when String.starts_with(key, ~prefix="hidden_bugs_") =>
    let n =
      String.sub(
        key,
        String.length("hidden_bugs_"),
        String.length(key) - String.length("hidden_bugs_"),
      );
    HiddenBugs(int_of_string(n));
  | _ when key == hidden_tests_key => HiddenTests
  | _ => failwith("invalid key")
  };

// // Module Export

let editor_pp = (fmt, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let serialization = Zipper.show(zipper);
  // let string_literal = "\"" ++ String.escaped(serialization) ++ "\"";
  Format.pp_print_string(fmt, serialization);
};

let export_module = ({eds, _}: state) => {
  let prefix = "open Haz3lcore\n\n" ++ "let exercise: Exercise.spec = \n";
  let record = show_p(editor_pp, eds);
  let data = prefix ++ record ++ "\n";
  data;
};

let transitionary_editor_pp = (fmt, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let code = PersistentZipper.to_string(zipper);
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
        {
          impl: zipper,
          hint: "TODO: hint " ++ string_of_int(i),
        };
      },
    );
  let hidden_tests_tests = Zipper.next_blank();
  {
    id: Id.mk(),
    title,
    module_name,
    prompt: "",
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

let persist = (state: state, ~instructor_mode: bool) => {
  let zippers =
    positioned_editors(state.eds)
    |> List.filter(((pos, _)) => visible_in(pos, ~instructor_mode))
    |> List.map(((pos, editor: Editor.t)) => {
         (pos, PersistentZipper.persist(editor.state.zipper))
       });
  let persistent_hidden_bugs =
    state.eds.hidden_bugs
    |> List.map(({impl, hint}: wrong_impl(Editor.t)) => {
         {
           impl: PersistentZipper.persist(impl.state.zipper),
           hint,
         }
       });
  {
    editors: zippers,
    title: state.eds.title,
    hidden_bugs: persistent_hidden_bugs,
    prompt: state.eds.prompt,
    point_distribution: state.eds.point_distribution,
    required: state.eds.your_tests.required,
    module_name: state.eds.module_name,
    syntax_tests: state.eds.syntax_tests,
    hidden_test_hints: state.eds.hidden_tests.hints,
  };
};

let unpersist =
    (
      {
        editors,
        title,
        hidden_bugs,
        prompt,
        point_distribution,
        required,
        module_name,
        syntax_tests,
        hidden_test_hints,
      }: persistent_state,
      ~spec: spec,
      ~instructor_mode: bool,
    )
    : state => {
  let lookup = (pos, default) =>
    if (visible_in(pos, ~instructor_mode)) {
      switch (List.assoc_opt(pos, editors)) {
      | Some(persisted_zipper) =>
        let zipper = PersistentZipper.unpersist(persisted_zipper);
        Editor.Model.mk(zipper);
      | None => Editor.Model.mk(default)
      };
    } else {
      Editor.Model.mk(default);
    };
  let prelude = lookup(Prelude, spec.prelude);
  let correct_impl = lookup(CorrectImpl, spec.correct_impl);
  let your_tests_tests = lookup(YourTestsValidation, spec.your_tests.tests);
  let your_impl = lookup(YourImpl, spec.your_impl);
  let hidden_bugs =
    hidden_bugs
    |> List.map(({impl, hint}) => {
         let impl = Editor.Model.mk(PersistentZipper.unpersist(impl));
         {
           impl,
           hint,
         };
       });
  let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);
  {
    eds: {
      id: spec.id,
      title,
      module_name,
      prompt,
      point_distribution,
      prelude,
      correct_impl,
      your_tests: {
        tests: your_tests_tests,
        required,
        provided: spec.your_tests.provided,
      },
      your_impl,
      hidden_bugs,
      hidden_tests: {
        tests: hidden_tests_tests,
        hints: hidden_test_hints,
      },
      syntax_tests,
    },
  };
};
