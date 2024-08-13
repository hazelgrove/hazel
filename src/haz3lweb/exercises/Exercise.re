open Util;
open Haz3lcore;
open Web;

let output_header_grading = _module_name =>
  "module Exercise = GradePrelude.Exercise\n" ++ "let prompt = ()\n";

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
  title: string,
  version: int,
  module_name: string,
  prompt:
    [@printer (fmt, _) => Format.pp_print_string(fmt, "prompt")] [@opaque] Node.t,
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

let map = (p: p('a), f: 'a => 'b, f_hidden: 'a => 'b): p('b) => {
  {
    title: p.title,
    version: p.version,
    module_name: p.module_name,
    prompt: p.prompt,
    point_distribution: p.point_distribution,
    prelude: f_hidden(p.prelude),
    correct_impl: f_hidden(p.correct_impl),
    your_tests: {
      tests: f(p.your_tests.tests),
      required: p.your_tests.required,
      provided: p.your_tests.provided,
    },
    your_impl: f(p.your_impl),
    hidden_bugs:
      p.hidden_bugs
      |> List.map(wrong_impl => {
           {impl: f_hidden(wrong_impl.impl), hint: wrong_impl.hint}
         }),
    hidden_tests: {
      tests: f_hidden(p.hidden_tests.tests),
      hints: p.hidden_tests.hints,
    },
    syntax_tests: p.syntax_tests,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type eds = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {eds};

let key_of_state = eds => key_of(eds);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = list((pos, PersistentZipper.t));

let main_editor_of_state = (~selection: pos, eds) =>
  switch (selection) {
  | Prelude => eds.prelude
  | CorrectImpl => eds.correct_impl
  | YourTestsValidation => eds.your_tests.tests
  | YourTestsTesting => eds.your_tests.tests
  | YourImpl => eds.your_impl
  | HiddenBugs(i) => List.nth(eds.hidden_bugs, i).impl
  | HiddenTests => eds.hidden_tests.tests
  };

let put_main_editor = (~selection: pos, eds: p('a), editor: 'a): p('a) =>
  switch (selection) {
  | Prelude => {...eds, prelude: editor}
  | CorrectImpl => {...eds, correct_impl: editor}
  | YourTestsValidation
  | YourTestsTesting => {
      ...eds,
      your_tests: {
        ...eds.your_tests,
        tests: editor,
      },
    }
  | YourImpl => {...eds, your_impl: editor}
  | HiddenBugs(n) => {
      ...eds,
      hidden_bugs:
        Util.ListUtil.put_nth(
          n,
          {...List.nth(eds.hidden_bugs, n), impl: editor},
          eds.hidden_bugs,
        ),
    }
  | HiddenTests => {
      ...eds,
      hidden_tests: {
        ...eds.hidden_tests,
        tests: editor,
      },
    }
  };

let editors = eds =>
  [
    eds.prelude,
    eds.correct_impl,
    eds.your_tests.tests,
    eds.your_tests.tests,
    eds.your_impl,
  ]
  @ List.map(wrong_impl => wrong_impl.impl, eds.hidden_bugs)
  @ [eds.hidden_tests.tests];

let editor_positions = eds =>
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

let eds_of_spec =
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
      ~settings: CoreSettings.t,
    ) => {
  let editor_of_serialization = Editor.init(~settings);
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

let set_instructor_mode = ({eds}: state, new_mode: bool) => {
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
  | YourTestsTesting => true
  | YourImpl => true
  | HiddenBugs(_) => instructor_mode
  | HiddenTests => instructor_mode
  };
};

// # Stitching

module TermItem = {
  type t = {
    term: Exp.t,
    editor: Editor.t,
  };
};

module StaticsItem = {
  type t = CachedStatics.statics;
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
  | YourTestsValidation => {...s, test_validation: x}
  | YourImpl => {...s, user_impl: x}
  | YourTestsTesting => {...s, user_tests: x}
  | Prelude => {...s, prelude: x}
  | CorrectImpl => {...s, instructor: x}
  | HiddenBugs(i) => {
      ...s,
      hidden_bugs: Util.ListUtil.put_nth(i, x, s.hidden_bugs),
    }
  | HiddenTests => {...s, hidden_tests: x}
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

let wrap = (term, editor: Editor.t): TermItem.t => {term, editor};

let term_of = (editor: Editor.t): UExp.t =>
  MakeTerm.from_zip_for_sem(editor.state.zipper).term;

let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
  EditorUtil.append_exp(
    EditorUtil.append_exp(term_of(ed1), term_of(ed2)),
    term_of(ed3),
  );

let stitch_term = (eds: p('a)): stitched(TermItem.t) => {
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
    prompt: Node.text("TODO: prompt"),
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

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_exercise_mode = list((pos, PersistentZipper.t));

let unpersist = (~instructor_mode, positioned_zippers, spec: spec): spec => {
  let lookup = (pos, default) =>
    if (visible_in(pos, ~instructor_mode)) {
      positioned_zippers
      |> List.assoc_opt(pos)
      |> Option.map(PersistentZipper.unpersist)
      |> Option.value(~default);
    } else {
      default;
    };
  let prelude = lookup(Prelude, spec.prelude);
  let correct_impl = lookup(CorrectImpl, spec.correct_impl);
  let your_tests_tests = lookup(YourTestsValidation, spec.your_tests.tests);
  let your_impl = lookup(YourImpl, spec.your_impl);
  let (_, hidden_bugs) =
    List.fold_left(
      ((i, hidden_bugs: list(wrong_impl('a))), {impl, hint}) => {
        let impl = lookup(HiddenBugs(i), impl);
        (i + 1, hidden_bugs @ [{impl, hint}]);
      },
      (0, []),
      spec.hidden_bugs,
    );
  let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);
  {
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
  };
};
