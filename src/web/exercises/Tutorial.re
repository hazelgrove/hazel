open Util;
open Haz3lcore;
// open Web;

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
  id: Id.t,
  title: string,
  // description: string,
  version: int,
  module_name: string,
  prompt: string,
  //   point_distribution,
  //   prelude: 'code,
  // correct_impl: 'code,
  display_hint: string,
  your_impl: 'code,
  //   hidden_bugs: list(wrong_impl('code)),
  hidden_tests: hidden_tests('code),
  wrapper: bool,
  show_report: bool,
  //   syntax_tests,
};

let id_of = p => {
  p.id;
};

let find_id_opt = (id, specs: list(p('code))) => {
  specs |> Util.ListUtil.findi_opt(spec => id_of(spec) == id);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  //   | Prelude
  // | CorrectImpl
  //   | YourTestsValidation
  //   | YourTestsTesting
  | YourImpl
  //   | HiddenBugs(int)
  | HiddenTests;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = p(Zipper.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type transitionary_spec = p(string);

let map = (p: p('a), f: 'a => 'b, f_hidden: 'a => 'b): p('b) => {
  {
    id: p.id,
    title: p.title,
    // description: p.description,
    version: p.version,
    module_name: p.module_name,
    prompt: p.prompt,
    // point_distribution: p.point_distribution,
    // prelude: f_hidden(p.prelude),
    // correct_impl: f_hidden(p.correct_impl),
    // your_tests: {
    //   tests: f(p.your_tests.tests),
    //   required: p.your_tests.required,
    //   provided: p.your_tests.provided,
    // },
    display_hint: p.display_hint,
    your_impl: f(p.your_impl),
    // hidden_bugs:
    //   p.hidden_bugs
    //   |> List.map(wrong_impl => {
    //        {impl: f_hidden(wrong_impl.impl), hint: wrong_impl.hint}
    //      }),
    hidden_tests: {
      tests: f_hidden(p.hidden_tests.tests),
      hints: p.hidden_tests.hints,
    },
    wrapper: p.wrapper,
    show_report: p.show_report,
    // syntax_tests: p.syntax_tests,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type eds = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {eds};

// [@deriving (show({with_path: false}), sexp, yojson)]
// type persistent_state = list((pos, PersistentZipper.t));
// [@deriving (show({with_path: false}), sexp, yojson)]
// type persistent_state = {
//   title: string,
//   display_hint: string,
//   editors: list((pos, PersistentZipper.t)),
//   wrapper: bool,
//   show_report: bool,
// };

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = list((pos, PersistentZipper.t));

let main_editor_of_state = (~selection: pos, eds) =>
  switch (selection) {
  //   | Prelude => eds.prelude
  // | CorrectImpl => eds.correct_impl
  //   | YourTestsValidation => eds.your_tests.tests
  //   | YourTestsTesting => eds.your_tests.tests
  | YourImpl => eds.your_impl
  //   | HiddenBugs(i) => List.nth(eds.hidden_bugs, i).impl
  | HiddenTests => eds.hidden_tests.tests
  };

let put_main_editor = (~selection: pos, eds: p('a), editor: 'a): p('a) =>
  switch (selection) {
  //   | Prelude => {...eds, prelude: editor}
  // | CorrectImpl => {...eds, correct_impl: editor}
  //   | YourTestsValidation
  //   | YourTestsTesting => {
  //       ...eds,
  //       your_tests: {
  //         ...eds.your_tests,
  //         tests: editor,
  //       },
  //     }
  | YourImpl => {
      ...eds,
      your_impl: editor,
    }
  //   | HiddenBugs(n) => {
  //       ...eds,
  //       hidden_bugs:
  //         Util.ListUtil.put_nth(
  //           n,
  //           {...List.nth(eds.hidden_bugs, n), impl: editor},
  //           eds.hidden_bugs,
  //         ),
  //     }
  | HiddenTests => {
      ...eds,
      hidden_tests: {
        ...eds.hidden_tests,
        tests: editor,
      },
    }
  };

let editors = eds => [
  // eds.prelude,
  // eds.correct_impl,
  // eds.your_tests.tests,
  eds.your_impl,
  eds.hidden_tests.tests,
];
//   @ List.map(wrong_impl => wrong_impl.impl, eds.hidden_bugs)

let editor_positions = [YourImpl, HiddenTests];
//   [Prelude, CorrectImpl, YourTestsTesting, YourTestsValidation, YourImpl]
//   @ List.mapi((i, _) => HiddenBugs(i), eds.hidden_bugs)
//   @
[YourImpl, HiddenTests];

let positioned_editors = state =>
  List.combine(editor_positions, editors(state));

let idx_of_pos = pos =>
  switch (pos) {
  //   | Prelude => 0
  // | CorrectImpl => 0
  //   | YourTestsTesting => 2
  //   | YourTestsValidation => 3
  | YourImpl => 0
  //   | HiddenBugs(i) =>
  //     if (i < List.length(p.hidden_bugs)) {
  //       5 + i;
  //     } else {
  //       failwith("invalid hidden bug index");
  //     }
  | HiddenTests => 1
  //   5 + List.length(p.hidden_bugs)
  };

let pos_of_idx = (idx: int) =>
  switch (idx) {
  //   | 0 => Prelude
  // | 0 => CorrectImpl
  //   | 2 => YourTestsTesting
  //   | 3 => YourTestsValidation
  | 0 => YourImpl
  | _ =>
    if (idx < 0) {
      failwith(
        "negative idx",
        // } else if (idx < 5 + List.length(p.hidden_bugs)) {
        //   HiddenBugs(idx - 5);
        // } else if (idx == 0 + (+ List.length(p.hidden_tests.tests))) {
      );
    } else if (idx == 1) {
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

// let transition: transitionary_spec => spec =
//   (
//     {
//       title,
//       description,
//     //   version,
//     //   module_name,
//     //   prompt,
//     //   point_distribution,
//     //   prelude,
//     //   correct_impl,
//     //   your_tests,
//       your_impl,
//     //   hidden_bugs,
//       hidden_tests,
//     //   syntax_tests,
//     },
//   ) => {
//     // let prelude = zipper_of_code(prelude);
//     // let correct_impl = zipper_of_code(correct_impl);
//     // let your_tests = {
//     //   let tests = zipper_of_code(your_tests.tests);
//     //   {tests, required: your_tests.required, provided: your_tests.provided};
//     // };
//     let your_impl = zipper_of_code(your_impl);
//     // let hidden_bugs =
//     //   List.fold_left(
//     //     (acc, {impl, hint}) => {
//     //       let impl = zipper_of_code(impl);
//     //       acc @ [{impl, hint}];
//     //     },
//     //     [],
//     //     hidden_bugs,
//     //   );
//     let hidden_tests = {
//       let {tests, hints} = hidden_tests;
//       let tests = zipper_of_code(tests);
//       {tests, hints};
//     };
//     {
//       title,
//       description,
//     //   version,
//     //   module_name,
//     //   prompt,
//     //   point_distribution,
//     //   prelude,
//     //   correct_impl,
//     //   your_tests,
//       your_impl,
//     //   hidden_bugs,
//       hidden_tests,
//     //   syntax_tests,
//     };
//   };

let eds_of_spec =
    (
      {
        id,
        title,
        // description,
        version,
        module_name,
        prompt,
        // point_distribution,
        // prelude,
        // correct_impl,
        // your_tests,
        your_impl,
        display_hint,
        hidden_tests,
        wrapper,
        show_report,
        // syntax_tests,
      },
      ~settings as _: Language.CoreSettings.t,
    ) => {
  let editor_of_serialization = Editor.Model.mk;
  //   let prelude = editor_of_serialization(prelude);
  // let correct_impl = editor_of_serialization(correct_impl);
  //   let your_tests = {
  //     let tests = editor_of_serialization(your_tests.tests);
  //     {tests, required: your_tests.required, provided: your_tests.provided};
  //   };
  let your_impl = editor_of_serialization(your_impl);
  //   let hidden_bugs =
  //     hidden_bugs
  //     |> List.map(({impl, hint}) => {
  //          let impl = editor_of_serialization(impl);
  //          {impl, hint};
  //        });
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
    // description,
    version,
    module_name,
    prompt,
    // point_distribution,
    // prelude,
    // correct_impl,
    // your_tests,
    display_hint,
    your_impl,
    // hidden_bugs,
    hidden_tests,
    // syntax_tests,
    wrapper,
    show_report,
  };
};

//
// Old version of above that did string-based parsing, may be useful
// for transitions between zipper data structure versions (TODO)
//

let visible_in = (pos, ~instructor_mode) => {
  switch (pos) {
  //   | Prelude => instructor_mode
  // | CorrectImpl => instructor_mode
  //   | YourTestsValidation => true
  //   | YourTestsTesting => true
  | YourImpl => true
  //   | HiddenBugs(_) => instructor_mode
  | HiddenTests => instructor_mode
  };
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
  //   test_validation: 'a, // prelude + correct_impl + your_tests
  user_impl: 'a, // prelude + your_impl
  //   user_tests: 'a, // prelude + your_impl + your_tests
  //   prelude: 'a, // prelude
  // instructor: 'a, // prelude + correct_impl + hidden_tests.tests // TODO only needs to run in instructor mode
  //   hidden_bugs: list('a), // prelude + hidden_bugs[i].impl + your_tests,
  hidden_tests: 'a,
  // raw_result: 'a,
};

let map_stitched = (f: (pos, 'a) => 'b, s: stitched('a)): stitched('b) => {
  //   test_validation: f(YourTestsValidation, s.test_validation),
  user_impl: f(YourImpl, s.user_impl),
  //   user_tests: f(YourTestsTesting, s.user_tests),
  //   prelude: f(Prelude, s.prelude),
  // instructor: f(YourImpl, s.instructor),
  //   hidden_bugs: List.mapi((i, p) => f(HiddenBugs(i), p), s.hidden_bugs),
  hidden_tests: f(HiddenTests, s.hidden_tests),
  // raw_result: f(YourImpl, s.raw_result),
};

let get_stitched = (pos, s: stitched('a)): 'a =>
  switch (pos) {
  //   | YourTestsValidation => s.test_validation
  | YourImpl => s.user_impl
  //   | YourTestsTesting => s.user_tests
  //   | Prelude => s.prelude
  // | CorrectImpl => s.instructor
  //   | HiddenBugs(i) => List.nth(s.hidden_bugs, i)
  | HiddenTests => s.hidden_tests
  };

let map2_stitched =
    (f: (pos, 'a, 'b) => 'c, s1: stitched('a), s2: stitched('b))
    : stitched('c) =>
  map_stitched((pos, a) => f(pos, a, get_stitched(pos, s2)), s1);

let put_stitched = (pos, s: stitched('a), x: 'a): stitched('a) =>
  switch (pos) {
  //   | YourTestsValidation => {...s, test_validation: x}
  | YourImpl => {
      ...s,
      user_impl: x,
    }
  //   | YourTestsTesting => {...s, user_tests: x}
  //   | Prelude => {...s, prelude: x}
  // | CorrectImpl => {...s, instructor: x}
  //   | HiddenBugs(i) => {
  //       ...s,
  //       hidden_bugs: Util.ListUtil.put_nth(i, x, s.hidden_bugs),
  //     }
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
              Some(Some(Unknown(Internal) |> Language.Typ.fresh)),
            ),
          annotation: {
            // copied: false,
            ids: [Id.mk()],
          },
        },
      }),
      term,
    ),
  annotation: {
    // copied: false,
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
  | Constructor(_)
  | Closure(_)
  | Fun(_)
  | TypFun(_)
  | FixF(_)
  | Tuple(_)
  | TupLabel(_)
  | Label(_)
  | Dot(_)
  | Var(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Test(_)
  | Parens(_)
  | Probe(_)
  | Cons(_)
  | ListConcat(_)
  | LivelitName(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Asc(_)
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
  // Printf.printf("Wrapper: %b\n", eds.wrapper);
  // Printf.printf("Name: %s\n", eds.module_name);
  Printf.printf("Wrapper: %b\n", eds.wrapper);

  let user_impl_term = {
    eds.your_impl |> term_of;
                            // eds.your_impl |> term_of |> wrap_filter(FilterAction.Step);
  };

  // let user_impl_term =
  //   MakeTerm.from_zip_for_sem(eds.your_impl.state.zipper).term;
  // Printf.printf(
  //   "User Implementation Term: %s\n",
  //   TermBase.Exp.show(user_impl_term),
  // );
  let wrapped_user_impl =
    Let(
      Var("answer") |> Language.Pat.fresh,
      user_impl_term,
      EmptyHole |> Language.Exp.fresh,
    )
    |> Language.Exp.fresh;

  // let hidden_tests_term =
  //   eds.wrapper
  //     ? EditorUtil.append_exp(
  //         wrapped_user_impl,
  //         term_of(eds.hidden_tests.tests),
  //       )
  //     : term_of(eds.hidden_tests.tests);

  let hidden_tests_term =
    eds.wrapper
      ? append_exp(wrapped_user_impl, term_of(eds.hidden_tests.tests))
      : append_exp(user_impl_term, term_of(eds.hidden_tests.tests));

  {
    user_impl: wrap(user_impl_term, eds.your_impl),
    // instructor: wrap(instructor, eds.your_impl),
    hidden_tests: wrap(hidden_tests_term, eds.hidden_tests.tests),
    // raw_result: wrap((user_impl_term), eds.your_impl),
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
  //   | Prelude => prelude_key
  // | CorrectImpl => instructor_key
  //   | YourTestsValidation => test_validation_key
  //   | YourTestsTesting => user_tests_key
  | YourImpl => user_impl_key
  | HiddenTests => hidden_tests_key
  };

let pos_of_key = (key: string): pos =>
  switch () {
  | _ when key == user_impl_key => YourImpl
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

let blank_spec = (~title) => {
  let your_impl = Zipper.next_blank();
  let hidden_tests_tests = Zipper.next_blank();
  let wrapper = false;
  let show_report = true;
  {
    id: Id.mk(),
    title,
    display_hint: "",
    version: 1,
    module_name: "Blank",
    prompt: "",
    your_impl,
    hidden_tests: {
      tests: hidden_tests_tests,
      hints: [],
    },
    wrapper,
    show_report,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_tutorial_mode = list((pos, PersistentZipper.t));

// let persist = (state: state, ~instructor_mode: bool) => {
//   let zippers =
//     positioned_editors(state.eds)
//     |> List.filter(((pos, _)) => visible_in(pos, ~instructor_mode))
//     |> List.map(((pos, editor: Editor.t)) => {
//          (pos, PersistentZipper.persist(editor.state.zipper))
//        });
//   {
//     editors: zippers,
//     title: state.eds.title,
//     wrapper: state.eds.wrapper,
//     show_report: state.eds.show_report,
//     display_hint: state.eds.display_hint,
//   };
// };

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
  // let prelude = lookup(Prelude, spec.prelude);
  // let correct_impl = lookup(CorrectImpl, spec.correct_impl);
  // let your_tests_tests = lookup(YourTestsValidation, spec.your_tests.tests);
  let your_impl = lookup(YourImpl, spec.your_impl);
  let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);
  {
    id: spec.id,
    title: spec.title,
    version: spec.version,
    module_name: spec.module_name,
    prompt: spec.prompt,
    display_hint: spec.display_hint,
    wrapper: spec.wrapper,
    show_report: spec.show_report,
    // point_distribution: spec.point_distribution,
    // prelude,
    // correct_impl,
    // your_tests: {
    //   tests: your_tests_tests,
    //   required: spec.your_tests.required,
    //   provided: spec.your_tests.provided,
    // },
    your_impl,
    // hidden_bugs,
    hidden_tests: {
      tests: hidden_tests_tests,
      hints: spec.hidden_tests.hints,
    },
    // syntax_tests: spec.syntax_tests,
  };
} /* }*/;

// let unpersist =
//     (
//       {
//         wrapper,
//         show_report,
//         editors,
//         title,
//         display_hint,
//         // hidden_bugs,
//         // prompt,
//         // point_distribution,
//         // required,
//         // module_name,
//         // syntax_tests,
//       }: persistent_state,
//       ~spec: spec,
//       ~instructor_mode: bool,
//     )
//     : state => {
//   // Js.Console.log2("Wrapper:", spec.wrapper);
//   // Printf.printf("Wrapper: %b\n", spec.wrapper);
//   let lookup = (pos, default) =>
//     if (visible_in(pos, ~instructor_mode)) {
//       let persisted_zipper = List.assoc_opt(pos, editors);
//       let zipper = PersistentZipper.unpersist(persisted_zipper);
//       Editor.Model.mk(zipper);
//     } else {
//       Editor.Model.mk(default);
//     };
//   let your_impl = lookup(YourImpl, spec.your_impl);
//   let hidden_tests_tests = lookup(HiddenTests, spec.hidden_tests.tests);
//   {
//     // let

//     eds: {
//       id: spec.id,
//       title,
//       module_name: spec.module_name,
//       prompt: spec.prompt,
//       your_impl,
//       hidden_tests: {
//         tests: hidden_tests_tests,
//         hints: spec.hidden_tests.hints,
//       },
//       display_hint,
//       version: spec.version,
//       wrapper,
//       show_report,
//     },
//   };
