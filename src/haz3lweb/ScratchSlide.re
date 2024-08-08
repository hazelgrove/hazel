open Sexplib.Std;
open Haz3lcore;
// open SyntaxTest;

[@deriving (show({with_path: false}), sexp, yojson)]
type hidden_tests('code) = {
  tests: 'code,
  hints: list(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type p('code) = {
  title: string,
  description: string,
  hidden_tests: hidden_tests('code),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type state = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = p(PersistentZipper.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type hint = string;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type syntax_test = (hint, SyntaxTest.predicate);

// [@deriving (show({with_path: false}), sexp, yojson)]
// type syntax_tests = list(syntax_test);

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

// why are neither of these functions working?
let toEditor = (state: state): Editor.t => {
  switch (state) {
  | s => s.hidden_tests.tests
  };
};

let fromEditor = (editor: Editor.t): state => {
  title: "",
  description: "",
  hidden_tests: {
    tests: editor,
    hints: [],
  },
};

let scratch_key = n => "scratch_" ++ n;

let persist = (editor: Editor.t) => {
  PersistentZipper.persist(editor.state.zipper);
};

// let persist = (editor: p(Editor.t)) => {
//   let zip = editor.hidden_tests.tests.state.zipper;
//   PersistentZipper.persist(zip);
// };

let unpersist = (zipper: persistent_state) => {
  let zipper = PersistentZipper.unpersist(zipper.hidden_tests.tests);
  Editor.init(zipper, ~read_only=false);
};

let serialize = (state: state) => {
  let editor = persist(state.hidden_tests.tests);
  let persistent_state: persistent_state = {
    title: state.title,
    description: state.description,
    hidden_tests: {
      tests: editor,
      hints: state.hidden_tests.hints,
    },
  };
  // Sexplib.Sexp.to_string (sexp_of_persistent_state persistent_state)
  persistent_state |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
  // Persist(editor) |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string) => {
  Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp |> unpersist;
};

let deserialize_opt = (data: string) => {
  let sexp =
    try(Some(Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp)) {
    | _ => None
    };
  sexp |> Option.map(sexp => sexp |> unpersist);
};

let export = (state: state) => {
  let editor = persist(state.hidden_tests.tests);
  let persistent_state: persistent_state = {
    title: state.title,
    description: state.description,
    hidden_tests: {
      tests: editor,
      hints: state.hidden_tests.hints,
    },
  };
  persistent_state |> yojson_of_persistent_state;
};

// let export = (state: persistent_state) => {
//   state |> yojson_of_persistent_state;
// };

let import = (data: string) => {
  data |> Yojson.Safe.from_string |> persistent_state_of_yojson |> unpersist;
};

let export_init = (state: state) => {
  let editor = persist(state.hidden_tests.tests);
  let persistent_state: persistent_state = {
    title: state.title,
    description: state.description,
    hidden_tests: {
      tests: editor,
      hints: state.hidden_tests.hints,
    },
  };
  persistent_state |> show_persistent_state;
};

// let export_init = (state: persistent_state) => {
//   state |> show_persistent_state;
// };

let mk_statics =
    (~settings: Settings.t, editor: Editor.t, ctx_init: Ctx.t)
    : CachedStatics.statics => {
  let term = MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst;
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let error_ids =
    Statics.Map.error_ids(editor.state.meta.term_ranges, info_map);
  {term, info_map, error_ids};
};

//// CREATING MODULE

// module type ScratchSlideEnv = {
//   type node;
//   let default: node;
//   let output_header: string => string;
// };

// let output_header_grading = _module_name =>
//   "module Documentation = GradePrelude.Documentation\n" ++ "let prompt = ()\n";

// module S = (ScratchSlideEnv: ScratchSlideEnv) => {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type hidden_tests('code) = {
//     tests: 'code,
//     hints: list(string),
//   };

// [@deriving (show({with_path: false}), sexp, yojson)]
//   type p('code) = {
//     title: string,
//     description: string,
//     hidden_tests: hidden_tests('code),
//   };

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type state = p(Editor.t);

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type persistent_state = p(PersistentZipper.t);

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type hint = string;

//   let toEditor = (state: state): Editor.t => {
//    switch (state) {
//     | s => s.hidden_tests.tests
//     };
//   };

//   let fromEditor = (editor: Editor.t): state => {
//     title: "",
//     description: "",
//     hidden_tests: {
//       tests: editor,
//       hints: [],
//     },
//   };

// let scratch_key = n => "scratch_" ++ n;

// let persist = (editor: Editor.t) => {
//   PersistentZipper.persist(editor.state.zipper);
// };

// // let persist = (editor: p(Editor.t)) => {
// //   let zip = editor.hidden_tests.tests.state.zipper;
// //   PersistentZipper.persist(zip);
// // };

// let unpersist = (zipper: persistent_state) => {
//   let zipper = PersistentZipper.unpersist(zipper.hidden_tests.tests);
//   Editor.init(zipper, ~read_only=false);
// };

// let serialize = (state: state) => {
//   let editor = persist(state.hidden_tests.tests);
//   let persistent_state: persistent_state = {
//     title: state.title,
//     description: state.description,
//     hidden_tests: {
//       tests: editor,
//       hints: state.hidden_tests.hints,
//     },
//   };
//   // Sexplib.Sexp.to_string (sexp_of_persistent_state persistent_state)
//   persistent_state |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
//   // Persist(editor) |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
// };

// let deserialize = (data: string) => {
//   Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp |> unpersist;
// };

// let deserialize_opt = (data: string) => {
//   let sexp =
//     try(Some(Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp)) {
//     | _ => None
//     };
//   sexp |> Option.map(sexp => sexp |> unpersist);
// };

// let export = (state: state) => {
//   let editor = persist(state.hidden_tests.tests);
//   let persistent_state: persistent_state = {
//     title: state.title,
//     description: state.description,
//     hidden_tests: {
//       tests: editor,
//       hints: state.hidden_tests.hints,
//     },
//   };
//   persistent_state |> yojson_of_persistent_state;
// };

// let import = (data: string) => {
//   data |> Yojson.Safe.from_string |> persistent_state_of_yojson |> unpersist;
// };

// let export_init = (state: state) => {
//   let editor = persist(state.hidden_tests.tests);
//   let persistent_state: persistent_state = {
//     title: state.title,
//     description: state.description,
//     hidden_tests: {
//       tests: editor,
//       hints: state.hidden_tests.hints,
//     },
//   };
//   persistent_state |> show_persistent_state;
// };

// let mk_statics =
//     (~settings: Settings.t, editor: Editor.t, ctx_init: Ctx.t)
//     : CachedStatics.statics => {
//   let term = MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst;
//   let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
//   let error_ids =
//     Statics.Map.error_ids(editor.state.meta.term_ranges, info_map);
//   {term, info_map, error_ids};
// };

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type key = (string, int);

//   let key_of = p => {
//     (p.title);
//   };

//   let find_key_opt = (key, specs: list(p('code))) => {
//     specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);
//   };

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type pos =
//     // | YourImpl
//     | HiddenTests;

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type slide_state = {
//     pos : pos,
//     eds : state,
//   };

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type spec = p(Zipper.t);

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type transitionary_spec = p(CodeString.t);

//   let map = (p: p('a), _: 'a => 'b): p('b) => {
//     {
//       title: p.title,
//       description: p.description,
//       // point_distribution: p.point_distribution,
//       // your_tests: {
//       //   tests: f(p.your_tests.tests),
//       //   required: p.your_tests.required,
//       //   provided: p.your_tests.provided,
//       // },
//       // your_impl: f(p.your_impl),
//       hidden_tests: {
//         tests: PersistentZipper.persist(p.hidden_tests.tests),
//         hints: p.hidden_tests.hints,
//       },
//       // syntax_tests: p.syntax_tests,
//     };
//   };

//   // [@deriving (show({with_path: false}), sexp, yojson)]
//   // type eds = p(Editor.t);

//   // let key_of_state = ({eds, _}) => key_of(eds);

//   // [@deriving (show({with_path: false}), sexp, yojson)]
//   // type persistent_state = (pos, list((pos, PersistentZipper.t)));

//   let editor_of_state = (state : state) : Editor.t => {
//     state.hidden_tests.tests
//       // switch (pos) {
//       // | YourImpl => eds.your_impl
//       // | HiddenTests => eds.hidden_tests.tests
//       };

//   let put_editor = ({pos, eds} as state: slide_state, editor: Editor.t) =>
//     switch (pos) {
//     // | YourImpl => {
//     //     ...state,
//     //     eds: {
//     //       ...eds,
//     //       your_impl: editor,
//     //     },
//     //   }
//     | HiddenTests => {
//         ...state,
//         eds: {
//           ...eds,
//           hidden_tests: {
//             ...eds.hidden_tests,
//             tests: editor,
//           },
//         },
//       }
//     };

//   let editors = ({eds, _}: slide_state) =>
//     // [
//     //   eds.your_impl,
//     // ]
//     // @ List.map(wrong_impl => wrong_impl.impl, eds.hidden_bugs)
//     // @
//     [eds.hidden_tests.tests];

//   let editor_positions =
//   // ({eds, _}: slide_state) =>
//     // [YourImpl]
//     // @ List.mapi((i, _) => HiddenBugs(i), eds.hidden_bugs)
//     // @
//     [HiddenTests];

//   let positioned_editors = state =>
//     List.combine(editor_positions, editors(state));

//   let idx_of_pos = (pos, p: p('code)) =>
//     switch (pos) {
//     // | YourImpl => 4
//     | HiddenTests => 0
//     };

//   let pos_of_idx = (idx: int) =>
//     switch (idx) {
//     // | 4 => YourImpl
//     | _ =>
//       if (idx < 0) {
//         failwith("negative idx");
//       } else if (idx == 5) {
//         HiddenTests;
//       } else {
//         failwith("element idx");
//       }
//     };

//   let switch_editor = (~pos, instructor_mode, ~exercise) =>
//     if (!instructor_mode) {
//       switch (pos) {
//       | HiddenTests
//       // | HiddenBugs(_) => exercise
//       | _ => {eds: exercise.eds, pos}
//       };
//     } else {
//       {eds: exercise.eds, pos};
//     };

//   let zipper_of_code = code => {
//     switch (Printer.zipper_of_string(code)) {
//     | None => failwith("Transition failed.")
//     | Some(zipper) => zipper
//     };
//   };

//   let transition: transitionary_spec => spec =
//     (
//       {
//         title,
//         description,
//         // your_impl,
//         hidden_tests,
//       },
//     ) => {
//       // let your_impl = zipper_of_code(your_impl);
//       // let hidden_bugs =
//       //   List.fold_left(
//       //     (acc, {impl, hint}) => {
//       //       let impl = zipper_of_code(impl);
//       //       acc @ [{impl, hint}];
//       //     },
//       //     [],
//       //     hidden_bugs,
//       //   );
//       let hidden_tests = {
//         let {tests, hints} = hidden_tests;
//         let tests = zipper_of_code(tests);
//         {tests, hints};
//       };
//       {
//         title,
//         description,
//         // your_impl,
//         hidden_tests,
//       };
//     };

//   type eds = {eds : state};
//   let editor_of_serialization = zipper => Editor.init(zipper);
//   let eds_of_spec: spec => eds =
//     (
//       {
//         title,
//         description,
//         // your_impl,
//         hidden_tests,
//       },
//     ) => {
//       // let your_impl = editor_of_serialization(your_impl);
//       let hidden_tests = {
//         let {tests, hints} = hidden_tests;
//         let tests = editor_of_serialization(tests);
//         {tests, hints};
//       };
//       {
//         title,
//         description,
//         hidden_tests,
//       };
//     };

//   let set_instructor_mode = ({eds, _} as state: slide_state, new_mode: bool) => {
//     ...state,
//     eds: {
//       ...eds,
//       // hidden_tests: _,
//     },
//   };

//   let visible_in = (pos, ~instructor_mode) => {
//     switch (pos) {
//     // | YourImpl => true
//     | HiddenTests => instructor_mode
//     };
//   };

//   let state_of_spec = (spec, _: bool): state => {
//     let eds = eds_of_spec(spec);
//     // set_instructor_mode({pos: YourImpl, eds}, instructor_mode);
//   };
