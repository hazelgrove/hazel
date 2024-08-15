open Util;

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
type model('code) = {
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
type pos =
  | Prelude
  | CorrectImpl
  | YourTests(your_tests_form)
  | YourImpl
  | HiddenBugs(int)
  | HiddenTests
and your_tests_form =
  | Testing
  | Validation;

module ModelUtil = {
  let map = (f: 'a => 'b, m: model('a)): model('b) => {
    {
      ...m,
      prelude: m.prelude |> f,
      correct_impl: m.correct_impl |> f,
      your_tests: {
        ...m.your_tests,
        tests: m.your_tests.tests |> f,
      },
      your_impl: m.your_impl |> f,
      hidden_bugs:
        m.hidden_bugs |> List.map(({impl, hint}) => {impl: f(impl), hint}),
      hidden_tests: {
        ...m.hidden_tests,
        tests: m.hidden_tests.tests |> f,
      },
    };
  };

  let mapi = (f: (pos, 'a) => 'b, m: model('a)): model('b) => {
    {
      ...m,
      prelude: m.prelude |> f(Prelude),
      correct_impl: m.correct_impl |> f(CorrectImpl),
      your_tests: {
        ...m.your_tests,
        tests: m.your_tests.tests |> f(YourTests(Testing)),
      },
      your_impl: m.your_impl |> f(YourImpl),
      hidden_bugs:
        m.hidden_bugs
        |> List.mapi((i, {impl, hint}) =>
             {impl: impl |> f(HiddenBugs(i)), hint}
           ),
      hidden_tests: {
        ...m.hidden_tests,
        tests: m.hidden_tests.tests |> f(HiddenTests),
      },
    };
  };

  let nth = (m: model('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | CorrectImpl => m.correct_impl
    | YourTests(_) => m.your_tests.tests
    | YourImpl => m.your_impl
    | HiddenBugs(i) => List.nth(m.hidden_bugs, i).impl
    | HiddenTests => m.hidden_tests.tests
    };

  let map_nth = (f: 'a => 'b, m: model('a), pos: pos): model('b) =>
    switch (pos) {
    | Prelude => {...m, prelude: m.prelude |> f}
    | CorrectImpl => {...m, correct_impl: m.correct_impl |> f}
    | YourTests(_) => {
        ...m,
        your_tests: {
          ...m.your_tests,
          tests: m.your_tests.tests |> f,
        },
      }
    | YourImpl => {...m, your_impl: m.your_impl |> f}
    | HiddenBugs(i) => {
        ...m,
        hidden_bugs:
          Util.ListUtil.map_nth(
            i,
            ({impl, hint}) => {impl: impl |> f, hint},
            m.hidden_bugs,
          ),
      }
    | HiddenTests => {
        ...m,
        hidden_tests: {
          ...m.hidden_tests,
          tests: m.hidden_tests.tests |> f,
        },
      }
    };

  let flatten = (p: model('a)): list('a) =>
    [p.prelude, p.correct_impl, p.your_tests.tests, p.your_impl]
    @ (p.hidden_bugs |> List.map(({impl, _}) => impl))
    @ [p.hidden_tests.tests];

  // Config Functions/Parameters

  let switch_editor = (pos: pos, instructor_mode: bool): bool =>
    switch (pos) {
    | HiddenTests
    | HiddenBugs(_) when !instructor_mode => false
    | _ => true
    };

  let readonly_in = (pos: pos, instructor_mode: bool): bool =>
    switch (pos) {
    | Prelude => !instructor_mode
    | _ => false
    };

  let visible_in = (pos: pos, instructor_mode: bool): bool =>
    switch (pos) {
    | Prelude => instructor_mode
    | CorrectImpl => instructor_mode
    | YourTests(Validation) => true
    | YourTests(Testing) => false
    | YourImpl => true
    | HiddenBugs(_) => instructor_mode
    | HiddenTests => instructor_mode
    };

  let init = (init: pos => 'a): model('a) => {
    point_distribution: {
      test_validation: 0,
      mutation_testing: 0,
      impl_grading: 0,
    },
    prelude: Prelude |> init,
    correct_impl: CorrectImpl |> init,
    your_tests: {
      tests: YourTests(Testing) |> init,
      required: 0,
      provided: 0,
    },
    your_impl: YourImpl |> init,
    hidden_bugs: [],
    hidden_tests: {
      tests: HiddenTests |> init,
      hints: [],
    },
    syntax_tests: [],
  };

  let fill = (p: model('a), init: pos => 'b): model('b) => {
    point_distribution: p.point_distribution,
    prelude: Prelude |> init,
    correct_impl: CorrectImpl |> init,
    your_tests: {
      tests: YourTests(Testing) |> init,
      required: p.your_tests.required,
      provided: p.your_tests.provided,
    },
    your_impl: YourImpl |> init,
    hidden_bugs:
      List.map(
        ({impl, hint}) => {impl: impl |> init, hint},
        p.hidden_bugs,
      ),
    hidden_tests: {
      tests: HiddenTests |> init,
      hints: p.hidden_tests.hints,
    },
    syntax_tests: p.syntax_tests,
  };
};

/* Multiple stitchings are needed for each exercise
   (see comments in the stitched type above)

   Stitching is necessary to concatenate terms
   from different editors, which are then typechecked. */
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

module StitchUtil = {
  let stitch = (stitch2: ('a, 'a) => 'a, model: model('a)): stitched('a) => {
    let stitch3 = (t1, t2, t3) => stitch2(stitch2(t1, t2), t3);
    let instructor_term =
      stitch3(model.prelude, model.correct_impl, model.hidden_tests.tests);
    let your_impl_term = model.your_impl; //|> wrap_filter(FilterAction.Step);
    let prelude_term = model.prelude; // |> wrap_filter(FilterAction.Eval);
    let user_impl_term = stitch2(prelude_term, your_impl_term);
    {
      test_validation:
        stitch3(model.prelude, model.correct_impl, model.your_tests.tests),
      user_impl: user_impl_term,
      user_tests: stitch2(model.prelude, model.your_tests.tests),
      prelude: instructor_term,
      instructor: instructor_term,
      hidden_bugs:
        List.map(
          (t: wrong_impl('a)) =>
            stitch3(model.prelude, t.impl, model.your_tests.tests),
          model.hidden_bugs,
        ),
      hidden_tests: stitch2(model.prelude, model.hidden_tests.tests),
    };
  };

  let key = (pos: pos): string =>
    switch (pos) {
    | YourTests(Validation) => "test_validation"
    | YourImpl => "user_impl"
    | YourTests(Testing) => "user_tests"
    | Prelude => "prelude"
    | CorrectImpl => "instructor"
    | HiddenBugs(i) => "hidden_bugs_" ++ string_of_int(i)
    | HiddenTests => "hidden_tests"
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

  let map = (f: 'a => 'b, m: stitched('a)): stitched('b) => {
    {
      test_validation: m.test_validation |> f,
      user_impl: m.user_impl |> f,
      user_tests: m.user_tests |> f,
      prelude: m.prelude |> f,
      instructor: m.instructor |> f,
      hidden_bugs: m.hidden_bugs |> List.map(f),
      hidden_tests: m.hidden_tests |> f,
    };
  };

  let mapi = (f: (pos, 'a) => 'b, m: stitched('a)): stitched('b) => {
    {
      test_validation: m.test_validation |> f(YourTests(Validation)),
      user_impl: m.user_impl |> f(YourImpl),
      user_tests: m.user_tests |> f(YourTests(Testing)),
      prelude: m.prelude |> f(Prelude),
      instructor: m.instructor |> f(CorrectImpl),
      hidden_bugs:
        m.hidden_bugs |> List.mapi((i, x) => f(HiddenBugs(i), x)),
      hidden_tests: m.hidden_tests |> f(HiddenTests),
    };
  };

  let nth = (m: stitched('a), pos: pos): 'a =>
    switch (pos) {
    | YourTests(Validation) => m.test_validation
    | YourImpl => m.user_impl
    | YourTests(Testing) => m.user_tests
    | Prelude => m.prelude
    | CorrectImpl => m.instructor
    | HiddenBugs(i) => List.nth(m.hidden_bugs, i)
    | HiddenTests => m.hidden_tests
    };

  let map_nth = (f: 'a => 'b, m: stitched('a), pos: pos): stitched('b) =>
    switch (pos) {
    | YourTests(Validation) => {
        ...m,
        test_validation: m.test_validation |> f,
      }
    | YourImpl => {...m, user_impl: m.user_impl |> f}
    | YourTests(Testing) => {...m, user_tests: m.user_tests |> f}
    | Prelude => {...m, prelude: m.prelude |> f}
    | CorrectImpl => {...m, instructor: m.instructor |> f}
    | HiddenBugs(i) => {
        ...m,
        hidden_bugs: Util.ListUtil.map_nth(i, f, m.hidden_bugs),
      }
    | HiddenTests => {...m, hidden_tests: m.hidden_tests |> f}
    };

  let flatten = (m: stitched('a)): list('a) =>
    [m.test_validation, m.user_impl, m.user_tests, m.prelude, m.instructor]
    @ (m.hidden_bugs |> List.map(Fun.id))
    @ [m.hidden_tests];

  let init = (init: pos => 'a): stitched('a) => {
    test_validation: YourTests(Validation) |> init,
    user_impl: YourImpl |> init,
    user_tests: YourTests(Testing) |> init,
    prelude: Prelude |> init,
    instructor: CorrectImpl |> init,
    hidden_bugs: [],
    hidden_tests: HiddenTests |> init,
  };

  let fill = (p: model('a), init: pos => 'b): stitched('b) => {
    test_validation: YourTests(Validation) |> init,
    user_impl: YourImpl |> init,
    user_tests: YourTests(Testing) |> init,
    prelude: Prelude |> init,
    instructor: CorrectImpl |> init,
    hidden_bugs: List.mapi((i, _) => HiddenBugs(i) |> init, p.hidden_bugs),
    hidden_tests: HiddenTests |> init,
  };
};
