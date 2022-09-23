open Sexplib.Std;
open Haz3lcore;

module Common = {
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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | Prelude
    | CorrectImpl
    | YourTestsValidation
    | YourTestsTesting
    | YourImpl
    | HiddenBugs(int)
    | HiddenTests;
};

open Common;

[@deriving (show({with_path: false}), sexp, yojson)]
type p('code, 'node) = {
  next_id: Id.t,
  title: string,
  version: int,
  module_name: string,
  prompt: 'node,
  point_distribution,
  prelude: 'code,
  correct_impl: 'code,
  your_tests: your_tests('code),
  your_impl: 'code,
  hidden_bugs: list(wrong_impl('code)),
  hidden_tests: hidden_tests('code),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type spec('node) = p(Zipper.t, 'node);

[@deriving (show({with_path: false}), sexp, yojson)]
type eds('node) = p(Editor.t, 'node);

[@deriving (show({with_path: false}), sexp, yojson)]
type state('node) = {
  pos,
  eds: eds('node),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = (pos, Id.t, list((pos, Zipper.t)));
