open Virtual_dom.Vdom;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type wrong_impl('code) = {
  impl: 'code,
  hint: string
};

[@deriving (show({with_path: false}), sexp, yojson)]
type hidden_tests('code) = {
  tests: 'code,
  hint: string
};

[@deriving (show({with_path: false}), sexp, yojson)]
type p('code) = {
  prompt: [@opaque] Node.t,
  prelude: 'code,
  reference_impl: 'code,
  your_tests: 'code,
  your_impl: 'code,
  hidden_bugs: list(wrong_impl('code)),
  hidden_tests: hidden_tests('code)
};

type exercise_spec = p(CodeString.t);
type exercise_state = p(Core.Editor.t);

let exercise_spec_to_state : exercise_spec => exercise_state = ({
  prompt, prelude, reference_impl, your_tests, your_impl, hidden_bugs, hidden_tests
}) => {
  prompt: prompt,
  prelude: editor_from_code(prelude),
  reference_impl: editor_from_code(reference_impl),
  your_tests: editor_from_code(your_tests),
  your_impl: editor_from_code(your_impl),
  hidden_bugs: hidden_bugs_spec_to_state(hidden_bugs),
  hidden_tests: hidden_tests_spec_to_state(hidden_tests)
}

let the_exercise: exercise = FilterOddsExercise.exercise;

let init: Model.school = Model.editors_for(the_exercise, SchoolCell.code_of);
