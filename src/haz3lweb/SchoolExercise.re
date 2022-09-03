open Virtual_dom.Vdom;
open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type wrong_impl = {
  impl: CodeString.t,
  hint: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type hidden_tests = {
  tests: CodeString.t,
  hints: list(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = {
  prompt: [@opaque] Node.t,
  prelude: CodeString.t,
  reference_impl: CodeString.t,
  your_tests: CodeString.t,
  your_impl: CodeString.t,
  hidden_bugs: list(wrong_impl),
  hidden_tests,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {
  spec,
  editor: Editor.t,
};

let state_of_spec = spec => {
  let section = (title, code) => {
    "let hazel_section__"
    ++ title
    ++ " = 0 in \n"
    ++ code
    ++ (String.ends_with(~suffix="\n", code) ? "" : "\n");
  };
  let sections = [
    section("prelude", spec.prelude),
    section("your_impl", spec.your_impl),
    section("your_tests", spec.your_tests),
    section("hidden_tests", spec.hidden_tests.tests),
  ];
  let sections_code = String.concat("", sections);
  let (id, editor) =
    Option.get(EditorUtil.editor_of_code(0, sections_code));
  (id, {spec, editor});
};
