open Virtual_dom.Vdom;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type wrong_impl('code) = {
  impl: 'code,
  hint: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type p('code) =
  | Prompt([@opaque] Node.t)
  | Prelude('code)
  | ReferenceImpl('code)
  | YourTests('code)
  | YourImpl('code)
  | HiddenBug(wrong_impl('code))
  | HiddenTests({
      tests: 'code,
      descriptions: list(string),
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = p(CodeString.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = p(Core.Editor.t);
let codestring_of_wrong_impls: list(wrong_impl(CodeString.t)) => CodeString.t =
  wrong_impls =>
    List.fold_left(
      (acc, wrong_impl) => {acc ++ "\n" ++ wrong_impl.impl},
      "",
      wrong_impls,
    );

let codestring_of: spec => option(string) =
  x =>
    switch (x) {
    | Prompt(_) => None
    | Prelude(code) => Some(code)
    | YourImpl(code) => Some(code)
    | YourTests(code) => Some(code)
    | ReferenceImpl(code) => Some(code)
    | HiddenBugs(wrong_impls) =>
      Some(codestring_of_wrong_impls(wrong_impls))
    | HiddenTests({tests, _}) => Some(tests)
    };