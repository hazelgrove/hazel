open Virtual_dom.Vdom;
open Sexplib.Std;

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
type p('code) = {
  prompt: [@opaque] Node.t,
  prelude: 'code,
  reference_impl: 'code,
  your_tests: 'code,
  your_impl: 'code,
  hidden_bugs: list(wrong_impl('code)),
  hidden_tests: hidden_tests('code),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Prelude
  | ReferenceImpl
  | YourTests
  | YourImpl
  | HiddenBugs(int)
  | HiddenTests;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = p(Core.CodeString.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type ed = p(Core.Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (pos, ed);

let editor_of_state: state => Core.Editor.t =
  ((pos, ed)) =>
    switch (pos) {
    | Prelude => ed.prelude
    | ReferenceImpl => ed.reference_impl
    | YourTests => ed.your_tests
    | YourImpl => ed.your_impl
    | HiddenBugs(i) => List.nth(ed.hidden_bugs, i).impl
    | HiddenTests => ed.hidden_tests.tests
    };

let editors = (ed: ed) =>
  [ed.prelude, ed.reference_impl, ed.your_tests, ed.your_impl]
  @ List.map(wrong_impl => wrong_impl.impl, ed.hidden_bugs)
  @ [ed.hidden_tests.tests];

let idx_of_pos = (ed: ed, pos: pos) =>
  switch (pos) {
  | Prelude => 0
  | ReferenceImpl => 1
  | YourTests => 2
  | YourImpl => 3
  | HiddenBugs(i) =>
    if (i < List.length(ed.hidden_bugs)) {
      4 + i;
    } else {
      failwith("invalid hidden bug index");
    }
  | HiddenTests => 4 + List.length(ed.hidden_bugs)
  };

let pos_of_idx = (ed: ed, idx: int) =>
  switch (idx) {
  | 0 => Prelude
  | 1 => ReferenceImpl
  | 2 => YourTests
  | 3 => YourImpl
  | _ =>
    if (idx < 0) {
      failwith("negative idx");
    } else if (idx < 4 + List.length(ed.hidden_bugs)) {
      HiddenBugs(idx - 5);
    } else if (idx == 4 + List.length(ed.hidden_bugs)) {
      HiddenTests;
    } else {
      failwith("element idx");
    }
  };

let spec_to_ed: spec => ed =
  (
    {
      prompt,
      prelude,
      reference_impl,
      your_tests,
      your_impl,
      hidden_bugs,
      hidden_tests,
    },
  ) => {
    let editor_of_code = code =>
      switch (EditorUtil.editor_of_code(code)) {
      | None => failwith("Exercise error: invalid code")
      | Some(ed) => ed
      };
    {
      prompt,
      prelude: editor_of_code(prelude),
      reference_impl: editor_of_code(reference_impl),
      your_tests: editor_of_code(your_tests),
      your_impl: editor_of_code(your_impl),
      hidden_bugs:
        List.map(
          ({impl, hint}) => {impl: editor_of_code(impl), hint},
          hidden_bugs,
        ),
      hidden_tests: {
        let {tests, hints} = hidden_tests;
        {tests: editor_of_code(tests), hints};
      },
    };
  };

let spec_to_state: spec => state = spec => (YourImpl, spec_to_ed(spec));
