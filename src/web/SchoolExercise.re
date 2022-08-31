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

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = (pos, list(Core.Zipper.t));

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

let put_editor = ((pos, ed): state, editor: Core.Editor.t) =>
  switch (pos) {
  | Prelude => (pos, {...ed, prelude: editor})
  | ReferenceImpl => (pos, {...ed, reference_impl: editor})
  | YourTests => (pos, {...ed, your_tests: editor})
  | YourImpl => (pos, {...ed, your_impl: editor})
  | HiddenBugs(n) => (
      pos,
      {
        ...ed,
        hidden_bugs:
          Util.ListUtil.put_nth(
            n,
            {...List.nth(ed.hidden_bugs, n), impl: editor},
            ed.hidden_bugs,
          ),
      },
    )
  | HiddenTests => (
      pos,
      {
        ...ed,
        hidden_tests: {
          ...ed.hidden_tests,
          tests: editor,
        },
      },
    )
  };

let editors = ((_, ed): state) =>
  [ed.prelude, ed.reference_impl, ed.your_tests, ed.your_impl]
  @ List.map(wrong_impl => wrong_impl.impl, ed.hidden_bugs)
  @ [ed.hidden_tests.tests];

let idx_of_pos = (pos, p: p('code)) =>
  switch (pos) {
  | Prelude => 0
  | ReferenceImpl => 1
  | YourTests => 2
  | YourImpl => 3
  | HiddenBugs(i) =>
    if (i < List.length(p.hidden_bugs)) {
      4 + i;
    } else {
      failwith("invalid hidden bug index");
    }
  | HiddenTests => 4 + List.length(p.hidden_bugs)
  };

let pos_of_idx = (p: p('code), idx: int) =>
  switch (idx) {
  | 0 => Prelude
  | 1 => ReferenceImpl
  | 2 => YourTests
  | 3 => YourImpl
  | _ =>
    if (idx < 0) {
      failwith("negative idx");
    } else if (idx < 4 + List.length(p.hidden_bugs)) {
      HiddenBugs(idx - 5);
    } else if (idx == 4 + List.length(p.hidden_bugs)) {
      HiddenTests;
    } else {
      failwith("element idx");
    }
  };

let ed_of_spec: spec => (Core.Id.t, ed) =
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
    let editor_of_code = (init_id, code) =>
      switch (EditorUtil.editor_of_code(init_id, code)) {
      | None => failwith("Exercise error: invalid code")
      | Some(x) => x
      };
    let id = 0;
    let (id, prelude) = editor_of_code(id, prelude);
    let (id, reference_impl) = editor_of_code(id, reference_impl);
    let (id, your_tests) = editor_of_code(id, your_tests);
    let (id, your_impl) = editor_of_code(id, your_impl);
    let (id, hidden_bugs) =
      List.fold_left(
        ((id, acc), {impl, hint}) => {
          let (id, impl) = editor_of_code(id, impl);
          (id, acc @ [{impl, hint}]);
        },
        (id, []),
        hidden_bugs,
      );
    let (id, hidden_tests) = {
      let {tests, hints} = hidden_tests;
      let (id, tests) = editor_of_code(id, tests);
      (id, {tests, hints});
    };
    (
      id,
      {
        prompt,
        prelude,
        reference_impl,
        your_tests,
        your_impl,
        hidden_bugs,
        hidden_tests,
      },
    );
  };

let state_of_spec: spec => (Core.Id.t, state) =
  spec => {
    let (id, ed) = ed_of_spec(spec);
    (id, (YourImpl, ed));
  };

let persistent_state_of_state: state => persistent_state =
  ((pos, _) as state) => {
    let editors = editors(state);
    let zippers =
      List.map((editor: Core.Editor.t) => editor.state.zipper, editors);
    (pos, zippers);
  };

let unpersist_state = ((pos, zippers): persistent_state, spec: spec): state => {
  let lookup = pos =>
    Core.Editor.init(List.nth(zippers, idx_of_pos(pos, spec)));
  (
    pos,
    {
      prompt: spec.prompt,
      prelude: lookup(Prelude),
      reference_impl: lookup(ReferenceImpl),
      your_tests: lookup(YourTests),
      your_impl: lookup(YourImpl),
      hidden_bugs:
        List.mapi(
          (i, {impl: _, hint}) => {{impl: lookup(HiddenBugs(i)), hint}},
          spec.hidden_bugs,
        ),
      hidden_tests: {
        tests: lookup(HiddenTests),
        hints: spec.hidden_tests.hints,
      },
    },
  );
};
