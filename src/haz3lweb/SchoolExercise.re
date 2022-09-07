open Virtual_dom.Vdom;
open Sexplib.Std;
open Haz3lcore;

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
type spec = p(CodeString.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type ed = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (pos, ed);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = (pos, list(Zipper.t));

let editor_of_state: state => Editor.t =
  ((pos, ed)) =>
    switch (pos) {
    | Prelude => ed.prelude
    | ReferenceImpl => ed.reference_impl
    | YourTests => ed.your_tests
    | YourImpl => ed.your_impl
    | HiddenBugs(i) => List.nth(ed.hidden_bugs, i).impl
    | HiddenTests => ed.hidden_tests.tests
    };

let put_editor = ((pos, ed): state, editor: Editor.t) =>
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
      HiddenBugs(idx - 4);
    } else if (idx == 4 + List.length(p.hidden_bugs)) {
      HiddenTests;
    } else {
      failwith("element idx");
    }
  };

let switch_editor = (idx: int, (_, ed)) => (pos_of_idx(ed, idx), ed);

let ed_of_spec: spec => (Id.t, ed) =
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

let state_of_spec: spec => (Id.t, state) =
  spec => {
    let (id, ed) = ed_of_spec(spec);
    (id, (YourImpl, ed));
  };

let persistent_state_of_state: state => persistent_state =
  ((pos, _) as state) => {
    let editors = editors(state);
    let zippers =
      List.map((editor: Editor.t) => editor.state.zipper, editors);
    (pos, zippers);
  };

let unpersist_state = ((pos, zippers): persistent_state, spec: spec): state => {
  let lookup = pos => Editor.init(List.nth(zippers, idx_of_pos(pos, spec)));
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

// # Stitching

module StaticsItem = {
  type t = {
    term: TermBase.UExp.t,
    info_map: Statics.map,
  };
};

type stitched('a) = {
  user_impl: 'a, // prelude + your_impl
  user_tests: 'a, // prelude + your_impl + your_tests
  instructor: 'a, // prelude + reference_impl + hidden_tests.tests
  hidden_bugs: list('a) // prelude + hidden_bugs[i].impl
};

type stitched_statics = stitched(StaticsItem.t);

let stitch_static = ((_, ed): state): stitched_statics => {
  let (user_impl_term, _) = EditorUtil.stitch([ed.prelude, ed.your_impl]);
  let (user_tests_term, _) =
    EditorUtil.stitch([ed.prelude, ed.your_impl, ed.your_tests]);
  let user_info_map = Statics.mk_map(user_tests_term);
  let user_impl = StaticsItem.{term: user_impl_term, info_map: user_info_map};
  let user_tests =
    StaticsItem.{term: user_tests_term, info_map: user_info_map};

  let (instructor_term, _) =
    EditorUtil.stitch([ed.prelude, ed.reference_impl, ed.hidden_tests.tests]);
  let instructor_info_map = Statics.mk_map(instructor_term);
  let instructor =
    StaticsItem.{term: instructor_term, info_map: instructor_info_map};

  let hidden_bugs =
    List.map(
      ({impl, hint}) => {
        let (term, _) = EditorUtil.stitch([ed.prelude, impl]);
        let info_map = Statics.mk_map(term);
        StaticsItem.{term, info_map};
      },
      ed.hidden_bugs,
    );
  {user_impl, user_tests, instructor, hidden_bugs};
};

let user_impl_key = "user_impl";
let user_tests_key = "user_tests";
let instructor_key = "instructor";

let spliced_elabs: state => list((ModelResults.key, DHExp.t)) =
  state => {
    let {user_impl, user_tests, instructor, hidden_bugs: _} =
      stitch_static(state);
    [
      (
        user_impl_key,
        Interface.elaborate(user_impl.info_map, user_impl.term),
      ),
      (
        user_tests_key,
        Interface.elaborate(user_tests.info_map, user_tests.term),
      ),
      (
        instructor_key,
        Interface.elaborate(instructor.info_map, instructor.term),
      ),
    ];
  };

module DynamicsItem = {
  type t = {
    term: TermBase.UExp.t,
    info_map: Statics.map,
    simple_result: option(ModelResult.simple),
  };
};
let stitch_dynamic = (state: state, results: option(ModelResults.t)) => {
  let StaticsItem.{user_impl, user_tests, instructor, hidden_bugs} =
    stitch_static(state);
  let simple_result_of = key =>
    Option.map(
      results => ModelResult.get_simple(ModelResults.lookup(results, key)),
      results,
    );
  let user_impl =
    DynamicsItem.{
      term: user_impl.term,
      info_map: user_impl.info_map,
      simple_result: simple_result_of(user_impl_key),
    };
  let user_tests =
    DynamicsItem.{
      term: user_tests.term,
      info_map: user_tests.info_map,
      simple_result: simple_result_of(user_tests_key),
    };
  let instructor =
    DynamicsItem.{
      term: instructor.term,
      info_map: instructor.info_map,
      simple_result: simple_result_of(instructor_key),
    };
  let hidden_bugs =
    List.map(
      (statics_item: StaticsItem.t) =>
        DynamicsItem.{
          term: statics_item.term,
          info_map: statics_item.info_map,
          simple_result: None,
        },
      hidden_bugs,
    );
  {user_impl, user_tests, instructor, hidden_bugs};
};
