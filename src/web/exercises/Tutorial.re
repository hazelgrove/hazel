open Util_web;
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

[@deriving (show({with_path: false}), sexp, yojson)]
type p('code) = {
  id: Id.t,
  title: string,
  version: int,
  module_name: string,
  prompt: string,
  display_hint: string,
  your_impl: 'code,
  hidden_tests: hidden_tests('code),
  wrapper: bool,
  show_report: bool,
};

let id_of = p => {
  p.id;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | YourImpl
  | HiddenTests;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = p(Zipper.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type transitionary_spec = p(string);

let map = (p: p('a), f: 'a => 'b, f_hidden: 'a => 'b): p('b) => {
  {
    id: p.id,
    title: p.title,
    version: p.version,
    module_name: p.module_name,
    prompt: p.prompt,
    display_hint: p.display_hint,
    your_impl: f(p.your_impl),
    hidden_tests: {
      tests: f_hidden(p.hidden_tests.tests),
      hints: p.hidden_tests.hints,
    },
    wrapper: p.wrapper,
    show_report: p.show_report,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type eds = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {eds};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = list((pos, PersistentZipper.t));

let main_editor_of_state = (~selection: pos, eds) =>
  switch (selection) {
  | YourImpl => eds.your_impl
  | HiddenTests => eds.hidden_tests.tests
  };

let put_main_editor = (~selection: pos, eds: p('a), editor: 'a): p('a) =>
  switch (selection) {
  | YourImpl => {
      ...eds,
      your_impl: editor,
    }
  | HiddenTests => {
      ...eds,
      hidden_tests: {
        ...eds.hidden_tests,
        tests: editor,
      },
    }
  };

let editors = eds => [eds.your_impl, eds.hidden_tests.tests];

let editor_positions = [YourImpl, HiddenTests];
[YourImpl, HiddenTests];

let positioned_editors = state =>
  List.combine(editor_positions, editors(state));

let is_editable = (pos, ~instructor_mode) => {
  switch (pos) {
  | YourImpl => true
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
  user_impl: 'a, // prelude + your_impl
  hidden_tests: 'a,
};

let map_stitched = (f: (pos, 'a) => 'b, s: stitched('a)): stitched('b) => {
  user_impl: f(YourImpl, s.user_impl),
  hidden_tests: f(HiddenTests, s.hidden_tests),
};

let get_stitched = (pos, s: stitched('a)): 'a =>
  switch (pos) {
  | YourImpl => s.user_impl
  | HiddenTests => s.hidden_tests
  };

let map2_stitched =
    (f: (pos, 'a, 'b) => 'c, s1: stitched('a), s2: stitched('b))
    : stitched('c) =>
  map_stitched((pos, a) => f(pos, a, get_stitched(pos, s2)), s1);

let put_stitched = (pos, s: stitched('a), x: 'a): stitched('a) =>
  switch (pos) {
  | YourImpl => {
      ...s,
      user_impl: x,
    }
  | HiddenTests => {
      ...s,
      hidden_tests: x,
    }
  };

let wrap = (term, editor: Editor.t): TermItem.t => {
  term,
  editor,
};

let term_of = (editor: Editor.t): Language.Exp.t =>
  MakeTerm.from_zip_for_sem(editor.state.zipper, ~root=editor.root).term;

let rec append_exp = (e1: Language.Exp.t, e2: Language.Exp.t): Language.Exp.t => {
  switch (e1.term) {
  | EmptyHole
  | Invalid(_)
  | MultiHole(_)
  | DynamicErrorHole(_)
  | Undefined
  | Deferral(_)
  | Atom(_)
  | DrvQuote(_)
  | ListLit(_)
  | Constructor(_)
  | Closure(_)
  | Fun(_)
  | TypFun(_)
  | FixF(_)
  | Tuple(_)
  | TupLabel(_)
  | TupleExtension(_)
  | Label(_)
  | ExplicitNonlabel
  | Dot(_)
  | Var(_)
  | Ap(_)
  | TypAp(_)
  | DeferredAp(_)
  | If(_)
  | Test(_)
  | HintedTest(_)
  | Parens(_)
  | Projector(_)
  | Cons(_)
  | ListConcat(_)
  | LivelitName(_)
  | UnOp(_)
  | BinOp(_)
  | BuiltinFun(_)
  | Module(_)
  | ModuleExp(_)
  | Asc(_)
  | ProofObject(_)
  | Forall(_)
  | Match(_) => {
      term: Seq(e1, e2),
      annotation: Language.IdTagged.IdTag.fresh(),
    }
  | Seq(e11, e12) =>
    let e12' = append_exp(e12, e2);
    {
      term: Seq(e11, e12'),
      annotation:
        Language.IdTagged.IdTag.mk_internal(Language.IdTagged.ids(e1)),
    };
  | Filter(kind, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Filter(kind, ebody'),
      annotation:
        Language.IdTagged.IdTag.mk_internal(Language.IdTagged.ids(e1)),
    };
  | Let(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Let(p, edef, ebody'),
      annotation:
        Language.IdTagged.IdTag.mk_internal(Language.IdTagged.ids(e1)),
    };
  | Theorem(p, edef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Theorem(p, edef, ebody'),
      annotation:
        Language.IdTagged.IdTag.mk_internal(Language.IdTagged.ids(e1)),
    };
  | TyAlias(tp, tdef, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: TyAlias(tp, tdef, ebody'),
      annotation:
        Language.IdTagged.IdTag.mk_internal(Language.IdTagged.ids(e1)),
    };
  | Use(t, ebody) =>
    let ebody' = append_exp(ebody, e2);
    {
      term: Use(t, ebody'),
      annotation:
        Language.IdTagged.IdTag.mk_internal(Language.IdTagged.ids(e1)),
    };
  };
};

let stitch_term = (eds: p('a)): stitched(TermItem.t) => {
  ();

  let user_impl_term = {
    eds.your_impl |> term_of;
  };

  let wrapped_user_impl =
    Let(
      Var("answer") |> Language.Pat.fresh,
      user_impl_term,
      EmptyHole |> Language.Exp.fresh,
    )
    |> Language.Exp.fresh;

  let hidden_tests_term =
    eds.wrapper
      ? append_exp(wrapped_user_impl, term_of(eds.hidden_tests.tests))
      : append_exp(user_impl_term, term_of(eds.hidden_tests.tests));

  {
    user_impl: wrap(user_impl_term, eds.your_impl),
    hidden_tests: wrap(hidden_tests_term, eds.hidden_tests.tests),
  };
};

let user_impl_key = "user_impl";
let hidden_tests_key = "hidden_tests";

let key_for_statics = (pos: pos): string =>
  switch (pos) {
  | YourImpl => user_impl_key
  | HiddenTests => hidden_tests_key
  };

let pos_of_key = (key: string): pos =>
  switch () {
  | _ when key == user_impl_key => YourImpl
  | _ when key == hidden_tests_key => HiddenTests
  | _ => failwith("invalid key")
  };

// Module Export

let editor_pp = (fmt, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let serialization = Zipper.show(zipper);
  Format.pp_print_string(fmt, serialization);
};

let export_module = (_module_name, {eds, _}: state) => {
  let prefix = "let exercise: Tutorial.spec = ";
  let record = show_p(editor_pp, eds);
  let data = prefix ++ record ++ "\n";
  data;
};

let transitionary_editor_pp = (fmt, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let code = PersistentZipper.to_string(zipper);
  Format.pp_print_string(fmt, "\"" ++ String.escaped(code) ++ "\"");
};

let export_transitionary_module = (_module_name, {eds, _}: state) => {
  let prefix = "let exercise: Tutorial.spec = Tutorial.transition(";
  let record = show_p(transitionary_editor_pp, eds);
  let data = prefix ++ record ++ ")\n";
  data;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_tutorial_mode = list((pos, PersistentZipper.t));

let unpersist = (~instructor_mode, positioned_zippers, spec: spec): spec => {
  let lookup = (pos, default) =>
    if (is_editable(pos, ~instructor_mode)) {
      positioned_zippers
      |> List.assoc_opt(pos)
      |> Option.map(PersistentZipper.unpersist(~root=Exp))
      |> Option.value(~default);
    } else {
      default;
    };
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
    your_impl,
    hidden_tests: {
      tests: hidden_tests_tests,
      hints: spec.hidden_tests.hints,
    },
  };
};
