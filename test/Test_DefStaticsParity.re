open Alcotest;
open Haz3lcore;
open Language;

/* after each edit step, DefStatics computed from the previous step matches
   a cold calc and monolithic statics, and seeded evaluation a cold one */

let settings = CoreSettings.on;

let parse = (~root, src: string): Segment.t =>
  switch (CorpusUtil.parse(~root, src)) {
  | Some(seg) => seg
  | None =>
    switch (MarkerParse.of_text(~root, src)) {
    | Some(z) => Zipper.unselect_and_zip(z)
    | None => fail("parse failed: " ++ src)
    }
  };

let term_of = (~root, seg): Exp.t =>
  root == Sort.Mod ? MakeTerm.go_mod_root(seg).term : MakeTerm.go(seg).term;

/* the outline row at [path] (labels from the top) */
let row = (term: Exp.t, path: list(string)): Id.t => {
  let rec go = (nodes: list(Web.OutlineTree.node), path) =>
    switch (path) {
    | [] => None
    | [l, ...rest] =>
      List.find_opt((n: Web.OutlineTree.node) => n.o_label == l, nodes)
      |> Option.map((n: Web.OutlineTree.node) =>
           rest == [] ? n.o_id : go(n.o_children, rest)
         )
      |> Option.join
    };
  switch (go(Web.OutlineTree.of_term(term), path)) {
  | Some(id) => id
  | None => fail("no row " ++ String.concat("/", path))
  };
};

type step = (~root: Sort.t, Segment.t) => Segment.t;

let token = (needle, repl): step =>
  (~root as _, seg) =>
    switch (CorpusUtil.edit_token(~needle, ~repl, seg)) {
    | (seg, true) => seg
    | (_, false) => fail("no token " ++ needle)
    };

let op = (op: Web.OutlineSidebar.def_op, path): step =>
  (~root, seg) =>
    switch (
      Web.ItemEdit.apply(
        ~mod_root=root == Sort.Mod,
        op,
        row(term_of(~root, seg), path),
        seg,
      )
    ) {
    | Some((seg, _)) => seg
    | None => fail("op failed at " ++ String.concat("/", path))
    };

let evaluate = (~prev=IncrEval.empty, ds: DefStatics.t) =>
  switch (DefStatics.whole_elab(ds)) {
  | None => fail("whole_elab: shape gap")
  | Some(elab) =>
    Evaluator.evaluate(
      ~prev,
      ~eval_info=
        EvalInfo.of_info_map(
          ~probe_all=settings.probe_all,
          ~targets=Id.Map.empty,
          ds.merged,
        ),
      ~env=Builtins.env_init,
      elab,
    )
  };

let tests_of = (state: EvaluatorState.t) =>
  List.sort(
    compare,
    List.map(
      ((id, rs)) =>
        (Id.show(id), TestStatus.show(TestMap.joint_status(rs))),
      state.tests,
    ),
  );

let script = (~root=Sort.Exp, ~eval=true, src: string, steps: list(step), ()) => {
  let exact = (i, ds, prev) => {
    let name = "step " ++ string_of_int(i);
    check(list(string), name, [], DefStatics.divergences(~settings, ds));
    eval
      ? {
        let (v, state) = evaluate(~prev, ds);
        let (v_cold, state_cold) = evaluate(ds);
        check(
          bool,
          name ++ " seeded value",
          true,
          Exp.fast_equal(v_cold, v),
        );
        check(
          list(pair(string, string)),
          name ++ " seeded tests",
          tests_of(state_cold),
          tests_of(state),
        );
        state.incr_eval;
      }
      : prev;
  };
  let seg = parse(~root, src);
  let ds = DefStatics.calc(~settings, term_of(~root, seg));
  let incr = exact(0, ds, IncrEval.empty);
  ignore(
    List.fold_left(
      ((i, seg, prev, incr), step: step) => {
        let seg = step(~root, seg);
        let ds = DefStatics.calc(~settings, ~prev, term_of(~root, seg));
        let incr = exact(i, ds, incr);
        (i + 1, seg, ds, incr);
      },
      (1, seg, ds, incr),
      steps,
    ),
  );
};

let mega = (~root, ~eval=true, file, steps, ()) =>
  switch (CorpusUtil.mega_src(file)) {
  | Some(src) => script(~root, ~eval, src, steps, ())
  | None => fail("corpus unreadable: " ++ file)
  };

let tests = (
  "DefStaticsParity",
  [
    test_case(
      "deleting a member dirties its users",
      `Quick,
      script(
        "module M = {\n  let x = 1;\n  let y = 2\n} in\nM.x + M.y",
        [op(Delete, ["M", "x"])],
      ),
    ),
    test_case(
      "deleting a member its sibling uses",
      `Quick,
      script(
        "module M = {\n  let x = 1;\n  let y = x + 1\n} in\nM.y",
        [op(Delete, ["M", "x"])],
      ),
    ),
    test_case(
      "moving a member below its user",
      `Quick,
      script(
        "module M = {\n  let x = 1;\n  let y = 2;\n  let z = x + y\n} in\nM.z",
        [op(MoveDown, ["M", "x"]), op(MoveDown, ["M", "x"])],
      ),
    ),
    test_case(
      "moving a member above its dependency",
      `Quick,
      script(
        "module M = {\n  let x = 1;\n  let y = x + 1\n} in\nM.y",
        [op(MoveUp, ["M", "y"])],
      ),
    ),
    test_case(
      "an item moving above its neighbor",
      `Quick,
      script(
        "let a = 1 in\nlet b = 2 in\nlet c = a + b in\nc",
        [op(MoveUp, ["c"]), op(MoveUp, ["c"])],
      ),
    ),
    test_case(
      "top-level stepper filter",
      `Quick,
      script(
        "let x = 1 in\neval x in\nlet y = x + 1 in\ny",
        [token("1", "5")],
      ),
    ),
    test_case(
      "constructor resolves to the latest sum",
      `Quick,
      script(
        "type A = Foo + Bar in\nlet v = Foo in\ntype B = Qux + Baz in\nlet w = Foo in\nw",
        [token("Qux", "Foo")],
      ),
    ),
    test_case(
      "a module named like a constructor",
      `Quick,
      script(
        "type A = Foo + Bar in\nmodule Baz = {\n  let x = 1\n} in\nlet w = Foo in\nw",
        [token("Baz", "Foo"), token("Foo", "Qux")],
      ),
    ),
    test_case(
      "unused binders with a hole in scope",
      `Quick,
      script("let x = 1 in\nlet y = 2 in\n¿", [token("2", "3")]),
    ),
    test_case(
      "mega-1k",
      `Quick,
      mega(
        ~root=Exp,
        "mega-1k.hz",
        [token("75", "\"x\""), op(Delete, ["WateringTimer", "format"])],
      ),
    ),
    test_case(
      "mega-mod-1k",
      `Quick,
      mega(
        ~root=Mod,
        ~eval=false,
        "mega-mod-1k.hz",
        [token("75", "\"x\""), op(Delete, ["WateringTimer", "format"])],
      ),
    ),
  ],
);
