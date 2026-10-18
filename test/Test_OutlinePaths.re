open Alcotest;
open Haz3lcore;
open Language;
module OutlineTree = Web.OutlineTree;
module ScratchPersist = Web.ScratchPersist;

/* outline paths are occurrence-qualified: every row's label_path resolves
   back to its own id, even among same-labeled rows */

let term_of_text = (text: string): Exp.t => {
  let z =
    switch (MarkerParse.of_text(~root=Sort.Exp, text)) {
    | Some(z) => z
    | None => failwith("parse failed: " ++ text)
    };
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
};

/* all (id, path) pairs of the outline, depth-first */
let rec walk =
        (
          path: OutlineTree.path,
          ns: list((OutlineTree.node, OutlineTree.path_seg)),
        )
        : list((Id.t, OutlineTree.path)) =>
  List.concat_map(
    ((n: OutlineTree.node, seg)) => {
      let here =
        switch (n.o_id) {
        | Some(id) => [(id, path @ [seg])]
        | None => []
        };
      here @ walk(path @ [seg], OutlineTree.segs(n.o_children));
    },
    ns,
  );

let all_rows = (e: Exp.t): list((Id.t, OutlineTree.path)) =>
  walk([], OutlineTree.segs(OutlineTree.of_term(e)));

let check_self_resolution = (label: string, text: string): unit => {
  let e = term_of_text(text);
  let rows = all_rows(e);
  check(bool, label ++ ": outline nonempty", true, rows != []);
  List.iter(
    ((id, _)) =>
      switch (OutlineTree.label_path(id, e)) {
      | None => fail(label ++ ": no label_path for a row id")
      | Some(path) =>
        switch (OutlineTree.resolve_path(path, e)) {
        | Some(id') when id' == id => ()
        | Some(_) => fail(label ++ ": path resolved to a DIFFERENT row")
        | None => fail(label ++ ": path failed to resolve")
        }
      },
    rows,
  );
};

let dup_defs = "let f = 1 in\nlet g = 2 in\nlet f = 3 in\nf + g";

let two_test_groups = "let a = 1 in\ntest 1 == 1 end;\ntest 2 == 2 end;\nlet b = 2 in\ntest 3 == 3 end;\ntest 4 == 4 end;\na + b";

let nested_dups = "let m = module\nlet x = 1 in\nlet x = 2 in\nin\n1";

/* a test's path counts only its own block's tests */
let test_elsewhere = () => {
  let before = "let a = 1 in\ntest 1 == 1 end;\nlet b = 2 in\ntest 2 == 2 end;\ntest 3 == 3 end;\na + b";
  let after = "let a = 1 in\ntest 1 == 1 end;\ntest 0 == 0 end;\nlet b = 2 in\ntest 2 == 2 end;\ntest 3 == 3 end;\na + b";
  let e0 = term_of_text(before);
  let e1 = term_of_text(after);
  /* the program's last test: its number moves from 3 to 4, its path doesn't */
  let last = (e: Exp.t) => {
    let rec tests = (ns: list(OutlineTree.node)) =>
      List.concat_map(
        (n: OutlineTree.node) =>
          n.o_kind == KTest ? [n] : tests(n.o_children),
        ns,
      );
    switch (List.rev(tests(OutlineTree.of_term(e)))) {
    | [n, ..._] => n.o_id
    | [] => None
    };
  };
  switch (Option.bind(last(e0), id => OutlineTree.label_path(id, e0))) {
  | None => fail("no path")
  | Some(path) =>
    check(
      bool,
      "resolves to the same test",
      true,
      OutlineTree.resolve_path(path, e1) == last(e1),
    )
  };
};

/* a test goes by the named row before it: a `use` added above it leaves
   its path */
let use_above_test = () => {
  let before = term_of_text("let b = 1 in\ntest b == 1 end;\nb");
  let after = term_of_text("let b = 1 in\nuse Nat in\ntest b == 1 end;\nb");
  let test = (e: Exp.t) =>
    List.find_map(
      (n: OutlineTree.node) => n.o_kind == KTest ? n.o_id : None,
      OutlineTree.flatten(OutlineTree.of_term(e)),
    );
  switch (test(before), test(after)) {
  | (Some(t0), Some(t1)) =>
    check(
      bool,
      "resolves to the same test",
      true,
      Option.bind(OutlineTree.label_path(t0, before), path =>
        OutlineTree.resolve_path(path, after)
      )
      == Some(t1),
    )
  | _ => fail("no test row")
  };
};

let uses_and_signatures = "type S = {\n  let x : Int\n} in\nuse Nat in\nmodule M : S = {\n  let x = 1\n} in\nuse Nat in\ntest M.x == 1 end;\nM.x";

let cases = [
  test_case(
    "a test added elsewhere leaves a test's path",
    `Quick,
    test_elsewhere,
  ),
  test_case("a use above a test leaves its path", `Quick, use_above_test),
  test_case("use rows and a module with a signature", `Quick, () =>
    check_self_resolution("uses and signatures", uses_and_signatures)
  ),
  test_case(
    "rename moves a slide's keys; delete removes them",
    `Quick,
    () => {
      let db = Web.HazelDB.kv_save;
      db("scratch:Old", "blob");
      db("scratch:Old:pins", "p");
      db("scratch:Older", "other");
      ScratchPersist.rename_slide("scratch", "Old", "New");
      let get = Web.HazelDB.kv_get;
      check(bool, "blob moved", true, get("scratch:New") == Some("blob"));
      check(bool, "pins moved", true, get("scratch:New:pins") == Some("p"));
      check(bool, "old gone", true, get("scratch:Old") == None);
      check(
        bool,
        "neighbour kept",
        true,
        get("scratch:Older") == Some("other"),
      );
      ScratchPersist.forget_slide("scratch", "New");
      check(
        bool,
        "deleted",
        true,
        get("scratch:New") == None && get("scratch:New:pins") == None,
      );
      check(
        bool,
        "neighbour still kept",
        true,
        get("scratch:Older") == Some("other"),
      );
    },
  ),
  test_case(
    "duplicate top-level names: each row round-trips to itself", `Quick, () =>
    check_self_resolution("dup defs", dup_defs)
  ),
  test_case(
    "two separated tests groups: each round-trips to itself", `Quick, () =>
    check_self_resolution("two groups", two_test_groups)
  ),
  test_case("duplicate names inside a module", `Quick, () =>
    check_self_resolution("nested dups", nested_dups)
  ),
  test_case(
    "distinct same-labeled rows get distinct paths",
    `Quick,
    () => {
      let e = term_of_text(two_test_groups);
      let rows = all_rows(e);
      let paths = List.map(snd, rows);
      check(
        int,
        "no two rows share a path",
        List.length(paths),
        List.length(List.sort_uniq(compare, paths)),
      );
    },
  ),
];

/* after a delete the cursor goes where the row was, not to another row
   with the same name */
let successors = () => {
  let ids = ns => List.filter_map((n: OutlineTree.node) => n.o_id, ns);
  let e = term_of_text("let x = 1 in\nlet y = 2 in\nlet x = 3 in\nx + y");
  switch (ids(OutlineTree.of_term(e))) {
  | [x0, y, _, ..._] =>
    check(
      bool,
      "the first x: the row after it",
      true,
      OutlineTree.successor(x0, e) == Some(y),
    )
  | _ => fail("expected rows x, y, x")
  };
  let e = term_of_text("module M = {\n  let a = 1;\n  let b = 2\n} in\nM.a");
  switch (OutlineTree.of_term(e)) {
  | [{o_id: Some(_), o_children, _}, ..._] =>
    switch (ids(o_children)) {
    | [a, b] =>
      check(
        bool,
        "the last member: the one before",
        true,
        OutlineTree.successor(b, e) == Some(a),
      )
    | _ => fail("expected members a, b")
    };
    let e = term_of_text("module M = {\n  let a = 1\n} in\nM.a");
    switch (OutlineTree.of_term(e)) {
    | [{o_id: Some(m'), o_children: [{o_id: Some(a), _}], _}, ..._] =>
      check(
        bool,
        "an only member: its module",
        true,
        OutlineTree.successor(a, e) == Some(m'),
      )
    | _ => fail("expected M with one member")
    };
  | _ => fail("expected module M")
  };
};

/* flipping Outline Arrows redraws the outline, all else unchanged */
let arrows_redraw = () => {
  let statics = {
    ...CachedStatics.empty,
    term:
      term_of_text("let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)"),
  };
  let view = arrows =>
    Web.OutlineControl.view(
      ~arrows,
      ~deck=None,
      ~statics,
      ~segment=[],
      ~inject=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~inject_workspace=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~jump=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~leave=Virtual_dom.Vdom.Effect.Ignore,
      ~statics_on=true,
      (),
    );
  let off = view(false);
  check(
    bool,
    "the same inputs reuse the outline",
    true,
    view(false) === off,
  );
  check(bool, "arrows on: a new outline", false, view(true) === off);
};

/* where a saved name leaves text mode's caret: where the keys put it
   (↑ while renaming), in row mode on the renamed row (Esc), on a new line
   below a new definition (Enter, its keyword leaving); a refused save
   brings the caret back to what was typed */
let save_caret = () => {
  module C = Web.OutlineControl;
  module E = Web.OutlineEdit;
  let settings = Web.Globals.Model.init().settings;
  let seg = CorpusUtil.seg_of("let a = 1 in\nlet b = 2 in\na + b");
  let code =
    switch (
      Web.ScratchModel.Scratchpad.mk_code(
        ~name="t",
        ~editor=Web.ScratchFocus.cell_of_seg(seg),
        (),
      ).
        kind
    ) {
    | Code(code) => code
    | Drv(_) => fail("not a code slide")
    };
  let term = MakeTerm.Incr.term_of(seg);
  let row = CorpusUtil.row_by_label(term);
  let save = (sv, after) =>
    C.update(
      ~settings,
      ~schedule_workspace=_ => (),
      ~prefix="scratch",
      ~name="t",
      Save(sv, after),
      code,
    ).
      model;
  let line = label =>
    E.Row(
      Option.get(OutlineTree.label_path(row(label), term)),
      Some(Item(row(label))),
    );
  let rename_b: E.save = {
    sv_line: line("b"),
    sv_text: "c",
    sv_col: 1,
  };
  let up = E.at(line("a"));
  C.text := Some(up);
  ignore(save(rename_b, Stay));
  check(bool, "↑ renaming: the caret stays up", true, C.text^ == Some(up));
  C.text := None;
  let renamed = save(rename_b, Leave);
  let term' = Web.Program.statics(renamed.program).term;
  check(
    bool,
    "Esc: row mode, the cursor on the renamed row",
    true,
    C.cursor^ == OutlineTree.label_path(row("b"), term')
    && OutlineTree.node_of(row("b"), term')
    |> Option.map((n: OutlineTree.node) => n.o_label) == Some("c"),
  );
  let new_t = E.New(row("a"), Below);
  C.text := Some(E.at(new_t));
  C.created := None;
  let act = (action, code) =>
    C.update(
      ~settings,
      ~schedule_workspace=_ => (),
      ~prefix="scratch",
      ~name="t",
      action,
      code,
    ).
      model;
  let made = act(Create(new_t, "type "), code);
  let t =
    switch (C.text^) {
    | Some({fresh: Some(f), _}) => f.f_id
    | _ => fail("no new type")
    };
  let typed = act(Type(new_t, Insert("T")), made);
  /* the Enter's own keydown puts the caret on the next line, waiting */
  C.text := Some(E.at(New(t, Below)));
  ignore(
    act(
      Settle(
        new_t,
        {
          f_id: t,
          f_kw: "type ",
        },
        Next,
      ),
      typed,
    ),
  );
  check(
    bool,
    "Enter: a new line below the new type, its keyword leaving",
    true,
    (
      switch (C.text^) {
      | Some({line: New(a, Below), fresh: Some(_), _}) => a == t
      | _ => false
      }
    )
    && C.created^ == Some((t, "type ")),
  );
  C.text := Some(up);
  ignore(
    save(
      {
        sv_line: line("b"),
        sv_text: "B",
        sv_col: 1,
      },
      Stay,
    ),
  );
  check(
    bool,
    "refused: the caret back on b, with what was typed and why",
    true,
    switch (C.text^) {
    | Some({line: back, col: 1, pending: Some("B"), error: Some(_), _}) =>
      E.same_line(back, line("b"))
    | _ => false
    },
  );
  C.cursor := None;
  C.created := None;
  C.text := None;
};

/* the outline's keys over [src]'s rows, every row shown, the cell code
   deciding which open; the handlers record the cells asked for and text
   mode's state */
let sidebar = (src: string) => {
  module S = Web.OutlineSidebar;
  let seg = CorpusUtil.seg_of(src);
  let term = MakeTerm.Incr.term_of(seg);
  let refused = Web.ScratchFocus.refusals(~root=Exp, ~want=_ => true, seg);
  let visible = OutlineHarness.visible(term);
  let cursor = ref(None);
  let toggled = ref([]);
  let text = ref(None);
  let on =
    OutlineHarness.handlers(
      ~cursor,
      ~text,
      ~toggle=
        id => {
          toggled := [id, ...toggled^];
          Virtual_dom.Vdom.Effect.Ignore;
        },
      (),
    );
  let props =
    OutlineHarness.props(~refusal=id => Id.Map.find_opt(id, refused), ());
  let row = label =>
    Array.find_opt((r: S.visible_row) => r.r_node.o_label == label, visible);
  /* [key] pressed with the cursor on [label]'s row: the cells it asked
     to open or close, and text mode's state after */
  let press = (~meta=false, key: string, label: string) => {
    cursor := Option.map((r: S.visible_row) => r.r_path, row(label));
    toggled := [];
    text := None;
    ignore(
      S.keys(~props, ~on, ~visible, OutlineHarness.keydown(~meta, key)),
    );
    (toggled^, text^);
  };
  (term, press);
};

/* Space opens a row's cell, but not under a projector: the cell code
   refuses those rows, and the outline offers nothing */
let space_under_projector = () => {
  let (term, press) = sidebar("let a = 1 in\n^^fold((let b = 2 in\nb))");
  let row = CorpusUtil.row_by_label(term);
  check(bool, "a opens", true, fst(press(" ", "a")) == [row("a")]);
  check(bool, "b, folded away, doesn't", true, fst(press(" ", "b")) == []);
};

/* F2 and ⌘Enter start text mode on the cursor's row: at a name's end, or
   a ghost line's one caret position */
let start_text = () => {
  module E = Web.OutlineEdit;
  let (term, press) = sidebar("let ab = 1 in\ntest ab == 1 end;\nab");
  let at = (label, col) =>
    Option.map(
      path =>
        E.at(
          Row(path, Some(Item(CorpusUtil.row_by_label(term, label)))),
          ~col,
        ),
      OutlineTree.label_path(CorpusUtil.row_by_label(term, label), term),
    );
  check(bool, "F2 on ab", true, snd(press("F2", "ab")) == at("ab", 2));
  check(
    bool,
    "⌘Enter on ab",
    true,
    snd(press(~meta=true, "Enter", "ab")) == at("ab", 2),
  );
  check(bool, "F2 on a test", true, snd(press("F2", "1")) == at("1", 0));
};

/* runs of tests: in a module, at the top level, and a lone test */
let runs = "let a = 1 in\nmodule M = {\n  let x = 1;\n  test x == 1 end;\n  test x == 2 end\n} in\ntest a == 1 end;\ntest a == 2 end;\nlet b = 2 in\ntest b == 2 end;\nb";

/* the rows a slide with [folds] shows, by label (the ⇒ has none) */
let shown = (~folds, e: Exp.t): list(string) =>
  List.map(
    (r: Web.OutlineSidebar.visible_row) => r.r_node.o_label,
    Array.to_list(OutlineHarness.visible(~folds, e)),
  );

/* a run starts folded, every other row open; a choice by hand wins */
let default_fold = () => {
  let e = term_of_text(runs);
  let row = CorpusUtil.row_by_label(e);
  check(
    list(string),
    "runs folded, the rest open",
    ["a", "M", "x", "tests", "tests", "b", "5", ""],
    shown(~folds=[], e),
  );
  let top = Option.get(OutlineTree.run_of(row("3"), e));
  check(
    list(string),
    "a run opened by hand",
    ["a", "M", "x", "tests", "tests", "3", "4", "b", "5", ""],
    shown(~folds=[(top, false)], e),
  );
  let m = Option.get(OutlineTree.label_path(row("M"), e));
  check(
    list(string),
    "a module folded by hand",
    ["a", "M", "tests", "b", "5", ""],
    shown(~folds=[(m, true)], e),
  );
};

let code_of = (src: string): Web.ScratchModel.Scratchpad.code =>
  switch (
    Web.ScratchModel.Scratchpad.mk_code(
      ~name="t",
      ~editor=Web.ScratchFocus.cell_of_seg(CorpusUtil.seg_of(src)),
      (),
    ).
      kind
  ) {
  | Code(code) => code
  | Drv(_) => fail("not a code slide")
  };

/* a run opened by hand stays open through a reload, and through a test
   added elsewhere, which renumbers its tests but leaves its path */
let opened_run_kept = () => {
  module C = Web.OutlineControl;
  let (prefix, name) = ("foldtest", "t");
  let code = code_of(runs);
  let e = Web.Workspace.fresh_term(code.program);
  let top =
    Option.get(OutlineTree.run_of(CorpusUtil.row_by_label(e, "3"), e));
  let flip = () =>
    ignore(
      C.update(
        ~settings=Web.Globals.Model.init().settings,
        ~schedule_workspace=_ => (),
        ~prefix,
        ~name,
        Collapse(top),
        code,
      ),
    );
  let folded = () =>
    OutlineTree.folded(
      ~folds=ScratchPersist.folds(prefix, name),
      top,
      KTests,
    );
  check(bool, "folded with no choice made", true, folded());
  flip();
  check(bool, "opened by hand", false, folded());
  /* a reload: the slide's folds as saved */
  let ck = ScratchPersist.content_key(prefix, name);
  Hashtbl.remove(ScratchPersist.slide_folds, ck);
  ScratchPersist.forget_gates(ck);
  ScratchPersist.stage_side(prefix, name);
  check(bool, "still open after a reload", false, folded());
  let added =
    term_of_text(
      "let a = 1 in\nmodule M = {\n  let x = 1;\n  test x == 1 end;\n  test x == 2 end;\n  test x == 3 end\n} in\ntest a == 1 end;\ntest a == 2 end;\nlet b = 2 in\ntest b == 2 end;\nb",
    );
  check(
    list(string),
    "a test added in M's run: this one still open",
    ["a", "M", "x", "tests", "tests", "4", "5", "b", "6", ""],
    shown(~folds=ScratchPersist.folds(prefix, name), added),
  );
  flip();
  check(bool, "folded again by hand", true, folded());
  ScratchPersist.forget_slide(prefix, name);
};

/* a run's tally, from its tests' statuses as their rows show them */
let tallies = () => {
  module S = Web.OutlineSidebar;
  let tally_of = (src: string) => {
    let term = MakeTerm.go(CorpusUtil.seg_of(src)).term;
    let (_, elab) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );
    let (_, state) = Evaluator.evaluate(~env=Builtins.env_init(), elab);
    let results = EvaluatorState.get_tests(state);
    let test_status = id =>
      TestMap.lookup(id, results) |> Option.map(TestMap.joint_status);
    switch (
      List.find_opt(
        (n: OutlineTree.node) => n.o_kind == KTests,
        OutlineTree.flatten(OutlineTree.of_term(term)),
      )
    ) {
    | Some(run) => S.run_tally(S.run_statuses(~test_status, run))
    | None => fail("no run")
    };
  };
  let run = tests =>
    "let f = fun x -> x + 1 in\n"
    ++ String.concat("", List.map(t => "test " ++ t ++ " end;\n", tests))
    ++ "f(0)";
  let tally = testable(Fmt.(option(pair(string, bool))), (==));
  check(
    tally,
    "all pass",
    Some(("3/3", false)),
    tally_of(run(["f(1) == 2", "f(2) == 3", "f(3) == 4"])),
  );
  check(
    tally,
    "one fails: in red",
    Some(("1 failing", true)),
    tally_of(run(["f(1) == 2", "f(2) == 3", "f(3) == 5"])),
  );
  check(
    tally,
    "holes, none failing: those passing",
    Some(("2/4", false)),
    tally_of(
      run([
        {js|f(1) == 2|js},
        {js|f(2) == ¿|js},
        {js|f(¿) == 4|js},
        "f(3) == 4",
      ]),
    ),
  );
  check(
    tally,
    "a failure over holes",
    Some(("1 failing", true)),
    tally_of(run([{js|f(1) == ¿|js}, "f(2) == 4"])),
  );
  check(tally, "no results yet", None, S.run_tally([None, None]));
};

/* the editor-caret mark goes where its row shows: a folded run's label,
   by its first test, as a folded module's row takes its members' */
let mark_in_runs = () => {
  module C = Web.OutlineControl;
  module E = Web.OutlineEdit;
  let e = term_of_text(runs);
  let row = CorpusUtil.row_by_label(e);
  let top = Option.get(OutlineTree.run_of(row("4"), e));
  let key = testable(Fmt.of_to_string(E.show_key), (==));
  check(
    key,
    "in a folded run: its label",
    E.Run(row("3")),
    C.shown_row(~folds=[], row("4"), e),
  );
  check(
    key,
    "opened: the test's own row",
    E.Item(row("4")),
    C.shown_row(~folds=[(top, false)], row("4"), e),
  );
  let m = Option.get(OutlineTree.label_path(row("M"), e));
  check(
    key,
    "in a run in a folded module: the module",
    E.Item(row("M")),
    C.shown_row(~folds=[(m, true)], row("1"), e),
  );
  check(
    key,
    "a lone test: its own row",
    E.Item(row("5")),
    C.shown_row(~folds=[], row("5"), e),
  );
  check(
    bool,
    "a run's label, by path: the outline's cursor there",
    true,
    C.key_path(E.Run(row("3")), e) == Some(top),
  );
};

/* a level above the slide, as a caller supplies it */
let above: Web.OutlineSidebar.above = {
  a_label: "up",
  a_out: "out to up",
  a_here: "up here",
  a_chip: Some(("chip", "chip title")),
  a_alarm: None,
};

/* a slide in a deck, and a level above it, shown or not */
let slide_deck = (): Web.OutlineControl.deck => (
  "scratch",
  {
    current: 0,
    scratchpads: [
      Web.ScratchModel.Scratchpad.mk_code(
        ~name="Docs / Slide",
        ~editor=
          Web.ScratchFocus.cell_of_seg(CorpusUtil.seg_of("let a = 1 in\na")),
        (),
      ),
    ],
  },
);
let enclosing = (shown): Web.OutlineControl.enclosing => {
  e_above: above,
  e_shown: shown,
  e_go: _ => Virtual_dom.Vdom.Effect.Ignore,
  e_chip: Virtual_dom.Vdom.Effect.Ignore,
};

/* what a key asked of the outline's handlers */
type asked = {
  k_toggled: list(Id.t),
  k_ops: list((Web.OutlineEdit.def_op, Id.t)),
  k_text: bool,
  k_menus: int,
  k_went: list(bool),
  k_cursor: option(OutlineTree.path),
};

/* the outline's keys over a module and two items at [level], zoomed
   along [trail] */
let at_level = (~trail=[], level: Web.OutlineSidebar.level) => {
  module S = Web.OutlineSidebar;
  let seg =
    CorpusUtil.seg_of(
      "module M = {\n  let a = 1\n} in\nlet b = 2 in\ntest b == 2 end;\nb",
    );
  let term = MakeTerm.Incr.term_of(seg);
  let visible = OutlineHarness.visible(term);
  let cursor = ref(None);
  let toggled = ref([]);
  let ops = ref([]);
  let text = ref(None);
  let menus = ref(0);
  let went = ref([]);
  let on =
    OutlineHarness.handlers(
      ~cursor,
      ~text,
      ~menus,
      ~went,
      ~toggle=
        id => {
          toggled := [id, ...toggled^];
          Virtual_dom.Vdom.Effect.Ignore;
        },
      ~def_op=
        (op, id) => {
          ops := [(op, id), ...ops^];
          Virtual_dom.Vdom.Effect.Ignore;
        },
      (),
    );
  let props = OutlineHarness.props(~level, ());
  let props = {
    ...props,
    header: {
      ...props.header,
      h_trail: trail,
    },
  };
  /* [key] pressed with the cursor on [label]'s row */
  let press = (~meta=false, ~alt=false, ~shift=false, ~code=?, key, label) => {
    cursor :=
      Array.find_opt(
        (r: S.visible_row) => r.r_node.o_label == label,
        visible,
      )
      |> Option.map((r: S.visible_row) => r.r_path);
    toggled := [];
    ops := [];
    text := None;
    menus := 0;
    went := [];
    ignore(
      S.keys(
        ~props,
        ~on,
        ~visible,
        OutlineHarness.keydown(~meta, ~alt, ~shift, ~code?, key),
      ),
    );
    {
      k_toggled: toggled^,
      k_ops: ops^,
      k_text: text^ != None,
      k_menus: menus^,
      k_went: went^,
      k_cursor: cursor^,
    };
  };
  (term, on, press);
};

/* every edit key the slide's outline takes, whether it asked for its
   edit (the menu keys need a page: enclosing_level has them) */
let edit_keys =
    (
      press:
        (
          ~meta: bool=?,
          ~alt: bool=?,
          ~shift: bool=?,
          ~code: string=?,
          string,
          string
        ) =>
        asked,
    ) => [
  ("Space", press(" ", "b").k_toggled != []),
  ("⌘D", press(~meta=true, ~code="KeyD", "d", "b").k_ops != []),
  ("⌘⌫", press(~meta=true, "Backspace", "b").k_ops != []),
  ("Delete", press("Delete", "b").k_ops != []),
  ("⌥↑", press(~alt=true, "ArrowUp", "b").k_ops != []),
  ("F2", press("F2", "b").k_text),
  ("⌘Enter", press(~meta=true, "Enter", "b").k_text),
];

/* without a level above, the outline is as it always was: no crumbs or
   rows of a level, edits taken, ⌥← zooms out */
let plain_level = () => {
  module S = Web.OutlineSidebar;
  check(bool, "no crumbs", true, S.level_crumbs(Plain) == []);
  check(bool, "no rows", true, S.level_rows(Plain) == []);
  check(bool, "no chip", true, S.level_chip(Plain) == None);
  let (_, _, press) = at_level(Plain);
  List.iter(
    ((name, took)) => check(bool, name ++ " edits", true, took),
    edit_keys(press),
  );
  check(
    bool,
    "⌥← zooms out",
    true,
    press(~alt=true, "ArrowLeft", "b").k_went == [],
  );
  check(
    bool,
    "⌥→ zooms in",
    true,
    press(~alt=true, "ArrowRight", "M").k_went == [],
  );
  /* OutlineControl: Plain unless a caller supplies a level above */
  let deck = slide_deck();
  let level = Web.OutlineControl.level;
  check(
    bool,
    "a slide: Plain",
    true,
    level(~enclosing=None, Some(deck)) == Plain,
  );
  check(
    bool,
    "no slide, a level above: Plain",
    true,
    level(~enclosing=Some(enclosing(true)), None) == Plain,
  );
  check(
    bool,
    "a slide under a level above",
    true,
    level(~enclosing=Some(enclosing(false)), Some(deck)) == InSlide(above),
  );
  check(
    bool,
    "the level above, the slide its way back",
    true,
    level(~enclosing=Some(enclosing(true)), Some(deck))
    == Enclosing(above, "Docs / Slide"),
  );
};

/* in a slide under a level above: its crumb leads, and ⌥← at the slide's
   top goes up */
let in_slide_level = () => {
  module S = Web.OutlineSidebar;
  check(
    bool,
    "the level's crumb, up",
    true,
    S.level_crumbs(InSlide(above)) == [("up", "out to up", Up)],
  );
  check(bool, "no rows", true, S.level_rows(InSlide(above)) == []);
  check(bool, "no chip", true, S.level_chip(InSlide(above)) == None);
  let (term, _, press) = at_level(InSlide(above));
  List.iter(
    ((name, took)) => check(bool, name ++ " edits", true, took),
    edit_keys(press),
  );
  check(
    bool,
    "⌥← at the top: up",
    true,
    press(~alt=true, "ArrowLeft", "b").k_went == [true],
  );
  let m = CorpusUtil.row_by_label(term, "M");
  let (_, _, press) = at_level(~trail=[(m, "M")], InSlide(above));
  check(
    bool,
    "⌥← zoomed in: zooms out",
    true,
    press(~alt=true, "ArrowLeft", "a").k_went == [],
  );
};

/* at the level above: its crumb, the slide's to go back, the chip; a
   last row back; keys navigate only, ⌥→ goes back */
let enclosing_level = () => {
  module S = Web.OutlineSidebar;
  let level: S.level = Enclosing(above, "Docs / Slide");
  check(
    bool,
    "here, then the slide's crumb back",
    true,
    S.level_crumbs(level)
    == [("up", "up here", Here), ("Slide", "back into Docs / Slide", Back)],
  );
  check(
    bool,
    "the chip",
    true,
    S.level_chip(level) == Some(("chip", "chip title")),
  );
  check(
    bool,
    "no chip, none drawn",
    true,
    S.level_chip(
      Enclosing(
        {
          ...above,
          a_chip: None,
        },
        "s",
      ),
    )
    == None,
  );
  check(
    bool,
    "the last row: back into the slide",
    true,
    S.level_rows(level) == [("Slide", "back into Docs / Slide")],
  );
  let (term, on, press) = at_level(level);
  List.iter(
    ((name, took)) => check(bool, name ++ " refused", false, took),
    edit_keys(press)
    @ [
      ("⌘.", press(~meta=true, ".", "b").k_menus > 0),
      ("⇧F10", press(~shift=true, "F10", "b").k_menus > 0),
    ],
  );
  check(
    bool,
    "⌥→ from a row: back",
    true,
    press(~alt=true, "ArrowRight", "b").k_went == [false],
  );
  check(
    bool,
    "⌥→ from a module: back, not into it",
    true,
    press(~alt=true, "ArrowRight", "M").k_went == [false],
  );
  check(
    bool,
    "⌥←: nothing above",
    true,
    press(~alt=true, "ArrowLeft", "b").k_went == [],
  );
  check(
    bool,
    "↓ still moves: M to its member",
    true,
    press("ArrowDown", "M").k_cursor
    == OutlineTree.label_path(CorpusUtil.row_by_label(term, "a"), term),
  );
  /* the clicks: the back crumb and row go back, the chip is the caller's */
  let went = ref([]);
  let on' = OutlineHarness.handlers(~went, ());
  ignore(S.level_effect(on', Back));
  ignore(S.level_effect(on', Up));
  check(bool, "back, up", true, went^ == [true, false]);
  check(
    bool,
    "the chip's",
    true,
    S.level_effect(on, Chip) === on.above_chip,
  );
};

/* the level above folds its own rows, for the session: the slide's folds
   and outline are untouched */
let above_folds = () => {
  module C = Web.OutlineControl;
  let term =
    term_of_text("let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)");
  let statics = {
    ...CachedStatics.empty,
    term,
  };
  let deck = slide_deck();
  let view = shown =>
    C.view(
      ~arrows=true,
      ~deck=Some(deck),
      ~statics,
      ~segment=[],
      ~inject=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~inject_workspace=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~jump=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~leave=Virtual_dom.Vdom.Effect.Ignore,
      ~statics_on=true,
      ~enclosing=enclosing(shown),
      (),
    );
  let f =
    Option.get(
      OutlineTree.label_path(CorpusUtil.row_by_label(term, "f"), term),
    );
  C.above_folds := [];
  let up = view(true);
  check(bool, "the same folds: the same outline", true, view(true) === up);
  C.fold_above(term, f);
  check(bool, "folded", true, C.above_folds^ == [(f, true)]);
  check(bool, "a fold: a new outline", false, view(true) === up);
  let slide = view(false);
  C.fold_above(term, f);
  check(bool, "unfolded", true, C.above_folds^ == [(f, false)]);
  check(bool, "the slide's outline: unchanged", true, view(false) === slide);
  check(
    bool,
    "the slide's folds: none",
    true,
    Web.ScratchPersist.folds("scratch", "Docs / Slide") == [],
  );
  /* a run there starts folded too: its first flip opens it */
  let runs = term_of_text(runs);
  let run =
    Option.get(
      OutlineTree.run_of(CorpusUtil.row_by_label(runs, "3"), runs),
    );
  C.above_folds := [];
  C.fold_above(runs, run);
  check(bool, "a run: opened", true, C.above_folds^ == [(run, false)]);
  C.above_folds := [];
};

/* a level above in trouble: its crumb is the alarm, holding the chip's way
   out, in a slide and at the level */
let alarmed_level = () => {
  module S = Web.OutlineSidebar;
  let al: S.alarm = {
    al_why: "up is broken",
    al_act: "x",
    al_act_title: "undo it",
  };
  let alarmed = {
    ...above,
    a_alarm: Some(al),
  };
  check(
    bool,
    "none by default",
    true,
    S.level_alarm(InSlide(above)) == None,
  );
  check(
    bool,
    "in a slide",
    true,
    S.level_alarm(InSlide(alarmed)) == Some(al),
  );
  check(
    bool,
    "at the level",
    true,
    S.level_alarm(Enclosing(alarmed, "s")) == Some(al),
  );
  check(
    bool,
    "the pill holds the chip's way out",
    true,
    S.level_chip(Enclosing(alarmed, "s")) == None,
  );
};

let tests = [
  (
    "OutlineLevels",
    [
      test_case("no level above: as before", `Quick, plain_level),
      test_case("a slide under a level above", `Quick, in_slide_level),
      test_case("at the level above", `Quick, enclosing_level),
      test_case("the level above's folds", `Quick, above_folds),
      test_case("an alarmed level above", `Quick, alarmed_level),
    ],
  ),
  (
    "OutlinePaths",
    cases
    @ [
      test_case("a deleted row's successor", `Quick, successors),
      test_case("Outline Arrows redraws the outline", `Quick, arrows_redraw),
      test_case("where a saved name leaves the caret", `Quick, save_caret),
      test_case(
        "Space does nothing under a projector",
        `Quick,
        space_under_projector,
      ),
      test_case("F2 and ⌘Enter start text mode", `Quick, start_text),
      test_case("test runs start folded", `Quick, default_fold),
      test_case(
        "an opened run stays open: a reload, a test elsewhere",
        `Quick,
        opened_run_kept,
      ),
      test_case("a run's tally", `Quick, tallies),
      test_case("the caret's mark in a folded run", `Quick, mark_in_runs),
    ],
  ),
];
