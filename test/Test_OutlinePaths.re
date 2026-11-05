open Alcotest;
open Haz3lcore;
open Language;
module OutlineTree = Web.OutlineTree;
module ScratchPersist = Web.ScratchPersist;

/* outline paths are occurrence-qualified: every row's label_path resolves
   back to its own id, even among same-labeled rows */

let term_of_text = (text: string): OutlineTree.t =>
  CorpusUtil.outline_of_text(text);

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

let all_rows = (e: OutlineTree.t): list((Id.t, OutlineTree.path)) =>
  walk([], OutlineTree.segs(OutlineTree.rows(e)));

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
  let last = (e: OutlineTree.t) => {
    let rec tests = (ns: list(OutlineTree.node)) =>
      List.concat_map(
        (n: OutlineTree.node) =>
          n.o_kind == KTest ? [n] : tests(n.o_children),
        ns,
      );
    switch (List.rev(tests(OutlineTree.rows(e)))) {
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
  let test = (e: OutlineTree.t) =>
    List.find_map(
      (n: OutlineTree.node) => n.o_kind == KTest ? n.o_id : None,
      OutlineTree.flatten(OutlineTree.rows(e)),
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

/* rows as their kinds and labels, children bracketed */
let rec kinds = (ns: list(OutlineTree.node)): string =>
  ns
  |> List.map((n: OutlineTree.node) =>
       (
         switch (n.o_kind) {
         | KModule => "module"
         | KFn => "fn"
         | KConst => "const"
         | KType => "type"
         | KTest => "test"
         | KTests => "tests"
         | KStmt => "stmt"
         | KUse => "use"
         | KTrail => "trail"
         }
       )
       ++ (n.o_label == "" ? "" : " " ++ n.o_label)
       ++ (n.o_children == [] ? "" : " [" ++ kinds(n.o_children) ++ "]")
     )
  |> String.concat(", ");

let kinds_of = (text: string): string =>
  kinds(OutlineTree.rows(term_of_text(text)));

/* `let f(x) = …` keeps its parameters in its pattern */
let sugar_kinds = () => {
  check(
    string,
    "a sugared function is a function, with its ⇒ row",
    "fn sum [const total, trail], fn sum2 [const total, trail], const k [const total], trail",
    kinds_of(
      "let sum = fun xs ->\n  let total = xs + 1 in\n  total\nin\nlet sum2(xs) =\n  let total = xs + 1 in\n  total\nin\nlet k =\n  let total = 1 + 2 in\n  total\nin\nk",
    ),
  );
  check(
    string,
    "annotated, nullary, and as module members",
    "fn f, fn g, module M [fn h, fn i], trail",
    kinds_of(
      "let f(x : Int) : Int = x in\nlet g() = 1 in\nmodule M = {\n  let h(y) = y;\n  let i = fun y -> y\n} in\nf(g())",
    ),
  );
};

let alias_kinds = () =>
  check(
    string,
    "an alias is a module",
    "module Base [type Id], module App [module Db, type UserId], trail",
    kinds_of(
      "module Base = {\n  type Id = Int\n} in\nmodule App = {\n  module Db = Base;\n  type UserId = Db.Id\n} in\nApp.Db",
    ),
  );

/* a livelit's `^name` is a row; a literal's members are below it */
let livelit_kinds = () =>
  check(
    string,
    "a livelit is a module",
    "module ^pct [type T, const init], module m [const a], trail",
    kinds_of(
      "let ^pct = {\n  type T = Int;\n  let init = 50\n} in\nlet m = { let a = 1 } in\n^pct(m.a)",
    ),
  );

let cases = [
  test_case(
    "a test added elsewhere leaves a test's path",
    `Quick,
    test_elsewhere,
  ),
  test_case("a module alias's row is a module", `Quick, alias_kinds),
  test_case("a livelit definition's row and members", `Quick, livelit_kinds),
  test_case("a definition being typed at the end has its row", `Quick, () =>
    check(
      string,
      "`let b =` at the end",
      "const a, const b, trail",
      kinds_of("let a = 1 in\nlet b ="),
    )
  ),
  test_case("a sugared function's row is a function", `Quick, sugar_kinds),
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
      db("scratch:Old:side", "p");
      db("scratch:Older", "other");
      ScratchPersist.rename_slide("scratch", "Old", "New");
      let get = Web.HazelDB.kv_get;
      check(bool, "blob moved", true, get("scratch:New") == Some("blob"));
      check(bool, "side moved", true, get("scratch:New:side") == Some("p"));
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
        get("scratch:New") == None && get("scratch:New:side") == None,
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
  switch (ids(OutlineTree.rows(e))) {
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
  switch (OutlineTree.rows(e)) {
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
    switch (OutlineTree.rows(e)) {
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
  let e = term_of_text("let a = 1 in\ntest a == 1 end;\nlet b = 2 in\nb");
  check(
    bool,
    "a run's only test: the row after its run",
    true,
    OutlineTree.successor(CorpusUtil.row_by_label(e, "1"), e)
    == Some(CorpusUtil.row_by_label(e, "b")),
  );
};

/* flipping Outline Arrows redraws the outline, all else unchanged */
let arrows_redraw = () => {
  let statics = CachedStatics.empty;
  let segment =
    Option.get(
      CorpusUtil.typed_seg(
        "let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)",
      ),
    );
  let view = (~statics=statics, arrows) =>
    Web.OutlineControl.view(
      ~arrows,
      ~deck=None,
      ~statics,
      ~segment,
      ~inject=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~inject_workspace=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~jump=_ => Virtual_dom.Vdom.Effect.Ignore,
      ~leave=Virtual_dom.Vdom.Effect.Ignore,
      ~statics_on=true,
    );
  let off = view(false);
  check(
    bool,
    "the same inputs reuse the outline",
    true,
    view(false) === off,
  );
  /* a frame's statics are a new record over the same analysis */
  check(
    bool,
    "the same analysis reuses it",
    true,
    view(
      ~statics={
        ...statics,
        warning_ids: [],
      },
      false,
    )
    === off,
  );
  check(bool, "arrows on: a new outline", false, view(true) === off);
};

/* the outline is kept across a caret move, which re-zips the tiles
   above the caret as new objects, and an edit keeps the entries below
   the definitions it didn't touch */
let outline_kept = () => {
  let seg =
    CorpusUtil.seg_of(
      "let a = 1 in\nlet b = fun x ->\n  let y = x in\n  y\nin\nb(a)",
    );
  let o = Outline.of_program(~root=Exp, seg);
  let rezip =
    List.map(
      fun
      | Piece.Tile(t) =>
        Piece.Tile({
          ...t,
          children: List.map(List.map(Fun.id), t.children),
        })
      | p => p,
      seg,
    );
  check(bool, "re-zipped: new objects", false, Segment.ptr_eq(rezip, seg));
  check(
    bool,
    "re-zipped: the same outline",
    true,
    Outline.of_program(~root=Exp, rezip) === o,
  );
  /* [a]'s definition gets a new token */
  let fresh =
    fun
    | Piece.Tile(t) =>
      Piece.Tile({
        ...t,
        id: Id.mk(),
      })
    | p => p;
  let edited =
    switch (seg) {
    | [Tile({children: [pat, def], _} as t), ...rest] => [
        Piece.Tile({
          ...t,
          children: [pat, List.map(fresh, def)],
        }),
        ...rest,
      ]
    | _ => fail("expected `let a` first")
    };
  let o' = Outline.of_program(~root=Exp, edited);
  check(bool, "edited: a new outline", false, o' === o);
  let b = (o: Outline.t) => List.nth(o.entries, 1);
  check(bool, "b has a body", true, b(o).children != []);
  check(
    bool,
    "edited: b's body entries are the same objects",
    true,
    b(o').children === b(o).children,
  );
};

/* entries kept across an edit have the paths a fresh build gives:
   removing the second `M` drops the first one's `#1` below it */
let kept_paths = () => {
  let seg =
    CorpusUtil.seg_of(
      "module M = {\n  module X = {\n    let y = 1\n  };\n  let w = 2\n} in\nmodule M = {\n  module X = {\n    let z = 1\n  }\n} in\n1",
    );
  let rec copy = (seg: Segment.t): Segment.t =>
    List.map(
      fun
      | Piece.Tile(t) =>
        Piece.Tile({
          ...t,
          children: List.map(copy, t.children),
        })
      | p => p,
      seg,
    );
  let paths = (seg: Segment.t) =>
    List.map(
      (e: Outline.entry) => Outline.path_string(e),
      Outline.all(Outline.of_program(~root=Exp, seg)),
    );
  let without_second =
    switch (seg) {
    | [m1, ...rest] =>
      let rec drop = (ps: Segment.t) =>
        switch (ps) {
        | [Piece.Tile(t), ...rest] when Tile.token(t, 0) == "module" => rest
        | [_, ...rest] => drop(rest)
        | [] => []
        };
      [m1, ...drop(rest)];
    | [] => fail("empty")
    };
  ignore(paths(seg));
  check(
    list(string),
    "the second M removed",
    paths(copy(without_second)),
    paths(without_second),
  );
  check(
    bool,
    "M/X loses its #1",
    true,
    List.mem("M/X", paths(without_second)),
  );
  check(list(string), "and back", paths(copy(seg)), paths(seg));
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
  let term = CorpusUtil.outline_of(seg);
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
  let term' = Web.Program.outline(renamed.program);
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
  let term = CorpusUtil.outline_of(seg);
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

/* runs of tests: in a module, at the top level, and a lone test's */
let runs = "let a = 1 in\nmodule M = {\n  let x = 1;\n  test x == 1 end;\n  test x == 2 end\n} in\ntest a == 1 end;\ntest a == 2 end;\nlet b = 2 in\ntest b == 2 end;\nb";

/* the rows a slide with [folds] shows, by label (the ⇒ has none) */
let shown = (~folds, e: OutlineTree.t): list(string) =>
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
    "runs folded, a lone test's too, the rest open",
    ["a", "M", "x", "tests", "tests", "b", "tests", ""],
    shown(~folds=[], e),
  );
  let top = Option.get(OutlineTree.run_of(row("3"), e));
  check(
    list(string),
    "a run opened by hand",
    ["a", "M", "x", "tests", "tests", "3", "4", "b", "tests", ""],
    shown(~folds=[(top, false)], e),
  );
  let m = Option.get(OutlineTree.label_path(row("M"), e));
  check(
    list(string),
    "a module folded by hand",
    ["a", "M", "tests", "b", "tests", ""],
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
  let e = Web.Program.outline(code.program);
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
    ["a", "M", "x", "tests", "tests", "4", "5", "b", "tests", ""],
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
    let seg = CorpusUtil.seg_of(src);
    let term = MakeTerm.go(seg).term;
    let outline = CorpusUtil.outline_of(seg);
    let (_, elab) =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Operators.default_mode)),
        term,
      );
    let (_, state) = Evaluator.evaluate(~env=Builtins.env_init, elab);
    let results = EvaluatorState.get_tests(state);
    let test_status = id =>
      TestMap.lookup(id, results) |> Option.map(TestMap.joint_status);
    switch (
      List.find_opt(
        (n: OutlineTree.node) => n.o_kind == KTests,
        OutlineTree.flatten(OutlineTree.rows(outline)),
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

/* a hinted test (`hint "…" test … end;`) joins its run as a plain one does */
let hinted_run = () => {
  let seg =
    CorpusUtil.seg_of(
      "let f = fun x -> x + 1 in\n"
      ++ "hint \"one\" test f(1) == 2 end;\n"
      ++ "test f(2) == 3 end;\n"
      ++ "hint \"two\" test f(3) == 4 end;\n"
      ++ "f(0)",
    );
  let hint =
    switch (
      List.find_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) when Tile.token(t, 0) == "hint" => Some(t.id)
          | _ => None
          },
        seg,
      )
    ) {
    | Some(id) => id
    | None => fail("no hint tile")
    };
  switch (Web.ScratchFocus.run_at(~root=Exp, hint, seg)) {
  | Some((_, _, _, (_, n))) => check(int, "tests in the run", 3, n)
  | None => fail("the hinted test is in no run")
  };
};

/* a hinted test's row goes by its hint, an unhinted one by its number
   among all the tests; in a module-rooted program's result too */
let hinted_names = () => {
  let labels = (~root=Sort.Exp, src) =>
    List.map(
      (n: OutlineTree.node) => n.o_label,
      OutlineTree.flatten(
        OutlineTree.rows(CorpusUtil.outline_of_text(~root, src)),
      ),
    );
  check(
    list(string),
    "by hint, else by number",
    ["f", "tests", "one", "2", "3", "", "tests", "4", ""],
    labels(
      "let f = fun x -> x + 1 in\n"
      ++ "hint \"one\" test f(1) == 2 end;\n"
      ++ "test f(2) == 3 end;\n"
      ++ "hint \"\" test f(3) == 4 end;\n"
      ++ "f(0);\n"
      ++ "test f(4) == 5 end;\n"
      ++ "f(0)",
    ),
  );
  check(
    list(string),
    "a module-rooted program's result",
    ["x", "tests", "x is one"],
    labels(~root=Mod, "let x = 1;\nhint \"x is one\" test x == 1 end"),
  );
  check(
    string,
    "a long hint cut short",
    {js|counts every element of…|js},
    Web.OutlineSidebar.clipped("counts every element of a long list"),
  );
  check(
    string,
    "a short one whole",
    "empty list",
    Web.OutlineSidebar.clipped("empty list"),
  );
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
    "a lone test: its run's label",
    E.Run(row("5")),
    C.shown_row(~folds=[], row("5"), e),
  );
  check(
    bool,
    "a run's label, by path: the outline's cursor there",
    true,
    C.key_path(E.Run(row("3")), e) == Some(top),
  );
};

let tests = [
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
      test_case("a hinted test joins its run", `Quick, hinted_run),
      test_case(
        "a hinted test's row is named by its hint",
        `Quick,
        hinted_names,
      ),
      test_case("the caret's mark in a folded run", `Quick, mark_in_runs),
      test_case(
        "a caret move keeps the outline, an edit its untouched entries",
        `Quick,
        outline_kept,
      ),
      test_case(
        "kept entries have a fresh build's paths",
        `Quick,
        kept_paths,
      ),
    ],
  ),
];
