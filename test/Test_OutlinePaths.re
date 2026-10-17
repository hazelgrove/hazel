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
  let refused = Web.ScratchFocus.refusals(~want=_ => true, seg);
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
    ],
  ),
];
