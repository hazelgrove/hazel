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

let cases = [
  test_case(
    "a test added elsewhere leaves a test's path",
    `Quick,
    test_elsewhere,
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

/* a name saved by a key or click that also moved the cursor (↑ while
   renaming) leaves the cursor where it went; a plain save puts it on the
   row it named */
let commit_cursor = () => {
  module C = Web.OutlineControl;
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
  let commit = (ed, after) =>
    C.update(
      ~settings,
      ~schedule_workspace=_ => (),
      ~prefix="scratch",
      ~name="t",
      Commit(ed, after),
      code,
    ).
      model;
  let at_a = OutlineTree.label_path(row("a"), term);
  let rename = Web.OutlineEdit.mk(~row=row("b"), "c");
  C.cursor := at_a;
  ignore(commit(rename, MoveTo(at_a)));
  check(bool, "↑ renaming: the cursor stays up", true, C.cursor^ == at_a);
  let renamed = commit(rename, Stay);
  check(
    bool,
    "a plain rename: the cursor on the renamed row",
    true,
    C.cursor^
    == OutlineTree.label_path(
         row("b"),
         Web.Program.statics(renamed.program).term,
       )
    && C.cursor^ != at_a,
  );
  let new_type = Web.OutlineEdit.mk(~anchor=row("a"), "type T");
  C.cursor := at_a;
  C.created := None;
  ignore(commit(new_type, MoveTo(at_a)));
  check(
    bool,
    "↓ naming a type: the cursor stays, no keyword animates",
    true,
    C.cursor^ == at_a && C.created^ == None,
  );
  ignore(commit(new_type, Stay));
  check(
    bool,
    "a plain save: the cursor on the new type, its keyword animating",
    true,
    C.cursor^ != at_a && C.created^ != None,
  );
  C.cursor := None;
  C.created := None;
  C.edit := None;
};

/* the outline's keys over [src]'s rows, every row shown, the cell code
   deciding which open; the handlers record the cells asked for and the
   name opened for typing */
let sidebar = (src: string) => {
  module S = Web.OutlineSidebar;
  let seg = CorpusUtil.seg_of(src);
  let term = MakeTerm.Incr.term_of(seg);
  let refused = Web.ScratchFocus.refusals(~want=_ => true, seg);
  let rows = OutlineTree.flatten(OutlineTree.of_term(term));
  let path = (n: OutlineTree.node) =>
    Option.get(Option.bind(n.o_id, id => OutlineTree.label_path(id, term)));
  let visible =
    Array.of_list(
      List.map(
        (n: OutlineTree.node): S.visible_row =>
          {
            r_path: path(n),
            r_node: n,
            r_name: n.o_label,
            r_parent: None,
            r_expanded: n.o_children != [],
          },
        rows,
      ),
    );
  let cursor = ref(None);
  let toggled = ref([]);
  let typing = ref(None);
  let none = _ => Virtual_dom.Vdom.Effect.Ignore;
  let on: S.handlers = {
    jump: none,
    focus: none,
    toggle: id => {
      toggled := [id, ...toggled^];
      Virtual_dom.Vdom.Effect.Ignore;
    },
    toggle_run: none,
    toggle_collapse: none,
    zoom_to: none,
    zoom_in: none,
    show_whole: none,
    discard: Virtual_dom.Vdom.Effect.Ignore,
    zoom_out: Virtual_dom.Vdom.Effect.Ignore,
    get_cursor: () => cursor^,
    set_cursor: c => {
      cursor := c;
      Virtual_dom.Vdom.Effect.Ignore;
    },
    focused: Virtual_dom.Vdom.Effect.Ignore,
    leave: Virtual_dom.Vdom.Effect.Ignore,
    edit: {
      current: None,
      get: () => typing^,
      set: e => {
        typing := e;
        Virtual_dom.Vdom.Effect.Ignore;
      },
      commit: (_, _) => Virtual_dom.Vdom.Effect.Ignore,
    },
    menu_open: (_, _, _, _) => Virtual_dom.Vdom.Effect.Ignore,
    menu_close: Virtual_dom.Vdom.Effect.Ignore,
    menu_select: none,
    get_menu: () => None,
    get_menu_sel: () => 0,
    def_op: (_, _) => Virtual_dom.Vdom.Effect.Ignore,
  };
  let props: S.props = {
    arrows: true,
    stack_controls: true,
    refusal: id => Id.Map.find_opt(id, refused),
    is_collapsed: _ => false,
    header: {
      h_program: "",
      h_trail: [],
      h_open: 0,
      h_parked: 0,
    },
    zoom_root: None,
    cursor: None,
    created: None,
    pinned: [],
    focused_entries: [],
    error_items: [],
    error_subtree: [],
    menu: None,
    menu_sel: 0,
    test_status: _ => None,
  };
  let row = label =>
    Array.find_opt((r: S.visible_row) => r.r_node.o_label == label, visible);
  /* [key] pressed with the cursor on [label]'s row: the cells it asked
     to open or close, and the name open after */
  let press = (key: string, label: string) => {
    cursor := Option.map((r: S.visible_row) => r.r_path, row(label));
    toggled := [];
    typing := None;
    let evt =
      Js_of_ocaml.Js.(
        Unsafe.coerce(
          Unsafe.obj([|
            ("key", Unsafe.inject(string(key))),
            ("code", Unsafe.inject(string(key))),
            ("metaKey", Unsafe.inject(_false)),
            ("ctrlKey", Unsafe.inject(_false)),
            ("altKey", Unsafe.inject(_false)),
            ("shiftKey", Unsafe.inject(_false)),
          |]),
        )
      );
    ignore(S.keys(~props, ~on, ~visible, evt));
    (toggled^, typing^);
  };
  /* a double-click on [label]'s name, [typing] already open: the name
     open after */
  let double_click = (~typing as t=None, label: string) => {
    typing := t;
    let r = Option.get(row(label));
    ignore(S.start_rename(~on, r.r_node, r.r_name));
    typing^;
  };
  (term, press, double_click);
};

/* Space opens a row's cell, but not under a projector: the cell code
   refuses those rows, and the outline offers nothing */
let space_under_projector = () => {
  let (term, press, _) = sidebar("let a = 1 in\n^^fold((let b = 2 in\nb))");
  let row = CorpusUtil.row_by_label(term);
  check(bool, "a opens", true, fst(press(" ", "a")) == [row("a")]);
  check(bool, "b, folded away, doesn't", true, fst(press(" ", "b")) == []);
};

/* a double-click on a name opens it for typing as F2 does; a test or a
   statement has no name of its own, and an open name stays */
let double_click_names = () => {
  let (term, press, double_click) =
    sidebar("let a = 1 in\nlet b = 2 in\ntest a == 1 end;\na + b;\na");
  let typing = label =>
    Some(
      Web.OutlineEdit.mk(~row=CorpusUtil.row_by_label(term, label), label),
    );
  check(
    bool,
    "F2 opens a's name",
    true,
    snd(press("F2", "a")) == typing("a"),
  );
  check(
    bool,
    "so does a double-click",
    true,
    double_click("a") == typing("a"),
  );
  check(bool, "not a test's", true, double_click("1") == None);
  check(bool, "nor a statement's", true, double_click("") == None);
  check(
    bool,
    "b's name, being typed, stays",
    true,
    double_click(~typing=typing("b"), "a") == typing("b"),
  );
};

let tests = [
  (
    "OutlinePaths",
    cases
    @ [
      test_case("a deleted row's successor", `Quick, successors),
      test_case("Outline Arrows redraws the outline", `Quick, arrows_redraw),
      test_case(
        "where a saved name leaves the cursor",
        `Quick,
        commit_cursor,
      ),
      test_case(
        "Space does nothing under a projector",
        `Quick,
        space_under_projector,
      ),
      test_case(
        "a double-click on a name renames, as F2 does",
        `Quick,
        double_click_names,
      ),
    ],
  ),
];
