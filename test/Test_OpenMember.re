open Alcotest;
open Web;

/* `let q = (1 + 2` typed after List.length's `end;` in the List module at
   a Mod root. Read raw, its open `(` takes every member below it into one
   item: the undo's analysis of that item overflowed the browser's stack,
   the outline's ops on it took the rest of the module along, and the undo
   dropped pins to the members it swallowed. Every path reads the members
   per item, as the editor's statics do. Here List to `cons` (short_src):
   the members below are 15, not 64 */

module Cells = Test_Undo.Cells;

/* the audit's copy (a stdlib tree's List.hz may differ); at an Exp root,
   its module bound by `in` */
let list_hz = () =>
  Option.get(
    Test_ModAudit.read_any([
      "test/modaudit/List.hz",
      "../test/modaudit/List.hz",
    ]),
  );
let at_root = (~root: Haz3lcore.Sort.t, list: string) =>
  root == Mod ? list : String.trim(list) ++ " in\n\nList.length([1, 2, 3])";
let src = (~root: Haz3lcore.Sort.t) => at_root(~root, list_hz());

/* List to `cons`, 16 of its 65 members: for what an edit costs per
   member, where List's size isn't the point (the statics check costs
   ~16x more on all 65) */
let short_src = (~root: Haz3lcore.Sort.t) => {
  let list = list_hz();
  let head =
    Str.search_forward(Str.regexp_string(";\n\n  let head "), list, 0);
  at_root(~root, String.sub(list, 0, head) ++ "\n}");
};

let mk = (~root: Haz3lcore.Sort.t, src: string): History.Model.t => {
  let editor =
    CorpusUtil.seg_of(~root, src)
    |> Haz3lcore.Zipper.unzip(~direction=Left)
    |> Haz3lcore.Editor.Model.mk(~root)
    |> CellEditor.Model.mk;
  let editors: Editors.Model.t =
    Scratch(
      ScratchModel.Model.{
        current: 0,
        scratchpads: [
          ScratchModel.Scratchpad.mk_code(~name="List", ~editor, ()),
        ],
      },
    );
  {
    current: {
      globals: Globals.Model.init(),
      editors,
      explain_this: ExplainThisModel.init,
      selection: Editors.Selection.default_selection(editors),
    },
    undo_stack: [],
    redo_stack: [],
    tentative_redo: None,
  };
};

/* the debounced statics, now */
let refresh = m => Cells.run(m, Editors(Scratch(RefreshStatics)));

let type_ = (m, s: string) =>
  List.fold_left(
    (m, i) => Cells.run(m, Test_Undo.insert(String.sub(s, i, 1))),
    m,
    List.init(String.length(s), i => i),
  );

/* the end of [needle]'s first line in [text] */
let after = (text: string, needle: string): Util.Point.t =>
  Option.get(
    Test_ModAudit.point_of(~off=String.length(needle), text, needle),
  );

/* the caret after List.length's `end;` */
let loaded = (~root=Haz3lcore.Sort.Mod, ()) => {
  let src = short_src(~root);
  let m = Cells.run(mk(~root, src), Refresh);
  Cells.run(
    m,
    Globals(ActiveEditor(Move(Point(after(src, "    end;"), None)))),
  );
};

let typed = "\nlet q = (1 + 2";
/* models are values: the tests share one per root */
let with_q = {
  let at_mod = lazy(refresh(type_(loaded(~root=Mod, ()), typed)));
  let at_exp = lazy(refresh(type_(loaded(~root=Exp, ()), typed)));
  (~root=Haz3lcore.Sort.Mod, ()) =>
    Lazy.force(root == Mod ? at_mod : at_exp);
};

let term = m => Program.statics(Cells.program(m)).term;
let row = (m, path) => CorpusUtil.row(term(m), path);
let doc = m =>
  Haz3lcore.Printer.of_segment(
    ~holes="?",
    ~refractors=[],
    Program.document(Cells.program(m)),
  );

/* List's rows in [term]'s outline */
let rows = (term: Language.Exp.t): list(string) =>
  switch (
    List.find_opt(
      (n: OutlineTree.node) => n.o_label == "List",
      OutlineTree.of_term(term),
    )
  ) {
  | Some(n) => List.map((n: OutlineTree.node) => n.o_label, n.o_children)
  | None => []
  };

/* each member its own row, as the editor's statics show them with q */
let per_member =
    (~root=Haz3lcore.Sort.Mod, what: string, term: Language.Exp.t) => {
  let want = rows(Program.statics(Cells.program(with_q(~root, ()))).term);
  check(
    bool,
    "q and the members below it are rows",
    true,
    List.mem("q", want) && List.mem("map", want),
  );
  check(list(string), what ++ ": a row per member", want, rows(term));
};

/* the module's member items in the analysis last run on [term]'s
   document */
let member_items = (term: Language.Exp.t): option(int) =>
  Option.bind(
    Haz3lcore.DefStatics.cached(term), (ds: Haz3lcore.DefStatics.t) =>
    List.find_map(
      (it: Haz3lcore.DefStatics.item) =>
        it.d_members == [] ? None : Some(List.length(it.d_members)),
      ds.items,
    )
  );

/* the undone program's analysis (the debounce's, as after a key) and
   the outline drawn from it read the members per item */
let undo_redo = (~root, ()) => {
  let m = with_q(~root, ());
  let typed_text = Test_Undo.text_of(m);
  let m = refresh(Cells.run(m, Test_Undo.undo));
  check(bool, "the key undone", true, Test_Undo.text_of(m) != typed_text);
  let term = OutlineControl.outline_term(Cells.program(m));
  per_member(~root, "undo", term);
  check(
    option(int),
    "the analysis: an item per member, and the exports",
    Some(List.length(rows(term)) + 1),
    member_items(term),
  );
  let m = refresh(Cells.run(m, Test_Undo.redo));
  check(string, "the key redone", typed_text, Test_Undo.text_of(m));
  per_member(~root, "redo", OutlineControl.outline_term(Cells.program(m)));
};

let pin_kept = () => {
  let m = with_q();
  let map = row(m, ["List", "map"]);
  let m = refresh(Cells.run(m, Cells.workspace(FocusToggle(map))));
  let is_open = m =>
    switch (Cells.program(m)) {
    | Divided(d) => Divided.cell(map, d) != None
    | Whole(_) => false
    };
  check(bool, "map's cell opens", true, is_open(m));
  let body = Cells.body(m, map);
  let m = Cells.run(m, Test_Undo.insert("0"));
  check(bool, "typing reaches it", true, Cells.body(m, map) != body);
  let m = Cells.run(m, Test_Undo.undo);
  check(bool, "undo keeps it open", true, is_open(m));
  check(string, "and undoes its edit", body, Cells.body(m, map));
};

/* a divided program's statics read its document: here a zoomed module's
   members cell holds the open form */
let zoomed = () => {
  let m = refresh(loaded());
  let list = row(m, ["List"]);
  let m = Cells.run(m, Cells.workspace(ZoomIn(list)));
  let to_ = after(Cells.body(m, list), "  end;");
  let m =
    Cells.run(
      m,
      Cells.workspace(
        Stack(list, Body, MainEditor(Perform(Move(Point(to_, None))))),
      ),
    );
  let m = refresh(type_(m, typed));
  check(
    bool,
    "q follows List.length",
    true,
    CorpusUtil.contains("end;\n  let q = (1 + 2", doc(m)),
  );
  per_member("zoomed", term(m));
};

/* an op takes an item whole: the open form's would take the members below
   it, so nothing moves it, or anything it runs into */
let ops_refuse = () => {
  let m = with_q();
  let before = doc(m);
  let op = (o, name) =>
    doc(
      Cells.run(
        m,
        Editors(Scratch(Outline(DefOp(o, row(m, ["List", name]))))),
      ),
    );
  List.iter(
    ((what, o, name)) =>
      check(string, what ++ " refuses", before, op(o, name)),
    [
      ("deleting q", OutlineEdit.Delete, "q"),
      ("duplicating q", Duplicate, "q"),
      ("moving q up", MoveUp, "q"),
      ("deleting a member below it", Delete, "filter"),
      ("moving one below it up", MoveUp, "map"),
      ("moving one below it down", MoveDown, "map"),
      ("moving the one above it down", MoveDown, "length"),
    ],
  );
  check(
    bool,
    "the member above still deletes",
    true,
    op(Delete, "length") != before,
  );
};

/* the app's loop (Main.apply): each action through the crash handler,
   then what it scheduled */
let rec app = (m: CrashHandling.Model.t, a: Page.Update.t) => {
  let scheduled = ref([]);
  let schedule_action = a => scheduled := scheduled^ @ [a];
  let u =
    CrashHandling.Update.update(
      ~import_log=_ => (),
      ~get_log_and=_ => (),
      ~schedule_action,
      a,
      m,
    );
  let m =
    CrashHandling.Update.calculate(
      ~schedule_action,
      ~is_edited=u.is_edit,
      ~dynamics=false,
      m,
      u.model,
    );
  List.fold_left(app, m, scheduled^);
};

let text_of_app = (m: CrashHandling.Model.t) =>
  Test_Undo.text_of(m.model.current);

/* the overflow, should one come back: the crash screen keeps the program
   and holds the autosave; after "Revert to previous state" the save
   writes the program whole */
let crash_saves_whole = () => {
  let m = refresh(type_(loaded(), "\nlet q = (1 + "));
  let undone = Test_Undo.text_of(m);
  let h = refresh(type_(m, "2"));
  let m: CrashHandling.Model.t = {
    model: {
      current: h,
      future_log: [],
      past_log: [],
      replay_messages: [],
      replay_toggle: false,
    },
  };
  let db = HazelDB.cache^;
  let hook = Haz3lcore.DefStatics.after_calc^;
  let ck = ScratchPersist.content_key("scratch", "List");
  Fun.protect(
    ~finally=
      () => {
        HazelDB.cache := db;
        Haz3lcore.DefStatics.after_calc := hook;
        ScratchPersist.forget_gates(ck);
        CrashHandling.clear_last_exception();
        CrashHandling.clear_current_exception();
      },
    () => {
      Haz3lcore.DefStatics.after_calc :=
        Some((_, _) => raise(Stack_overflow));
      /* the undo shows the live statics: the debounce analyzes */
      let m =
        app(app(m, Globals(Undo)), Editors(Scratch(RefreshStatics)));
      Haz3lcore.DefStatics.after_calc := hook;
      check(
        bool,
        "the undone program's analysis crashed",
        true,
        CrashHandling.current_exception^ != None,
      );
      check(string, "the program is the undone one", undone, text_of_app(m));
      let writes = HazelDB.write_transactions^;
      let m = app(m, Save);
      check(
        int,
        "no save under the crash screen",
        writes,
        HazelDB.write_transactions^,
      );
      let m = app(m, Globals(ClearException));
      check(
        bool,
        "revert clears it",
        true,
        CrashHandling.current_exception^ == None,
      );
      let m = app(m, Save);
      check(
        bool,
        "then it saves",
        true,
        HazelDB.write_transactions^ > writes,
      );
      let saved =
        switch (
          ScratchPersist.load_scratchpad(
            ~settings=Language.CoreSettings.on,
            "scratch",
            "List",
          ).
            kind
        ) {
        | Code({program, _}) =>
          Program.whole(program).editor.editor.state.zipper
          |> Haz3lcore.Zipper.zip
          |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[])
        | Drv(_) => fail("a derivation slide")
        };
      check(string, "the saved program is whole", text_of_app(m), saved);
    },
  );
};

let tests = (
  "OpenMember",
  [
    test_case(
      "undo and redo read the members per item",
      `Quick,
      undo_redo(~root=Mod),
    ),
    test_case(
      "the same at an Exp root, the module bound by `in`",
      `Quick,
      undo_redo(~root=Exp),
    ),
    test_case("undo keeps a pin below the open form", `Quick, pin_kept),
    test_case("a zoomed module's statics, per item", `Quick, zoomed),
    test_case("outline ops refuse the open form's item", `Quick, ops_refuse),
    test_case(
      "a crash in the undo's analysis saves the program whole",
      `Quick,
      crash_saves_whole,
    ),
  ],
);
