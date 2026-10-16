open Alcotest;
open Haz3lcore;

/* the outline's text mode (OutlineEdit): what each key does on each kind
   of line, as data, over a real program's rows; and the saves and ops
   OutlineControl makes of them */

module E = Web.OutlineEdit;
module O = Web.OutlineTree;
module C = Web.OutlineControl;
module H = OutlineHarness;

let src = {|let alpha = 1 in
let beta = fun x ->
  let inner = x + 1 in
  inner
in
type Shape = Int in
module Geo = {
  let origin = 0;
  let unit = 1;
} in
test alpha == 1 end;
test beta(1) == 2 end;
let blank = ¿ in
alpha;
beta(alpha)|};

let term = MakeTerm.Incr.term_of(CorpusUtil.seg_of(src));
let id = CorpusUtil.row_by_label(term);

/* the program's lines, with [collapsed] rows folded and a new
   definition's line if [text] has one */
let lines = (~collapsed=[], ~text=?, ()) =>
  E.lines(~text, H.visible(~collapsed, term));

/* a line by what it shows: a name, a test's number, the "tests" and ";"
   rows, beta's "beta⇒" and the program's "⇒", a new line's "+" */
let find_line = (lines: array(E.entry), which: string): int => {
  let is = (e: E.entry) =>
    switch (e) {
    | Of_row(r) =>
      switch (which, r.r_node.o_kind) {
      | ("tests", KTests)
      | (";", KStmt)
      | ({js|⇒|js} | {js|beta⇒|js}, KTrail) => true
      | (_, KTests | KStmt | KTrail) => false
      | (l, _) => r.r_node.o_label == l
      }
    | Of_new(_) => which == "+"
    };
  let found =
    which == {js|⇒|js}
      ? Option.map(
          i => Array.length(lines) - 1 - i,
          Array.find_index(
            is,
            Array.of_list(List.rev(Array.to_list(lines))),
          ),
        )
      : Array.find_index(is, lines);
  switch (found) {
  | Some(i) => i
  | None => fail("no line " ++ which)
  };
};
let line_of = (lines, which) => E.line_of(lines[find_line(lines, which)]);

let k = (~alt=false, ~meta=false, ~shift=false, ~code=?, key): E.press => {
  key,
  code: Option.value(code, ~default=key),
  alt,
  meta,
  ctrl: false,
  shift,
  composing: false,
};

/* [key] on the line showing [which], the caret at [col] (in its field
   when [field]), what's typed [pending] */
let press =
    (
      ~collapsed=[],
      ~col=0,
      ~field=false,
      ~sel=false,
      ~pending=?,
      ~col_target=?,
      ~error=?,
      ~blank=_ => false,
      ~on=?,
      which: string,
      p: E.press,
    )
    : E.outcome => {
  let base = lines(~collapsed, ());
  let line =
    switch (on) {
    | Some(line) => line
    | None => line_of(base, which)
    };
  let t: E.t = {
    ...E.at(line, ~col, ~pending?),
    col_target,
    error,
  };
  let ls = lines(~collapsed, ~text=t, ());
  E.decide(
    ~lines=ls,
    ~at=Option.get(E.find(ls, line)),
    ~t,
    ~caret={
      in_field: field,
      c_col: col,
      c_sel: sel,
    },
    ~blank,
    ~page=() => 3,
    p,
  );
};

let outcome = testable(Fmt.of_to_string(E.show_outcome), (==));

let go = (which, col, target) => E.Go(line_of(lines(), which), col, target);

/* ↑↓ land at the same column: set by the first vertical move, kept
   across ghost lines and short names, a line's end when it's shorter */
let columns = () => {
  check(
    outcome,
    "down from alpha",
    go("beta", 3, Some(3)),
    press(~col=3, "alpha", k("ArrowDown")),
  );
  check(
    outcome,
    "the column kept, not the one landed at",
    go("inner", 5, Some(5)),
    press(~col=4, ~col_target=5, "beta", k("ArrowDown")),
  );
  check(
    outcome,
    "a ghost line: its start, the column kept",
    go({js|beta⇒|js}, 0, Some(5)),
    press(~col=5, ~col_target=5, "inner", k("ArrowDown")),
  );
  check(
    outcome,
    "on past it",
    go("Shape", 5, Some(5)),
    press(~col_target=5, {js|beta⇒|js}, k("ArrowDown")),
  );
  check(
    outcome,
    "up from the first line: its start",
    Place(0),
    press(~col=3, "alpha", k("ArrowUp")),
  );
  let last = {
    let ls = lines();
    E.line_of(ls[Array.length(ls) - 1]);
  };
  check(
    outcome,
    "⌘↓: the last line",
    Go(last, 0, None),
    press(~col=2, "alpha", k(~meta=true, "ArrowDown")),
  );
  check(
    outcome,
    "⌘↑: the first",
    go("alpha", 0, None),
    press(~col=2, "Geo", k(~meta=true, "ArrowUp")),
  );
};

/* rows folded away aren't lines */
let collapsed = () =>
  check(
    outcome,
    "down from a folded beta: Shape",
    E.Go(line_of(lines(~collapsed=["beta"], ()), "Shape"), 2, Some(2)),
    press(~collapsed=["beta"], ~col=2, "beta", k("ArrowDown")),
  );

/* ←→ move within a name and on to the neighbour lines at its ends */
let sideways = () => {
  check(
    outcome,
    "within: the field's",
    Field,
    press(~field=true, ~col=2, "alpha", k("ArrowLeft")),
  );
  check(
    outcome,
    "within, before the field has the keys",
    Place(1),
    press(~col=2, "alpha", k("ArrowLeft")),
  );
  check(
    outcome,
    "the first line's start",
    Swallow,
    press("alpha", k("ArrowLeft")),
  );
  check(
    outcome,
    "a name's end: the next line's start",
    go("beta", 0, None),
    press(~field=true, ~col=5, "alpha", k("ArrowRight")),
  );
  check(
    outcome,
    "a name's start: the line above's end",
    go("alpha", 5, None),
    press(~field=true, "beta", k("ArrowLeft")),
  );
  check(
    outcome,
    "a selection collapses first",
    Field,
    press(~field=true, ~sel=true, "beta", k("ArrowLeft")),
  );
  check(
    outcome,
    "Home in a name",
    Field,
    press(~field=true, ~col=3, "beta", k("Home")),
  );
};

/* a ghost line has one caret position: arrows step over it, typing and
   Backspace flash */
let ghosts = () => {
  check(
    outcome,
    "← to the line above's end",
    go("blank", 5, None),
    press(";", k("ArrowLeft")),
  );
  check(
    outcome,
    "→ to the next line's start",
    go({js|⇒|js}, 0, None),
    press(";", k("ArrowRight")),
  );
  check(outcome, "typing flashes", Flash, press("tests", k("x")));
  check(outcome, "so does a space", Flash, press("tests", k(" ")));
  check(outcome, "and Backspace", Flash, press("1", k("Backspace")));
  check(outcome, "and Delete", Flash, press("1", k("Delete")));
  check(outcome, "Home stays", Swallow, press({js|⇒|js}, k("Home")));
};

/* Backspace on a blank line removes it, never merging lines nor
   deleting a definition with a body */
let backspace = () => {
  let new_below = E.New(id("alpha"), Below);
  check(
    outcome,
    "a blank new line: the caret up to the line above's end",
    go("alpha", 5, None),
    press(~on=new_below, "+", k("Backspace")),
  );
  check(
    outcome,
    "a new line with a name: flashes",
    Flash,
    press(~on=new_below, ~pending="x", "+", k("Backspace")),
  );
  check(
    outcome,
    "a new line's keyword goes first",
    Edit("", 0),
    press(~on=new_below, ~pending="type ", "+", k("Backspace")),
  );
  let blank = i => i == id("blank");
  check(
    outcome,
    "an erased name over a hole: removed, the caret up",
    Remove(id("blank"), Some((line_of(lines(), "2"), 0))),
    press(~pending="", ~blank, "blank", k("Backspace")),
  );
  check(
    outcome,
    "an erased name over a body: flashes",
    Flash,
    press(~pending="", ~blank, "alpha", k("Backspace")),
  );
  check(
    outcome,
    "a name's start: flashes",
    Flash,
    press(~field=true, "alpha", k("Backspace")),
  );
  check(
    outcome,
    "inside a name: the field's",
    Field,
    press(~field=true, ~col=2, "alpha", k("Backspace")),
  );
  check(
    outcome,
    "before the field has the keys",
    Edit("apha", 1),
    press(~col=2, "alpha", k("Backspace")),
  );
  check(
    outcome,
    "⌘⌫: the item",
    Op(Delete, id("alpha")),
    press("alpha", k(~meta=true, "Backspace")),
  );
  check(
    outcome,
    "⌘⌫ on a new line: it goes",
    go("alpha", 5, None),
    press(~on=new_below, "+", k(~meta=true, "Backspace")),
  );
};

/* Enter: a new definition below a name, first inside an expanded module;
   on a ghost line, by its kind */
let enter = () => {
  let new_at = (which, place) => E.Go(New(id(which), place), 0, None);
  check(
    outcome,
    "a name: a new line below",
    new_at("alpha", Below),
    press("alpha", k("Enter")),
  );
  check(
    outcome,
    "an expanded fn: below it too",
    new_at("beta", Below),
    press("beta", k("Enter")),
  );
  check(
    outcome,
    "an expanded module: first inside",
    new_at("Geo", First),
    press("Geo", k("Enter")),
  );
  check(
    outcome,
    "a folded module: below",
    new_at("Geo", Below),
    press(~collapsed=["Geo"], "Geo", k("Enter")),
  );
  let tests =
    List.filter_map(
      (n: O.node) => n.o_kind == KTest ? n.o_id : None,
      O.flatten(O.of_term(term)),
    );
  check(
    outcome,
    "a test: one below it",
    NewTest(NewTestBelow, List.nth(tests, 0)),
    press("1", k("Enter")),
  );
  check(
    outcome,
    "tests, expanded: first of the run",
    NewTest(NewTestAbove, List.nth(tests, 0)),
    press("tests", k("Enter")),
  );
  check(
    outcome,
    "tests, folded: after the run",
    NewTest(NewTestBelow, List.nth(tests, 1)),
    press(~collapsed=["tests"], "tests", k("Enter")),
  );
  let stmt =
    List.find_map(
      (n: O.node) => n.o_kind == KStmt ? n.o_id : None,
      O.flatten(O.of_term(term)),
    );
  check(
    outcome,
    "a statement: a new definition below",
    E.Go(New(Option.get(stmt), Below), 0, None),
    press(";", k("Enter")),
  );
  check(
    outcome,
    {js|⇒: flashes|js},
    Flash,
    press({js|⇒|js}, k("Enter")),
  );
  let new_below = E.New(id("alpha"), Below);
  check(
    outcome,
    "a new line, named: saved, a next below",
    Save(Next),
    press(~on=new_below, ~pending="b", "+", k("Enter")),
  );
  check(
    outcome,
    "a blank new line: gone, text mode too",
    Save(Leave),
    press(~on=new_below, "+", k("Enter")),
  );
};

/* every way out saves; a refused name's second Esc drops it */
let leaving = () => {
  check(outcome, "Esc", Save(Leave), press("alpha", k("Escape")));
  check(
    outcome,
    "⌘Enter",
    Save(Leave),
    press("alpha", k(~meta=true, "Enter")),
  );
  check(
    outcome,
    "Shift+Enter",
    Save(Leave),
    press("1", k(~shift=true, "Enter")),
  );
  check(
    outcome,
    "Esc after a refusal",
    Abandon,
    press(~pending="Bad", ~error="no", "alpha", k("Escape")),
  );
};

/* ⌘Z undoes what's typed on the line, else it's the app's undo */
let undo = () => {
  let z = k(~meta=true, "z");
  check(outcome, "typed: undone", Revert, press(~pending="alp", "alpha", z));
  check(
    outcome,
    "typed back as it was: the app's",
    Page,
    press(~pending="alpha", "alpha", z),
  );
  check(outcome, "nothing typed: the app's", Page, press("alpha", z));
  check(outcome, "a ghost line: the app's", Page, press("1", z));
  let new_below = E.New(id("alpha"), Below);
  check(
    outcome,
    "a new line, named: its name",
    Revert,
    press(~on=new_below, ~pending="x", "+", z),
  );
  check(
    outcome,
    "a blank new line: the Enter that made it",
    go("alpha", 5, None),
    press(~on=new_below, "+", z),
  );
  check(
    outcome,
    "⌘⇧Z: the app's redo",
    Page,
    press(~pending="alp", "alpha", k(~meta=true, ~shift=true, "Z")),
  );
};

/* the item ops: Alt↑↓ moves, ⌘D duplicates, ⌘⌫ deletes, never the ⇒ */
let ops = () => {
  let alt = key => k(~alt=true, key);
  check(
    outcome,
    "Alt↑ moves",
    Op(MoveUp, id("Shape")),
    press("Shape", alt("ArrowUp")),
  );
  check(
    outcome,
    "a test moves",
    Op(MoveDown, id("1")),
    press("1", alt("ArrowDown")),
  );
  check(
    outcome,
    {js|⇒ stays last|js},
    Flash,
    press({js|⇒|js}, alt("ArrowUp")),
  );
  check(
    outcome,
    "a tests label isn't an item",
    Flash,
    press("tests", alt("ArrowUp")),
  );
  check(
    outcome,
    "nor is a new line",
    Flash,
    press(~on=New(id("alpha"), Below), "+", alt("ArrowUp")),
  );
  let d = k(~meta=true, ~code="KeyD", "d");
  check(outcome, "⌘D", Op(Duplicate, id("alpha")), press("alpha", d));
  check(outcome, "⌘D on a test", Op(Duplicate, id("2")), press("2", d));
  check(outcome, {js|⌘D on ⇒|js}, Flash, press({js|⇒|js}, d));
  check(
    outcome,
    {js|⌘⌫ on ⇒|js},
    Flash,
    press({js|⇒|js}, k(~meta=true, "Backspace")),
  );
  check(outcome, "No Tab", Swallow, press("alpha", k("Tab")));
};

/* ⌥F folds the line's row, as the editor's Fold does */
let fold = () => {
  let f = k(~alt=true, ~code="KeyF", {js|ƒ|js});
  let path = which => {
    let ls = lines();
    switch (ls[find_line(ls, which)]) {
    | Of_row(r) => r.r_path
    | Of_new(_) => fail("a row")
    };
  };
  check(outcome, "a fn", Fold(path("beta")), press("beta", f));
  check(outcome, "a tests label", Fold(path("tests")), press("tests", f));
  check(outcome, "a leaf flashes", Flash, press("alpha", f));
  check(
    outcome,
    "Alt+O leaves",
    Exit,
    press("alpha", k(~alt=true, ~code="KeyO", {js|ø|js})),
  );
};

/* keys typed before a field has them land in the name at the caret */
let early = () => {
  check(
    outcome,
    "a letter",
    Edit("alXpha", 3),
    press(~col=2, "alpha", k("X")),
  );
  check(
    outcome,
    "in the field: its own",
    Field,
    press(~field=true, ~col=2, "alpha", k("X")),
  );
  check(
    outcome,
    "a new line's keyword sets itself apart",
    Edit("type ", 0),
    press(
      ~on=New(id("alpha"), Below),
      ~pending="type",
      ~col=4,
      "+",
      k(" "),
    ),
  );
};

/* a new line sits after its anchor's rows, or first inside it */
let new_lines = () => {
  let at = (anchor, place) => {
    let ls = lines(~text=E.at(New(id(anchor), place)), ());
    find_line(ls, "+");
  };
  let base = lines();
  check(
    int,
    "below alpha",
    find_line(base, "alpha") + 1,
    at("alpha", Below),
  );
  check(
    int,
    "below beta's rows",
    find_line(base, "Shape"),
    at("beta", Below),
  );
  check(
    int,
    "first inside Geo",
    find_line(base, "Geo") + 1,
    at("Geo", First),
  );
  check(
    int,
    "last inside Geo",
    find_line(base, "unit") + 1,
    at("Geo", Last),
  );
};

/* the caret's line found by its key across a rename, by the row above it
   folded away, near where it was when it vanished */
let finding = () => {
  let base = lines();
  let stale =
    E.Row(
      [
        {
          s_label: "old_name",
          s_occ: 0,
        },
      ],
      Some(Item(id("beta"))),
    );
  check(
    option(int),
    "by key",
    Some(find_line(base, "beta")),
    E.find(base, stale),
  );
  let origin = line_of(base, "origin");
  let folded = lines(~collapsed=["Geo"], ());
  check(
    option(int),
    "folded away: its module",
    Some(find_line(folded, "Geo")),
    E.find(folded, origin),
  );
  let gone =
    E.Row(
      [
        {
          s_label: "gone",
          s_occ: 0,
        },
      ],
      Some(Item(Id.mk())),
    );
  check(
    option(int),
    "vanished: the line above where it was",
    Some(2),
    E.find(~last=3, base, gone),
  );
};

let program_of = (src: string): Web.Program.t =>
  Whole(Web.ScratchFocus.cell_of_seg(CorpusUtil.seg_of(src)));

/* a definition of only holes (or an empty module) is blank */
let blank_bodies = () => {
  let p =
    program_of(
      {|let a = ¿ in
let b = 1 in
type T = ¿ in
module E = {} in
let f = fun x -> ¿ in
module M = {
  let c = ¿;
  let d = 2
} in
0|},
    );
  let term = Web.Workspace.fresh_term(p);
  let blank = label => C.blank_body(p, CorpusUtil.row_by_label(term, label));
  List.iter(
    ((label, want)) => check(bool, label, want, blank(label)),
    [
      ("a", true),
      ("b", false),
      ("T", true),
      ("E", true),
      ("f", false),
      ("c", true),
      ("d", false),
    ],
  );
};

/* a code slide over [src], as OutlineControl edits it */
let code_of = (src: string) =>
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

let run = (action, code: Web.ScratchModel.Scratchpad.code) =>
  C.update(
    ~settings=Web.Globals.Model.init().settings,
    ~schedule_workspace=_ => (),
    ~prefix="scratch",
    ~name="t",
    action,
    code,
  ).
    model;

let text_of = (code: Web.ScratchModel.Scratchpad.code) =>
  CorpusUtil.text_of(Web.Program.document(code.program));

let caret_on = (code: Web.ScratchModel.Scratchpad.code): option(O.node) =>
  switch (C.text^) {
  | Some({line: Row(_, Some(Item(row))), _}) =>
    O.node_of(row, Web.Workspace.fresh_term(code.program))
  | _ => None
  };

let reset = () => {
  C.text := None;
  C.cursor := None;
  C.flash := 0;
};

/* a new test below a test, or first of a run: the caret on it */
let new_tests = () => {
  let code = code_of("let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\na");
  let term = Web.Workspace.fresh_term(code.program);
  let tests =
    List.filter_map(
      (n: O.node) => n.o_kind == KTest ? n.o_id : None,
      O.flatten(O.of_term(term)),
    );
  let first = List.nth(tests, 0);
  C.text :=
    Some(
      E.at(
        Row(Option.get(O.label_path(first, term)), Some(Item(first))),
      ),
    );
  let code' = run(DefOp(NewTestBelow, first), code);
  check(
    string,
    "a test below the first",
    CorpusUtil.text_of(
      CorpusUtil.seg_of(
        {js|let a = 1 in
test a == 1 end;
test ¿ end;
test a > 0 end;
a|js},
      ),
    ),
    text_of(code'),
  );
  check(
    option(string),
    "the caret on the new test, the second",
    Some("2"),
    Option.map((n: O.node) => n.o_label, caret_on(code')),
  );
  C.text := None;
  C.cursor := None;
  let code'' = run(DefOp(NewTestAbove, first), code);
  check(
    bool,
    "row mode: the cursor on the first of the run",
    true,
    C.cursor^
    == O.label_path(
         List.hd(
           List.filter_map(
             (n: O.node) => n.o_kind == KTest ? n.o_id : None,
             O.flatten(O.of_term(Web.Workspace.fresh_term(code''.program))),
           ),
         ),
         Web.Workspace.fresh_term(code''.program),
       ),
  );
  reset();
};

/* Enter on an expanded module names its new first member */
let first_member = () => {
  let code = code_of("module M = {\n  let a = 1;\n  let b = 2\n} in\nM.a");
  let m =
    CorpusUtil.row_by_label(Web.Workspace.fresh_term(code.program), "M");
  let line = E.New(m, First);
  C.text := Some(E.at(line));
  let code' =
    run(
      Save(
        {
          sv_line: line,
          sv_text: "zero",
          sv_col: 4,
        },
        Next,
      ),
      code,
    );
  check(
    string,
    "zero first in M",
    CorpusUtil.text_of(
      CorpusUtil.seg_of(
        {js|module M = {
  let zero = ¿;
  let a = 1;
  let b = 2
} in
M.a|js},
      ),
    ),
    text_of(code'),
  );
  let zero =
    CorpusUtil.row_by_label(Web.Workspace.fresh_term(code'.program), "zero");
  check(
    bool,
    "a new line below it",
    true,
    Option.map((t: E.t) => t.line, C.text^) == Some(New(zero, Below)),
  );
  reset();
};

/* ⌘D puts the caret in the copy; ⌘⌫ on the next row; a refused op
   flashes */
let copy_and_delete = () => {
  let code = code_of("let a = 1 in\nlet b = 2 in\na + b");
  let term = Web.Workspace.fresh_term(code.program);
  let row = CorpusUtil.row_by_label(term);
  let on = label =>
    C.text :=
      Some({
        ...
          E.at(
            Row(
              Option.get(O.label_path(row(label), term)),
              Some(Item(row(label))),
            ),
            ~col=1,
          ),
        pending: None,
      });
  on("a");
  let copied = run(DefOp(Duplicate, row("a")), code);
  let caret =
    switch (C.text^) {
    | Some({line: Row(_, Some(Item(id))), col, _}) => Some((id, col))
    | _ => None
    };
  check(
    bool,
    "the caret in the copy, its column kept",
    true,
    switch (caret) {
    | Some((id, 1)) =>
      id != row("a")
      && O.node_of(id, Web.Workspace.fresh_term(copied.program))
      |> Option.map((n: O.node) => n.o_label) == Some("a")
    | _ => false
    },
  );
  on("a");
  let deleted = run(DefOp(Delete, row("a")), code);
  check(
    option(string),
    "deleted: the caret on the next row",
    Some("b"),
    Option.map((n: O.node) => n.o_label, caret_on(deleted)),
  );
  on("a");
  ignore(run(DefOp(MoveUp, row("a")), code));
  check(int, "a refused move flashes", 1, C.flash^);
  reset();
};

let tests = (
  "OutlineText",
  [
    test_case("↑↓ keep the column", `Quick, columns),
    test_case("folded rows aren't lines", `Quick, collapsed),
    test_case("←→ cross lines at a name's ends", `Quick, sideways),
    test_case("ghost lines", `Quick, ghosts),
    test_case("Backspace on blank lines", `Quick, backspace),
    test_case("Enter on each kind of line", `Quick, enter),
    test_case("ways out", `Quick, leaving),
    test_case("⌘Z", `Quick, undo),
    test_case("item ops", `Quick, ops),
    test_case("⌥F folds", `Quick, fold),
    test_case("keys before the field has them", `Quick, early),
    test_case("new lines' places", `Quick, new_lines),
    test_case("finding the caret's line", `Quick, finding),
    test_case("blank definitions", `Quick, blank_bodies),
    test_case("new tests take the caret", `Quick, new_tests),
    test_case("a module's new first member", `Quick, first_member),
    test_case("copy and delete move the caret", `Quick, copy_and_delete),
  ],
);
