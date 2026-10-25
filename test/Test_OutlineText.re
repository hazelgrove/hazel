open Alcotest;
open Haz3lcore;

/* the outline's text mode (OutlineEdit): what each key does on each kind
   of line, as data, over a real program's rows; and the saves, ops and
   new definitions OutlineControl makes of them */

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

let term = CorpusUtil.outline_of(CorpusUtil.seg_of(src));
let id = CorpusUtil.row_by_label(term);

/* the program's lines ([of_]: another's), with [collapsed] rows folded
   (or as a slide with [folds] folds them) and a new definition's line if
   [text] has one */
let lines = (~of_=term, ~collapsed=[], ~folds=?, ~text=?, ()) =>
  E.lines(~text, H.visible(~collapsed, ~folds?, of_));

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
   when [field]), what's typed [pending]; on a new definition's line, its
   definition [fresh] once in and its name [live] as drawn */
let press =
    (
      ~of_=term,
      ~collapsed=[],
      ~folds=?,
      ~col=0,
      ~field=false,
      ~sel=false,
      ~pending=?,
      ~col_target=?,
      ~error=?,
      ~blank=_ => false,
      ~fresh=?,
      ~live=?,
      ~on=?,
      which: string,
      p: E.press,
    )
    : E.outcome => {
  let base = lines(~of_, ~collapsed, ~folds?, ());
  let line =
    switch (on) {
    | Some(line) => line
    | None => line_of(base, which)
    };
  let t: E.t = {
    ...E.at(line, ~col, ~pending?, ~fresh?),
    col_target,
    error,
  };
  let ls = lines(~of_, ~collapsed, ~folds?, ~text=t, ());
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
    ~live?,
    p,
  );
};

/* a new definition's name as the program has it, the caret in it and the
   selection's other end, if anything is selected */
let named = (~anchor=?, l_name, l_at): E.live => {
  l_name,
  l_at,
  l_anchor: Option.value(anchor, ~default=l_at),
};
let fresh = (~kw="", ()): E.fresh => {
  f_id: Id.mk(),
  f_kw: kw,
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
    "a new line's name: a character, in the program",
    Type(Erase(Left, Char)),
    press(~on=new_below, ~live=named("x", 1), "+", k("Backspace")),
  );
  check(
    outcome,
    "a named new line's start: the editor's to refuse",
    Type(Erase(Left, Char)),
    press(~on=new_below, ~live=named("x", 0), "+", k("Backspace")),
  );
  check(
    outcome,
    "a new type's keyword goes first",
    Rekind(""),
    press(
      ~on=new_below,
      ~fresh=fresh(~kw="type ", ()),
      ~live=named("", 0),
      "+",
      k("Backspace"),
    ),
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
  let f = fresh();
  check(
    outcome,
    "⌘⌫ on a named new line: its definition too",
    Drop(f.f_id, Some((line_of(lines(), "alpha"), 5))),
    press(
      ~on=new_below,
      ~fresh=f,
      ~live=named("x", 1),
      "+",
      k(~meta=true, "Backspace"),
    ),
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
      O.flatten(O.rows(term)),
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
      O.flatten(O.rows(term)),
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
    "a new line, named: a next below",
    Save(Next),
    press(~on=new_below, ~live=named("b", 1), "+", k("Enter")),
  );
  check(
    outcome,
    "a blank new line: gone, text mode too",
    Save(Leave),
    press(~on=new_below, "+", k("Enter")),
  );
};

/* every way out saves; Esc leaves whatever the save's outcome, in one
   press */
let leaving = () => {
  check(outcome, "Esc", Save(Done), press("alpha", k("Escape")));
  check(
    outcome,
    "Esc with a refusal shown: one press still",
    Save(Done),
    press(~pending="Bad", ~error="no", "alpha", k("Escape")),
  );
  check(
    outcome,
    "Esc on a new line",
    Save(Done),
    press(
      ~on=New(id("alpha"), Below),
      ~fresh=fresh(),
      ~live=named("B", 1),
      ~error="no",
      "+",
      k("Escape"),
    ),
  );
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
};

/* a kind keyword typed first in a rename moves out of the field, shown
   apart; Backspace at the field's start takes it back off */
let rename_keywords = () => {
  check(
    pair(string, string),
    "split",
    ("module ", "M"),
    E.split_typed("module M"),
  );
  check(
    pair(string, string),
    "a value's",
    ("let ", "x"),
    E.split_typed("let x"),
  );
  check(
    pair(string, string),
    "no space yet",
    ("", "type"),
    E.split_typed("type"),
  );
  /* not underlined before its space; still refused if saved as a name */
  check(
    bool,
    "a kind keyword waits for its space",
    true,
    E.kind_pending("type"),
  );
  check(bool, "so does `let`", true, E.kind_pending("let"));
  check(bool, "a longer word is a name", false, E.kind_pending("types"));
  check(bool, "other keywords aren't kinds", false, E.kind_pending("fun"));
  check(
    pair(string, int),
    "a keyword's space typed at the field's end: the caret to its start",
    ("module ", 0),
    E.retyped("", "module ", 7),
  );
  check(
    pair(string, int),
    "under a keyword, the field's caret as typed",
    ("type Ab", 2),
    E.retyped("type ", "Ab", 2),
  );
  check(
    outcome,
    "a space after `module`, before the field has the keys",
    Edit("module ", 0),
    press(~pending="module", ~col=6, "alpha", k(" ")),
  );
  check(
    outcome,
    "a letter under the keyword",
    Edit("module Mx", 2),
    press(~pending="module M", ~col=1, "alpha", k("x")),
  );
  check(
    outcome,
    "Backspace at the field's start: the keyword goes",
    Edit("M", 0),
    press(~field=true, ~pending="module M", ~col=0, "alpha", k("Backspace")),
  );
  check(
    outcome,
    "inside the name: the field's",
    Field,
    press(~field=true, ~pending="module M", ~col=1, "alpha", k("Backspace")),
  );
  check(
    outcome,
    "End: the field's end, not the text's",
    Place(1),
    press(~pending="module M", "alpha", k("End")),
  );
  check(
    outcome,
    "⌘Z: the keyword undone too",
    Revert,
    press(~pending="module alpha", "alpha", k(~meta=true, "z")),
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
    "a new line: the page's, which takes its definition, the caret up",
    Undo(line_of(lines(), "alpha"), 5),
    press(~on=new_below, ~fresh=fresh(), ~live=named("x", 1), "+", z),
  );
  check(
    outcome,
    "its definition not in yet: the Enter that made it",
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
};

let tab = k("Tab");
let shift_tab = k(~shift=true, "Tab");

/* Tab: an item into the module above it, or the value (a `let`) above
   it, last; ⇧Tab: a module's or a value's last item out. Anything else
   flashes */
let nesting = () => {
  let tests =
    List.filter_map(
      (n: O.node) => n.o_kind == KTest ? n.o_id : None,
      O.flatten(O.rows(term)),
    );
  let semi =
    List.find_map(
      (n: O.node) => n.o_kind == KStmt ? n.o_id : None,
      O.flatten(O.rows(term)),
    )
    |> Option.get;
  List.iter(
    ((label, want, got)) => check(outcome, label, want, got),
    [
      (
        "Tab: a run's first test into the module above it",
        E.Op(Indent, List.nth(tests, 0)),
        press("1", tab),
      ),
      (
        "into a folded one too",
        Op(Indent, List.nth(tests, 0)),
        press(~collapsed=["Geo"], "1", tab),
      ),
      (
        "a value into the value above",
        Op(Indent, id("beta")),
        press("beta", tab),
      ),
      ("a statement", Op(Indent, semi), press(";", tab)),
      (
        "a member into the member above",
        Op(Indent, id("unit")),
        press("unit", tab),
      ),
      ("nothing above: flashes", Flash, press("alpha", tab)),
      ("first in a body", Flash, press("inner", tab)),
      ("a type above", Flash, press("Geo", tab)),
      ("a test above", Flash, press("2", tab)),
      ("a tests label isn't an item", Flash, press("tests", tab)),
      ({js|⇒|js}, Flash, press({js|⇒|js}, tab)),
      (
        {js|⇧Tab: a module's last member out|js},
        Op(Outdent, id("unit")),
        press("unit", shift_tab),
      ),
      (
        "a value's last local definition, above its ⇒",
        Op(Outdent, id("inner")),
        press("inner", shift_tab),
      ),
      ("a member that isn't last", Flash, press("origin", shift_tab)),
      ("the top level", Flash, press("alpha", shift_tab)),
      ("a top-level test", Flash, press("2", shift_tab)),
      ({js|a body's ⇒|js}, Flash, press({js|beta⇒|js}, shift_tab)),
      ({js|⌥Tab: nothing|js}, Swallow, press("1", k(~alt=true, "Tab"))),
    ],
  );
};

/* a new definition's line: Tab takes it into a module above it, last;
   ⇧Tab, or Enter on it blank, out of a module or value it's the last
   line of, just after it. Enter on a blank line anywhere else leaves */
let nesting_lines = () => {
  let at = (~name="", line, key) =>
    press(
      ~on=line,
      ~fresh=fresh(),
      ~live=named(name, E.units(name)),
      "+",
      key,
    );
  let enter = k("Enter");
  List.iter(
    ((label, want, got)) => check(outcome, label, want, got),
    [
      (
        "below a module: Tab takes it in, last",
        E.Shift(New(id("Geo"), Last)),
        at(New(id("Geo"), Below), tab),
      ),
      (
        "named too",
        Shift(New(id("Geo"), Last)),
        at(~name="z", New(id("Geo"), Below), tab),
      ),
      (
        "below a value: into its body",
        Shift(New(id("alpha"), Last)),
        at(New(id("alpha"), Below), tab),
      ),
      (
        {js|below a function: inside it, above its ⇒|js},
        Shift(New(id("beta"), Last)),
        at(New(id("beta"), Below), tab),
      ),
      ("below a type: flashes", Flash, at(New(id("Shape"), Below), tab)),
      (
        {js|last in a function, above its ⇒: ⇧Tab takes it out|js},
        Shift(New(id("beta"), Below)),
        at(New(id("beta"), Last), shift_tab),
      ),
      (
        "and Enter, blank",
        Shift(New(id("beta"), Below)),
        at(New(id("beta"), Last), k("Enter")),
      ),
      (
        "first inside a module: nothing above",
        Flash,
        at(New(id("Geo"), First), tab),
      ),
      (
        {js|last in a module: ⇧Tab takes it out, below the module|js},
        Shift(New(id("Geo"), Below)),
        at(New(id("unit"), Below), shift_tab),
      ),
      (
        "last inside it: the same",
        Shift(New(id("Geo"), Below)),
        at(~name="z", New(id("Geo"), Last), shift_tab),
      ),
      (
        "first inside a module with members",
        Flash,
        at(New(id("Geo"), First), shift_tab),
      ),
      (
        "last in a value's body",
        Shift(New(id("beta"), Below)),
        at(New(id("inner"), Below), shift_tab),
      ),
      ("the top level", Flash, at(New(id("alpha"), Below), shift_tab)),
      (
        "Enter, blank, last in a module: out",
        Shift(New(id("Geo"), Below)),
        at(New(id("unit"), Below), enter),
      ),
      (
        "last in a value's body: out",
        Shift(New(id("beta"), Below)),
        at(New(id("inner"), Below), enter),
      ),
      (
        "mid-module: gone, text mode too",
        Save(Leave),
        at(New(id("origin"), Below), enter),
      ),
      (
        "the top level: gone",
        Save(Leave),
        at(New(id("alpha"), Below), enter),
      ),
      (
        "named: the next line",
        Save(Next),
        at(~name="z", New(id("unit"), Below), enter),
      ),
    ],
  );
};

/* a program with an empty module, a `use`, and a module in a module */
let nested =
  CorpusUtil.outline_of(
    CorpusUtil.seg_of(
      {|module A = {} in
use Nat in
module M = {
  let a = 1;
  module N = {
    let x = 2
  }
} in
let b = 3 in
b|},
    ),
  );
let nid = CorpusUtil.row_by_label(nested);

/* Enter on an empty module goes inside it; a blank last line inside a
   module goes out of it, one level at a time */
let empty_modules = () => {
  let empty = i => i == nid("A");
  let at = (line, key) =>
    press(
      ~of_=nested,
      ~on=line,
      ~fresh=fresh(),
      ~live=named("", 0),
      "+",
      key,
    );
  let enter = k("Enter");
  List.iter(
    ((label, want, got)) => check(outcome, label, want, got),
    [
      (
        "Enter on an empty module: first inside it",
        E.Go(New(nid("A"), First), 0, None),
        press(~of_=nested, ~blank=empty, "A", enter),
      ),
      (
        "on a value: below it",
        Go(New(nid("b"), Below), 0, None),
        press(~of_=nested, ~blank=empty, "b", enter),
      ),
      (
        "a blank line inside it, Enter: out, below it",
        Shift(New(nid("A"), Below)),
        at(New(nid("A"), First), enter),
      ),
      (
        "a blank last line in N: out of N, still in M",
        Shift(New(nid("N"), Below)),
        at(New(nid("x"), Below), enter),
      ),
      (
        "then out of M",
        Shift(New(nid("M"), Below)),
        at(New(nid("N"), Below), enter),
      ),
      ("mid-module: gone", Save(Leave), at(New(nid("a"), Below), enter)),
      (
        "Tab below N: into it",
        Shift(New(nid("N"), Last)),
        at(New(nid("N"), Below), tab),
      ),
      (
        "last inside M, below N: into N",
        Shift(New(nid("N"), Last)),
        at(New(nid("M"), Last), tab),
      ),
      (
        "a use never goes into a module",
        Flash,
        press(~of_=nested, "use Nat", tab),
      ),
    ],
  );
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
    "a new line's name begun `type `: a type instead",
    Rekind("type "),
    press(
      ~on=New(id("alpha"), Below),
      ~fresh=fresh(),
      ~live=named("type", 4),
      "+",
      k(" "),
    ),
  );
  check(
    outcome,
    "a letter on a new line: into the program",
    Type(Insert("q")),
    press(~on=New(id("alpha"), Below), ~live=named("", 0), "+", k("q")),
  );
};

/* a new line's name takes a field's keys: Shift selects, ⌥ reaches a
   word, ⌘ the name's start or end; ⌘A, ⌘V. IME and dead keys flash */
let live_keys = () => {
  let at = (~anchor=?, name, col, key) =>
    press(
      ~on=New(id("alpha"), Below),
      ~fresh=fresh(),
      ~live=named(~anchor?, name, col),
      "+",
      key,
    );
  let shift = k(~shift=true);
  List.iter(
    ((label, want, got)) => check(outcome, label, want, got),
    [
      (
        {js|Shift+←: the selection|js},
        E.Type(Select(Left, Char)),
        at("abc", 3, shift("ArrowLeft")),
      ),
      (
        "at the name's start: the editor's, which goes no further",
        Type(Select(Left, Char)),
        at("abc", 0, shift("ArrowLeft")),
      ),
      (
        {js|Shift+⌥←: a word|js},
        Type(Select(Left, Word)),
        at("abc", 3, k(~shift=true, ~alt=true, "ArrowLeft")),
      ),
      (
        {js|Shift+⌘→: to the end|js},
        Type(Select(Right, Edge)),
        at("abc", 1, k(~shift=true, ~meta=true, "ArrowRight")),
      ),
      (
        "Shift+Home",
        Type(Select(Left, Edge)),
        at("abc", 2, shift("Home")),
      ),
      (
        "Shift+End at the end",
        Type(Select(Right, Edge)),
        at("abc", 3, shift("End")),
      ),
      ({js|⌘A|js}, Type(SelectAll), at("abc", 1, k(~meta=true, "a"))),
      (
        {js|⌘A on a blank name: the editor's, which selects nothing|js},
        Type(SelectAll),
        at("", 0, k(~meta=true, "a")),
      ),
      (
        {js|← on a selection at the start: it collapses, the line kept|js},
        Type(Step(Left, Char)),
        at(~anchor=2, "abc", 0, k("ArrowLeft")),
      ),
      (
        {js|⌥←: a word|js},
        Type(Step(Left, Word)),
        at("abc", 2, k(~alt=true, "ArrowLeft")),
      ),
      (
        {js|⌘←: the start|js},
        Type(Step(Left, Edge)),
        at("abc", 2, k(~meta=true, "ArrowLeft")),
      ),
      (
        {js|⌫ on a selection at the start: it goes|js},
        Type(Erase(Left, Char)),
        at(~anchor=2, "abc", 0, k("Backspace")),
      ),
      (
        {js|⌥⌫: a word|js},
        Type(Erase(Left, Word)),
        at("abc", 2, k(~alt=true, "Backspace")),
      ),
      (
        {js|⌥⌦: a word|js},
        Type(Erase(Right, Word)),
        at("abc", 1, k(~alt=true, "Delete")),
      ),
      (
        {js|⌥⌦ at the end: the editor's to refuse|js},
        Type(Erase(Right, Word)),
        at("abc", 3, k(~alt=true, "Delete")),
      ),
      (
        {js|⌥⌫ on a blank name: the line goes, as ⌫'s does|js},
        go("alpha", 5, None),
        at("", 0, k(~alt=true, "Backspace")),
      ),
      (
        "a letter over a selection: the editor's to replace",
        Type(Insert("x")),
        at(~anchor=0, "abc", 3, k("x")),
      ),
      (
        "a space over a selected `type`: no keyword",
        Type(Insert(" ")),
        at(~anchor=0, "type", 4, k(" ")),
      ),
      ({js|⌘V|js}, Paste, at("abc", 3, k(~meta=true, "v"))),
      (
        {js|⌘C: the selection|js},
        Copy("bc", false),
        at(~anchor=3, "abc", 1, k(~meta=true, "c")),
      ),
      (
        {js|⌘X: cut|js},
        Copy("bc", true),
        at(~anchor=1, "abc", 3, k(~meta=true, "x")),
      ),
      (
        {js|⌘C with nothing selected|js},
        Swallow,
        at("abc", 1, k(~meta=true, "c")),
      ),
      (
        {js|⌘V in a rename's field: the field's|js},
        Field,
        press(~field=true, ~col=2, "alpha", k(~meta=true, "v")),
      ),
      (
        "a dead key",
        Flash,
        at("abc", 3, k(~alt=true, ~code="KeyE", "Dead")),
      ),
      (
        "an IME's composition",
        Flash,
        at(
          "abc",
          3,
          {
            ...k("Process"),
            composing: true,
          },
        ),
      ),
    ],
  );
};

/* clipboard text that can be part of a name; where a word ends */
let fragments_and_words = () => {
  List.iter(
    ((label, text, want)) =>
      check(option(string), label, want, E.fragment(text)),
    [
      ("a name's characters", "Bar_2'", Some("Bar_2'")),
      ("a copied line's newline trimmed", "  bar\n", Some("bar")),
      ("two lines", "a\nb", None),
      ("code", "x = 1", None),
      ("a space inside", "a b", None),
      ("only space", " \n", None),
      ("a qualified name", "M.x", None),
      ("a livelit's", "^slider", None),
    ],
  );
  List.iter(
    ((name, at, side, want)) =>
      check(
        int,
        Printf.sprintf(
          "%s from %d %s",
          name,
          at,
          side == Util.Direction.Left ? "left" : "right",
        ),
        want,
        E.word_edge(name, at, side),
      ),
    [
      ("foo_bar", 7, Util.Direction.Left, 0),
      ("fooBar", 6, Left, 0),
      ("foo'bar", 7, Left, 0),
      ("x'", 2, Left, 0),
      ("ab''cd", 6, Left, 4),
      ("ab''cd", 4, Left, 0),
      ("ab''cd", 0, Right, 2),
      ("ab''cd", 2, Right, 6),
    ],
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
  check(
    int,
    {js|last inside beta: above its ⇒|js},
    find_line(base, {js|beta⇒|js}),
    at("beta", Last),
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
  let term = Web.Program.outline(p);
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

let update = (action, code: Web.ScratchModel.Scratchpad.code) =>
  C.update(
    ~settings=Web.Globals.Model.init().settings,
    ~schedule_workspace=_ => (),
    ~prefix="scratch",
    ~name="t",
    action,
    code,
  );
let run = (action, code) => update(action, code).model;

let text_of = (code: Web.ScratchModel.Scratchpad.code) =>
  CorpusUtil.text_of(Web.Program.document(code.program));

let caret_on = (code: Web.ScratchModel.Scratchpad.code): option(O.node) =>
  switch (C.text^) {
  | Some({line: Row(_, Some(Item(row))), _}) =>
    O.node_of(row, Web.Program.outline(code.program))
  | _ => None
  };

let reset = () => {
  C.text := None;
  C.cursor := None;
  C.flash := 0;
  C.created := None;
  Hashtbl.remove(
    Web.ScratchPersist.slide_folds,
    Web.ScratchPersist.content_key("scratch", "t"),
  );
};

/* a new test below a test, or first of a run: the caret on it */
let new_tests = () => {
  let code = code_of("let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\na");
  let term = Web.Program.outline(code.program);
  let tests =
    List.filter_map(
      (n: O.node) => n.o_kind == KTest ? n.o_id : None,
      O.flatten(O.rows(term)),
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
             O.flatten(O.rows(Web.Program.outline(code''.program))),
           ),
         ),
         Web.Program.outline(code''.program),
       ),
  );
  reset();
};

/* the same text, as the program prints it */
let printed = (src: string) => CorpusUtil.text_of(CorpusUtil.seg_of(src));

/* text mode's new definition, once in */
let fresh_def = (): Id.t =>
  switch (C.text^) {
  | Some({fresh: Some(f), _}) => f.f_id
  | _ => fail("no new definition")
  };

/* the new definition's name and the caret in it, as the editor has them */
let live_of = (code: Web.ScratchModel.Scratchpad.code) =>
  C.live_name(code.program, fresh_def())
  |> Option.map((l: E.live) => (l.l_name, l.l_at));

/* keys on a new definition's line, in order: none its own undo step */
let typing = (line, edits, code) =>
  List.fold_left(
    (code, e) => {
      let u = update(Type(line, e), code);
      check(bool, "not an undo step of its own", false, u.historic);
      u.model;
    },
    code,
    edits,
  );

let at_name = testable(Fmt.(option(pair(string, int))), (==));

/* Enter makes the new definition at once, a hole for its name and the
   editor's caret in it; keys typed on its line are the editor's there */
let live_typing = () => {
  let code = code_of("let a = 1 in\nlet b = 2 in\na + b");
  let a = CorpusUtil.row_by_label(Web.Program.outline(code.program), "a");
  let line = E.New(a, Below);
  C.text := Some(E.at(line));
  let made = update(Create(line, ""), code);
  check(bool, "one undo step", true, made.historic);
  let code = made.model;
  check(
    string,
    "in below a, a hole for its name",
    printed({js|let a = 1 in

let ¿ = ¿ in

let b = 2 in
a + b|js}),
    text_of(code),
  );
  check(at_name, "the editor's caret in it", Some(("", 0)), live_of(code));
  let code = typing(line, [Insert("a"), Insert("b")], code);
  check(
    string,
    "letters into the program",
    printed({js|let a = 1 in

let ab = ¿ in

let b = 2 in
a + b|js}),
    text_of(code),
  );
  check(at_name, "the caret after them", Some(("ab", 2)), live_of(code));
  let code = typing(line, [Step(Left, Char), Insert("x")], code);
  check(at_name, "← then x", Some(("axb", 2)), live_of(code));
  let code = typing(line, [Step(Right, Edge)], code);
  check(at_name, "End", Some(("axb", 3)), live_of(code));
  let code = typing(line, [Step(Left, Edge)], code);
  check(at_name, "Home", Some(("axb", 0)), live_of(code));
  C.flash := 0;
  let same = run(Type(line, Erase(Left, Char)), code);
  check(
    bool,
    "⌫ at the start: refused, the program as it was",
    true,
    C.flash^ == 1 && text_of(same) == text_of(code),
  );
  let code = typing(line, [Erase(Right, Char)], code);
  check(at_name, "Delete", Some(("xb", 0)), live_of(code));
  /* a key toward another line, or one that never came: nothing */
  let other = run(Type(New(a, First), Insert("q")), code);
  check(string, "keys for another line", text_of(code), text_of(other));
  reset();
};

/* the new definition's line left: a blank name takes the definition
   with it, not an undo step; a name that isn't one is refused, and a
   second Esc drops it */
let live_leaving = () => {
  let src = "let a = 1 in\nlet b = 2 in\na + b";
  let start = () => {
    let code = code_of(src);
    let a = CorpusUtil.row_by_label(Web.Program.outline(code.program), "a");
    let line = E.New(a, Below);
    C.text := Some(E.at(line));
    (line, run(Create(line, ""), code));
  };
  let (line, code) = start();
  let code = typing(line, [Insert("q"), Erase(Left, Char)], code);
  let f: E.fresh = {
    f_id: fresh_def(),
    f_kw: "",
  };
  let gone = update(Settle(line, f, Stay), code);
  check(string, "blank: gone", printed(src), text_of(gone.model));
  check(bool, "not an undo step", false, gone.historic);
  let (line, code) = start();
  let code = typing(line, [Insert("B")], code);
  let f: E.fresh = {
    f_id: fresh_def(),
    f_kw: "",
  };
  let kept = run(Settle(line, f, Leave), code);
  check(
    bool,
    "a capital: refused, the caret back on its line",
    true,
    switch (C.text^) {
    | Some({line: back, error: Some(_), fresh: Some(f'), _}) =>
      E.same_line(back, line) && f' == f
    | _ => false
    },
  );
  check(
    bool,
    "the name still in the program",
    true,
    text_of(kept) == text_of(code),
  );
  let dropped = update(Discard(line, f.f_id), kept);
  check(string, "Esc again: dropped", printed(src), text_of(dropped.model));
  check(bool, "not an undo step", false, dropped.historic);
  let (line, code) = start();
  let code = typing(line, [Insert("c")], code);
  let d = fresh_def();
  C.text := None;
  ignore(
    run(
      Settle(
        line,
        {
          f_id: d,
          f_kw: "",
        },
        Leave,
      ),
      code,
    ),
  );
  check(
    bool,
    "a name: row mode's cursor on its row",
    true,
    C.cursor^ == O.label_path(d, Web.Program.outline(code.program)),
  );
  reset();
};

/* Enter on an expanded module: its new first member, live; Enter after
   its name, a next line below it */
let first_member = () => {
  let code = code_of("module M = {\n  let a = 1;\n  let b = 2\n} in\nM.a");
  let m = CorpusUtil.row_by_label(Web.Program.outline(code.program), "M");
  let line = E.New(m, First);
  C.text := Some(E.at(line));
  let code = typing(line, [Insert("z")], run(Create(line, ""), code));
  check(
    string,
    "z first in M",
    printed(
      {js|module M = {
  let z = ¿;

  let a = 1;
  let b = 2
} in
M.a|js},
    ),
    text_of(code),
  );
  let z = fresh_def();
  /* Enter: the next line waits for its definition (the save's keydown) */
  C.text := Some(E.at(New(z, Below)));
  let code =
    run(
      Settle(
        line,
        {
          f_id: z,
          f_kw: "",
        },
        Next,
      ),
      code,
    );
  check(
    string,
    "the next one below z",
    printed(
      {js|module M = {
  let z = ¿;

  let ¿ = ¿;

  let a = 1;
  let b = 2
} in
M.a|js},
    ),
    text_of(code),
  );
  check(
    bool,
    "the caret on it",
    true,
    switch (C.text^) {
    | Some({line: New(a, Below), fresh: Some(_), _}) => a == z
    | _ => false
    },
  );
  reset();
};

/* Tab and ⇧Tab move the caret's item a level in and out, one step
   each: the caret goes with it, at its column; a folded module it goes
   into unfolds; no cell opens */
let nesting_ops = () => {
  let src = "let a = 1 in\nmodule M = {\n  let x = 2\n} in\n\nlet bee = 4 in\n\nbee";
  let code = code_of(src);
  let term = Web.Program.outline(code.program);
  let ck = Web.ScratchPersist.content_key("scratch", "t");
  let m_path =
    Option.get(O.label_path(CorpusUtil.row_by_label(term, "M"), term));
  Hashtbl.replace(Web.ScratchPersist.slide_folds, ck, [(m_path, true)]);
  /* the caret on [label]'s row, at column 2 */
  let on = (code: Web.ScratchModel.Scratchpad.code, label) => {
    let term = Web.Program.outline(code.program);
    let id = CorpusUtil.row_by_label(term, label);
    C.text :=
      Some(
        E.at(
          Row(Option.get(O.label_path(id, term)), Some(Item(id))),
          ~col=2,
        ),
      );
    id;
  };
  let caret = (code: Web.ScratchModel.Scratchpad.code) =>
    switch (caret_on(code), C.text^) {
    | (Some(n), Some(t)) => Some((n.o_label, t.col))
    | _ => None
    };
  let scheduled = ref([]);
  let in_ =
    C.update(
      ~settings=Web.Globals.Model.init().settings,
      ~schedule_workspace=a => scheduled := [a, ...scheduled^],
      ~prefix="scratch",
      ~name="t",
      DefOp(Indent, on(code, "bee")),
      code,
    );
  check(
    string,
    "Tab: M's last member",
    printed(
      "let a = 1 in\nmodule M = {\n  let x = 2;\n\n  let bee = 4\n} in\n\nbee",
    ),
    text_of(in_.model),
  );
  check(bool, "one undo step", true, in_.historic);
  check(at_name, "the caret along", Some(("bee", 2)), caret(in_.model));
  check(
    bool,
    "M unfolded",
    false,
    O.folded(
      ~folds=Web.ScratchPersist.folds("scratch", "t"),
      m_path,
      KModule,
    ),
  );
  check(bool, "no cell opened", true, scheduled^ == []);
  let out = run(DefOp(Outdent, on(in_.model, "bee")), in_.model);
  check(string, {js|⇧Tab: out again|js}, printed(src), text_of(out));
  check(at_name, "the caret along", Some(("bee", 2)), caret(out));
  Hashtbl.remove(Web.ScratchPersist.slide_folds, ck);
  reset();
};

/* a new line moved a level, as its key moves it: the caret there at
   once, its definition after */
let shift = (from: E.line, to_: E.line, code) => {
  C.text :=
    Option.map(
      (t: E.t) =>
        {
          ...t,
          line: to_,
        },
      C.text^,
    );
  update(Shift(from, to_), code);
};

/* `module M` named, Enter: the next line first inside it. Blank there,
   Enter takes the line out, just after M, and Tab back in; named, it
   keeps its name, the editor's caret where it was. Still the step that
   made it */
let module_lines = () => {
  let code = code_of("let a = 1 in\na");
  let a = CorpusUtil.row_by_label(Web.Program.outline(code.program), "a");
  let line = E.New(a, Below);
  C.text := Some(E.at(line));
  let code =
    typing(line, [Insert("M")], run(Create(line, "module "), code));
  let m: E.fresh = {
    f_id: fresh_def(),
    f_kw: "module ",
  };
  let inside = E.next_line(m);
  check(
    bool,
    "Enter after a module's name: first inside it",
    true,
    inside == New(m.f_id, First),
  );
  check(
    bool,
    "after a value's: below it",
    true,
    E.next_line({
      ...m,
      f_kw: "",
    })
    == New(m.f_id, Below),
  );
  C.text := Some(E.at(inside));
  let code = run(Settle(line, m, Next), code);
  check(
    string,
    "a line inside M",
    printed({js|let a = 1 in

module M = {
  let ¿ = ¿
} in

a|js}),
    text_of(code),
  );
  let below = E.New(m.f_id, Below);
  let out = shift(inside, below, code);
  check(bool, "not a step of its own", false, out.historic);
  check(
    string,
    "out, just after M",
    printed({js|let a = 1 in

module M = {} in

let ¿ = ¿ in

a|js}),
    text_of(out.model),
  );
  check(
    bool,
    "the caret's line there",
    true,
    switch (C.text^) {
    | Some({line, fresh: Some(_), _}) => line == below
    | _ => false
    },
  );
  let code =
    typing(
      below,
      [Insert("z"), Insert("q"), Step(Left, Char)],
      out.model,
    );
  let code = shift(below, New(m.f_id, Last), code).model;
  check(
    string,
    "named, Tab: back in, the name kept",
    printed({js|let a = 1 in

module M = {
  let zq = ¿
} in

a|js}),
    text_of(code),
  );
  check(at_name, "the caret where it was", Some(("zq", 1)), live_of(code));
  reset();
};

/* a new line below a function, Tab: inside it, its last local
   definition; ⇧Tab: out again */
let value_lines = () => {
  let src = "let f = fun x ->\n  x\nin\nf";
  let code = code_of(src);
  let f = CorpusUtil.row_by_label(Web.Program.outline(code.program), "f");
  let below = E.New(f, Below);
  C.text := Some(E.at(below));
  let code = run(Create(below, ""), code);
  let inside = E.New(f, Last);
  let code = shift(below, inside, code).model;
  check(
    string,
    "Tab: inside f, before its result",
    printed({js|let f = fun x ->
  let ¿ = ¿ in
  x
in

f|js}),
    text_of(code),
  );
  let code =
    shift(inside, below, typing(inside, [Insert("y")], code)).model;
  check(
    string,
    {js|named, ⇧Tab: out again|js},
    printed({js|let f = fun x ->
  x
in

let y = ¿ in

f|js}),
    text_of(code),
  );
  /* into what takes no definitions, refused: the caret back */
  C.flash := 0;
  let into_x =
    E.New(
      CorpusUtil.row_by_label(Web.Program.outline(code.program), "f"),
      First,
    );
  let same = shift(below, into_x, code);
  check(
    bool,
    "refused: flashed, the program and the caret's line as they were",
    true,
    C.flash^ == 1
    && text_of(same.model) == text_of(code)
    && (
      switch (C.text^) {
      | Some({line, _}) => line == below
      | None => false
      }
    ),
  );
  reset();
};

/* `type ` typed as a value's name makes it a type, blank; ⌫ at its
   start makes it a value again: still the step that made it */
let live_kinds = () => {
  let code = code_of("let a = 1 in\na");
  let a = CorpusUtil.row_by_label(Web.Program.outline(code.program), "a");
  let line = E.New(a, Below);
  C.text := Some(E.at(line));
  let code =
    typing(
      line,
      [Insert("t"), Insert("y"), Insert("p"), Insert("e")],
      run(Create(line, ""), code),
    );
  let typed = update(Rekind(line, "type "), code);
  check(bool, "not an undo step", false, typed.historic);
  check(
    string,
    "a type",
    printed({js|let a = 1 in

type ¿ = ¿ in

a|js}),
    text_of(typed.model),
  );
  check(at_name, "its name blank", Some(("", 0)), live_of(typed.model));
  let code = typing(line, [Insert("T")], typed.model);
  check(
    string,
    "named",
    printed({js|let a = 1 in

type T = ¿ in

a|js}),
    text_of(code),
  );
  let back =
    run(Rekind(line, ""), typing(line, [Erase(Left, Char)], code));
  check(
    string,
    "a value again",
    printed({js|let a = 1 in

let ¿ = ¿ in

a|js}),
    text_of(back),
  );
  reset();
};

/* with cells open, a new definition the stack doesn't show gets a cell
   of its own, its name typed in the header; named, it stays pinned */
let live_in_stack = () => {
  let code = code_of("let a = 1 in\nlet b = 2 in\na + b");
  let term = Web.Program.outline(code.program);
  let a = CorpusUtil.row_by_label(term, "a");
  let code =
    Web.Workspace.realize(
      ~info_map=
        Web.Workspace.fresh_info_map(
          ~settings=Web.Globals.Model.init().settings,
          code.program,
          Web.Workspace.fresh_term(code.program),
        ),
      Web.SlideView.pin(~outline=term, a, code.view),
      code,
    );
  let line = E.New(a, Below);
  C.text := Some(E.at(line));
  let code = typing(line, [Insert("n")], run(Create(line, ""), code));
  let d = fresh_def();
  let header = (code: Web.ScratchModel.Scratchpad.code) =>
    switch (code.program) {
    | Divided(dv) =>
      Option.map(
        (e: Web.ScratchCell.t) =>
          CorpusUtil.text_of(Web.ScratchFocus.zip_of_cell(e.e_header)),
        Web.Divided.cell(d, dv),
      )
    | Whole(_) => None
    };
  check(
    option(string),
    "its own cell, n in its header",
    Some("n"),
    header(code),
  );
  C.text := None;
  let code =
    run(
      Settle(
        line,
        {
          f_id: d,
          f_kw: "",
        },
        Leave,
      ),
      code,
    );
  check(
    bool,
    "pinned",
    true,
    List.exists((p: Web.SlideView.pin) => p.p_id == d, code.view.pins),
  );
  reset();
};

/* the page's undo after a new definition's name: the definition goes,
   name and all, in one step */
let live_undo = () => {
  module U = Test_Undo.Cells;
  let outline = (a: C.Action.t): Web.Page.Update.t =>
    Editors(Scratch(Outline(a)));
  let program = m => CorpusUtil.text_of(Web.Program.document(U.program(m)));
  let m = U.run(U.mk(), Refresh);
  let before = program(m);
  let line = E.New(U.row(m, "a"), Below);
  C.text := Some(E.at(line));
  let m = U.run(m, outline(Create(line, "")));
  let m =
    List.fold_left(
      (m, c) => U.run(m, outline(Type(line, Insert(c)))),
      m,
      ["n", "e", "w"],
    );
  check(
    string,
    "named in the program",
    printed({js|let a = 1 in

let new = ¿ in

let b = 2 in
a + b|js}),
    program(m),
  );
  check(int, "one undo step", 1, List.length(m.undo_stack));
  let m = U.run(m, Globals(Undo));
  check(string, "⌘Z: as before the Enter", before, program(m));
  reset();
};

/* a blank new definition dropped takes its insertion out of the history:
   ⌘Z then undoes the change before it, and a redo the Enter cleared
   comes back. Named, it stays a step */
let undo_after_drop = () => {
  module U = Test_Undo.Cells;
  let outline = (a: C.Action.t): Web.Page.Update.t =>
    Editors(Scratch(Outline(a)));
  let program = m => CorpusUtil.text_of(Web.Program.document(U.program(m)));
  let depth = (m: Web.History.Model.t) => List.length(m.undo_stack);
  /* Enter below a, then [settle]d as the line's left */
  let enter = (~name="", m) => {
    let line = E.New(U.row(m, "a"), Below);
    C.text := Some(E.at(line));
    let m = U.run(m, outline(Create(line, "")));
    let f: E.fresh = {
      f_id: fresh_def(),
      f_kw: "",
    };
    let m =
      String.fold_left(
        (m, c) =>
          U.run(m, outline(Type(line, Insert(String.make(1, c))))),
        m,
        name,
      );
    (m, () => U.run(m, outline(Settle(line, f, Leave))));
  };
  let m = U.run(U.mk(), Refresh);
  let before = program(m);
  let m = U.run(m, outline(DefOp(MoveUp, U.row(m, "b"))));
  let moved = program(m);
  check(int, "a move: one step", 1, depth(m));
  let (m, leave) = enter(m);
  check(int, "Enter: a step of its own", 2, depth(m));
  let m = leave();
  check(string, "left blank: dropped", moved, program(m));
  check(int, "its step gone with it", 1, depth(m));
  let m = U.run(m, Globals(Undo));
  check(string, {js|⌘Z: the move undone|js}, before, program(m));
  let (_, leave) = enter(m);
  let m = leave();
  check(int, "a redo the Enter cleared, back", 1, List.length(m.redo_stack));
  let m = U.run(m, Globals(Redo));
  check(string, {js|⌘⇧Z: the move again|js}, moved, program(m));
  let (_, leave) = enter(~name="n", m);
  let m = leave();
  check(int, "named: its step kept", 2, depth(m));
  reset();
};

/* a new definition below a in [src], made, and its line */
let live_line = (src: string) => {
  let code = code_of(src);
  let a = CorpusUtil.row_by_label(Web.Program.outline(code.program), "a");
  let line = E.New(a, Below);
  C.text := Some(E.at(line));
  (line, run(Create(line, ""), code));
};
let two = "let a = 1 in\nlet b = 2 in\na + b";
/* the program with the new definition named [name] */
let named_two = name =>
  printed(
    "let a = 1 in\n\nlet " ++ name ++ {js| = ¿ in

let b = 2 in
a + b|js},
  );

/* the new definition's name, the caret in it and the selection's other
   end, as the editor has them */
let selection_of = (code: Web.ScratchModel.Scratchpad.code) =>
  C.live_name(code.program, fresh_def())
  |> Option.map((l: E.live) => (l.l_name, l.l_at, l.l_anchor));
let at_sel = option(triple(string, int, int));

/* what the editor holding the name has selected */
let editor_selected = (code: Web.ScratchModel.Scratchpad.code) =>
  switch (C.live_pane(code.program, fresh_def())) {
  | Some((_, z, _)) =>
    Zipper.trim_selected_text(z, Web.ItemEdit.text(z.selection.content))
  | None => fail("no editor holds the name")
  };

/* ⌘V on a new definition's line: the clipboard's text, through the
   editor's paste, when it can be part of a name; else refused, flashed,
   the program as it was */
let live_paste = () => {
  let (line, code) = live_line(two);
  let code =
    typing(line, [Insert("f"), Insert("o"), Step(Left, Char)], code);
  let code = typing(line, [Paste("Bar")], code);
  check(at_name, "at the caret", Some(("fBaro", 4)), live_of(code));
  check(string, "in the program", named_two("fBaro"), text_of(code));
  C.flash := 0;
  let same = run(Type(line, Paste("x = 1")), code);
  check(
    bool,
    "code: refused",
    true,
    C.flash^ == 1 && text_of(same) == text_of(code),
  );
  let same = run(Type(line, Paste("c\nd")), code);
  check(
    bool,
    "two lines: refused",
    true,
    C.flash^ == 2 && text_of(same) == text_of(code),
  );
  let code = typing(line, [Paste("z\n")], code);
  check(
    at_name,
    "a copied line: its newline left out",
    Some(("fBarzo", 5)),
    live_of(code),
  );
  let code = typing(line, [SelectAll, Paste("q")], code);
  check(
    at_name,
    "over a selection: replacing it",
    Some(("q", 1)),
    live_of(code),
  );
  check(string, "so the program", named_two("q"), text_of(code));
  reset();
};

/* ⌥⌫ ⌥⌦ ⌥← ⌥→ by words: runs of letters, digits and underscores, and an
   apostrophe inside one */
let live_words = () => {
  let (line, code) = live_line(two);
  let code = typing(line, [Paste("ab''cd''ef")], code);
  let code = typing(line, [Erase(Left, Word)], code);
  check(
    at_name,
    {js|⌥⌫: the last word|js},
    Some(("ab''cd''", 8)),
    live_of(code),
  );
  check(string, "in the program", named_two("ab''cd''"), text_of(code));
  let code = typing(line, [Erase(Left, Word)], code);
  check(
    at_name,
    "again: the '' and the word before them",
    Some(("ab''", 4)),
    live_of(code),
  );
  let code = typing(line, [Step(Left, Word)], code);
  check(at_name, {js|⌥←|js}, Some(("ab''", 0)), live_of(code));
  let code = typing(line, [Step(Right, Word)], code);
  check(at_name, {js|⌥→|js}, Some(("ab''", 2)), live_of(code));
  let code =
    typing(
      line,
      [
        Paste("x_y"),
        Step(Left, Char),
        Step(Left, Char),
        Step(Left, Char),
      ],
      code,
    );
  let code = typing(line, [Erase(Right, Word)], code);
  check(
    at_name,
    {js|⌥⌦: the rest of the word, _ and all|js},
    Some(("ab''", 2)),
    live_of(code),
  );
  check(string, "in the program", named_two("ab''"), text_of(code));
  reset();
};

/* Shift's selection is the editor's, kept to the name; typing replaces
   it, ⌫ erases it */
let live_selection = () => {
  let (line, code) = live_line(two);
  let code = typing(line, [Paste("alpha")], code);
  let code = typing(line, [Select(Left, Char), Select(Left, Char)], code);
  check(
    at_sel,
    {js|Shift+← twice|js},
    Some(("alpha", 3, 5)),
    selection_of(code),
  );
  check(string, "the editor's selection", "ha", editor_selected(code));
  let code = typing(line, [Insert("X")], code);
  check(at_sel, "typed over", Some(("alpX", 4, 4)), selection_of(code));
  check(string, "in the program", named_two("alpX"), text_of(code));
  let code = typing(line, [Step(Left, Char), Select(Left, Edge)], code);
  check(
    at_sel,
    "Shift+Home from inside",
    Some(("alpX", 0, 3)),
    selection_of(code),
  );
  check(string, "the editor's too", "alp", editor_selected(code));
  let code = typing(line, [Erase(Left, Char)], code);
  check(
    at_sel,
    {js|⌫: what's selected|js},
    Some(("X", 0, 0)),
    selection_of(code),
  );
  let code = typing(line, [Insert("y"), Insert("z"), SelectAll], code);
  check(
    at_sel,
    {js|⌘A: the name|js},
    Some(("yzX", 3, 0)),
    selection_of(code),
  );
  C.flash := 0;
  let same = run(Type(line, Select(Right, Char)), code);
  check(
    bool,
    "no further than the name, quietly",
    true,
    C.flash^ == 0 && selection_of(same) == selection_of(code),
  );
  let code = typing(line, [Step(Left, Char)], code);
  check(
    at_sel,
    {js|←: collapsed to its start|js},
    Some(("yzX", 0, 0)),
    selection_of(code),
  );
  let code = typing(line, [Select(Right, Word)], code);
  check(
    at_sel,
    {js|Shift+⌥→: the word|js},
    Some(("yzX", 3, 0)),
    selection_of(code),
  );
  let code =
    typing(line, [Step(Right, Char), Select(Left, Char), Cut], code);
  check(at_sel, {js|⌘X's cut|js}, Some(("yz", 2, 2)), selection_of(code));
  let code = typing(line, [Cut], code);
  check(
    at_sel,
    "a cut with nothing selected: nothing",
    Some(("yz", 2, 2)),
    selection_of(code),
  );
  reset();
};

/* keys typed on a new line before its definition is in (the Enter's
   frame) wait for it, not for the line the Enter left */
let keys_wait = () => {
  let visible = H.visible(term);
  let line = E.New(id("alpha"), Below);
  let text = ref(Some(E.at(line)));
  let edits = ref([]);
  let on = H.handlers(~text, ~edits, ());
  Web.OutlineFollow.held := None;
  ignore(
    Web.OutlineSidebar.keys(
      ~props=H.props(),
      ~on,
      ~visible,
      H.keydown(~code="KeyQ", "q"),
    ),
  );
  check(bool, "nothing typed yet", true, edits^ == []);
  check(
    bool,
    "held for the new line",
    true,
    switch (Web.OutlineFollow.held^) {
    | Some((key, [_])) => key == E.new_key(id("alpha"), Below)
    | _ => false
    },
  );
  Web.OutlineFollow.held := None;
};

/* ⌘D puts the caret in the copy; ⌘⌫ on the next row; a refused op
   flashes */
let copy_and_delete = () => {
  let code = code_of("let a = 1 in\nlet b = 2 in\na + b");
  let term = Web.Program.outline(code.program);
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
      && O.node_of(id, Web.Program.outline(copied.program))
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

/* ⌥F folds the cursor's row in row mode too; a leaf has nothing to fold */
let row_mode_fold = () => {
  let visible = H.visible(term);
  let cursor = ref(None);
  let folded = ref([]);
  let on =
    H.handlers(
      ~cursor,
      ~toggle_collapse=
        p => {
          folded := [p, ...folded^];
          H.ignore;
        },
      (),
    );
  let press = label => {
    cursor :=
      Option.map(
        (r: Web.OutlineSidebar.visible_row) => r.r_path,
        Array.find_opt(
          (r: Web.OutlineSidebar.visible_row) => r.r_node.o_label == label,
          visible,
        ),
      );
    folded := [];
    ignore(
      Web.OutlineSidebar.keys(
        ~props=H.props(),
        ~on,
        ~visible,
        H.keydown(~alt=true, ~code="KeyF", {js|ƒ|js}),
      ),
    );
    folded^;
  };
  let beta = press("beta");
  check(bool, "beta folds", true, beta == Option.to_list(cursor^));
  check(bool, "alpha has nothing to fold", true, press("alpha") == []);
};

/* a run of tests, folded by default: ⌥F, → and Enter open it, ← goes
   to its parent, ↓ past its tests; opened, ← folds it, → and ↓ go in */
let run_fold_keys = () => {
  let run = Option.get(O.run_of(id("1"), term));
  let path = label => O.label_path(id(label), term);
  let press = (~folds, key) => {
    let cursor = ref(Some(run));
    let flipped = ref([]);
    let on =
      H.handlers(
        ~cursor,
        ~toggle_collapse=
          p => {
            flipped := [p, ...flipped^];
            H.ignore;
          },
        (),
      );
    ignore(
      Web.OutlineSidebar.keys(
        ~props=H.props(),
        ~on,
        ~visible=H.visible(~folds, term),
        key,
      ),
    );
    (flipped^, cursor^);
  };
  let alt_f = H.keydown(~alt=true, ~code="KeyF", {js|ƒ|js});
  let flips =
    testable(
      Fmt.of_to_string(((ps, c)) =>
        "flipped ["
        ++ String.concat("; ", List.map(O.show_path, ps))
        ++ "], cursor "
        ++ Option.fold(~none="none", ~some=O.show_path, c)
      ),
      (==),
    );
  let opens = ([run], Some(run));
  check(flips, "⌥F opens it", opens, press(~folds=[], alt_f));
  check(flips, "→ too", opens, press(~folds=[], H.keydown("ArrowRight")));
  check(flips, "Enter too", opens, press(~folds=[], H.keydown("Enter")));
  check(
    flips,
    "←: to its parent, the header",
    ([], None),
    press(~folds=[], H.keydown("ArrowLeft")),
  );
  check(
    flips,
    "↓: past its tests",
    ([], path("blank")),
    press(~folds=[], H.keydown("ArrowDown")),
  );
  let opened = [(run, false)];
  check(
    flips,
    "opened, ← folds it",
    opens,
    press(~folds=opened, H.keydown("ArrowLeft")),
  );
  check(
    flips,
    "→ goes in",
    ([], path("1")),
    press(~folds=opened, H.keydown("ArrowRight")),
  );
  check(
    flips,
    "↓ too",
    ([], path("1")),
    press(~folds=opened, H.keydown("ArrowDown")),
  );
};

/* in text mode a folded run is one ghost line: ↑↓ step over its tests,
   Enter adds a test after the run, ⌥F opens it */
let run_text = () => {
  let ls = lines(~folds=[], ());
  check(
    bool,
    "its tests aren't lines",
    false,
    Array.exists(
      (e: E.entry) =>
        switch (e) {
        | Of_row(r) => r.r_node.o_kind == KTest
        | Of_new(_) => false
        },
      ls,
    ),
  );
  let go = (which, col, target) => E.Go(line_of(ls, which), col, target);
  check(
    outcome,
    "↓ onto the run",
    go("tests", 0, Some(0)),
    press(~folds=[], "unit", k("ArrowDown")),
  );
  check(
    outcome,
    "↓ past its tests",
    go("blank", 0, Some(0)),
    press(~folds=[], "tests", k("ArrowDown")),
  );
  check(
    outcome,
    "↑ back onto it",
    go("tests", 0, Some(0)),
    press(~folds=[], "blank", k("ArrowUp")),
  );
  check(
    outcome,
    "Enter: a test after the run",
    NewTest(NewTestBelow, id("2")),
    press(~folds=[], "tests", k("Enter")),
  );
  check(
    outcome,
    "⌥F opens it",
    Fold(Option.get(O.run_of(id("1"), term))),
    press(~folds=[], "tests", k(~alt=true, ~code="KeyF", {js|ƒ|js})),
  );
};

/* a test added in a folded run opens it, so the caret shows on the new
   test; so does one a lone test's new neighbour makes */
let new_test_opens_run = () => {
  let folded = path =>
    O.folded(~folds=Web.ScratchPersist.folds("scratch", "t"), path, KTests);
  let label = (code: Web.ScratchModel.Scratchpad.code) =>
    Option.map((n: O.node) => n.o_label, caret_on(code));
  let code = code_of("let a = 1 in\ntest a == 1 end;\ntest a > 0 end;\na");
  let term = Web.Program.outline(code.program);
  let row = CorpusUtil.row_by_label(term);
  let tests = Option.get(O.run_of(row("1"), term));
  check(bool, "folded to start", true, folded(tests));
  C.text := Some(E.at(Row(tests, Some(Run(row("1"))))));
  let code' = run(DefOp(NewTestBelow, row("2")), code);
  check(bool, "Enter on it: opened", false, folded(tests));
  check(
    option(string),
    "the caret on the new test",
    Some("3"),
    label(code'),
  );
  reset();
  let code = code_of("let a = 1 in\ntest a == 1 end;\na");
  let term = Web.Program.outline(code.program);
  let lone = CorpusUtil.row_by_label(term, "1");
  C.text :=
    Some(
      E.at(Row(Option.get(O.label_path(lone, term)), Some(Item(lone)))),
    );
  let code' = run(DefOp(NewTestBelow, lone), code);
  let term' = Web.Program.outline(code'.program);
  check(
    bool,
    "a lone test's new neighbour: their run opened",
    false,
    folded(
      Option.get(O.run_of(CorpusUtil.row_by_label(term', "2"), term')),
    ),
  );
  check(option(string), "the caret on it", Some("2"), label(code'));
  reset();
};

/* a test's menu and a tests label's offer a new test: below the test,
   first of an expanded run, after a folded one */
let test_menus = () => {
  let ops = ref([]);
  let on = H.handlers(~ops, ());
  let labels = rows =>
    List.filter_map(
      (r: Web.OutlineSidebar.menu_row) =>
        switch (r) {
        | Item(label, _, _) => Some(label)
        | Divider => None
        },
      rows,
    );
  let act = (rows, label) =>
    List.iter(
      (r: Web.OutlineSidebar.menu_row) =>
        switch (r) {
        | Item(l, _, act) when l == label => ignore(act())
        | _ => ()
        },
      rows,
    );
  let tests =
    List.filter_map(
      (n: O.node) => n.o_kind == KTest ? n.o_id : None,
      O.flatten(O.rows(term)),
    );
  let (first, last) = (List.nth(tests, 0), List.nth(tests, 1));
  let on_test = Web.OutlineSidebar.menu_rows(~on, On_item(first, KTest));
  check(
    bool,
    "a test's menu",
    true,
    List.mem("New test below", labels(on_test)),
  );
  act(on_test, "New test below");
  check(bool, "below it", true, ops^ == [(NewTestBelow, first)]);
  check(
    bool,
    "not a definition's",
    false,
    List.mem(
      "New test below",
      labels(
        Web.OutlineSidebar.menu_rows(~on, On_item(id("alpha"), KConst)),
      ),
    ),
  );
  ops := [];
  act(
    Web.OutlineSidebar.menu_rows(~on, On_run(first, last, true)),
    "New test below",
  );
  check(
    bool,
    "an expanded run: first of it",
    true,
    ops^ == [(NewTestAbove, first)],
  );
  ops := [];
  act(
    Web.OutlineSidebar.menu_rows(~on, On_run(first, last, false)),
    "New test below",
  );
  check(
    bool,
    "a folded run: after it",
    true,
    ops^ == [(NewTestBelow, last)],
  );
};

/* Esc leaves in one press: a name that will do is saved; one that won't
   is dropped, the program as it was, its row flashing; a new
   definition's that won't goes with its definition */
let esc_leaves = () => {
  let src = "let a = 1 in\nlet b = a + 1 in\nb";
  let code = code_of(src);
  let term = Web.Program.outline(code.program);
  let a = CorpusUtil.row_by_label(term, "a");
  let line = E.Row(Option.get(O.label_path(a, term)), Some(Item(a)));
  /* the key's save has left text mode already */
  let esc = typed => {
    reset();
    update(
      Save(
        {
          sv_line: line,
          sv_text: typed,
          sv_col: 1,
        },
        Done,
      ),
      code,
    );
  };
  let saved = esc("c");
  check(
    string,
    "a name that will do: saved",
    printed("let c = 1 in\nlet b = c + 1 in\nb"),
    text_of(saved.model),
  );
  check(bool, "row mode, no flash", true, C.text^ == None && C.flash^ == 0);
  check(
    bool,
    "the cursor on its row",
    true,
    C.cursor^ == O.label_path(a, Web.Program.outline(saved.model.program)),
  );
  let kept = esc("B");
  check(
    string,
    "one that won't: as it was",
    printed(src),
    text_of(kept.model),
  );
  check(bool, "not an undo step", false, kept.historic);
  check(
    bool,
    "row mode, its row flashing",
    true,
    C.text^ == None && C.flash^ == 1,
  );
  let (line, code) = live_line(two);
  let code = typing(line, [Insert("B")], code);
  let f: E.fresh = {
    f_id: fresh_def(),
    f_kw: "",
  };
  C.text := None;
  C.flash := 0;
  let gone = update(Settle(line, f, Done), code);
  check(
    string,
    "a new definition's that won't: gone with it",
    printed(two),
    text_of(gone.model),
  );
  check(bool, "flashed, row mode", true, C.flash^ == 1 && C.text^ == None);
  let (line, code) = live_line(two);
  let code = typing(line, [Insert("c")], code);
  let d = fresh_def();
  C.text := None;
  let named =
    run(
      Settle(
        line,
        {
          f_id: d,
          f_kw: "",
        },
        Done,
      ),
      code,
    );
  check(string, "one that will: kept", named_two("c"), text_of(named));
  check(
    bool,
    "the cursor on its row",
    true,
    C.cursor^ == O.label_path(d, Web.Program.outline(code.program)),
  );
  reset();
};

/* a refusal shown on a new definition's line follows its name as it's
   typed, and goes once the name will do */
let live_rechecks = () => {
  let (line, code) = live_line(two);
  let code = typing(line, [Insert("B")], code);
  let error = () => Option.bind(C.text^, (t: E.t) => t.error);
  C.text :=
    Option.map(
      (t: E.t) =>
        {
          ...t,
          error: Some("value names start lowercase"),
        },
      C.text^,
    );
  let code = typing(line, [Insert("x")], code);
  check(
    option(string),
    "still won't do: shown",
    Some("value names start lowercase"),
    error(),
  );
  let code = typing(line, [SelectAll, Insert("b")], code);
  check(option(string), "will do: gone", None, error());
  ignore(
    typing(line, [Insert("C"), Step(Left, Edge), Insert("C")], code),
  );
  check(option(string), "won't again: not till a save tries", None, error());
  reset();
};

/* a refusal shown on a rename's line follows what's typed there */
let rename_rechecks = () => {
  let visible = H.visible(term);
  let reason = "value names start lowercase";
  let text =
    ref(
      Some({
        ...E.at(line_of(lines(), "alpha"), ~col=1, ~pending="A"),
        error: Some(reason),
      }),
    );
  let on = H.handlers(~text, ());
  /* a stand-in: a capital first won't do */
  let check_name = (t: E.t) =>
    switch (t.pending) {
    | Some(s) when s != "" && Char.uppercase_ascii(s.[0]) == s.[0] =>
      Some(reason)
    | _ => None
    };
  let key = (~code=?, k) =>
    ignore(
      Web.OutlineSidebar.keys(
        ~props=H.props(~check=check_name, ()),
        ~on,
        ~visible,
        H.keydown(~code?, k),
      ),
    );
  let error = () => Option.bind(text^, (t: E.t) => t.error);
  key(~code="KeyB", "b");
  check(option(string), "still won't do: shown", Some(reason), error());
  key("Backspace");
  key("Backspace");
  check(option(string), "will do: gone", None, error());
  key(~code="KeyC", "C");
  check(option(string), "won't again: not till a save tries", None, error());
};

/* a kind change is one undo step, its uses with it */
let kind_undo = () => {
  module U = Test_Undo.Cells;
  let outline = (a: C.Action.t): Web.Page.Update.t =>
    Editors(Scratch(Outline(a)));
  let program = m => CorpusUtil.text_of(Web.Program.document(U.program(m)));
  let m = U.run(U.mk(~src="let m = {\n  let x = 1\n} in\nm.x", ()), Refresh);
  let before = program(m);
  let row = U.row(m, "m");
  let term = Web.Program.outline(U.program(m));
  C.text := None;
  let m =
    U.run(
      m,
      outline(
        Save(
          {
            sv_line:
              Row(Option.get(O.label_path(row, term)), Some(Item(row))),
            sv_text: "module M",
            sv_col: 1,
          },
          Leave,
        ),
      ),
    );
  check(
    string,
    "a module, its use too",
    printed("module M = {\n  let x = 1\n} in\nM.x"),
    program(m),
  );
  check(int, "one undo step", 1, List.length(m.undo_stack));
  check(
    bool,
    "its keyword leaves as a new definition's does",
    true,
    C.created^ == Some((row, "module ")),
  );
  let m = U.run(m, Globals(Undo));
  check(string, "⌘Z: a value again", before, program(m));
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
    test_case("Tab and Shift+Tab nest items", `Quick, nesting),
    test_case("Tab and Shift+Tab on a new line", `Quick, nesting_lines),
    test_case("Enter in and out of modules", `Quick, empty_modules),
    test_case("⌥F folds", `Quick, fold),
    test_case("keys before the field has them", `Quick, early),
    test_case("a new line's keys, as a field's", `Quick, live_keys),
    test_case("pasted fragments; words", `Quick, fragments_and_words),
    test_case("new lines' places", `Quick, new_lines),
    test_case("finding the caret's line", `Quick, finding),
    test_case("blank definitions", `Quick, blank_bodies),
    test_case("new tests take the caret", `Quick, new_tests),
    test_case("a module's new first member", `Quick, first_member),
    test_case("a new definition's name, live", `Quick, live_typing),
    test_case("leaving a new definition's line", `Quick, live_leaving),
    test_case("a keyword picks a new definition's kind", `Quick, live_kinds),
    test_case("a new definition with cells open", `Quick, live_in_stack),
    test_case("keys before a new definition is in", `Quick, keys_wait),
    test_case("⌘Z takes a new definition, name and all", `Quick, live_undo),
    test_case(
      "a dropped definition leaves no undo step",
      `Quick,
      undo_after_drop,
    ),
    test_case("Tab and Shift+Tab keep the caret", `Quick, nesting_ops),
    test_case("a new line in and out of a module", `Quick, module_lines),
    test_case("a new line in and out of a function", `Quick, value_lines),
    test_case("⌘V on a new definition's line", `Quick, live_paste),
    test_case("words in a new definition's name", `Quick, live_words),
    test_case("selecting in a new definition's name", `Quick, live_selection),
    test_case("copy and delete move the caret", `Quick, copy_and_delete),
    test_case("⌥F in row mode", `Quick, row_mode_fold),
    test_case("a folded run's keys", `Quick, run_fold_keys),
    test_case("a folded run in text mode", `Quick, run_text),
    test_case("a new test opens its run", `Quick, new_test_opens_run),
    test_case("menus offer a new test", `Quick, test_menus),
    test_case("a keyword typed first in a rename", `Quick, rename_keywords),
    test_case("Esc leaves in one press", `Quick, esc_leaves),
    test_case("a new line's refusal follows its name", `Quick, live_rechecks),
    test_case("a rename's refusal follows its text", `Quick, rename_rechecks),
    test_case("a kind change is one undo step", `Quick, kind_undo),
  ],
);
