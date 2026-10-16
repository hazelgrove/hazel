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
   when [field]), what's typed [pending]; on a new definition's line, its
   definition [fresh] once in and its name [live] as drawn */
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
      ~fresh=?,
      ~live=?,
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
    ...E.at(line, ~col, ~pending?, ~fresh?),
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
      ("foo'bar", 7, Left, 4),
      ("foo'bar", 4, Left, 0),
      ("foo'bar", 0, Right, 3),
      ("foo'bar", 3, Right, 7),
      ("x''", 3, Left, 0),
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
  let a =
    CorpusUtil.row_by_label(Web.Workspace.fresh_term(code.program), "a");
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
    let a =
      CorpusUtil.row_by_label(Web.Workspace.fresh_term(code.program), "a");
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
    C.cursor^ == O.label_path(d, Web.Workspace.fresh_term(code.program)),
  );
  reset();
};

/* Enter on an expanded module: its new first member, live; Enter after
   its name, a next line below it */
let first_member = () => {
  let code = code_of("module M = {\n  let a = 1;\n  let b = 2\n} in\nM.a");
  let m =
    CorpusUtil.row_by_label(Web.Workspace.fresh_term(code.program), "M");
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

/* `type ` typed as a value's name makes it a type, blank; ⌫ at its
   start makes it a value again: still the step that made it */
let live_kinds = () => {
  let code = code_of("let a = 1 in\na");
  let a =
    CorpusUtil.row_by_label(Web.Workspace.fresh_term(code.program), "a");
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
  let term = Web.Workspace.fresh_term(code.program);
  let a = CorpusUtil.row_by_label(term, "a");
  let code =
    Web.Workspace.realize(
      ~info_map=
        Web.Workspace.fresh_info_map(
          ~settings=Web.Globals.Model.init().settings,
          code.program,
          term,
        ),
      ~term,
      Web.SlideView.pin(~term, a, code.view),
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
  let a =
    CorpusUtil.row_by_label(Web.Workspace.fresh_term(code.program), "a");
  let line = E.New(a, Below);
  C.text := Some(E.at(line));
  (line, run(Create(line, ""), code));
};
let two = "let a = 1 in\nlet b = 2 in\na + b";
/* the program with the new definition named [name] */
let named_two = name =>
  printed(
    "let a = 1 in\nlet " ++ name ++ {js| = ¿ in
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

/* ⌥⌫ ⌥⌦ ⌥← ⌥→ by words: runs of letters, digits and underscores */
let live_words = () => {
  let (line, code) = live_line(two);
  let code = typing(line, [Paste("ab'cd'ef")], code);
  let code = typing(line, [Erase(Left, Word)], code);
  check(
    at_name,
    {js|⌥⌫: the last word|js},
    Some(("ab'cd'", 6)),
    live_of(code),
  );
  check(string, "in the program", named_two("ab'cd'"), text_of(code));
  let code = typing(line, [Erase(Left, Word)], code);
  check(
    at_name,
    "again: the ' and the word before it",
    Some(("ab'", 3)),
    live_of(code),
  );
  let code = typing(line, [Step(Left, Word)], code);
  check(at_name, {js|⌥←|js}, Some(("ab'", 0)), live_of(code));
  let code = typing(line, [Step(Right, Word)], code);
  check(at_name, {js|⌥→|js}, Some(("ab'", 2)), live_of(code));
  let code = typing(line, [Paste("x_y"), Step(Left, Char)], code);
  let code = typing(line, [Erase(Right, Word)], code);
  check(
    at_name,
    {js|⌥⌦: the rest of the word, _ and all|js},
    Some(("abx_'", 4)),
    live_of(code),
  );
  check(string, "in the program", named_two("abx_'"), text_of(code));
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
      O.flatten(O.of_term(term)),
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
    test_case("⌘V on a new definition's line", `Quick, live_paste),
    test_case("words in a new definition's name", `Quick, live_words),
    test_case("selecting in a new definition's name", `Quick, live_selection),
    test_case("copy and delete move the caret", `Quick, copy_and_delete),
    test_case("⌥F in row mode", `Quick, row_mode_fold),
    test_case("menus offer a new test", `Quick, test_menus),
  ],
);
