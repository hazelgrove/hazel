open Util;

/* The outline's text mode: a line-based text area of names. Every shown
   row is a line; a nameable row's line is its name, any other row is a
   ghost line with one caret position, at its start. A new definition's
   line is the definition itself, in the program from the line's start:
   its name is typed there, by the editor. Here are the caret, the text
   typed on a row's line, and what each key does, as data: the sidebar
   turns outcomes into effects, and OutlineControl saves names to the
   program. */

module Id = Language.Id;

/* structural operations on a definition, from its row's context menu;
   ItemEdit applies them at the item's owning block */
[@deriving (show({with_path: false}), sexp, yojson)]
type def_op =
  | NewBelow
  | NewTypeBelow
  | NewModuleBelow
  /* a `test ¿ end` below the item, or above it: the first of a run */
  | NewTestBelow
  | NewTestAbove
  | Duplicate
  | MoveUp
  | MoveDown
  /* Tab: into the module or value above, last; ⇧Tab: a module's or
     value's last item out, just after it */
  | Indent
  | Outdent
  | Delete;

/* a typed definition's kind from its leading keyword, and the keyword:
   `type `, `module `, or `let ` (a value, as no keyword is) */
let new_kind = (text: string): (OutlineTree.kind, string) =>
  String.starts_with(~prefix="type ", text)
    ? (KType, "type ")
    : String.starts_with(~prefix="module ", text)
        ? (KModule, "module ")
        : String.starts_with(~prefix="let ", text)
            ? (KConst, "let ") : (KConst, "");

/* a rename's typed text: its leading kind keyword, shown apart from the
   field, and the field's text */
let split_typed = (text: string): (string, string) => {
  let (_, kw) = new_kind(text);
  let n = String.length(kw);
  (kw, String.sub(text, n, String.length(text) - n));
};

/* a kind keyword still short of its space: refused if saved as a name,
   but not yet underlined while typed, as the space makes it a kind */
let kind_pending = (text: string): bool =>
  List.mem(text, ["type", "module", "let"]);

/* where a new definition goes: below its anchor, or first or last inside
   it (a module) */
[@deriving (show({with_path: false}), sexp, yojson)]
type place =
  | Below
  | First
  | Last;

/* a row's identity across renames (its path is made of labels): its id,
   or a tests label's first test's */
[@deriving (show({with_path: false}), sexp, yojson)]
type key =
  | Item(Id.t)
  | Run(Id.t);

/* the caret's line: a shown row, or a new definition being named */
[@deriving (show({with_path: false}), sexp, yojson)]
type line =
  | Row(OutlineTree.path, option(key))
  | New(Id.t, place);

/* a New line's definition, once it's in the program, and its keyword
   (`type ` or `module `, else none): its name is typed there, live */
[@deriving (show({with_path: false}), sexp, yojson)]
type fresh = {
  f_id: Id.t,
  f_kw: string,
};

/* the line Enter starts once new definition [f] is named: below it, or
   first inside it, a module (an empty one) */
let next_line = (f: fresh): line =>
  New(f.f_id, fst(new_kind(f.f_kw)) == KModule ? First : Below);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  line,
  /* in the line's field, which leaves out a keyword typed first */
  col: int,
  /* the column ↑↓ aim for: set by the first vertical move, kept by the
     next ones, cleared by any other key */
  col_target: option(int),
  /* typed on the line, not yet saved: a kind keyword, then the field's
     text */
  pending: option(string),
  /* why the line's name can't be saved: shown once a save has tried it,
     kept up to date as it's typed, gone once the name will do */
  error: option(string),
  /* bumped as the keys place the caret: the field then moves its own */
  seq: int,
  fresh: option(fresh),
};

let at = (~col=0, ~pending=?, ~seq=0, ~fresh=?, line: line): t => {
  line,
  col,
  col_target: None,
  pending,
  error: None,
  seq,
  fresh,
};

/* a new definition's name as the program has it, the caret's place in
   it and the selection's other end (the caret's place when nothing is
   selected), as last drawn */
type live = {
  l_name: string,
  l_at: int,
  l_anchor: int,
};

/* how far a key reaches from the caret: a character, a word, or the
   name's start or end */
[@deriving (show({with_path: false}), sexp, yojson)]
type reach =
  | Char
  | Word
  | Edge;

/* a key on a new definition's line, for the editor to apply to its name.
   A selection is replaced by what's typed or pasted, and erased by an
   erase whatever its reach */
[@deriving (show({with_path: false}), sexp, yojson)]
type edit =
  | Insert(string)
  /* the clipboard's text: refused unless it can be part of a name */
  | Paste(string)
  | Erase(Direction.t, reach)
  | Step(Direction.t, reach)
  /* the selection's moving end */
  | Select(Direction.t, reach)
  | SelectAll
  /* the selection erased, if there is one */
  | Cut;

/* a name to save, from the line it was typed on; the caret's column comes
   back with the line if the save is refused */
[@deriving (show({with_path: false}), sexp, yojson)]
type save = {
  sv_line: line,
  sv_text: string,
  sv_col: int,
};

/* what follows a save: the caret where the keys put it, a new definition
   below what was saved, or row mode; or ([Done]) row mode whatever the
   save's outcome, a refused name undone and its row flashed */
[@deriving (show({with_path: false}), sexp, yojson)]
type after =
  | Stay
  | Next
  | Leave
  | Done;

/* a save that ends text mode */
let exits = (a: after): bool => a == Leave || a == Done;

/* a context menu's row: an item (its kind picks the menu's rows), or a
   tests label, by its first and last tests, and whether it's expanded */
type menu_at =
  | On_item(Id.t, OutlineTree.kind)
  | On_run(Id.t, Id.t, bool);

/* a row on screen, as the keys see it */
type visible_row = {
  r_path: OutlineTree.path,
  /* its label may be an open cell's live header text */
  r_node: OutlineTree.node,
  /* the row's name in the program */
  r_name: string,
  r_parent: option(OutlineTree.path),
  r_expanded: bool,
};

/* rows with a name to type; tests, ⇒, statements and the like have none */
let nameable = (n: OutlineTree.node): bool =>
  n.o_id != None
  && (
    switch (n.o_kind) {
    | KFn
    | KConst
    | KType
    | KModule => true
    | _ => false
    }
  );

let key_of_node = (n: OutlineTree.node): option(key) =>
  switch (n.o_id, n.o_kind) {
  | (Some(id), _) => Some(Item(id))
  | (None, KTests) =>
    List.find_map((c: OutlineTree.node) => c.o_id, n.o_children)
    |> Option.map(id => Run(id))
  | (None, _) => None
  };
let key_of = (r: visible_row): option(key) => key_of_node(r.r_node);

/* what the editor shows for a row: a test's own term, a run's first */
let shown_of_node = (n: OutlineTree.node): option(Id.t) => {
  let own = (n: OutlineTree.node) =>
    switch (n.o_kind, n.o_test) {
    | (KTest, Some(_) as t) => t
    | _ => n.o_id
    };
  n.o_kind == KTests ? List.find_map(own, n.o_children) : own(n);
};

/* the item a key names, if a row has one */
let item_of = (k: option(key)): option(Id.t) =>
  switch (k) {
  | Some(Item(id)) => Some(id)
  | _ => None
  };

/* text mode's lines: the shown rows, and a new definition being named */
type entry =
  | Of_row(visible_row)
  | Of_new(Id.t, place);

let line_of = (e: entry): line =>
  switch (e) {
  | Of_row(r) => Row(r.r_path, key_of(r))
  | Of_new(a, p) => New(a, p)
  };

let shown = (e: entry): option(Id.t) =>
  switch (e) {
  | Of_row(r) => shown_of_node(r.r_node)
  | Of_new(a, _) => Some(a)
  };

/* [q] is below [p] in the tree */
let rec under = (p: OutlineTree.path, q: OutlineTree.path): bool =>
  switch (p, q) {
  | ([], [_, ..._]) => true
  | ([a, ...p], [b, ...q]) => a == b && under(p, q)
  | _ => false
  };

/* the rows as lines, with a new definition's after its anchor: first
   inside it, or past the rows under it (below it, or last inside it, but
   above a function's ⇒, which stays last) */
let lines = (~text: option(t), visible: array(visible_row)): array(entry) => {
  let rows = Array.map(r => Of_row(r), visible);
  switch (text) {
  | Some({line: New(a, place), _}) =>
    switch (Array.find_index(r => r.r_node.o_id == Some(a), visible)) {
    | None => rows
    | Some(i) =>
      let n = Array.length(visible);
      let rec past = j =>
        j < n && under(visible[i].r_path, visible[j].r_path)
          ? past(j + 1) : j;
      let trail = j =>
        j > i
        && visible[j].r_node.o_kind == KTrail
        && visible[j].r_parent == Some(visible[i].r_path);
      let k =
        switch (place) {
        | First => i + 1
        | Below => past(i + 1)
        | Last =>
          let j = past(i + 1);
          trail(j - 1) ? j - 1 : j;
        };
      Array.concat([
        Array.sub(rows, 0, k),
        [|Of_new(a, place)|],
        Array.sub(rows, k, n - k),
      ]);
    }
  | _ => rows
  };
};

/* a new definition's line, named in the page */
let new_key = (a: Id.t, p: place): string =>
  Id.to_string(a) ++ ":" ++ show_place(p);

/* the same line: a row by its key when known (renames change paths) */
let same_line = (a: line, b: line): bool =>
  switch (a, b) {
  | (Row(_, Some(k)), Row(_, Some(k'))) => k == k'
  | (Row(p, _), Row(p', _)) => p == p'
  | (New(x, y), New(x', y')) => x == x' && y == y'
  | _ => false
  };

/* the caret's line: by key (a rename keeps it), by path, else the
   nearest shown row above it (folded away), else near where it was last
   drawn ([last]: its index then; a vanished line's caret goes up one) */
let find =
    (~last: option(int)=?, lines: array(entry), line: line): option(int) => {
  let row_at = (f: visible_row => bool) =>
    Array.find_index(
      fun
      | Of_row(r) => f(r)
      | Of_new(_) => false,
      lines,
    );
  let near = () =>
    switch (last) {
    | Some(i) when Array.length(lines) > 0 =>
      Some(max(0, min(i - 1, Array.length(lines) - 1)))
    | _ => None
    };
  switch (line) {
  | New(a, p) =>
    switch (
      Array.find_index(
        fun
        | Of_new(a', p') => a == a' && p == p'
        | Of_row(_) => false,
        lines,
      )
    ) {
    | Some(i) => Some(i)
    | None => near()
    }
  | Row(path, key) =>
    let by_key = key == None ? None : row_at(r => key_of(r) == key);
    switch (by_key) {
    | Some(i) => Some(i)
    | None =>
      switch (row_at(r => r.r_path == path)) {
      | Some(i) => Some(i)
      | None =>
        let rec up = p =>
          switch (ListUtil.split_last_opt(p)) {
          | Some(([_, ..._] as above, _)) =>
            switch (row_at(r => r.r_path == above)) {
            | Some(i) => Some(i)
            | None => up(above)
            }
          | _ => near()
          };
        up(path);
      }
    };
  };
};

/* JS string units: the field's selection counts these */
let units = (s: string): int => Js_of_ocaml.Js.string(s)##.length;
let slice = (s: string, a: int, b: int): string =>
  Js_of_ocaml.Js.to_string(Js_of_ocaml.Js.string(s)##substring(a, b));

/* a rename's field typed to [field], its caret at [col], under keyword
   [kw]: the line's typed text, and the caret in the field it shows (a
   keyword typed at the field's start moves out of it) */
let retyped = (kw: string, field: string, col: int): (string, int) => {
  let text = kw ++ field;
  let (kw', _) = split_typed(text);
  (text, max(0, col + units(kw) - units(kw')));
};

/* where a word ends from [at] toward [side] in [name]: past any
   separators, then the word. Words as a text field reads them: letters,
   digits, underscores and anything outside ASCII, an apostrophe between
   two of them included (`can't`) */
let word_edge = (name: string, at: int, side: Direction.t): int => {
  let n = units(name);
  let wordish = i =>
    i >= 0
    && i < n
    && {
      let c = slice(name, i, i + 1);
      String.length(c) > 1 || Language.Token.is_wordish_char(c.[0]);
    };
  let word = i =>
    wordish(i)
    || slice(name, i, i + 1) == "'"
    && wordish(i - 1)
    && wordish(i + 1);
  let rec skip = (i, f) =>
    switch (side) {
    | Left => i > 0 && f(i - 1) ? skip(i - 1, f) : i
    | Right => i < n && f(i) ? skip(i + 1, f) : i
    };
  skip(skip(max(0, min(at, n)), i => !word(i)), word);
};

/* where [reach] from [at] toward [side] ends in [name] */
let reach_to = (name: string, at: int, side: Direction.t, r: reach): int =>
  switch (r, side) {
  | (Char, Left) => max(0, at - 1)
  | (Char, Right) => min(units(name), at + 1)
  | (Word, _) => word_edge(name, at, side)
  | (Edge, Left) => 0
  | (Edge, Right) => units(name)
  };

/* clipboard text that can go into a name: its ends trimmed, the rest
   name characters, but no `.` (qualified names, floats) or the `?^$`
   that start or end special tokens */
let fragment = (text: string): option(string) => {
  let s = String.trim(text);
  s != ""
  && Language.Token.is_potential_operand(s)
  && !String.exists(c => String.contains(".?^$", c), s)
    ? Some(s) : None;
};

/* a line's name off the caret: a nameable row's (a new definition's
   line is only ever the caret's) */
let name_of_line = (e: entry): string =>
  switch (e) {
  | Of_row(r) when nameable(r.r_node) => r.r_name
  | _ => ""
  };

/* a key, as text mode reads it */
type press = {
  key: string,
  code: string,
  alt: bool,
  /* ⌘, or Ctrl off the Mac */
  meta: bool,
  /* Ctrl on the Mac: the field's own (emacs) keys */
  ctrl: bool,
  shift: bool,
  composing: bool,
};

/* the caret where a key finds it: in the line's field (its selection),
   or, before the field has the keys, the model's */
type caret = {
  in_field: bool,
  c_col: int,
  c_sel: bool,
};

[@deriving show({with_path: false})]
type outcome =
  /* the field's own: typing, moves within the name */
  | Field
  /* the page's (undo, with nothing typed on the line) */
  | Page
  /* nothing */
  | Swallow
  /* refused */
  | Flash
  /* save the line; the caret to [line], at a column, ↑↓'s column kept */
  | Go(line, int, option(int))
  /* the caret within the line */
  | Place(int)
  /* the line's text and caret, as typed while the field lacked the keys */
  | Edit(string, int)
  /* what's typed on the line, undone */
  | Revert
  | Save(after)
  | NewTest(def_op, Id.t)
  /* save, then an op on the line's item */
  | Op(def_op, Id.t)
  /* a blank item deleted, the caret to a neighbour line */
  | Remove(Id.t, option((line, int)))
  | Fold(OutlineTree.path)
  | Menu
  /* save, row mode, and back to the editor */
  | Exit
  /* a new definition's name, edited in the program */
  | Type(edit)
  /* the clipboard's text, typed into a new definition's name */
  | Paste
  /* a new definition's selection to the clipboard, and with [cut] out of
     the name */
  | Copy(string, bool)
  /* a new definition remade as a keyword's kind (none: a value) */
  | Rekind(string)
  /* a new definition's line moved a level in or out, to [line]: its
     definition goes there */
  | Shift(line)
  /* a new definition's line gone, its definition with it: the caret to a
     neighbour line */
  | Drop(Id.t, option((line, int)))
  /* the page's undo, which takes a new definition away: the caret to
     [line] first */
  | Undo(line, int);

/* the line where Enter puts a new definition, for a nameable row: first
   inside an expanded module or an empty one ([blank]), else below */
let new_place = (~blank: Id.t => bool, r: visible_row): place =>
  r.r_node.o_kind == KModule
  && (r.r_expanded || Option.fold(~none=false, ~some=blank, r.r_node.o_id))
    ? First : Below;

let len_of_line = (e: entry): int => units(name_of_line(e));

let ghost_line = (e: entry): bool =>
  switch (e) {
  | Of_row(r) => !nameable(r.r_node)
  | Of_new(_) => false
  };

/* the row at [path] among the lines */
let row_at =
    (lines: array(entry), path: OutlineTree.path): option(visible_row) =>
  Array.find_map(
    fun
    | Of_row(r) when r.r_path == path => Some(r)
    | _ => None,
    lines,
  );

/* the row whose items [r] is one of, a run's tests being their level's
   items; none at the top level */
let level_row = (lines: array(entry), r: visible_row): option(visible_row) =>
  switch (Option.bind(r.r_parent, row_at(lines))) {
  | Some(run) when run.r_node.o_kind == KTests =>
    Option.bind(run.r_parent, row_at(lines))
  | parent => parent
  };

/* where a line sits among the items of its level, as the program has
   them: the row the level is in (none: the top level), and the items
   just above and below the line there */
type beside = {
  b_level: option(visible_row),
  b_above: option(OutlineTree.node),
  b_below: option(OutlineTree.node),
};

/* a line's [beside]; a run of tests counts as its tests, and [skip] (a
   new definition's own row) isn't there */
let beside =
    (~skip: option(Id.t), lines: array(entry), e: entry): option(beside) => {
  let items = (level: option(visible_row)) =>
    (
      switch (level) {
      | Some(r) => r.r_node.o_children
      | None =>
        List.filter_map(
          fun
          | Of_row({r_parent: None, r_node, _}) => Some(r_node)
          | _ => None,
          Array.to_list(lines),
        )
      }
    )
    |> List.concat_map((n: OutlineTree.node) =>
         n.o_kind == KTests ? n.o_children : [n]
       )
    |> List.filter((n: OutlineTree.node) => skip == None || n.o_id != skip);
  /* item [id] in [level]'s, and the items before and after it */
  let split = (level, id) => {
    let rec go = (above, ns: list(OutlineTree.node)) =>
      switch (ns) {
      | [] => None
      | [n, ...rest] when n.o_id == Some(id) => Some((above, n, rest))
      | [n, ...rest] => go(Some(n), rest)
      };
    go(None, items(level));
  };
  let row_of = (id: Id.t) =>
    Array.find_map(
      fun
      | Of_row(r) when r.r_node.o_id == Some(id) => Some(r)
      | _ => None,
      lines,
    );
  switch (e) {
  | Of_row(r) =>
    Option.bind(
      r.r_node.o_id,
      id => {
        let level = level_row(lines, r);
        Option.map(
          ((above, _, rest)) =>
            {
              b_level: level,
              b_above: above,
              b_below: ListUtil.hd_opt(rest),
            },
          split(level, id),
        );
      },
    )
  | Of_new(a, Below) =>
    Option.bind(
      row_of(a),
      r => {
        let level = level_row(lines, r);
        Option.map(
          ((_, n, rest)) =>
            {
              b_level: level,
              b_above: Some(n),
              b_below: ListUtil.hd_opt(rest),
            },
          split(level, a),
        );
      },
    )
  | Of_new(a, place) =>
    Option.map(
      r => {
        let ns = items(Some(r));
        switch (place, ListUtil.split_last_opt(ns)) {
        | (First, _) => {
            b_level: Some(r),
            b_above: None,
            b_below: ListUtil.hd_opt(ns),
          }
        /* last in a function: above its ⇒ */
        | (_, Some((init, {o_kind: KTrail, _} as trail))) => {
            b_level: Some(r),
            b_above: ListUtil.last_opt(init),
            b_below: Some(trail),
          }
        | _ => {
            b_level: Some(r),
            b_above: ListUtil.last_opt(ns),
            b_below: None,
          }
        };
      },
      row_of(a),
    )
  };
};

/* Tab takes an item of kind [item] into the item [above] it: a module
   (never a `use`), or a value, as its body's last local definition */
let joins = (~above: OutlineTree.node, item: OutlineTree.kind): bool =>
  switch (above.o_kind, item) {
  | (KModule, KUse) => false
  | (KModule | KFn | KConst, _) => true
  | _ => false
  };

/* ⇧Tab takes the last item of [level] out of it, [below] being what
   follows that item there: a module's last member, or a value's last
   local definition, a function's ⇒ staying last */
let leaves = (~level: visible_row, below: option(OutlineTree.node)): bool =>
  switch (level.r_node.o_kind, below) {
  | (KModule, None)
  | (KFn | KConst, None | Some({o_kind: KTrail, _})) => true
  | _ => false
  };

/* what key [p] does on new definition line [at]: its name is typed into
   the program by the editor, [live] as last drawn (none yet: blank). Keys
   can outrun the drawing, so what happens in the name (at its ends too:
   a refusal, or nothing) is the editor's to decide */
let live_key =
    (
      ~lines: array(entry),
      ~at: int,
      ~t: t,
      ~live: option(live),
      ~page: unit => int,
      p: press,
    )
    : outcome => {
  let n = Array.length(lines);
  let (name, col, anchor) =
    switch (live) {
    | Some(l) => (l.l_name, l.l_at, l.l_anchor)
    | None => ("", 0, 0)
    };
  let len = units(name);
  let sel = anchor != col;
  let to_end = j =>
    Go(
      line_of(lines[j]),
      ghost_line(lines[j]) ? 0 : len_of_line(lines[j]),
      None,
    );
  let to_start = j => Go(line_of(lines[j]), 0, None);
  let vertical = (~up, j) =>
    if (j == at) {
      Type(Step(up ? Left : Right, Edge));
    } else {
      let target = Option.value(t.col_target, ~default=col);
      Go(
        line_of(lines[j]),
        ghost_line(lines[j]) ? 0 : min(target, len_of_line(lines[j])),
        Some(target),
      );
    };
  let at_side = (side: Direction.t) => side == Left ? col == 0 : col >= len;
  /* ←→ Home End: the caret by [r], or with Shift the selection's moving
     end; a character past the name's end goes on to the next line */
  let horizontal = (side: Direction.t, r: reach) =>
    if (p.shift) {
      Type(Select(side, r));
    } else if (sel || !at_side(side)) {
      Type(Step(side, r));
    } else if (r != Char) {
      Swallow;
    } else if (side == Left) {
      at > 0 ? to_end(at - 1) : Swallow;
    } else {
      at < n - 1 ? to_start(at + 1) : Swallow;
    };
  /* the line above, where the caret goes when this one does */
  let above = () => at > 0 ? to_end(at - 1) : Save(Leave);
  let blank = String.trim(name) == "";
  let kw = Option.fold(~none="", ~some=f => f.f_kw, t.fresh);
  let alone = !p.alt && !p.meta && !p.shift;
  /* where the line goes in a level ([out]: out of one): last into the
     module or value above it; out of the module or value it's the last
     line of, just after that */
  let shifted = (~out) =>
    switch (
      beside(~skip=Option.map(f => f.f_id, t.fresh), lines, lines[at]),
      out,
    ) {
    | (Some({b_above: Some({o_id: Some(m), _} as above), _}), false)
        when joins(~above, fst(new_kind(kw))) =>
      Some(New(m, Last))
    | (Some({b_level: Some(level), b_below, _}), true)
        when leaves(~level, b_below) =>
      Option.map(m => New(m, Below), level.r_node.o_id)
    | _ => None
    };
  switch (p.key) {
  /* IME and dead keys compose text in a field, and this line isn't one:
     refused */
  | _ when p.composing => Flash
  | "Dead"
  | "Process" => Flash
  | "Shift"
  | "Alt"
  | "Meta"
  | "Control"
  | "CapsLock" => Page
  | "Escape" => Save(Done)
  | "Enter" when p.meta || p.shift => Save(Leave)
  | "Enter" when p.alt || p.ctrl => Swallow
  /* a blank last line in a module or a value's body goes out of it, as
     ⇧Tab takes it; elsewhere it's gone, and text mode with it */
  | "Enter" when blank =>
    switch (shifted(~out=true)) {
    | Some(to_) => Shift(to_)
    | None => Save(Leave)
    }
  | "Enter" => Save(Next)
  | "Tab" when !p.alt && !p.meta && !p.ctrl =>
    switch (shifted(~out=p.shift)) {
    | Some(to_) => Shift(to_)
    | None => Flash
    }
  | "Tab"
  | "F2" => Swallow
  /* not an item to move yet */
  | "ArrowUp"
  | "ArrowDown" when p.alt && !p.meta && !p.shift => Flash
  | "ArrowUp" when p.meta && !p.alt && !p.shift =>
    at == 0 ? Type(Step(Left, Edge)) : to_start(0)
  | "ArrowDown" when p.meta && !p.alt && !p.shift =>
    at == n - 1 ? Type(Step(Right, Edge)) : to_end(n - 1)
  | "ArrowUp" when alone => vertical(~up=true, max(0, at - 1))
  | "ArrowDown" when alone => vertical(~up=false, min(n - 1, at + 1))
  | "PageUp" when alone => vertical(~up=true, max(0, at - page()))
  | "PageDown" when alone => vertical(~up=false, min(n - 1, at + page()))
  /* ⌥: a word; ⌘: the name's start or end */
  | ("ArrowLeft" | "ArrowRight") as k when !p.ctrl && !(p.alt && p.meta) =>
    horizontal(
      k == "ArrowLeft" ? Left : Right,
      p.alt ? Word : p.meta ? Edge : Char,
    )
  | ("Home" | "End") as k when !p.alt && !p.meta && !p.ctrl =>
    horizontal(k == "Home" ? Left : Right, Edge)
  | "ArrowUp"
  | "ArrowDown"
  | "ArrowLeft"
  | "ArrowRight"
  | "Home"
  | "End"
  | "PageUp"
  | "PageDown" => Swallow
  /* ⌘⌫: the definition goes, named or not */
  | "Backspace" when p.meta =>
    switch (t.fresh) {
    | Some(f) =>
      Drop(
        f.f_id,
        at > 0
          ? Some((
              line_of(lines[at - 1]),
              ghost_line(lines[at - 1]) ? 0 : len_of_line(lines[at - 1]),
            ))
          : None,
      )
    | None => above()
    }
  | "Backspace" when p.ctrl => Swallow
  | "Backspace" when !blank => Type(Erase(Left, p.alt ? Word : Char))
  /* at a blank name's start: a type or module's keyword goes first */
  | "Backspace" when kw != "" && t.fresh != None => Rekind("")
  /* a blank name: the line goes, and its definition with it */
  | "Backspace" => above()
  | "Delete" when !p.meta && !p.ctrl && !p.shift && !blank =>
    Type(Erase(Right, p.alt ? Word : Char))
  | "Delete" => Flash
  | _ when p.alt && !p.meta && p.code == "KeyF" => Flash
  | _ when p.alt && !p.meta && p.code == "KeyO" => Exit
  | "." when p.meta => Menu
  | "F10" when p.shift => Menu
  | _ when p.meta && !p.alt && !p.shift && p.code == "KeyD" => Flash
  /* the page's undo takes the definition away, typed name and all */
  | _
      when
        p.meta && !p.alt && !p.shift && String.lowercase_ascii(p.key) == "z" =>
    switch (t.fresh) {
    | Some(_) when at > 0 =>
      Undo(
        line_of(lines[at - 1]),
        ghost_line(lines[at - 1]) ? 0 : len_of_line(lines[at - 1]),
      )
    | Some(_) => Page
    /* not in yet: the Enter that made it */
    | None => above()
    }
  | _
      when
        p.meta && !p.alt && !p.shift && String.lowercase_ascii(p.key) == "a" =>
    Type(SelectAll)
  | _
      when
        p.meta && !p.alt && !p.shift && String.lowercase_ascii(p.key) == "v" =>
    Paste
  /* what's drawn: what was just seen selected (the clipboard is written
     as the key goes down) */
  | ("c" | "C" | "x" | "X") as c when p.meta && !p.alt && !p.shift =>
    sel
      ? Copy(
          slice(name, min(col, anchor), max(col, anchor)),
          String.lowercase_ascii(c) == "x",
        )
      : Swallow
  | _ when p.meta => Page
  | _ when p.ctrl => Swallow
  /* a value's name begun `type ` or `module `: that kind instead */
  | " "
      when
        kw == ""
        && t.fresh != None
        && !sel
        && col == len
        && (name == "type" || name == "module") =>
    Rekind(name ++ " ")
  | _ when Unicode.count_codepoints(p.key) == 1 => Type(Insert(p.key))
  | _ => Swallow
  };
};

/* what key [p] does on row [r]'s line, [at] */
let decide_row =
    (
      ~lines: array(entry),
      ~at: int,
      ~r: visible_row,
      ~t: t,
      ~caret: caret,
      ~blank: Id.t => bool,
      ~page: unit => int,
      p: press,
    )
    : outcome => {
  let n = Array.length(lines);
  let ghost = j => ghost_line(lines[j]);
  let here_ghost = !nameable(r.r_node);
  /* a kind keyword typed first, shown apart, and the field's text */
  let (kw, value) =
    here_ghost
      ? ("", "") : split_typed(Option.value(t.pending, ~default=r.r_name));
  let len = units(value);
  /* a line's length in its field */
  let len_of = j => j == at ? len : len_of_line(lines[j]);
  let col =
    here_ghost ? 0 : caret.in_field ? caret.c_col : max(0, min(t.col, len));
  let sel = caret.in_field && caret.c_sel;
  /* the field's own key, or the model's while the field lacks the keys */
  let own = o => caret.in_field ? Field : o;
  let to_end = j => Go(line_of(lines[j]), ghost(j) ? 0 : len_of(j), None);
  let to_start = j => Go(line_of(lines[j]), 0, None);
  /* past the first or last line: to its start or end */
  let vertical = (~up, j) =>
    if (j == at) {
      Place(up ? 0 : len);
    } else {
      let target = Option.value(t.col_target, ~default=col);
      Go(
        line_of(lines[j]),
        ghost(j) ? 0 : min(target, len_of(j)),
        Some(target),
      );
    };
  let item = r.r_node.o_id;
  /* an op on the line's item: never the ⇒, which stays last */
  let op = o =>
    switch (item) {
    | Some(id) when r.r_node.o_kind != KTrail => Op(o, id)
    | _ => Flash
    };
  let unsaved = t.pending != None && t.pending != Some(r.r_name);
  /* [s] into the name at the caret: the new text, and the caret after it */
  let typed = (s: string, a: int, b: int) => {
    let (text, col) =
      retyped(
        kw,
        slice(value, 0, a) ++ s ++ slice(value, b, len),
        a + units(s),
      );
    Edit(text, col);
  };
  let alone = !p.alt && !p.meta && !p.shift;
  switch (p.key) {
  | _ when p.composing => Field
  | "Shift"
  | "Alt"
  | "Meta"
  | "Control"
  | "CapsLock" => Page
  | "Escape" => Save(Done)
  | "Enter" when p.meta || p.shift => Save(Leave)
  | "Enter" when p.alt || p.ctrl => Swallow
  | "Enter" when !here_ghost =>
    switch (item) {
    | Some(id) => Go(New(id, new_place(~blank, r)), 0, None)
    | None => Flash
    }
  | "Enter" =>
    switch (r.r_node.o_kind, item) {
    | (KTrail, _) => Flash
    | (KTest, Some(id)) => NewTest(NewTestBelow, id)
    | (KTests, _) =>
      switch (
        List.filter_map((c: OutlineTree.node) => c.o_id, r.r_node.o_children)
      ) {
      | [] => Flash
      | [first, ..._] as ids =>
        r.r_expanded
          ? NewTest(NewTestAbove, first)
          : NewTest(NewTestBelow, ListUtil.last(ids))
      }
    | (_, Some(id)) => Go(New(id, Below), 0, None)
    | (_, None) => Flash
    }
  /* Tab: into the module or value above, last; ⇧Tab: out of the one
     it's the last item of */
  | "Tab" when !p.alt && !p.meta && !p.ctrl =>
    switch (beside(~skip=None, lines, lines[at]), p.shift) {
    | (Some({b_above: Some(above), _}), false)
        when joins(~above, r.r_node.o_kind) =>
      op(Indent)
    | (Some({b_level: Some(level), b_below, _}), true)
        when leaves(~level, b_below) =>
      op(Outdent)
    | _ => Flash
    }
  | "Tab"
  | "F2" => Swallow
  | "ArrowUp"
  | "ArrowDown" when p.alt && !p.meta && !p.shift =>
    op(p.key == "ArrowUp" ? MoveUp : MoveDown)
  | "ArrowUp" when p.meta && !p.alt && !p.shift =>
    at == 0 ? Place(0) : to_start(0)
  | "ArrowDown" when p.meta && !p.alt && !p.shift =>
    at == n - 1 ? Place(len) : to_end(n - 1)
  | "ArrowUp" when alone => vertical(~up=true, max(0, at - 1))
  | "ArrowDown" when alone => vertical(~up=false, min(n - 1, at + 1))
  | "PageUp" when alone => vertical(~up=true, max(0, at - page()))
  | "PageDown" when alone => vertical(~up=false, min(n - 1, at + page()))
  | "ArrowLeft" when alone =>
    here_ghost || !sel && col == 0
      ? at > 0 ? to_end(at - 1) : Swallow : own(Place(col - 1))
  | "ArrowRight" when alone =>
    here_ghost || !sel && col >= len
      ? at < n - 1 ? to_start(at + 1) : Swallow : own(Place(col + 1))
  | "Home" when alone && !here_ghost => own(Place(0))
  | "End" when alone && !here_ghost => own(Place(len))
  | "ArrowUp"
  | "ArrowDown"
  | "ArrowLeft"
  | "ArrowRight"
  | "Home"
  | "End"
  | "PageUp"
  | "PageDown" => here_ghost ? Swallow : own(Swallow)
  | "Backspace" when p.meta => op(Delete)
  | "Backspace" when here_ghost => Flash
  /* at the field's start, a keyword typed before it goes first */
  | "Backspace" when kw != "" && !sel && col == 0 => Edit(value, 0)
  | "Backspace" when sel || col > 0 =>
    p.alt || p.ctrl ? own(Swallow) : own(typed("", col - 1, col))
  | "Backspace" =>
    switch (item) {
    | Some(id) when String.trim(value) == "" && blank(id) =>
      Remove(
        id,
        at > 0
          ? Some((
              line_of(lines[at - 1]),
              ghost(at - 1) ? 0 : len_of(at - 1),
            ))
          : at < n - 1 ? Some((line_of(lines[at + 1]), 0)) : None,
      )
    /* rows never merge */
    | _ => Flash
    }
  | "Delete" when here_ghost => Flash
  | "Delete" when alone && (sel || col < len) => own(typed("", col, col + 1))
  | "Delete" when alone => Flash
  | _ when p.alt && !p.meta && p.code == "KeyF" =>
    r.r_node.o_children != [] ? Fold(r.r_path) : Flash
  | _ when p.alt && !p.meta && p.code == "KeyO" => Exit
  | "." when p.meta => Menu
  | "F10" when p.shift => Menu
  | _ when p.meta && !p.alt && !p.shift && p.code == "KeyD" => op(Duplicate)
  | _ when p.meta && !p.alt && String.lowercase_ascii(p.key) == "z" =>
    p.shift ? Page : unsaved ? Revert : Page
  | _ when p.meta => here_ghost ? Page : Field
  | _ when p.ctrl => here_ghost ? Swallow : Field
  | _ when Unicode.count_codepoints(p.key) == 1 =>
    here_ghost ? Flash : own(typed(p.key, col, col))
  | _ => here_ghost ? Swallow : Field
  };
};

/* what key [p] does on line [at]. [blank]: an item whose definition is
   only holes; [page]: a screenful of lines; [live]: a new definition's
   name */
let decide =
    (
      ~lines: array(entry),
      ~at: int,
      ~t: t,
      ~caret: caret,
      ~blank: Id.t => bool,
      ~page: unit => int,
      ~live: option(live)=?,
      p: press,
    )
    : outcome =>
  switch (lines[at]) {
  | Of_new(_) => live_key(~lines, ~at, ~t, ~live, ~page, p)
  | Of_row(r) => decide_row(~lines, ~at, ~r, ~t, ~caret, ~blank, ~page, p)
  };
