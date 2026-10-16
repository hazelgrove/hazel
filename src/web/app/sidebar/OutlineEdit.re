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
  | Delete;

/* a new definition's kind from its leading keyword */
let new_kind = (text: string): (OutlineTree.kind, string) =>
  String.starts_with(~prefix="type ", text)
    ? (KType, "type ")
    : String.starts_with(~prefix="module ", text)
        ? (KModule, "module ") : (KConst, "");

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

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  line,
  /* in the line's field, which leaves out a new definition's keyword */
  col: int,
  /* the column ↑↓ aim for: set by the first vertical move, kept by the
     next ones, cleared by any other key */
  col_target: option(int),
  /* typed on the line, not yet saved */
  pending: option(string),
  /* why the line's last save was refused */
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

/* a new definition's name as the program has it, and the caret's place
   in it, as last drawn */
type live = {
  l_name: string,
  l_at: int,
};

/* a key on a new definition's line, for the editor to apply to its name */
[@deriving (show({with_path: false}), sexp, yojson)]
type edit =
  | Insert(string)
  /* the character toward a side */
  | Erase(Direction.t)
  | Step(Direction.t)
  /* to the name's start or end */
  | Edge(Direction.t);

/* a name to save, from the line it was typed on; the caret's column comes
   back with the line if the save is refused */
[@deriving (show({with_path: false}), sexp, yojson)]
type save = {
  sv_line: line,
  sv_text: string,
  sv_col: int,
};

/* what follows a save: the caret where the keys put it, a new definition
   below what was saved, or row mode */
[@deriving (show({with_path: false}), sexp, yojson)]
type after =
  | Stay
  | Next
  | Leave;

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
   inside it, or past the rows under it (below it, or last inside it) */
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
      let k = place == First ? i + 1 : past(i + 1);
      Array.concat([
        Array.sub(rows, 0, k),
        [|Of_new(a, place)|],
        Array.sub(rows, k, n - k),
      ]);
    }
  | _ => rows
  };
};

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
  /* row mode, what's typed dropped */
  | Abandon
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
  /* a new definition remade as a keyword's kind (none: a value) */
  | Rekind(string)
  /* a new definition's line gone, its definition with it: the caret to a
     neighbour line */
  | Drop(Id.t, option((line, int)))
  /* the page's undo, which takes a new definition away: the caret to
     [line] first */
  | Undo(line, int);

/* the line where Enter puts a new definition, for a nameable row: first
   inside an expanded module, else below */
let new_place = (r: visible_row): place =>
  r.r_expanded && r.r_node.o_kind == KModule ? First : Below;

let len_of_line = (e: entry): int => units(name_of_line(e));

let ghost_line = (e: entry): bool =>
  switch (e) {
  | Of_row(r) => !nameable(r.r_node)
  | Of_new(_) => false
  };

/* what key [p] does on new definition line [at]: its name is typed into
   the program by the editor, [live] as last drawn (none yet: blank) */
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
  let (name, col) =
    switch (live) {
    | Some(l) => (l.l_name, l.l_at)
    | None => ("", 0)
    };
  let len = units(name);
  let to_end = j =>
    Go(
      line_of(lines[j]),
      ghost_line(lines[j]) ? 0 : len_of_line(lines[j]),
      None,
    );
  let to_start = j => Go(line_of(lines[j]), 0, None);
  let vertical = (~up, j) =>
    if (j == at) {
      Type(Edge(up ? Left : Right));
    } else {
      let target = Option.value(t.col_target, ~default=col);
      Go(
        line_of(lines[j]),
        ghost_line(lines[j]) ? 0 : min(target, len_of_line(lines[j])),
        Some(target),
      );
    };
  /* the line above, where the caret goes when this one does */
  let above = () => at > 0 ? to_end(at - 1) : Save(Leave);
  let blank = String.trim(name) == "";
  let kw = Option.fold(~none="", ~some=f => f.f_kw, t.fresh);
  let alone = !p.alt && !p.meta && !p.shift;
  switch (p.key) {
  | _ when p.composing => Swallow
  | "Shift"
  | "Alt"
  | "Meta"
  | "Control"
  | "CapsLock" => Page
  | "Escape" => t.error != None ? Abandon : Save(Leave)
  | "Enter" when p.meta || p.shift => Save(Leave)
  | "Enter" when p.alt || p.ctrl => Swallow
  | "Enter" => blank ? Save(Leave) : Save(Next)
  | "Tab"
  | "F2" => Swallow
  /* not an item to move yet */
  | "ArrowUp"
  | "ArrowDown" when p.alt && !p.meta && !p.shift => Flash
  | "ArrowUp" when p.meta && !p.alt && !p.shift =>
    at == 0 ? Type(Edge(Left)) : to_start(0)
  | "ArrowDown" when p.meta && !p.alt && !p.shift =>
    at == n - 1 ? Type(Edge(Right)) : to_end(n - 1)
  | "ArrowUp" when alone => vertical(~up=true, max(0, at - 1))
  | "ArrowDown" when alone => vertical(~up=false, min(n - 1, at + 1))
  | "PageUp" when alone => vertical(~up=true, max(0, at - page()))
  | "PageDown" when alone => vertical(~up=false, min(n - 1, at + page()))
  | "ArrowLeft" when alone =>
    col > 0 ? Type(Step(Left)) : at > 0 ? to_end(at - 1) : Swallow
  | "ArrowRight" when alone =>
    col < len ? Type(Step(Right)) : at < n - 1 ? to_start(at + 1) : Swallow
  | "Home" when alone => Type(Edge(Left))
  | "End" when alone => Type(Edge(Right))
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
  | "Backspace" when p.alt || p.ctrl => Swallow
  | "Backspace" when col > 0 => Type(Erase(Left))
  /* at the start: a type or module's keyword goes first */
  | "Backspace" when blank && kw != "" && t.fresh != None => Rekind("")
  /* a blank name: the line goes, and its definition with it */
  | "Backspace" => blank ? above() : Flash
  | "Delete" when alone && col < len => Type(Erase(Right))
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
  | _ when p.meta => Page
  | _ when p.ctrl => Swallow
  /* a value's name begun `type ` or `module `: that kind instead */
  | " "
      when
        kw == ""
        && t.fresh != None
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
  let value = here_ghost ? "" : Option.value(t.pending, ~default=r.r_name);
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
  let typed = (s: string, a: int, b: int) =>
    Edit(slice(value, 0, a) ++ s ++ slice(value, b, len), a + units(s));
  let alone = !p.alt && !p.meta && !p.shift;
  switch (p.key) {
  | _ when p.composing => Field
  | "Shift"
  | "Alt"
  | "Meta"
  | "Control"
  | "CapsLock" => Page
  | "Escape" => t.error != None ? Abandon : Save(Leave)
  | "Enter" when p.meta || p.shift => Save(Leave)
  | "Enter" when p.alt || p.ctrl => Swallow
  | "Enter" when !here_ghost =>
    switch (item) {
    | Some(id) => Go(New(id, new_place(r)), 0, None)
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
