open Util;
open Language;

/* The assistant's structural tools on the editing kernel (plans/
   editing-in-the-large.html, phase 4). Each tool is an adapter: it
   resolves its path (EntryPath) against the program as the reply's
   earlier calls left it and emits kernel ops. A reply's calls are all or
   nothing. The finished change is checked against per-item statics: no
   uses left dangling by a delete, no rename capture, no new errors. What
   changed is reported by canonical path */

module K = EntryEdit;
module L = EntryLayout;
module Nav = SpineNav;
module P = EntryPath;
module S = ItemSpans;

type call = Action.Structural.t;

let ( let* ) = (r, f) =>
  switch (r) {
  | Ok(x) => f(x)
  | Error(_) as e => e
  };

/* ---- statics ---- */

/* per-item statics (DefStatics): an edit re-analyzes the items it changed
   and those that depend on them. [shared]: the editor's parse cache, whose
   item parses stand in for parsing the same pieces again */
let statics_of =
    (
      ~settings: CoreSettings.t,
      ~root: Sort.t,
      ~probe_ids: Id.Map.t(unit),
      ~shared=?,
      seg: Segment.t,
    )
    : CachedStatics.t => {
  let term =
    PerfTimer.time("statics/parse", () =>
      MakeTerm.semantic_term(~root, ~shared?, seg)
    );
  PerfTimer.time("statics", () =>
    CachedStatics.init_compositional_term(~settings, ~probe_ids, term)
  );
};

/* [z]'s program's statics: the editor's [statics] when they describe it,
   else [statics_of] it */
let statics_for =
    (
      ~settings: CoreSettings.t,
      ~statics_of: Segment.t => CachedStatics.t,
      z: Zipper.t,
      statics: CachedStatics.t,
    )
    : CachedStatics.t =>
  switch (CachedStatics.for_zipper(~settings, z, statics)) {
  | Some(st) when st.info_map != Id.Map.empty => st
  | _ => statics_of(Zipper.unselect_and_zip(~erase_buffer=true, z))
  };

/* ---- the code a call brings ---- */

/* form delimiters that lex like names: as a binder, the code misreads */
let reserved_words: list(Token.t) = List.filter(Token.is_var, Form.delims);

let reserved_binder = (code: string): option(string) => {
  let word = c =>
    c >= 'a'
    && c <= 'z'
    || c >= 'A'
    && c <= 'Z'
    || c >= '0'
    && c <= '9'
    || c == '_'
    || c == '\'';
  let (rev, last) =
    String.fold_left(
      ((ws, w), c) =>
        word(c)
          ? (ws, w ++ String.make(1, c))
          : w == "" ? (ws, "") : ([w, ...ws], ""),
      ([], ""),
      code,
    );
  let words = List.rev(last == "" ? rev : [last, ...rev]);
  let rec scan = words =>
    switch (words) {
    | [intro, w, ..._]
        when
          List.mem(intro, ["let", "fun", "type"])
          && List.mem(w, reserved_words) =>
      Some(w)
    | [_, ...rest] => scan(rest)
    | [] => None
    };
  List.mem(String.trim(code), reserved_words)
    ? Some(String.trim(code)) : scan(words);
};

/* a backslash escape outside string literals: code written as a JSON
   string's text */
let escaped = (code: string): bool => {
  let n = String.length(code);
  let rec go = (i, in_string) =>
    i < n
    - 1
    && (
      switch (code.[i]) {
      | '"' => go(i + 1, !in_string)
      | '\\' when in_string => go(i + 2, in_string)
      | '\\' when List.mem(code.[i + 1], ['n', 't', '"']) => true
      | _ => go(i + 1, in_string)
      }
    );
  go(0, false);
};

/* code of several entries, two with no separator between: a line at the
   code's base indentation starts an entry, and the line before it ends in
   neither `in` nor `;` */
let unseparated = (code: string): bool => {
  let lines =
    String.split_on_char('\n', code)
    |> List.filter(l => String.trim(l) != "");
  let indent = l => {
    let rec go = i => i < String.length(l) && l.[i] == ' ' ? go(i + 1) : i;
    go(0);
  };
  let base = List.fold_left((m, l) => min(m, indent(l)), max_int, lines);
  let starts_entry = l =>
    indent(l) == base
    && (
      switch (String.split_on_char(' ', String.trim(l))) {
      | ["let" | "type" | "module" | "test" | "hint" | "use", ..._] => true
      | _ => false
      }
    );
  let separated = l => {
    let t = String.trim(l);
    String.ends_with(~suffix=";", t)
    || ListUtil.last_opt(String.split_on_char(' ', t)) == Some("in");
  };
  let rec go = ls =>
    switch (ls) {
    | [prev, l, ...rest] =>
      starts_entry(l) && !separated(prev) || go([l, ...rest])
    | _ => false
    };
  go(lines);
};

/* why code didn't read: escapes, entries without separators, the batch
   parser's note, a reserved binder */
let unreadable = (code: string): string =>
  escaped(code)
    ? {|`code` contains `\n` as two characters; write real line breaks (and plain quotes).|}
    : unseparated(code)
        ? "Entries in one `code` need `in` between them (`;` in a module); leave it off the last."
        : "The code does not parse."
          ++ (
            switch (FastParse.bail_note^) {
            | Some(n) => " Parser: " ++ n ++ "."
            | None => ""
            }
          )
          ++ (
            switch (reserved_binder(code)) {
            | Some(w) =>
              " Note: `"
              ++ w
              ++ "` is a reserved keyword in Hazel and cannot be used as a variable name."
            | None => ""
            }
          );

/* a comment only the view adds, `#› … #`, copied into code */
let view_comment = StringUtil.regexp(CompositionView.marker ++ "[^#\n]*#");
let copies_view_comment = (code: string): bool =>
  StringUtil.search(view_comment, code, 0) != None;

/* ---- one call ---- */

/* the program a call runs on: its statics only when a planner asks */
type program = {
  root: Sort.t,
  seg: Segment.t,
  info_map: Lazy.t(Statics.Map.t),
};

/* what a call did, for its report */
type what =
  | Added
  | Replaced(option(P.part))
  /* the name before, after, and how many uses it had */
  | Renamed(string, string, int)
  | Deleted
  /* where to, as the call put it */
  | Moved(string);

type plan = {
  ops: list(K.op),
  /* the entry the call names, by its anchor before the call */
  target: option(Id.t),
  what,
  /* the names a delete removes, each with its binder and uses then */
  removes: list((string, Id.t, list(Id.t))),
  /* a rename's binder, its new name and its uses then */
  renames: list((Id.t, string, list(Id.t))),
  /* a replace's binders then, each by name, and their uses outside it */
  rebinds: option((list((string, Id.t)), list((Id.t, Id.t)))),
  /* a whole-entry replace kept the entry's doc comment */
  kept_comment: bool,
};

let plan = (~target=?, what, ops) => {
  ops,
  target,
  what,
  removes: [],
  renames: [],
  rebinds: None,
  kept_comment: false,
};

/* entries elsewhere named [el]: what a path missing its outer names
   meant */
let elsewhere = (p: program, el: string): string =>
  switch (
    P.entries(~root=p.root, p.seg)
    |> List.filter_map(((_, path: P.t)) =>
         switch (List.rev(path.elems)) {
         | [last, _, ..._] when P.elem_to_string(last) == el =>
           Some(P.to_string(path))
         | _ => None
         }
       )
  ) {
  | [] => ""
  | paths =>
    " Did you mean " ++ String.concat(" or ", ListUtil.take(3, paths)) ++ "?"
  };

let path_string = (~root, seg: Segment.t, id: Id.t): option(string) =>
  Option.map(P.to_string, P.describe(~root, seg, id));

/* the entry [path] names; a part only where [~parts] */
let resolve =
    (~parts=false, p: program, path: string): result(P.found, string) =>
  switch (P.resolve_string(~root=p.root, p.seg, path)) {
  /* the agent's parts: a definition's whole, not past its `fun` heads */
  | Ok({part: Some(Body), _} as f) =>
    let x =
      Option.value(path_string(~root=p.root, p.seg, f.id), ~default="X");
    Error(
      "`:body` isn't a part. Use `"
      ++ x
      ++ ":definition` (for `let f(x) = e` that is `e`), or `"
      ++ x
      ++ "/=>` for the result of a block of definitions.",
    );
  | Ok({part: Some(part), _}) when !parts =>
    Error(
      "\""
      ++ path
      ++ "\": only replace takes a part; name the entry, without :"
      ++ P.part_word(part),
    )
  | Ok(f) => Ok(f)
  | Error(Unknown(_, el, _) as e) =>
    Error(P.message(e) ++ elsewhere(p, el))
  | Error(e) => Error(P.message(e))
  };

let level_of = (f: P.found): (S.level, int) => ListUtil.last(f.steps);

let kind_name = (k: P.kind): string =>
  switch (k) {
  | Tail => "result"
  | Stmt => "statement"
  | k => P.kind_word(k)
  };

let part = (p: program, path: string, f: P.found, part: P.part) =>
  switch (K.part(~root=p.root, f.id, part, p.seg)) {
  | Some(id) => Ok(id)
  | None =>
    Error("\"" ++ path ++ "\" has no " ++ P.part_word(part) ++ " to replace")
  };

/* the block an entry holds: a module's members, a value's body */
let block_of = (path: string, f: P.found): result(K.block, string) =>
  switch (f.kind) {
  | Module => Ok(Members(f.id))
  | Def =>
    let (lv, k) = level_of(f);
    Ok(S.members(lv, k) == None ? Body(f.id) : Members(f.id));
  | kind =>
    Error(
      "\""
      ++ path
      ++ "\" is a "
      ++ kind_name(kind)
      ++ ": only a module or a value holds entries",
    )
  };

let tail_index = (lv: S.level): option(int) => {
  let n = Array.length(lv.spans);
  n > 0 && P.entry(lv, n - 1).kind == Tail ? Some(n - 1) : None;
};

/* the program's end: above its result, if it has one */
let at_end = (p: program): K.place => {
  let top = S.top(~root=p.root, p.seg);
  switch (tail_index(top)) {
  | Some(t) => Before(top.spans[t].anchor)
  | None => LastIn(Top)
  };
};

/* a name the entry's kind takes: a rename's, not a pattern's */
let is_name = (kind: P.kind, s: string): bool =>
  !Token.is_keyword(s)
  && !List.mem(s, Form.delims)
  && (
    switch (kind) {
    | Def => Token.is_var(s)
    | Type => Token.is_typ_var(s)
    | Module => Token.is_ctr(s)
    | _ => false
    }
  );

/* [binder]'s uses in [seg] by [info_map]: tokens of [seg] */
let uses_in = (~root, ~info_map, seg: Segment.t, binder: Id.t): list(Id.t) =>
  Rename.references(~info_map, binder)
  |> List.filter(id => K.is_token(~root, id, seg));
let uses = (p: program, binder: Id.t): list(Id.t) =>
  uses_in(~root=p.root, ~info_map=Lazy.force(p.info_map), p.seg, binder);

/* what [id] names in [seg]: an entry's pieces, or a node's */
let pieces_at = (~root, seg: Segment.t, id: Id.t): Segment.t =>
  switch (P.find(~root, seg, id)) {
  | Some(steps) =>
    let (lv, k) = ListUtil.last(steps);
    S.core(lv, k);
  | None =>
    switch (K.node(~root, id, seg)) {
    | n => K.node_pieces(n)
    | exception (K.Refused(_)) => []
    }
  };

/* the tokens of [ps] that bind, each by its name */
let binders_in =
    (~info_map: Statics.Map.t, ps: Segment.t): list((string, Id.t)) => {
  let rec go = (acc, ps: Segment.t) =>
    List.fold_left(
      (acc, p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          let acc = List.fold_left(go, acc, t.children);
          switch (
            Tile.single_token(t),
            Option.bind(Id.Map.find_opt(t.id, info_map), Info.binder_name),
          ) {
          | (Some(x), Some(_)) => [(x, t.id), ...acc]
          | _ => acc
          };
        | _ => acc
        },
      acc,
      ps,
    );
  List.rev(go([], ps));
};

/* the uses in [seg] of [binders], each with its binder: one pass over
   [info_map] (Rename.references' rule, for several binders) */
let uses_of =
    (~root, ~info_map, seg: Segment.t, binders: list((string, Id.t)))
    : list((Id.t, Id.t)) => {
  let names =
    List.fold_left(
      (m, (x, b)) => Id.Map.add(b, x, m),
      Id.Map.empty,
      binders,
    );
  Id.Map.fold(
    (id, info, acc) =>
      switch (Info.binding_site(~info_map, info)) {
      | Some(b) when b != id && Id.Map.mem(b, names) =>
        let named =
          switch (Info.label_name(info)) {
          | Some(l) => Id.Map.find_opt(b, names) == Some(l)
          | None => true
          };
        named && K.is_token(~root, id, seg) ? [(id, b), ...acc] : acc;
      | _ => acc
      },
    info_map,
    [],
  );
};

/* "/" (or "") names the top level */
let is_top = (path: string): bool => {
  let s = String.trim(path);
  s == "" || s == "/";
};

/* the place a call's place names */
let place_at =
    (p: program, place: Action.Structural.place): result(K.place, string) =>
  switch (place) {
  | Before(s)
  | FirstIn(s) when is_top(s) => Ok(FirstIn(Top))
  | After(s)
  | LastIn(s) when is_top(s) => Ok(at_end(p))
  | Before(path) => Result.map(~f=f => K.Before(f.P.id), resolve(p, path))
  | After(path) => Result.map(~f=f => K.After(f.P.id), resolve(p, path))
  | FirstIn(path) =>
    let* f = resolve(p, path);
    Result.map(~f=b => K.FirstIn(b), block_of(path, f));
  | LastIn(path) =>
    let* f = resolve(p, path);
    Result.map(~f=b => K.LastIn(b), block_of(path, f));
  };

let place_words = (place: Action.Structural.place): string =>
  switch (place) {
  | Before(d) => "before " ++ d
  | After(d) => "after " ++ d
  | FirstIn(d) when is_top(d) => "first in the program"
  | LastIn(d) when is_top(d) => "last in the program"
  | FirstIn(d) => "first in " ++ d
  | LastIn(d) => "last in " ++ d
  };

/* the binder a rename's path names: its last name, else the entry's only
   one */
let binder_of =
    (path: string, f: P.found, binders: list((string, Id.t)))
    : result((string, Id.t), string) => {
  let named =
    switch (P.parse(path)) {
    | Ok({elems, _}) =>
      switch (ListUtil.last_opt(elems)) {
      | Some(Name(x, _)) => List.find_opt(((y, _)) => x == y, binders)
      | _ => None
      }
    | Error(_) => None
    };
  switch (f.kind, named, binders) {
  | (Def | Type | Module, Some(b), _)
  | (Def | Type | Module, None, [b]) => Ok(b)
  | (Def | Type | Module, None, []) =>
    Error("\"" ++ path ++ "\" binds no name; replace its :pattern")
  | (Def | Type | Module, None, _) =>
    Error(
      "\""
      ++ path
      ++ "\" binds "
      ++ String.concat(", ", List.map(fst, binders))
      ++ ": name the one to rename",
    )
  | (kind, _, _) =>
    Error(
      "\""
      ++ path
      ++ "\" is a "
      ++ kind_name(kind)
      ++ ": only a definition, type or module has a name to rename",
    )
  };
};

/* the comment lines an entry's pieces start with, one per line */
let leading_comment = (ps: Segment.t): option(string) => {
  let rec lead = (ps: Segment.t) =>
    switch (ps) {
    | [p, ...rest] when Piece.is_secondary(p) => [p, ...lead(rest)]
    | _ => []
    };
  let lead = lead(ps);
  List.exists(S.is_comment, lead)
    ? Some(
        Printer.of_segment(~holes="?", lead)
        |> String.split_on_char('\n')
        |> List.map(String.trim)
        |> List.filter(l => l != "")
        |> String.concat("\n"),
      )
    : None;
};

/* the variable [x] bound in the pattern of the entry [path] names, a
   function definition's parameters included (`f(x)`, `fun x ->`) */
let bound_in =
    (p: program, path: string, f: P.found, x: string)
    : result((string, Id.t), string) => {
  let (lv, k) = level_of(f);
  let pieces =
    switch (f.kind, S.head(lv, k)) {
    | (Def | Type | Module, Some((i, t))) =>
      let rec heads = (ps: Segment.t) =>
        switch (ps) {
        | [p, ...rest] when Piece.is_secondary(p) => heads(rest)
        | [p, ...rest] when S.is_fun_head(p) => [p, ...heads(rest)]
        | _ => []
        };
      Some(
        Nav.pat_of(t)
        @ heads(Option.value(Nav.def_at((lv.seg, i, t)), ~default=[])),
      );
    | _ => None
    };
  switch (pieces) {
  | None =>
    Error(
      "\""
      ++ path
      ++ "\" is a "
      ++ kind_name(f.kind)
      ++ ": only a definition, type or module has a pattern to rename in",
    )
  | Some(ps) =>
    let binders = binders_in(~info_map=Lazy.force(p.info_map), ps);
    switch (List.filter(((y, _)) => x == y, binders)) {
    | [b] => Ok(b)
    | [_, ..._] =>
      Error("\"" ++ path ++ "\" binds " ++ x ++ " more than once")
    | [] =>
      Error(
        "\""
        ++ path
        ++ "\" binds no "
        ++ x
        ++ (
          switch (binders) {
          | [] => ""
          | bs =>
            "; its pattern binds " ++ String.concat(", ", List.map(fst, bs))
          }
        ),
      )
    };
  };
};

let plan_call = (p: program, call: call): result(plan, string) => {
  let text = code => K.Text(Unicode.nfc_outside_strings(code));
  switch (call) {
  | Insert(_, code)
  | Replace(_, code) when copies_view_comment(code) =>
    Error({js|`#› … #` comments are added by Hazel; leave them out|js})
  | Insert(place, code) =>
    let* place = place_at(p, place);
    Ok(plan(Added, [Insert(place, text(code))]));
  | Replace(path, code) =>
    let* f = resolve(~parts=true, p, path);
    let* id =
      switch (f.part) {
      | None => Ok(f.id)
      | Some(pt) => part(p, path, f, pt)
      };
    /* new code without a comment keeps the entry's */
    let comment =
      f.part == None && !String.starts_with(~prefix="#", String.trim(code))
        ? leading_comment(pieces_at(~root=p.root, p.seg, f.id)) : None;
    let code =
      switch (comment) {
      | Some(c) => c ++ "\n" ++ code
      | None => code
      };
    let pl = {
      ...plan(~target=f.id, Replaced(f.part), [Replace(id, text(code))]),
      kept_comment: comment != None,
    };
    let info_map = Lazy.force(p.info_map);
    let binders = binders_in(~info_map, pieces_at(~root=p.root, p.seg, id));
    Ok({
      ...pl,
      rebinds:
        binders == []
          ? None
          : Some((binders, uses_of(~root=p.root, ~info_map, p.seg, binders))),
    });
  | Delete(path) =>
    let* f = resolve(p, path);
    let (lv, k) = level_of(f);
    let removes =
      switch (f.kind) {
      | Def
      | Type
      | Module =>
        List.map(
          ((x, binder)) => (x, binder, uses(p, binder)),
          P.entry(lv, k).binders,
        )
      | _ => []
      };
    Ok({
      ...plan(~target=f.id, Deleted, [Delete(f.id)]),
      removes,
    });
  | Move(_, Before(s) | After(s)) when is_top(s) =>
    Error("move needs an entry to go before or after")
  | Move(path, place) =>
    let* f = resolve(p, path);
    let* dest = place_at(p, place);
    Ok(
      plan(~target=f.id, Moved(place_words(place)), [Move(f.id, dest)]),
    );
  | Rename(path, bound, name) =>
    let* f = resolve(p, path);
    let (lv, k) = level_of(f);
    let* (old, binder) =
      switch (bound) {
      | Some(x) => bound_in(p, path, f, String.trim(x))
      | None => binder_of(path, f, P.entry(lv, k).binders)
      };
    let name = String.trim(name);
    if (!is_name(f.kind, name)) {
      Error(
        "\""
        ++ name
        ++ "\" is not a "
        ++ (
          switch (f.kind) {
          | Def => "value's name: lowercase, not a keyword"
          | _ => "type's or module's name: capitalized"
          }
        ),
      );
    } else if (name == old) {
      Ok(plan(~target=f.id, Renamed(old, name, 0), []));
    } else {
      let refs = uses(p, binder);
      Ok({
        ...
          plan(
            ~target=f.id,
            Renamed(old, name, List.length(refs)),
            List.map(id => K.Replace(id, Token(name)), [binder, ...refs]),
          ),
        renames: [(binder, name, refs)],
      });
    };
  };
};

let code_of = (call: call): option(string) =>
  switch (call) {
  | Insert(_, code)
  | Replace(_, code) => Some(code)
  | Delete(_)
  | Move(_)
  | Rename(_) => None
  };

/* the entry holding piece [id], by path */
let where_is = (~root, seg: Segment.t, id: Id.t): option(string) =>
  switch (L.locate(~mod_root=root == Sort.Mod, id, seg)) {
  | Some((b, j, _)) => path_string(~root, seg, b.lv.spans[j].anchor)
  | None => None
  };

let refusal_message =
    (p: program, call: call, r: K.refusal, ops: list(K.op)): string => {
  let about =
    switch (Option.bind(r.id, id => where_is(~root=p.root, p.seg, id))) {
    | Some(path) when List.length(ops) > 1 => " (at " ++ path ++ ")"
    | _ => ""
    };
  switch (r.reason, code_of(call)) {
  | (Unreadable, Some(code)) => unreadable(code)
  | (reason, _) => "Refused: " ++ K.describe(reason) ++ about ++ "."
  };
};

/* a call run on [p]: its plan, and the kernel's change */
let run_call = (p: program, call: call): result((plan, K.change), string) => {
  let* pl = plan_call(p, call);
  switch (
    PerfTimer.time("kernel", () => K.transaction(~root=p.root, p.seg, pl.ops))
  ) {
  | Ok(change) => Ok((pl, change))
  | Error(r) => Error(refusal_message(p, call, r, pl.ops))
  };
};

/* ---- what a call did ---- */

/* the entries the change made that [before] didn't have, by path: those
   new in the level of each row it added */
let added_paths = (~root, before: Segment.t, after: Segment.t, rows) =>
  List.concat_map(
    row =>
      switch (P.find(~root, after, row)) {
      | Some(steps) =>
        let (lv, _) = ListUtil.last(steps);
        Array.to_list(lv.spans)
        |> List.filter((sp: S.span) =>
             P.find(~root, before, sp.anchor) == None
           )
        |> List.filter_map((sp: S.span) =>
             path_string(~root, after, sp.anchor)
           );
      | None => []
      },
    rows,
  )
  |> List.sort_uniq(compare);

type made = {
  call,
  /* its index among the reply's tool calls */
  index: int,
  before: Segment.t,
  plan,
  change: K.change,
};

/* the text of what [id] names in [seg]: an entry, or a node */
let text_at = (~root, seg: Segment.t, id: Id.t): option(string) =>
  switch (pieces_at(~root, seg, id)) {
  | [] => None
  | ps => Some(String.trim(Printer.of_segment(~holes="?", ps)))
  };

/* the call's edited region before and after it, as text */
let diff = (~root, m: made): option((string, option(string))) =>
  switch (m.change.steps) {
  | [] => None
  | [first, ..._] =>
    let row = ListUtil.hd_opt(first.added);
    switch (m.plan.what, m.plan.target) {
    | (Added, _) =>
      Some((
        "",
        Option.bind(
          ListUtil.hd_opt(first.added),
          text_at(~root, m.change.program),
        ),
      ))
    | (Deleted, Some(t)) =>
      Option.map(old => (old, None), text_at(~root, m.before, t))
    | (_, Some(t)) =>
      let after = Option.value(row, ~default=t);
      Option.map(
        old => (old, text_at(~root, m.change.program, after)),
        text_at(~root, m.before, t),
      );
    | (_, None) => None
    };
  };

/* the lines of [old] that [now] doesn't keep, in order, indentation
   aside: those off a longest common subsequence; None past a size where
   that costs too much to find */
let removed_lines = (old: string, now: string): option(list(string)) => {
  let lines = s =>
    String.split_on_char('\n', s)
    |> List.map(String.trim)
    |> List.filter(l => l != "")
    |> Array.of_list;
  let (a, b) = (lines(old), lines(now));
  let (n, m) = (Array.length(a), Array.length(b));
  if (n * m > 250000) {
    None;
  } else {
    /* lcs[i][j]: of a's lines from i and b's from j */
    let lcs = Array.make_matrix(n + 1, m + 1, 0);
    for (i in n - 1 downto 0) {
      for (j in m - 1 downto 0) {
        lcs[i][j] =
          a[i] == b[j]
            ? lcs[i + 1][j + 1] + 1 : max(lcs[i + 1][j], lcs[i][j + 1]);
      };
    };
    let rec go = (i, j) =>
      if (i == n) {
        [];
      } else if (j < m && a[i] == b[j]) {
        go(i + 1, j + 1);
      } else if (j < m && lcs[i][j + 1] >= lcs[i + 1][j]) {
        go(i, j + 1);
      } else {
        [a[i], ...go(i + 1, j)];
      };
    Some(go(0, 0));
  };
};

/* a replace's removed lines, for its result: a few, else how many */
let removal = (~root, m: made): string =>
  switch (diff(~root, m)) {
  | Some((old, Some(now))) =>
    switch (removed_lines(old, now)) {
    | Some([]) => ""
    | Some(ls) when List.length(ls) <= 10 =>
      String.concat("", List.map(l => "\n- " ++ l, ls))
    | Some(ls) => " " ++ string_of_int(List.length(ls)) ++ " lines replaced."
    | None =>
      " "
      ++ string_of_int(List.length(String.split_on_char('\n', old)))
      ++ " lines replaced."
    }
  | _ => ""
  };

let report = (~root, m: made): string => {
  let after = m.change.program;
  let was =
    Option.value(
      Option.bind(m.plan.target, path_string(~root, m.before)),
      ~default="the program",
    );
  let now = id => path_string(~root, after, id);
  let now_target = () =>
    switch (Option.bind(m.plan.target, now)) {
    | Some(p) when p != was => " (now " ++ p ++ ")"
    | _ => ""
    };
  switch (m.plan.what) {
  | _ when m.change.steps == [] => "No change: it was so already."
  | Added =>
    let rows = List.concat_map((s: K.step) => s.added, m.change.steps);
    switch (added_paths(~root, m.before, after, rows)) {
    | [] => "Added code."
    | paths => "Added " ++ String.concat(", ", paths) ++ "."
    };
  | Replaced(None) =>
    let made =
      List.concat_map((s: K.step) => s.added, m.change.steps)
      |> List.filter_map(now);
    "Replaced "
    ++ was
    ++ (
      switch (made) {
      | [p] when p != was => " (now " ++ p ++ ")"
      | _ => ""
      }
    )
    ++ (m.plan.kept_comment ? ", kept the comment above it" : "")
    ++ "."
    ++ removal(~root, m);
  | Replaced(Some(part)) =>
    "Replaced "
    ++ was
    ++ ":"
    ++ P.part_word(part)
    ++ now_target()
    ++ "."
    ++ removal(~root, m)
  | Renamed(old, name, n) =>
    let uses =
      switch (n) {
      | 0 => ""
      | 1 => ", and its 1 use"
      | n => ", and its " ++ string_of_int(n) ++ " uses"
      };
    switch (Option.bind(m.plan.target, now)) {
    | Some(p) when p != was => "Renamed " ++ was ++ " to " ++ p ++ uses ++ "."
    | _ => "Renamed " ++ old ++ " to " ++ name ++ " in " ++ was ++ uses ++ "."
    };
  | Deleted => "Deleted " ++ was ++ "."
  | Moved(whereto) => "Moved " ++ was ++ " " ++ whereto ++ now_target() ++ "."
  };
};

/* ---- a reply ---- */

module Reply = {
  /* the checks a reply's finished change gets; rename capture always */
  type checks = {
    no_new_errors: bool,
    dangling_uses: bool,
  };
  let all_checks = {
    no_new_errors: true,
    dangling_uses: true,
  };

  type t = {
    checks,
    root: Sort.t,
    start: Segment.t,
    start_statics: Lazy.t(CachedStatics.t),
    /* latest first */
    made: list(made),
    /* the latest program's statics */
    statics: Lazy.t(CachedStatics.t),
    statics_of: Segment.t => CachedStatics.t,
  };

  let start =
      (
        ~checks=all_checks,
        ~root: Sort.t,
        ~statics: Lazy.t(CachedStatics.t),
        ~statics_of: Segment.t => CachedStatics.t,
        seg: Segment.t,
      )
      : t => {
    checks,
    root: root == Sort.Mod ? Sort.Mod : Sort.Exp,
    start: seg,
    start_statics: statics,
    made: [],
    statics,
    statics_of,
  };

  /* a reply on [z]'s program, the editor's [statics] standing in for its
     own when they describe it */
  let of_zipper =
      (
        ~checks=?,
        ~settings: CoreSettings.t,
        ~root: Sort.t,
        ~shared=?,
        z: Zipper.t,
        statics: CachedStatics.t,
      )
      : t => {
    let statics_of =
      statics_of(
        ~settings,
        ~root,
        ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
        ~shared?,
      );
    start(
      ~checks?,
      ~root,
      ~statics=lazy(statics_for(~settings, ~statics_of, z, statics)),
      ~statics_of,
      Zipper.unselect_and_zip(~erase_buffer=true, z),
    );
  };

  let program = (r: t): Segment.t =>
    switch (r.made) {
    | [m, ..._] => m.change.program
    | [] => r.start
    };

  let made = (r: t): list(made) => List.rev(r.made);

  /* [call], the reply's [index]th tool call, on the program as the calls
     before it left it, or as [seg] when the reply's other tools changed
     it since */
  let add =
      (r: t, ~index: int, ~seg=program(r), call: call): result(t, string) => {
    let p = {
      root: r.root,
      seg,
      info_map: lazy(Lazy.force(r.statics).info_map),
    };
    let* (plan, change) = run_call(p, call);
    Ok({
      ...r,
      made: [
        {
          call,
          index,
          before: seg,
          plan,
          change,
        },
        ...r.made,
      ],
      statics:
        change.steps == [] ? r.statics : (lazy(r.statics_of(change.program))),
    });
  };

  type failure = {
    /* the tool call it concerns */
    index: int,
    message: string,
  };

  let error_line = (~root, seg, info_map, id: Id.t): string => {
    let at =
      switch (where_is(~root, seg, id)) {
      | Some(p) => p ++ ": "
      | None => ""
      };
    switch (Id.Map.find_opt(id, info_map)) {
    | Some(info) =>
      at
      ++ ErrorPrint.string_of_marks(~info_map, info, Info.marks_of(info))
      ++ " (in `"
      ++ ErrorPrint.term_string_of(info)
      ++ "`)"
    | None => at ++ "a static error"
    };
  };

  let lines = (ls: list(string)): string =>
    String.concat("", List.map(l => "\n- " ++ l, ls));

  /* the call that wrote piece [id]: the first it appears after; else the
     last, whose change the error is in */
  let author = (r: t, id: Id.t): made => {
    let has = (seg: Segment.t) =>
      Nav.find_path(~root=r.root, id, seg) != None;
    switch (
      List.find_opt(
        (m: made) => !has(m.before) && has(m.change.program),
        made(r),
      )
    ) {
    | Some(m) => m
    | None => List.hd(r.made)
    };
  };

  /* the entries holding pieces [ids], by path */
  let where_all = (r: t, ids: list(Id.t)): string =>
    List.filter_map(where_is(~root=r.root, program(r)), ids)
    |> List.sort_uniq(compare)
    |> String.concat(", ");

  /* a delete's uses still in the program, now unbound */
  let dangling = (r: t, final: CachedStatics.t): option(failure) => {
    let seg = program(r);
    let unbound = id =>
      K.is_token(~root=r.root, id, seg)
      && (
        switch (Id.Map.find_opt(id, final.info_map)) {
        | Some(info) =>
          Info.binding_site(~info_map=final.info_map, info) == None
        | None => false
        }
      );
    List.find_map(
      (m: made) =>
        switch (
          List.filter_map(
            ((x, _, refs)) =>
              switch (List.filter(unbound, refs)) {
              | [] => None
              | left => Some((x, left))
              },
            m.plan.removes,
          )
        ) {
        | [] => None
        | found =>
          Some({
            index: m.index,
            message:
              "Not deleted: it would leave uses dangling. Delete or change them first, in the same reply:"
              ++ lines(
                   List.map(
                     ((x, left)) => x ++ ", used at " ++ where_all(r, left),
                     found,
                   ),
                 ),
          })
        },
      made(r),
    );
  };

  /* a rename's uses that now read another binder, and names that now
     read it and didn't */
  let capture = (r: t, final: CachedStatics.t): option(failure) => {
    let seg = program(r);
    let start_map = Lazy.force(r.start_statics).info_map;
    List.find_map(
      (m: made) =>
        List.find_map(
          ((binder, name, refs)) => {
            let now =
              uses_in(~root=r.root, ~info_map=final.info_map, seg, binder);
            let lost =
              List.filter(
                id =>
                  K.is_token(~root=r.root, id, seg) && !List.mem(id, now),
                refs,
              );
            let taken =
              List.filter(
                id =>
                  id != binder
                  && !List.mem(id, refs)
                  && Id.Map.mem(id, start_map),
                now,
              );
            switch (lost @ taken) {
            | [] => None
            | ids =>
              Some({
                index: m.index,
                message:
                  "Not renamed to "
                  ++ name
                  ++ ": names would change what they refer to, at "
                  ++ where_all(r, ids)
                  ++ ". Choose a name not in use there.",
              })
            };
          },
          m.plan.renames,
        ),
      made(r),
    );
  };

  /* a replace's uses outside its code that now read another binder. A use
     of a binder it replaced may read the new code's binder of that name,
     or none (the error check's); any other use, none of the new code's */
  let rebinding = (r: t, final: CachedStatics.t): option(failure) => {
    let seg = program(r);
    let start_map = Lazy.force(r.start_statics).info_map;
    let binder = (info_map, id) =>
      Option.bind(
        Id.Map.find_opt(id, info_map),
        Info.binding_site(~info_map),
      );
    List.find_map(
      (m: made) =>
        switch (m.plan.rebinds) {
        | None => None
        | Some((old, refs)) =>
          /* the new code's binders, named as it wrote them */
          let made =
            List.concat_map(
              (s: K.step) => List.map(snd, s.replaced),
              m.change.steps,
            )
            |> List.concat_map(id =>
                 binders_in(
                   ~info_map=final.info_map,
                   pieces_at(~root=r.root, m.change.program, id),
                 )
               );
          let name_of = (bs, b) =>
            Option.map(fst, List.find_opt(((_, b')) => b' == b, bs));
          let moved = ((ref, was)) =>
            switch (binder(final.info_map, ref)) {
            | Some(now)
                when
                  now != was
                  && K.is_token(~root=r.root, ref, seg)
                  && (
                    switch (name_of(made, now)) {
                    | Some(x) => Some(x) != name_of(old, was)
                    | None => true
                    }
                  ) =>
              Some((ref, was, now))
            | _ => None
            };
          let lost = List.filter_map(moved, refs);
          let taken =
            made == []
              ? []
              : uses_of(~root=r.root, ~info_map=final.info_map, seg, made)
                |> List.filter_map(((ref, now)) =>
                     switch (binder(start_map, ref)) {
                     | Some(was) when !List.mem_assoc(ref, refs) =>
                       Some((ref, was, now))
                     | _ => None
                     }
                   );
          let at = (seg', id) =>
            Option.value(
              where_is(~root=r.root, seg', id),
              ~default="the top level",
            );
          let before = id =>
            switch (where_is(~root=r.root, m.before, id)) {
            | Some(p) => p
            | None => at(r.start, id)
            };
          switch (lost @ taken) {
          | [] => None
          | found =>
            Some({
              index: m.index,
              message:
                "Not replaced: uses outside it would change what they refer to. To rename a binder with its uses, use rename (with name for a parameter):"
                ++ lines(
                     List.map(
                       ((ref, was, now)) =>
                         Option.value(
                           text_at(~root=r.root, seg, ref),
                           ~default="a use",
                         )
                         ++ " at "
                         ++ at(seg, ref)
                         ++ ": bound at "
                         ++ before(was)
                         ++ " before, at "
                         ++ at(seg, now)
                         ++ " after",
                       found,
                     ),
                   ),
            })
          };
        },
      made(r),
    );
  };

  /* an error's kind: its marks', by rank */
  let error_kind = (info_map, id: Id.t): string =>
    switch (Id.Map.find_opt(id, info_map)) {
    | Some(Info.InfoDrv(_)) => "drv"
    | Some(info) =>
      Info.marks_of(info)
      |> List.map(m => string_of_int(Mark.Variants.to_rank(m)))
      |> List.sort_uniq(compare)
      |> String.concat(",")
    | None => "?"
    };

  /* the errors of [final] that [before] didn't have: by the erring term's
     id and kind, or, for a term the reply replaced (its id gone), by its
     entry's path and kind, one before for each after */
  let new_errors =
      (r: t, ~before: CachedStatics.t, ~final: CachedStatics.t): list(Id.t) => {
    let kind_before = error_kind(before.info_map);
    let kind_after = error_kind(final.info_map);
    let had =
      List.map(id => (id, kind_before(id)), before.error_ids)
      |> List.to_seq
      |> Hashtbl.of_seq;
    let by_path = Hashtbl.create(16);
    List.iter(
      id =>
        Hashtbl.add(
          by_path,
          (where_is(~root=r.root, r.start, id), kind_before(id)),
          (),
        ),
      before.error_ids,
    );
    let seg = program(r);
    List.filter(
      id => {
        let kind = kind_after(id);
        switch (Hashtbl.find_opt(had, id)) {
        | Some(k) when k == kind => false
        | _ when Id.Map.mem(id, before.info_map) => true
        | _ =>
          let key = (where_is(~root=r.root, seg, id), kind);
          Hashtbl.mem(by_path, key)
            ? {
              Hashtbl.remove(by_path, key);
              false;
            }
            : true;
        };
      },
      final.error_ids,
    );
  };

  type outcome = {
    program: Segment.t,
    statics: CachedStatics.t,
    /* per call index: what it did, then the reply's errors */
    reports: list((int, string)),
  };

  /* the reply's checks over its finished change, and its reports */
  let finish = (r: t): result(outcome, failure) =>
    switch (r.made) {
    | [] =>
      Ok({
        program: r.start,
        statics: Lazy.force(r.start_statics),
        reports: [],
      })
    | [last, ..._] =>
      let seg = program(r);
      let final = Lazy.force(r.statics);
      let before = Lazy.force(r.start_statics);
      let fresh =
        PerfTimer.time("new errors", () => new_errors(r, ~before, ~final));
      let (n0, n1) = (
        List.length(before.error_ids),
        List.length(final.error_ids),
      );
      let line = id => error_line(~root=r.root, seg, final.info_map, id);
      let checked =
        switch (r.checks.dangling_uses ? dangling(r, final) : None) {
        | Some(f) => Error(f)
        | None =>
          switch (
            List.find_map(check => check(r, final), [capture, rebinding])
          ) {
          | Some(f) => Error(f)
          | None when r.checks.no_new_errors && fresh != [] =>
            Error({
              index: author(r, List.hd(fresh)).index,
              message:
                "Not applied: it would add static errors ("
                ++ string_of_int(n0)
                ++ " before, "
                ++ string_of_int(n1)
                ++ " after):"
                ++ lines(List.map(line, fresh)),
            })
          | None => Ok()
          }
        };
      let* () = checked;
      let errors =
        switch (n1, fresh) {
        | (0, _) => "Static errors: none."
        | (_, []) =>
          "Static errors: "
          ++ string_of_int(n1)
          ++ " ("
          ++ string_of_int(n0)
          ++ " before), none new."
        | (_, fresh) =>
          "Static errors: "
          ++ string_of_int(n1)
          ++ " ("
          ++ string_of_int(n0)
          ++ " before). New:"
          ++ lines(List.map(line, fresh))
        };
      Ok({
        program: seg,
        statics: final,
        reports:
          List.map(
            (m: made) => {
              let said = report(~root=r.root, m);
              let sep = String.contains(said, '\n') ? "\n" : " ";
              (m.index, said ++ (m.index == last.index ? sep ++ errors : ""));
            },
            made(r),
          ),
      });
    };
};

/* ---- the editor's single action ---- */

/* [seg] as the program of [z]: its probes and its caret's neighbour */
let rezip = (z: Zipper.t, seg: Segment.t): Zipper.t => {
  let z' = {
    ...Zipper.unzip(~direction=Left, seg),
    refractors: z.refractors,
  };
  let anchor =
    switch (Siblings.neighbors(z.relatives.siblings)) {
    | (_, Some(p)) => Some((Direction.Left, Piece.id(p)))
    | (Some(p), None) => Some((Direction.Right, Piece.id(p)))
    | (None, None) => None
    };
  switch (anchor) {
  | Some((side, id)) =>
    Option.value(Move.jump_to_side_of_id(side, z', id), ~default=z')
  | None => z'
  };
};

/* one structural call as a reply of its own (Perform's Structural):
   [statics] stands in for the program's when they describe it */
let go =
    (
      ~settings: CoreSettings.t,
      ~root: Sort.t,
      ~statics: CachedStatics.t,
      z: Zipper.t,
      call: call,
    )
    : result(Zipper.t, Action.Failure.t) => {
  let root = root == Sort.Mod ? Sort.Mod : Sort.Exp;
  let r = Reply.of_zipper(~settings, ~root, z, statics);
  switch (Reply.add(r, ~index=0, call)) {
  | Error(msg) => Error(Composition_action_failure(msg))
  | Ok(r) =>
    switch (Reply.finish(r)) {
    | Error(f) => Error(Composition_action_failure(f.message))
    | Ok(o) => Ok(o.program === r.start ? z : rezip(z, o.program))
    }
  };
};

/* a call's kernel steps, for a replay (AgentToolResult.step) */
let step_records = (~root, m: made): list(AgentToolResult.step) => {
  let path = (seg, id) => path_string(~root, seg, id);
  let record = (before, s: K.step): AgentToolResult.step => {
    let made = ListUtil.hd_opt(s.added);
    let (op, from, into) =
      switch (s.op) {
      | Insert(_) => ("insert", None, Option.bind(made, path(s.program)))
      | Replace(x, _) => (
          "replace",
          path(before, x),
          path(s.program, Option.value(made, ~default=x)),
        )
      | Delete(x) => ("delete", path(before, x), None)
      | Move(x, _) => ("move", path(before, x), path(s.program, x))
      };
    {
      op,
      from,
      into,
      added: s.added,
      removed: s.removed,
      moved: List.map(((x, _, _)) => x, s.moved),
      replaced: s.replaced,
    };
  };
  let rec go = (before, steps: list(K.step)) =>
    switch (steps) {
    | [] => []
    | [s, ...rest] => [record(before, s), ...go(s.program, rest)]
    };
  go(m.before, m.change.steps);
};
