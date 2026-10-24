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

/* the whole program's statics, monolithically: kept for the views that
   read the node map */
let mk_statics_with =
    (~root=Sort.Exp, settings: CoreSettings.t, z: Zipper.t): StaticsBase.Map.t => {
  let term =
    if (root == Sort.Mod) {
      let (seg, masks) =
        MakeTerm.semantic_segment(~root, MakeTerm.semantic_source(z));
      MakeTerm.Incr.term_of_root(~masks, ~root, seg);
    } else {
      MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    };
  fst(
    Statics.mk(
      settings,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    ),
  );
};
let mk_statics = (~root=Sort.Exp, z: Zipper.t): StaticsBase.Map.t =>
  mk_statics_with(~root, CoreSettings.on, z);

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

/* why code didn't read: the batch parser's note, a reserved binder */
let unreadable = (code: string): string =>
  "The code does not parse."
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

/* `test code end`, from the test's expression; code that already opens
   with `test` or `hint` is taken as the whole test */
let test_text = (code: string): string => {
  let code = String.trim(code);
  let code =
    String.ends_with(~suffix=";", code)
      ? String.trim(String.sub(code, 0, String.length(code) - 1)) : code;
  let opens = kw =>
    String.starts_with(~prefix=kw, code)
    && (
      String.length(code) == String.length(kw)
      || String.contains(" \n\t\"", code.[String.length(kw)])
    );
  opens("test") || opens("hint") ? code : "test " ++ code ++ " end";
};

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
  /* the entries after the target and its block's result: `update_body` */
  | Rest
  /* each name: old, new, its uses */
  | Renamed(list((string, string, int)))
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
};

let plan = (~target=?, what, ops) => {
  ops,
  target,
  what,
  removes: [],
  renames: [],
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

let resolve = (p: program, path: string): result(P.found, string) =>
  switch (P.resolve_string(~root=p.root, p.seg, path)) {
  | Ok({part: Some(part), _}) =>
    Error(
      "\""
      ++ path
      ++ "\": the tool picks the part itself; name the entry, without :"
      ++ P.part_word(part),
    )
  | Ok(f) => Ok(f)
  | Error(Unknown(_, el, _) as e) =>
    Error(P.message(e) ++ elsewhere(p, el))
  | Error(e) => Error(P.message(e))
  };

let level_of = (f: P.found): (S.level, int) => ListUtil.last(f.steps);

/* a level of module members: a module body's, or a Mod root's top */
let members_level = (p: program, lv: S.level): bool =>
  switch (lv.owner) {
  | Members(_) => true
  | Top => p.root == Sort.Mod
  | Chain(_)
  | Block(_) => false
  };

let kind_name = (k: P.kind): string =>
  switch (k) {
  | Tail => "result"
  | Stmt => "statement"
  | k => P.kind_word(k)
  };

let kernel_part = (part: P.part): K.part =>
  switch (part) {
  | Pattern => Pattern
  | Definition => Definition
  | Body => Body
  | Head => Head
  };

let part = (p: program, path: string, f: P.found, part: P.part) =>
  switch (f.kind) {
  | Def
  | Type
  | Module =>
    switch (K.part(~root=p.root, f.id, kernel_part(part), p.seg)) {
    | Some(id) => Ok(id)
    | None =>
      Error("\"" ++ path ++ "\" has no " ++ P.part_word(part) ++ " to edit")
    }
  | kind =>
    Error(
      "\""
      ++ path
      ++ "\" is a "
      ++ kind_name(kind)
      ++ ", which has no "
      ++ P.part_word(part)
      ++ ": replace it whole with update_binding_clause",
    )
  };

/* the block an entry holds: a module's members, a value's body */
let block_of = (path: string, f: P.found): result(K.block, string) =>
  switch (f.kind) {
  | Module => Ok(Members(f.id))
  | Def => Ok(Body(f.id))
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

let uses = (p: program, binder: Id.t): list(Id.t) =>
  Rename.references(~info_map=Lazy.force(p.info_map), binder)
  |> List.filter(id => K.is_token(~root=p.root, id, p.seg));

/* a pattern's tokens in order, a name's with its id */
let rec tokens = (seg: Segment.t): list((string, option(Id.t))) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile({form: Tok(s), id, _}) => [(s, Some(id))]
      | Tile(t) =>
        Aba.mk(t.shards, t.children)
        |> Aba.join(i => [(List.nth(Tile.label(t), i), None)], tokens)
        |> List.concat
      | Grout(_) => [("?", None)]
      | Projector(pr) => tokens([pr.syntax])
      | Secondary(_) => []
      },
    seg,
  );

/* [code], a pattern for entry [f], as its old pattern with only names
   changed: each changed name's token and its new name */
let renamed_names =
    (p: program, f: P.found, code: string)
    : option(list((Id.t, string, string))) => {
  let sort: Sort.t =
    switch (f.kind) {
    | Type => TPat
    | Module => MPat
    | _ => Pat
    };
  switch (
    Nav.tile_ctx(~root=p.root, f.id, p.seg),
    K.parse_at(sort, Unicode.nfc_outside_strings(code)),
  ) {
  | (Some((_, _, t)), Some(pat)) =>
    let (was, now) = (tokens(Nav.pat_of(t)), tokens(pat));
    List.length(was) == List.length(now)
      ? List.fold_left2(
          (acc, (a, id), (b, _)) =>
            switch (acc, id) {
            | (None, _) => None
            | (Some(_), _) when a == b => acc
            | (Some(l), Some(id))
                when is_name(f.kind, a) && is_name(f.kind, b) =>
              Some([(id, a, b), ...l])
            | _ => None
            },
          Some([]),
          was,
          now,
        )
        |> Option.map(List.rev)
      : None;
  | _ => None
  };
};

/* entry [k] of [lv] and the entries after it, past the tests directly
   following it: where a test of it goes */
let rec past_tests = (lv: S.level, k: int): int =>
  k + 1 < Array.length(lv.spans) && P.entry(lv, k + 1).kind == Test
    ? past_tests(lv, k + 1) : k;

/* the place [where] names relative to the entry at [path]; an empty path
   is the program's top level */
let place_at =
    (p: program, where: Action.Structural.insert_target, path: string)
    : result(K.place, string) =>
  switch (where, path) {
  | (Before | FirstIn, "") => Ok(FirstIn(Top))
  | (After | LastIn, "") => Ok(at_end(p))
  | _ =>
    let* f = resolve(p, path);
    switch (where) {
    | Before => Ok(K.Before(f.id))
    | After => Ok(K.After(f.id))
    | FirstIn => Result.map(~f=b => K.FirstIn(b), block_of(path, f))
    | LastIn => Result.map(~f=b => K.LastIn(b), block_of(path, f))
    };
  };

let rec plan_call = (p: program, call: call): result(plan, string) => {
  let text = code => K.Text(Unicode.nfc_outside_strings(code));
  switch (call) {
  | Update(Definition, path, code) =>
    let* f = resolve(p, path);
    let* def = part(p, path, f, Definition);
    Ok(
      plan(
        ~target=f.id,
        Replaced(Some(Definition)),
        [Replace(def, text(code))],
      ),
    );
  /* the old meaning, kept: what follows the entry after its `in`, the
     entries after it in its block and the block's result. A member has
     no `in`: its body is its definition past its `fun` heads */
  | Update(Body, path, code) =>
    let* f = resolve(p, path);
    let (lv, k) = level_of(f);
    if (members_level(p, lv)) {
      let* body = part(p, path, f, Body);
      Ok(
        plan(
          ~target=f.id,
          Replaced(Some(Body)),
          [Replace(body, text(code))],
        ),
      );
    } else {
      switch (f.kind, tail_index(lv)) {
      | (Tail, _) => Error("\"" ++ path ++ "\" is a result: it has no body")
      | (_, Some(t)) =>
        let after =
          List.init(t - k - 1, j => K.Delete(lv.spans[k + 1 + j].anchor));
        Ok(
          plan(
            ~target=f.id,
            Rest,
            after @ [Replace(lv.spans[t].anchor, text(code))],
          ),
        );
      | (_, None) =>
        Error("\"" ++ path ++ "\" is last in its block: it has no body")
      };
    };
  | Update(Pattern, path, code) =>
    let* f = resolve(p, path);
    let (lv, k) = level_of(f);
    let binders = P.entry(lv, k).binders;
    let named =
      switch (P.parse(path)) {
      | Ok({elems, _}) =>
        switch (ListUtil.last_opt(elems)) {
        | Some(Name(x, _)) => List.find_opt(((y, _)) => x == y, binders)
        | _ => None
        }
      | Error(_) => None
      };
    let binder =
      switch (named, binders) {
      | (Some(b), _)
      | (None, [b]) => Some(b)
      | _ => None
      };
    let name = String.trim(code);
    /* a single name renames the binder the path names, or its only one;
       a pattern that differs from its old one only in names renames
       each; else the pattern is replaced, and the error check judges */
    let renames =
      switch (binder) {
      | Some((old, _)) when old == name && is_name(f.kind, name) => Some([])
      | Some((old, binder)) when is_name(f.kind, name) =>
        Some([(binder, old, name)])
      | _ => renamed_names(p, f, code)
      };
    switch (renames) {
    | Some(renames) =>
      let renames =
        List.map(
          ((binder, old, name)) => (binder, old, name, uses(p, binder)),
          renames,
        );
      Ok({
        ...
          plan(
            ~target=f.id,
            Renamed(
              List.map(
                ((_, old, name, refs)) => (old, name, List.length(refs)),
                renames,
              ),
            ),
            List.concat_map(
              ((binder, _, name, refs)) =>
                List.map(
                  id => K.Replace(id, Token(name)),
                  [binder, ...refs],
                ),
              renames,
            ),
          ),
        renames:
          List.map(
            ((binder, _, name, refs)) => (binder, name, refs),
            renames,
          ),
      });
    | None =>
      let* pat = part(p, path, f, Pattern);
      Ok(
        plan(
          ~target=f.id,
          Replaced(Some(Pattern)),
          [Replace(pat, text(code))],
        ),
      );
    };
  | Update(BindingClause, path, code) =>
    let* f = resolve(p, path);
    Ok(plan(~target=f.id, Replaced(None), [Replace(f.id, text(code))]));
  | Delete(BindingClause, path) =>
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
  | Delete(Body, path) => plan_call(p, Update(Body, path, "?"))
  | Delete(Definition | Pattern, _) =>
    Error("Deleting a definition or pattern alone is not supported.")
  | Insert(where, path, code) =>
    let* place = place_at(p, where, path);
    Ok(plan(Added, [Insert(place, text(code))]));
  | InsertTest(None, code) =>
    Ok(plan(Added, [Insert(at_end(p), text(test_text(code)))]))
  | InsertTest(Some(path), code) =>
    let* f = resolve(p, path);
    let (lv, k) = level_of(f);
    let after = lv.spans[past_tests(lv, k)].anchor;
    Ok(plan(Added, [Insert(After(after), text(test_text(code)))]));
  | Move(_, Before | After, "") =>
    Error("move needs an entry to go before or after")
  | Move(path, where, dest) =>
    let* f = resolve(p, path);
    let* place = place_at(p, where, dest);
    let whereto =
      switch (where, dest) {
      | (Before, d) => "before " ++ d
      | (After, d) => "after " ++ d
      | (FirstIn, "") => "first in the program"
      | (LastIn, "") => "last in the program"
      | (FirstIn, d) => "first in " ++ d
      | (LastIn, d) => "last in " ++ d
      };
    Ok(plan(~target=f.id, Moved(whereto), [Move(f.id, place)]));
  };
};

let code_of = (call: call): option(string) =>
  switch (call) {
  | Update(_, _, code)
  | Insert(_, _, code)
  | InsertTest(_, code) => Some(code)
  | Delete(_)
  | Move(_) => None
  };

let path_string = (~root, seg: Segment.t, id: Id.t): option(string) =>
  Option.map(P.to_string, P.describe(~root, seg, id));

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
    ++ ".";
  | Replaced(Some(part)) =>
    "Replaced " ++ was ++ ":" ++ P.part_word(part) ++ now_target() ++ "."
  | Rest => "Replaced what follows " ++ was ++ "."
  | Renamed(names) =>
    let uses = n => n == 1 ? "1 use" : string_of_int(n) ++ " uses";
    switch (names, Option.bind(m.plan.target, now)) {
    | ([(_, _, n)], Some(p)) when p != was =>
      "Renamed " ++ was ++ " to " ++ p ++ ", and its " ++ uses(n) ++ "."
    | _ =>
      "Renamed "
      ++ String.concat(
           "; ",
           List.map(
             ((old, name, n)) =>
               old ++ " to " ++ name ++ " (" ++ uses(n) ++ ")",
             names,
           ),
         )
      ++ ", in "
      ++ was
      ++ "."
    };
  | Deleted => "Deleted " ++ was ++ "."
  | Moved(whereto) => "Moved " ++ was ++ " " ++ whereto ++ now_target() ++ "."
  };
};

/* the text of what [id] names in [seg]: an entry, or a node */
let text_at = (~root, seg: Segment.t, id: Id.t): option(string) => {
  let print = ps => String.trim(Printer.of_segment(~holes="?", ps));
  switch (P.find(~root, seg, id)) {
  | Some(steps) =>
    let (lv, k) = ListUtil.last(steps);
    Some(print(Nav.core(lv, k)));
  | None =>
    switch (K.node(~root, id, seg)) {
    | n => Some(print(K.node_pieces(n)))
    | exception (K.Refused(_)) => None
    }
  };
};

/* the call's edited region before and after it, as text */
let diff = (~root, m: made): option((string, option(string))) =>
  switch (m.change.steps) {
  | [] => None
  | [first, ..._] as steps =>
    let last = ListUtil.last(steps);
    let row = ListUtil.hd_opt(last.added);
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
      ++ ErrorPrint.string_of_marks(info, Info.marks_of(info))
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
              Rename.references(~info_map=final.info_map, binder)
              |> List.filter(id => K.is_token(~root=r.root, id, seg));
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
          switch (capture(r, final)) {
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
            (m: made) =>
              (
                m.index,
                report(~root=r.root, m)
                ++ (m.index == last.index ? " " ++ errors : ""),
              ),
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

module Public = {
  let mk_statics_with = mk_statics_with;
  let mk_statics = mk_statics;

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
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    let root = root == Sort.Mod ? Sort.Mod : Sort.Exp;
    let statics_of =
      statics_of(
        ~settings,
        ~root,
        ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
      );
    let start =
      lazy(
        switch (CachedStatics.for_zipper(~settings, z, statics)) {
        | Some(st) when st.info_map != Id.Map.empty => st
        | _ => statics_of(seg)
        }
      );
    let r = Reply.start(~root, ~statics=start, ~statics_of, seg);
    switch (Reply.add(r, ~index=0, call)) {
    | Error(msg) => Error(Composition_action_failure(msg))
    | Ok(r) =>
      switch (Reply.finish(r)) {
      | Error(f) => Error(Composition_action_failure(f.message))
      | Ok(o) => Ok(o.program === seg ? z : rezip(z, o.program))
      }
    };
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
