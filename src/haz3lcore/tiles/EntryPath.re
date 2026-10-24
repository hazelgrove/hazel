open Util;

/* Paths to a program's entries (plans/editing-in-the-large.html,
   "Addressing"): the edges' names for what the kernel addresses by id.
   A path picks one entry per block, outermost first, down ItemSpans'
   levels: module members, a member's let chain, a definition's body.

     M.parse ≡ M/parse   a def, type or module by any name it binds
     M/"parses ints"     a hinted test
     M/test#2, f/let#1   the k-th of a kind in its block (1-based): let
                         (a let or theorem), type, module, test, use,
                         stmt
     f/=>  (or ⇒)        an expression block's final value; a Mod root's
                         final `;`-less expression
     parse#2             the 2nd of the block's entries named parse
     f:body              a part, after the last element: pattern,
                         definition, body, head

   A name wins over `stmt#k` (it names a `let stmt` when the block has
   one); the other kind words are keywords. A shared name short of the
   last element needs no `#k` when only one of its entries has the rest.
   Canonical paths are the shortest that resolve. Pure: pieces only, no
   parse, no statics. */

module S = ItemSpans;
module SM = Maps.StringMap;

type kind =
  /* let, theorem */
  | Def
  | Type
  | Module
  /* `test … end`, hinted or not */
  | Test
  /* any other `…;` */
  | Stmt
  /* `use M in`, a filter */
  | Use
  | Tail;

/* a part of a def, type or module entry; the resolver checks the entry
   has it, and the kernel finds it */
type part =
  | Pattern
  /* the whole definition, `fun` heads included */
  | Definition
  /* a let's definition past its `fun` heads: ItemSpans' block */
  | Body
  | Head;

type elem =
  /* a name, and which of the block's entries binding it */
  | Name(string, option(int))
  | Hint(string, option(int))
  /* the k-th entry of a kind; never Tail */
  | Nth(kind, int)
  | Tail;

type t = {
  elems: list(elem),
  part: option(part),
};

let kind_words = [
  ("let", Def),
  ("type", Type),
  ("module", Module),
  ("test", Test),
  ("stmt", Stmt),
  ("use", Use),
];
let kind_word = (k: kind): string =>
  switch (List.find_opt(((_, k')) => k' == k, kind_words)) {
  | Some((w, _)) => w
  | None => "=>"
  };

let part_words = [
  ("pattern", Pattern),
  ("definition", Definition),
  ("body", Body),
  ("head", Head),
];
let part_word = (p: part): string =>
  fst(List.find(((_, p')) => p' == p, part_words));

/* ---- text ---- */

let occ = (base: string, k: option(int)): string =>
  switch (k) {
  | Some(k) => base ++ "#" ++ string_of_int(k)
  | None => base
  };

let elem_to_string = (e: elem): string =>
  switch (e) {
  | Name(x, k) => occ(x, k)
  | Hint(h, k) => occ("\"" ++ h ++ "\"", k)
  | Nth(kind, k) => occ(kind_word(kind), Some(k))
  | Tail => "=>"
  };

let to_string = (p: t): string =>
  String.concat("/", List.map(elem_to_string, p.elems))
  ++ (
    switch (p.part) {
    | Some(part) => ":" ++ part_word(part)
    | None => ""
    }
  );

let parse = (s: string): result(t, string) => {
  let n = String.length(s);
  let fail = (i, what) =>
    Error(Printf.sprintf("path \"%s\", at %d: %s", s, i, what));
  let rec spaces = i => i < n && s.[i] == ' ' ? spaces(i + 1) : i;
  let starts = (i, lit) =>
    i
    + String.length(lit) <= n
    && String.sub(s, i, String.length(lit)) == lit;
  let rec word_end = i =>
    i < n && !List.mem(s.[i], ['.', '/', '#', ':', ' ', '"'])
      ? word_end(i + 1) : i;
  let rec digits_end = i =>
    i < n && s.[i] >= '0' && s.[i] <= '9' ? digits_end(i + 1) : i;
  /* an optional `#k` at [i] */
  let hash_k = i =>
    if (i < n && s.[i] == '#') {
      let j = digits_end(i + 1);
      switch (int_of_string_opt(String.sub(s, i + 1, j - i - 1))) {
      | Some(k) when k >= 1 => Ok((Some(k), j))
      | _ => fail(i, "`#` takes a number from 1")
      };
    } else {
      Ok((None, i));
    };
  /* an element at [i], and where it ends */
  let elem = i =>
    if (i < n && s.[i] == '"') {
      switch (String.index_from_opt(s, i + 1, '"')) {
      | None => fail(i, "an unclosed hint string")
      | Some(j) =>
        let h = String.sub(s, i + 1, j - i - 1);
        Stdlib.Result.map(((k, e)) => (Hint(h, k), e), hash_k(j + 1));
      };
    } else if (starts(i, "=>")) {
      Ok((Tail, i + 2));
    } else if (starts(i, "⇒")) {
      Ok((Tail, i + String.length("⇒")));
    } else {
      let j = word_end(i);
      if (j == i) {
        fail(i, "a name, a hint string, kind#k or =>");
      } else {
        let w = String.sub(s, i, j - i);
        Stdlib.Result.map(
          ((k, e)) =>
            switch (List.assoc_opt(w, kind_words), k) {
            | (Some(kind), Some(k)) => (Nth(kind, k), e)
            | _ => (Name(w, k), e)
            },
          hash_k(j),
        );
      };
    };
  let rec elems = (i, acc) =>
    switch (elem(spaces(i))) {
    | Error(_) as e => e
    | Ok((e, j)) =>
      let j = spaces(j);
      if (j >= n) {
        Ok({
          elems: List.rev([e, ...acc]),
          part: None,
        });
      } else if (s.[j] == '.' || s.[j] == '/') {
        elems(j + 1, [e, ...acc]);
      } else if (s.[j] == ':') {
        let w = String.trim(String.sub(s, j + 1, n - j - 1));
        switch (List.assoc_opt(w, part_words)) {
        | Some(part) =>
          Ok({
            elems: List.rev([e, ...acc]),
            part: Some(part),
          })
        | None => fail(j + 1, "a part: pattern, definition, body or head")
        };
      } else {
        fail(j, "`.`, `/` or `:` between elements");
      };
    };
  elems(0, []);
};

/* ---- entries ---- */

/* what a level's span is as an entry */
type entry = {
  kind,
  /* the names it binds, each with its token, in order */
  binders: list((string, Id.t)),
  hint: option(string),
};

let content = (seg: Segment.t): list(Piece.t) =>
  List.filter(p => !Piece.is_secondary(p), seg);

/* the names a pattern binds: its vars, but not a function definition's
   parameters (`f(x)`), a label (`x = p`) or past an ascription's `:` */
let rec pat_binders = (seg: Segment.t): list((string, Id.t)) => {
  let rec go = (ps: list(Piece.t), ~asc, acc) =>
    switch (ps) {
    | [] => acc
    | [Tile(t), ...rest] =>
      switch (Tile.single_token(t)) {
      | _ when Tile.is_comma(t) => go(rest, ~asc=false, acc)
      | _ when asc => go(rest, ~asc, acc)
      | _ when t.form == Form.Compound(TypeAsc) => go(rest, ~asc=true, acc)
      | Some(x) when Token.is_var(x) =>
        switch (rest) {
        | [Tile(lab), ...rest] when Tile.is_tuple_label_eq(lab) =>
          go(rest, ~asc, acc)
        | [Tile(ap), ...rest] when ap.form == Form.Compound(Ap) =>
          go(rest, ~asc, [(x, t.id), ...acc])
        | _ => go(rest, ~asc, [(x, t.id), ...acc])
        }
      | Some(_) => go(rest, ~asc, acc)
      | None =>
        go(
          rest,
          ~asc,
          List.rev_append(List.concat_map(pat_binders, t.children), acc),
        )
      }
    | [_, ...rest] => go(rest, ~asc, acc)
    };
  List.rev(go(content(seg), ~asc=false, []));
};

/* a type's or module's name: its first name token, before any `:` */
let head_name = (~ok: Token.t => bool, seg: Segment.t): list((string, Id.t)) =>
  switch (content(seg)) {
  | [Tile(t), ..._] =>
    switch (Tile.single_token(t)) {
    | Some(x) when ok(x) => [(x, t.id)]
    | _ => []
    }
  | _ => []
  };

/* a statement that is only a test, and its hint if a string */
let test_of = (lv: S.level, c0: int, c1: int): option(option(string)) =>
  switch (Option.map(i => lv.pieces[i], S.only_content(lv.pieces, c0, c1))) {
  | Some(Tile(t)) when t.form == Form.Compound(Test) => Some(None)
  | Some(Tile({children: [hint, ..._], _} as t))
      when t.form == Form.Compound(HintedTest) =>
    switch (content(hint)) {
    | [Tile(s)] =>
      switch (Tile.single_token(s)) {
      | Some(h) when Token.is_string(h) =>
        Some(Some(String.sub(h, 1, String.length(h) - 2)))
      | _ => Some(None)
      }
    | _ => Some(None)
    }
  | _ => None
  };

let entry_of = (lv: S.level, k: int): entry => {
  let sp = lv.spans[k];
  let (c0, c1) = sp.core;
  let mk = (~binders=[], ~hint=None, kind) => {
    kind,
    binders,
    hint,
  };
  switch (sp.kind) {
  | Def =>
    switch (S.head(lv, k)) {
    | Some((_, t)) =>
      let pat = Option.value(List.nth_opt(t.children, 0), ~default=[]);
      switch (Tile.token(t, 0)) {
      | "type" => mk(~binders=head_name(~ok=Token.is_typ_var, pat), Type)
      | "module" =>
        mk(
          ~binders=
            head_name(~ok=x => Token.is_var(x) || Token.is_ctr(x), pat),
          Module,
        )
      | _ => mk(~binders=pat_binders(pat), Def)
      };
    | None => mk(Def)
    }
  | Use => mk(Use)
  /* a Mod root's final `;`-less expression is its result */
  | Stmt
      when
        lv.owner == Top
        && lv.sort == Sort.Mod
        && sp.sep == None
        && k == Array.length(lv.spans)
        - 1 =>
    mk(Tail)
  | Stmt =>
    switch (test_of(lv, c0, c1)) {
    | Some(hint) => mk(~hint, Test)
    | None => mk(Stmt)
    }
  | Tail => mk(Tail)
  };
};

/* a level's entries, and their names, hints and kinds to span indices in
   program order */
type index = {
  entries: array(entry),
  names: SM.t(list(int)),
  hints: SM.t(list(int)),
  kinds: list((kind, array(int))),
  /* the anchors and first tiles of the levels below to the span holding
     them, made on first use by describing */
  mutable below: option(Id.Map.t(int)),
};

let index_of = (lv: S.level): index => {
  let entries = Array.init(Array.length(lv.spans), entry_of(lv));
  let add = (key, k, m) =>
    SM.update(key, l => Some([k, ...Option.value(l, ~default=[])]), m);
  let names = ref(SM.empty);
  let hints = ref(SM.empty);
  Array.iteri(
    (k, e) => {
      /* an entry binding a name twice counts once */
      List.iter(
        x => names := add(x, k, names^),
        List.sort_uniq(compare, List.map(fst, e.binders)),
      );
      Option.iter(h => hints := add(h, k, hints^), e.hint);
    },
    entries,
  );
  let kinds =
    List.map(
      kind => {
        let ks = ref([]);
        Array.iteri(
          (k, e) => e.kind == kind ? ks := [k, ...ks^] : (),
          entries,
        );
        (kind, Array.of_list(List.rev(ks^)));
      },
      [Def, Type, Module, Test, Stmt, Use, Tail],
    );
  {
    entries,
    names: SM.map(List.rev, names^),
    hints: SM.map(List.rev, hints^),
    kinds,
    below: None,
  };
};

/* indexes by level, valid while the level is the same: ItemSpans keeps a
   level while its pieces are */
type key = (S.ends, Sort.t, S.owner, Id.t);
let cache: Swept.t(key, (S.level, index)) = Swept.mk();
/* indexes built, cumulative: observability for tests */
let built = ref(0);

let key_of = (lv: S.level): key => (
  lv.ends,
  lv.sort,
  lv.owner,
  Array.length(lv.pieces) > 0 ? Piece.id(lv.pieces[0]) : Id.invalid,
);

let index = (lv: S.level): index => {
  let key = key_of(lv);
  switch (Swept.find_opt(cache, key)) {
  | Some((lv', ix)) when lv' === lv => ix
  | _ =>
    incr(built);
    let ix = index_of(lv);
    Swept.replace(cache, key, (lv, ix));
    ix;
  };
};

let entry = (lv: S.level, k: int): entry => index(lv).entries[k];

/* the block under span [k]: a module literal's members, a member's let
   chain, a definition's body */
let sub = (lv: S.level, k: int): option(S.level) =>
  switch (S.children(lv, k)) {
  | Some(_) as c => c
  | None => S.block(lv, k)
  };

let kinds_of = (ix: index, kind: kind): array(int) =>
  List.assoc(kind, ix.kinds);

let of_list = (l: option(list(int))): list(int) =>
  Option.value(l, ~default=[]);

/* [k]'s place in [l], 1-based */
let place = (k: int, l: list(int)): int => {
  let rec go = (i, l) =>
    switch (l) {
    | [] => 1
    | [k', ...l] => k' == k ? i : go(i + 1, l)
    };
  go(1, l);
};
/* [k]'s `#k` among the entries [l] sharing its name or hint */
let occurrence = (k: int, l: list(int)): option(int) =>
  switch (l) {
  | [_] => None
  | l => Some(place(k, l))
  };

/* the element naming span [k] of a level: its first name, its hint, else
   its place among its kind; with `#k` when it shares its name or hint */
let explicit = (ix: index, k: int): elem => {
  let e = ix.entries[k];
  switch (e.kind, e.hint, e.binders) {
  | (Tail, _, _) => Tail
  | (Test, Some(h), _) =>
    Hint(h, occurrence(k, of_list(SM.find_opt(h, ix.hints))))
  | (_, _, [(x, _), ..._]) =>
    Name(x, occurrence(k, of_list(SM.find_opt(x, ix.names))))
  | (kind, _, _) => Nth(kind, place(k, Array.to_list(kinds_of(ix, kind))))
  };
};

/* span [k] is the only entry of its level its element picks: no `#k` */
let unshared = (lv: S.level, k: int): bool =>
  switch (explicit(index(lv), k)) {
  | Name(_, Some(_))
  | Hint(_, Some(_)) => false
  | Name(_, None)
  | Hint(_, None)
  | Nth(_)
  | Tail => true
  };

/* every element that picks an entry of the level, in program order: an
   entry's explicit element, then its other names */
let candidates = (ix: index): list(string) =>
  Array.to_list(ix.entries)
  |> List.mapi((k, e) => {
       let own = elem_to_string(explicit(ix, k));
       let names =
         List.map(
           ((x, _)) =>
             elem_to_string(
               Name(x, occurrence(k, of_list(SM.find_opt(x, ix.names)))),
             ),
           e.binders,
         );
       [own, ...List.filter(s => s != own, names)];
     })
  |> List.concat;

/* ---- resolving ---- */

type error =
  | Syntax(string)
  /* the block's path, the element, the block's candidates */
  | Unknown(string, string, list(string))
  /* the block's path, the element, its `#k` options */
  | Ambiguous(string, string, list(string))
  /* the block's path, the element, how many it could be */
  | Out_of_range(string, string, int)
  /* the entry's path: it has no block below */
  | No_block(string)
  /* the entry's path, the part, the entry's kind */
  | No_part(string, part, kind);

let where = (block: string): string =>
  block == "" ? "the program's top level" : "\"" ++ block ++ "\"";

let message = (e: error): string =>
  switch (e) {
  | Syntax(s) => s
  | Unknown(block, el, []) =>
    Printf.sprintf("No entry \"%s\" in %s: it has none", el, where(block))
  | Unknown(block, el, cs) =>
    Printf.sprintf(
      "No entry \"%s\" in %s. Its entries: %s",
      el,
      where(block),
      String.concat(", ", cs),
    )
  | Ambiguous(block, el, opts) =>
    Printf.sprintf(
      "\"%s\" is ambiguous in %s: %d entries. Pick one: %s (in program order)",
      el,
      where(block),
      List.length(opts),
      String.concat(", ", opts),
    )
  | Out_of_range(block, el, n) =>
    Printf.sprintf(
      "No entry \"%s\" in %s: there %s only %d",
      el,
      where(block),
      n == 1 ? "is" : "are",
      n,
    )
  | No_block(entry) =>
    Printf.sprintf("\"%s\" has no entries of its own to descend into", entry)
  | No_part(entry, part, kind) =>
    Printf.sprintf(
      "\"%s\" is a %s, which has no %s",
      entry,
      kind == Tail ? "result" : kind_word(kind),
      part_word(part),
    )
  };

type found = {
  /* the entry's anchor: ItemSpans' join key */
  id: Id.t,
  kind,
  part: option(part),
  /* the levels down to it, outermost first, as ItemSpans.find's */
  steps: list((S.level, int)),
};

let has_part = (kind: kind, part: part): bool =>
  switch (kind, part) {
  | (Def | Type | Module, Pattern | Definition | Head) => true
  | (Def, Body) => true
  | _ => false
  };

/* the entries [el] picks among [ix]'s, each as the element with its `#k`:
   one, or all of a shared name or hint when [el] has no `#k` */
let pick =
    (~block: string, ix: index, el: elem): result(list((int, elem)), error) => {
  let shown = elem_to_string(el);
  let among = (mk: option(int) => elem, l: list(int), k: option(int)) =>
    switch (l, k) {
    | ([], _) => Error(Unknown(block, shown, candidates(ix)))
    | ([k], None) => Ok([(k, el)])
    | (l, None) => Ok(List.mapi((i, k) => (k, mk(Some(i + 1))), l))
    | (l, Some(i)) =>
      switch (List.nth_opt(l, i - 1)) {
      | Some(k) => Ok([(k, el)])
      | None => Error(Out_of_range(block, shown, List.length(l)))
      }
    };
  let by_name = (x, k) =>
    among(k => Name(x, k), of_list(SM.find_opt(x, ix.names)), k);
  switch (el) {
  | Name(x, k) => by_name(x, k)
  | Hint(h, k) =>
    among(k => Hint(h, k), of_list(SM.find_opt(h, ix.hints)), k)
  /* `stmt` can be a name; a name wins */
  | Nth(Stmt, k) when SM.mem(kind_word(Stmt), ix.names) =>
    by_name(kind_word(Stmt), Some(k))
  | Nth(kind, k) =>
    let ks = kinds_of(ix, kind);
    if (Array.length(ks) == 0) {
      Error(Unknown(block, shown, candidates(ix)));
    } else if (k > Array.length(ks)) {
      Error(Out_of_range(block, shown, Array.length(ks)));
    } else {
      Ok([(ks[k - 1], el)]);
    };
  | Tail =>
    switch (kinds_of(ix, Tail)) {
    | [|k|] => Ok([(k, el)])
    | _ => Error(Unknown(block, shown, candidates(ix)))
    }
  };
};

/* A shared name or hint short of the last element is ambiguous only if
   more than one of its entries has the rest of the path (`m/x` when one
   `m` has an `x`). [done_]: the elements
   so far, innermost first */
let resolve_in = (top: S.level, p: t): result(found, error) => {
  let prefix = (done_: list(elem)) =>
    to_string({
      elems: List.rev(done_),
      part: None,
    });
  let rec go = (lv: S.level, elems, done_, steps) =>
    switch (elems) {
    | [] => Error(Syntax("an empty path"))
    | [el, ...rest] =>
      let ix = index(lv);
      let block = prefix(done_);
      let at = ((k, el')) => {
        let steps = [(lv, k), ...steps];
        let done_ = [el', ...done_];
        switch (rest) {
        | [] =>
          let kind = ix.entries[k].kind;
          switch (p.part) {
          | Some(part) when !has_part(kind, part) =>
            Error(No_part(prefix(done_), part, kind))
          | _ =>
            Ok({
              id: lv.spans[k].anchor,
              kind,
              part: p.part,
              steps: List.rev(steps),
            })
          };
        | _ =>
          switch (sub(lv, k)) {
          | Some(below) => go(below, rest, done_, steps)
          | None => Error(No_block(prefix(done_)))
          }
        };
      };
      let ambiguous = picks =>
        Error(
          Ambiguous(
            block,
            elem_to_string(el),
            List.map(((_, el')) => elem_to_string(el'), picks),
          ),
        );
      switch (pick(~block, ix, el)) {
      | Error(_) as e => e
      | Ok([one]) => at(one)
      | Ok(picks) when rest == [] => ambiguous(picks)
      | Ok(picks) =>
        let tried = List.map(pk => (pk, at(pk)), picks);
        switch (List.filter(((_, r)) => Result.is_ok(r), tried)) {
        | [(_, r)] => r
        | [] => snd(List.hd(tried))
        | ok => ambiguous(List.map(fst, ok))
        };
      };
    };
  go(top, p.elems, [], []);
};

let resolve = (~root: Sort.t, seg: Segment.t, p: t): result(found, error) => {
  Swept.next_pass(cache);
  resolve_in(S.top(~root, seg), p);
};

let resolve_string =
    (~root: Sort.t, seg: Segment.t, s: string): result(found, error) =>
  switch (parse(s)) {
  | Error(e) => Error(Syntax(e))
  | Ok(p) => resolve(~root, seg, p)
  };

/* ---- describing ---- */

/* the shortest path to the entry [steps] reach from [top]: each element
   explicit, then outermost first without its `#k` wherever the path
   still resolves to the entry */
let path_of = (top: S.level, steps: list((S.level, int))): t => {
  let id = {
    let (lv, k) = ListUtil.last(steps);
    lv.spans[k].anchor;
  };
  let resolves = elems =>
    switch (
      resolve_in(
        top,
        {
          elems,
          part: None,
        },
      )
    ) {
    | Ok(f) => f.id == id
    | Error(_) => false
    };
  let rec drop = (before, after) =>
    switch (after) {
    | [] => List.rev(before)
    | [el, ...after] =>
      let bare =
        switch (el) {
        | Name(x, Some(_)) => Some(Name(x, None))
        | Hint(h, Some(_)) => Some(Hint(h, None))
        | _ => None
        };
      switch (bare) {
      | Some(el') when resolves(List.rev_append(before, [el', ...after])) =>
        drop([el', ...before], after)
      | _ => drop([el, ...before], after)
      };
    };
  {
    elems: drop([], List.map(((lv, k)) => explicit(index(lv), k), steps)),
    part: None,
  };
};

/* [lv]'s descendants' anchors and first tiles, to the span holding them */
let rec below = (lv: S.level): Id.Map.t(int) => {
  let ix = index(lv);
  switch (ix.below) {
  | Some(m) => m
  | None =>
    let m = ref(Id.Map.empty);
    Array.iteri(
      (k, _) =>
        Option.iter(
          sub => {
            let add = (id, _, m) => Id.Map.add(id, k, m);
            m := Id.Map.fold(add, S.index(sub), m^);
            m := Id.Map.fold(add, below(sub), m^);
          },
          sub(lv, k),
        ),
      lv.spans,
    );
    ix.below = Some(m^);
    m^;
  };
};

/* the levels down to the entry anchored at [id] (or whose first tile it
   is), through every block: one probe per level. An anchor below a first
   tile is the closer match: a let chain's first item */
let rec find_in = (lv: S.level, id: Id.t): option(list((S.level, int))) => {
  let down = k =>
    Option.map(
      path => [(lv, k), ...path],
      Option.bind(sub(lv, k), below => find_in(below, id)),
    );
  switch (Id.Map.find_opt(id, S.index(lv))) {
  | Some(k) when lv.spans[k].anchor == id => Some([(lv, k)])
  | Some(k) =>
    switch (down(k)) {
    | Some(_) as path => path
    | None => Some([(lv, k)])
    }
  | None => Option.bind(Id.Map.find_opt(id, below(lv)), down)
  };
};

let find = (~root: Sort.t, seg: Segment.t, id: Id.t) => {
  Swept.next_pass(cache);
  find_in(S.top(~root, seg), id);
};

/* the canonical path of the entry anchored at [id] */
let describe = (~root: Sort.t, seg: Segment.t, id: Id.t): option(t) => {
  Swept.next_pass(cache);
  let top = S.top(~root, seg);
  Option.map(path_of(top), find_in(top, id));
};

/* every entry with its canonical path, depth first, in program order */
let entries = (~root: Sort.t, seg: Segment.t): list((Id.t, t)) => {
  Swept.next_pass(cache);
  let top = S.top(~root, seg);
  let rec go = (rev_steps, lv: S.level, acc) => {
    let acc = ref(acc);
    Array.iteri(
      (k, sp: S.span) => {
        let here = [(lv, k), ...rev_steps];
        acc := [(sp.anchor, path_of(top, List.rev(here))), ...acc^];
        switch (sub(lv, k)) {
        | Some(below) => acc := go(here, below, acc^)
        | None => ()
        };
      },
      lv.spans,
    );
    acc^;
  };
  List.rev(go([], top, []));
};
