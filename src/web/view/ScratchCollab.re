/* Collaborative editing bridge (docs/collab-modular.md).

   The shared (Automerge) document stores a program as ITEMS — one per
   top-level definition / statement / trailing expression — each holding
   plain-text LEAVES: the header (pattern + annotation, type name, module
   name) and the body (RHS, statement, trailing expression). Delimiters
   (`let … = … in`, `;`) are structure, derived from the item's kind.

   This module converts between that representation and Hazel segments:
   - [items_of_seg]: program segment -> items (initial import);
   - [seg_of_items]: items -> program segment whose item tiles carry the
     items' ids, so every peer agrees on item identity;
   - [leaf_text] / [set_leaf]: read / replace one leaf in a program;
   - [caret_offset] / [with_caret_at]: caret <-> text offset in a leaf's
     cell editor, for exchanging carets as positions in the shared text.

   Leaf text is canonical: grout (holes) prints as nothing, since the
   parser re-derives it. Offsets are in UTF-16 code units, which is what
   Automerge's JS API uses. */
open Haz3lcore;
open Util;
module Focus = ScratchFocus;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type kind =
  | Def
  | Type
  | Module
  | Stmt
  | Tail;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type leaf =
  | Lead
  | Header
  | Body;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type item = {
  id: Id.t,
  kind,
  /* comments preceding a def-like item ("" if none); statements and the
     tail keep leading comments in their body */
  lead: string,
  /* "" for Stmt and Tail */
  header: string,
  body: string,
};

let has_header = (kind: kind): bool =>
  switch (kind) {
  | Def
  | Type
  | Module => true
  | Stmt
  | Tail => false
  };

let root_of = (kind: kind, leaf: leaf): Sort.t =>
  switch (kind, leaf) {
  | (Type, Header) => TPat
  | (Type, Body) => Typ
  | (_, Header) => Pat
  | (_, Body | Lead) => Exp
  };

let kind_of_label = (label: Label.t): option(kind) =>
  switch (label) {
  | ["let", ..._] => Some(Def)
  | ["type", ..._] => Some(Type)
  | ["module", ..._] => Some(Module)
  | _ => None
  };

/* ---- text ---- */

let text_of_seg = (seg: Segment.t): string =>
  Printer.of_segment(
    ~holes="",
    ~concave_holes="",
    ~indent="",
    ~refractors=[],
    seg,
  );

let utf16_length = (s: string): int => {
  let n = ref(0);
  String.iter(
    c => {
      let b = Char.code(c);
      if (b land 0xC0 != 0x80) {
        n := n^ + (b >= 0xF0 ? 2 : 1);
      };
    },
    s,
  );
  n^;
};

let is_multiline = (s: string): bool => String.contains(s, '\n');

/* Parse one leaf's text at [root]. The fast parser is linear and keeps
   formatting but only handles complete terms; incomplete ones (common
   mid-edit) fall back to the typing parser, which grouts. Refractor
   triggers (probes) are collected and dropped: they're per-user, and
   never printed into leaf text. */
let parse = (~root: Sort.t, text: string): Segment.t => {
  let fast =
    switch (root) {
    | Exp
    | Mod =>
      FastParse.of_text(
        ~materialize=Triggers.invoked_projector,
        ~collect_refractors=true,
        ~root,
        text,
      )
    | _ => None
    };
  switch (fast) {
  | Some(seg) =>
    seg
    |> Zipper.unzip(~direction=Left)
    |> Zipper.remold_regrout(Left, ~root)
    |> Zipper.unselect_and_zip
  | None =>
    switch (MarkerParse.of_text(~root, text)) {
    | Some(z) => Zipper.unselect_and_zip(z)
    | None => []
    }
  };
};

/* ---- the program's items ----

   Items are read off modular-editors' top-level spans (`…in` tiles and
   top-level `;`s), made total so that EVERY program maps to items, since
   half-typed code is the normal state while editing:
   - only complete `let`/`type`/`module … = … in` tiles are definition
     boundaries; an incomplete one (a stray `in`, a `let` still being
     typed) is ordinary content;
   - content between items belongs to the next item: a definition's
     `lead` text, or the start of a statement's / the tail's body. */

let only_secondary = (ps: list(Piece.t)): bool =>
  List.for_all(Piece.is_secondary, ps);

type cspan = {
  c_kind: kind,
  c_id: option(Id.t), /* the def tile's or `;`'s id; None for the tail */
  c_start: int, /* where the item's region starts (previous item's end) */
  c_tile: int, /* the def tile's index (= c_start for stmt/tail) */
  c_stop: int /* exclusive */
};

let def_tile = (p: Piece.t): option((kind, Base.tile)) =>
  switch (p) {
  | Tile(t) when Focus.ends_with_in(t) && List.length(t.children) == 2 =>
    Option.map(k => (k, t), kind_of_label(t.label))
  | _ => None
  };

let cspans = (seg: Segment.t): list(cspan) => {
  let arr = Array.of_list(seg);
  let n = Array.length(arr);
  let rec go = (prev, spans: list(Focus.item_span), acc) =>
    switch (spans) {
    | [] =>
      let tail = {
        c_kind: Tail,
        c_id: None,
        c_start: prev,
        c_tile: prev,
        c_stop: n,
      };
      List.rev([tail, ...acc]);
    | [sp, ...rest] =>
      switch (sp.sp_kind) {
      | IDef =>
        switch (def_tile(arr[sp.sp_start])) {
        | Some((kind, t)) =>
          go(
            sp.sp_stop,
            rest,
            [
              {
                c_kind: kind,
                c_id: Some(t.id),
                c_start: prev,
                c_tile: sp.sp_start,
                c_stop: sp.sp_stop,
              },
              ...acc,
            ],
          )
        | None => go(prev, rest, acc) /* not a boundary: content */
        }
      | IStmt =>
        go(
          sp.sp_stop,
          rest,
          [
            {
              c_kind: Stmt,
              c_id: sp.sp_id,
              c_start: prev,
              c_tile: prev,
              c_stop: sp.sp_stop,
            },
            ...acc,
          ],
        )
      | ITail => go(prev, rest, acc) /* the final tail covers it */
      }
    };
  go(0, Focus.item_spans(seg), []);
};

/* index of a statement's `;` (its region minus trailing whitespace) */
let semi_index = (c: cspan, arr: array(Piece.t)): int => {
  let rec back = i =>
    i > c.c_start && Focus.is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
  let stop = back(c.c_stop);
  stop > c.c_start && Focus.is_semi(arr[stop - 1]) ? stop - 1 : stop;
};

/* the pieces a leaf's text covers (untrimmed), as [start, stop) indices
   into the top level; header/body are tile children instead */
let content_range = (c: cspan, arr: array(Piece.t)): (int, int) =>
  switch (c.c_kind) {
  | Def
  | Type
  | Module => (c.c_start, c.c_tile) /* the lead */
  | Stmt => (c.c_start, semi_index(c, arr))
  | Tail => (c.c_start, c.c_stop)
  };

let item_of_cspan =
    (~tail_id: Id.t, seg: Segment.t, arr: array(Piece.t), c: cspan): item => {
  let text = s => text_of_seg(Focus.core_ws(s));
  let (a, b) = content_range(c, arr);
  let region = Focus.slice(a, b, seg);
  switch (c.c_kind, arr) {
  | (Def | Type | Module, _) =>
    switch (def_tile(arr[c.c_tile])) {
    | Some((_, t)) => {
        id: t.id,
        kind: c.c_kind,
        lead: text(region),
        header: text(List.nth(t.children, 0)),
        body: text(List.nth(t.children, 1)),
      }
    | None => failwith("ScratchCollab: cspan without a def tile")
    }
  | (Stmt, _) => {
      id: Option.get(c.c_id),
      kind: Stmt,
      lead: "",
      header: "",
      body: text(region),
    }
  | (Tail, _) => {
      id: tail_id,
      kind: Tail,
      lead: "",
      header: "",
      body: text(region),
    }
  };
};

let items_of_seg = (~tail_id: Id.t, seg: Segment.t): option(list(item)) => {
  let arr = Array.of_list(seg);
  Some(List.map(item_of_cspan(~tail_id, seg, arr), cspans(seg)));
};

/* ---- items -> program ---- */

/* The skeleton gives each item a fixed, always-parseable placeholder, so
   the program's item structure is exactly the items' — whatever the leaf
   texts contain. The real leaves are then parsed on their own and
   spliced into their slots, just like modular-editors' cells. */
let skeleton_text = (it: item): string =>
  switch (it.kind) {
  | Def => "let a = 0 in"
  | Type => "type A = Int in"
  | Module => "module A = { let a = 0; } in"
  | Stmt => "0;"
  | Tail => "0"
  };

let retag = (old_id: Id.t, new_id: Id.t, seg: Segment.t): Segment.t =>
  List.map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when t.id == old_id =>
        Piece.Tile({
          ...t,
          id: new_id,
        })
      | _ => p
      },
    seg,
  );

/* Regrout the top level after splicing content into it: unlike tile
   children (header, body), top-level content sits next to its neighbours
   (e.g. a lead `0` before a `let` needs an operator hole between them).
   Identity of unchanged pieces is preserved. */
let regrout_top = (seg: Segment.t): Segment.t =>
  seg
  |> Zipper.unzip(~direction=Left)
  |> Zipper.remold_regrout_global(Left, ~root=Exp)
  |> Zipper.unselect_and_zip;

/* Replace top-level pieces [a, b) with [repl], keeping their edge
   whitespace (padding round-trips without being stored). */
let splice_range =
    (a: int, b: int, repl: Segment.t, seg: Segment.t): Segment.t => {
  let (pre, _, suf) = Focus.trim_ws(Focus.slice(a, b, seg));
  Focus.take(a, seg) @ pre @ repl @ suf @ Focus.drop(b, seg);
};

let splice_header = (id: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  Focus.splice_pat(id, Focus.rewrap_ws(Focus.find_pat, id, seg, repl), seg);

let splice_body = (id: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  Focus.splice_def(id, Focus.rewrap_ws(Focus.find_def, id, seg, repl), seg);

let find_cspan = (kind: kind, id: Id.t, seg: Segment.t): option(cspan) =>
  List.find_opt(
    c => kind == Tail ? c.c_kind == Tail : c.c_id == Some(id),
    cspans(seg),
  );

/* replace an item's lead / statement / tail content */
let splice_content =
    (kind: kind, id: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  switch (find_cspan(kind, id, seg)) {
  | Some(c) =>
    let (a, b) = content_range(c, Array.of_list(seg));
    regrout_top(splice_range(a, b, repl, seg));
  | None => seg
  };

let seg_of_items = (items: list(item)): option(Segment.t) => {
  let skel =
    parse(~root=Exp, String.concat("\n", List.map(skeleton_text, items)));
  let cs = cspans(skel);
  if (List.length(cs) != List.length(items)) {
    None;
  } else {
    let pairs = List.combine(cs, items);
    let skel =
      List.fold_left(
        (seg, (c, it: item)) =>
          switch (c.c_id) {
          | Some(old_id) when it.kind != Tail => retag(old_id, it.id, seg)
          | _ => seg
          },
        skel,
        pairs,
      );
    /* splice leaves last item first, so earlier indices stay valid when a
       splice changes the top level's length */
    Some(
      regrout_top @@
      List.fold_left(
        (seg, (_, it: item)) =>
          switch (it.kind) {
          | Def
          | Type
          | Module =>
            let seg =
              seg
              |> splice_body(
                   it.id,
                   parse(~root=root_of(it.kind, Body), it.body),
                 )
              |> splice_header(
                   it.id,
                   parse(~root=root_of(it.kind, Header), it.header),
                 );
            it.lead == ""
              ? seg
              : splice_content(
                  it.kind,
                  it.id,
                  parse(~root=Exp, it.lead),
                  seg,
                );
          | Stmt
          | Tail =>
            splice_content(it.kind, it.id, parse(~root=Exp, it.body), seg)
          },
        skel,
        List.rev(pairs),
      ),
    );
  };
};

/* ---- single leaves in a program ---- */

/* The segment a leaf's cell edits (edge whitespace trimmed). */
let leaf_seg =
    (kind: kind, id: Id.t, leaf: leaf, seg: Segment.t): option(Segment.t) =>
  switch (kind, leaf) {
  | (Def | Type | Module, Header) =>
    Option.map(Focus.core_ws, Focus.find_pat(id, seg))
  | (Def | Type | Module, Body) =>
    Option.map(Focus.core_ws, Focus.find_def(id, seg))
  | (Def | Type | Module, Lead)
  | (Stmt | Tail, Body) =>
    Option.map(
      c => {
        let (a, b) = content_range(c, Array.of_list(seg));
        Focus.core_ws(Focus.slice(a, b, seg));
      },
      find_cspan(kind, id, seg),
    )
  | _ => None
  };

let leaf_text =
    (kind: kind, id: Id.t, leaf: leaf, seg: Segment.t): option(string) =>
  Option.map(text_of_seg, leaf_seg(kind, id, leaf, seg));

/* Replace one leaf's content with [repl] (already parsed at the leaf's
   root). The item tile keeps its id, so only that item's statics rerun. */
let splice_leaf =
    (kind: kind, id: Id.t, leaf: leaf, repl: Segment.t, seg: Segment.t)
    : Segment.t =>
  switch (kind, leaf) {
  | (Def | Type | Module, Header) => splice_header(id, repl, seg)
  | (Def | Type | Module, Body) => splice_body(id, repl, seg)
  | (Def | Type | Module, Lead)
  | (Stmt | Tail, Body) => splice_content(kind, id, repl, seg)
  | _ => seg
  };

let set_leaf =
    (kind: kind, id: Id.t, leaf: leaf, text: string, seg: Segment.t)
    : Segment.t =>
  splice_leaf(kind, id, leaf, parse(~root=root_of(kind, leaf), text), seg);

/* ---- caret <-> text offset (within one leaf's editor) ---- */

/* Printed length of a generalized neighbor: a single-shard projection of
   a tile, a secondary, grout (zero: holes aren't text) or a projector
   (its trigger syntax). */
let piece_length = (p: Piece.t): int =>
  switch (p) {
  | Grout(_) => 0
  | _ => utf16_length(text_of_seg([p]))
  };

/* UTF-16 length of the first [n] graphemes of [token] */
let grapheme_prefix_length = (token: string, n: int): int => {
  let gs = Unicode.graphemes(token);
  let n = max(0, min(n, Array.length(gs)));
  let len = ref(0);
  for (i in 0 to n - 1) {
    len := len^ + utf16_length(gs[i]);
  };
  len^;
};

/* Offset of the caret in the printed leaf text: walk left to the start,
   summing the printed length of each token passed, plus the caret's
   position inside its right-neighbor token. With a selection, this is
   the focus end. */
let caret_offset = (z: Zipper.t): int => {
  let z =
    Selection.is_empty(z.selection)
      ? z : Zipper.directional_unselect(z.selection.focus, z);
  let (inner, z) =
    switch (z.caret) {
    | Outer => (0, z)
    | Inner(n) =>
      switch (Zipper.neighbor_token(Right, z)) {
      | Some(tok) => (
          grapheme_prefix_length(tok, n + 1),
          Zipper.Caret.set(Outer, z),
        )
      | None =>
        /* end of the leaf: Inner indexes the LEFT token */
        switch (Zipper.neighbor_token(Left, z)) {
        | Some(tok) => (
            grapheme_prefix_length(tok, n + 1) - utf16_length(tok),
            Zipper.Caret.set(Outer, z),
          )
        | None => (0, Zipper.Caret.set(Outer, z))
        }
      }
    };
  let rec walk = (acc, z) =>
    switch (Zipper.generalized_neighbor(Left, z)) {
    | None => acc
    | Some(p) =>
      switch (Move.local(ByToken, Left, z)) {
      | Some(z') => walk(acc + piece_length(p), z')
      | None => acc + piece_length(p)
      }
    };
  walk(inner, z);
};

/* The zipper [z] with its caret at text [offset] (clamped). Walks right
   from the start; an offset inside a token becomes an Inner caret. At an
   offset shared by several caret stops (grout is zero-width), the
   leftmost one wins. */
let with_caret_at = (offset: int, z: Zipper.t): Zipper.t => {
  let z = Move.to_start(Zipper.unselect(z));
  let rec walk = (acc, z) =>
    if (acc >= offset) {
      z;
    } else {
      switch (Zipper.generalized_neighbor(Right, z)) {
      | None => z
      | Some(p) =>
        let len = piece_length(p);
        if (acc + len > offset) {
          /* inside this token: count graphemes up to the offset */
          switch (Piece.token_of(p)) {
          | Some(tok) =>
            let gs = Unicode.graphemes(tok);
            let rec count = (i, units) =>
              i >= Array.length(gs) || units >= offset - acc
                ? i : count(i + 1, units + utf16_length(gs[i]));
            let k = count(0, 0);
            k <= 0 ? z : Zipper.Caret.set(Inner(k - 1), z);
          | None => z
          };
        } else {
          switch (Move.local(ByToken, Right, z)) {
          | Some(z') => walk(acc + len, z')
          | None => z
          };
        };
      };
    };
  walk(0, z);
};

/* ---- local edits -> text splices ---- */

type splice = {
  index: int, /* UTF-16 offset in the old text */
  delete: int, /* UTF-16 units removed */
  insert: string,
};

/* The single splice turning [old_] into [new_]: common prefix/suffix,
   measured in UTF-16 units but cut on code-point boundaries. */
let diff = (old_: string, new_: string): option(splice) =>
  if (old_ == new_) {
    None;
  } else {
    let lo = String.length(old_)
    and ln = String.length(new_);
    let rec pre = i =>
      i < lo && i < ln && old_.[i] == new_.[i] ? pre(i + 1) : i;
    let p = pre(0);
    let rec suf = j =>
      j < lo - p && j < ln - p && old_.[lo - 1 - j] == new_.[ln - 1 - j]
        ? suf(j + 1) : j;
    let s = suf(0);
    /* back off to code-point boundaries (UTF-8 continuation bytes) */
    let is_cont = (str, i) =>
      i < String.length(str) && Char.code(str.[i]) land 0xC0 == 0x80;
    let rec fix_p = p => p > 0 && is_cont(old_, p) ? fix_p(p - 1) : p;
    let p = fix_p(p);
    let rec fix_s = s => s > 0 && is_cont(old_, lo - s) ? fix_s(s - 1) : s;
    let s = fix_s(s);
    let s = min(s, min(lo - p, ln - p));
    Some({
      index: utf16_length(String.sub(old_, 0, p)),
      delete: utf16_length(String.sub(old_, p, lo - p - s)),
      insert: String.sub(new_, p, ln - p - s),
    });
  };

/* ---- incremental itemization + local diff ---- */

let slice_eq = (a: list(Piece.t), b: list(Piece.t)): bool =>
  List.length(a) == List.length(b) && List.for_all2((===), a, b);

/* [items_of_seg], reusing the previous item for any region whose pieces
   are physically unchanged — modular-editors' splices and remold preserve
   piece identity, so after an edit only the touched item is re-printed. */
let items_of_seg_cached =
    (~tail_id: Id.t, ~prev: list((item, list(Piece.t))), seg: Segment.t)
    : option(list((item, list(Piece.t)))) => {
  let arr = Array.of_list(seg);
  Some(
    List.map(
      c => {
        let pieces = Focus.slice(c.c_start, c.c_stop, seg);
        let id =
          switch (c.c_kind) {
          | Tail => tail_id
          | _ => Option.get(c.c_id)
          };
        switch (List.find_opt(((it: item, _)) => it.id == id, prev)) {
        | Some((it, ps)) when it.kind == c.c_kind && slice_eq(ps, pieces) => (
            it,
            pieces,
          )
        | _ => (item_of_cspan(~tail_id, seg, arr, c), pieces)
        };
      },
      cspans(seg),
    ),
  );
};

type op =
  | Edit(Id.t, leaf, string) /* the leaf's new full text */
  | Insert(item, option(Id.t)) /* after this sibling (None = first) */
  | Remove(Id.t)
  | Move(Id.t, option(Id.t));

let leaf_of = (it: item, leaf: leaf): string =>
  switch (leaf) {
  | Lead => it.lead
  | Header => it.header
  | Body => it.body
  };

/* Longest common subsequence of two id lists (ids unique in each). */
let lcs = (a: list(Id.t), b: list(Id.t)): list(Id.t) => {
  let a = Array.of_list(a)
  and b = Array.of_list(b);
  let n = Array.length(a)
  and m = Array.length(b);
  let dp = Array.make_matrix(n + 1, m + 1, 0);
  for (i in n - 1 downto 0) {
    for (j in m - 1 downto 0) {
      dp[i][j] =
        a[i] == b[j] ? dp[i + 1][j + 1] + 1 : max(dp[i + 1][j], dp[i][j + 1]);
    };
  };
  let rec walk = (i, j, acc) =>
    if (i >= n || j >= m) {
      List.rev(acc);
    } else if (a[i] == b[j]) {
      walk(i + 1, j + 1, [a[i], ...acc]);
    } else if (dp[i + 1][j] >= dp[i][j + 1]) {
      walk(i + 1, j, acc);
    } else {
      walk(i, j + 1, acc);
    };
  walk(0, 0, []);
};

/* The ops turning [prev] into [cur] (both in program order). The tail is
   never inserted, removed or moved. */
let diff_items = (prev: list(item), cur: list(item)): list(op) => {
  let has = (items, id) => List.exists((it: item) => it.id == id, items);
  let find = (items, id) => List.find((it: item) => it.id == id, items);
  let removes =
    prev
    |> List.filter((it: item) => it.kind != Tail && !has(cur, it.id))
    |> List.map((it: item) => Remove(it.id));
  let common =
    cur
    |> List.filter((it: item) => has(prev, it.id))
    |> List.map(it => it.id);
  let prev_common =
    prev
    |> List.filter((it: item) => has(cur, it.id))
    |> List.map((it: item) => it.id);
  let stable = lcs(prev_common, common);
  /* walk cur in order: insert new items, move displaced ones */
  let (_, placements) =
    List.fold_left(
      ((after, acc), it: item) =>
        if (it.kind == Tail) {
          (after, acc);
        } else if (!has(prev, it.id)) {
          (Some(it.id), [Insert(it, after), ...acc]);
        } else if (!List.mem(it.id, stable)) {
          (Some(it.id), [Move(it.id, after), ...acc]);
        } else {
          (Some(it.id), acc);
        },
      (None, []),
      cur,
    );
  let edits =
    cur
    |> List.filter((it: item) => has(prev, it.id))
    |> List.concat_map((it: item) => {
         let old = find(prev, it.id);
         List.filter_map(
           leaf =>
             leaf_of(old, leaf) == leaf_of(it, leaf)
               ? None : Some(Edit(it.id, leaf, leaf_of(it, leaf))),
           [Lead, Header, Body],
         );
       });
  removes @ List.rev(placements) @ edits;
};

/* ---- leaf positions in a whole program (for master-editor carets) ---- */

type range = {
  r_id: Id.t,
  r_kind: kind,
  r_leaf: leaf,
  r_start: int,
  r_stop: int,
};

/* Where each item leaf sits in the program's printed text, in one pass. */
let leaf_ranges = (~tail_id: Id.t, seg: Segment.t): list(range) => {
  let arr = Array.of_list(seg);
  let n = Array.length(arr);
  let lens = Array.map(p => utf16_length(text_of_seg([p])), arr);
  let starts = Array.make(n + 1, 0);
  for (i in 0 to n - 1) {
    starts[i + 1] = starts[i] + lens[i];
  };
  let core_range = (start: int, s: Segment.t) => {
    let (pre, core, _) = Focus.trim_ws(s);
    let a = start + utf16_length(text_of_seg(pre));
    (a, a + utf16_length(text_of_seg(core)));
  };
  List.concat_map(
    c => {
      let (a, b) = content_range(c, arr);
      let (ca, cb) = core_range(starts[a], Focus.slice(a, b, seg));
      switch (c.c_kind) {
      | Def
      | Type
      | Module =>
        switch (def_tile(arr[c.c_tile])) {
        | Some((kind, t)) =>
          let (kw, eq) =
            switch (t.label) {
            | [kw, eq, ..._] => (kw, eq)
            | _ => ("", "")
            };
          let pat = List.nth(t.children, 0)
          and def = List.nth(t.children, 1);
          let h0 = starts[c.c_tile] + utf16_length(kw);
          let (ha, hb) = core_range(h0, pat);
          let b0 = h0 + utf16_length(text_of_seg(pat)) + utf16_length(eq);
          let (ba, bb) = core_range(b0, def);
          let mk = (r_leaf, r_start, r_stop) => {
            r_id: t.id,
            r_kind: kind,
            r_leaf,
            r_start,
            r_stop,
          };
          [mk(Lead, ca, cb), mk(Header, ha, hb), mk(Body, ba, bb)];
        | None => []
        }
      | Stmt => [
          {
            r_id: Option.get(c.c_id),
            r_kind: Stmt,
            r_leaf: Body,
            r_start: ca,
            r_stop: cb,
          },
        ]
      | Tail => [
          {
            r_id: tail_id,
            r_kind: Tail,
            r_leaf: Body,
            r_start: ca,
            r_stop: cb,
          },
        ]
      };
    },
    cspans(seg),
  );
};

/* A whole-program caret, relative to a leaf so it survives edits
   elsewhere: inside the leaf, or [k] characters past its end (on the
   delimiters/whitespace after it, e.g. right after a `let`'s `in`), or [k]
   before its start (before the first leaf). */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type rel =
  | In(int)
  | After(int)
  | Before(int);

type anchor = (Id.t, leaf, rel);

let anchor_of = (~tail_id, z: Zipper.t): option(anchor) => {
  let g = caret_offset(z);
  let seg = Zipper.unselect_and_zip(z);
  let ranges = leaf_ranges(~tail_id, seg);
  switch (
    List.find_opt(r => g >= r.r_start && g <= r.r_stop, ranges),
    List.fold_left(
      (acc, r) =>
        r.r_stop <= g
          ? switch (acc) {
            | Some(a) when a.r_stop > r.r_stop => acc
            | _ => Some(r)
            }
          : acc,
      None,
      ranges,
    ),
    ranges,
  ) {
  | (Some(r), _, _) => Some((r.r_id, r.r_leaf, In(g - r.r_start)))
  | (None, Some(r), _) => Some((r.r_id, r.r_leaf, After(g - r.r_stop)))
  | (None, None, [r, ..._]) =>
    Some((r.r_id, r.r_leaf, Before(r.r_start - g)))
  | (None, None, []) => None
  };
};

/* The item delimiter the caret is on, if any: (item, shard, offset into
   the token), e.g. just after a definition's `=` is (item, 1, 1). */
let delim_of =
    (~is_item: Id.t => bool, z: Zipper.t): option((Id.t, int, int)) => {
  let z =
    Selection.is_empty(z.selection)
      ? z : Zipper.directional_unselect(z.selection.focus, z);
  let shard = (p: option(Piece.t)) =>
    switch (p) {
    | Some(Tile({id, shards: [i], label, _})) when is_item(id) =>
      Some((id, i, Option.value(List.nth_opt(label, i), ~default="")))
    | _ => None
    };
  switch (shard(Zipper.generalized_neighbor(Right, z)), z.caret) {
  | (Some((id, i, _)), Outer) => Some((id, i, 0))
  | (Some((id, i, _)), Inner(n)) => Some((id, i, n + 1))
  | (None, Outer) =>
    switch (shard(Zipper.generalized_neighbor(Left, z))) {
    | Some((id, i, tok)) => Some((id, i, Unicode.length(tok)))
    | None => None
    }
  | (None, Inner(_)) => None
  };
};

/* Where a delimiter caret shows in a cell, which holds only one leaf: the
   leaf and whether at its start or end. */
let delim_in_leaf =
    (kind: kind, shard: int, off: int)
    : (
        leaf,
        [
          | `Start
          | `End
        ],
      ) =>
  switch (kind, shard) {
  | (Def | Type | Module, 0) => (Header, `Start)
  | (Def | Type | Module, 1) => off == 0 ? (Header, `End) : (Body, `Start)
  | _ => (Body, `End) /* `in`, or a statement's `;` */
  };

/* The caret's place in its leaf, for presence: clamped into the leaf. */
let leaf_position = ((id, leaf, rel): anchor, ~len: int): (Id.t, leaf, int) =>
  switch (rel) {
  | In(off) => (id, leaf, off)
  | After(_) => (id, leaf, len)
  | Before(_) => (id, leaf, 0)
  };

/* [seg] unzipped with the caret at an anchor (start if not found).
   [remap] maps an in-leaf offset through a change to the anchor's leaf. */
let zipper_at =
    (
      ~tail_id,
      ~remap=(_: Id.t, _: leaf, off: int) => off,
      anchor: option(anchor),
      seg: Segment.t,
    )
    : Zipper.t => {
  let z = Zipper.unzip(~direction=Left, seg);
  switch (anchor) {
  | None => z
  | Some((id, leaf, rel)) =>
    switch (
      List.find_opt(
        r => r.r_id == id && r.r_leaf == leaf,
        leaf_ranges(~tail_id, seg),
      )
    ) {
    | Some(r) =>
      let target =
        switch (rel) {
        | In(off) => min(r.r_start + remap(id, leaf, off), r.r_stop)
        | After(k) => r.r_stop + k
        | Before(k) => max(0, r.r_start - k)
        };
      with_caret_at(target, z);
    | None => z
    }
  };
};

/* Map an offset through the edit turning [old_] into [new_]: positions
   before the change stay, after it shift, inside it snap to its end. */
let map_offset = (old_: string, new_: string, off: int): int =>
  switch (diff(old_, new_)) {
  | None => off
  | Some({index, delete, insert}) =>
    if (off <= index) {
      off;
    } else if (off >= index + delete) {
      off - delete + utf16_length(insert);
    } else {
      index + utf16_length(insert);
    }
  };

/* ---- the wire (JSON exchanged with collab.js) ---- */

module Wire = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type item = {
    id: string,
    parent: option(string),
    order: string,
    kind: string,
    lead: string,
    header: string,
    body: string,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type leaf_change = {
    id: string,
    leaf: string,
    text: string,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type load = {
    seq: int,
    items: list(item),
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type remote = {
    seq: int,
    leaves: list(leaf_change),
    upserts: list(item),
    deletes: list(string),
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  /* a caret in a leaf (leaf/anchor/head) or on a delimiter (delim/off) */
  type peer = {
    peer: string,
    user: option(string),
    name: string,
    color: string,
    id: string,
    leaf: option(string),
    anchor: option(int),
    head: option(int),
    delim: option(int),
    off: option(int),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type msg =
  | Load(Wire.load)
  | Remote(Wire.remote)
  | Peers(list(Wire.peer));

let kind_of_string = (s: string): kind =>
  switch (s) {
  | "type" => Type
  | "module" => Module
  | "stmt" => Stmt
  | "tail" => Tail
  | _ => Def
  };
let string_of_kind = (k: kind): string =>
  switch (k) {
  | Def => "def"
  | Type => "type"
  | Module => "module"
  | Stmt => "stmt"
  | Tail => "tail"
  };
let leaf_of_string = (s: string): leaf =>
  switch (s) {
  | "lead" => Lead
  | "header" => Header
  | _ => Body
  };
let string_of_leaf = (l: leaf): string =>
  switch (l) {
  | Lead => "lead"
  | Header => "header"
  | Body => "body"
  };

let id_of_string = (s: string): Id.t =>
  switch (Id.of_string(s)) {
  | Some(id) => id
  | None => Id.mk_str(s) /* non-UUID ids (e.g. a doc's seeded tail) */
  };

let item_of_wire = (w: Wire.item): item => {
  id: id_of_string(w.id),
  kind: kind_of_string(w.kind),
  lead: w.lead,
  header: w.header,
  body: w.body,
};

/* The local caret as sent to collaborators: in a leaf's text, or on one of
   an item's delimiters (`let`/`=`/`in`, `;`), which are structure, not
   text, as the tile shard index and an offset into that token. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type caret =
  | LeafAt(Id.t, leaf, int, int) /* item, leaf, anchor, head */
  | DelimAt(Id.t, int, int); /* item, shard, offset into the token */

/* ---- session state ----
   Mirrors the shared document; deliberately outside the (undoable,
   persisted) app model. */
module State = {
  let active = ref(false);
  /* per-leaf basis: the seq of the document version each leaf's text
     derives from (see collab/src/session.ts); unset leaves use [load_seq] */
  let load_seq = ref(0);
  let bases: Hashtbl.t((Id.t, leaf), int) = Hashtbl.create(64);
  let tail_id = ref(Id.invalid);
  /* doc id string for each item id (ids that aren't UUIDs map via mk_str) */
  let doc_ids: ref(Id.Map.t(string)) = ref(Id.Map.empty);
  let orders: ref(Id.Map.t(string)) = ref(Id.Map.empty);
  /* the items as last synced with the doc, with their top-level pieces */
  let synced: ref(list((item, list(Piece.t)))) = ref([]);
  let peers: ref(list(Wire.peer)) = ref([]);
  let last_caret: ref(option(caret)) = ref(None);
};

let doc_id = (id: Id.t): string =>
  switch (Id.Map.find_opt(id, State.doc_ids^)) {
  | Some(s) => s
  | None => Id.to_string(id)
  };

let remember = (w: Wire.item): item => {
  let it = item_of_wire(w);
  State.doc_ids := Id.Map.add(it.id, w.id, State.doc_ids^);
  State.orders := Id.Map.add(it.id, w.order, State.orders^);
  if (it.kind == Tail && w.parent == None) {
    State.tail_id := it.id;
  };
  it;
};

/* Program order: by (order key, doc id), tail last (as collab.js). */
let sort_items = (items: list(item)): list(item) => {
  let key = (it: item) => (
    it.kind == Tail ? 1 : 0,
    Option.value(Id.Map.find_opt(it.id, State.orders^), ~default=""),
    doc_id(it.id),
  );
  List.stable_sort((a, b) => compare(key(a), key(b)), items);
};

let basis_of = (id: Id.t, leaf: leaf): int =>
  switch (Hashtbl.find_opt(State.bases, (id, leaf))) {
  | Some(b) => b
  | None => State.load_seq^
  };
let set_basis = (id: Id.t, leaf: leaf, seq: int): unit =>
  Hashtbl.replace(State.bases, (id, leaf), seq);
/* would a message at [seq] carry news for this leaf? (no if we've edited
   it since the message was made) */
let fresh_for = (id: Id.t, leaf: leaf, seq: int): bool =>
  basis_of(id, leaf) <= seq;

/* ---- calls into collab.js ---- */

module JsApi = {
  open Js_of_ocaml;

  let api = (): option(Js.Unsafe.any) => {
    let a = Js.Unsafe.get(Js.Unsafe.global, "hazelCollab");
    Js.Optdef.test(a) && Js.Opt.test(a) ? Some(a) : None;
  };

  let call =
      (meth: string, args: array(Js.Unsafe.any)): option(Js.Unsafe.any) =>
    switch (api()) {
    | Some(a) => Some(Js.Unsafe.meth_call(a, meth, args))
    | None => None
    };

  let str = (s: string) => Js.Unsafe.inject(Js.string(s));
  let int = (n: int) => Js.Unsafe.inject(n);
  let opt_str = (s: option(string)) =>
    switch (s) {
    | Some(s) => str(s)
    | None => Js.Unsafe.inject(Js.null)
    };

  let edit = (basis: int, id: Id.t, leaf: leaf, text: string): int =>
    switch (
      call(
        "edit",
        [|
          int(basis),
          str(doc_id(id)),
          str(string_of_leaf(leaf)),
          str(text),
        |],
      )
    ) {
    | Some(r) => int_of_float(Js.float_of_number(Js.Unsafe.coerce(r)))
    | None => basis
    };

  let item_json = (it: item): string =>
    Yojson.Safe.to_string(
      `Assoc([
        ("id", `String(doc_id(it.id))),
        ("kind", `String(string_of_kind(it.kind))),
        ("lead", `String(it.lead)),
        ("header", `String(it.header)),
        ("body", `String(it.body)),
      ]),
    );

  /* returns the basis for the new item's leaves */
  let insert = (it: item, after: option(Id.t)): option(int) =>
    call(
      "insert",
      [|str(item_json(it)), opt_str(Option.map(doc_id, after))|],
    )
    |> Option.map(r =>
         int_of_float(Js.float_of_number(Js.Unsafe.coerce(r)))
       );
  let remove = (id: Id.t) => ignore(call("remove", [|str(doc_id(id))|]));
  let move = (id: Id.t, after: option(Id.t)) =>
    ignore(
      call(
        "move",
        [|str(doc_id(id)), opt_str(Option.map(doc_id, after))|],
      ),
    );
  let caret = (c: option(caret)) =>
    switch (c) {
    | Some(DelimAt(id, shard, off)) =>
      ignore(
        call("caretDelim", [|str(doc_id(id)), int(shard), int(off)|]),
      )
    | Some(LeafAt(id, leaf, anchor, head)) =>
      ignore(
        call(
          "caret",
          [|
            str(doc_id(id)),
            str(string_of_leaf(leaf)),
            int(anchor),
            int(head),
          |],
        ),
      )
    | None => ignore(call("caret", [|Js.Unsafe.inject(Js.null)|]))
    };

  /* collab.js calls these; each schedules a message for the app */
  let register_host = (schedule: msg => unit) => {
    let parse = (f, json) =>
      switch (f(Yojson.Safe.from_string(Js.to_string(json)))) {
      | m => schedule(m)
      | exception e =>
        Firebug.console##error(
          Js.string("[hazel-collab] bad message: " ++ Printexc.to_string(e)),
        )
      };
    let host =
      Js.Unsafe.obj([|
        (
          "load",
          Js.Unsafe.inject(
            Js.wrap_callback(parse(j => Load(Wire.load_of_yojson(j)))),
          ),
        ),
        (
          "remote",
          Js.Unsafe.inject(
            Js.wrap_callback(parse(j => Remote(Wire.remote_of_yojson(j)))),
          ),
        ),
        (
          "peers",
          Js.Unsafe.inject(
            Js.wrap_callback(
              parse(j =>
                Peers(
                  switch (j) {
                  | `List(ps) => List.map(Wire.peer_of_yojson, ps)
                  | _ => []
                  },
                )
              ),
            ),
          ),
        ),
      |]);
    Js.Unsafe.set(Js.Unsafe.global, "hazelCollabHost", host);
    /* collab.js may have booted first and be waiting for us */
    switch (
      Js.Optdef.to_option(
        Js.Unsafe.get(Js.Unsafe.global, "hazelCollabReady"),
      )
    ) {
    | Some(f) => ignore(Js.Unsafe.fun_call(f, [||]))
    | None => ()
    };
  };
};

/* ---- local changes -> the doc ---- */

/* Diff the live program against what was last synced and send the
   difference. Cheap per keystroke: unchanged items are reused by piece
   identity. */
let sync_local = (seg: Segment.t): unit =>
  if (State.active^) {
    switch (
      items_of_seg_cached(~tail_id=State.tail_id^, ~prev=State.synced^, seg)
    ) {
    | None => () /* not an item chain right now; try again next edit */
    | Some(cur) =>
      let ops =
        diff_items(List.map(fst, State.synced^), List.map(fst, cur));
      List.iter(
        fun
        | Edit(id, leaf, text) =>
          set_basis(
            id,
            leaf,
            JsApi.edit(basis_of(id, leaf), id, leaf, text),
          )
        | Insert(it, after) =>
          switch (JsApi.insert(it, after)) {
          | Some(b) =>
            List.iter(l => set_basis(it.id, l, b), [Lead, Header, Body])
          | None => ()
          }
        | Remove(id) => JsApi.remove(id)
        | Move(id, after) => JsApi.move(id, after),
        ops,
      );
      State.synced := cur;
    };
  };

/* Record [seg] as in sync with the doc (after applying remote changes). */
let mark_synced = (seg: Segment.t): unit =>
  switch (
    items_of_seg_cached(~tail_id=State.tail_id^, ~prev=State.synced^, seg)
  ) {
  | Some(cur) => State.synced := cur
  | None => ()
  };

let send_caret = (c: option(caret)): unit =>
  if (State.active^ && c != State.last_caret^) {
    State.last_caret := c;
    JsApi.caret(c);
  };
