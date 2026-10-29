/* A cell's body is the definition's RHS child segment (between `=` and
   `in`/`;`), complete and grouted; slicing the whole `let…in` tile
   leaves a prefix tile without its operand and crashes Skel. A program's
   items are ItemSpans' (the division the parser and the layout use): a
   row is found through its levels, never by walking the program */
open Haz3lcore;
open Util;

module S = ItemSpans;

/* cells hold slices without edge whitespace (stray in an isolated
   cell); splicing re-wraps them in the program's own padding, so it
   round-trips unstored. Comments are content, not padding. */
let is_edge_ws = (p: Piece.t): bool =>
  switch (p) {
  | Secondary({content: Whitespace(_), _}) => true
  | _ => false
  };

let trim_ws = (seg: Segment.t): (Segment.t, Segment.t, Segment.t) => {
  let rec take = ps =>
    switch (ps) {
    | [p, ...rest] when is_edge_ws(p) =>
      let (pre, core) = take(rest);
      ([p, ...pre], core);
    | _ => ([], ps)
    };
  let (pre, rest) = take(seg);
  let (fus, eroc) = take(List.rev(rest));
  (pre, List.rev(eroc), List.rev(fus));
};

let core_ws = (seg: Segment.t): Segment.t => {
  let (_, core, _) = trim_ws(seg);
  core;
};

/* [content] in the edge whitespace of [find]'s slice of [seg], which
   is untouched while the cell is open */
let rewrap_ws =
    (find: (Id.t, Segment.t) => option(Segment.t), fid, seg, content)
    : Segment.t =>
  switch (find(fid, seg)) {
  | Some(old) =>
    let (pre, _, suf) = trim_ws(old);
    pre @ content @ suf;
  | None => content
  };

let seg_contains_id = (id: Id.t, seg: Segment.t): bool =>
  Option.is_some(Segment.find_ctx(seg, id));

/* split [ps] at the first `;` piece: (def run, separator + rest) */
let split_at_semi = (ps: list(Piece.t)): (list(Piece.t), list(Piece.t)) => {
  let rec go = (acc, ps) =>
    switch (ps) {
    | [] => (List.rev(acc), [])
    | [p, ..._] when Piece.is_semi(p) => (List.rev(acc), ps)
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], ps);
};

/* the first tile among [arr]'s pieces [lo, hi) */
let rec first_tile = (arr: array(Piece.t), lo: int, hi: int): option(Tile.t) =>
  lo >= hi
    ? None
    : (
      switch (arr[lo]) {
      | Tile(t) => Some(t)
      | _ => first_tile(arr, lo + 1, hi)
      }
    );

/* a def item's head: a let, type or module tile */
let is_def_head = (t: Tile.t): bool =>
  switch (Tile.label(t)) {
  | ["let" | "type" | "module", ..._] => true
  | _ => false
  };

/* `use X in`: an item with no cell, as it has nothing of its own */
let is_use = (t: Tile.t): bool => Tile.token(t, 0) == "use";

/* a form whose child is a module's member list: a module body's braces */
let holds_members = (m: Mold.t): bool => List.mem(Sort.Mod, m.in_);

/* identity-preserving map: [xs] itself when [f] changed nothing, so
   splices re-mint only the spine above the replacement and pointer-keyed
   caches downstream (incremental parse, outline memo) still hit */
let map_sharing = (f: 'a => 'a, xs: list('a)): list('a) => {
  let ys = List.map(f, xs);
  List.for_all2((===), xs, ys) ? xs : ys;
};

/* the original piece on no change: rebuilding even the variant wrapper
   (Piece.Tile(t)) breaks pointer equality upstream */
let tile_sharing =
    (p: Piece.t, t: Base.tile, children: list(Segment.t)): Piece.t =>
  children === t.children
    ? p
    : Piece.Tile({
        ...t,
        children,
      });

/* ---- where a piece is: the levels down to it ---- */

/* one level on the way down to a piece: an ItemSpans level, the index
   there of the piece on the way, and how it hangs from the step above */
type step = {
  lv: S.level,
  i: int,
  from,
}
and from =
  | Top
  /* that child of the tile at the step above */
  | Kid(Tile.t, int)
  /* the step above's pieces from [lv.offset]: a member's let chain or its
     definition's body run */
  | Sub
  /* the syntax of the projector at the step above, which no cell cuts */
  | Syntax;

let projected = (path: list(step)): bool =>
  List.exists(
    (st: step) =>
      switch (st.from) {
      | Syntax => true
      | Top
      | Kid(_)
      | Sub => false
      },
    path,
  );

let step = (~from: from, lv: S.level, i: int): step => {
  lv,
  i,
  from,
};

/* the span whose slice holds [lv]'s piece [i]: slices partition a level */
let slice_at = (lv: S.level, i: int): option(int) => {
  let rec go = (a, b) =>
    if (a >= b) {
      None;
    } else {
      let m = (a + b) / 2;
      let sp = lv.spans[m];
      i < sp.lo ? go(a, m) : i >= sp.hi ? go(m + 1, b) : Some(m);
    };
  go(0, Array.length(lv.spans));
};

/* the item holding [lv]'s piece [i]: a span holds its core and its `;` or
   `in`. What precedes a core (a function's head, comments before an `…in`
   item) is in no item */
let item_at = (lv: S.level, i: int): option(int) =>
  switch (slice_at(lv, i)) {
  | Some(k) when fst(lv.spans[k].core) <= i => Some(k)
  | _ => None
  };

/* span [k] of [lv] ends with its `;` */
let semi_ended = (lv: S.level, k: int): bool => {
  let sp = lv.spans[k];
  let rec go = i => i < sp.hi && (Piece.is_semi(lv.pieces[i]) || go(i + 1));
  go(snd(sp.core));
};

/* span [k] reads as its level's result: a tail, or a module's last member
   without its `;` */
let is_tail = (lv: S.level, k: int): bool =>
  switch (lv.spans[k].kind) {
  | Tail => true
  | Stmt => !semi_ended(lv, k)
  | Def
  | Use => false
  };

/* a module literal's members (by braces in a program, or anywhere) */
let is_members = (lv: S.level): bool =>
  switch (lv.owner) {
  | Members(_) => true
  | Top
  | Chain(_)
  | Block(_) => false
  };

/* span [k]'s level over its own pieces: a member's let chain, or its
   definition's body run */
let sub = (lv: S.level, k: int): option(S.level) =>
  switch (S.chain(lv, k)) {
  | Some(_) as chain => chain
  | None =>
    switch (S.site_of(lv, k)) {
    | Some(S.Run(_)) => S.block(lv, k)
    | Some(S.Kid(_))
    | None => None
    }
  };

/* the sub-level holding [lv]'s piece [i] */
let sub_at = (lv: S.level, i: int): option(S.level) =>
  switch (Option.bind(slice_at(lv, i), sub(lv))) {
  | Some(s) when s.offset <= i && i < s.offset + Array.length(s.pieces) =>
    Some(s)
  | _ => None
  };

/* the level of child [c] of tile [t], [lv]'s piece [i]. A module literal's
   members and a definition's body are ItemSpans' levels; any other child
   is cut by the nested rule (an undivided expression has no items), a
   member list by the module rule */
let kid_level = (lv: S.level, i: int, t: Tile.t, c: int): S.level => {
  let own =
    switch (slice_at(lv, i)) {
    | Some(k) when lv.spans[k].body == Some(t.id) => S.members(lv, k)
    | Some(k) =>
      switch (S.site_of(lv, k)) {
      | Some(S.Kid(h, _)) when h == i && c == List.length(t.children) - 1 =>
        S.block(lv, k)
      | _ => None
      }
    | None => None
    };
  switch (own) {
  | Some(level) => level
  | None =>
    let members = holds_members(Tile.mold(t));
    S.build(
      ~sort=members ? Sort.Mod : Sort.Exp,
      ~owner=members ? Members(t.id) : Block(t.id),
      ~nested=!members,
      ~bol=false,
      ~eol=false,
      ~indent=Some(S.kid_indent(~outer=S.outer(lv), lv.pieces, i, t, c)),
      ~offset=0,
      List.nth(t.children, c),
    );
  };
};

/* the syntax of the projector [pr], [lv]'s piece [i] */
let syntax_level = (lv: S.level, i: int, pr: Base.projector): S.level =>
  S.build(
    ~sort=Exp,
    ~owner=Block(pr.id),
    ~nested=true,
    ~bol=false,
    ~eol=false,
    ~indent=Some(S.indent_at(~outer=S.outer(lv), lv.pieces, i)),
    ~offset=0,
    [pr.syntax],
  );

let rec piece_holds = (fid: Id.t, p: Piece.t): bool =>
  Piece.id(p) == fid
  || (
    switch (p) {
    | Tile(t) => List.exists(List.exists(piece_holds(fid)), t.children)
    | Projector(pr) => piece_holds(fid, pr.syntax)
    | Grout(_)
    | Secondary(_) => false
    }
  );

/* every piece deep in a span's slice, to the index in the slice of the
   piece holding it: which item holds a piece, without walking the
   program. Memoized per span on its pieces, as ItemSpans' levels are */
type held = {
  h_pieces: array(Piece.t),
  h_at: Hashtbl.t(Id.t, int),
};
let held_cache: Swept.t((Id.t, Id.t), held) = Swept.mk();

let held = (lv: S.level, k: int): Hashtbl.t(Id.t, int) => {
  let sp = lv.spans[k];
  let n = sp.hi - sp.lo;
  let same = (h: held) => {
    let rec go = j =>
      j >= n || h.h_pieces[j] === lv.pieces[sp.lo + j] && go(j + 1);
    Array.length(h.h_pieces) == n && go(0);
  };
  let key = (sp.anchor, Piece.id(lv.pieces[sp.lo]));
  switch (Swept.find_opt(held_cache, key)) {
  | Some(h) when same(h) => h.h_at
  | _ =>
    let at = Hashtbl.create(64);
    let rec add = (j, p: Piece.t) => {
      Hashtbl.replace(at, Piece.id(p), j);
      switch (p) {
      | Tile(t) => List.iter(List.iter(add(j)), t.children)
      | Projector(pr) => add(j, pr.syntax)
      | Grout(_)
      | Secondary(_) => ()
      };
    };
    for (j in 0 to n - 1) {
      add(j, lv.pieces[sp.lo + j]);
    };
    Swept.replace(
      held_cache,
      key,
      {
        h_pieces: Array.sub(lv.pieces, sp.lo, n),
        h_at: at,
      },
    );
    at;
  };
};

/* the index in [lv]'s pieces of [fid] itself, or of the piece it lies
   deep in: by its spans' indexes (a level without items, an undivided
   expression, is searched) */
let holding = (lv: S.level, fid: Id.t): option(int) => {
  let n = Array.length(lv.pieces);
  let rec flat = i =>
    i >= n ? None : Piece.id(lv.pieces[i]) == fid ? Some(i) : flat(i + 1);
  let rec span = k =>
    k >= Array.length(lv.spans)
      ? None
      : (
        switch (Hashtbl.find_opt(held(lv, k), fid)) {
        | Some(j) => Some(lv.spans[k].lo + j)
        | None => span(k + 1)
        }
      );
  let rec deep = i =>
    i >= n ? None : piece_holds(fid, lv.pieces[i]) ? Some(i) : deep(i + 1);
  switch (flat(0)) {
  | Some(_) as found => found
  | None => Array.length(lv.spans) > 0 ? span(0) : deep(0)
  };
};

/* the way down inside piece [p] to [fid]: each child (or projector
   syntax) on the way, with the index there of the next piece */
let rec way = (fid: Id.t, p: Piece.t): option(list((from, int))) => {
  let rec in_seg = (j, ps: Segment.t) =>
    switch (ps) {
    | [] => None
    | [q, ..._] when Piece.id(q) == fid => Some((j, []))
    | [q, ...ps] =>
      switch (way(fid, q)) {
      | Some(w) => Some((j, w))
      | None => in_seg(j + 1, ps)
      }
    };
  switch (p) {
  | Tile(t) =>
    List.find_mapi(
      (c, kid) =>
        Option.map(((j, w)) => [(Kid(t, c), j), ...w], in_seg(0, kid)),
      t.children,
    )
  | Projector(pr) =>
    Option.map(((j, w)) => [(Syntax, j), ...w], in_seg(0, [pr.syntax]))
  | Grout(_)
  | Secondary(_) => None
  };
};

/* the steps from [lv]'s piece [i] down the way [w] */
let rec descend =
        (~from: from, lv: S.level, i: int, w: list((from, int)))
        : list(step) => {
  let st = step(~from, lv, i);
  switch (sub_at(lv, i), w) {
  | (Some(s), _) => [st, ...descend(~from=Sub, s, i - s.offset, w)]
  | (None, [(Kid(t, c), j), ...w]) => [
      st,
      ...descend(~from=Kid(t, c), kid_level(lv, i, t, c), j, w),
    ]
  | (None, [(Syntax, j), ...w]) =>
    switch (lv.pieces[i]) {
    | Projector(pr) => [
        st,
        ...descend(~from=Syntax, syntax_level(lv, i, pr), j, w),
      ]
    | _ => [st]
    }
  | (None, [(Top | Sub, _), ..._])
  | (None, []) => [st]
  };
};

/* the steps from [lv] (reached [from]) into span [k]'s module literal:
   its brace's piece (or the head it defines, and the definition's), then
   [members], the member level's steps reached from the brace */
let into_literal =
    (~from: from, lv: S.level, k: int, members: from => option(list(step)))
    : option(list(step)) =>
  switch (S.site_of(lv, k)) {
  | Some(site) =>
    switch (S.brace_of(lv.pieces, site), site) {
    | (Some((b, brace)), S.Run(_)) =>
      Option.map(
        below => [step(~from, lv, b), ...below],
        members(Kid(brace, 0)),
      )
    | (Some((b, brace)), S.Kid(h, t)) =>
      let c = List.length(t.children) - 1;
      Option.map(
        below =>
          [
            step(~from, lv, h),
            step(~from=Kid(t, c), kid_level(lv, h, t, c), b),
            ...below,
          ],
        members(Kid(brace, 0)),
      );
    | _ => None
    }
  | None => None
  };

/* the steps from [lv]'s piece [i], which is or holds [fid], down to it */
let rec steps_at = (~from: from, lv: S.level, i: int, fid: Id.t): list(step) =>
  switch (sub_at(lv, i)) {
  | Some(s) => [
      step(~from, lv, i),
      ...steps_at(~from=Sub, s, i - s.offset, fid),
    ]
  | None when Piece.id(lv.pieces[i]) == fid => [step(~from, lv, i)]
  | None =>
    switch (literal(~from, lv, i, fid)) {
    | Some(steps) => steps
    | None =>
      descend(
        ~from,
        lv,
        i,
        Option.value(way(fid, lv.pieces[i]), ~default=[]),
      )
    }
  }
/* through the module literal at [lv]'s piece [i], when its members hold
   [fid]: their indexes find it without walking the module */
and literal =
    (~from: from, lv: S.level, i: int, fid: Id.t): option(list(step)) =>
  switch (slice_at(lv, i)) {
  | Some(k) when lv.spans[k].body != None =>
    switch (S.members(lv, k)) {
    | Some(m) =>
      Option.bind(holding(m, fid), j =>
        into_literal(~from, lv, k, from => Some(steps_at(~from, m, j, fid)))
      )
    | None => None
    }
  | _ => None
  };

/* ItemSpans.find's way to the span anchored at [fid], as steps */
let found_steps =
    (fid: Id.t, found: list((S.level, int))): option(list(step)) => {
  let rec go = (~from, path) =>
    switch (path) {
    | [] => None
    | [(lv, k)] =>
      let sp = lv.spans[k];
      let rec at = i =>
        i >= sp.hi
          ? None : Piece.id(lv.pieces[i]) == fid ? Some(i) : at(i + 1);
      Option.map(i => steps_at(~from, lv, i, fid), at(sp.lo));
    | [(lv, k), (child: S.level, _) as next, ...rest] =>
      switch (child.owner) {
      | Chain(_) =>
        Option.bind(go(~from=Sub, [next, ...rest]), below =>
          switch (below) {
          | [st, ..._] =>
            Some([step(~from, lv, st.i + child.offset), ...below])
          | [] => None
          }
        )
      | Members(_) =>
        into_literal(~from, lv, k, from => go(~from, [next, ...rest]))
      | Top
      | Block(_) => None
      }
    };
  go(~from=Top, found);
};

/* the steps from the program [seg] (at [root]) down to the piece [fid],
   outermost first: by ItemSpans' anchors where it heads an item or a
   statement, else through the levels' indexes. Through projectors too: a
   ⇒ row's id is its expression's root, inside a projector around it */
let find_path =
    (~root: Sort.t, fid: Id.t, seg: Segment.t): option(list(step)) => {
  Swept.next_pass(held_cache);
  let found = Option.bind(S.find(~root, fid, seg), found_steps(fid));
  switch (found) {
  | Some(_) => found
  | None =>
    let top = S.top(~root, seg);
    Option.map(i => steps_at(~from=Top, top, i, fid), holding(top, fid));
  };
};

/* the whole segment holding [path]'s piece and its index there, and the
   steps down to it: a sub-level's pieces lie in the level above */
let rec home = (~seg, path: list(step)): (list(step), Segment.t, int) =>
  switch (List.rev(path)) {
  | [{from: Sub, lv, i}, ...above] =>
    let (up, level, _) = home(~seg, List.rev(above));
    (up, level, i + lv.offset);
  | [{from: Top, i, _}] => (path, seg, i)
  | [{lv, i, _}, ..._] => (path, lv.seg, i)
  | [] => ([], seg, 0)
  };

/* a step's level's pieces, the program's own at the top */
let pieces_of = (~seg: Segment.t, st: step): Segment.t =>
  switch (st.from) {
  | Top => seg
  | Kid(_)
  | Sub
  | Syntax => st.lv.seg
  };

/* the program [seg] with [path]'s last level made [pieces]: only the levels
   on the way are re-made, and nothing when nothing changed */
let rebuild =
    (~seg: Segment.t, path: list(step), pieces: Segment.t): Segment.t => {
  let own = pieces_of(~seg);
  /* innermost first */
  let rec up = (rev: list(step), kid: Segment.t) =>
    switch (rev) {
    | [] => seg
    | [top] => Segment.ptr_eq(kid, own(top)) ? seg : kid
    | [st, ...[above, ..._] as rest] =>
      let parent = own(above);
      up(
        rest,
        Segment.ptr_eq(kid, own(st))
          ? parent
          : (
            switch (st.from) {
            | Sub =>
              ListUtil.truncate(st.lv.offset, parent)
              @ kid
              @ ListUtil.remove_first_n(
                  st.lv.offset + Array.length(st.lv.pieces),
                  parent,
                )
            | Kid(t, c) =>
              ListUtil.put_nth(
                above.i,
                tile_sharing(
                  above.lv.pieces[above.i],
                  t,
                  ListUtil.put_nth(c, kid, t.children),
                ),
                parent,
              )
            /* no cell splices into a projector's syntax */
            | Syntax
            | Top => parent
            }
          ),
      );
    };
  up(List.rev(path), pieces);
};

/* [path]'s last level with its pieces [a, b) replaced by [repl], inside
   their edge whitespace */
let splice_range =
    (~seg: Segment.t, path: list(step), a: int, b: int, repl: Segment.t)
    : Segment.t => {
  let pieces = ListUtil.last(path).lv.seg;
  let (pre, _, suf) = trim_ws(ListUtil.slice(a, b - a, pieces));
  rebuild(
    ~seg,
    path,
    ListUtil.truncate(a, pieces)
    @ pre
    @ repl
    @ suf
    @ ListUtil.remove_first_n(b, pieces),
  );
};

/* span [k]'s core: the item without edge whitespace or its `;` */
let core = (lv: S.level, k: int): Segment.t => {
  let (c0, c1) = lv.spans[k].core;
  Array.to_list(Array.sub(lv.pieces, c0, c1 - c0));
};

/* the indentation of the line the piece [id] is on */
let line_indent = (~root: Sort.t, id: Id.t, seg: Segment.t): option(int) =>
  switch (find_path(~root, id, seg)) {
  | Some(path) when !projected(path) =>
    let st = ListUtil.last(path);
    Some(S.indent_at(~outer=S.outer(st.lv), st.lv.pieces, st.i));
  | _ => None
  };

/* how a step's level holds its piece on the way to the row [fid]: in a
   statement or the tail (span k), as the row's own definition, or not */
type claim =
  | Span(int)
  | Item
  | Pass;

let claim_at = (fid: Id.t, st: step): claim =>
  switch (item_at(st.lv, st.i)) {
  | None => Pass
  | Some(k) =>
    switch (st.lv.spans[k].kind) {
    | Stmt
    | Tail => Span(k)
    | Def
    | Use => st.lv.spans[k].anchor == fid ? Item : Pass
    }
  };

/* a headless cell's symbol: a statement's `;`, or a result's ⇒ */
let span_sym = (lv: S.level, k: int): string =>
  is_tail(lv, k) ? {js|⇒|js} : {js|;|js};

/* where the headless row [fid]'s cell is cut: the steps down to the level
   holding its span, and the span. The innermost level holding the row's
   piece decides, as OutlineTree reads a block on through the parentheses
   or projector around its tail. None for a definition, and under a
   projector, whose syntax no cell cuts. [path] is find_path's */
let locate_along =
    (fid: Id.t, path: list(step)): option((list(step), int)) => {
  /* innermost first */
  let rec up = (steps: list(step)) =>
    switch (steps) {
    | [] => None
    | [st, ...outer] =>
      switch (claim_at(fid, st)) {
      | Pass => up(outer)
      | Item => None
      | Span(k) => projected(steps) ? None : Some((List.rev(steps), k))
      }
    };
  up(List.rev(path));
};

let locate_headless =
    (~root: Sort.t, fid: Id.t, seg: Segment.t): option((list(step), int)) =>
  Option.bind(find_path(~root, fid, seg), locate_along(fid));

let headless_content_deep =
    (~root: Sort.t, fid: Id.t, seg: Segment.t): option((Segment.t, string)) =>
  locate_headless(~root, fid, seg)
  |> Option.map(((path, k)) => {
       let lv = ListUtil.last(path).lv;
       (core(lv, k), span_sym(lv, k));
     });

/* [seg] with [repl] in place of the headless row [fid]'s span, inside its
   edge whitespace; only the levels on the way down to it are re-made */
let splice_headless =
    (~root: Sort.t, fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  switch (locate_headless(~root, fid, seg)) {
  | None => seg
  | Some((path, k)) =>
    let (c0, c1) = ListUtil.last(path).lv.spans[k].core;
    splice_range(~seg, path, c0, c1, repl);
  };

/* the tile [fid] (not under a projector, whose syntax no cell cuts), the
   steps down to the segment it sits in, the segment and its index there */
let tile_at =
    (~root: Sort.t, fid: Id.t, seg: Segment.t)
    : option((list(step), Segment.t, int, Tile.t)) =>
  switch (find_path(~root, fid, seg)) {
  | Some(path) when !projected(path) =>
    let {lv, i, _} = ListUtil.last(path);
    switch (lv.pieces[i]) {
    | Tile(t) =>
      let (up, level, j) = home(~seg, path);
      Some((up, level, j, t));
    | _ => None
    };
  | _ => None
  };

/* the tile [fid], the segment it sits in and its index there */
let tile_ctx =
    (~root: Sort.t, fid: Id.t, seg: Segment.t)
    : option((Segment.t, int, Tile.t)) =>
  Option.map(
    ((_, level, i, t)) => (level, i, t),
    tile_at(~root, fid, seg),
  );

/* [seg] with the segment holding tile [fid] replaced by [f] of it, the
   tile's index and the tile; the program's own pieces wherever nothing
   changed */
let splice_at =
    (
      ~root: Sort.t,
      fid: Id.t,
      f: (Segment.t, int, Tile.t) => Segment.t,
      seg: Segment.t,
    )
    : Segment.t =>
  switch (tile_at(~root, fid, seg)) {
  | None => seg
  | Some((path, level, i, t)) =>
    let level' = f(level, i, t);
    Segment.ptr_eq(level', level) ? seg : rebuild(~seg, path, level');
  };

/* the `test` before the statement `;` [semi]: a run member's row is its
   `;`, which stays outside the run's cell */
let test_before_semi =
    (~root: Sort.t, semi: Id.t, seg: Segment.t): option(Id.t) =>
  Option.bind(tile_ctx(~root, semi, seg), ((level, i, _)) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t)
            when Tile.token(t, 0) == "test" || Tile.token(t, 0) == "hint" =>
          Some(t.id)
        | _ => None
        },
      List.rev(ListUtil.truncate(i, level)),
    )
  );

/* span [k]'s leading `test` tile: module members are repped by it,
   top-level statements by their `;`, so run members carry both ids
   (harmless: consumers test membership against outline ids) */
let span_test = (lv: S.level, k: int): option(Id.t) => {
  let sp = lv.spans[k];
  switch (first_tile(lv.pieces, fst(sp.core), sp.hi)) {
  /* a hinted test (`hint "…" test … end`) is a test too, as the outline
     counts it */
  | Some(t) when Tile.token(t, 0) == "test" || Tile.token(t, 0) == "hint" =>
    Some(t.id)
  | _ => None
  };
};

/* contiguous test runs: the outline's "tests" container opens one cell
   spanning the whole run. A test statement joins a run; in a module body,
   a Mod root's top level included, a final `;`-less test does too, but an
   expression block's tail is its value and never does */
let in_run = (lv: S.level, k: int): bool => {
  let members =
    switch (lv.owner) {
    | Top => lv.sort == Sort.Mod
    | Members(_)
    | Chain(_)
    | Block(_) => is_members(lv)
    };
  span_test(lv, k) != None
  && (
    switch (lv.spans[k].kind) {
    | Stmt => semi_ended(lv, k) || members
    | Tail => members
    | Def
    | Use => false
    }
  );
};

/* the maximal run of tests holding [path]'s piece: the steps down to its
   level, its first and last spans, (member ids, test count). The
   outermost level whose item holding the piece is in one decides */
let run_along =
    (path: list(step))
    : option((list(step), int, int, (list(Id.t), int))) => {
  let rec down = (above, steps: list(step)) =>
    switch (steps) {
    | [] => None
    | [st, ...below] =>
      switch (item_at(st.lv, st.i)) {
      | Some(j) when in_run(st.lv, j) =>
        let n = Array.length(st.lv.spans);
        let rec lo = j => j > 0 && in_run(st.lv, j - 1) ? lo(j - 1) : j;
        let rec hi = j => j + 1 < n && in_run(st.lv, j + 1) ? hi(j + 1) : j;
        let (a, b) = (lo(j), hi(j));
        let members =
          List.concat_map(
            k =>
              switch (span_test(st.lv, k)) {
              | Some(t) when t != st.lv.spans[k].anchor => [
                  t,
                  st.lv.spans[k].anchor,
                ]
              | _ => [st.lv.spans[k].anchor]
              },
            List.init(b - a + 1, k => a + k),
          );
        Some((List.rev([st, ...above]), a, b, (members, b - a + 1)));
      | _ => down([st, ...above], below)
      }
    };
  down([], path);
};

let run_at =
    (~root: Sort.t, fid: Id.t, seg: Segment.t)
    : option((list(step), int, int, (list(Id.t), int))) =>
  Option.bind(find_path(~root, fid, seg), run_along);

/* a run's text: from its first test's start to its last's `;` */
let run_text = (lv: S.level, a: int, b: int): Segment.t => {
  let c0 = fst(lv.spans[a].core);
  Array.to_list(Array.sub(lv.pieces, c0, snd(lv.spans[b].core) - c0));
};

/* the run holding [fid]: its text, its member ids and how many tests it
   has (members carry two ids each) */
let test_run_deep =
    (~root: Sort.t, fid: Id.t, seg: Segment.t)
    : option((Segment.t, list(Id.t), int)) =>
  run_at(~root, fid, seg)
  |> Option.map(((path, a, b, (members, n))) =>
       (run_text(ListUtil.last(path).lv, a, b), members, n)
     );

/* [seg] with [repl] in place of the run holding [fid], inside its edge
   whitespace */
let splice_run =
    (~root: Sort.t, fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  switch (run_at(~root, fid, seg)) {
  | None => seg
  | Some((path, a, b, _)) =>
    let lv = ListUtil.last(path).lv;
    splice_range(
      ~seg,
      path,
      fst(lv.spans[a].core),
      snd(lv.spans[b].core),
      repl,
    );
  };

/* the definition RHS of the item [t] at [i] in [level]: a 3-shard
   `let … = … in`'s last child, or for a 2-shard member `let … =`, the
   sibling run after it up to the next `;` (or segment end) */
let def_at = ((level, i, t): (Segment.t, int, Tile.t)): option(Segment.t) =>
  Tile.ends_with_in(t)
    ? ListUtil.last_opt(t.children)
    : Some(fst(split_at_semi(ListUtil.remove_first_n(i + 1, level))));

let find_def = (~root: Sort.t, fid: Id.t, seg: Segment.t): option(Segment.t) =>
  Option.bind(tile_ctx(~root, fid, seg), def_at);

let splice_def =
    (~root: Sort.t, fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  splice_at(
    ~root,
    fid,
    (level, i, t) =>
      if (Tile.ends_with_in(t)) {
        switch (ListUtil.split_last_opt(t.children)) {
        /* the program's own tile when the text is its own */
        | Some((_, last)) when Segment.ptr_eq(repl, last) => level
        | Some((init, _)) =>
          ListUtil.put_nth(
            i,
            Piece.Tile({
              ...t,
              children: init @ [repl],
            }),
            level,
          )
        | None => level
        };
      } else {
        ListUtil.truncate(i + 1, level)
        @ repl
        @ snd(split_at_semi(ListUtil.remove_first_n(i + 1, level)));
      },
    seg,
  );

let zip_of_cell = (cell: CellEditor.Model.t): Segment.t =>
  Zipper.unselect_and_zip(cell.editor.editor.state.zipper);

/* the caret starts at the top of a fresh cell: unzip's default
   direction (Right) would leave it after the whole segment */
let cell_of_seg = (~root=Sort.Exp, seg: Segment.t): CellEditor.Model.t =>
  seg
  |> Zipper.unzip(~direction=Left)
  |> Editor.Model.mk(~root)
  |> CellEditor.Model.mk;

/* the header pattern is the first child of 2- and 3-shard items alike */
let find_pat = (~root: Sort.t, fid: Id.t, seg: Segment.t): option(Segment.t) =>
  Option.bind(tile_ctx(~root, fid, seg), ((_, _, t)) =>
    ListUtil.hd_opt(t.children)
  );

let splice_pat =
    (~root: Sort.t, fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  splice_at(
    ~root,
    fid,
    (level, i, t) =>
      switch (t.children) {
      | [first, ...rest] when !Segment.ptr_eq(repl, first) =>
        ListUtil.put_nth(
          i,
          Piece.Tile({
            ...t,
            children: [repl, ...rest],
          }),
          level,
        )
      | _ => level
      },
    seg,
  );

/* A cell shows its item from its own left edge: the indentation every
   line after the first has in the program (the [base] of the line the
   cell's text starts on) is cut on the way in and put back on the way
   out. Putting back hands over the program's own pieces wherever the
   cell left them alone (the incremental parse and layout check identity),
   and a line new to the cell gets [base] spaces made once for its line
   break. */

/* up to [n] of the spaces that open [ps] */
let rec drop_spaces = (n: int, ps: Segment.t): Segment.t =>
  switch (ps) {
  | [p, ...rest] when n > 0 && Piece.is_space(p) => drop_spaces(n - 1, rest)
  | _ => ps
  };

let dedent = (base: int, seg: Segment.t): Segment.t =>
  if (base == 0) {
    seg;
  } else {
    let rec go = (seg: Segment.t): Segment.t => {
      let rec walk = (ps: Segment.t): Segment.t =>
        switch (ps) {
        | [] => []
        | [p, ...rest] when Piece.is_linebreak(p) => [
            p,
            ...walk(drop_spaces(base, rest)),
          ]
        | [Tile(t) as p, ...rest] => [
            tile_sharing(p, t, map_sharing(go, t.children)),
            ...walk(rest),
          ]
        | [p, ...rest] => [p, ...walk(rest)]
        };
      let out = walk(seg);
      Segment.ptr_eq(out, seg) ? seg : out;
    };
    go(seg);
  };

/* a cell's text from [content], cut from [master] (at [root]) */
let cut_indent =
    (~root: Sort.t, master: Segment.t, content: Segment.t)
    : (Segment.t, ScratchCell.indent) => {
  let base =
    switch (content) {
    | [p, ..._] =>
      Option.value(line_indent(~root, Piece.id(p), master), ~default=0)
    | [] => 0
    };
  let cut = dedent(base, content);
  (
    cut,
    {
      base,
      cut,
      orig: content,
    },
  );
};

/* what the cut took, by id: each tile as cut with its original piece,
   and the very spaces cut after each line break ([] where none) */
type cut_pairs = {
  tiles: Id.Map.t((Tile.t, Piece.t)),
  after: Id.Map.t(Segment.t),
};

let pairs_of = (ind: ScratchCell.indent): cut_pairs => {
  let tiles = ref(Id.Map.empty);
  let after = ref(Id.Map.empty);
  /* [o] and [c] match piece for piece but for the spaces cut after line
     breaks, which lead the rest of [o] only */
  let rec pair = (o: Segment.t, c: Segment.t) =>
    switch (o, c) {
    | ([lb, ...o_rest], [_, ...c_rest]) when Piece.is_linebreak(lb) =>
      let (o_sp, o_rest) = Segment.split_space_run(o_rest);
      let (c_sp, c_rest) = Segment.split_space_run(c_rest);
      let removed =
        ListUtil.take(List.length(o_sp) - List.length(c_sp), o_sp);
      after := Id.Map.add(Piece.id(lb), removed, after^);
      pair(o_rest, c_rest);
    | ([Tile(ot) as op, ...o_rest], [Tile(ct), ...c_rest]) =>
      /* tiles the cut shared too: a zip re-assembles the caret's
         ancestors, and those should come back as the program's */
      tiles := Id.Map.add(ct.id, (ct, op), tiles^);
      if (List.length(ot.children) == List.length(ct.children)) {
        List.iter2(pair, ot.children, ct.children);
      };
      pair(o_rest, c_rest);
    | ([_, ...o_rest], [_, ...c_rest]) => pair(o_rest, c_rest)
    | _ => ()
    };
  pair(ind.orig, ind.cut);
  {
    tiles: tiles^,
    after: after^,
  };
};

/* pairs per cut, held while the cut is (cells re-cut on structural
   change, so a short list covers the open ones) */
let pairs_memo: ref(list((Segment.t, cut_pairs))) = ref([]);
let pairs = (ind: ScratchCell.indent): cut_pairs =>
  switch (List.find_opt(((c, _)) => c === ind.cut, pairs_memo^)) {
  | Some((_, p)) => p
  | None =>
    let p = pairs_of(ind);
    pairs_memo := [(ind.cut, p), ...ListUtil.take(31, pairs_memo^)];
    p;
  };

/* [base] spaces after a line break new to its cell, made once */
let fresh_memo: Hashtbl.t((Id.t, int), Segment.t) = Hashtbl.create(64);
let fresh_indent = (lb: Id.t, base: int): Segment.t =>
  switch (Hashtbl.find_opt(fresh_memo, (lb, base))) {
  | Some(sp) => sp
  | None =>
    if (Hashtbl.length(fresh_memo) > 4096) {
      Hashtbl.reset(fresh_memo);
    };
    let sp =
      List.init(base, i =>
        Piece.Secondary(
          Secondary.mk_space(
            Id.derive(~salt="cell-indent-" ++ string_of_int(i), lb),
          ),
        )
      );
    Hashtbl.replace(fresh_memo, (lb, base), sp);
    sp;
  };

/* the cell's text [cur] in program coordinates */
let restore = (ind: ScratchCell.indent, cur: Segment.t): Segment.t =>
  if (ind.base == 0) {
    cur;
  } else if (Segment.ptr_eq(cur, ind.cut)) {
    ind.orig;
  } else {
    let {tiles, after} = pairs(ind);
    /* t is c, maybe re-assembled by a zip: same shape, same children */
    let as_cut = (t: Tile.t, c: Tile.t) =>
      t === c
      || t.form == c.form
      && t.sort == c.sort
      && t.shards == c.shards
      && List.length(t.children) == List.length(c.children)
      && List.for_all2(Segment.ptr_eq, t.children, c.children);
    let rec go = (seg: Segment.t): Segment.t => {
      let rec walk = (ps: Segment.t): Segment.t =>
        switch (ps) {
        | [] => []
        | [lb, ...rest] when Piece.is_linebreak(lb) =>
          let back =
            switch (Id.Map.find_opt(Piece.id(lb), after)) {
            | Some(sp) => sp
            | None =>
              switch (rest) {
              | [next, ..._] when Piece.is_linebreak(next) => []
              | _ => fresh_indent(Piece.id(lb), ind.base)
              }
            };
          [lb, ...back @ walk(rest)];
        | [Tile(t) as p, ...rest] =>
          let p =
            switch (Id.Map.find_opt(t.id, tiles)) {
            | Some((c, orig)) when as_cut(t, c) => orig
            | Some((c, Tile(o)))
                when List.length(c.children) == List.length(t.children) =>
              /* children the cell left alone are the program's own */
              tile_sharing(
                p,
                t,
                List.mapi(
                  (i, kid) =>
                    Segment.ptr_eq(kid, List.nth(c.children, i))
                      ? List.nth(o.children, i) : go(kid),
                  t.children,
                ),
              )
            | _ => tile_sharing(p, t, map_sharing(go, t.children))
            };
          [p, ...walk(rest)];
        | [p, ...rest] => [p, ...walk(rest)]
        };
      let out = walk(seg);
      Segment.ptr_eq(out, seg) ? seg : out;
    };
    let out = go(cur);
    Segment.ptr_eq(out, ind.orig) ? ind.orig : out;
  };

/* the ctx inside the def, params included (funlets need them): the
   first info among the def's pieces, else the item's own */
let captured_ctx =
    (~info_map: Language.Statics.Map.t, fid: Id.t, def_seg: Segment.t)
    : option(Language.Ctx.t) => {
  let info_of = id => Id.Map.find_opt(id, info_map);
  let rec seg_info = (seg: Segment.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (info_of(Piece.id(p)), p) {
        | (Some(_) as info, _) => info
        | (None, Tile(t)) => List.find_map(seg_info, t.children)
        | (None, _) => None
        },
      seg,
    );
  switch (seg_info(def_seg), info_of(fid)) {
  | (Some(info), _)
  | (None, Some(info)) => Some(Language.Info.ctx_of(info))
  | (None, None) => None
  };
};

/* headerless cells carry a grout header, never rendered or spliced: a
   bare [] zipper would crash Skel */
let empty_header_cell = (): CellEditor.Model.t =>
  cell_of_seg(
    ~root=Pat,
    [
      Piece.Grout({
        id: Id.mk(),
        shape: Convex,
      }),
    ],
  );

/* only delimiter-complete items open as cells: an unfinished `let x =`
   has no definition slot to show */
let unfinished = (path: list(step)): bool =>
  !projected(path)
  && (
    switch (ListUtil.last(path)) {
    | {lv, i, _} =>
      switch (lv.pieces[i]) {
      | Tile(t) => !Tile.is_complete(t)
      | _ => false
      }
    }
  );

/* a module definition's braces, and their index in the definition */
let brace_at = (def_seg: Segment.t): option((int, Tile.t)) =>
  List.find_mapi(
    (j, p: Piece.t) =>
      switch (p) {
      | Tile({children: [_], _} as t) when Tile.label(t) == ["{", "}"] =>
        Some((j, t))
      | _ => None
      },
    def_seg,
  );

/* a module definition's members: the child of its braces */
let brace_child = (def_seg: Segment.t): option(Segment.t) =>
  Option.map(((_, t: Tile.t)) => List.hd(t.children), brace_at(def_seg));

let with_brace_child = (def_seg: Segment.t, kid: Segment.t): Segment.t =>
  map_sharing(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t)
          when Tile.label(t) == ["{", "}"] && List.length(t.children) == 1 =>
        Segment.ptr_eq(kid, List.hd(t.children))
          ? p
          : Piece.Tile({
              ...t,
              children: [kid],
            })
      | p => p
      },
    def_seg,
  );

/* a [kind] cell for item [fid] showing [body], its slice of [master] (at
   [root]), at [cell_root]: under [header] (a pattern and its root) if it
   has one, else headerless. Its ctx is the one inside [body] */
let mk_cell =
    (
      ~info_map: Language.Statics.Map.t,
      ~root: Sort.t,
      ~master: Segment.t,
      ~cell_root=Sort.Exp,
      ~header: option((Sort.t, Segment.t))=?,
      ~kind: ScratchCell.kind,
      fid: Id.t,
      body: Segment.t,
    )
    : ScratchCell.t => {
  let (e_header, e_header_indent) =
    switch (header) {
    | Some((cell_root, pat)) =>
      let (cut, indent) = cut_indent(~root, master, core_ws(pat));
      (cell_of_seg(~root=cell_root, cut), indent);
    | None => (empty_header_cell(), ScratchCell.no_indent)
    };
  let (cut, e_body_indent) = cut_indent(~root, master, core_ws(body));
  {
    e_id: fid,
    e_kind: kind,
    e_header,
    e_body: cell_of_seg(~root=cell_root, cut),
    e_header_indent,
    e_body_indent,
    e_ctx:
      Option.value(
        captured_ctx(~info_map, fid, body),
        ~default=
          Language.Builtins.ctx_init(Some(Language.Operators.default_mode)),
      ),
  };
};

let pat_of = (t: Tile.t): Segment.t =>
  Option.value(ListUtil.hd_opt(t.children), ~default=[]);

/* only a definition's own tile has a pattern over a definition: a let,
   type or module, or a theorem, whose ⇒ row's cell holds its statement.
   [path] is find_path's */
let mk_def_entry =
    (
      ~root: Sort.t,
      ~info_map: Language.Statics.Map.t,
      ~master: Segment.t,
      path: list(step),
    )
    : option(ScratchCell.t) =>
  switch (projected(path) ? None : Some(home(~seg=master, path))) {
  | Some((_, level, i)) =>
    switch (List.nth(level, i)) {
    | Tile(t) when is_def_head(t) || Tile.token(t, 0) == "theorem" =>
      Option.map(
        def_seg => {
          let kw = Tile.token(t, 0);
          /* a `type … = …` alias takes a Typ body and a TPat header */
          let is_type = kw == "type";
          mk_cell(
            ~info_map,
            ~root,
            ~master,
            ~cell_root=is_type ? Typ : Exp,
            ~header=(is_type ? TPat : Pat, pat_of(t)),
            ~kind=Def({binds_module: kw == "module"}),
            t.id,
            def_seg,
          );
        },
        def_at((level, i, t)),
      )
    | _ => None
    }
  | None => None
  };

let mk_entry =
    (
      ~root: Sort.t,
      ~info_map: Language.Statics.Map.t,
      ~sym: option(string)=?,
      fid: Id.t,
      master: Segment.t,
    )
    : option(ScratchCell.t) =>
  switch (find_path(~root, fid, master)) {
  | None => None
  | Some(path) when unfinished(path) => None
  | Some(path) =>
    switch (locate_along(fid, path)) {
    | Some((at, k)) =>
      let lv = ListUtil.last(at).lv;
      Some(
        mk_cell(
          ~info_map,
          ~root,
          ~master,
          ~kind=Headless(Option.value(sym, ~default=span_sym(lv, k))),
          fid,
          core(lv, k),
        ),
      );
    | None => mk_def_entry(~root, ~info_map, ~master, path)
    }
  };

let mk_members_entry =
    (
      ~root: Sort.t,
      ~info_map: Language.Statics.Map.t,
      fid: Id.t,
      master: Segment.t,
    )
    : option(ScratchCell.t) =>
  switch (tile_at(~root, fid, master)) {
  | Some((up, level, i, t)) when !unfinished(up) =>
    Option.map(
      mk_cell(
        ~info_map,
        ~root,
        ~master,
        ~cell_root=Mod,
        ~header=(Pat, pat_of(t)),
        ~kind=Members,
        fid,
      ),
      Option.bind(def_at((level, i, t)), brace_child),
    )
  | _ => None
  };

let mk_run_entry =
    (
      ~root: Sort.t,
      ~info_map: Language.Statics.Map.t,
      fid: Id.t,
      master: Segment.t,
    )
    : option(ScratchCell.t) =>
  switch (find_path(~root, fid, master)) {
  | None => None
  | Some(path) when unfinished(path) => None
  | Some(path) =>
    switch (run_along(path)) {
    | None => mk_entry(~root, ~info_map, fid, master)
    | Some((at, a, b, (members, _))) =>
      Some(
        mk_cell(
          ~info_map,
          ~root,
          ~master,
          ~kind=Run(members),
          fid,
          run_text(ListUtil.last(at).lv, a, b),
        ),
      )
    }
  };

/* why an item opens no cell: unfinished, under a projector, whose
   syntax no cell cuts, or a `use`, which never opens */
type refusal =
  | Unfinished
  | Projected
  | Directive;

/* of the items [want] picks, those the entry makers above refuse for one
   of these, from one walk over [seg] rather than a search per item. Under
   a projector only a headless span cut outside it opens */
let refusals =
    (~root: Sort.t, ~want: Id.t => bool, seg: Segment.t): Id.Map.t(refusal) => {
  let out = ref(Id.Map.empty);
  /* the steps from the piece at [path]'s end down to [id], inside it */
  let below = (path: list(step), id: Id.t): list(step) => {
    let (above, st) = ListUtil.split_last(path);
    above
    @ descend(
        ~from=st.from,
        st.lv,
        st.i,
        Option.value(way(id, st.lv.pieces[st.i]), ~default=[]),
      );
  };
  /* [under]: the steps down to the projector the walk is in */
  let rec walk = (~under: option(list(step)), level: Segment.t) =>
    List.iter(
      (p: Piece.t) => {
        let id = Piece.id(p);
        switch (under) {
        | Some(path)
            when
              want(id) && Option.is_none(locate_along(id, below(path, id))) =>
          out := Id.Map.add(id, Projected, out^)
        | _ => ()
        };
        switch (p) {
        | Tile(t) =>
          if (Option.is_none(under) && want(id) && !Tile.is_complete(t)) {
            out := Id.Map.add(id, Unfinished, out^);
          };
          if (want(id) && is_use(t)) {
            out := Id.Map.add(id, Directive, out^);
          };
          List.iter(walk(~under), t.children);
        | Projector(pr) =>
          let path =
            switch (under) {
            | Some(path) => Some(below(path, id))
            | None => find_path(~root, id, seg)
            };
          walk(~under=path, [pr.syntax]);
        | Grout(_)
        | Secondary(_) => ()
        };
      },
      level,
    );
  walk(~under=None, seg);
  out^;
};

/* a cell's header and body text in program coordinates */
let body_text = (e: ScratchCell.t): Segment.t =>
  restore(e.e_body_indent, zip_of_cell(e.e_body));
let header_text = (e: ScratchCell.t): Segment.t =>
  restore(e.e_header_indent, zip_of_cell(e.e_header));

/* splice a cell's text back into [seg], keeping [seg]'s edge whitespace */

let splice_entry =
    (~root: Sort.t, e: ScratchCell.t, seg: Segment.t): Segment.t =>
  switch (e.e_kind) {
  | Members =>
    switch (find_def(~root, e.e_id, seg)) {
    | Some(def_seg) =>
      let members =
        rewrap_ws(
          (id, seg) => Option.bind(find_def(~root, id, seg), brace_child),
          e.e_id,
          seg,
          body_text(e),
        );
      splice_def(~root, e.e_id, with_brace_child(def_seg, members), seg)
      |> splice_pat(
           ~root,
           e.e_id,
           rewrap_ws(find_pat(~root), e.e_id, seg, header_text(e)),
         );
    | None => seg
    }
  | Run(_) => splice_run(~root, e.e_id, body_text(e), seg)
  | Headless(_) => splice_headless(~root, e.e_id, body_text(e), seg)
  | Def(_) =>
    splice_def(
      ~root,
      e.e_id,
      rewrap_ws(find_def(~root), e.e_id, seg, body_text(e)),
      seg,
    )
    |> splice_pat(
         ~root,
         e.e_id,
         rewrap_ws(find_pat(~root), e.e_id, seg, header_text(e)),
       )
  };

let cell_content =
    (~root: Sort.t, e: ScratchCell.t, seg: Segment.t): option(Segment.t) =>
  switch (e.e_kind) {
  | Members => Option.bind(find_def(~root, e.e_id, seg), brace_child)
  | Run(_) =>
    test_run_deep(~root, e.e_id, seg) |> Option.map(((run, _, _)) => run)
  | Headless(_) =>
    headless_content_deep(~root, e.e_id, seg) |> Option.map(fst)
  | Def(_) => find_def(~root, e.e_id, seg)
  };
