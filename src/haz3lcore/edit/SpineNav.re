/* Where a piece is in a program: the ItemSpans levels down to it, and
   splices that re-make only the levels on the way. Pure: no parse, no
   statics. The cells (ScratchFocus) and the editing kernel (EntryEdit)
   share it */
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

/* ItemSpans.find's way to the span anchored at [fid], as steps; with
   [at_core], down to the span's first piece of content rather than to
   [fid] (an anchor that is no piece) */
let found_steps =
    (~at_core=false, fid: Id.t, found: list((S.level, int)))
    : option(list(step)) => {
  let rec go = (~from, path) =>
    switch (path) {
    | [] => None
    | [(lv, k)] when at_core =>
      Some([step(~from, lv, fst(lv.spans[k].core))])
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

/* the steps down to the span anchored at [anchor] when that is no piece
   (a let-chain member's, derived from its first item's) */
let anchor_path =
    (~root: Sort.t, anchor: Id.t, seg: Segment.t): option(list(step)) =>
  switch (S.find(~root, anchor, seg)) {
  | Some(found) =>
    let (lv, k) = ListUtil.last(found);
    lv.spans[k].anchor == anchor
      ? found_steps(~at_core=true, anchor, found) : None;
  | None => None
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
            /* a projector's syntax is its one piece */
            | Syntax =>
              switch (above.lv.pieces[above.i], kid) {
              | (Projector(pr), [syntax]) =>
                ListUtil.put_nth(
                  above.i,
                  Piece.Projector({
                    ...pr,
                    syntax,
                  }),
                  parent,
                )
              | _ => parent
              }
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

let pat_of = (t: Tile.t): Segment.t =>
  Option.value(ListUtil.hd_opt(t.children), ~default=[]);
