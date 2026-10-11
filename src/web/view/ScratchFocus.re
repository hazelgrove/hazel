/* A cell's body is the definition's RHS child segment (between `=` and
   `in`/`;`), complete and grouted; slicing the whole `let…in` tile
   leaves a prefix tile without its operand and crashes Skel. */
open Haz3lcore;
open Util;

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

exception Indent(int);

/* the indentation of the line the piece [id] is on */
let line_indent = (id: Id.t, seg: Segment.t): option(int) => {
  let ind = ref(0);
  let lead = ref(true);
  let rec go = (ps: Segment.t) =>
    List.iter(
      (p: Piece.t) => {
        if (Piece.id(p) == id) {
          raise(Indent(ind^));
        };
        switch (p) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          ind := 0;
          lead := true;
        | Secondary(w) when Secondary.is_space(w) =>
          if (lead^) {
            ind := ind^ + 1;
          }
        | Tile(t) =>
          lead := false;
          List.iter(
            ch => {
              go(ch);
              lead := false;
            },
            t.children,
          );
        | _ => lead := false
        };
      },
      ps,
    );
  switch (go(seg)) {
  | () => None
  | exception (Indent(n)) => Some(n)
  };
};

let seg_contains_id = (id: Id.t, seg: Segment.t): bool =>
  Option.is_some(Segment.find_ctx(seg, id));

/* the tile [fid], the segment it sits in and its index there */
let tile_ctx = (fid: Id.t, seg: Segment.t): option((Segment.t, int, Tile.t)) =>
  switch (Segment.find_ctx(seg, fid)) {
  | Some((level, i, Tile(t))) => Some((level, i, t))
  | _ => None
  };

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

/* top-level item spans by piece structure, without parsing: an `…in`
   tile (+ trailing ws) is a def item, the run through a top-level `;`
   (+ ws) a statement, and what remains the trailing expression. A 2-shard
   member's body is its sibling run, so an `…in` there stays in the member */
type item_kind =
  | IDef /* let / type / module: header+body cells */
  | IStmt /* a `…;` statement: headerless cell */
  | ITail; /* the trailing expression: headerless cell */

type item_span = {
  sp_id: option(Id.t), /* the def head or `;` tile; None for the tail */
  sp_start: int,
  sp_stop: int, /* exclusive */
  sp_kind: item_kind,
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

/* [stop] moved back over edge whitespace and a `;`, not past [start]:
   where a `…;` run's content ends */
let before_semi = (arr: array(Piece.t), start: int, stop: int): int => {
  let rec back = i => i > start && is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
  let stop = back(stop);
  stop > start && Piece.is_semi(arr[stop - 1]) ? stop - 1 : stop;
};

let item_spans = (~divided_only_tail=false, seg: Segment.t): list(item_span) => {
  let arr = Array.of_list(seg);
  let len = Array.length(arr);
  let rec ws_end = i => i < len && is_edge_ws(arr[i]) ? ws_end(i + 1) : i;
  let is_in_tile = (p: Piece.t) =>
    switch (p) {
    | Tile(t) => Tile.ends_with_in(t)
    | _ => false
    };
  /* module bodies have 2-shard member defs ended by `;`: a `;`-run
     starting with a def head is a def item, not a statement */
  let run_def_head = (start: int, stop: int): option(Id.t) =>
    switch (first_tile(arr, start, stop)) {
    | Some(t) when is_def_head(t) => Some(t.id)
    | _ => None
    };
  let span = (sp_start, sp_stop, sp_kind, sp_id) => {
    sp_id,
    sp_start,
    sp_stop,
    sp_kind,
  };
  let rec walk = (i, start, acc) =>
    if (i >= len) {
      /* the remainder (if it has content) is the trailing expr —
         or a trailing 2-shard member def */
      let has_content = {
        let rec go = j => j < len && (!is_edge_ws(arr[j]) || go(j + 1));
        start < len && go(start);
      };
      let head = run_def_head(start, len);
      /* a nested segment without boundaries is an expression, not a
         block, so no tail item (deep containment would swallow
         arbitrary ids); the program's top level keeps its tail */
      let tail_ok = head != None || !divided_only_tail || acc != [];
      List.rev(
        has_content && tail_ok
          ? [span(start, len, head == None ? ITail : IDef, head), ...acc]
          : acc,
      );
    } else if (is_in_tile(arr[i]) && run_def_head(start, i) == None) {
      let stop = ws_end(i + 1);
      walk(
        stop,
        stop,
        [span(i, stop, IDef, Some(Piece.id(arr[i]))), ...acc],
      );
    } else if (Piece.is_semi(arr[i])) {
      let stop = ws_end(i + 1);
      let sp =
        switch (run_def_head(start, i)) {
        | Some(_) as head => span(start, stop, IDef, head)
        | None => span(start, stop, IStmt, Some(Piece.id(arr[i])))
        };
      walk(stop, stop, [sp, ...acc]);
    } else {
      walk(i + 1, start, acc);
    };
  /* a function body shares its segment with its `fun x ->` head: its
     items start after it */
  let is_head = (p: Piece.t) =>
    switch (p) {
    | Tile(t) =>
      switch (Tile.label(t)) {
      | ["fun", "->"]
      | ["typfun", "->"] => true
      | _ => false
      }
    | _ => false
    };
  let rec after_heads = i => {
    let j = ws_end(i);
    j < len && is_head(arr[j]) ? after_heads(j + 1) : i;
  };
  let first =
    switch (after_heads(0)) {
    | 0 => 0
    | i => ws_end(i)
    };
  walk(first, first, []);
};

/* a member body opening with `…in` lies flat in the member's run: it
   is a let-in block, from that first `…in` up to the member's `;` */
let flat_body = (arr: array(Piece.t), sp: item_span): option((int, int)) => {
  /* the first `…in` past the head; a span opening with one is a let-in */
  let rec first_in = (~head, i) =>
    i >= sp.sp_stop
      ? None
      : (
        switch (arr[i]) {
        | Tile(t) when Tile.ends_with_in(t) => head ? None : Some(i)
        | Tile(_) => first_in(~head=false, i + 1)
        | _ => first_in(~head, i + 1)
        }
      );
  sp.sp_kind == IDef
    ? Option.map(
        b0 => (b0, before_semi(arr, sp.sp_start, sp.sp_stop)),
        first_in(~head=true, sp.sp_start),
      )
    : None;
};

/* [fid] is one of the pieces [lo, hi) themselves */
let piece_at = (fid: Id.t, lo: int, hi: int, arr: array(Piece.t)): bool => {
  let rec go = i => i < hi && (Piece.id(arr[i]) == fid || go(i + 1));
  go(lo);
};

/* the flat body of one of [spans] with [fid] among its own pieces */
let flat_body_of =
    (fid: Id.t, arr: array(Piece.t), spans: list(item_span))
    : option((int, int)) =>
  List.find_map(
    sp =>
      switch (flat_body(arr, sp)) {
      | Some((b0, b1)) when piece_at(fid, b0, b1, arr) => Some((b0, b1))
      | _ => None
      },
    spans,
  );

/* the span holding [fid]: by its id first, then containment (outline
   ids can be tiles inside an item: module binders, the tail's root) */
let find_item_span =
    (~divided_only_tail=false, fid: Id.t, seg: Segment.t): option(item_span) => {
  let spans = item_spans(~divided_only_tail, seg);
  switch (List.find_opt(sp => sp.sp_id == Some(fid), spans)) {
  | Some(_) as r => r
  | None =>
    List.find_opt(
      sp =>
        seg_contains_id(
          fid,
          ListUtil.slice(sp.sp_start, sp.sp_stop - sp.sp_start, seg),
        ),
      spans,
    )
  };
};

/* the sub-span a headerless item's cell holds (a statement's run before
   its `;`, or the whole tail) and its header symbol; None for defs */
let rec headless_span =
        (~divided_only_tail=false, fid: Id.t, seg: Segment.t)
        : option((int, int, string)) =>
  switch (find_item_span(~divided_only_tail, fid, seg)) {
  | Some({sp_kind: IStmt, sp_start, sp_stop, _}) =>
    Some((
      sp_start,
      before_semi(Array.of_list(seg), sp_start, sp_stop),
      {js|;|js},
    ))
  | Some({sp_kind: ITail, sp_start, sp_stop, _}) =>
    Some((sp_start, sp_stop, {js|⇒|js}))
  | Some(sp) =>
    let arr = Array.of_list(seg);
    switch (flat_body_of(fid, arr, [sp])) {
    | Some((b0, b1)) =>
      headless_span(
        ~divided_only_tail=true,
        fid,
        ListUtil.slice(b0, b1 - b0, seg),
      )
      |> Option.map(((a, b, sym)) => (b0 + a, b0 + b, sym))
    | None => None
    };
  | None => None
  };

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

/* [seg] with the segment holding tile [fid] replaced by [f] of it, the
   tile's index and the tile; the program's own pieces wherever nothing
   changed */
let splice_at =
    (fid: Id.t, f: (Segment.t, int, Tile.t) => Segment.t, seg: Segment.t)
    : Segment.t =>
  switch (tile_ctx(fid, seg)) {
  | None => seg
  | Some((level, i, t)) =>
    let level' = f(level, i, t);
    let rec go = (s: Segment.t): Segment.t =>
      s === level
        ? level'
        : map_sharing(
            (p: Piece.t) =>
              switch (p) {
              | Tile(t) => tile_sharing(p, t, map_sharing(go, t.children))
              | _ => p
              },
            s,
          );
    Segment.ptr_eq(level', level) ? seg : go(seg);
  };

/* an item's span (start, stop, …) at any block depth: nested blocks share
   the boundary structure, so [at] applies at each level, top down; it
   gets the tile the level is a child of (None at the top) */
let rec find_deep =
        (
          ~parent: option(Tile.t)=?,
          at: (option(Tile.t), Segment.t) => option((int, int, 'x)),
          seg: Segment.t,
        )
        : option((Segment.t, 'x)) =>
  switch (at(parent, seg)) {
  | Some((start, stop, x)) =>
    Some((ListUtil.slice(start, stop - start, seg), x))
  | None =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) => List.find_map(find_deep(~parent=t, at), t.children)
        | _ => None
        },
      seg,
    )
  };

/* [seg] with [repl] in place of [at]'s span, inside its edge whitespace */
let rec splice_deep =
        (
          ~parent: option(Tile.t)=?,
          at: (option(Tile.t), Segment.t) => option((int, int, 'x)),
          repl: Segment.t,
          seg: Segment.t,
        )
        : Segment.t =>
  switch (at(parent, seg)) {
  | Some((start, stop, _)) =>
    let (pre, _, suf) = trim_ws(ListUtil.slice(start, stop - start, seg));
    let out =
      ListUtil.truncate(start, seg)
      @ pre
      @ repl
      @ suf
      @ ListUtil.remove_first_n(stop, seg);
    Segment.ptr_eq(out, seg) ? seg : out;
  | None =>
    map_sharing(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          tile_sharing(
            p,
            t,
            map_sharing(splice_deep(~parent=t, at, repl), t.children),
          )
        | _ => p
        },
      seg,
    )
  };

/* headerless content: the top level's tail is unconditional (the ⇒ row);
   nested levels have one only after an item boundary. An id inside a def
   span yields None there and the walk descends */
let headless_at = (fid: Id.t, parent: option(Tile.t), seg: Segment.t) =>
  headless_span(~divided_only_tail=Option.is_some(parent), fid, seg);

let headless_content_deep =
    (fid: Id.t, seg: Segment.t): option((Segment.t, string)) =>
  find_deep(headless_at(fid), seg);

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

/* a cell's text from [content], cut from [master] */
let cut_indent =
    (master: Segment.t, content: Segment.t): (Segment.t, ScratchCell.indent) => {
  let base =
    switch (content) {
    | [p, ..._] =>
      Option.value(line_indent(Piece.id(p), master), ~default=0)
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

/* the `test` before the statement `;` [semi]: a run member's row is its
   `;`, which stays outside the run's cell */
let test_before_semi = (semi: Id.t, seg: Segment.t): option(Id.t) =>
  Option.bind(tile_ctx(semi, seg), ((level, i, _)) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.token(t, 0) == "test" => Some(t.id)
        | _ => None
        },
      List.rev(ListUtil.truncate(i, level)),
    )
  );

/* the span's leading `test` tile: module members are repped by it,
   top-level statements by their `;`, so run members carry both ids
   (harmless: consumers test membership against outline ids) */
let span_test_tile_id = (arr: array(Piece.t), sp: item_span): option(Id.t) =>
  switch (first_tile(arr, sp.sp_start, sp.sp_stop)) {
  | Some(t) when Tile.token(t, 0) == "test" => Some(t.id)
  | _ => None
  };

/* contiguous test runs: the outline's "tests" container opens one cell
   spanning the whole run. The maximal run of adjacent test statements
   holding [fid], as (start, stop, (member ids, test count)); the last `;`
   stays outside, like a statement's. In a module body a final `;`-less test
   parses as the tail but joins the run; an expression block's tail is
   its value and never does. */
let test_run =
    (~module_body=false, fid: Id.t, seg: Segment.t)
    : option((int, int, (list(Id.t), int))) => {
  let arr = Array.of_list(seg);
  let spans = Array.of_list(item_spans(seg));
  let n = Array.length(spans);
  let in_run = (sp: item_span): bool =>
    switch (sp.sp_kind) {
    | IStmt => span_test_tile_id(arr, sp) != None
    | ITail => module_body && span_test_tile_id(arr, sp) != None
    | IDef => false
    };
  let rec idx = j =>
    j >= n
      ? None
      : spans[j].sp_id == Some(fid)
        || seg_contains_id(
             fid,
             ListUtil.slice(
               spans[j].sp_start,
               spans[j].sp_stop - spans[j].sp_start,
               seg,
             ),
           )
          ? Some(j) : idx(j + 1);
  switch (idx(0)) {
  | Some(j) when in_run(spans[j]) =>
    let rec lo = j => j > 0 && in_run(spans[j - 1]) ? lo(j - 1) : j;
    let rec hi = j => j + 1 < n && in_run(spans[j + 1]) ? hi(j + 1) : j;
    let (a, b) = (lo(j), hi(j));
    let members =
      List.concat_map(
        k =>
          List.filter_map(
            x => x,
            [span_test_tile_id(arr, spans[k]), spans[k].sp_id],
          ),
        List.init(b - a + 1, k => a + k),
      );
    let start = spans[a].sp_start;
    Some((
      start,
      before_semi(arr, start, spans[b].sp_stop),
      (members, b - a + 1),
    ));
  | _ => None
  };
};

/* test runs at any block depth; a level is a module body when its tile
   has a member child */
let run_at = (fid: Id.t, parent: option(Tile.t), seg: Segment.t) =>
  test_run(
    ~module_body=
      switch (parent) {
      | Some(t) => List.mem(Sort.Mod, Tile.mold(t).in_)
      | None => false
      },
    fid,
    seg,
  );

let test_run_deep = (fid: Id.t, seg: Segment.t) =>
  find_deep(run_at(fid), seg)
  |> Option.map(((run, (members, _))) => (run, members));

/* how many tests the run holding [fid] has (members carry two ids each) */
let test_run_size_deep = (fid: Id.t, seg: Segment.t): option(int) =>
  find_deep(run_at(fid), seg) |> Option.map(((_, (_, n))) => n);

/* the definition RHS of the item [t] at [i] in [level]: a 3-shard
   `let … = … in`'s last child, or for a 2-shard member `let … =`, the
   sibling run after it up to the next `;` (or segment end) */
let def_at = ((level, i, t): (Segment.t, int, Tile.t)): option(Segment.t) =>
  Tile.ends_with_in(t)
    ? ListUtil.last_opt(t.children)
    : Some(fst(split_at_semi(ListUtil.remove_first_n(i + 1, level))));

let find_def = (fid: Id.t, seg: Segment.t): option(Segment.t) =>
  Option.bind(tile_ctx(fid, seg), def_at);

let splice_def = (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  splice_at(
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
let find_pat = (fid: Id.t, seg: Segment.t): option(Segment.t) =>
  Option.bind(tile_ctx(fid, seg), ((_, _, t)) =>
    ListUtil.hd_opt(t.children)
  );

let splice_pat = (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  splice_at(
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
let item_complete = (fid: Id.t, seg: Segment.t): bool =>
  !
    List.exists(
      (t: Tile.t) => t.id == fid,
      Segment.incomplete_tiles_deep(seg),
    );

/* a module definition's members: the child of its braces */
let brace_child = (def_seg: Segment.t): option(Segment.t) =>
  List.find_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when Tile.label(t) == ["{", "}"] =>
        switch (t.children) {
        | [kid] => Some(kid)
        | _ => None
        }
      | _ => None
      },
    def_seg,
  );

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

/* a [kind] cell for item [fid] showing [body], its slice of [master], at
   [root]: under [header] (a pattern and its root) if it has one, else
   headerless. Its ctx is the one inside [body] */
let mk_cell =
    (
      ~info_map: Language.Statics.Map.t,
      ~master: Segment.t,
      ~root=Sort.Exp,
      ~header: option((Sort.t, Segment.t))=?,
      ~kind: ScratchCell.kind,
      fid: Id.t,
      body: Segment.t,
    )
    : ScratchCell.t => {
  let (e_header, e_header_indent) =
    switch (header) {
    | Some((root, pat)) =>
      let (cut, indent) = cut_indent(master, core_ws(pat));
      (cell_of_seg(~root, cut), indent);
    | None => (empty_header_cell(), ScratchCell.no_indent)
    };
  let (cut, e_body_indent) = cut_indent(master, core_ws(body));
  {
    e_id: fid,
    e_kind: kind,
    e_header,
    e_body: cell_of_seg(~root, cut),
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

let mk_def_entry =
    (~info_map: Language.Statics.Map.t, fid: Id.t, master_seg: Segment.t)
    : option(ScratchCell.t) =>
  Option.bind(tile_ctx(fid, master_seg), ((_, _, t) as at) =>
    Option.map(
      def_seg => {
        let kw = Tile.token(t, 0);
        /* a `type … = …` alias takes a Typ body and a TPat header */
        let is_type = kw == "type";
        mk_cell(
          ~info_map,
          ~master=master_seg,
          ~root=is_type ? Typ : Exp,
          ~header=(is_type ? TPat : Pat, pat_of(t)),
          ~kind=Def({binds_module: kw == "module"}),
          fid,
          def_seg,
        );
      },
      def_at(at),
    )
  );

let mk_entry =
    (
      ~info_map: Language.Statics.Map.t,
      ~sym: option(string)=?,
      fid: Id.t,
      master_seg: Segment.t,
    )
    : option(ScratchCell.t) =>
  !item_complete(fid, master_seg)
    ? None
    : (
      switch (headless_content_deep(fid, master_seg)) {
      | Some((raw, span_sym)) =>
        Some(
          mk_cell(
            ~info_map,
            ~master=master_seg,
            ~kind=Headless(Option.value(sym, ~default=span_sym)),
            fid,
            raw,
          ),
        )
      | None => mk_def_entry(~info_map, fid, master_seg)
      }
    );

let mk_members_entry =
    (~info_map: Language.Statics.Map.t, fid: Id.t, master_seg: Segment.t)
    : option(ScratchCell.t) =>
  !item_complete(fid, master_seg)
    ? None
    : Option.bind(tile_ctx(fid, master_seg), ((_, _, t) as at) =>
        Option.map(
          mk_cell(
            ~info_map,
            ~master=master_seg,
            ~root=Mod,
            ~header=(Pat, pat_of(t)),
            ~kind=Members,
            fid,
          ),
          Option.bind(def_at(at), brace_child),
        )
      );

let mk_run_entry =
    (~info_map: Language.Statics.Map.t, fid: Id.t, master_seg: Segment.t)
    : option(ScratchCell.t) =>
  !item_complete(fid, master_seg)
    ? None
    : (
      switch (test_run_deep(fid, master_seg)) {
      | None => mk_entry(~info_map, fid, master_seg)
      | Some((run_slice, members)) =>
        Some(
          mk_cell(
            ~info_map,
            ~master=master_seg,
            ~kind=Run(members),
            fid,
            run_slice,
          ),
        )
      }
    );

/* a cell's header and body text in program coordinates */
let body_text = (e: ScratchCell.t): Segment.t =>
  restore(e.e_body_indent, zip_of_cell(e.e_body));
let header_text = (e: ScratchCell.t): Segment.t =>
  restore(e.e_header_indent, zip_of_cell(e.e_header));

/* splice a cell's text back into [seg], keeping [seg]'s edge whitespace */

let splice_entry = (e: ScratchCell.t, seg: Segment.t): Segment.t =>
  switch (e.e_kind) {
  | Members =>
    switch (find_def(e.e_id, seg)) {
    | Some(def_seg) =>
      let members =
        rewrap_ws(
          (id, seg) => Option.bind(find_def(id, seg), brace_child),
          e.e_id,
          seg,
          body_text(e),
        );
      splice_def(e.e_id, with_brace_child(def_seg, members), seg)
      |> splice_pat(
           e.e_id,
           rewrap_ws(find_pat, e.e_id, seg, header_text(e)),
         );
    | None => seg
    }
  | Run(_) => splice_deep(run_at(e.e_id), body_text(e), seg)
  | Headless(_) => splice_deep(headless_at(e.e_id), body_text(e), seg)
  | Def(_) =>
    splice_def(e.e_id, rewrap_ws(find_def, e.e_id, seg, body_text(e)), seg)
    |> splice_pat(e.e_id, rewrap_ws(find_pat, e.e_id, seg, header_text(e)))
  };

let cell_content = (e: ScratchCell.t, seg: Segment.t): option(Segment.t) =>
  switch (e.e_kind) {
  | Members => Option.bind(find_def(e.e_id, seg), brace_child)
  | Run(_) => test_run_deep(e.e_id, seg) |> Option.map(fst)
  | Headless(_) => headless_content_deep(e.e_id, seg) |> Option.map(fst)
  | Def(_) => find_def(e.e_id, seg)
  };
