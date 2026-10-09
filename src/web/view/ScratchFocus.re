/* A cell's body is the definition's RHS child segment (between `=` and
   `in`/`;`), complete and grouted; slicing the whole `let…in` tile
   leaves a prefix tile without its operand and crashes Skel. */
open Haz3lcore;

include Haz3lcore.DefinitionSpans;

/* a member body opening with `…in` lies flat in the member's run: it
   is a let-in block, from that first `…in` up to the member's `;` */
let flat_body = (arr: array(Piece.t), sp: item_span): option((int, int)) => {
  /* the first `…in` past the head; a span opening with one is a let-in */
  let rec first_in = (~head, i) =>
    i >= sp.sp_stop
      ? None
      : (
        switch (arr[i]) {
        | Tile(t) when ends_with_in(t) => head ? None : Some(i)
        | Tile(_) => first_in(~head=false, i + 1)
        | _ => first_in(~head, i + 1)
        }
      );
  let rec back = i =>
    i > sp.sp_start && is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
  let stop = back(sp.sp_stop);
  let stop = stop > sp.sp_start && is_semi(arr[stop - 1]) ? stop - 1 : stop;
  sp.sp_kind == IDef
    ? Option.map(b0 => (b0, stop), first_in(~head=true, sp.sp_start)) : None;
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
      sp => seg_contains_id(fid, slice(sp.sp_start, sp.sp_stop, seg)),
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
    let arr = Array.of_list(seg);
    let rec back = i =>
      i > sp_start && is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
    let stop = back(sp_stop);
    let stop = stop > sp_start && is_semi(arr[stop - 1]) ? stop - 1 : stop;
    Some((sp_start, stop, {js|;|js}));
  | Some({sp_kind: ITail, sp_start, sp_stop, _}) =>
    Some((sp_start, sp_stop, {js|⇒|js}))
  | Some(sp) =>
    let arr = Array.of_list(seg);
    switch (flat_body_of(fid, arr, [sp])) {
    | Some((b0, b1)) =>
      headless_span(~divided_only_tail=true, fid, slice(b0, b1, seg))
      |> Option.map(((a, b, sym)) => (b0 + a, b0 + b, sym))
    | None => None
    };
  | None => None
  };

/* headerless content at any block depth: nested blocks share the
   boundary structure, so the span walk applies at each level; an id
   inside a def span yields None there and the walk descends */
let rec headless_deep_go =
        (fid: Id.t, seg: Segment.t): option((Segment.t, string)) =>
  switch (headless_span(~divided_only_tail=true, fid, seg)) {
  | Some((start, stop, sym)) => Some((slice(start, stop, seg), sym))
  | None =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.find_map(ch => headless_deep_go(fid, ch), t.children)
        | _ => None
        },
      seg,
    )
  };

/* the top level's tail is unconditional (the ⇒ row); nested levels
   have one only after an item boundary */
let headless_content_deep =
    (fid: Id.t, seg: Segment.t): option((Segment.t, string)) =>
  switch (headless_span(fid, seg)) {
  | Some((start, stop, sym)) => Some((slice(start, stop, seg), sym))
  | None =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.find_map(ch => headless_deep_go(fid, ch), t.children)
        | _ => None
        },
      seg,
    )
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

/* A cell shows its item from its own left edge: the indentation every
   line after the first has in the program (the [base] of the line the
   cell's text starts on) is cut on the way in and put back on the way
   out. Putting back hands over the program's own pieces wherever the
   cell left them alone (the incremental parse and layout check identity),
   and a line new to the cell gets [base] spaces made once for its line
   break. */

let is_space_piece = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(w) => Secondary.is_space(w)
  | _ => false
  };

let is_linebreak_piece = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(w) => Secondary.is_linebreak(w)
  | _ => false
  };

/* up to [n] of the spaces that open [ps] */
let rec drop_spaces = (n: int, ps: Segment.t): Segment.t =>
  switch (ps) {
  | [p, ...rest] when n > 0 && is_space_piece(p) => drop_spaces(n - 1, rest)
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
        | [p, ...rest] when is_linebreak_piece(p) => [
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

let rec split_spaces = (ps: Segment.t): (Segment.t, Segment.t) =>
  switch (ps) {
  | [p, ...rest] when is_space_piece(p) =>
    let (sp, rest) = split_spaces(rest);
    ([p, ...sp], rest);
  | _ => ([], ps)
  };

let pairs_of = (ind: ScratchCell.indent): cut_pairs => {
  let tiles = ref(Id.Map.empty);
  let after = ref(Id.Map.empty);
  /* [o] and [c] match piece for piece but for the spaces cut after line
     breaks, which lead the rest of [o] only */
  let rec pair = (o: Segment.t, c: Segment.t) =>
    switch (o, c) {
    | ([lb, ...o_rest], [_, ...c_rest]) when is_linebreak_piece(lb) =>
      let (o_sp, o_rest) = split_spaces(o_rest);
      let (c_sp, c_rest) = split_spaces(c_rest);
      let removed =
        Util.ListUtil.take(List.length(o_sp) - List.length(c_sp), o_sp);
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
    pairs_memo := [(ind.cut, p), ...Util.ListUtil.take(31, pairs_memo^)];
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
        | [lb, ...rest] when is_linebreak_piece(lb) =>
          let back =
            switch (Id.Map.find_opt(Piece.id(lb), after)) {
            | Some(sp) => sp
            | None =>
              switch (rest) {
              | [next, ..._] when is_linebreak_piece(next) => []
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

let splice_headless_deep =
    (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t => {
  let rec go = (~top: bool, seg: Segment.t): Segment.t =>
    switch (headless_span(~divided_only_tail=!top, fid, seg)) {
    | Some((start, stop, _)) =>
      let (pre, _, suf) = trim_ws(slice(start, stop, seg));
      let out = take(start, seg) @ pre @ repl @ suf @ drop(stop, seg);
      Segment.ptr_eq(out, seg) ? seg : out;
    | None =>
      map_sharing(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) =>
            tile_sharing(p, t, map_sharing(go(~top=false), t.children))
          | _ => p
          },
        seg,
      )
    };
  go(~top=true, seg);
};

/* the `test` before the statement `;` [semi]: a run member's row is its
   `;`, which stays outside the run's cell */
let rec test_before_semi = (semi: Id.t, seg: Segment.t): option(Id.t) => {
  let rec scan = (last, ps: Segment.t) =>
    switch (ps) {
    | [] => None
    | [Piece.Tile(t), ..._] when t.id == semi => last
    | [Piece.Tile(t), ...rest] =>
      switch (List.find_map(test_before_semi(semi), t.children)) {
      | Some(_) as found => found
      | None =>
        scan(
          switch (Tile.label(t)) {
          | ["test", ..._] => Some(t.id)
          | _ => last
          },
          rest,
        )
      }
    | [_, ...rest] => scan(last, rest)
    };
  scan(None, seg);
};

/* contiguous test runs: the outline's "tests" container opens one cell
   spanning the whole run */
let span_is_test = (arr: array(Piece.t), sp: item_span): bool =>
  if (sp.sp_kind != IStmt) {
    false;
  } else {
    let rec first_tile = i =>
      i >= sp.sp_stop
        ? None
        : (
          switch (arr[i]) {
          | Tile(t) => Some(t)
          | _ => first_tile(i + 1)
          }
        );
    switch (first_tile(sp.sp_start)) {
    | Some(t) =>
      switch (Tile.label(t)) {
      | [hd, ..._] => hd == "test"
      | [] => false
      }
    | None => false
    };
  };

/* the span's leading `test` tile: module members are repped by it,
   top-level statements by their `;`, so run members carry both ids
   (harmless: consumers test membership against outline ids) */
let span_test_tile_id = (arr: array(Piece.t), sp: item_span): option(Id.t) => {
  let rec go = i =>
    i >= sp.sp_stop
      ? None
      : (
        switch (arr[i]) {
        | Piece.Tile(t) =>
          switch (Tile.label(t)) {
          | ["test", ..._] => Some(t.id)
          | _ => None
          }
        | _ => go(i + 1)
        }
      );
  go(sp.sp_start);
};

/* the maximal run of adjacent test statements holding [fid], as (start,
   stop, member ids); the last `;` stays outside, like a statement's. In
   a module body a final `;`-less test parses as the tail but joins the
   run; an expression block's tail is its value and never does. */
let test_run =
    (~module_body=false, fid: Id.t, seg: Segment.t)
    : option((int, int, list(Id.t), int)) => {
  let arr = Array.of_list(seg);
  let spans = Array.of_list(item_spans(seg));
  let n = Array.length(spans);
  let in_run = (sp: item_span): bool =>
    switch (sp.sp_kind) {
    | IStmt => span_is_test(arr, sp)
    | ITail => module_body && span_test_tile_id(arr, sp) != None
    | IDef => false
    };
  let rec idx = j =>
    j >= n
      ? None
      : spans[j].sp_id == Some(fid)
        || seg_contains_id(
             fid,
             slice(spans[j].sp_start, spans[j].sp_stop, seg),
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
    let rec back = i =>
      i > start && is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
    let stop = back(spans[b].sp_stop);
    let stop = stop > start && is_semi(arr[stop - 1]) ? stop - 1 : stop;
    Some((start, stop, members, b - a + 1));
  | _ => None
  };
};

/* test runs at any block depth, like headless_deep_go */
let rec test_run_deep_go =
        (~module_body: bool, fid: Id.t, seg: Segment.t)
        : option((Segment.t, list(Id.t), int)) =>
  switch (test_run(~module_body, fid, seg)) {
  | Some((start, stop, members, n)) =>
    Some((slice(start, stop, seg), members, n))
  | None =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          List.find_map(
            test_run_deep_go(
              ~module_body=List.mem(Sort.Mod, Tile.mold(t).in_),
              fid,
            ),
            t.children,
          )
        | _ => None
        },
      seg,
    )
  };

let test_run_deep = (fid: Id.t, seg: Segment.t) =>
  test_run_deep_go(~module_body=false, fid, seg)
  |> Option.map(((run, members, _)) => (run, members));

/* how many tests the run holding [fid] has (members carry two ids each) */
let test_run_size_deep = (fid: Id.t, seg: Segment.t): option(int) =>
  test_run_deep_go(~module_body=false, fid, seg)
  |> Option.map(((_, _, n)) => n);

let splice_run_deep = (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t => {
  let rec go = (~module_body: bool, seg: Segment.t): Segment.t =>
    switch (test_run(~module_body, fid, seg)) {
    | Some((start, stop, _, _)) =>
      let (pre, _, suf) = trim_ws(slice(start, stop, seg));
      let out = take(start, seg) @ pre @ repl @ suf @ drop(stop, seg);
      Segment.ptr_eq(out, seg) ? seg : out;
    | None =>
      map_sharing(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) =>
            tile_sharing(
              p,
              t,
              map_sharing(
                go(~module_body=List.mem(Sort.Mod, Tile.mold(t).in_)),
                t.children,
              ),
            )
          | _ => p
          },
        seg,
      )
    };
  go(~module_body=false, seg);
};

/* the definition RHS of item [fid]: a 3-shard `let … = … in`'s last
   child, or for a 2-shard member `let … =`, the sibling run after it
   up to the next `;` (or segment end) */
let rec find_def = (fid: Id.t, seg: Segment.t): option(Segment.t) => {
  let rec scan = (ps: list(Piece.t)): option(Segment.t) =>
    switch (ps) {
    | [] => None
    | [Piece.Tile(t), ...rest] when t.id == fid =>
      if (ends_with_in(t)) {
        switch (List.rev(t.children)) {
        | [def, ..._] => Some(def)
        | [] => None
        };
      } else {
        Some(fst(split_at_semi(rest)));
      }
    | [Piece.Tile(t), ...rest] =>
      switch (
        List.fold_left(
          (acc, child) => acc == None ? find_def(fid, child) : acc,
          None,
          t.children,
        )
      ) {
      | Some(d) => Some(d)
      | None => scan(rest)
      }
    | [_, ...rest] => scan(rest)
    };
  scan(seg);
};

let rec splice_def = (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t => {
  let rec scan = (ps: list(Piece.t)): list(Piece.t) =>
    switch (ps) {
    | [] => []
    | [Piece.Tile(t) as p, ...rest] when t.id == fid =>
      /* the program's own tile when the text is its own */
      if (ends_with_in(t)) {
        switch (List.rev(t.children)) {
        | [last, ..._] when Segment.ptr_eq(repl, last) => [p, ...rest]
        | [_, ...rev_rest] => [
            Piece.Tile({
              ...t,
              children: List.rev([repl, ...rev_rest]),
            }),
            ...rest,
          ]
        | [] => [p, ...rest]
        };
      } else {
        let (_, tail) = split_at_semi(rest);
        [p, ...repl] @ tail;
      }
    | [Piece.Tile(t) as p, ...rest] => [
        tile_sharing(p, t, map_sharing(splice_def(fid, repl), t.children)),
        ...scan(rest),
      ]
    | [p, ...rest] => [p, ...scan(rest)]
    };
  /* keep the list itself on no change: parents compare child segments
     by ===, so a fresh copy would still rebuild every ancestor tile */
  let out = scan(seg);
  Segment.ptr_eq(out, seg) ? seg : out;
};

let zip_of_cell = (cell: CellEditor.Model.t): Segment.t =>
  Zipper.unselect_and_zip(cell.editor.editor.state.zipper);

/* the caret starts at the top of a fresh cell: unzip's default
   direction (Right) would leave it after the whole segment */
let cell_of_seg = (~root=Sort.Exp, seg: Segment.t): CellEditor.Model.t =>
  seg
  |> Zipper.unzip(~direction=Left)
  |> Editor.Model.mk(~root)
  |> CellEditor.Model.mk;

let pat_cell_of_seg = (seg: Segment.t): CellEditor.Model.t =>
  seg
  |> Zipper.unzip(~direction=Left)
  |> Editor.Model.mk(~root=Pat)
  |> CellEditor.Model.mk;

let typ_cell_of_seg = (seg: Segment.t): CellEditor.Model.t =>
  seg
  |> Zipper.unzip(~direction=Left)
  |> Editor.Model.mk(~root=Typ)
  |> CellEditor.Model.mk;

let tpat_cell_of_seg = (seg: Segment.t): CellEditor.Model.t =>
  seg
  |> Zipper.unzip(~direction=Left)
  |> Editor.Model.mk(~root=TPat)
  |> CellEditor.Model.mk;

/* a `type … = …` alias takes a Typ body and a TPat header */
let rec is_type_item = (fid: Id.t, seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when t.id == fid =>
        switch (Tile.label(t)) {
        | ["type", ..._] => true
        | _ => false
        }
      | Tile(t) => List.exists(is_type_item(fid), t.children)
      | _ => false
      },
    seg,
  );

let rec is_module_item = (fid: Id.t, seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when t.id == fid =>
        switch (Tile.label(t)) {
        | ["module", ..._] => true
        | _ => false
        }
      | Tile(t) => List.exists(is_module_item(fid), t.children)
      | _ => false
      },
    seg,
  );

/* the header pattern is the first child of 2- and 3-shard items alike */
let rec find_pat = (fid: Id.t, seg: Segment.t): option(Segment.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (acc) {
      | Some(_) => acc
      | None =>
        switch (p) {
        | Tile(t) when t.id == fid =>
          switch (t.children) {
          | [pat, ..._] => Some(pat)
          | [] => None
          }
        | Tile(t) =>
          List.fold_left(
            (acc, child) => acc == None ? find_pat(fid, child) : acc,
            None,
            t.children,
          )
        | _ => None
        }
      },
    None,
    seg,
  );

let rec splice_pat = (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t =>
  map_sharing(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when t.id == fid =>
        switch (t.children) {
        | [first, ..._] when Segment.ptr_eq(repl, first) => p
        | [_, ...rest] =>
          Piece.Tile({
            ...t,
            children: [repl, ...rest],
          })
        | [] => p
        }
      | Tile(t) =>
        tile_sharing(p, t, map_sharing(splice_pat(fid, repl), t.children))
      | _ => p
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
    List.fold_left(
      (acc, p: Piece.t) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          switch (info_of(Piece.id(p))) {
          | Some(i) => Some(i)
          | None =>
            switch (p) {
            | Tile(t) =>
              List.fold_left(
                (acc, ch) => acc == None ? seg_info(ch) : acc,
                None,
                t.children,
              )
            | _ => None
            }
          }
        },
      None,
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
  pat_cell_of_seg([
    Piece.Grout({
      id: Id.mk(),
      shape: Convex,
    }),
  ]);

/* only delimiter-complete items open as cells: an unfinished `let x =`
   has no definition slot to show */
let item_complete = (fid: Id.t, seg: Segment.t): bool =>
  !
    List.exists(
      (t: Tile.t) => t.id == fid,
      Segment.incomplete_tiles_deep(seg),
    );

let rec mk_entry =
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
        let sym = Option.value(sym, ~default=span_sym);
        let content = core_ws(raw);
        let (cut, body_indent) = cut_indent(master_seg, content);
        let e_ctx =
          switch (captured_ctx(~info_map, fid, content)) {
          | Some(ctx) => ctx
          | None =>
            Language.Builtins.ctx_init(Some(Language.Operators.default_mode))
          };
        Some(
          ScratchCell.{
            e_id: fid,
            e_mod: false,
            e_sym: Some(sym),
            e_run: false,
            e_members: [],
            e_inner: false,
            e_header: empty_header_cell(),
            e_body: cell_of_seg(cut),
            e_header_indent: ScratchCell.no_indent,
            e_body_indent: body_indent,
            e_ctx,
          },
        );
      | None => mk_def_entry(~info_map, fid, master_seg)
      }
    )
and mk_def_entry =
    (~info_map: Language.Statics.Map.t, fid: Id.t, master_seg: Segment.t)
    : option(ScratchCell.t) =>
  switch (find_def(fid, master_seg)) {
  | None => None
  | Some(def_seg) =>
    let is_type = is_type_item(fid, master_seg);
    let (header, header_indent) =
      cut_indent(
        master_seg,
        core_ws(Option.value(find_pat(fid, master_seg), ~default=[])),
      );
    let (body, body_indent) = cut_indent(master_seg, core_ws(def_seg));
    let e_ctx =
      switch (captured_ctx(~info_map, fid, def_seg)) {
      | Some(ctx) => ctx
      | None =>
        Language.Builtins.ctx_init(Some(Language.Operators.default_mode))
      };
    Some(
      ScratchCell.{
        e_id: fid,
        e_mod: is_module_item(fid, master_seg),
        e_sym: None,
        e_run: false,
        e_members: [],
        e_inner: false,
        e_header: (is_type ? tpat_cell_of_seg : pat_cell_of_seg)(header),
        e_body: is_type ? typ_cell_of_seg(body) : cell_of_seg(body),
        e_header_indent: header_indent,
        e_body_indent: body_indent,
        e_ctx,
      },
    );
  };

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

let mk_members_entry =
    (~info_map: Language.Statics.Map.t, fid: Id.t, master_seg: Segment.t)
    : option(ScratchCell.t) =>
  !item_complete(fid, master_seg)
    ? None
    : (
      switch (Option.bind(find_def(fid, master_seg), brace_child)) {
      | None => None
      | Some(members) =>
        let (header, header_indent) =
          cut_indent(
            master_seg,
            core_ws(Option.value(find_pat(fid, master_seg), ~default=[])),
          );
        let (body, body_indent) = cut_indent(master_seg, core_ws(members));
        let e_ctx =
          switch (captured_ctx(~info_map, fid, members)) {
          | Some(ctx) => ctx
          | None =>
            Language.Builtins.ctx_init(Some(Language.Operators.default_mode))
          };
        Some(
          ScratchCell.{
            e_id: fid,
            e_mod: true,
            e_sym: None,
            e_run: false,
            e_members: [],
            e_inner: true,
            e_header: pat_cell_of_seg(header),
            e_body: cell_of_seg(~root=Sort.Mod, body),
            e_header_indent: header_indent,
            e_body_indent: body_indent,
            e_ctx,
          },
        );
      }
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
        let (content, body_indent) =
          cut_indent(master_seg, core_ws(run_slice));
        let e_ctx =
          switch (captured_ctx(~info_map, fid, content)) {
          | Some(ctx) => ctx
          | None =>
            Language.Builtins.ctx_init(Some(Language.Operators.default_mode))
          };
        Some(
          ScratchCell.{
            e_id: fid,
            e_mod: false,
            e_sym: Some("tests"),
            e_run: true,
            e_members: members,
            e_inner: false,
            e_header: empty_header_cell(),
            e_body: cell_of_seg(content),
            e_header_indent: ScratchCell.no_indent,
            e_body_indent: body_indent,
            e_ctx,
          },
        );
      }
    );

/* a cell's header and body text in program coordinates */
let body_text = (e: ScratchCell.t): Segment.t =>
  restore(e.e_body_indent, zip_of_cell(e.e_body));
let header_text = (e: ScratchCell.t): Segment.t =>
  restore(e.e_header_indent, zip_of_cell(e.e_header));

/* splice a cell's text back into [seg], keeping [seg]'s edge whitespace */

let splice_entry = (e: ScratchCell.t, seg: Segment.t): Segment.t =>
  switch (e.e_sym) {
  | None when e.e_inner =>
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
  | Some(_) when e.e_run => splice_run_deep(e.e_id, body_text(e), seg)
  | Some(_) => splice_headless_deep(e.e_id, body_text(e), seg)
  | None =>
    splice_def(e.e_id, rewrap_ws(find_def, e.e_id, seg, body_text(e)), seg)
    |> splice_pat(e.e_id, rewrap_ws(find_pat, e.e_id, seg, header_text(e)))
  };

let cell_content = (e: ScratchCell.t, seg: Segment.t): option(Segment.t) =>
  switch (e.e_sym) {
  | None when e.e_inner => Option.bind(find_def(e.e_id, seg), brace_child)
  | Some(_) when e.e_run => test_run_deep(e.e_id, seg) |> Option.map(fst)
  | Some(_) => headless_content_deep(e.e_id, seg) |> Option.map(fst)
  | None => find_def(e.e_id, seg)
  };
