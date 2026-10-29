/* A cell's body is the definition's RHS child segment (between `=` and
   `in`/`;`), complete and grouted; slicing the whole `let…in` tile
   leaves a prefix tile without its operand and crashes Skel. A program's
   items are ItemSpans' (the division the parser and the layout use): a
   row is found through its levels, never by walking the program */
open Haz3lcore;
open Util;

/* the navigation and splices the editing kernel shares (S: ItemSpans) */
include SpineNav;

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
       (S.core(lv, k), span_sym(lv, k));
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
  let members = lv.sort == Sort.Mod;
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
          S.core(lv, k),
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
          walk(~under=path, pr.syntax);
        | Splice(_)
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
