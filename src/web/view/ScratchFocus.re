module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;

/* ---- definition-focus helpers (modular-editors phase 2) ----
   Focus targets the definition's RHS child segment (between `=` and
   `in`/`;`) — a complete, properly-grouted expression, per the adopted
   cell design (plan §2). Slicing the whole `let…in` tile instead
   leaves a prefix tile without its operand and crashes Skel. */
open Haz3lcore;

let ends_with_in = (t: Base.tile): bool =>
  switch (List.rev(t.label)) {
  | ["in", ..._] => true
  | _ => false
  };
let is_semi = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => t.label == [";"]
  | _ => false
  };
/* Edge-whitespace handling: the raw pat/def slices carry the
   master's padding (spaces around the pat, the linebreak+indent
   before a def) — in an isolated cell that reads as stray
   whitespace begging to be deleted. Cells hold the TRIMMED core;
   the splice re-wraps with whatever edge whitespace the (stale)
   master copy still carries, so padding round-trips without being
   stored. Comments are content, not padding — they stay. */
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

/* re-wrap [content] in the edge whitespace of the segment [find]
   locates in [seg] (the master's copy, untouched while focused) */
let rewrap_ws =
    (find: (Id.t, Segment.t) => option(Segment.t), fid, seg, content)
    : Segment.t =>
  switch (find(fid, seg)) {
  | Some(old) =>
    let (pre, _, suf) = trim_ws(old);
    pre @ content @ suf;
  | None => content
  };

/* does [seg] contain a piece with id [target] (recursively)? */
let rec seg_contains_id = (target: Id.t, seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      Piece.id(p) == target
      || (
        switch (p) {
        | Tile(t) => List.exists(seg_contains_id(target), t.children)
        | _ => false
        }
      ),
    seg,
  );

let rec take = (n, xs) =>
  switch (n, xs) {
  | (0, _)
  | (_, []) => []
  | (n, [x, ...xs]) => [x, ...take(n - 1, xs)]
  };
let rec drop = (n, xs) =>
  switch (n, xs) {
  | (0, _)
  | (_, []) => xs
  | (n, [_, ...xs]) => drop(n - 1, xs)
  };
let slice = (a, b, xs) => take(b - a, drop(a, xs));

/* split [ps] at the first `;` piece: (def run, separator + rest) */
let split_at_semi = (ps: list(Piece.t)): (list(Piece.t), list(Piece.t)) => {
  let rec go = (acc, ps) =>
    switch (ps) {
    | [] => (List.rev(acc), [])
    | [p, ..._] when is_semi(p) => (List.rev(acc), ps)
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], ps);
};

/* --- top-level item spans, BY PIECE STRUCTURE (no parse) ---
   Boundaries are `…in`-tiles (def items: the tile + trailing ws)
   and top-level `;`s (statement items: the run since the previous
   boundary through the `;` + trailing ws); whatever remains is the
   trailing expression. Spans partition the top-level piece list, so
   restructure ops and headerless cells slice/splice without ever
   parsing the program. */
type item_kind =
  | IDef /* let / type / module: header+body cells */
  | IStmt /* a `…;` statement: headerless cell */
  | ITail; /* the trailing expression: headerless cell */

type item_span = {
  sp_id: option(Id.t), /* the boundary tile's id; None for the tail */
  sp_start: int,
  sp_stop: int, /* exclusive */
  sp_kind: item_kind,
};

let item_spans = (~divided_only_tail=false, seg: Segment.t): list(item_span) => {
  let arr = Array.of_list(seg);
  let len = Array.length(arr);
  let rec ws_end = i => i < len && is_edge_ws(arr[i]) ? ws_end(i + 1) : i;
  let is_in_tile = (p: Piece.t) =>
    switch (p) {
    | Tile(t) => ends_with_in(t)
    | _ => false
    };
  /* MODULE BODIES have 2-shard member defs terminated by `;`: a
     `;`-run whose first tile is a def head is a DEF item, not a
     statement (its cell takes the header/body path) */
  let run_def_head = (start: int, stop: int): option(Id.t) => {
    let rec first_tile = i =>
      i >= stop
        ? None
        : (
          switch (arr[i]) {
          | Piece.Tile(t) => Some(t)
          | _ => first_tile(i + 1)
          }
        );
    switch (first_tile(start)) {
    | Some(t) =>
      switch (t.label) {
      | ["let", ..._]
      | ["type", ..._]
      | ["module", ..._] => Some(t.id)
      | _ => None
      }
    | None => None
    };
  };
  let rec walk = (i, start, acc) =>
    if (i >= len) {
      /* the remainder (if it has content) is the trailing expr —
         or a trailing 2-shard member def */
      let has_content = {
        let rec go = j => j < len && (!is_edge_ws(arr[j]) || go(j + 1));
        start < len && go(start);
      };
      let tail_ok =
        switch (run_def_head(start, len)) {
        | Some(_) => true
        /* a boundary-less segment is an EXPRESSION, not a block: its
           content must not read as a trailing item (deep containment
           would otherwise swallow arbitrary ids). The program's own
           top level keeps unconditional tails (the ⇒ row). */
        | None => !divided_only_tail || acc != []
        };
      List.rev(
        has_content && tail_ok
          ? [
            switch (run_def_head(start, len)) {
            | Some(id) => {
                sp_id: Some(id),
                sp_start: start,
                sp_stop: len,
                sp_kind: IDef,
              }
            | None => {
                sp_id: None,
                sp_start: start,
                sp_stop: len,
                sp_kind: ITail,
              }
            },
            ...acc,
          ]
          : acc,
      );
    } else if (is_in_tile(arr[i])) {
      let stop = ws_end(i + 1);
      walk(
        stop,
        stop,
        [
          {
            sp_id: Some(Piece.id(arr[i])),
            sp_start: i,
            sp_stop: stop,
            sp_kind: IDef,
          },
          ...acc,
        ],
      );
    } else if (is_semi(arr[i])) {
      let stop = ws_end(i + 1);
      let sp =
        switch (run_def_head(start, i)) {
        | Some(id) => {
            sp_id: Some(id),
            sp_start: start,
            sp_stop: stop,
            sp_kind: IDef,
          }
        | None => {
            sp_id: Some(Piece.id(arr[i])),
            sp_start: start,
            sp_stop: stop,
            sp_kind: IStmt,
          }
        };
      walk(stop, stop, [sp, ...acc]);
    } else {
      walk(i + 1, start, acc);
    };
  walk(0, 0, []);
};

/* the span holding [fid]: by boundary id first, then containment
   (outline ids can be tiles INSIDE an item — module binders, the
   trailing expression's root) */
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

/* the content sub-span a HEADERLESS item's cell holds (statement:
   the run before its `;`; trailing expr: the whole span), plus the
   static header symbol. None for def items. */
let headless_span =
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
  | _ => None
  };

/* headless extraction/splice at ANY block depth: try this segment's
   top level, then recurse into tile children. Nested blocks (module
   bodies, fn bodies) share the boundary structure (`…in`-tiles and
   `;`s), so the same span walk applies at each level; a nested id
   contained in a DEF span yields None there and recursion descends. */
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

/* the top level keeps unconditional-tail semantics (the ⇒ row of a
   bare-expression program); nested levels require DIVIDED blocks */
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

let splice_headless_deep =
    (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t => {
  let rec go = (~top: bool, seg: Segment.t): Segment.t =>
    switch (headless_span(~divided_only_tail=!top, fid, seg)) {
    | Some((start, stop, _)) =>
      let (pre, _, suf) = trim_ws(slice(start, stop, seg));
      take(start, seg) @ pre @ repl @ suf @ drop(stop, seg);
    | None =>
      List.map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) =>
            Piece.Tile({
              ...t,
              children: List.map(go(~top=false), t.children),
            })
          | _ => p
          },
        seg,
      )
    };
  go(~top=true, seg);
};

/* --- contiguous TEST RUNS (the outline's "tests" container pins
   one cell spanning the whole run) --- */
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
      switch (t.label) {
      | [hd, ..._] => hd == "test"
      | [] => false
      }
    | None => false
    };
  };

/* the maximal run of adjacent test statements containing [fid]:
   (content start, content stop — before the LAST `;` + trivia,
   which stay master-side like any statement's — member item ids) */
let test_run = (fid: Id.t, seg: Segment.t): option((int, int, list(Id.t))) => {
  let arr = Array.of_list(seg);
  let spans = Array.of_list(item_spans(seg));
  let n = Array.length(spans);
  let rec idx = j =>
    j >= n ? None : spans[j].sp_id == Some(fid) ? Some(j) : idx(j + 1);
  switch (idx(0)) {
  | Some(j) when span_is_test(arr, spans[j]) =>
    let rec lo = j =>
      j > 0 && span_is_test(arr, spans[j - 1]) ? lo(j - 1) : j;
    let rec hi = j =>
      j + 1 < n && span_is_test(arr, spans[j + 1]) ? hi(j + 1) : j;
    let (a, b) = (lo(j), hi(j));
    let members =
      List.filter_map(k => spans[k].sp_id, List.init(b - a + 1, k => a + k));
    let start = spans[a].sp_start;
    let rec back = i =>
      i > start && is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
    let stop = back(spans[b].sp_stop);
    let stop = stop > start && is_semi(arr[stop - 1]) ? stop - 1 : stop;
    Some((start, stop, members));
  | _ => None
  };
};

/* The definition RHS for the item tile [fid]:
   - `let … = … in` (3 shards): the def is the tile's LAST CHILD;
   - module-member `let … =` (2 shards): the def is the SIBLING run
     after the tile, up to the member separator `;` (or segment end).
   Returns a complete, properly-grouted child segment either way. */
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

/* replace the definition RHS of item [fid] with [repl] */
let rec splice_def = (fid: Id.t, repl: Segment.t, seg: Segment.t): Segment.t => {
  let rec scan = (ps: list(Piece.t)): list(Piece.t) =>
    switch (ps) {
    | [] => []
    | [Piece.Tile(t), ...rest] when t.id == fid =>
      if (ends_with_in(t)) {
        let t' =
          switch (List.rev(t.children)) {
          | [_, ...rev_rest] => {
              ...t,
              children: List.rev([repl, ...rev_rest]),
            }
          | [] => t
          };
        [Piece.Tile(t'), ...rest];
      } else {
        let (_, tail) = split_at_semi(rest);
        [Piece.Tile(t), ...repl] @ tail;
      }
    | [Piece.Tile(t), ...rest] => [
        Piece.Tile({
          ...t,
          children: List.map(splice_def(fid, repl), t.children),
        }),
        ...scan(rest),
      ]
    | [p, ...rest] => [p, ...scan(rest)]
    };
  scan(seg);
};

let zip_of_cell = (cell: CellEditor.Model.t): Segment.t =>
  Zipper.unselect_and_zip(cell.editor.editor.state.zipper);

/* caret starts at the TOP of a fresh cell: unzip's default
   direction (Right) would leave it after the whole segment */
let cell_of_seg = (seg: Segment.t): CellEditor.Model.t =>
  seg
  |> Zipper.unzip(~direction=Left)
  |> Editor.Model.mk(~root=Exp)
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

/* is the item tile a `type … = …` alias? (roots differ: Typ body,
   TPat header) */
let rec is_type_item = (fid: Id.t, seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when t.id == fid =>
        switch (t.label) {
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
        switch (t.label) {
        | ["module", ..._] => true
        | _ => false
        }
      | Tile(t) => List.exists(is_module_item(fid), t.children)
      | _ => false
      },
    seg,
  );

/* the pattern (header) is the FIRST child for every focusable item
   shape: `let <pat> = …` 2- and 3-shard alike */
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
  List.map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when t.id == fid =>
        switch (t.children) {
        | [_, ...rest] =>
          Piece.Tile({
            ...t,
            children: [repl, ...rest],
          })
        | [] => p
        }
      | Tile(t) =>
        Piece.Tile({
          ...t,
          children: List.map(splice_pat(fid, repl), t.children),
        })
      | _ => p
      },
    seg,
  );

/* the master slide with the live focus-cell content spliced back in
   (pure; used by unfocus AND by persistence while focused) */
/* build a stack entry for the item [fid] (None if not found) */
/* the ctx INSIDE the def — params included, which matters for
   funlets: the first info found among the def's pieces, falling
   back to the item's own info */
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

/* headerless entries carry an empty (grout) header cell — never
   rendered, never spliced; a bare [] zipper would crash Skel */
let empty_header_cell = (): CellEditor.Model.t =>
  pat_cell_of_seg([
    Piece.Grout({
      id: Id.mk(),
      shape: Convex,
    }),
  ]);

let rec mk_entry =
        (
          ~info_map: Language.Statics.Map.t,
          ~sym: option(string)=?,
          fid: Id.t,
          master_seg: Segment.t,
        )
        : option(Model.stack_entry) =>
  switch (headless_content_deep(fid, master_seg)) {
  | Some((raw, span_sym)) =>
    let sym = Option.value(sym, ~default=span_sym);
    let content = core_ws(raw);
    let e_ctx =
      switch (captured_ctx(~info_map, fid, content)) {
      | Some(ctx) => ctx
      | None =>
        Language.Builtins.ctx_init(Some(Language.Operators.default_mode))
      };
    Some(
      Model.{
        e_id: fid,
        e_mod: false,
        e_sym: Some(sym),
        e_run: false,
        e_members: [],
        e_header: empty_header_cell(),
        e_body: cell_of_seg(content),
        e_ctx,
      },
    );
  | None => mk_def_entry(~info_map, fid, master_seg)
  }
and mk_def_entry =
    (~info_map: Language.Statics.Map.t, fid: Id.t, master_seg: Segment.t)
    : option(Model.stack_entry) =>
  switch (find_def(fid, master_seg)) {
  | None => None
  | Some(def_seg) =>
    let is_type = is_type_item(fid, master_seg);
    let e_ctx =
      switch (captured_ctx(~info_map, fid, def_seg)) {
      | Some(ctx) => ctx
      | None =>
        Language.Builtins.ctx_init(Some(Language.Operators.default_mode))
      };
    Some(
      Model.{
        e_id: fid,
        e_mod: is_module_item(fid, master_seg),
        e_sym: None,
        e_run: false,
        e_members: [],
        e_header:
          (is_type ? tpat_cell_of_seg : pat_cell_of_seg)(
            core_ws(Option.value(find_pat(fid, master_seg), ~default=[])),
          ),
        e_body: (is_type ? typ_cell_of_seg : cell_of_seg)(core_ws(def_seg)),
        e_ctx,
      },
    );
  };

/* ONE cell for a whole contiguous test run (outline "tests"
   container), anchored at the FIRST test's item id */
let mk_run_entry =
    (~info_map: Language.Statics.Map.t, fid: Id.t, master_seg: Segment.t)
    : option(Model.stack_entry) =>
  switch (test_run(fid, master_seg)) {
  | None => mk_entry(~info_map, fid, master_seg)
  | Some((start, stop, members)) =>
    let content = core_ws(slice(start, stop, master_seg));
    let e_ctx =
      switch (captured_ctx(~info_map, fid, content)) {
      | Some(ctx) => ctx
      | None =>
        Language.Builtins.ctx_init(Some(Language.Operators.default_mode))
      };
    Some(
      Model.{
        e_id: fid,
        e_mod: false,
        e_sym: Some("tests"),
        e_run: true,
        e_members: members,
        e_header: empty_header_cell(),
        e_body: cell_of_seg(content),
        e_ctx,
      },
    );
  };

/* splice ONE entry's header+body home into [seg], restoring the
   edge whitespace the master's stale copies still carry */
let splice_entry = (e: Model.stack_entry, seg: Segment.t): Segment.t =>
  switch (e.e_sym) {
  | Some(_) when e.e_run =>
    switch (test_run(e.e_id, seg)) {
    | Some((start, stop, _)) =>
      let (pre, _, suf) = trim_ws(slice(start, stop, seg));
      take(start, seg) @ pre @ zip_of_cell(e.e_body) @ suf @ drop(stop, seg);
    | None => seg
    }
  | Some(_) =>
    /* headerless: replace the item's content run in place (works at
       any block depth) */
    splice_headless_deep(e.e_id, zip_of_cell(e.e_body), seg)
  | None =>
    splice_def(
      e.e_id,
      rewrap_ws(find_def, e.e_id, seg, zip_of_cell(e.e_body)),
      seg,
    )
    |> splice_pat(
         e.e_id,
         rewrap_ws(find_pat, e.e_id, seg, zip_of_cell(e.e_header)),
       )
  };

/* the cell-content slice for any entry kind (ctx recapture) */
let cell_content = (e: Model.stack_entry, seg: Segment.t): option(Segment.t) =>
  switch (e.e_sym) {
  | Some(_) when e.e_run =>
    test_run(e.e_id, seg)
    |> Option.map(((start, stop, _)) => slice(start, stop, seg))
  | Some(_) => headless_content_deep(e.e_id, seg) |> Option.map(fst)
  | None => find_def(e.e_id, seg)
  };

/* the master segment with every live entry spliced home */
let splice_all = (focus: Model.focus_t): Segment.t =>
  List.fold_left(
    (seg, e) => splice_entry(e, seg),
    focus.f_master_seg,
    focus.f_entries,
  );

/* the master scratchpad with live stack edits spliced in (pure;
   used by unfocus AND by persistence while the stack is open) */
let spliced_master = (focus: Model.focus_t, sp: Scratchpad.t): Scratchpad.t =>
  switch (sp.kind) {
  | Code({agent, _}) => {
      ...sp,
      kind:
        Code({
          editor: cell_of_seg(splice_all(focus)),
          agent,
        }),
    }
  | _ => sp
  };
