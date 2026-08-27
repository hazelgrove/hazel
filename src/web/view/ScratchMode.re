open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Scratchpad = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type code = {
    editor: CellEditor.Model.t,
    agent: Agent.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind =
    | Code(code)
    | Drv(DerivationExerciseMode.Model.t);

  /* Lazy hydration: boot builds a full editor (parse + statics cache +
     agent state) for the CURRENT slide only; every other slide is a
     blank placeholder with [dormant] set, swapped for the real slide on
     first switch (Persist.hydrate_current). save_current refuses to
     write a dormant placeholder over the stored slide. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    name: string,
    kind,
    dormant: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type code_persistent = {
    editor: option(CellEditor.Model.persistent),
    agent: Agent.Persistent.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind_persistent =
    | CodePersist(code_persistent)
    | DrvPersist(DerivationExerciseMode.Model.persistent);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    name: string,
    kind: kind_persistent,
  };

  let persist = (s: t): persistent => {
    switch (s.kind) {
    | Code({editor, agent}) =>
      let current_zipper = editor.editor.editor.state.zipper;
      let current_segment = Zipper.zip(current_zipper);
      let original = Init.find_documentation_slide(s.name);
      /* Originals are text-backed (committed .hz) and mint fresh ids on
         every parse, so id-sensitive segment equality can never match;
         compare by the text projection instead — FastParse loads the
         text verbatim, so an unedited slide prints byte-identically
         modulo the stored final newline (the writer's artifact, which
         the print never carries). */
      let unchanged =
        switch (original) {
        | None => false
        | Some(pce) =>
          MarkerParse.seg_to_text(
            ~refractors=current_zipper.refractors.manuals,
            current_segment,
          )
          == Util.StringUtil.strip_final_newline(
               pce.editor.zipper.backup_text,
             )
        };
      let editor_persist =
        if (unchanged) {
          None;
        } else {
          Some(CellEditor.Model.persist(editor));
        };
      {
        name: s.name,
        kind:
          CodePersist({
            editor: editor_persist,
            agent: Agent.Persistent.persist(agent),
          }),
      };
    | Drv(m) => {
        name: s.name,
        kind:
          DrvPersist(
            DerivationExerciseMode.Model.persist(m, ~instructor_mode=false),
          ),
      }
    };
  };

  let mk_code = (~name, ~editor, ()): t => {
    name,
    kind:
      Code({
        editor,
        agent: Agent.Utils.init(),
      }),
    dormant: false,
  };

  let blank_code = (name: string): t =>
    mk_code(
      ~name,
      ~editor=CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp)),
      (),
    );

  let dormant_code = (name: string): t => {
    ...blank_code(name),
    dormant: true,
  };

  let blank_drv = (~settings, name: string): t => {
    name,
    kind:
      Drv(
        DerivationExerciseMode.Model.of_spec(
          ~settings,
          ~instructor_mode=false,
          DerivationExercise.blank_spec(~title=name, ~module_name=name),
        ),
      ),
    dormant: false,
  };
};

module Model = {
  /* Definition-focus mode, STACKED (modular-editors phases 2-3):
     focusing definitions opens a STACK of (header, body) cell pairs
     rendered INSTEAD of the master cell — the master itself stays in
     its scratchpad slot untouched (statics warm, zipper immutable
     while the stack is open). Closing splices every entry's header
     into its pattern slot and body into its definition slot.
     Transient — never persisted; persistence splices live
     (Persist.persist_spliced, a text-backed snapshot). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type stack_entry = {
    e_id: Haz3lcore.Id.t, /* the item tile's id in the master */
    /* header: pattern+signature, PAT- (or TPAT-)rooted */
    e_header: CellEditor.Model.t,
    /* module items: binder is an MPat — wrapped pat statics would
       misread the capitalized name as a constructor, so their headers
       stay statics-off */
    e_mod: bool,
    /* headerless items (top-level statements / the trailing
       expression): the static symbol shown instead of a header cell */
    e_sym: option(string),
    /* a RUN cell: one editor spanning a contiguous run of test
       statements, anchored at the first test's item id */
    e_run: bool,
    /* run cells: the item ids the run covers (first = e_id) */
    e_members: list(Haz3lcore.Id.t),
    /* body: the definition RHS, EXP- (or TYP-)rooted */
    e_body: CellEditor.Model.t,
    e_ctx: Language.Ctx.t /* frozen outer ctx at the definition */
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus_t = {
    f_entries: list(stack_entry),
    /* the master's zipped segment, cached when the stack opens (and
       updated when an entry closes): persistence splices every
       autosave tick — don't re-zip each second */
    f_master_seg: Haz3lcore.Segment.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list(Scratchpad.t),
    focus: option(focus_t),
  };

  let rec header_name = (e: stack_entry): option(string) =>
    switch (e.e_sym) {
    | Some(sym) => Some(sym)
    | None => header_name_of_cell(e)
    }
  and header_name_of_cell = (e: stack_entry): option(string) => {
    let txt =
      Haz3lcore.MarkerParse.to_text(e.e_header.editor.editor.state.zipper);
    let name =
      switch (String.index_opt(txt, ':')) {
      | Some(i) => String.sub(txt, 0, i)
      | None => txt
      };
    let name = String.trim(name);
    name == "" ? None : Some(name);
  };

  /* (id, live name) for every stack entry — outline labels track
     header renames before any splice-back. Headerless entries (tests,
     statements, ⇒) report None: their outline labels are the
     outline's own (a pinned test was showing ';' instead of its
     number). */
  let focused_names = (model: t): list((Haz3lcore.Id.t, option(string))) =>
    switch (model.focus) {
    | None => []
    | Some(f) =>
      List.concat_map(
        (e: stack_entry) =>
          e.e_run
            ? List.map(id => (id, None), e.e_members)
            : [(e.e_id, e.e_sym == None ? header_name(e) : None)],
        f.f_entries,
      )
    };

  /* The monolithic export/import format (per-slide keys are the live
     storage; see Persist below). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list(Scratchpad.persistent));

  let scratchpad_names = (model: t): list(string) =>
    List.map((s: Scratchpad.t) => s.name, model.scratchpads);
};

/* Per-slide IndexedDB persistence. Each scratchpad's editor and agent
   data is stored as separate HazelDB KV keys, so autosave only writes
   the current slide.

   Key layout:
     <prefix>:_meta         → slide_meta (current_index, names)
     <prefix>:<name>        → CellEditor.Model.persistent
     <prefix>:<name>:agent  → Agent.Persistent.t */
/* ---- definition-focus helpers (modular-editors phase 2) ----
   Focus targets the definition's RHS child segment (between `=` and
   `in`/`;`) — a complete, properly-grouted expression, per the adopted
   cell design (plan §2). Slicing the whole `let…in` tile instead
   leaves a prefix tile without its operand and crashes Skel. */
module Focus = {
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

  let item_spans =
      (~divided_only_tail=false, seg: Segment.t): list(item_span) => {
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
      (~divided_only_tail=false, fid: Id.t, seg: Segment.t)
      : option(item_span) => {
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
  let test_run =
      (fid: Id.t, seg: Segment.t): option((int, int, list(Id.t))) => {
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
        List.filter_map(
          k => spans[k].sp_id,
          List.init(b - a + 1, k => a + k),
        );
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
          e_body:
            (is_type ? typ_cell_of_seg : cell_of_seg)(core_ws(def_seg)),
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
        take(start, seg)
        @ pre
        @ zip_of_cell(e.e_body)
        @ suf
        @ drop(stop, seg);
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
  let cell_content =
      (e: Model.stack_entry, seg: Segment.t): option(Segment.t) =>
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
};

/* outline context-menu state (row id + screen position): transient
   UI, module-level like the other view caches — not model data */
let outline_menu: ref(option((Haz3lcore.Id.t, float, float))) = ref(None);

/* the header symbol a headerless cell for [fid] should show, from
   the OUTLINE's view of the row (span kinds mis-read member-fn tails:
   a member terminates with `;`, so its fn-body tail extracts from an
   IStmt-shaped run — the row is still a ⇒) */
let outline_sym = (fid: Haz3lcore.Id.t, term: Language.Exp.t): option(string) =>
  switch (OutlineTree.kind_of(fid, term)) {
  | Some(OutlineTree.KTrail) => Some({js|⇒|js})
  | Some(OutlineTree.KTest)
  | Some(OutlineTree.KStmt) => Some({js|;|js})
  | _ => None
  };

/* PROJECTION (plan §9e / program-view-split step 3): a stack cell's
   statics come from its DefStatics ITEM — the same ids, analyzed with
   the program's real context (headers see the type the def gave their
   binder; module headers get real MPat info; warnings appear) —
   scoped to the ids the cell actually contains so id-keyed consumers
   (Arms, occurrence highlight) never see foreign ids. The private
   init_* wrappers remain only as the fallback when no item is found.
   [engine_warnings]: unused-binder warnings are computed by the
   ENGINE across items (an item alone can't see its downstream uses),
   so headers take them from the whole-program list. */
let project_cell_statics =
    (
      ~item: Haz3lcore.DefStatics.item,
      ~engine_warnings: list(Haz3lcore.Id.t),
      cell: CellEditor.Model.t,
    )
    : Haz3lcore.CachedStatics.t => {
  let term_data = cell.editor.editor.syntax.term_data;
  let in_cell = id => Haz3lcore.Id.Map.mem(id, term_data);
  Haz3lcore.CachedStatics.{
    term: item.d_node,
    elaborated: item.d_elab,
    info_map: Haz3lcore.Id.Map.filter((id, _) => in_cell(id), item.d_map),
    error_ids: List.filter(in_cell, item.d_error_ids),
    warning_ids: List.filter(in_cell, item.d_warning_ids @ engine_warnings),
    targets: Haz3lcore.Id.Map.empty, /* with_targets refreshes */
    probe_ids:
      Haz3lcore.CachedStatics.probe_ids_of_zipper(
        cell.editor.editor.state.zipper,
      ),
  };
};

/* per-slide pin retention (andrew): switching slides splices the
   stack home; coming back re-opens the same cells. Keyed by slide
   NAME; ids stay valid in-session because hydrated slides keep their
   models (a dormant slide re-parses with fresh ids, but you can't
   have pinned on a slide you haven't visited). Transient by design —
   text-backed reload re-mints ids (name-anchored persistence is
   docketed with the outline-generality spec). */
let slide_pins: Hashtbl.t(string, list((Haz3lcore.Id.t, bool))) =
  Hashtbl.create(8);

/* modeled outline collapse (andrew: DOM-owned <details> state bled
   across slides positionally and reset whenever a structural edit
   made the vdom recreate elements). Per-slide sets of label paths;
   the summary click dispatches OutlineCollapse; the open attr renders
   from this. Persisted per slide (a ":collapse" side key). */
let slide_collapse: Hashtbl.t(string, list(list(string))) =
  Hashtbl.create(8);

let collapse_paths = (name: string): list(list(string)) =>
  switch (Hashtbl.find_opt(slide_collapse, name)) {
  | Some(ps) => ps
  | None => []
  };

/* The spliced whole-program statics computed while a stack is open
   (Force frames, first open frame, and restructure ops — which seed
   it directly to avoid a second whole-program parse): term + merged
   map + grafted elaboration. Feeds the master's EvalResult so
   whole-program DYNAMICS keeps running while stacked. */
let stacked_statics: ref(option(Haz3lcore.CachedStatics.t)) = ref(None);

/* Structural operations on TOP-LEVEL definitions (outline context
   menu): insert / duplicate / move / delete. All act on the LIVE
   whole-program segment (spliced when a stack is open) and rebuild
   the master editor. Untouched items keep their piece ids, so open
   cells still find their definitions and probes stay pinned. */
module Restructure = {
  open Haz3lcore;

  let parse = (~root=Sort.Exp, txt: string): option(Segment.t) =>
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root,
      txt,
    );

  let first_tile_id = (seg: Segment.t): option(Id.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) => Some(t.id)
        | _ => None
        },
      seg,
    );

  /* fresh-id MEMBER pieces for [txt] (a single member, `;` optional):
     the Mod-root fast parse rejects chunks with a trailing separator,
     so parse two members and cut after the last top-level `;` (the
     member's own terminator + trailing trivia) */
  let member_chunk = (txt: string): option(Segment.t) => {
    let txt = String.trim(txt);
    let txt =
      String.length(txt) > 0 && txt.[String.length(txt) - 1] == ';'
        ? String.sub(txt, 0, String.length(txt) - 1) : txt;
    /* the Mod-root wrap parses plain braces whose child is EXP-sorted
       (members came out as let-ins): parse a real module instead and
       extract its body child, then cut after the member's own `;` */
    switch (parse("module Zz = {" ++ txt ++ ";\nlet zz = ?} in\n0")) {
    | None => None
    | Some(seg) =>
      let body = {
        let rec find_mod = (ps: Segment.t) =>
          switch (ps) {
          | [] => None
          | [Piece.Tile(t), ...rest] =>
            switch (t.label) {
            | ["module", ..._] =>
              switch (List.rev(t.children)) {
              | [def, ..._] =>
                List.find_map(
                  (p: Piece.t) =>
                    switch (p) {
                    | Tile(bt) =>
                      switch (bt.children) {
                      | [inner] => Some(inner)
                      | _ => None
                      }
                    | _ => None
                    },
                  def,
                )
              | [] => None
              }
            | _ => find_mod(rest)
            }
          | [_, ...rest] => find_mod(rest)
          };
        find_mod(seg);
      };
      switch (body) {
      | None => None
      | Some(members) =>
        let arr = Array.of_list(members);
        let n = Array.length(arr);
        let rec last_semi = (i, best) =>
          i >= n
            ? best
            : last_semi(i + 1, Focus.is_semi(arr[i]) ? Some(i) : best);
        switch (last_semi(0, None)) {
        | None => None
        | Some(j) =>
          let rec ws_end = i =>
            i < n && Focus.is_edge_ws(arr[i]) ? ws_end(i + 1) : i;
          Some(Focus.take(ws_end(j + 1), members));
        };
      };
    };
  };

  /* apply [op] to the item holding [fid] AT ITS OWNING BLOCK: a span
     whose id is exactly [fid] applies at this level; an id contained
     in a DEF span recurses into that def's tiles (module bodies, fn
     bodies); an id contained in a statement/tail span means that span
     IS the item. No cross-level fallback — an op invalid at its own
     level (move at a block edge) no-ops rather than acting on the
     enclosing item. [in_module]: the block is a module body, so
     inserted/duplicated skeletons are 2-shard MEMBERS parsed at Mod
     root, not `… in` forms. */
  let apply_at =
      (
        op: OutlineSidebar.def_op,
        ~in_module: bool,
        spans: array(Focus.item_span),
        j: int,
        seg: Segment.t,
      )
      : option((Segment.t, option(Id.t))) => {
    let n = Array.length(spans);
    let start_of = j => spans[j].Focus.sp_start;
    let end_of = j => spans[j].Focus.sp_stop;
    let movable = j => spans[j].Focus.sp_kind != Focus.ITail;
    /* member-fn bodies FLATTEN into the module-body level (a fun's
       body is siblings, not a child), so a module-level span can be a
       let-in belonging to a member's inner chain. The op FORM follows
       the target span's own head: an `…in`-headed span takes let-in
       forms even inside a module; moves must not mix the two families
       (swapping a nested let with its enclosing member head would
       cross block levels). */
    let arr = Array.of_list(seg);
    let span_in_tile = j => {
      let rec first_tile = i =>
        i >= end_of(j)
          ? None
          : (
            switch (arr[i]) {
            | Piece.Tile(t) => Some(t)
            | _ => first_tile(i + 1)
            }
          );
      switch (first_tile(start_of(j))) {
      | Some(t) => Focus.ends_with_in(t)
      | None => false
      };
    };
    let member_form = j => in_module && !span_in_tile(j);
    let same_family = (j, k) => span_in_tile(j) == span_in_tile(k);
    Focus.(
      switch (op) {
      | Delete when movable(j) =>
        Some((take(start_of(j), seg) @ drop(end_of(j), seg), None))
      | Delete => None
      | MoveUp
          when
            j > 0 && movable(j) && movable(j - 1) && same_family(j, j - 1) =>
        let (a, b, c) = (start_of(j - 1), start_of(j), end_of(j));
        Some((
          take(a, seg) @ slice(b, c, seg) @ slice(a, b, seg) @ drop(c, seg),
          None,
        ));
      | MoveDown
          when
            j
            + 1 < n
            && movable(j)
            && movable(j + 1)
            && same_family(j, j + 1) =>
        let (a, b, c) = (start_of(j), start_of(j + 1), end_of(j + 1));
        Some((
          take(a, seg) @ slice(b, c, seg) @ slice(a, b, seg) @ drop(c, seg),
          None,
        ));
      | MoveUp
      | MoveDown => None
      | NewBelow
      | NewTypeBelow
      | NewModuleBelow =>
        let sk =
          if (member_form(j)) {
            let txt =
              switch (op) {
              | NewTypeBelow => "type NewType = ?"
              | NewModuleBelow => "module NewModule = {let member = ?}"
              | _ => "let new_def = ?"
              };
            member_chunk(txt);
          } else {
            /* a bare `let _ = _ in` is not a complete program: parse
               with a dummy tail, then drop the trailing tail tile */
            let strip_tail = (sk: Segment.t): Segment.t =>
              switch (List.rev(sk)) {
              | [Piece.Tile(_), ...rest] => List.rev(rest)
              | _ => sk
              };
            let txt =
              switch (op) {
              | NewTypeBelow => "type NewType = ? in\n0"
              | NewModuleBelow => "module NewModule = {let member = ?} in\n0"
              | _ => "let new_def = ? in\n0"
              };
            Option.map(strip_tail, parse(txt));
          };
        switch (sk) {
        | None => None
        | Some(sk) =>
          /* inserting below the trailing expression would strand it
             above the new def: insert ABOVE the tail instead */
          let at = movable(j) ? end_of(j) : start_of(j);
          Some((take(at, seg) @ sk @ drop(at, seg), first_tile_id(sk)));
        };
      | Duplicate when movable(j) =>
        let span = slice(start_of(j), end_of(j), seg);
        let txt = MarkerParse.to_text(Zipper.unzip(span));
        switch (member_form(j) ? member_chunk(txt) : parse(txt)) {
        | None => None
        | Some(copy) =>
          let at = end_of(j);
          Some((
            take(at, seg) @ copy @ drop(at, seg),
            first_tile_id(copy),
          ));
        };
      | Duplicate => None
      }
    );
  };

  /* where a level sits: module members live under a BRACE tile inside
     the module tile's def child, so "is my parent a module" is two
     hops away — thread it */
  type block_ctx =
    | BPlain
    | BModDef /* the module tile's def child: the brace lives here */
    | BModBody; /* the brace's child: the member list */

  let rec apply_deep =
          (
            op: OutlineSidebar.def_op,
            fid: Id.t,
            ~bctx: block_ctx,
            ~top: bool,
            seg: Segment.t,
          )
          : option((Segment.t, option(Id.t))) => {
    let spans =
      Array.of_list(Focus.item_spans(~divided_only_tail=!top, seg));
    let n = Array.length(spans);
    let find = pred => {
      let rec go = j =>
        j >= n ? None : pred(spans[j]) ? Some(j) : go(j + 1);
      go(0);
    };
    let in_module = bctx == BModBody;
    switch (find((sp: Focus.item_span) => sp.sp_id == Some(fid))) {
    | Some(j) => apply_at(op, ~in_module, spans, j, seg)
    | None =>
      /* descend into tile children first (the owning block may be a
         module or fn body) */
      let is_module_tile = (t: Base.tile) =>
        switch (t.label) {
        | ["module", ..._] => true
        | _ => false
        };
      let child_bctx = (t: Base.tile, is_last: bool): block_ctx =>
        if (is_module_tile(t) && is_last) {
          BModDef;
        } else if (bctx == BModDef
                   && t.label == ["{", "}"]
                   && List.length(t.children) == 1) {
          BModBody;
        } else {
          BPlain;
        };
      let rec try_children =
              (ps: Segment.t): option((Segment.t, option(Id.t))) =>
        switch (ps) {
        | [] => None
        | [Piece.Tile(t), ...rest] =>
          let n_kids = List.length(t.children);
          let rec try_kids = (before, k, kids) =>
            switch (kids) {
            | [] => None
            | [ch, ...more] =>
              switch (
                apply_deep(
                  op,
                  fid,
                  ~bctx=child_bctx(t, k == n_kids - 1),
                  ~top=false,
                  ch,
                )
              ) {
              | Some((ch', target)) =>
                Some((List.rev(before) @ [ch', ...more], target))
              | None => try_kids([ch, ...before], k + 1, more)
              }
            };
          switch (try_kids([], 0, t.children)) {
          | Some((children, target)) =>
            Some((
              [
                Piece.Tile({
                  ...t,
                  children,
                }),
                ...rest,
              ],
              target,
            ))
          | None =>
            try_children(rest)
            |> Option.map(((rest', target)) =>
                 ([Piece.Tile(t), ...rest'], target)
               )
          };
        | [p, ...rest] =>
          try_children(rest)
          |> Option.map(((rest', target)) => ([p, ...rest'], target))
        };
      switch (try_children(seg)) {
      | Some(_) as r => r
      | None =>
        /* contained in one of THIS level's statement/tail spans (e.g.
           a ModExp test's row id is the inner test term): that span
           is the item */
        switch (
          find((sp: Focus.item_span) =>
            Focus.seg_contains_id(
              fid,
              Focus.slice(sp.sp_start, sp.sp_stop, seg),
            )
          )
        ) {
        | Some(j) => apply_at(op, ~in_module, spans, j, seg)
        | None => None
        }
      };
    };
  };

  let apply =
      (op: OutlineSidebar.def_op, fid: Id.t, seg: Segment.t)
      : option((Segment.t, option(Id.t))) =>
    apply_deep(op, fid, ~bctx=BPlain, ~top=true, seg);
};

/* the outline's ids in document order — the stack mirrors this order */
let outline_order = (term: Language.Exp.t): list(Haz3lcore.Id.t) => {
  let rec flatten = (acc, ns: list(OutlineTree.node)) =>
    List.fold_left(
      (acc, n: OutlineTree.node) =>
        flatten(
          switch (n.o_id) {
          | Some(id) => [id, ...acc]
          | None => acc
          },
          n.o_children,
        ),
      acc,
      ns,
    );
  List.rev(flatten([], OutlineTree.of_term(term)));
};

/* where a cell for [fid] goes (or sits) in the stack: entries keep
   PROGRAM order, not click order */
let stack_position =
    (~term, fid: Haz3lcore.Id.t, entries: list(Model.stack_entry)): int => {
  let rec index_of = (k, l: list(Model.stack_entry)) =>
    switch (l) {
    | [] => None
    | [e, ..._] when e.e_id == fid => Some(k)
    | [_, ...rest] => index_of(k + 1, rest)
    };
  switch (index_of(0, entries)) {
  | Some(j) => j
  | None =>
    let order = outline_order(term);
    let rank = id => {
      let rec go = (k, l) =>
        switch (l) {
        | [] => max_int
        | [x, ..._] when x == id => k
        | [_, ...rest] => go(k + 1, rest)
        };
      go(0, order);
    };
    let r = rank(fid);
    List.length(
      List.filter((e: Model.stack_entry) => rank(e.e_id) < r, entries),
    );
  };
};

let insert_entry =
    (~term, entry: Model.stack_entry, entries: list(Model.stack_entry))
    : list(Model.stack_entry) => {
  let pos = stack_position(~term, entry.e_id, entries);
  let rec ins = (k, es) =>
    k == 0
      ? [entry, ...es]
      : (
        switch (es) {
        | [] => [entry]
        | [e, ...rest] => [e, ...ins(k - 1, rest)]
        }
      );
  ins(pos, entries);
};

module Persist = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type slide_meta = {
    current: int,
    names: list(string),
  };

  let meta_key = (prefix: string): string => prefix ++ ":_meta";
  let slide_key = (prefix: string, name: string): string =>
    prefix ++ ":" ++ name;
  let agent_key = (prefix: string, name: string): string =>
    prefix ++ ":" ++ name ++ ":agent";
  let caret_key = (prefix: string, name: string): string =>
    prefix ++ ":" ++ name ++ ":caret";
  let pins_key = (prefix: string, name: string): string =>
    prefix ++ ":" ++ name ++ ":pins";
  let collapse_key = (prefix: string, name: string): string =>
    prefix ++ ":" ++ name ++ ":collapse";

  /* set when a loaded slide has a saved caret; the next calculate
     schedules the Move(Point) (measured exists by then) */
  let pending_caret: ref(option(Point.t)) = ref(None);

  /* saved pins are NAME-anchored (text-backed persistence re-mints
     ids on every load): one line per pin, "0|1 <outline/label/path>";
     resolved against the loaded slide's outline on RestorePins */
  let pending_pins: ref(option(list((list(string), bool)))) = ref(None);

  let read_pins = (prefix: string, name: string): unit =>
    switch (HazelDB.kv_get(pins_key(prefix, name))) {
    | Some(txt) =>
      let pins =
        String.split_on_char('\n', txt)
        |> List.filter_map(line =>
             switch (String.split_on_char(' ', String.trim(line))) {
             | [flag, path] when path != "" =>
               Some((String.split_on_char('/', path), flag == "1"))
             | _ => None
             }
           );
      pending_pins := pins == [] ? None : Some(pins);
    | None => pending_pins := None
    };

  let read_collapse = (prefix: string, name: string): unit =>
    switch (HazelDB.kv_get(collapse_key(prefix, name))) {
    | Some(txt) =>
      let paths =
        String.split_on_char('\n', txt)
        |> List.filter_map(line => {
             let line = String.trim(line);
             line == "" ? None : Some(String.split_on_char('/', line));
           });
      paths == []
        ? Hashtbl.remove(slide_collapse, name)
        : Hashtbl.replace(slide_collapse, name, paths);
    | None => ()
    };

  let write_collapse = (prefix: string, name: string): unit =>
    HazelDB.kv_save(
      collapse_key(prefix, name),
      collapse_paths(name)
      |> List.map(String.concat("/"))
      |> String.concat("\n"),
    );

  let write_pins =
      (prefix: string, name: string, pins: list((list(string), bool)))
      : unit =>
    HazelDB.kv_save(
      pins_key(prefix, name),
      pins
      |> List.map(((path, run)) =>
           (run ? "1 " : "0 ") ++ String.concat("/", path)
         )
      |> String.concat("\n"),
    );

  let read_caret = (prefix: string, name: string): unit =>
    switch (HazelDB.kv_get(caret_key(prefix, name))) {
    | Some(txt) =>
      switch (String.split_on_char(' ', String.trim(txt))) {
      | [r, c] =>
        switch (int_of_string_opt(r), int_of_string_opt(c)) {
        | (Some(row), Some(col)) =>
          pending_caret :=
            Some(
              Point.{
                row,
                col,
              },
            )
        | _ => ()
        }
      | _ => ()
      }
    | None => ()
    };

  let save_meta = (prefix: string, m: slide_meta): unit => {
    let key = meta_key(prefix);
    let serialized = m |> sexp_of_slide_meta |> Sexplib.Sexp.to_string;
    HazelDB.kv_save(key, serialized);
  };

  let load_meta = (prefix: string): option(slide_meta) =>
    switch (HazelDB.kv_get(meta_key(prefix))) {
    | Some(data) =>
      try(Some(data |> Sexplib.Sexp.of_string |> slide_meta_of_sexp)) {
      | _ => None
      }
    | None => None
    };

  let save_slide_kind =
      (prefix: string, name: string, kind: Scratchpad.kind_persistent): unit => {
    let key = slide_key(prefix, name);
    let serialized =
      kind |> Scratchpad.sexp_of_kind_persistent |> Sexplib.Sexp.to_string;
    HazelDB.kv_save(key, serialized);
  };

  /* Load a slide blob. Tries the new schema first; on parse failure,
     falls back to legacy CellEditor-only blobs and wraps them as a Code kind. */
  let load_slide_kind =
      (prefix: string, name: string): option(Scratchpad.kind_persistent) =>
    switch (HazelDB.kv_get(slide_key(prefix, name))) {
    | None => None
    | Some(data) =>
      let sexp = Sexplib.Sexp.of_string(data);
      switch (Scratchpad.kind_persistent_of_sexp(sexp)) {
      | k => Some(k)
      | exception _ =>
        switch (CellEditor.Model.persistent_of_sexp(sexp)) {
        | e =>
          Some(
            Scratchpad.CodePersist({
              editor: Some(e),
              agent: Agent.Persistent.persist(Agent.Utils.init()),
            }),
          )
        | exception _ => None
        }
      };
    };

  let save_agent =
      (prefix: string, name: string, agent: Agent.Persistent.t): unit => {
    let key = agent_key(prefix, name);
    let serialized =
      agent |> Agent.Persistent.sexp_of_t |> Sexplib.Sexp.to_string;
    HazelDB.kv_save(key, serialized);
  };

  let load_agent = (prefix: string, name: string): option(Agent.Persistent.t) =>
    switch (HazelDB.kv_get(agent_key(prefix, name))) {
    | Some(data) =>
      try(Some(data |> Sexplib.Sexp.of_string |> Agent.Persistent.t_of_sexp)) {
      | _ => None
      }
    | None => None
    };

  /* Change-gate for agent saves: serializing a long conversation on
     every editor autosave is the expensive part, so skip when the agent
     model is physically unchanged (edits rebuild the scratchpad record
     but reuse the agent field). */
  let last_saved_agent: Hashtbl.t(string, Agent.Model.t) = Hashtbl.create(8);

  /* Same gate for the EDITOR blob: the 1Hz autosave re-serialized the
     whole program (splice + to_text, ~0.7s at 1k) even while idle.
     Content identity = the unstacked master zipper, or the live focus
     record (any cell edit — including caret moves, which the caret
     side key wants — rebuilds them). */
  type save_stamp =
    | Unstacked(Zipper.t)
    | Stacked(Model.focus_t);
  let last_saved_content: Hashtbl.t(string, save_stamp) = Hashtbl.create(8);
  let stamp_equal = (a: save_stamp, b: save_stamp): bool =>
    switch (a, b) {
    | (Unstacked(x), Unstacked(y)) => x === y
    | (Stacked(x), Stacked(y)) => x === y
    | _ => false
    };

  /* the scratchpad persistence should see: the master with any live
     focus-cell edits spliced in — never the bare focus cell. But it
     must NOT build a live editor for the spliced program: cell_of_seg
     pays CachedSyntax.init (MakeTerm + Measured) and Zipper.sexp_of_t
     re-serializes the whole zipper — measured at ~2s + ~2.5s PER
     AUTOSAVE TICK on Mega 1k. The spliced zipper's caret is synthetic
     anyway (the live caret is in a stack cell), so snapshot as
     TEXT-backed persistence — the same lossless path committed .hz
     slides load through. */
  let persist_spliced =
      (f: Model.focus_t, editor: CellEditor.Model.t)
      : CellEditor.Model.persistent => {
    let z = Focus.splice_all(f) |> Zipper.unzip;
    CellEditor.Model.{
      editor:
        Editor.Model.mk_persistent(
          PersistentZipper.of_text(PersistentZipper.to_string(z) ++ "\n"),
          ~root=Sort.Exp,
        ),
      result: EvalResult.Model.persist(editor.result),
    };
  };

  let save_current = (prefix: string, model: Model.t): unit => {
    let names = Model.scratchpad_names(model);
    save_meta(
      prefix,
      {
        current: model.current,
        names,
      },
    );
    let sp = List.nth(model.scratchpads, model.current);
    switch (sp.dormant, sp.kind) {
    | (true, _) => () /* never write a placeholder over the stored slide */
    | (false, Code({editor, agent})) =>
      let stamp =
        switch (model.focus) {
        | Some(f) => Stacked(f)
        | None => Unstacked(editor.editor.editor.state.zipper)
        };
      let content_key = prefix ++ ":" ++ sp.name;
      let content_unchanged =
        switch (Hashtbl.find_opt(last_saved_content, content_key)) {
        | Some(prev) => stamp_equal(prev, stamp)
        | None => false
        };
      if (!content_unchanged) {
        Hashtbl.replace(last_saved_content, content_key, stamp);
      };
      if (!content_unchanged) {
        /* UNSTACKED saves are text-backed too: Zipper.sexp_of_t costs
           ~2.5s per autosave tick at 1k lines. The caret can't ride the
           text, so it saves as a (row col) side key and restores as a
           Move(Point) after hydration. */
        switch (model.focus) {
        | Some(_) => ()
        | None =>
          let z = editor.editor.editor.state.zipper;
          switch (Zipper.Caret.point(editor.editor.editor.syntax.measured, z)) {
          | exception _ => ()
          | Point.{row, col} =>
            HazelDB.kv_save(
              caret_key(prefix, sp.name),
              string_of_int(row) ++ " " ++ string_of_int(col),
            )
          };
        };
        {
          /* pins ride a side key, name-anchored via the outline */

          let term = editor.editor.statics.term;
          let pins =
            switch (model.focus) {
            | None => []
            | Some(f) =>
              List.filter_map(
                (e: Model.stack_entry) =>
                  OutlineTree.label_path(e.e_id, term)
                  |> Option.map(path => (path, e.e_run)),
                f.f_entries,
              )
            };
          write_pins(prefix, sp.name, pins);
        };
        switch (
          switch (model.focus) {
          | Some(f) => persist_spliced(f, editor)
          | None =>
            CellEditor.Model.{
              editor:
                Editor.Model.mk_persistent(
                  PersistentZipper.of_text(
                    PersistentZipper.to_string(
                      editor.editor.editor.state.zipper,
                    )
                    ++ "\n",
                  ),
                  ~root=Sort.Exp,
                ),
              result: EvalResult.Model.persist(editor.result),
            }
          }
        ) {
        | e =>
          /* The slide blob carries the editor only; the conversation
             lives solely under the :agent key (it used to be embedded
             here TOO, doubling every write and boot deserialization). */
          save_slide_kind(
            prefix,
            sp.name,
            CodePersist({
              editor: Some(e),
              agent: Agent.Persistent.persist(Agent.Utils.init()),
            }),
          )
        };
      };
      let agent_key_str = prefix ++ ":" ++ sp.name;
      let unchanged =
        switch (Hashtbl.find_opt(last_saved_agent, agent_key_str)) {
        | Some(prev) => prev === agent
        | None => false
        };
      if (!unchanged) {
        save_agent(prefix, sp.name, Agent.Persistent.persist(agent));
        Hashtbl.replace(last_saved_agent, agent_key_str, agent);
      };
    | (false, Drv(_)) =>
      switch (Scratchpad.persist(sp).kind) {
      | DrvPersist(_) as k => save_slide_kind(prefix, sp.name, k)
      | CodePersist(_) => ()
      }
    };
  };

  let load_scratchpad =
      (~settings, prefix: string, name: string): Scratchpad.t => {
    read_caret(prefix, name);
    read_pins(prefix, name);
    read_collapse(prefix, name);
    switch (load_slide_kind(prefix, name)) {
    | Some(CodePersist({editor: e, agent})) =>
      let agent =
        switch (load_agent(prefix, name)) {
        | Some(p) => p
        | None => agent
        };
      Scratchpad.{
        name,
        kind:
          Code({
            editor:
              (
                switch (e) {
                | Some(e) => e
                | None => Init.default_documentation_slide_name(name)
                }
              )
              |> CellEditor.Model.unpersist(~settings),
            agent: Agent.Persistent.unpersist(agent),
          }),
        dormant: false,
      };
    | Some(DrvPersist(p)) =>
      Scratchpad.{
        name,
        kind:
          Drv(
            DerivationExerciseMode.Model.unpersist(
              ~settings,
              ~instructor_mode=false,
              p,
              DerivationExercise.blank_spec(~title=name, ~module_name=name),
            ),
          ),
        dormant: false,
      }
    | None =>
      /* No persisted data for this slide. If the name matches a Drv
         documentation slide, seed it as a derivation scratchpad from the
         registered spec. Otherwise fall back to a code slide (either the
         named documentation slide, or an empty code scratchpad). */
      switch (Init.find_documentation_drv_spec(name)) {
      | Some(spec) =>
        Scratchpad.{
          name,
          kind:
            Drv(
              DerivationExerciseMode.Model.of_spec(
                ~settings,
                ~instructor_mode=false,
                spec,
              ),
            ),
          dormant: false,
        }
      | None =>
        let agent =
          switch (load_agent(prefix, name)) {
          | Some(p) => Agent.Persistent.unpersist(p)
          | None => Agent.Utils.init()
          };
        Scratchpad.{
          name,
          kind:
            Code({
              editor:
                Init.default_documentation_slide_name(name)
                |> CellEditor.Model.unpersist(~settings),
              agent,
            }),
          dormant: false,
        };
      }
    };
  };

  let load_all =
      (
        prefix: string,
        ~settings,
        ~default_names: list(string),
        ~default_current: int,
      )
      : Model.t => {
    let (current, names) =
      switch (load_meta(prefix)) {
      | Some(meta) => (meta.current, meta.names)
      | None => (default_current, default_names)
      };
    Model.{
      current,
      scratchpads:
        List.mapi(
          (i, name) =>
            i == current
              ? load_scratchpad(~settings, prefix, name)
              : Scratchpad.dormant_code(name),
          names,
        ),
      focus: None,
    };
  };

  /* Swap the placeholder at [current] for the real slide, if dormant. */
  let hydrate_current = (~settings, prefix: string, model: Model.t): Model.t => {
    let sp = List.nth(model.scratchpads, model.current);
    if (sp.dormant) {
      {
        ...model,
        scratchpads:
          Util.ListUtil.put_nth(
            model.current,
            load_scratchpad(~settings, prefix, sp.name),
            model.scratchpads,
          ),
      };
    } else {
      model;
    };
  };

  /* Serialize all slides into the monolithic export format. */
  let export_all =
      (prefix: string, ~default_names: list(string), ~default_current: int)
      : string => {
    let (current, names) =
      switch (load_meta(prefix)) {
      | Some(meta) => (meta.current, meta.names)
      | None => (default_current, default_names)
      };
    let scratchpads: list(Scratchpad.persistent) =
      List.map(
        name =>
          switch (load_slide_kind(prefix, name)) {
          | Some(CodePersist({editor, agent})) =>
            let agent =
              switch (load_agent(prefix, name)) {
              | Some(a) => a
              | None => agent
              };
            Scratchpad.{
              name,
              kind:
                CodePersist({
                  editor,
                  agent,
                }),
            };
          | Some(DrvPersist(_) as k) =>
            Scratchpad.{
              name,
              kind: k,
            }
          | None =>
            let agent =
              switch (load_agent(prefix, name)) {
              | Some(a) => a
              | None => Agent.Persistent.persist(Agent.Utils.init())
              };
            Scratchpad.{
              name,
              kind:
                CodePersist({
                  editor: None,
                  agent,
                }),
            };
          },
        names,
      );
    let persistent: Model.persistent = (current, scratchpads);
    persistent |> Model.sexp_of_persistent |> Sexplib.Sexp.to_string;
  };

  /* Deserialize monolithic export format and distribute to per-slide keys. */
  let import_all = (prefix: string, data: string): unit =>
    try({
      let persistent: Model.persistent =
        data |> Sexplib.Sexp.of_string |> Model.persistent_of_sexp;
      let (current, scratchpads) = persistent;
      let names =
        List.map((sp: Scratchpad.persistent) => sp.name, scratchpads);
      save_meta(
        prefix,
        {
          current,
          names,
        },
      );
      List.iter(
        (sp: Scratchpad.persistent) =>
          switch (sp.kind) {
          | CodePersist({editor, agent}) =>
            switch (editor) {
            | Some(_) =>
              save_slide_kind(
                prefix,
                sp.name,
                CodePersist({
                  editor,
                  agent,
                }),
              )
            | None => ()
            };
            save_agent(prefix, sp.name, agent);
          | DrvPersist(_) as k => save_slide_kind(prefix, sp.name, k)
          },
        scratchpads,
      );
    }) {
    | _ => print_endline("ScratchMode.Persist.import_all: error")
    };
};

let integrate_share =
    (~settings: Language.CoreSettings.t, model: Model.t): Model.t => {
  let share_name =
    switch (JsUtil.QueryParams.get_param("name")) {
    | None => "Unknown Share"
    | Some(name) => name
    };
  switch (JsUtil.QueryParams.get_param("share")) {
  | None => model
  | Some(data) =>
    let shared_text = data |> StringUtil.decompress;
    /* zipper: "" = the intentional text path (share links carry only
       text); a non-empty sentinel would take the sexp arm and print the
       stale-serialization warning on every share-link load */
    let shared: PersistentZipper.t = {
      zipper: "",
      backup_text: shared_text,
    };
    let shared: CellEditor.Model.persistent = {
      editor: {
        root: Exp,
        zipper: shared,
      },
      result: EvalResult.Model.init |> EvalResult.Model.persist,
    };
    let new_sp =
      Scratchpad.mk_code(
        ~name=share_name,
        ~editor=CellEditor.Model.unpersist(~settings, shared),
        (),
      );
    Model.{
      current: List.length(model.scratchpads),
      scratchpads: model.scratchpads @ [new_sp],
      focus: None,
    };
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
    | StackHeader(int, CellEditor.Update.t)
    | StackBody(int, CellEditor.Update.t)
    | FocusDef(Haz3lcore.Id.t) /* replace the stack with this one def */
    | FocusToggle(Haz3lcore.Id.t) /* add/remove a def in the stack */
    | FocusToggleRun(Haz3lcore.Id.t) /* one cell for a whole test run */
    | RestorePins /* deferred per-slide pin restore after slide load */
    | OutlineCollapse(list(string)) /* toggle a branch's collapse */
    | FocusEnsure(Haz3lcore.Id.t) /* add if absent (cross-cell jump) */
    | RestoreCaret(Point.t) /* deferred caret restore after slide load */
    | OutlineMenu(option((Haz3lcore.Id.t, float, float)))
    | OutlineDefOp(OutlineSidebar.def_op, Haz3lcore.Id.t)
    | UnfocusDef
    | RefreshStatics
    | HydrateCurrent /* deferred slide hydration (SwitchSlide shows a
                        loading frame first) */
    | AgentAction(Agent.Update.Action.t)
    | DrvAction(DerivationExerciseMode.Update.t)
    | SwitchSlide(int)
    | ResetCurrent
    | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImportScratchpad(option(string))
    | Export
    | Encode
    | AddSlide
    | AddDrvSlide
    | RenameSlide
    | DeleteSlide;

  /* splice any live focus back and clear it — MUST run before any
     operation that changes which slide [current] denotes, else a later
     unfocus would splice into the wrong slide */
  let commit_focus = (model: Model.t): Model.t =>
    switch (model.focus) {
    | None => model
    | Some(f) => {
        ...model,
        scratchpads:
          ListUtil.put_nth(
            model.current,
            Focus.spliced_master(
              f,
              List.nth(model.scratchpads, model.current),
            ),
            model.scratchpads,
          ),
        focus: None,
      }
    };

  let export_scratch_slide = (model: Model.t): unit => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    switch (scratchpad.kind) {
    | Code({editor, _}) =>
      let persistent = CellEditor.Model.persist(editor);
      let data =
        persistent
        |> CellEditor.Model.sexp_of_persistent
        |> Sexplib.Sexp.to_string;
      let current_name = scratchpad.name;
      let filename = current_name |> StringUtil.sanitize_filename;
      JsUtil.download_string_file(
        ~filename,
        ~content_type="text/plain",
        ~contents=data,
      );
    | Drv(_) => ()
    };
  };

  let encode_scratch_slide = (model: Model.t): unit => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    JsUtil.QueryParams.set_param("name", scratchpad.name);
    switch (scratchpad.kind) {
    | Code({editor, _}) =>
      let c = editor |> CellEditor.Model.to_string;
      JsUtil.QueryParams.set_param("share", StringUtil.compress(c));
    | Drv(_) => ()
    };
  };
  let rec prompt_slide_name =
          (
            ~error: option(string)=?,
            ~existing_scratchpads: Seq.t(string),
            default: string,
          )
          : Option.t(string) => {
    let new_name =
      JsUtil.prompt(
        (
          switch (error) {
          | Some(e) => e ++ "\n"
          | None => ""
          }
        )
        ++ "Enter new slide name:",
        default,
      );

    if (existing_scratchpads |> Seq.exists(name => Some(name) == new_name)) {
      prompt_slide_name(
        ~error="Slide name already exists. Please choose a different name.",
        ~existing_scratchpads,
        Option.value(~default, new_name),
      );
    } else {
      new_name;
    };
  };

  /* Kind of scratchpad to create. Code is the default ("Scratchpad N");
     Drv creates a blank derivation slide with the same auto-naming scheme. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type new_slide_kind =
    | NewCode
    | NewDrv;

  let add_new_slide =
      (
        ~kind: new_slide_kind,
        ~settings: Language.CoreSettings.t,
        model: Model.t,
        is_documentation: bool,
      )
      : Model.t => {
    let blank = name =>
      switch (kind) {
      | NewCode => Scratchpad.blank_code(name)
      | NewDrv => Scratchpad.blank_drv(~settings, name)
      };
    let add_empty_slide = (name): Model.t => {
      current: List.length(model.scratchpads),
      scratchpads: model.scratchpads @ [blank(name)],
      focus: None,
    };
    switch (is_documentation) {
    | false =>
      let prefix =
        switch (kind) {
        | NewCode => "Scratchpad"
        | NewDrv => "Derivation"
        };
      let used_numbers =
        model.scratchpads
        |> List.filter_map((s: Scratchpad.t) => {
             switch (String.split_on_char(' ', s.name)) {
             | [p, num] when p == prefix => int_of_string_opt(num)
             | _ => None
             }
           });
      let unused_ids =
        Seq.filter(i => !List.mem(i, used_numbers), Seq.ints(1));
      let new_number =
        Seq.uncons(unused_ids)
        |> Option.get  // This is safe because unused_ids is infinite
        |> fst;

      add_empty_slide(prefix ++ " " ++ string_of_int(new_number));
    | true =>
      let new_name =
        prompt_slide_name(
          ~existing_scratchpads=
            model.scratchpads
            |> List.to_seq
            |> Seq.map((s: Scratchpad.t) => s.name),
          "New Slide Name",
        );
      switch (new_name) {
      | None => model // Prompt cancelled so no new scratchpad created
      | Some(name) => add_empty_slide(name)
      };
    };
  };

  let update =
      (
        ~schedule_action,
        ~settings: Settings.t,
        ~is_documentation: bool,
        action,
        model: Model.t,
      ) => {
    switch (action) {
    | AgentAction(a) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, agent}) =>
        let schedule_agent = (a: Agent.Update.Action.t) =>
          schedule_action(AgentAction(a));
        let (new_agent, updated_editor) =
          Agent.Update.update(a, agent, editor, settings, schedule_agent);
        let* new_ed = updated_editor;
        let new_sp =
          ListUtil.put_nth(
            model.current,
            {
              ...scratchpad,
              kind:
                Code({
                  editor: new_ed,
                  agent: new_agent,
                }),
            },
            model.scratchpads,
          );
        {
          ...model,
          scratchpads: new_sp,
        };
      | Drv(_) => model |> return_quiet
      };
    | FocusDef(fid) =>
      /* replace the whole stack with this one definition */
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, _}) =>
        let master_seg =
          switch (model.focus) {
          /* stack already open: splice its entries home first */
          | Some(f) => Focus.splice_all(f)
          | None => Focus.zip_of_cell(editor)
          };
        let info_map = editor.editor.statics.info_map;
        switch (
          Focus.mk_entry(
            ~info_map,
            ~sym=?outline_sym(fid, editor.editor.statics.term),
            fid,
            master_seg,
          )
        ) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          {
            ...model,
            focus:
              Some(
                Model.{
                  f_entries: [entry],
                  f_master_seg: master_seg,
                },
              ),
          }
          |> Updated.return
        };
      | Drv(_) => model |> Updated.return_quiet
      };
    | FocusToggle(fid) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, _}) =>
        switch (model.focus) {
        | None =>
          /* no stack yet: same as single focus */
          let master_seg = Focus.zip_of_cell(editor);
          let info_map = editor.editor.statics.info_map;
          switch (
            Focus.mk_entry(
              ~info_map,
              ~sym=?outline_sym(fid, editor.editor.statics.term),
              fid,
              master_seg,
            )
          ) {
          | None => model |> Updated.return_quiet
          | Some(entry) =>
            {
              ...model,
              focus:
                Some(
                  Model.{
                    f_entries: [entry],
                    f_master_seg: master_seg,
                  },
                ),
            }
            |> Updated.return
          };
        | Some(f) =>
          if (List.exists(
                (e: Model.stack_entry) => e.e_id == fid,
                f.f_entries,
              )) {
            /* remove: splice that entry home; empty stack = unfocus */
            let closing =
              List.find(
                (e: Model.stack_entry) => e.e_id == fid,
                f.f_entries,
              );
            let master_seg = Focus.splice_entry(closing, f.f_master_seg);
            let rest =
              List.filter(
                (e: Model.stack_entry) => e.e_id != fid,
                f.f_entries,
              );
            switch (rest) {
            | [] =>
              let restored =
                Focus.spliced_master(
                  Model.{
                    f_entries: [],
                    f_master_seg: master_seg,
                  },
                  scratchpad,
                );
              {
                ...model,
                scratchpads:
                  ListUtil.put_nth(
                    model.current,
                    restored,
                    model.scratchpads,
                  ),
                focus: None,
              }
              |> Updated.return;
            | _ =>
              {
                ...model,
                focus:
                  Some(
                    Model.{
                      f_entries: rest,
                      f_master_seg: master_seg,
                    },
                  ),
              }
              |> Updated.return
            };
          } else if (List.exists(
                       (e: Model.stack_entry) =>
                         e.e_run && List.mem(fid, e.e_members),
                       f.f_entries,
                     )) {
            /* the id lives inside an OPEN run cell: its ⊖ closes the
               run (the row reads as pinned because the run covers it) */
            schedule_action(FocusToggleRun(fid));
            model |> Updated.return_quiet;
          } else {
            /* add to the stack, keeping program order. Pinning a
               PARENT (module/fn) first splices its pinned descendants
               home and unpins them — the parent's cell holds their
               content (andrew: parent-pin unpins children). */
            let info_map = editor.editor.statics.info_map;
            let term = editor.editor.statics.term;
            let desc = OutlineTree.descendant_ids(fid, term);
            let (closing, keeping) =
              List.partition(
                (e: Model.stack_entry) => List.mem(e.e_id, desc),
                f.f_entries,
              );
            let master_seg =
              List.fold_left(
                (seg, e) => Focus.splice_entry(e, seg),
                f.f_master_seg,
                closing,
              );
            switch (
              Focus.mk_entry(
                ~info_map,
                ~sym=?outline_sym(fid, editor.editor.statics.term),
                fid,
                master_seg,
              )
            ) {
            | None => model |> Updated.return_quiet
            | Some(entry) =>
              {
                ...model,
                focus:
                  Some(
                    Model.{
                      f_entries: insert_entry(~term, entry, keeping),
                      f_master_seg: master_seg,
                    },
                  ),
              }
              |> Updated.return
            };
          }
        }
      | Drv(_) => model |> Updated.return_quiet
      };
    | FocusToggleRun(fid) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Drv(_) => model |> Updated.return_quiet
      | Code({editor, _}) =>
        let info_map = editor.editor.statics.info_map;
        let unfocus_with = (master_seg: Haz3lcore.Segment.t) => {
          let restored =
            Focus.spliced_master(
              Model.{
                f_entries: [],
                f_master_seg: master_seg,
              },
              scratchpad,
            );
          {
            ...model,
            scratchpads:
              ListUtil.put_nth(model.current, restored, model.scratchpads),
            focus: None,
          }
          |> Updated.return;
        };
        switch (model.focus) {
        | None =>
          let master_seg = Focus.zip_of_cell(editor);
          switch (Focus.mk_run_entry(~info_map, fid, master_seg)) {
          | None => model |> Updated.return_quiet
          | Some(entry) =>
            {
              ...model,
              focus:
                Some(
                  Model.{
                    f_entries: [entry],
                    f_master_seg: master_seg,
                  },
                ),
            }
            |> Updated.return
          };
        | Some(f) =>
          let covering =
            List.find_opt(
              (e: Model.stack_entry) =>
                e.e_run && (e.e_id == fid || List.mem(fid, e.e_members)),
              f.f_entries,
            );
          switch (covering) {
          | Some(run) =>
            /* toggle OFF: splice the run cell home */
            let master_seg = Focus.splice_entry(run, f.f_master_seg);
            let rest =
              List.filter(
                (e: Model.stack_entry) => !(e === run),
                f.f_entries,
              );
            rest == []
              ? unfocus_with(master_seg)
              : {
                  ...model,
                  focus:
                    Some(
                      Model.{
                        f_entries: rest,
                        f_master_seg: master_seg,
                      },
                    ),
                }
                |> Updated.return;
          | None =>
            let members =
              switch (Focus.test_run(fid, f.f_master_seg)) {
              | Some((_, _, ms)) => ms
              | None => [fid]
              };
            let (member_entries, keeping) =
              List.partition(
                (e: Model.stack_entry) => List.mem(e.e_id, members),
                f.f_entries,
              );
            let master_seg =
              List.fold_left(
                (seg, e) => Focus.splice_entry(e, seg),
                f.f_master_seg,
                member_entries,
              );
            let all_open =
              members != []
              && List.length(member_entries) == List.length(members);
            if (all_open) {
              /* the container's ⊖ with every test open individually:
                 close them all */
              keeping == []
                ? unfocus_with(master_seg)
                : {
                    ...model,
                    focus:
                      Some(
                        Model.{
                          f_entries: keeping,
                          f_master_seg: master_seg,
                        },
                      ),
                  }
                  |> Updated.return;
            } else {
              switch (Focus.mk_run_entry(~info_map, fid, master_seg)) {
              | None => model |> Updated.return_quiet
              | Some(entry) =>
                {
                  ...model,
                  focus:
                    Some(
                      Model.{
                        f_entries:
                          insert_entry(
                            ~term=editor.editor.statics.term,
                            entry,
                            keeping,
                          ),
                        f_master_seg: master_seg,
                      },
                    ),
                }
                |> Updated.return
              };
            };
          };
        };
      };
    | RestorePins =>
      switch (Persist.pending_pins^) {
      | None => model |> Updated.return_quiet
      | Some(pins) =>
        let scratchpad = List.nth(model.scratchpads, model.current);
        switch (scratchpad.kind) {
        | Code({editor, _})
            when
              List.exists(
                (n: OutlineTree.node) => n.o_label != "",
                OutlineTree.of_term(editor.editor.statics.term),
              ) =>
          Persist.pending_pins := None;
          let term = editor.editor.statics.term;
          List.iter(
            ((path, run)) =>
              switch (OutlineTree.resolve_path(path, term)) {
              | Some(id) =>
                schedule_action(run ? FocusToggleRun(id) : FocusToggle(id))
              | None => ()
              },
            pins,
          );
          model |> Updated.return_quiet;
        | _ => model |> Updated.return_quiet /* statics not ready: retry */
        };
      }
    | FocusEnsure(fid) =>
      /* cross-cell jump support: add [fid] to the stack iff absent
         (never removes; requires an open stack — the master handles
         its own jumps) */
      switch (model.focus) {
      | None => model |> Updated.return_quiet
      | Some(f) =>
        if (List.exists((e: Model.stack_entry) => e.e_id == fid, f.f_entries)) {
          model |> Updated.return_quiet;
        } else {
          let scratchpad = List.nth(model.scratchpads, model.current);
          switch (scratchpad.kind) {
          | Drv(_) => model |> Updated.return_quiet
          | Code({editor, _}) =>
            let info_map = editor.editor.statics.info_map;
            switch (
              Focus.mk_entry(
                ~info_map,
                ~sym=?outline_sym(fid, editor.editor.statics.term),
                fid,
                f.f_master_seg,
              )
            ) {
            | None => model |> Updated.return_quiet
            | Some(entry) =>
              {
                ...model,
                focus:
                  Some(
                    Model.{
                      ...f,
                      f_entries:
                        insert_entry(
                          ~term=editor.editor.statics.term,
                          entry,
                          f.f_entries,
                        ),
                    },
                  ),
              }
              |> Updated.return
            };
          };
        }
      }
    | RestoreCaret(p) =>
      /* clearing here (not at schedule time) makes delivery robust:
         the boot-time calculate runs with a no-op scheduler, so the
         ref keeps re-scheduling until a real action loop picks it up */
      Persist.pending_caret := None;
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, agent}) =>
        let* new_ed =
          CellEditor.Update.update(
            ~settings,
            MainEditor(Perform(Move(Point(p, None)))),
            editor,
          );
        {
          ...model,
          scratchpads:
            ListUtil.put_nth(
              model.current,
              {
                ...scratchpad,
                kind:
                  Code({
                    editor: new_ed,
                    agent,
                  }),
              },
              model.scratchpads,
            ),
        };
      | Drv(_) => model |> Updated.return_quiet
      };
    | OutlineMenu(m) =>
      outline_menu := m;
      model |> Updated.return_quiet;
    | OutlineCollapse(path) =>
      let name = List.nth(model.scratchpads, model.current).name;
      let cur = collapse_paths(name);
      let next =
        List.mem(path, cur)
          ? List.filter(p => p != path, cur) : [path, ...cur];
      next == []
        ? Hashtbl.remove(slide_collapse, name)
        : Hashtbl.replace(slide_collapse, name, next);
      Persist.write_collapse(is_documentation ? "doc" : "scratch", name);
      model |> Updated.return_quiet;
    | OutlineDefOp(op, fid) =>
      outline_menu := None;
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Drv(_) => model |> Updated.return_quiet
      | Code({editor, agent}) =>
        let live_seg =
          switch (model.focus) {
          | Some(f) => Focus.splice_all(f)
          | None => Focus.zip_of_cell(editor)
          };
        switch (Restructure.apply(op, fid, live_seg)) {
        | None => model |> Updated.return_quiet
        | Some((new_seg, focus_target)) =>
          /* Statics are seeded SYNCHRONOUSLY: the outline reads the
             master's statics.term, and while a stack is open the
             master's own calculate is skipped — a fresh empty statics
             would blank the outline. Probe-aware (union of master +
             open-cell zippers), so this single whole-program parse
             also serves as the stacked-statics frame: restructures
             used to Force a second parse the next frame. */
          let probe_union = (a, b) =>
            Haz3lcore.Id.Map.union((_, x, _) => Some(x), a, b);
          let entry_probes =
            switch (model.focus) {
            | None => Haz3lcore.Id.Map.empty
            | Some(f) =>
              List.fold_left(
                (acc, e: Model.stack_entry) =>
                  probe_union(
                    acc,
                    Haz3lcore.CachedStatics.probe_ids_of_zipper(
                      e.e_body.editor.editor.state.zipper,
                    ),
                  ),
                Haz3lcore.Id.Map.empty,
                f.f_entries,
              )
            };
          let probe_ids =
            probe_union(
              entry_probes,
              Haz3lcore.CachedStatics.probe_ids_of_zipper(
                editor.editor.editor.state.zipper,
              ),
            );
          let statics =
            settings.core.statics
              ? Haz3lcore.CachedStatics.init_compositional_term(
                  ~settings=settings.core,
                  ~probe_ids,
                  MakeTerm.Incr.term_of(new_seg),
                )
              : Haz3lcore.CachedStatics.empty;
          let stays_stacked =
            switch (model.focus) {
            | None => false
            | Some(f) =>
              (
                op == OutlineSidebar.Delete
                  ? List.filter(
                      (e: Model.stack_entry) => e.e_id != fid,
                      f.f_entries,
                    )
                  : f.f_entries
              )
              != []
            };
          let new_editor: CellEditor.Model.t =
            if (stays_stacked) {
              {
                /* master hidden while stacked: SKIP the whole-program
                   editor rebuild (cell_of_seg re-measures everything,
                   seconds on mega) — the zipper goes stale but every
                   consumer while stacked reads f_master_seg, and
                   unfocus rebuilds from it */

                editor: {
                  ...editor.editor,
                  statics,
                },
                result: editor.result,
              };
            } else {
              /* master (re)becomes visible — including when this op
                 deletes the LAST open cell: a stale zipper here would
                 resurrect the deleted def on the next calculate */
              let fresh = Focus.cell_of_seg(new_seg);
              {
                editor: {
                  ...fresh.editor,
                  statics,
                },
                result: editor.result,
              };
            };
          let new_sp = {
            ...scratchpad,
            kind:
              Code({
                editor: new_editor,
                agent,
              }),
          };
          /* a DELETEd definition's open cell closes with it; an empty
             stack unfocuses (the rebuilt master is already live) */
          let focus =
            switch (model.focus) {
            | None => None
            | Some(f) =>
              let entries =
                op == OutlineSidebar.Delete
                  ? List.filter(
                      (e: Model.stack_entry) => e.e_id != fid,
                      f.f_entries,
                    )
                  : f.f_entries;
              entries == []
                ? None
                : Some(
                    Model.{
                      f_entries: entries,
                      f_master_seg: new_seg,
                    },
                  );
            };
          /* the op may have landed INSIDE an open cell (a nested row
             of an open def): that cell's zipper is authoritative on
             the next splice and would silently ERASE the edit — and
             opening the created subdef as its own cell would overlap
             the parent. Rebuild containing cells from the post-op
             segment instead, and keep focus inside the parent. */
          let entry_contains = (e: Model.stack_entry, id: Haz3lcore.Id.t) =>
            e.e_id != id
            && (
              Focus.seg_contains_id(id, Focus.zip_of_cell(e.e_body))
              || Focus.seg_contains_id(id, Focus.zip_of_cell(e.e_header))
            );
          let op_inside_open =
            switch (focus) {
            | Some(f) =>
              List.exists(e => entry_contains(e, fid), f.f_entries)
            | None => false
            };
          let focus =
            switch (focus) {
            | None => None
            | Some(f) =>
              op_inside_open
                ? Some(
                    Model.{
                      ...f,
                      f_entries:
                        List.map(
                          (e: Model.stack_entry) =>
                            entry_contains(e, fid)
                              ? switch (
                                  Focus.mk_entry(
                                    ~info_map=statics.info_map,
                                    ~sym=?outline_sym(e.e_id, statics.term),
                                    e.e_id,
                                    new_seg,
                                  )
                                ) {
                                | Some(e') => e'
                                | None => e
                                }
                              : e,
                          f.f_entries,
                        ),
                    },
                  )
                : Some(f)
            };
          /* single-parse restructure: [statics] IS the stacked frame.
             Seed the slot and recapture the open cells' frozen ctxs
             from the fresh DefStatics items (a deleted/moved upstream
             def changes what downstream cells see) — no Force pass. */
          let focus =
            switch (focus) {
            | None =>
              stacked_statics := None;
              None;
            | Some(f) =>
              stacked_statics := Some(statics);
              let ds_items =
                switch (Haz3lcore.DefStatics.current()) {
                | Some(ds) => ds.items
                | None => []
                };
              let f_entries =
                List.map(
                  (e: Model.stack_entry) =>
                    switch (
                      List.find_opt(
                        (it: Haz3lcore.DefStatics.item) =>
                          it.d_id == e.e_id
                          || Haz3lcore.Id.Map.mem(e.e_id, it.d_map),
                        ds_items,
                      )
                    ) {
                    | Some(it) =>
                      switch (Focus.cell_content(e, new_seg)) {
                      | Some(content) =>
                        switch (
                          Focus.captured_ctx(
                            ~info_map=it.d_map,
                            e.e_id,
                            content,
                          )
                        ) {
                        | Some(ctx) => {
                            ...e,
                            e_ctx: ctx,
                          }
                        | None => e
                        }
                      | None => e
                      }
                    | None => e
                    },
                  f.f_entries,
                );
              Some(
                Model.{
                  ...f,
                  f_entries,
                },
              );
            };
          switch (focus_target) {
          | Some(_) when op_inside_open => () /* shown in the parent */
          | Some(id) =>
            schedule_action(
              focus == None ? FocusToggle(id) : FocusEnsure(id),
            )
          | None => ()
          };
          {
            ...model,
            scratchpads:
              ListUtil.put_nth(model.current, new_sp, model.scratchpads),
            focus,
          }
          |> Updated.return;
        };
      };
    | UnfocusDef =>
      switch (model.focus) {
      | None => model |> Updated.return_quiet
      | Some(f) =>
        let restored =
          Focus.spliced_master(
            f,
            List.nth(model.scratchpads, model.current),
          );
        {
          ...model,
          scratchpads:
            ListUtil.put_nth(model.current, restored, model.scratchpads),
          focus: None,
        }
        |> Updated.return;
      }
    | StackHeader(i, a) =>
      switch (model.focus) {
      | None => model |> Updated.return_quiet
      | Some(f) =>
        switch (List.nth_opt(f.f_entries, i)) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          let* new_header =
            CellEditor.Update.update(~settings, a, entry.e_header);
          {
            ...model,
            focus:
              Some(
                Model.{
                  ...f,
                  f_entries:
                    ListUtil.put_nth(
                      i,
                      {
                        ...entry,
                        e_header: new_header,
                      },
                      f.f_entries,
                    ),
                },
              ),
          };
        }
      }
    | StackBody(i, a) =>
      switch (model.focus) {
      | None => model |> Updated.return_quiet
      | Some(f) =>
        switch (List.nth_opt(f.f_entries, i)) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          let* new_body =
            CellEditor.Update.update(~settings, a, entry.e_body);
          {
            ...model,
            focus:
              Some(
                Model.{
                  ...f,
                  f_entries:
                    ListUtil.put_nth(
                      i,
                      {
                        ...entry,
                        e_body: new_body,
                      },
                      f.f_entries,
                    ),
                },
              ),
          };
        }
      }
    | CellAction(a) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, agent}) =>
        let* new_ed = CellEditor.Update.update(~settings, a, editor);
        let new_sp =
          ListUtil.put_nth(
            model.current,
            {
              ...scratchpad,
              kind:
                Code({
                  editor: new_ed,
                  agent,
                }),
            },
            model.scratchpads,
          );
        let new_model = {
          ...model,
          scratchpads: new_sp,
        };
        new_model;
      | Drv(_) => model |> return_quiet
      };
    | DrvAction(a) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Drv(m) =>
        let* new_m =
          DerivationExerciseMode.Update.update(
            ~settings,
            ~schedule_action=a => schedule_action(DrvAction(a)),
            ~scratch_mode=true,
            a,
            m,
          );
        let new_sp =
          ListUtil.put_nth(
            model.current,
            {
              ...scratchpad,
              kind: Drv(new_m),
            },
            model.scratchpads,
          );
        {
          ...model,
          scratchpads: new_sp,
        };
      | Code(_) => model |> return_quiet
      };
    | RefreshStatics =>
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      model |> Updated.return_quiet(~recalculate=true);
    | SwitchSlide(i) =>
      {
        let name = List.nth(model.scratchpads, model.current).name;
        switch (model.focus) {
        | Some(f) =>
          Hashtbl.replace(
            slide_pins,
            name,
            List.map(
              (e: Model.stack_entry) => (e.e_id, e.e_run),
              f.f_entries,
            ),
          )
        | None => Hashtbl.remove(slide_pins, name)
        };
      };
      let model = commit_focus(model);
      WorkerClient.cancel();
      /* hydration (parse + first statics) can take seconds on large
         slides: paint a loading frame first, then hydrate. A plain
         schedule_action drains before the next render, so defer via a
         real timer. */
      ignore(
        Js_of_ocaml.Dom_html.window##setTimeout(
          Js_of_ocaml.Js.wrap_callback(() => schedule_action(HydrateCurrent)),
          30.,
        ),
      );
      {
        ...model,
        current: i,
      }
      |> Updated.return(~historic=false);
    | HydrateCurrent =>
      let model =
        Persist.hydrate_current(
          ~settings=settings.core,
          is_documentation ? "doc" : "scratch",
          model,
        );
      {
        /* restore this slide's pins (stale ids no-op in FocusToggle) */

        let name = List.nth(model.scratchpads, model.current).name;
        switch (Hashtbl.find_opt(slide_pins, name)) {
        | Some(ids) when model.focus == None =>
          List.iter(
            ((id, run)) =>
              schedule_action(run ? FocusToggleRun(id) : FocusToggle(id)),
            ids,
          )
        | _ => ()
        };
      };
      model |> Updated.return(~historic=false);
    | AddSlide =>
      let model = commit_focus(model);
      WorkerClient.cancel();
      Updated.return(
        add_new_slide(
          ~kind=NewCode,
          ~settings=settings.core,
          model,
          is_documentation,
        ),
      );
    | AddDrvSlide =>
      let model = commit_focus(model);
      WorkerClient.cancel();
      Updated.return(
        add_new_slide(
          ~kind=NewDrv,
          ~settings=settings.core,
          model,
          is_documentation,
        ),
      );
    | RenameSlide =>
      let model = commit_focus(model);
      let current = List.nth(model.scratchpads, model.current);
      let new_name =
        prompt_slide_name(
          ~existing_scratchpads=
            model.scratchpads
            |> List.to_seq
            |> Seq.zip(Seq.ints(0))
            |> Seq.filter(((idx, _)) => idx != model.current)
            |> Seq.map(snd)
            |> Seq.map((s: Scratchpad.t) => s.name),
          current.name,
        );

      switch (new_name) {
      | None => model |> return_quiet
      | Some(new_name) =>
        let new_sp =
          ListUtil.put_nth(
            model.current,
            {
              ...current,
              name: new_name,
            },
            model.scratchpads,
          );
        Updated.return({
          ...model,
          scratchpads: new_sp,
        });
      };
    | DeleteSlide =>
      let model = commit_focus(model);
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to delete this slide? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        WorkerClient.cancel();
        let new_sp =
          ListUtil.remove_nth(model.current, model.scratchpads)
          |> Option.value(~default=model.scratchpads);

        let m: Model.t =
          List.is_empty(new_sp)
            ? add_new_slide(
                ~kind=NewCode,
                ~settings=settings.core,
                {
                  ...model,
                  scratchpads: [],
                },
                is_documentation,
              )
            : Persist.hydrate_current(
                ~settings=settings.core,
                is_documentation ? "doc" : "scratch",
                {
                  scratchpads: new_sp,
                  current: max(model.current - 1, 0),
                  focus: None,
                },
              );
        Updated.return(m);
      } else {
        model |> return_quiet;
      };

    | ResetCurrent =>
      let model = commit_focus(model);
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({agent, _}) =>
        let source =
          switch (is_documentation) {
          | false =>
            CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp))
            |> CellEditor.Model.persist
          | true => Init.default_documentation_slide_name(scratchpad.name)
          };
        let* data = source |> CellEditor.Model.unpersist |> Updated.return;
        {
          ...model,
          scratchpads:
            ListUtil.put_nth(
              model.current,
              {
                ...scratchpad,
                kind:
                  Code({
                    editor: data,
                    agent,
                  }),
              },
              model.scratchpads,
            ),
        };
      | Drv(_) =>
        let new_sp =
          Scratchpad.blank_drv(~settings=settings.core, scratchpad.name);
        {
          ...model,
          scratchpads:
            ListUtil.put_nth(model.current, new_sp, model.scratchpads),
        }
        |> Updated.return;
      };
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      model |> return_quiet;
    | FinishImportScratchpad(data) =>
      let model = commit_focus(model);
      // reset file input so same file can be re-imported if desired
      JsUtil.reset_file_input("import-scratchpad");
      switch (data) {
      | None => model |> return_quiet
      | Some(data) =>
        let scratchpad = List.nth(model.scratchpads, model.current);
        switch (scratchpad.kind) {
        | Code({agent, _}) =>
          let new_data =
            data
            |> Sexplib.Sexp.of_string
            |> CellEditor.Model.persistent_of_sexp
            |> CellEditor.Model.unpersist(~settings=settings.core);

          let scratchpads =
            ListUtil.put_nth(
              model.current,
              {
                ...scratchpad,
                kind:
                  Code({
                    editor: new_data,
                    agent,
                  }),
              },
              model.scratchpads,
            );
          {
            ...model,
            scratchpads,
          }
          |> Updated.return;
        | Drv(_) => model |> return_quiet
        };
      };
    | Export =>
      export_scratch_slide(model);
      model |> Updated.return_quiet;
    | Encode =>
      encode_scratch_slide(model);
      model |> Updated.return_quiet;
    };
  };

  /* per-entry calculate memo (see calc_entry): FIXPOINT check. An
     entry that comes in physically identical to the last calculate's
     OUTPUT is already calculated — update only replaces an entry's
     record when it's edited, so unchanged entries hit this on every
     recalculate (evaluator-streaming actions trigger them
     constantly). Reuse also preserves the entry's physical identity,
     which the stack view cache keys on. */
  let calc_entry_memo:
    Hashtbl.t(
      Haz3lcore.Id.t,
      (Language.CoreSettings.t, Language.Dynamics.Map.t, Model.stack_entry),
    ) =
    Hashtbl.create(8);

  let calculate =
      (
        ~settings,
        ~autoprobe_mode,
        ~schedule_action,
        ~is_edited,
        model: Model.t,
      )
      : Model.t => {
    let statics_mode =
      CodeWithStatics.StaticsDebounce.consume(~is_edited, ~schedule_refresh=() =>
        schedule_action(RefreshStatics)
      );

    let scratchpad = List.nth(model.scratchpads, model.current);
    switch (scratchpad.kind) {
    | Code({editor, agent}) =>
      /* restore a loaded slide's saved caret: the Move runs as its own
         follow-up action, after this calculate builds measured */
      switch (Persist.pending_caret^) {
      | Some(p) => schedule_action(RestoreCaret(p))
      | None => ()
      };
      switch (Persist.pending_pins^) {
      | Some(_)
          when
            model.focus == None
            && List.exists(
                 (n: OutlineTree.node) => n.o_label != "",
                 OutlineTree.of_term(editor.editor.statics.term),
               ) =>
        /* only once statics carries a NAMED outline: hydration's
           first frames run against placeholder/hole programs (whose
           outline is a lone unnamed ⇒ row), and resolving there
           would silently drop the pins */
        schedule_action(RestorePins)
      | _ => ()
      };
      let worker_request = ref([]);
      let queue_worker =
        Some(
          (req_value: WorkerServer.Request.value) => {
            worker_request := worker_request^ @ [("", req_value)]
          },
        );
      /* calculate every stack cell: bodies with their frozen ctx
         (statics off entirely for non-Exp roots, i.e. type bodies);
         headers molding-only. Memoized per entry: a keystroke in one
         cell must not re-run statics for the others (their zippers
         are unchanged), and reuse must preserve entry IDENTITY so the
         stack view cache can hit. Force-refresh frames recompute. */
      let statics_off = (cs: Language.CoreSettings.t) =>
        Language.CoreSettings.{
          ...cs,
          statics: false,
          dynamics: false,
        };
      /* TRACK B while a stack is open: on the debounced Force frame,
         re-run compositional statics on the SPLICED program so
         cross-cell effects propagate — a rename/retype in one cell
         errors its dependents, and the outline badges update. Only
         dirty items re-analyze; open cells whose item changed get
         their frozen ctx recaptured (a fresh entry record), which
         forces their own recalc below. */
      if (model.focus == None) {
        stacked_statics := None;
      };
      let model =
        switch (model.focus) {
        | Some(f)
            when
              statics_mode == CodeWithStatics.StaticsForce
              || stacked_statics^ == None =>
          let prev_items =
            switch (Haz3lcore.DefStatics.current()) {
            | Some(p) => p.items
            | None => []
            };
          let spliced = Focus.splice_all(f);
          /* per-item incremental parse: unchanged items reuse their
             terms (parity test-gated); a one-cell edit re-parses one
             item instead of the whole program (~165ms at 2k) */
          let term = Haz3lcore.MakeTerm.Incr.term_of(spliced);
          /* probes live in ZIPPERS: union the master's with every open
             cell's, so a probe placed in a cell reaches the
             whole-program evaluation */
          let probe_union = (a, b) =>
            Id.Map.union((_, x, _) => Some(x), a, b);
          let probe_ids =
            List.fold_left(
              (acc, e: Model.stack_entry) =>
                probe_union(
                  probe_union(
                    acc,
                    Haz3lcore.CachedStatics.probe_ids_of_zipper(
                      e.e_body.editor.editor.state.zipper,
                    ),
                  ),
                  /* header probes too: projected statics make header
                     positions probeable */
                  Haz3lcore.CachedStatics.probe_ids_of_zipper(
                    e.e_header.editor.editor.state.zipper,
                  ),
                ),
              Haz3lcore.CachedStatics.probe_ids_of_zipper(
                editor.editor.editor.state.zipper,
              ),
              f.f_entries,
            );
          let ds =
            Haz3lcore.DefStatics.calc_auto(~settings, ~probe_ids, term);
          stacked_statics :=
            Some(
              Haz3lcore.CachedStatics.{
                term,
                elaborated:
                  switch (Haz3lcore.DefStatics.whole_elab(ds)) {
                  | Some(elab) => elab
                  | None =>
                    Haz3lcore.CachedStatics.dh_err(
                      "Compositional elaboration gap",
                    )
                  },
                info_map: ds.merged,
                error_ids: Haz3lcore.DefStatics.all_error_ids(ds),
                warning_ids: Haz3lcore.DefStatics.all_warning_ids(ds),
                targets:
                  Haz3lcore.CachedStatics.compute_targets(
                    ~settings,
                    ~info_map=ds.merged,
                    ~probe_ids,
                  ),
                probe_ids,
              },
            );
          let fresh = it => !List.exists(p => p === it, prev_items);
          let f_entries =
            List.map(
              (e: Model.stack_entry) =>
                switch (
                  /* the entry may be a MODULE MEMBER: its containing
                     top-level item is the one whose map knows its id */
                  List.find_opt(
                    (it: Haz3lcore.DefStatics.item) =>
                      it.d_id == e.e_id || Id.Map.mem(e.e_id, it.d_map),
                    ds.items,
                  )
                ) {
                | Some(it) when fresh(it) =>
                  switch (Focus.cell_content(e, spliced)) {
                  | Some(def_seg) =>
                    switch (
                      Focus.captured_ctx(~info_map=it.d_map, e.e_id, def_seg)
                    ) {
                    | Some(ctx) => {
                        ...e,
                        e_ctx: ctx,
                      }
                    | None => e
                    }
                  | None => e
                  }
                | _ => e
                },
              f.f_entries,
            );
          {
            ...model,
            focus:
              Some(
                Model.{
                  ...f,
                  f_entries,
                },
              ),
          };
        | _ => model
        };
      /* While a stack is open the master's zipper cannot change (all
         edits route to stack cells; splices happen in update), so its
         EDITOR calculate is skipped — but its RESULT keeps evaluating
         the SPLICED program (stacked_statics): whole-program dynamics
         stays live while stacked. Requests only fire when the grafted
         elaboration actually changed (Calc-gated inside). */
      let new_ed =
        switch (model.focus, stacked_statics^) {
        | (Some(_), Some(synth)) =>
          let result =
            EvalResult.Update.calculate(
              ~settings={
                ...settings,
                assist: false,
              },
              ~queue_worker,
              /* the master's editor (and its pending highlight) isn't
                 rendered while stacked: skip the O(program) worklist */
              ~compute_pending=false,
              ~is_edited,
              synth,
              editor.result,
            );
          {
            ...editor,
            result,
          };
        | (Some(_), None) => editor
        | (None, _) =>
          CellEditor.Update.calculate(
            ~settings,
            ~autoprobe_mode,
            ~is_edited,
            ~statics_mode,
            ~compositional=true,
            ~queue_worker,
            ~stitch=x => x,
            editor,
          )
        };
      /* whole-program samples flow into every cell (probes with
         out-of-cell call sites); the memo gates on the dynamics map's
         identity so cells re-render when new samples land */
      let extra_dyn =
        switch (model.focus) {
        | Some(_) => EvalResult.Model.dynamics(new_ed.result)
        | None => Language.Dynamics.Map.empty
        };
      let calc_entry = (e: Model.stack_entry): Model.stack_entry => {
        let reuse =
          statics_mode != CodeWithStatics.StaticsForce
            ? switch (Hashtbl.find_opt(calc_entry_memo, e.e_id)) {
              | Some((s', d', prev))
                  when prev === e && s' === settings && d' === extra_dyn =>
                Some(prev)
              | _ => None
              }
            : None;
        switch (reuse) {
        | Some(prev) => prev
        | None =>
          let body_is_exp = e.e_body.editor.editor.root == Haz3lcore.Sort.Exp;
          let body_is_typ = e.e_body.editor.editor.root == Haz3lcore.Sort.Typ;
          /* PROJECTION: on Force frames (fresh DefStatics just ran on
             the spliced program earlier in this calculate), cells read
             their item's analysis instead of re-running a private one.
             Built only on Force — the statics gate inside only
             consults it then. */
          let (proj_header, proj_body) =
            statics_mode == CodeWithStatics.StaticsForce
              ? {
                switch (Haz3lcore.DefStatics.current()) {
                | Some(ds) =>
                  switch (
                    List.find_opt(
                      (it: Haz3lcore.DefStatics.item) =>
                        it.d_id == e.e_id
                        || Haz3lcore.Id.Map.mem(e.e_id, it.d_map),
                      ds.items,
                    )
                  ) {
                  | Some(it) =>
                    let warns = Haz3lcore.DefStatics.all_warning_ids(ds);
                    (
                      Some(
                        project_cell_statics(
                          ~item=it,
                          ~engine_warnings=warns,
                          e.e_header,
                        ),
                      ),
                      Some(
                        project_cell_statics(
                          ~item=it,
                          ~engine_warnings=warns,
                          e.e_body,
                        ),
                      ),
                    );
                  | None => (None, None)
                  }
                | None => (None, None)
                };
              }
              : (None, None);
          /* type bodies: STATICS on, dynamics off */
          let body_settings =
            body_is_exp
              ? settings
              : body_is_typ || proj_body != None
                  ? Language.CoreSettings.{
                      ...settings,
                      dynamics: false,
                    }
                  : statics_off(settings);
          let e' =
            Model.{
              ...e,
              e_header:
                /* headers: projected item statics when available
                   (real binder types, warnings, MPat info for module
                   headers); wrapped init_pat/init_tpat as fallback.
                   Module headers stay statics-off only when no
                   projection exists (the Pat wrapper misreads MPat). */
                CellEditor.Update.calculate(
                  ~settings=
                    e.e_mod && proj_header == None
                      ? statics_off(settings)
                      : Language.CoreSettings.{
                          ...settings,
                          dynamics: false,
                        },
                  ~is_edited,
                  ~statics_mode,
                  ~ctx=e.e_ctx,
                  ~projected=?proj_header,
                  ~queue_worker=None,
                  ~stitch=x => x,
                  e.e_header,
                ),
              e_body:
                CellEditor.Update.calculate(
                  ~settings=body_settings,
                  ~is_edited,
                  ~statics_mode,
                  ~ctx=e.e_ctx,
                  ~projected=?proj_body,
                  ~extra_dynamics=extra_dyn,
                  ~queue_worker=None,
                  ~stitch=x => x,
                  e.e_body,
                ),
            };
          Hashtbl.replace(
            calc_entry_memo,
            e.e_id,
            (settings, extra_dyn, e'),
          );
          e';
        };
      };
      let model = {
        ...model,
        focus:
          Option.map(
            (f: Model.focus_t) =>
              Model.{
                ...f,
                f_entries: List.map(calc_entry, f.f_entries),
              },
            model.focus,
          ),
      };
      let dispatch = (_key, action) =>
        schedule_action(CellAction(ResultAction(action)));
      EvalRequest.request(
        worker_request^,
        ~pos_of_key=key => key,
        ~dispatch,
        ~on_timeout=
          List.iter(((key, _)) =>
            dispatch(key, UpdateResult(ResultFail(Timeout)))
          ),
      );
      let new_sp =
        ListUtil.put_nth(
          model.current,
          {
            ...scratchpad,
            kind:
              Code({
                editor: new_ed,
                agent,
              }),
          },
          model.scratchpads,
        );
      {
        ...model,
        scratchpads: new_sp,
      };
    | Drv(m) =>
      let new_m =
        DerivationExerciseMode.Update.calculate(
          ~settings,
          ~is_edited,
          ~schedule_action=a => schedule_action(DrvAction(a)),
          m,
        );
      let new_sp =
        ListUtil.put_nth(
          model.current,
          {
            ...scratchpad,
            kind: Drv(new_m),
          },
          model.scratchpads,
        );
      {
        ...model,
        scratchpads: new_sp,
      };
    };
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(CellEditor.Selection.t)
    | StackH(int, CellEditor.Selection.t)
    | StackB(int, CellEditor.Selection.t)
    | Drv(DerivationExerciseMode.Selection.t)
    | TextBox;

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection, model: Model.t)
      : cursor(Update.t) => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    let cursor =
      switch (selection, scratchpad.kind) {
      | (Cell(selection), Code({editor, _})) =>
        let+ a =
          CellEditor.Selection.get_cursor_info(
            ~inject=a => inject(CellAction(a)),
            ~selection,
            editor,
          );
        Update.CellAction(a);
      | (StackH(i, selection), Code(_)) =>
        switch (
          Option.bind(model.focus, (f: Model.focus_t) =>
            List.nth_opt(f.f_entries, i)
          )
        ) {
        | Some(entry) =>
          let+ a =
            CellEditor.Selection.get_cursor_info(
              ~inject=a => inject(StackHeader(i, a)),
              ~selection,
              entry.e_header,
            );
          Update.StackHeader(i, a);
        | None => empty
        }
      | (StackB(i, selection), Code(_)) =>
        switch (
          Option.bind(model.focus, (f: Model.focus_t) =>
            List.nth_opt(f.f_entries, i)
          )
        ) {
        | Some(entry) =>
          let+ a =
            CellEditor.Selection.get_cursor_info(
              ~inject=a => inject(StackBody(i, a)),
              ~selection,
              entry.e_body,
            );
          Update.StackBody(i, a);
        | None => empty
        }
      | (Drv(selection), Drv(m)) =>
        let+ a =
          DerivationExerciseMode.Selection.get_cursor_info(
            ~inject=a => inject(DrvAction(a)),
            ~selection,
            m,
          );
        Update.DrvAction(a);
      | (Cell(_), Drv(_))
      | (StackH(_), Drv(_))
      | (StackB(_), Drv(_))
      | (Drv(_), Code(_))
      | (TextBox, _) => empty
      };
    cursor
    |> Cursor.with_actions([
         ContextualAction.mk(
           ~mdIcon="download",
           ~section="Export",
           ~action=inject(Export),
           "Export Current Scratchpad",
         ),
         ContextualAction.mk(
           ~mdIcon="download",
           ~section="Export",
           ~action=inject(Encode),
           "Encode Current Scratchpad in URL",
         ),
         ContextualAction.mk(
           ~mdIcon="add",
           ~section="Scratchpads",
           ~action=inject(AddSlide),
           "Add New Code Scratchpad",
         ),
         ContextualAction.mk(
           ~mdIcon="rule",
           ~section="Scratchpads",
           ~action=inject(AddDrvSlide),
           "Add New Derivation Scratchpad",
         ),
         ContextualAction.mk(
           ~mdIcon="edit",
           ~section="Scratchpads",
           ~action=inject(RenameSlide),
           "Rename Current Scratchpad",
         ),
         ContextualAction.mk(
           ~mdIcon="delete",
           ~section="Scratchpads",
           ~action=inject(DeleteSlide),
           "Delete Current Scratchpad",
         ),
       ]);
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    switch (scratchpad.kind) {
    | Code({editor, _}) =>
      CellEditor.Selection.jump_to_tile(tile, editor)
      |> Option.map(((x, y)) => (Update.CellAction(x), Cell(y)))
    | Drv(m) =>
      DerivationExerciseMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.DrvAction(x), Drv(y)))
    };
  };

  /* Cross-cell jump-to-definition: a stack cell's jump whose binder is
     OUTSIDE the cell becomes (ensure the binder's outline item is in
     the stack, select the pane holding the binder, then a follow-up
     caret jump there). None = local jump or not a jump — take the
     normal path. */
  /* resolve a MASTER-domain id to a cross-cell jump while a stack is
     open: (open the containing item, focus the right pane, move its
     caret). Serves goto-definition from any pane AND result-strip /
     test jumps (which used to move the hidden master's caret). */
  let cross_cell_target =
      (~target_id: Haz3lcore.Id.t, ~model: Model.t, ~f: Model.focus_t)
      : option((Update.t, t, Update.t)) => {
    Util.OptUtil.Syntax.(
      {
        let scratchpad = List.nth(model.scratchpads, model.current);
        switch (scratchpad.kind) {
        | Drv(_) => None
        | Code({editor, _}) =>
          let statics = editor.editor.statics;
          let* info = Id.Map.find_opt(target_id, statics.info_map);
          /* the nearest enclosing outline item is the def to focus */
          let rec outline_ids = (acc, ns: list(OutlineTree.node)) =>
            List.fold_left(
              (acc, n: OutlineTree.node) =>
                outline_ids(
                  switch (n.o_id) {
                  | Some(id) => [id, ...acc]
                  | None => acc
                  },
                  n.o_children,
                ),
              acc,
              ns,
            );
          let items = outline_ids([], OutlineTree.of_term(statics.term));
          let* fid =
            List.find_opt(
              id => List.mem(id, items),
              [target_id, ...Language.Info.ancestors_of(info)],
            );
          let j = stack_position(~term=statics.term, fid, f.f_entries);
          /* the target lives in the pattern (header cell) for def
             binders, in the body for everything else */
          let in_header =
            Focus.seg_contains_id(
              target_id,
              Option.value(Focus.find_pat(fid, f.f_master_seg), ~default=[]),
            );
          let caret: CellEditor.Update.t =
            MainEditor(Perform(Move(Goal(TileId(target_id)))));
          Some((
            Update.FocusEnsure(fid),
            in_header ? StackH(j, MainEditor) : StackB(j, MainEditor),
            in_header
              ? Update.StackHeader(j, caret) : Update.StackBody(j, caret),
          ));
        };
      }
    );
  };

  let stack_jump_override =
      (action: Update.t, model: Model.t): option((Update.t, t, Update.t)) => {
    Util.OptUtil.Syntax.(
      switch (action, model.focus) {
      | (
          StackBody(
            i,
            MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
          ) |
          StackHeader(
            i,
            MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
          ),
          Some(f),
        ) =>
        let from_header =
          switch (action) {
          | StackHeader(_) => true
          | _ => false
          };
        let* entry = List.nth_opt(f.f_entries, i);
        let cell = from_header ? entry.Model.e_header : entry.Model.e_body;
        let cell_map = cell.editor.statics.info_map;
        let* ci = Indicated.ci_of(cell.editor.editor.state.zipper, cell_map);
        let* binding_id = Language.Info.get_binding_site(ci);
        if (Id.Map.mem(binding_id, cell_map)) {
          None; /* binder is inside this cell: the cell's own jump works */
        } else {
          cross_cell_target(~target_id=binding_id, ~model, ~f);
        };
      | _ => None
      }
    );
  };

  /* the selection an outline add/ensure should land on: the body pane
     of [fid] at its (future) stack position. None for removals — the
     selection stays put. */
  let stack_add_selection = (action: Update.t, model: Model.t): option(t) => {
    let target = (fid, entries) => {
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Drv(_) => None
      | Code({editor, _}) =>
        Some(
          StackB(
            stack_position(~term=editor.editor.statics.term, fid, entries),
            MainEditor,
          ),
        )
      };
    };
    switch (action, model.focus) {
    | (FocusEnsure(fid), Some(f)) => target(fid, f.f_entries)
    | (FocusToggle(fid), Some(f)) =>
      List.exists((e: Model.stack_entry) => e.e_id == fid, f.f_entries)
        ? None : target(fid, f.f_entries)
    | (FocusToggle(_), None) => Some(StackB(0, MainEditor))
    | _ => None
    };
  };

  let get_derivation_info = (~selection: t, model: Model.t) => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    switch (selection, scratchpad.kind) {
    | (Drv(sel), Drv(m)) =>
      DerivationExerciseMode.Selection.get_derivation_info(~selection=sel, m)
    | _ => None
    };
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  /* Stack-cell view cache: with N cells open, a keystroke in one cell
     must not rebuild the other N-1 cell views (measured 150-380ms per
     keystroke at 5 cells vs 10-70ms at 1 on Mega 1k). Reusing the
     physically-same nodes also short-circuits the vdom diff. Keyed on
     everything the cell view reads; models/settings by physical
     identity, small values structurally. Pruned to the live stack
     every render. */
  type stack_cache_key = {
    k_index: int,
    k_stack_len: int, /* escape closures bound-check against it */
    k_header_sel: option(CellEditor.Selection.t),
    k_body_sel: option(CellEditor.Selection.t),
    k_meta_down: bool,
    k_visible_rows: option(Globals.VisibleRows.t),
  };
  type cached_cell = {
    c_key: stack_cache_key,
    c_header: CellEditor.Model.t,
    c_body: CellEditor.Model.t,
    c_settings: Settings.t,
    c_font_metrics: FontMetrics.t,
    c_colors: option(ColorSteps.colorMap),
    /* master-derived decoration inputs rendered inside the cell
       (frozen tint, pending/active-eval): identity-tracked so cached
       nodes don't show stale decorations */
    c_reuse: Language.EvaluatorState.incr_eval,
    c_pending: list(Haz3lcore.Id.t),
    c_active: bool,
    c_nodes: list(Virtual_dom.Vdom.Node.t),
  };
  let stack_cache: ref(list((Haz3lcore.Id.t, cached_cell))) = ref([]);

  /* IMPORTANT: the view must read the cache through this helper, never
     bind `stack_cache^` locally. jsoo closures share one context object
     per scope — with the previous generation bound in the view scope,
     every handler closure of render N retained render N-1's vdom
     (whose handlers retained N-2's …): a linked list of generations,
     measured at ~11MB leaked per edit on mega-1k. */
  let stack_cache_lookup = (id: Haz3lcore.Id.t): option(cached_cell) =>
    List.assoc_opt(id, stack_cache^);

  let view =
      (
        ~globals,
        ~signal: event => 'a,
        ~inject: Update.t => 'a,
        ~inject_explainthis,
        ~selected: option(Selection.t),
        model: Model.t,
      ) => {
    let current = List.nth(model.scratchpads, model.current);
    if (current.dormant) {
      [
        /* SwitchSlide painted this frame before hydration: the next
           update parses + runs first statics, which blocks for a bit on
           large slides */
        Virtual_dom.Vdom.Node.div(
          ~attrs=[Virtual_dom.Vdom.Attr.classes(["slide-loading"])],
          [
            Virtual_dom.Vdom.Node.span(
              ~attrs=[Virtual_dom.Vdom.Attr.classes(["slide-loading-dot"])],
              [Virtual_dom.Vdom.Node.text({js|●|js})],
            ),
            Virtual_dom.Vdom.Node.text(" loading "),
            Virtual_dom.Vdom.Node.text(current.name),
            Virtual_dom.Vdom.Node.text({js|…|js}),
          ],
        ),
      ];
    } else {
      switch (current.kind) {
      | Code({editor, _}) =>
        /* the STACK: [header band, body cell] per entry, thin rules
           between; rendered INSTEAD of the master cell */
        let stack_views = (f: Model.focus_t) => {
          /* the frozen tint was removed upstream; cells no longer take
             master tint/pending, so these cache inputs are constant */
          let deco_reuse = Language.IncrEval.empty;
          let deco_pending: list(Haz3lcore.Id.t) = [];
          let deco_active = false;
          let rendered =
            List.mapi(
              (i, e: Model.stack_entry) => {
                let header_sel =
                  switch (selected) {
                  | Some(Selection.StackH(j, sel)) when j == i => Some(sel)
                  | _ => None
                  };
                let body_sel =
                  switch (selected) {
                  | Some(Selection.StackB(j, sel)) when j == i => Some(sel)
                  | _ => None
                  };
                let key = {
                  k_index: i,
                  k_stack_len: List.length(f.f_entries),
                  k_header_sel: header_sel,
                  k_body_sel: body_sel,
                  k_meta_down: globals.Globals.Model.meta_down,
                  k_visible_rows: globals.Globals.Model.visible_rows,
                };
                switch (stack_cache_lookup(e.e_id)) {
                | Some(c)
                    when
                      c.c_key == key
                      && c.c_header === e.e_header
                      && c.c_body === e.e_body
                      && c.c_settings === globals.Globals.Model.settings
                      && c.c_font_metrics
                      === globals.Globals.Model.font_metrics
                      && c.c_colors === globals.Globals.Model.color_highlights
                      && c.c_reuse === deco_reuse
                      && c.c_pending === deco_pending
                      && c.c_active == deco_active => (
                    e.e_id,
                    c,
                  )
                | _ =>
                  /* qualifier chip: the def's module path (stable while
                     the stack is open — the master term is frozen) */
                  let qualifier =
                    switch (
                      OutlineTree.path_of(e.e_id, editor.editor.statics.term)
                    ) {
                    | [] => []
                    | path => [
                        Virtual_dom.Vdom.Node.span(
                          ~attrs=[
                            Virtual_dom.Vdom.Attr.classes([
                              "focus-qualifier",
                            ]),
                          ],
                          [
                            Virtual_dom.Vdom.Node.text(
                              String.concat(".", path) ++ ".",
                            ),
                          ],
                        ),
                      ]
                    };
                  /* arrow keys at a pane's edge walk the stack:
                     ... body(i-1) <- header(i) <-> body(i) -> header(i+1) ... */
                  let headerless = idx =>
                    switch (List.nth_opt(f.f_entries, idx)) {
                    | Some(e) => e.Model.e_sym != None
                    | None => false
                    };
                  let pane_focus =
                      (idx, to_header, move: Haz3lcore.Action.move) =>
                    if (idx < 0 || idx >= List.length(f.f_entries)) {
                      Virtual_dom.Vdom.Effect.Ignore;
                    } else {
                      /* headerless entries have no header pane */
                      let to_header = to_header && !headerless(idx);
                      /* DOM focus must follow the selection to the new
                         pane (after render — the active-cell id moves
                         with the re-render) or the caret vanishes and
                         arrows scroll the page */
                      Haz3lcore.ProbePerform.FocusEffect.schedule_cell();
                      Virtual_dom.Vdom.Effect.Many([
                        signal(
                          MakeActive(
                            to_header
                              ? StackH(idx, MainEditor)
                              : StackB(idx, MainEditor),
                          ),
                        ),
                        inject(
                          to_header
                            ? StackHeader(
                                idx,
                                MainEditor(Perform(Move(move))),
                              )
                            : StackBody(
                                idx,
                                MainEditor(Perform(Move(move))),
                              ),
                        ),
                      ]);
                    };
                  let header_escape = (d: Util.Direction.t) =>
                    switch (d) {
                    | Left => pane_focus(i - 1, false, End)
                    | Right => pane_focus(i, false, Start)
                    };
                  let body_escape = (d: Util.Direction.t) =>
                    switch (d) {
                    | Left =>
                      headerless(i)
                        ? pane_focus(i - 1, false, End)
                        : pane_focus(i, true, End)
                    | Right => pane_focus(i + 1, true, Start)
                    };
                  let header_pane =
                    switch (e.e_sym) {
                    | Some(sym) =>
                      /* headerless items (statements, trailing expr):
                         a static symbol chip instead of a header cell */
                      Virtual_dom.Vdom.Node.div(
                        ~attrs=[
                          Virtual_dom.Vdom.Attr.classes([
                            "focus-header",
                            "focus-header-sym",
                          ]),
                        ],
                        /* no qualifier chip: the symbol IS the label
                           (a run cell was rendering "tests tests") */
                        [
                          Virtual_dom.Vdom.Node.span(
                            ~attrs=[
                              Virtual_dom.Vdom.Attr.classes(["focus-sym"]),
                            ],
                            [Virtual_dom.Vdom.Node.text(sym)],
                          ),
                        ],
                      )
                    | None =>
                      Virtual_dom.Vdom.Node.div(
                        ~attrs=[
                          Virtual_dom.Vdom.Attr.classes(["focus-header"]),
                        ],
                        qualifier
                        @ [
                          CellEditor.View.view(
                            ~globals,
                            ~signal=
                              fun
                              | MakeActive(sel) =>
                                signal(MakeActive(StackH(i, sel))),
                            ~inject=a => inject(StackHeader(i, a)),
                            ~selected=header_sel,
                            ~result_kind=`NoResults,
                            ~locked=false,
                            ~lines=false,
                            ~escape=header_escape,
                            e.e_header,
                          ),
                        ],
                      )
                    };
                  let nodes = [
                    header_pane,
                    Virtual_dom.Vdom.Node.div(
                      ~attrs=[Virtual_dom.Vdom.Attr.classes(["focus-body"])],
                      [
                        CellEditor.View.view(
                          ~globals,
                          ~signal=
                            fun
                            | MakeActive(sel) =>
                              signal(MakeActive(StackB(i, sel))),
                          ~inject=a => inject(StackBody(i, a)),
                          ~selected=body_sel,
                          ~result_kind=`NoResults,
                          ~locked=false,
                          ~lines=true,
                          ~master_result=editor.result,
                          ~escape=body_escape,
                          e.e_body,
                        ),
                      ],
                    ),
                  ];
                  (
                    e.e_id,
                    {
                      c_key: key,
                      c_header: e.e_header,
                      c_body: e.e_body,
                      c_settings: globals.Globals.Model.settings,
                      c_font_metrics: globals.Globals.Model.font_metrics,
                      c_colors: globals.Globals.Model.color_highlights,
                      c_reuse: deco_reuse,
                      c_pending: deco_pending,
                      c_active: deco_active,
                      c_nodes: nodes,
                    },
                  );
                };
              },
              f.f_entries,
            );
          stack_cache := rendered;
          /* the whole program's RESULT stays live below the stack (the
             master keeps evaluating the spliced program) */
          let (result_footer, _overlays) =
            EvalResult.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive(a) => signal(MakeActive(Cell(Result(a))))
                | JumpTo(id) =>
                  /* result-strip / test jumps used to move the HIDDEN
                     master's caret: open the containing item instead */
                  switch (
                    Selection.cross_cell_target(~target_id=id, ~model, ~f)
                  ) {
                  | Some((ensure, sel, caret)) =>
                    Virtual_dom.Vdom.Effect.Many([
                      inject(ensure),
                      signal(MakeActive(sel)),
                      inject(caret),
                    ])
                  | None =>
                    Virtual_dom.Vdom.Effect.Many([
                      signal(MakeActive(Cell(MainEditor))),
                      inject(
                        CellAction(
                          MainEditor(Perform(Move(Goal(TileId(id))))),
                        ),
                      ),
                    ])
                  },
              ~inject=a => inject(CellAction(ResultAction(a))),
              ~selected=
                switch (selected) {
                | Some(Selection.Cell(Result(a))) => Some(a)
                | _ => None
                },
              ~locked=false,
              editor.result,
            );
          List.concat_map(((_, c)) => c.c_nodes, rendered)
          @ [
            Virtual_dom.Vdom.Node.div(
              ~attrs=[Virtual_dom.Vdom.Attr.classes(["stack-result"])],
              result_footer,
            ),
          ]
          @ [
            /* trailing slack: any entry (incl. the last) can align to
               the viewport top, and the user can scroll to position any
               def where they like */
            Virtual_dom.Vdom.Node.div(
              ~attrs=[Virtual_dom.Vdom.Attr.classes(["stack-slack"])],
              [],
            ),
          ];
        };
        switch (model.focus) {
        | Some(f) =>
          (SlideContent.get_content(current.name) |> Option.to_list)
          @ stack_views(f)
        | None =>
          (SlideContent.get_content(current.name) |> Option.to_list)
          @ [
            CellEditor.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive(selection) =>
                  signal(MakeActive(Cell(selection))),
              ~inject=a => inject(CellAction(a)),
              ~selected=
                switch (selected) {
                | Some(Selection.Cell(s)) => Some(s)
                | _ => None
                },
              ~locked=false,
              ~lines=true,
              editor,
            ),
          ]
        };
      | Drv(m) =>
        DerivationExerciseMode.View.view(
          ~globals,
          ~signal=
            fun
            | MakeActive(s) => signal(MakeActive(Drv(s))),
          ~inject=a => inject(DrvAction(a)),
          ~inject_explainthis,
          ~selection=
            switch (selected) {
            | Some(Selection.Drv(s)) => Some(s)
            | _ => None
            },
          ~scratch_mode=true,
          m,
        )
      };
    };
  };

  let file_menu = (~globals: Globals.t, ~inject: Update.t => 'a, _: Model.t) => {
    let export_button =
      Widgets.button_named(
        Icons.export,
        _ => inject(Export),
        ~tooltip="Export Scratchpad",
      );

    let export_button_for_init =
      Widgets.button_named(
        Icons.export,
        _ => globals.inject_global(ExportForInit),
        ~tooltip="Export for Init",
      );

    let encode_button =
      Widgets.button_named(
        Icons.export,
        _ => inject(Encode),
        ~tooltip="Encode Scratchpad in URL",
      );

    let import_button =
      Widgets.file_select_button_named(
        "import-scratchpad",
        Icons.import,
        file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => inject(InitImportScratchpad(file))
          }
        },
        ~accept=[],
        ~tooltip="Import Scratchpad",
      );

    let file_group_scratch =
      NutMenu.item_group(
        ~inject,
        "File",
        [export_button, export_button_for_init, encode_button, import_button],
      );

    let reset_button =
      Widgets.button_named(
        Icons.trash,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset this scratchpad? You will lose any existing code.",
            );
          if (confirmed) {
            inject(ResetCurrent);
          } else {
            Virtual_dom.Vdom.Effect.Ignore;
          };
        },
        ~tooltip="Reset Editor",
      );

    let reparse =
      Widgets.button_named(
        Icons.backpack,
        _ => inject(CellAction(MainEditor(Perform(Reparse)))),
        ~tooltip="Reparse Editor",
      );

    let reset_hazel =
      Widgets.button_named(
        Icons.bomb,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written, and course staff have no way to restore it!",
            );
          if (confirmed) {
            HazelDB.clear_all();
            Js_of_ocaml.Dom_html.window##.location##reload;
          };
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Reset Hazel (LOSE ALL DATA)",
      );

    let reset_group_scratch =
      NutMenu.item_group(
        ~inject,
        "Reset",
        [reset_button, reparse, reset_hazel],
      );

    [file_group_scratch, reset_group_scratch];
  };

  let add_drv_slide_button = (~is_documentation, ~inject: Update.t => 'a) =>
    Widgets.button(
      ~tooltip=
        "Add New Derivation " ++ (is_documentation ? "Slide" : "Scratchpad"),
      Icons.entail,
      _ =>
      inject(Update.AddDrvSlide)
    );

  let top_bar =
      (
        ~globals as _,
        ~is_documentation: bool,
        ~inject: Update.t => 'a,
        model: Model.t,
      ) => {
    let unit_name = is_documentation ? "Slide" : "Scratchpad";
    let add_tooltip =
      is_documentation ? "Add New Slide" : "Add New Code Scratchpad";
    EditorModeView.view(
      ~edit_buttons=true,
      ~extra_edit_buttons=[add_drv_slide_button(~is_documentation, ~inject)],
      ~nav_buttons=false,
      ~unit_name,
      ~add_tooltip,
      ~signal=
        fun
        | Previous =>
          inject(
            SwitchSlide(
              (model.current + List.length(model.scratchpads) - 1)
              mod List.length(model.scratchpads),
            ),
          )
        | Next =>
          inject(
            SwitchSlide(
              (model.current + 1) mod List.length(model.scratchpads),
            ),
          )
        | Add => inject(AddSlide)
        | Rename => inject(RenameSlide)
        | Delete => inject(DeleteSlide),
      ~indicator=
        EditorModeView.indicator_select(
          ~signal=i => inject(SwitchSlide(i)),
          model.current,
          List.map((s: Scratchpad.t) => s.name, model.scratchpads),
        ),
      (),
    );
  };
};
