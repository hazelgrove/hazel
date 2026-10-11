open Haz3lcore;
open Util;

/* A program divided into open cells, each owning its item's text.
   Abstract, so nothing outside can read or edit a stale whole-program copy. */

module Focus = ScratchFocus;
module Cell = ScratchCell;

[@deriving (show({with_path: false}), sexp, yojson)]
type side = ScratchCell.side = | Header | Body;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* the whole-program editor at the split, kept for its root, probes and
     result; its zipper is not the program while divided */
  shell: CellEditor.Model.t,
  /* open cells in program order */
  cells: list(Cell.t),
  /* the program as of the last structural change; cells overwrite
     their slots when the document is assembled */
  base: Segment.t,
  /* the cell (and side) with the focus, where keys go and join lands;
     None: the first body has it, and join keeps no caret. The page's
     selection says only that the stack has the focus */
  active: option((Id.t, side)),
  /* whole-program statics of the assembled document, refreshed on
     statics frames while divided */
  statics: option(CachedStatics.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type after_close =
  | Still(t)
  | Joined(CellEditor.Model.t);

let cells = (d: t): list(Cell.t) => d.cells;
let root = (d: t): Sort.t => d.shell.editor.editor.root;
let result = (d: t): EvalResult.Model.t => d.shell.result;
let with_result = (result: EvalResult.Model.t, d: t): t => {
  ...d,
  shell: {
    ...d.shell,
    result,
  },
};
let statics = (d: t): CachedStatics.t =>
  switch (d.statics) {
  | Some(s) => s
  | None => d.shell.editor.statics
  };
let has_fresh_statics = (d: t): bool => Option.is_some(d.statics);
let with_statics = (s: CachedStatics.t, d: t): t => {
  ...d,
  statics: Some(s),
};

/* [seg] with [cells]' text in their slots */
let spliced = (seg: Segment.t, cells: list(Cell.t)): Segment.t =>
  List.fold_left((seg, e) => Focus.splice_entry(e, seg), seg, cells);

let document = (d: t): Segment.t => spliced(d.base, d.cells);

/* in program coordinates: the indentation the cell cut is its own too */
let cell_ids = (e: Cell.t): list(Id.t) =>
  Segment.ids(Focus.header_text(e)) @ Segment.ids(Focus.body_text(e));

/* the cell covers [id]: its own item, a run member, or any piece inside
   its text */
let owns = (id: Id.t, e: Cell.t): bool =>
  List.mem(id, Cell.covers(e)) || List.mem(id, cell_ids(e));

let owner = (id: Id.t, d: t): option(Cell.t) =>
  List.find_opt(owns(id), d.cells);

let cell = (id: Id.t, d: t): option(Cell.t) =>
  List.find_opt((e: Cell.t) => e.e_id == id, d.cells);

let insert = (~term, entry: Cell.t, cells: list(Cell.t)): list(Cell.t) => {
  let order = OutlineTree.collect(OutlineTree.of_term(term));
  let rank = id =>
    Option.value(List.find_index(x => x == id, order), ~default=max_int);
  let r = rank(entry.e_id);
  let (before, after) =
    List.partition((e: Cell.t) => rank(e.e_id) < r, cells);
  before @ [entry, ...after];
};

let probes_of = (c: CellEditor.Model.t): Refractors.RefractorList.t =>
  c.editor.editor.state.zipper.refractors.manuals;

/* manual probes of the cells and of the shell (placed before the
   split), first per anchor */
let probes = (d: t): Refractors.RefractorList.t =>
  ListUtil.dedup_f(
    ((a, _), (b, _)) => a == b,
    List.concat_map(
      (e: Cell.t) => probes_of(e.e_header) @ probes_of(e.e_body),
      d.cells,
    )
    @ probes_of(d.shell),
  );

/* manual probes live with their anchors: in the open cell whose text
   holds them, the rest in the shell */
let settle = (~pool: Refractors.RefractorList.t, d: t): t => {
  let set = (c: CellEditor.Model.t, ms): CellEditor.Model.t =>
    List.map(fst, probes_of(c)) == List.map(fst, ms)
      ? c
      : CellEditor.Model.map_zipper(
          z =>
            ZipperBase.update_refractors(z, r =>
              Refractors.{
                ...r,
                manuals: ms,
              }
            ),
          c,
        );
  let placed = ref([]);
  let take = (c: CellEditor.Model.t) => {
    let ids = Segment.ids(Focus.zip_of_cell(c));
    let ms = List.filter(((id, _)) => List.mem(id, ids), pool);
    placed := List.map(fst, ms) @ placed^;
    set(c, ms);
  };
  let cells =
    Focus.map_sharing(
      (e: Cell.t) => {
        let (h, b) = (take(e.e_header), take(e.e_body));
        h === e.e_header && b === e.e_body
          ? e
          : {
            ...e,
            e_header: h,
            e_body: b,
          };
      },
      d.cells,
    );
  {
    ...d,
    cells,
    shell:
      set(
        d.shell,
        List.filter(((id, _)) => !List.mem(id, placed^), pool),
      ),
  };
};

/* the program before its first cell opens */
let of_whole = (editor: CellEditor.Model.t): t => {
  shell: editor,
  cells: [],
  base: Focus.zip_of_cell(editor),
  active: None,
  statics: None,
};

/* a cell for [id]: its header and body, or with [inner], a module's
   members alone */
let entry = (~info_map, ~sym=?, ~inner=false, id: Id.t, base: Segment.t) =>
  inner
    ? Focus.mk_members_entry(~info_map, id, base)
    : Focus.mk_entry(~info_map, ~sym?, id, base);

let active = (d: t): option((Id.t, side)) => d.active;

let active_cell = (d: t): option((Cell.t, side)) =>
  switch (d.active) {
  | Some((id, side)) => Option.map(e => (e, side), cell(id, d))
  | None => None
  };

/* the pane with the focus: the active one, else the first body */
let focused = (d: t): option((Cell.t, side)) =>
  switch (active_cell(d), d.cells) {
  | (Some(_) as a, _) => a
  | (None, [e, ..._]) => Some((e, Body))
  | (None, []) => None
  };

let active_editor = (d: t): CellEditor.Model.t =>
  switch (focused(d)) {
  | Some((e, side)) => Cell.pane(side, e)
  | None => d.shell
  };

/* a caret as a side of a piece id: before its right neighbour, else
   after its left one */
let anchor_of = (z: Zipper.t): option((Direction.t, Id.t)) =>
  switch (Siblings.neighbors(z.relatives.siblings)) {
  | (_, Some(p)) => Some((Direction.Left, Piece.id(p)))
  | (Some(p), None) => Some((Direction.Right, Piece.id(p)))
  | (None, None) => None
  };

let caret_anchor = (d: t): option((Direction.t, Id.t)) =>
  Option.map(
    ((e: Cell.t, side)) =>
      switch (anchor_of(Cell.pane(side, e).editor.editor.state.zipper)) {
      | Some(a) => a
      | None => (Direction.Left, e.e_id)
      },
    active_cell(d),
  );

/* the caret moves into the open cell holding [id], which becomes
   active; unchanged if no cell holds it */
let place_caret = ((dir, id): (Direction.t, Id.t), d: t): t => {
  /* the cell with the caret beside [id], and the side it is in */
  let moved = (e: Cell.t) =>
    List.find_map(
      side => {
        let c = Cell.pane(side, e);
        List.mem(id, Segment.ids(Focus.zip_of_cell(c)))
          ? Move.jump_to_side_of_id(dir, c.editor.editor.state.zipper, id)
            |> Option.map(z =>
                 (
                   Cell.with_pane(
                     side,
                     CellEditor.Model.map_zipper(_ => z, c),
                     e,
                   ),
                   side,
                 )
               )
          : None;
      },
      [Body, Header],
    );
  let rec go = (before, cells: list(Cell.t)) =>
    switch (cells) {
    | [] => d
    | [e, ...rest] =>
      switch (moved(e)) {
      | Some((e, side)) => {
          ...d,
          cells: List.rev(before) @ [e, ...rest],
          active: Some((e.e_id, side)),
        }
      | None => go([e, ...before], rest)
      }
    };
  go([], d.cells);
};

/* one editor holding [seg] at [root]: the [probes] still anchored in it,
   the caret beside [anchor]; with [prev], an earlier editor's parse and
   measure caches */
let editor_of =
    (
      ~prev: option(CellEditor.Model.t)=?,
      ~root: Sort.t,
      ~probes: Refractors.RefractorList.t,
      ~anchor: option((Direction.t, Id.t)),
      seg: Segment.t,
    )
    : CellEditor.Model.t => {
  let present = Id.Map.of_list(List.map(id => (id, ()), Segment.ids(seg)));
  let manuals = List.filter(((id, _)) => Id.Map.mem(id, present), probes);
  let z =
    Zipper.unzip(~direction=Left, seg)
    |> ZipperBase.update_refractors(_, r =>
         Refractors.{
           ...r,
           manuals,
         }
       );
  let z =
    switch (anchor) {
    | Some((side, id)) =>
      Option.value(Move.jump_to_side_of_id(side, z, id), ~default=z)
    | None => z
    };
  let syntax =
    Option.map((pe: CellEditor.Model.t) => pe.editor.editor.syntax, prev);
  CellEditor.Model.mk(
    Editor.Model.{
      root,
      state: {
        zipper: z,
        col_target: None,
      },
      syntax:
        CachedSyntax.init(
          ~root,
          ~m_cache=?Option.map((s: CachedSyntax.t) => s.m_cache, syntax),
          ~t_cache=?Option.map((s: CachedSyntax.t) => s.t_cache, syntax),
          z,
        ),
    },
  );
};

/* one editor again: root, probes and the result carry over, and the
   caret lands where it was in the active cell */
let join = (~prev: option(CellEditor.Model.t)=?, d: t): CellEditor.Model.t => {
  let fresh =
    editor_of(
      ~prev?,
      ~root=root(d),
      ~probes=probes(d),
      ~anchor=caret_anchor(d),
      document(d),
    );
  {
    editor: {
      ...fresh.editor,
      statics: statics(d),
    },
    result: d.shell.result,
  };
};

let close = (id: Id.t, d: t): after_close =>
  switch (List.partition((e: Cell.t) => e.e_id == id, d.cells)) {
  | ([], _) => Still(d)
  | ([closing, ..._], rest) =>
    switch (rest) {
    /* join's document splices it: splicing here too doubled its edge text */
    | [] =>
      Joined(
        join({
          ...d,
          cells: [closing],
        }),
      )
    | _ =>
      Still(
        settle(
          ~pool=probes(d),
          {
            ...d,
            cells: rest,
            base: Focus.splice_entry(closing, d.base),
            active:
              switch (d.active) {
              | Some((a, _)) when a == id => None
              | a => a
              },
          },
        ),
      )
    }
  };

/* [mk]'s cell, cut from the base with [closing] folded back in, opens
   among [keeping] and becomes active */
let open_among =
    (~term, mk: Segment.t => option(Cell.t), closing, keeping, d: t)
    : option(t) => {
  let base = spliced(d.base, closing);
  mk(base)
  |> Option.map((entry: Cell.t) =>
       settle(
         ~pool=probes(d),
         {
           ...d,
           base,
           cells: insert(~term, entry, keeping),
           active: Some((entry.e_id, Body)),
         },
       )
     );
};

/* opening a parent folds its open descendants back into it; an id inside
   an open cell opens nothing (the caller moves the caret there instead) */
let open_ =
    (~info_map, ~term, ~sym: option(string)=?, ~inner=false, id: Id.t, d: t)
    : option(t) =>
  switch (owner(id, d)) {
  | Some(_) => None
  | None =>
    let desc = OutlineTree.descendant_ids(id, term);
    let (closing, keeping) =
      List.partition((e: Cell.t) => List.mem(e.e_id, desc), d.cells);
    open_among(
      ~term,
      entry(~info_map, ~sym?, ~inner, id),
      closing,
      keeping,
      d,
    );
  };

/* [fid]'s test run as one cell, folding in members open alone; None if
   another open cell holds it */
let open_run = (~info_map, ~term, fid: Id.t, d: t): option(t) => {
  let members =
    switch (Focus.test_run_deep(fid, d.base)) {
    | Some((_, ms, _)) => ms
    | None => [fid]
    };
  let (alone, keeping) =
    List.partition(
      (e: Cell.t) => !Cell.is_run(e) && List.mem(e.e_id, members),
      d.cells,
    );
  List.exists(owns(fid), keeping)
    ? None
    : open_among(
        ~term,
        Focus.mk_run_entry(~info_map, fid),
        alone,
        keeping,
        d,
      );
};

/* after an edit to the joined program (agent, outline menu): the same
   cells, cut from the edited program; cells whose item is gone close */
let resplit =
    (~info_map, ~term, editor: CellEditor.Model.t, d: t): after_close => {
  let base = Focus.zip_of_cell(editor);
  /* a cell the edit didn't touch keeps its editor (caret, selection); a
     rename keeps its tokens' ids, so compare their labels too */
  let rec labels = (s: Segment.t): list(string) =>
    List.concat_map(
      fun
      | Piece.Tile(t) => Tile.label(t) @ List.concat_map(labels, t.children)
      | _ => [],
      s,
    );
  let same = (a: Segment.t, b: Segment.t) => {
    let a = Focus.core_ws(a);
    Segment.ids(a) == Segment.ids(b) && labels(a) == labels(b);
  };
  let untouched = (e: Cell.t): bool =>
    switch (Focus.cell_content(e, base)) {
    | None => false
    | Some(slice) =>
      same(slice, Focus.body_text(e))
      && (
        switch (e.e_kind) {
        | Run(_)
        | Headless(_) => true
        | Def(_)
        | Members =>
          switch (Focus.find_pat(e.e_id, base)) {
          | Some(pat) => same(pat, Focus.header_text(e))
          | None => false
          }
        }
      )
    };
  let cells =
    List.filter_map(
      (e: Cell.t) =>
        untouched(e)
          ? Some(e)
          : (
            switch (e.e_kind) {
            | Run(_) => Focus.mk_run_entry(~info_map, e.e_id, base)
            | Members => Focus.mk_members_entry(~info_map, e.e_id, base)
            | Headless(sym) => Focus.mk_entry(~info_map, ~sym, e.e_id, base)
            | Def(_) => Focus.mk_entry(~info_map, e.e_id, base)
            }
          ),
      d.cells,
    );
  /* an edit can carry one open item into another (a member into an open
     module): the outer cell holds it now, so the inner one closes */
  let cells =
    List.filter(
      (e: Cell.t) =>
        !
          List.exists(
            (o: Cell.t) =>
              o !== e
              && List.exists(
                   id => List.mem(id, cell_ids(o)),
                   Cell.covers(e),
                 ),
            cells,
          ),
      cells,
    );
  switch (cells) {
  | [] => Joined(editor)
  | _ =>
    Still(
      settle(
        ~pool=probes_of(editor),
        {
          shell: editor,
          cells:
            List.fold_left((acc, e) => insert(~term, e, acc), [], cells),
          base,
          /* a cell the edit removed is no longer where the caret is */
          active:
            switch (d.active) {
            | Some((a, _))
                when !List.exists((e: Cell.t) => e.e_id == a, cells) =>
              None
            | a => a
            },
          statics: None,
        },
      ),
    )
  };
};

/* same text, probes and carets: the base and every cell zipper are
   physically unchanged, and the same cell is active */
let same_content = (a: t, b: t): bool => {
  let same = (c: CellEditor.Model.t, c': CellEditor.Model.t) =>
    Zipper.same_content(
      c.editor.editor.state.zipper,
      c'.editor.editor.state.zipper,
    );
  a.base === b.base
  && a.active == b.active
  && List.length(a.cells) == List.length(b.cells)
  && List.for_all2(
       (x: Cell.t, y: Cell.t) =>
         same(x.e_header, y.e_header) && same(x.e_body, y.e_body),
       a.cells,
       b.cells,
     );
};

/* the whole program for the problems panel, its segment the current
   document (the shell's is stale); memoized: the panel caches on identity */
let outside_memo: Slot.t(t, CodeEditable.Model.t) = Slot.mk();
/* the document's term data, incrementally: the panel keeps only problems
   whose ids it holds, and the shell's are from the split */
let outside_cache = MakeTerm.Incr.mk_cache();
let outside_editor = (d: t): CodeEditable.Model.t =>
  Slot.get(
    ~same=
      (d', d) =>
        same_content(d', d)
        && d'.statics === d.statics
        /* not the shell itself: every calculate re-wraps it with a result */
        && d'.shell.editor === d.shell.editor,
    outside_memo,
    d,
    () => {
      let segment = document(d);
      {
        ...d.shell.editor,
        editor: {
          ...d.shell.editor.editor,
          syntax: {
            ...d.shell.editor.editor.syntax,
            segment,
            term_data:
              MakeTerm.Incr.go_incr(
                ~root=root(d),
                ~cache=outside_cache,
                segment,
              ).
                term_data,
          },
        },
        statics: statics(d),
      };
    },
  );

let map_cells = (f: Cell.t => Cell.t, d: t): t => {
  ...d,
  cells: List.map(f, d.cells),
};

/* a headerless cell (⇒, `;`) is keyed by its expression's root, which an
   edit can replace (`f(x)` to `f(x) + 1`): it takes the id of the
   headerless row now in its text. returns the (old, new) ids */
let follow_headless =
    (~rows: Id.Map.t(unit), ~headless: Id.Map.t(unit), d: t)
    : (t, list((Id.t, Id.t))) => {
  let moves = ref([]);
  let base = ref(d.base);
  let cells =
    List.map(
      (e: Cell.t) =>
        switch (e.e_kind) {
        | Headless(_) when !Id.Map.mem(e.e_id, rows) =>
          let body = Focus.zip_of_cell(e.e_body);
          let top =
            List.filter_map(
              fun
              | Piece.Tile(t) => Some(t.id)
              | _ => None,
              body,
            );
          let find = ids =>
            List.find_opt(id => Id.Map.mem(id, headless), ids);
          switch (
            switch (find(top)) {
            | Some(_) as found => found
            | None => find(Segment.ids(body))
            }
          ) {
          | Some(id) =>
            moves := [(e.e_id, id), ...moves^];
            /* the base takes the cell's text first, so its slot holds
               the new root: splices by that id must find it, or the
               cell's later edits never reach the program */
            base := Focus.splice_entry(e, base^);
            {
              ...e,
              e_id: id,
            };
          | None => e
          };
        | _ => e
        },
      d.cells,
    );
  let moved = id => Option.value(List.assoc_opt(id, moves^), ~default=id);
  (
    {
      ...d,
      base: base^,
      cells,
      active: Option.map(((id, side)) => (moved(id), side), d.active),
    },
    moves^,
  );
};

let update_cell = (id: Id.t, f: Cell.t => Cell.t, d: t): t => {
  ...d,
  cells: List.map((e: Cell.t) => e.e_id == id ? f(e) : e, d.cells),
};

let set_active = (id: Id.t, side: side, d: t): t =>
  Option.is_some(cell(id, d))
    ? {
      ...d,
      active: Some((id, side)),
    }
    : d;

/* [d] after cells of [prev] opened and closed: [prev]'s focused pane
   keeps the focus while open, and the first body, focused by default,
   stays so while still first */
let keep_focus = (~prev: t, d: t): t =>
  switch (focused(prev), d.cells) {
  | (Some((e, _)), [first, ..._])
      when prev.active == None && first.e_id == e.e_id => {
      ...d,
      active: None,
    }
  | (Some((e, side)), _) => set_active(e.e_id, side, d)
  | (None, _) => d
  };

let map_editors = (f: CellEditor.Model.t => CellEditor.Model.t, d: t): t => {
  ...d,
  shell: f(d.shell),
  cells:
    List.map(
      (e: Cell.t) =>
        {
          ...e,
          e_header: f(e.e_header),
          e_body: f(e.e_body),
        },
      d.cells,
    ),
};

let editors = (d: t): list(CellEditor.Model.t) => [
  d.shell,
  ...List.concat_map((e: Cell.t) => [e.e_header, e.e_body], d.cells),
];

let stepping = (d: t): option(ProjectorBase.stepping) =>
  List.find_map(
    (c: CellEditor.Model.t) =>
      c.editor.editor.state.zipper.refractors.stepping,
    editors(d),
  );

let keep_stepping = (span: Language.Sample.span_ref, d: t): t =>
  List.exists(
    (c: CellEditor.Model.t) =>
      switch (c.editor.editor.state.zipper.refractors.stepping) {
      | Some(st) => st.span != span
      | None => false
      },
    editors(d),
  )
    ? map_editors(
        (c: CellEditor.Model.t) =>
          switch (c.editor.editor.state.zipper.refractors.stepping) {
          | Some(st) when st.span != span =>
            CellEditor.Model.map_zipper(ProbePerform.hide_steps, c)
          | _ => c
          },
        d,
      )
    : d;

/* an undo snapshot: the whole-program statics recompute on restore */
let compact = (f: CellEditor.Model.t => CellEditor.Model.t, d: t): t => {
  ...map_editors(f, d),
  statics: None,
};
