open Haz3lcore;
open Util;

/* A program divided into open cells, each owning its item's text.
   Abstract, so nothing outside can read or edit a stale whole-program copy. */

module Focus = ScratchFocus;
module Cell = ScratchCell;

[@deriving (show({with_path: false}), sexp, yojson)]
type side =
  | Header
  | Body;

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
  /* the cell (and side) that last had the caret: join lands there */
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
let has_fresh_statics = (d: t): bool =>
  switch (d.statics) {
  | Some(_) => true
  | None => false
  };
let with_statics = (s: CachedStatics.t, d: t): t => {
  ...d,
  statics: Some(s),
};

let document = (d: t): Segment.t =>
  List.fold_left((seg, e) => Focus.splice_entry(e, seg), d.base, d.cells);

let cell_ids = (e: Cell.t): list(Id.t) =>
  Segment.ids(Focus.zip_of_cell(e.e_header))
  @ Segment.ids(Focus.zip_of_cell(e.e_body));

/* the open cell covering [id]: its own item, a run member, or any
   piece inside its text */
let owner = (id: Id.t, d: t): option(Cell.t) =>
  List.find_opt(
    (e: Cell.t) =>
      List.mem(id, Cell.covers(e)) || List.mem(id, cell_ids(e)),
    d.cells,
  );

let outline_order = (term: Language.Exp.t): list(Id.t) => {
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

let insert = (~term, entry: Cell.t, cells: list(Cell.t)): list(Cell.t) => {
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
  let r = rank(entry.e_id);
  let (before, after) =
    List.partition((e: Cell.t) => rank(e.e_id) < r, cells);
  before @ [entry, ...after];
};

/* where [id]'s cell sits, or would go, in program order */
let position = (~term, id: Id.t, d: t): int => {
  let rec index = (k, cells: list(Cell.t)) =>
    switch (cells) {
    | [] => None
    | [e, ..._] when e.e_id == id => Some(k)
    | [_, ...rest] => index(k + 1, rest)
    };
  switch (index(0, d.cells)) {
  | Some(k) => k
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
    let r = rank(id);
    List.length(List.filter((e: Cell.t) => rank(e.e_id) < r, d.cells));
  };
};

let probes_of = (c: CellEditor.Model.t): Refractors.RefractorList.t =>
  c.editor.editor.state.zipper.refractors.manuals;

/* manual probes of the cells and of the shell (placed before the
   split), first per anchor */
let probes = (d: t): Refractors.RefractorList.t =>
  List.fold_left(
    (acc, (id, _) as p) => List.mem_assoc(id, acc) ? acc : acc @ [p],
    [],
    List.concat_map(
      (e: Cell.t) => probes_of(e.e_header) @ probes_of(e.e_body),
      d.cells,
    )
    @ probes_of(d.shell),
  );

let mk = (editor: CellEditor.Model.t, base: Segment.t, cell: Cell.t): t => {
  shell: editor,
  cells: [cell],
  base,
  active: Some((cell.e_id, Body)),
  statics: None,
};

/* a cell for [id]: its header and body, or with [inner], a module's
   members alone */
let entry = (~info_map, ~sym=?, ~inner=false, id: Id.t, base: Segment.t) =>
  inner
    ? Focus.mk_members_entry(~info_map, id, base)
    : Focus.mk_entry(~info_map, ~sym?, id, base);

let split =
    (
      ~info_map,
      ~sym: option(string)=?,
      ~inner=false,
      editor: CellEditor.Model.t,
      id: Id.t,
    )
    : option(t) => {
  let base = Focus.zip_of_cell(editor);
  entry(~info_map, ~sym?, ~inner, id, base) |> Option.map(mk(editor, base));
};

let split_run = (~info_map, editor: CellEditor.Model.t, id: Id.t): option(t) => {
  let base = Focus.zip_of_cell(editor);
  Focus.mk_run_entry(~info_map, id, base) |> Option.map(mk(editor, base));
};

let active = (d: t): option((Id.t, side)) => d.active;

let active_editor = (d: t): CellEditor.Model.t =>
  switch (
    switch (d.active) {
    | Some((id, side)) =>
      List.find_opt((e: Cell.t) => e.e_id == id, d.cells)
      |> Option.map((e: Cell.t) => side == Header ? e.e_header : e.e_body)
    | None => None
    }
  ) {
  | Some(c) => c
  | None =>
    switch (d.cells) {
    | [e, ..._] => e.e_body
    | [] => d.shell
    }
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
  switch (d.active) {
  | None => None
  | Some((id, side)) =>
    switch (List.find_opt((e: Cell.t) => e.e_id == id, d.cells)) {
    | None => None
    | Some(e) =>
      let z =
        (side == Header ? e.e_header : e.e_body).editor.editor.state.zipper;
      switch (anchor_of(z)) {
      | Some(a) => Some(a)
      | None => Some((Direction.Left, e.e_id))
      };
    }
  };

let with_zipper = (c: CellEditor.Model.t, z: Zipper.t): CellEditor.Model.t => {
  ...c,
  editor: {
    ...c.editor,
    editor: {
      ...c.editor.editor,
      state: {
        ...c.editor.editor.state,
        zipper: z,
      },
    },
  },
};

/* the caret moves into the open cell holding [id], which becomes
   active; unchanged if no cell holds it */
let place_caret = ((side, id): (Direction.t, Id.t), d: t): t => {
  let moved = (c: CellEditor.Model.t) =>
    List.mem(id, Segment.ids(Focus.zip_of_cell(c)))
      ? Move.jump_to_side_of_id(side, c.editor.editor.state.zipper, id) : None;
  let rec go = (before, cells: list(Cell.t)) =>
    switch (cells) {
    | [] => d
    | [e, ...rest] =>
      switch (moved(e.e_body), moved(e.e_header)) {
      | (Some(z), _) => {
          ...d,
          cells:
            List.rev(before)
            @ [
              {
                ...e,
                e_body: with_zipper(e.e_body, z),
              },
              ...rest,
            ],
          active: Some((e.e_id, Body)),
        }
      | (None, Some(z)) => {
          ...d,
          cells:
            List.rev(before)
            @ [
              {
                ...e,
                e_header: with_zipper(e.e_header, z),
              },
              ...rest,
            ],
          active: Some((e.e_id, Header)),
        }
      | (None, None) => go([e, ...before], rest)
      }
    };
  go([], d.cells);
};

/* one editor again: root, probes and the result carry over, and the
   caret lands where it was in the active cell */
let join = (d: t): CellEditor.Model.t => {
  let seg = document(d);
  let present = Segment.ids(seg);
  let manuals =
    List.filter(((id, _)) => List.mem(id, present), probes(d));
  let z =
    Zipper.unzip(~direction=Left, seg)
    |> ZipperBase.update_refractors(_, r =>
         Refractors.{
           ...r,
           manuals,
         }
       );
  let z =
    switch (caret_anchor(d)) {
    | Some((side, id)) =>
      Option.value(Move.jump_to_side_of_id(side, z, id), ~default=z)
    | None => z
    };
  let fresh = CellEditor.Model.mk(Editor.Model.mk(z, ~root=root(d)));
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
    let base = Focus.splice_entry(closing, d.base);
    switch (rest) {
    | [] =>
      Joined(
        join({
          ...d,
          cells: [closing],
          base,
        }),
      )
    | _ =>
      Still({
        ...d,
        cells: rest,
        base,
        active:
          switch (d.active) {
          | Some((a, _)) when a == id => None
          | a => a
          },
      })
    };
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
    let base =
      List.fold_left(
        (seg, e) => Focus.splice_entry(e, seg),
        d.base,
        closing,
      );
    entry(~info_map, ~sym?, ~inner, id, base)
    |> Option.map(entry =>
         {
           ...d,
           base,
           cells: insert(~term, entry, keeping),
           active: Some((id, Body)),
         }
       );
  };

/* [fid]'s test run as one cell, folding in members open alone; None if
   another open cell holds it */
let open_run = (~info_map, ~term, fid: Id.t, d: t): option(t) => {
  let members =
    switch (Focus.test_run_deep(fid, d.base)) {
    | Some((_, ms)) => ms
    | None => [fid]
    };
  let (alone, keeping) =
    List.partition(
      (e: Cell.t) => !e.e_run && List.mem(e.e_id, members),
      d.cells,
    );
  let held =
    List.exists(
      (e: Cell.t) =>
        List.mem(fid, Cell.covers(e)) || List.mem(fid, cell_ids(e)),
      keeping,
    );
  held
    ? None
    : {
      let base =
        List.fold_left(
          (seg, e) => Focus.splice_entry(e, seg),
          d.base,
          alone,
        );
      Focus.mk_run_entry(~info_map, fid, base)
      |> Option.map(entry =>
           {
             ...d,
             base,
             cells: insert(~term, entry, keeping),
             active: Some((entry.e_id, Body)),
           }
         );
    };
};

/* the tests container's toggle: one cell for the whole run, or close
   the run (or every member open individually) */
let toggle_run = (~info_map, ~term, fid: Id.t, d: t): after_close => {
  let covering =
    List.find_opt(
      (e: Cell.t) =>
        e.e_run && (e.e_id == fid || List.mem(fid, e.e_members)),
      d.cells,
    );
  switch (covering) {
  | Some(run) => close(run.e_id, d)
  | None =>
    let members =
      switch (Focus.test_run(fid, d.base)) {
      | Some((_, _, ms)) => ms
      | None => [fid]
      };
    let (open_members, keeping) =
      List.partition((e: Cell.t) => List.mem(e.e_id, members), d.cells);
    let base =
      List.fold_left(
        (seg, e) => Focus.splice_entry(e, seg),
        d.base,
        open_members,
      );
    let all_open =
      members != [] && List.length(open_members) == List.length(members);
    if (all_open) {
      switch (keeping) {
      | [] =>
        Joined(
          join({
            ...d,
            cells: [],
            base,
          }),
        )
      | _ =>
        Still({
          ...d,
          cells: keeping,
          base,
        })
      };
    } else {
      switch (Focus.mk_run_entry(~info_map, fid, base)) {
      | None => Still(d)
      | Some(entry) =>
        Still({
          ...d,
          base,
          cells: insert(~term, entry, keeping),
          active: Some((entry.e_id, Body)),
        })
      };
    };
  };
};

/* after an edit to the joined program (agent, outline menu): the same
   cells, cut from the edited program; cells whose item is gone close */
let resplit =
    (~info_map, ~term, editor: CellEditor.Model.t, d: t): after_close => {
  let base = Focus.zip_of_cell(editor);
  /* a cell the edit didn't touch keeps its editor (caret, selection):
     edited tokens get fresh ids, so equal id sequences mean equal text */
  let same = (a: Segment.t, b: Segment.t) =>
    Segment.ids(Focus.core_ws(a)) == Segment.ids(b);
  let untouched = (e: Cell.t): bool =>
    switch (Focus.cell_content(e, base)) {
    | None => false
    | Some(slice) =>
      same(slice, Focus.zip_of_cell(e.e_body))
      && (
        e.e_run
        || e.e_sym != None
        || (
          switch (Focus.find_pat(e.e_id, base)) {
          | Some(pat) => same(pat, Focus.zip_of_cell(e.e_header))
          | None => false
          }
        )
      )
    };
  let cells =
    List.filter_map(
      (e: Cell.t) =>
        if (untouched(e)) {
          Some(e);
        } else if (e.e_run) {
          Focus.mk_run_entry(~info_map, e.e_id, base);
        } else {
          entry(~info_map, ~sym=?e.e_sym, ~inner=e.e_inner, e.e_id, base);
        },
      d.cells,
    );
  switch (cells) {
  | [] => Joined(editor)
  | _ =>
    Still({
      shell: editor,
      cells: List.fold_left((acc, e) => insert(~term, e, acc), [], cells),
      base,
      active: d.active,
      statics: None,
    })
  };
};

/* same text, probes and carets: the base and every cell zipper are
   physically unchanged, and the same cell is active */
let same_content = (a: t, b: t): bool => {
  let zip = (c: CellEditor.Model.t) => c.editor.editor.state.zipper;
  a.base === b.base
  && a.active == b.active
  && List.length(a.cells) == List.length(b.cells)
  && List.for_all2(
       (x: Cell.t, y: Cell.t) =>
         zip(x.e_header) === zip(y.e_header)
         && zip(x.e_body) === zip(y.e_body),
       a.cells,
       b.cells,
     );
};

/* the whole program for the problems panel, its segment the current
   document (the shell's is stale); memoized: the panel caches on identity */
let outside_memo: ref(option((t, CodeEditable.Model.t))) = ref(None);
let outside_editor = (d: t): CodeEditable.Model.t =>
  switch (outside_memo^) {
  | Some((d', e))
      when
        same_content(d', d)
        && d'.statics === d.statics
        && d'.shell === d.shell => e
  | _ =>
    let e: CodeEditable.Model.t = {
      ...d.shell.editor,
      editor: {
        ...d.shell.editor.editor,
        syntax: {
          ...d.shell.editor.editor.syntax,
          segment: document(d),
        },
      },
      statics: statics(d),
    };
    outside_memo := Some((d, e));
    e;
  };

let map_cells = (f: Cell.t => Cell.t, d: t): t => {
  ...d,
  cells: List.map(f, d.cells),
};

let update_cell = (id: Id.t, f: Cell.t => Cell.t, d: t): t => {
  ...d,
  cells: List.map((e: Cell.t) => e.e_id == id ? f(e) : e, d.cells),
};

let set_active = (id: Id.t, side: side, d: t): t =>
  List.exists((e: Cell.t) => e.e_id == id, d.cells)
    ? {
      ...d,
      active: Some((id, side)),
    }
    : d;

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

/* an undo snapshot: the whole-program statics recompute on restore */
let compact = (f: CellEditor.Model.t => CellEditor.Model.t, d: t): t => {
  ...map_editors(f, d),
  statics: None,
};
