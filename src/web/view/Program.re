open Haz3lcore;
open Util;

/* A scratchpad's program: one editor, or divided into open cells.
   Whole-program readers go through [document] or [whole]. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Whole(CellEditor.Model.t)
  | Divided(Divided.t);

let document = (p: t): Segment.t =>
  switch (p) {
  | Whole(e) => ScratchFocus.zip_of_cell(e)
  | Divided(d) => Divided.document(d)
  };

let root = (p: t): Sort.t =>
  switch (p) {
  | Whole(e) => e.editor.editor.root
  | Divided(d) => Divided.root(d)
  };

let result = (p: t): EvalResult.Model.t =>
  switch (p) {
  | Whole(e) => e.result
  | Divided(d) => Divided.result(d)
  };

let statics = (p: t): CachedStatics.t =>
  switch (p) {
  | Whole(e) => e.editor.statics
  | Divided(d) => Divided.statics(d)
  };

let whole = (p: t): CellEditor.Model.t =>
  switch (p) {
  | Whole(e) => e
  | Divided(d) => Divided.join(d)
  };

/* [whole] for readers (views, the agent): joining re-measures the
   whole program, so a divided one joins once per content change */
let joined: Slot.t(Divided.t, CellEditor.Model.t) = Slot.mk();
let whole_memo = (p: t): CellEditor.Model.t =>
  switch (p) {
  | Whole(e) => e
  | Divided(d) =>
    let e =
      Slot.get(~same=Divided.same_content, joined, d, () =>
        Divided.join(~prev=?Option.map(snd, joined^), d)
      );
    {
      editor: {
        ...e.editor,
        statics: Divided.statics(d),
      },
      result: Divided.result(d),
    };
  };

/* (id, live header name) per open cell; runs answer for their members,
   and a headerless cell's top-level tiles for it (an edit can replace
   its root, the row's id, before the view follows) */
let focused_names = (p: t): list((Id.t, option(string))) =>
  switch (p) {
  | Whole(_) => []
  | Divided(d) =>
    List.concat_map(
      (e: ScratchCell.t) =>
        switch (e.e_kind) {
        | Run(ids) => List.map(id => (id, None), ids)
        | Def(_)
        | Members => [(e.e_id, ScratchCell.header_name(e))]
        | Headless(_) => [
            (e.e_id, None),
            ...List.filter_map(
                 fun
                 | Piece.Tile(t) when t.id != e.e_id => Some((t.id, None))
                 | _ => None,
                 ScratchFocus.zip_of_cell(e.e_body),
               ),
          ]
        },
      Divided.cells(d),
    )
  };

/* [p]'s editors with the parse and measure caches of [from]'s in the same
   place: the whole program's editor (a stack's shell is the one it was cut
   from) and each open cell's panes. Undo restores a program that shares
   [from]'s pieces wherever the step changed nothing, so they hit there */
let with_caches_of = (~from: t, p: t): t =>
  switch (from, p) {
  | (Whole(f), Whole(e)) =>
    Whole(CellEditor.Model.with_caches_of(~from=f, e))
  | (Whole(f), Divided(d)) =>
    Divided(Divided.with_shell_caches_of(~from=f, d))
  | (Divided(f), Whole(e)) => Whole(Divided.shell_caches_into(f, e))
  | (Divided(f), Divided(d)) => Divided(Divided.with_caches_of(~from=f, d))
  };

/* [p] showing [from]'s statics and run until it's analyzed, as typing
   leaves a program: undo and redo defer the analysis to the debounce */
let with_statics_of = (~from: t, p: t): t =>
  switch (from, p) {
  | (Whole(f), Whole(e)) =>
    Whole(CellEditor.Model.with_statics_of(~from=f, e))
  | (Whole(f), Divided(d)) =>
    Divided(Divided.with_shell_statics_of(~from=f, d))
  | (Divided(f), Whole(e)) => Whole(Divided.shell_statics_into(f, e))
  | (Divided(f), Divided(d)) => Divided(Divided.with_statics_of(~from=f, d))
  };

let of_close = (c: Divided.after_close): t =>
  switch (c) {
  | Still(d) => Divided(d)
  | Joined(e) => Whole(e)
  };

let probes = (p: t): Refractors.RefractorList.t =>
  switch (p) {
  | Whole(e) => e.editor.editor.state.zipper.refractors.manuals
  | Divided(d) => Divided.probes(d)
  };

/* the probes the program's run samples: a stack's cells', expanded multis
   too, as a whole editor's, and the manual probes its shell keeps */
let probe_ids = (p: t): Id.Map.t(unit) =>
  switch (p) {
  | Whole(e) =>
    CachedStatics.probe_ids_of_zipper(e.editor.editor.state.zipper)
  | Divided(d) =>
    List.fold_left(
      (acc, e: ScratchCell.t) =>
        List.fold_left(
          (acc, c: CellEditor.Model.t) =>
            Id.Map.union(
              (_, (), ()) => Some(),
              acc,
              CachedStatics.probe_ids_of_zipper(c.editor.editor.state.zipper),
            ),
          acc,
          [e.e_header, e.e_body],
        ),
      Id.Map.map(_ => (), Id.Map.of_list(Divided.probes(d))),
      Divided.cells(d),
    )
  };

/* the ⇓ probe's term, once placed: the program's last expression, in a
   stack the ⇒ cell's (or a world slide's members cell's) */
let tail_target = (p: t): option(Id.t) =>
  switch (p) {
  | Whole(e) => e.editor.editor.state.zipper.refractors.tail_target
  | Divided(d) =>
    Divided.cells(d)
    |> List.find_map((e: ScratchCell.t) =>
         switch (e.e_kind) {
         | Headless(s) when s == {js|⇒|js} =>
           Some(e.e_body.editor.editor.state.zipper.refractors.tail_target)
         | Members =>
           Option.map(
             Option.some,
             e.e_body.editor.editor.state.zipper.refractors.tail_target,
           )
         | _ => None
         }
       )
    |> Option.join
  };

/* the program's ⇒ row: its last expression, which the ⇓ toggle probes */
let tail_row = (p: t): option(Id.t) =>
  OutlineTree.of_term(statics(p).term)
  |> List.find_opt((n: OutlineTree.node) => n.o_kind == KTrail)
  |> Option.map((n: OutlineTree.node) => n.o_id)
  |> Option.join;
