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
        if (e.e_run) {
          List.map(id => (id, None), e.e_members);
        } else if (e.e_sym == None) {
          [(e.e_id, ScratchCell.header_name(e))];
        } else {
          [
            (e.e_id, None),
            ...List.filter_map(
                 fun
                 | Piece.Tile(t) when t.id != e.e_id => Some((t.id, None))
                 | _ => None,
                 ScratchFocus.zip_of_cell(e.e_body),
               ),
          ];
        },
      Divided.cells(d),
    )
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

let probe_ids = (p: t): Id.Map.t(unit) =>
  switch (p) {
  | Whole(e) =>
    CachedStatics.probe_ids_of_zipper(e.editor.editor.state.zipper)
  | Divided(d) =>
    List.fold_left(
      (acc, (id, _)) => Id.Map.add(id, (), acc),
      Id.Map.empty,
      Divided.probes(d),
    )
  };

/* the program's ⇒ row: its last expression, which the ⇓ toggle probes */
let tail_row = (p: t): option(Id.t) =>
  OutlineTree.of_term(statics(p).term)
  |> List.find_opt((n: OutlineTree.node) => n.o_kind == KTrail)
  |> Option.map((n: OutlineTree.node) => n.o_id)
  |> Option.join;
