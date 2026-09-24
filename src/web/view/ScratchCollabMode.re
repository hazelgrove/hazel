/* Applying the shared document's changes to the scratch model
   (docs/collab-modular.md). The pure conversions live in ScratchCollab;
   this module knows about the model: the master editor, the stack of open
   definition cells, and which of them an incoming change touches.

   - A leaf edited remotely while it's open in a local cell: the cell is
     rebuilt from the new text, caret mapped through the edit.
   - A leaf of an item that isn't open: spliced into the master (or the
     stack's frozen master copy), so the item tile keeps its id and only
     that item's statics rerun.
   - Items added, removed or reordered: the program is rebuilt from the
     items; open cells stay authoritative for their own items. */
open Haz3lcore;
open Util;
module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module C = ScratchCollab;
module Focus = ScratchFocus;

let tail_sym = {js|⇒|js};

/* the item a stack entry edits */
let entry_item = (e: Model.stack_entry): Id.t =>
  e.e_sym == Some(tail_sym) ? C.State.tail_id^ : e.e_id;

let kind_of_id = (id: Id.t): C.kind =>
  switch (List.find_opt(((it: C.item, _)) => it.id == id, C.State.synced^)) {
  | Some((it, _)) => it.kind
  | None => id == C.State.tail_id^ ? Tail : Def
  };

let cell_zipper = (cell: CellEditor.Model.t): Zipper.t =>
  cell.editor.editor.state.zipper;

let mk_cell = (~root, z: Zipper.t): CellEditor.Model.t =>
  z |> Editor.Model.mk(~root) |> CellEditor.Model.mk;

/* [cell] with its content replaced by [text], caret carried across */
let rebuild_cell =
    (~root, text: string, cell: CellEditor.Model.t): CellEditor.Model.t => {
  let z = cell_zipper(cell);
  let old_text = C.text_of_seg(Zipper.unselect_and_zip(z));
  if (old_text == text) {
    cell;
  } else {
    let off = C.map_offset(old_text, text, C.caret_offset(z));
    let seg = C.parse(~root, text);
    /* a fresh result too: the old one's decorations name ids that the
       re-parse replaced */
    mk_cell(
      ~root,
      C.with_caret_at(off, Zipper.unzip(~direction=Left, seg)),
    );
  };
};

let live_seg = (model: Model.t): option(Segment.t) => {
  let sp = List.nth(model.scratchpads, model.current);
  switch (sp.kind, model.focus) {
  | (Code(_), Some(f)) => Some(Focus.splice_all(f))
  | (Code({editor, _}), None) => Some(Focus.zip_of_cell(editor))
  | (Drv(_), _) => None
  };
};

/* replace the current slide's master editor, keeping the caret's place;
   [remap] carries an in-leaf caret through a change to that leaf */
let with_master =
    (
      ~remap=(_: Id.t, _: C.leaf, off: int) => off,
      model: Model.t,
      f: Segment.t => Segment.t,
    )
    : Model.t => {
  let sp = List.nth(model.scratchpads, model.current);
  switch (sp.kind) {
  | Drv(_) => model
  | Code({editor, agent}) =>
    let tail_id = C.State.tail_id^;
    let z = cell_zipper(editor);
    let anchor = C.anchor_of(~tail_id, z);
    let seg = f(Zipper.unselect_and_zip(z));
    /* a fresh result too: the old one's decorations name ids that the
       re-parse replaced */
    let editor =
      mk_cell(~root=Exp, C.zipper_at(~tail_id, ~remap, anchor, seg));
    {
      ...model,
      scratchpads:
        ListUtil.put_nth(
          model.current,
          {
            ...sp,
            kind:
              Code({
                editor,
                agent,
              }),
          },
          model.scratchpads,
        ),
    };
  };
};

/* apply [f] to the program: the stack's frozen master while stacked,
   otherwise the master editor */
let map_program =
    (~remap=?, model: Model.t, f: Segment.t => Segment.t): Model.t =>
  switch (model.focus) {
  | Some(fo) => {
      ...model,
      focus:
        Some({
          ...fo,
          f_master_seg: f(fo.f_master_seg),
        }),
    }
  | None => with_master(~remap?, model, f)
  };

let apply_leaf = (model: Model.t, (id: Id.t, leaf: C.leaf, text: string)) => {
  let kind = kind_of_id(id);
  let root = C.root_of(kind, leaf);
  let open_entry =
    switch (model.focus) {
    | Some(fo) =>
      List.find_opt(
        (e: Model.stack_entry) => entry_item(e) == id && !e.e_run,
        fo.f_entries,
      )
    | None => None
    };
  switch (model.focus, open_entry) {
  | (Some(fo), Some(e)) =>
    let e' =
      switch (leaf) {
      | Header => {
          ...e,
          e_header: rebuild_cell(~root, text, e.e_header),
        }
      | Body
      | Lead => {
          ...e,
          e_body: rebuild_cell(~root, text, e.e_body),
        }
      };
    {
      ...model,
      focus:
        Some({
          ...fo,
          f_entries:
            List.map(
              (x: Model.stack_entry) => x.e_id == e.e_id ? e' : x,
              fo.f_entries,
            ),
        }),
    };
  | _ =>
    let old_text =
      switch (
        List.find_opt(((it: C.item, _)) => it.id == id, C.State.synced^)
      ) {
      | Some((it, _)) => C.leaf_of(it, leaf)
      | None => text
      };
    let remap = (id', leaf', off) =>
      id' == id && leaf' == leaf ? C.map_offset(old_text, text, off) : off;
    let model = map_program(~remap, model, C.set_leaf(kind, id, leaf, text));
    /* a test-run cell spanning this statement: rebuild it from the new
       program (its caret resets) */
    switch (model.focus) {
    | Some(fo)
        when
          List.exists(
            (e: Model.stack_entry) => e.e_run && List.mem(id, e.e_members),
            fo.f_entries,
          ) =>
      let seg = fo.f_master_seg;
      let info_map = Language.Statics.Map.empty;
      {
        ...model,
        focus:
          Some({
            ...fo,
            f_entries:
              List.map(
                (e: Model.stack_entry) =>
                  e.e_run && List.mem(id, e.e_members)
                    ? Option.value(
                        Focus.mk_run_entry(~info_map, e.e_id, seg),
                        ~default=e,
                      )
                    : e,
                fo.f_entries,
              ),
          }),
      };
    | _ => model
    };
  };
};

/* all items as the doc now has them, in program order */
let current_items = (upserts: list(C.item), deletes: list(Id.t)) => {
  let base =
    C.State.synced^
    |> List.map(fst)
    |> List.filter((it: C.item) => !List.mem(it.id, deletes));
  let replaced =
    List.map(
      (it: C.item) =>
        switch (List.find_opt((u: C.item) => u.id == it.id, upserts)) {
        | Some(u) => u
        | None => it
        },
      base,
    );
  let added =
    List.filter(
      (u: C.item) => !List.exists((it: C.item) => it.id == u.id, base),
      upserts,
    );
  C.sort_items(replaced @ added);
};

let apply_structure =
    (model: Model.t, items: list(C.item), deletes: list(Id.t)): Model.t =>
  switch (C.seg_of_items(items)) {
  | None => model
  | Some(seg) =>
    let model = map_program(model, _ => seg);
    /* close cells whose item is gone */
    switch (model.focus) {
    | Some(fo) =>
      let entries =
        List.filter(
          (e: Model.stack_entry) => !List.mem(entry_item(e), deletes),
          fo.f_entries,
        );
      entries == []
        ? with_master(
            {
              ...model,
              focus: None,
            },
            _ => seg)
        : {
          ...model,
          focus:
            Some({
              ...fo,
              f_entries: entries,
            }),
        };
    | None => model
    };
  };

let load = (model: Model.t, l: C.Wire.load): Model.t => {
  C.State.active := true;
  C.State.load_seq := l.seq;
  Hashtbl.reset(C.State.bases);
  C.State.orders := Id.Map.empty;
  C.State.doc_ids := Id.Map.empty;
  C.State.synced := [];
  let items =
    l.items
    |> List.filter((w: C.Wire.item) => w.parent == None)
    |> List.map(C.remember)
    |> C.sort_items;
  switch (C.seg_of_items(items)) {
  | None => model
  | Some(seg) =>
    let model = {
      ...model,
      focus: None,
    };
    let sp = List.nth(model.scratchpads, model.current);
    switch (sp.kind) {
    | Drv(_) => model
    | Code({agent, _}) =>
      let fresh = Focus.cell_of_seg(seg);
      C.mark_synced(seg);
      {
        ...model,
        scratchpads:
          ListUtil.put_nth(
            model.current,
            {
              ...sp,
              kind:
                Code({
                  editor: fresh,
                  agent,
                }),
            },
            model.scratchpads,
          ),
      };
    };
  };
};

/* An incoming leaf text is taken only if we haven't edited that leaf since
   the message was made; otherwise it's stale, and if the merged text
   differs from ours a newer message follows (collab/src/session.ts). */
let remote = (model: Model.t, r: C.Wire.remote): Model.t => {
  let seq = r.seq;
  let deletes = List.map(C.id_of_string, r.deletes);
  let synced = List.map(fst, C.State.synced^);
  let known = (id: Id.t) =>
    List.find_opt((it: C.item) => it.id == id, synced);
  /* upserted items, with stale leaves replaced by what we have */
  let upserts =
    r.upserts
    |> List.filter((w: C.Wire.item) => w.parent == None)
    |> List.map((w: C.Wire.item) => {
         let u = C.remember(w);
         switch (known(u.id)) {
         | None =>
           List.iter(
             l => C.set_basis(u.id, l, seq),
             [C.Lead, Header, Body],
           );
           u;
         | Some(old) =>
           let pick = (leaf: C.leaf) =>
             if (C.fresh_for(u.id, leaf, seq)) {
               C.set_basis(u.id, leaf, seq);
               C.leaf_of(u, leaf);
             } else {
               C.leaf_of(old, leaf);
             };
           {
             ...u,
             lead: pick(Lead),
             header: pick(Header),
             body: pick(Body),
           };
         };
       });
  let leaves =
    r.leaves
    |> List.map((w: C.Wire.leaf_change) =>
         (C.id_of_string(w.id), C.leaf_of_string(w.leaf), w.text)
       )
    |> List.filter(((id, leaf, _)) =>
         known(id) != None && C.fresh_for(id, leaf, seq)
       );
  List.iter(((id, leaf, _)) => C.set_basis(id, leaf, seq), leaves);
  /* texts we already have (e.g. the echo of our own edit) change nothing:
     don't rebuild for them */
  let leaves =
    List.filter(
      ((id, leaf, text)) =>
        switch (known(id)) {
        | Some(it) => C.leaf_of(it, leaf) != text
        | None => true
        },
      leaves,
    );
  let items = current_items(upserts, deletes);
  let same_order =
    List.map((it: C.item) => it.id, synced)
    == List.map((it: C.item) => it.id, items);
  let model =
    if (same_order) {
      /* upserts of items we already have (e.g. the echo of our own insert
         or move) reduce to their changed leaves, if any */
      let upsert_leaves =
        List.concat_map(
          (u: C.item) =>
            switch (known(u.id)) {
            | Some(old) =>
              List.filter_map(
                leaf =>
                  C.leaf_of(old, leaf) == C.leaf_of(u, leaf)
                    ? None : Some((u.id, leaf, C.leaf_of(u, leaf))),
                [C.Lead, Header, Body],
              )
            | None => []
            },
          upserts,
        );
      List.fold_left(apply_leaf, model, upsert_leaves @ leaves);
    } else {
      /* items added, removed or reordered: rebuild, with the fresh leaf
         texts folded in first */
      let items =
        List.map(
          (it: C.item) =>
            List.fold_left(
              (it: C.item, (id, leaf, text)) =>
                id != it.id
                  ? it
                  : (
                    switch (leaf) {
                    | C.Lead => {
                        ...it,
                        lead: text,
                      }
                    | Header => {
                        ...it,
                        header: text,
                      }
                    | Body => {
                        ...it,
                        body: text,
                      }
                    }
                  ),
              it,
              leaves,
            ),
          items,
        );
      /* open cells are authoritative for their items: bring their fresh
         leaves in too */
      List.fold_left(
        apply_leaf,
        apply_structure(model, items, deletes),
        leaves,
      );
    };
  switch (live_seg(model)) {
  | Some(seg) => C.mark_synced(seg)
  | None => ()
  };
  model;
};

let apply = (model: Model.t, msg: C.msg): Model.t =>
  switch (msg) {
  | Load(l) => load(model, l)
  | Remote(r) => remote(model, r)
  | Peers(ps) =>
    C.State.peers := ps;
    {
      /* a fresh record, so the view re-renders the carets */

      ...model,
      current: model.current,
    };
  };

/* The local caret as (item, leaf, anchor, head), after an action on the
   master ([None]) or on stack cell [Some((i, is_header))]. */
let local_caret =
    (model: Model.t, target: option((int, bool)))
    : option((Id.t, C.leaf, int, int)) =>
  switch (target, model.focus) {
  | (Some((i, is_header)), Some(fo)) =>
    switch (List.nth_opt(fo.f_entries, i)) {
    | Some(e) when !e.e_run =>
      let cell = is_header ? e.e_header : e.e_body;
      let off = C.caret_offset(cell_zipper(cell));
      Some((entry_item(e), is_header ? Header : Body, off, off));
    | _ => None
    }
  | (None, None) =>
    let sp = List.nth(model.scratchpads, model.current);
    switch (sp.kind) {
    | Code({editor, _}) =>
      C.anchor_of(~tail_id=C.State.tail_id^, cell_zipper(editor))
      |> Option.map(((id, leaf, _) as a) => {
           let len =
             switch (
               List.find_opt(
                 ((it: C.item, _)) => it.id == id,
                 C.State.synced^,
               )
             ) {
             | Some((it, _)) => C.utf16_length(C.leaf_of(it, leaf))
             | None => 0
             };
           let (id, leaf, off) = C.leaf_position(a, ~len);
           (id, leaf, off, off);
         })
    | Drv(_) => None
    };
  | _ => None
  };

/* Carets are decoration: a failure to place one must never take down the
   editor's view. */
let guard = (what: string, f: unit => list(Virtual_dom.Vdom.Node.t)) =>
  switch (f()) {
  | nodes => nodes
  | exception e =>
    Js_of_ocaml.Firebug.console##warn(
      Js_of_ocaml.Js.string(
        "[hazel-collab] " ++ what ++ ": " ++ Printexc.to_string(e),
      ),
    );
    [];
  };

/* Peers' carets in a cell editing ([item], [leaf]), as overlay nodes. */
let peer_overlays_unguarded =
    (
      ~font_metrics: FontMetrics.t,
      ~item: Id.t,
      ~leaf: C.leaf,
      cell: CellEditor.Model.t,
    )
    : list(Virtual_dom.Vdom.Node.t) => {
  let doc_id = C.doc_id(item);
  let mine =
    List.filter(
      (p: C.Wire.peer) => p.id == doc_id && C.leaf_of_string(p.leaf) == leaf,
      C.State.peers^,
    );
  if (mine == []) {
    [];
  } else {
    let z = cell_zipper(cell);
    let measured = cell.editor.editor.syntax.measured;
    List.filter_map(
      (p: C.Wire.peer) =>
        switch (Zipper.Caret.point(measured, C.with_caret_at(p.head, z))) {
        | origin =>
          Some(
            RemoteCaretDec.main(
              ~user_id=p.peer,
              ~user_name=Some(p.name),
              ~font_metrics,
              ~color=p.color,
              ~origin,
            ),
          )
        | exception _ => None
        },
      mine,
    );
  };
};

let peer_overlays = (~font_metrics, ~item, ~leaf, cell) =>
  guard("cell carets", () =>
    peer_overlays_unguarded(~font_metrics, ~item, ~leaf, cell)
  );

/* Peers' carets in the whole-program editor. Resolving a caret walks the
   program, so the result is memoized on (peers, program) identity. */
let master_memo:
  ref(
    option((list(C.Wire.peer), Zipper.t, list(Virtual_dom.Vdom.Node.t))),
  ) =
  ref(None);

let master_peer_overlays_unguarded =
    (~font_metrics: FontMetrics.t, cell: CellEditor.Model.t)
    : list(Virtual_dom.Vdom.Node.t) => {
  let peers = C.State.peers^;
  let z = cell_zipper(cell);
  switch (master_memo^) {
  | Some((ps, z', nodes)) when ps === peers && z' === z => nodes
  | _ =>
    let nodes =
      if (peers == [] || ! C.State.active^) {
        [];
      } else {
        let seg = Zipper.unselect_and_zip(z);
        let ranges = C.leaf_ranges(~tail_id=C.State.tail_id^, seg);
        let base = Zipper.unzip(~direction=Left, seg);
        let measured = cell.editor.editor.syntax.measured;
        List.filter_map(
          (p: C.Wire.peer) => {
            let id = C.id_of_string(p.id);
            let leaf = C.leaf_of_string(p.leaf);
            switch (
              List.find_opt(
                (r: C.range) => r.r_id == id && r.r_leaf == leaf,
                ranges,
              )
            ) {
            | Some(r) =>
              let g = min(r.r_start + p.head, r.r_stop);
              switch (Zipper.Caret.point(measured, C.with_caret_at(g, base))) {
              | origin =>
                Some(
                  RemoteCaretDec.main(
                    ~user_id=p.peer,
                    ~user_name=Some(p.name),
                    ~font_metrics,
                    ~color=p.color,
                    ~origin,
                  ),
                )
              | exception _ => None
              };
            | None => None
            };
          },
          peers,
        );
      };
    master_memo := Some((peers, z, nodes));
    nodes;
  };
};

let master_peer_overlays = (~font_metrics, cell) =>
  guard("master carets", () =>
    master_peer_overlays_unguarded(~font_metrics, cell)
  );
