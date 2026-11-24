open Haz3lcore;
open Util;
open Poly;

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;

/* Per-slide IndexedDB persistence. Each scratchpad's editor and agent
   data is stored as separate HazelDB KV keys, so autosave only writes
   the current slide.

   Key layout, with <k> = <prefix>:<name>:
     <prefix>:_meta  → slide_meta (current_index, names)
     <k>             → CellEditor.Model.persistent
     <k>:agent       → Agent.Persistent.t
     <k>:side        → what the slide shows beyond its text
     <k>:items:…     → per-item slices and their roster (ItemPersist) */

/* per-slide tables below key on this, never the bare name: a Scratch
   and a Documentation slide may share a name */
let content_key = (prefix: string, name: string): string =>
  prefix ++ ":" ++ name;

/* the outline's folds per slide, by occurrence-qualified label path;
   modeled because <details> DOM state bleeds across slides and resets
   when the vdom recreates elements */
let slide_folds: Stdlib.Hashtbl.t(string, OutlineTree.folds) =
  Stdlib.Hashtbl.create(8);

let folds = (prefix: string, name: string): OutlineTree.folds =>
  switch (Stdlib.Hashtbl.find_opt(slide_folds, content_key(prefix, name))) {
  | Some(fs) => fs
  | None => []
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type slide_meta = {
  current: int,
  names: list(string),
};

let meta_key = (prefix: string): string => prefix ++ ":_meta";
let agent_key = (prefix: string, name: string): string =>
  content_key(prefix, name) ++ ":agent";

/* what a slide shows beyond its text: the caret (text saves lose it),
   pins and zoom by outline label path, the rows folded and opened by
   hand, and manual probes (they live in zippers, which the item store
   doesn't carry) */
[@deriving sexp]
type side = {
  caret: option(Point.t),
  pins: list((OutlineTree.path, bool)),
  zoom: option(OutlineTree.path),
  parked: bool,
  collapse: list(OutlineTree.path),
  expand: list(OutlineTree.path),
  probes: Refractors.RefractorList.t,
};

/* the folds as the side keeps them: the rows folded, the rows opened */
let folds_of_side = (side: side): OutlineTree.folds =>
  List.map(~f=p => (p, true), side.collapse)
  @ List.map(~f=p => (p, false), side.expand);

let with_folds = (folds: OutlineTree.folds, side: side): side => {
  let paths = f =>
    List.filter_map(~f=((p, f')) => f' == f ? Some(p) : None);
  {
    ...side,
    collapse: paths(true, folds),
    expand: paths(false, folds),
  };
};

/* a world slide's module, by outline path (WorldSlide): what the side
   stores is the slide's own, its paths below this one */
let world_roots: Hashtbl.t(string, OutlineTree.path) = Hashtbl.create(8);

/* [path] as the slide's own: None when it's outside the slide */
let out_of_world =
    (root: OutlineTree.path, path: OutlineTree.path)
    : option(OutlineTree.path) => {
  let n = List.length(root);
  List.length(path) >= n
  && List.for_all2((==), root, Util.ListUtil.take(n, path))
    ? Some(snd(Util.ListUtil.split_n(n, path))) : None;
};

/* the live folds as stored: a world slide's, its own */
let stored_folds = (ck: string, folds: OutlineTree.folds): OutlineTree.folds =>
  switch (Hashtbl.find_opt(world_roots, ck)) {
  | Some(root) =>
    List.filter_map(
      ((path, f)) => Option.map(p => (p, f), out_of_world(root, path)),
      folds,
    )
  | None => folds
  };

let side_key = (prefix: string, name: string): string =>
  content_key(prefix, name) ++ ":side";

let no_side: side = {
  caret: None,
  pins: [],
  zoom: None,
  parked: false,
  collapse: [],
  expand: [],
  probes: [],
};

let read_side = (prefix: string, name: string): side =>
  HazelDB.kv_get_sexp(side_key(prefix, name), side_of_sexp)
  |> Option.value(~default=no_side);

/* the last side record written per slide: the next write's base (what it
   doesn't update carries over) and its change gate */
let last_side: Stdlib.Hashtbl.t(string, (side, string)) =
  Stdlib.Hashtbl.create(8);

let write_side = (prefix: string, name: string, update: side => side): unit => {
  let ck = content_key(prefix, name);
  let (prev, prev_s) =
    switch (Stdlib.Hashtbl.find_opt(last_side, ck)) {
    | Some(last) => last
    | None => (read_side(prefix, name), "")
    };
  let side = update(prev);
  let s = Sexplib.Sexp.to_string(sexp_of_side(side));
  if (s != prev_s) {
    Stdlib.Hashtbl.replace(last_side, ck, (side, s));
      HazelDB.kv_save(side_key(prefix, name), s);
  };
};

/* the stored probes whose anchors are still in [seg] */
let read_probes =
    (prefix: string, name: string, seg: Segment.t): Refractors.RefractorList.t => {
  let present = Segment.ids(seg);
  List.filter(
    ~f=((id, _)) => List.mem(present, id, ~equal=Poly.equal),
    read_side(prefix, name).probes,
  );
};

/* restores awaiting hydration, per slide; a load that finds nothing
   clears its entry, so no leftover survives a failed read */
let pending_caret: Stdlib.Hashtbl.t(string, Point.t) =
  Stdlib.Hashtbl.create(8);
let pending_pins: Stdlib.Hashtbl.t(string, side) = Stdlib.Hashtbl.create(8);

let stage_side = (prefix: string, name: string): unit => {
  let ck = content_key(prefix, name);
  let side = read_side(prefix, name);
  switch (side.caret) {
  | Some(p) => Stdlib.Hashtbl.replace(pending_caret, ck, p)
  | None => Stdlib.Hashtbl.remove(pending_caret, ck)
  };
  List.is_empty(side.pins) && Option.is_none(side.zoom)
    ? Stdlib.Hashtbl.remove(pending_pins, ck)
    : Stdlib.Hashtbl.replace(pending_pins, ck, side);
  switch (folds_of_side(side)) {
  | [] => Stdlib.Hashtbl.remove(slide_folds, ck)
  | folds => Stdlib.Hashtbl.replace(slide_folds, ck, folds)
  };
};

/* the saved view against the loaded program's outline */
let resolve_view = (saved: side, outline: OutlineTree.t): SlideView.t => {
  zoom:
    switch (
      Option.bind(saved.zoom, ~f=path =>
        OutlineTree.resolve_path(path, outline)
      )
    ) {
    | Some(m) =>
      OutlineTree.trail_of(m, outline) |> Option.value(~default=[])
    | None => []
    },
  pins:
    List.filter_map(
      ~f=
      ((path, run)) =>
        OutlineTree.resolve_path(path, outline)
          |> Option.map(~f=id =>
             SlideView.{
               p_id: id,
               p_run: run,
             }
           ),
      saved.pins,
    ),
  parked: saved.parked,
};

let write_folds = (prefix: string, name: string): unit =>
  write_side(
    prefix,
    name,
    with_folds(
      stored_folds(content_key(prefix, name), folds(prefix, name)),
    ),
  );

/* a world slide's staged view in the world's paths, under [root] (the
   slide's module) and zoomed to it at least: the side stores the slide's
   own. Its folds go there too, once */
let into_world = (ck: string, root: OutlineTree.path, side: side): side => {
  if (!Hashtbl.mem(world_roots, ck)) {
    Hashtbl.replace(world_roots, ck, root);
    switch (Hashtbl.find_opt(slide_folds, ck)) {
    | Some(folds) =>
      Hashtbl.replace(
        slide_folds,
        ck,
        List.map(((p, f)) => (root @ p, f), folds),
      )
    | None => ()
    };
  };
  {
    ...side,
    pins: List.map(((p, run)) => (root @ p, run), side.pins),
    zoom: Some(root @ Option.value(side.zoom, ~default=[])),
  };
};

let save_meta = (prefix: string, m: slide_meta): unit =>
  HazelDB.kv_save_sexp(meta_key(prefix), sexp_of_slide_meta, m);

let load_meta = (prefix: string): option(slide_meta) =>
  HazelDB.kv_get_sexp(meta_key(prefix), slide_meta_of_sexp);

let save_slide_kind =
    (prefix: string, name: string, kind: Scratchpad.kind_persistent): unit =>
  HazelDB.kv_save_sexp(
    content_key(prefix, name),
    Scratchpad.sexp_of_kind_persistent,
    kind,
  );

/* Load a slide blob. Tries the new schema first; on parse failure,
   falls back to legacy CellEditor-only blobs and wraps them as a Code kind. */
let load_slide_kind =
    (prefix: string, name: string): option(Scratchpad.kind_persistent) =>
  switch (HazelDB.kv_get(content_key(prefix, name))) {
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
    (prefix: string, name: string, agent: Agent.Persistent.t): unit =>
  HazelDB.kv_save_sexp(
    agent_key(prefix, name),
    Agent.Persistent.sexp_of_t,
    agent,
  );

let load_agent = (prefix: string, name: string): option(Agent.Persistent.t) =>
  HazelDB.kv_get_sexp(agent_key(prefix, name), Agent.Persistent.t_of_sexp);

/* Change-gate for agent saves: serializing a long conversation on
   every editor autosave is the expensive part, so skip when the agent
   model is physically unchanged (edits rebuild the scratchpad record
   but reuse the agent field). */
let last_saved_agent: Stdlib.Hashtbl.t(string, Agent.Model.t) =
  Stdlib.Hashtbl.create(8);
let last_agent_save_ts: Stdlib.Hashtbl.t(string, float) =
  Stdlib.Hashtbl.create(8);

/* the same gate for the editor blob, so an idle autosave serializes
   nothing: the zipper's content, or a divided program's cells' (caret
   moves count, for the saved caret) */
type save_stamp =
  | Unstacked(Zipper.t)
  | Stacked(Divided.t);
let last_saved_content: Stdlib.Hashtbl.t(string, save_stamp) =
  Stdlib.Hashtbl.create(8);

/* per-item persistence, the primary restore: item slices as sexps plus a
   roster, so autosave writes only touched items and reload restores the
   exact zipper unparsed; the text blob is the fallback for a bad roster */
let items_ns = (prefix: string, name: string): string =>
  content_key(prefix, name) ++ ":items:";

let item_store = (prefix: string, name: string): ItemPersist.store => {
  let ns = items_ns(prefix, name);
  {
    get: k => HazelDB.kv_get(ns ++ k),
    set: (k, v) => HazelDB.kv_save(ns ++ k, v),
    remove: k => HazelDB.kv_remove(ns ++ k),
  };
};

/* previously-saved item slices per content key: pieces are shared
   across ticks when unchanged, so dirtiness is a pointer walk */
let last_item_saves: Stdlib.Hashtbl.t(string, ItemPersist.saved) =
  Stdlib.Hashtbl.create(8);

let save_items = (prefix: string, name: string, z: Zipper.t): unit => {
  let ck = content_key(prefix, name);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let prev =
    Stdlib.Hashtbl.find_opt(last_item_saves, ck) |> Option.value(~default=[]);
  let saved = ItemPersist.save(~store=item_store(prefix, name), ~prev, seg);
  Stdlib.Hashtbl.replace(last_item_saves, ck, saved);
};

/* the keys a slide owns, exactly: its content key, its side keys, its item
   store. a name may itself contain `:`, so no prefix match */
let side_suffixes = [":agent", ":side"];
let slide_suffix = (ck: string, k: string): option(string) => {
  let n = String.length(ck);
  if (k == ck) {
    Some("");
  } else if (String.length(k) > n && String.sub(k, ~pos=0, ~len=n) == ck) {
    let rest = String.sub(k, ~pos=n, ~len=String.length(k) - n);
    List.mem(side_suffixes, rest, ~equal=Poly.equal)
    || String.is_prefix(~prefix=":items:", rest)
      ? Some(rest) : None;
  } else {
    None;
  };
};

/* the save gates under a slide's keys: once its keys move or go, the next
   autosave (or an undo bringing the slide back) must write everything */
let forget_gates = (ck: string): unit => {
  let drop = tbl =>
    Stdlib.Hashtbl.fold(
      (k, _, ks) => Option.is_none(slide_suffix(ck, k)) ? ks : [k, ...ks],
      tbl,
      [],
    )
    |> List.iter(~f=Stdlib.Hashtbl.remove(tbl));
  drop(last_side);
  drop(last_saved_agent);
  drop(last_agent_save_ts);
  drop(last_saved_content);
  drop(last_item_saves);
};

/* a deleted slide leaves nothing a later slide of the same name could
   pick up: stored keys, folds, pending restores, save gates */
let forget_slide = (prefix: string, name: string): unit => {
  let ck = content_key(prefix, name);
  HazelDB.kv_remove_where(k => Option.is_some(slide_suffix(ck, k)));
  Stdlib.Hashtbl.remove(slide_folds, ck);
  Stdlib.Hashtbl.remove(pending_pins, ck);
  Stdlib.Hashtbl.remove(pending_caret, ck);
  forget_gates(ck);
};

let rename_slide = (prefix: string, old_name: string, new_name: string): unit =>
  if (old_name != new_name) {
    let (old_ck, new_ck) = (
      content_key(prefix, old_name),
      content_key(prefix, new_name),
    );
    HazelDB.kv_rekey(k =>
      Option.map(~f=rest => new_ck ++ rest, slide_suffix(old_ck, k))
    );
    switch (Stdlib.Hashtbl.find_opt(slide_folds, old_ck)) {
    | Some(folds) => Stdlib.Hashtbl.replace(slide_folds, new_ck, folds)
    | None => Stdlib.Hashtbl.remove(slide_folds, new_ck)
    };
    Stdlib.Hashtbl.remove(slide_folds, old_ck);
    forget_gates(old_ck);
    forget_gates(new_ck);
  };
let stamp_equal = (a: save_stamp, b: save_stamp): bool =>
  switch (a, b) {
  | (Unstacked(x), Unstacked(y)) => Zipper.same_content(x, y)
  | (Stacked(x), Stacked(y)) => Divided.same_content(x, y)
  | _ => false
  };

let save_current' = (prefix: string, model: Model.t): unit => {
  let names = Model.scratchpad_names(model);
  save_meta(
    prefix,
    {
      current: model.current,
      names,
    },
  );
  let sp = List.nth_exn(model.scratchpads, model.current);
  switch (sp.dormant, sp.kind) {
  | (true, _) => () /* never write a placeholder over the stored slide */
  | (false, Code({program, agent, view, world, _})) =>
    let stamp =
      switch (program) {
      | Divided(d) => Stacked(d)
      | Whole(editor) => Unstacked(editor.editor.editor.state.zipper)
      };
    let ck = content_key(prefix, sp.name);
    let content_unchanged =
      switch (Stdlib.Hashtbl.find_opt(last_saved_content, ck)) {
      | Some(prev) => stamp_equal(prev, stamp)
      | None => false
      };
    let outline = Program.outline(program);
    /* a world slide's module, as the outline paths it */
    let root =
      Option.bind(world, slide => OutlineTree.label_path(slide, outline));
    Option.iter(Hashtbl.replace(world_roots, ck), root);
    let own = path =>
      switch (root) {
      | Some(root) => out_of_world(root, path)
      | None => Some(path)
      };
    /* what is stored: the program, or a world's slide members with
       their probes (None: the slide's module is gone) */
    let stored =
      switch (world) {
      | None =>
        Some((
          switch (program) {
          | Whole(editor) => editor.editor.editor.state.zipper
          | Divided(d) =>
            Divided.document(d)
            |> Zipper.unzip
            |> ZipperBase.update_refractors(_, r =>
                 Refractors.{
                   ...r,
                   manuals: Divided.probes(d),
                 }
               )
          },
          Program.root(program),
        ))
      | Some(slide) =>
        (
          switch (WorldTree.body(~slide, Program.document(program))) {
          | Loaded(members) => Some(members)
          | Unloaded
          | Gone => None
          }
        )
        |> Option.map(members => {
             let ids = Segment.ids(members);
             (
               Zipper.unzip(members)
               |> ZipperBase.update_refractors(_, r =>
                    Refractors.{
                      ...r,
                      manuals:
                        List.filter(
                          ((id, _)) => List.mem(id, ids),
                          Program.probes(program),
                        ),
                    }
                  ),
               Sort.Mod,
             );
           })
      };
    switch (stored) {
    | Some((z, stored_root)) when !content_unchanged =>
      Hashtbl.replace(last_saved_content, ck, stamp);
      /* text, without a live editor (syntax init and zipper sexps are
         slow) */
      save_items(prefix, sp.name, z);
      /* the slide blob carries the editor only; the conversation lives
         solely under the :agent key */
      save_slide_kind(
        prefix,
        sp.name,
        CodePersist({
          editor:
            Some(
              CellEditor.Model.{
                editor:
                  Editor.Model.mk_persistent(
                    PersistentZipper.of_text(
                      PersistentZipper.to_string(z) ++ "\n",
                    ),
                    /* the editor's own root: a Mod-rooted slide saved as
                       Exp reloads as an expression and wedges */
                    ~root=stored_root,
                  ),
                result: EvalResult.Model.persist(Program.result(program)),
              },
            ),
          agent: Agent.Persistent.persist(Agent.Utils.init()),
        }),
      );
    | Some(_) => ()
    | None => print_endline("scratch: " ++ sp.name ++ " lost its module")
    };
    /* the folds carry over: they're written as they change */
    write_side(prefix, sp.name, side =>
      {
        ...side,
        caret:
          switch (world, program) {
          /* text saves lose it, so it's restored after hydration */
          | (None, Whole(editor)) when !content_unchanged =>
            try(
              Some(
                Zipper.Caret.point(
                  editor.editor.editor.syntax.measured,
                  editor.editor.editor.state.zipper,
                ),
              )
            ) {
            | _ => side.caret
            }
          /* a world slide's is in its members' cell */
          | (Some(slide), Divided(d)) =>
            switch (Divided.active(d), Divided.cell(slide, d)) {
            | (Some((id, Body)), Some(e)) when id == slide =>
              try(
                Some(
                  Zipper.Caret.point(
                    e.e_body.editor.editor.syntax.measured,
                    e.e_body.editor.editor.state.zipper,
                  ),
                )
              ) {
              | _ => side.caret
              }
            | _ => side.caret
            }
          | _ => side.caret
          },
        probes:
          switch (stored) {
          | Some((z, _)) when !content_unchanged => z.refractors.manuals
          | _ => side.probes
          },
        /* the view can change without the text (a parked pin dropped) */
        pins:
          List.filter_map(
            ~f=
            (p: SlideView.pin) =>
              Option.bind(OutlineTree.label_path(p.p_id, outline), own)
              |> Option.map(path => (path, p.p_run)),
            view.pins,
          ),
        zoom:
          Option.bind(SlideView.zoom_root(view), m =>
            Option.bind(OutlineTree.label_path(m, outline), own)
          )
          /* zoomed to the slide's module: its whole */
          |> (
            fun
            | Some([]) => None
            | z => z
          ),
        parked: view.parked,
      }
    );
    /* the agent model changes on every streamed chunk, so a physical
       equality gate saved (and serialized, several MB) many times a
       second while the model spoke; gate on the fields that persist */
    let unchanged =
      switch (Stdlib.Hashtbl.find_opt(last_saved_agent, ck)) {
      | Some(prev) =>
        let prev: Agent.Model.t = prev;
        phys_equal(prev, agent)
        || phys_equal(prev.chat_system, agent.chat_system)
        && phys_equal(prev.prompting, agent.prompting)
        && prev.active_timeline_node == agent.active_timeline_node
        && prev.awaiting_response == agent.awaiting_response;
      | None => false
      };
    /* while the agent works (tools landing every few hundred ms) one save
       per 10 s is enough; the final save comes when it goes idle */
    let busy =
      Option.is_some(agent.awaiting_response)
      || Option.is_some(agent.pending_dispatch_send);
    let now = JsUtil.timestamp();
    let recently =
      switch (Stdlib.Hashtbl.find_opt(last_agent_save_ts, ck)) {
      | Some(t) => now -. t < 10000.
      | None => false
      };
    if (!unchanged && !(busy && recently)) {
      save_agent(prefix, sp.name, Agent.Persistent.persist(agent));
      Stdlib.Hashtbl.replace(last_saved_agent, ck, agent);
      Stdlib.Hashtbl.replace(last_agent_save_ts, ck, now);
    };
  | (false, Drv(m)) =>
    save_slide_kind(
      prefix,
      sp.name,
      DrvPersist(
        DerivationExerciseMode.Model.persist(m, ~instructor_mode=false),
      ),
    )
  };
};

/* a save lands whole or not at all: its items, roster, text and side
   keys go to storage in one transaction */
let save_current = (prefix: string, model: Model.t): unit =>
  HazelDB.kv_batch(() => save_current'(prefix, model));

/* the stored slide [name]'s editor over [seg] (at [root]): in the
   library's world when it has a member form (WorldSlide), its view staged
   under the slide's module, else as it is */
let open_slide =
    (
      ~settings,
      ~prefix,
      ~name,
      ~root,
      ~probes,
      persisted: CellEditor.Model.persistent,
      seg: Segment.t,
    )
    : (CellEditor.Model.t, option(Id.t)) => {
  let zipper = (seg, root) =>
    Zipper.unzip(~direction=Left, seg)
    |> Zipper.remold_regrout(Right, ~root)
    |> ZipperBase.update_refractors(_, r =>
         Refractors.{
           ...r,
           manuals: probes,
         }
       );
  switch (WorldSlide.enter(~name, ~root, seg)) {
  | Some((doc, slide)) =>
    /* its view restores once its outline is known, zoomed to its module
       at least (into_world) */
    Hashtbl.replace(
      pending_pins,
      content_key(prefix, name),
      read_side(prefix, name),
    );
    (
      CellEditor.Model.unpersist_with(
        ~settings,
        ~t_cache=WorldSlide.parse_cache,
        ~m_cache=WorldSlide.layout_cache,
        ~zipper=zipper(doc, Mod),
        {
          ...persisted,
          editor: {
            ...persisted.editor,
            root: Mod,
          },
        },
      ),
      Some(slide),
    );
  | None => (
      CellEditor.Model.unpersist_with(
        ~settings,
        ~zipper=zipper(seg, root),
        persisted,
      ),
      None,
    )
  };
};

/* the slide [name]'s program as stored (its segment, root and probes),
   without an editor; None when nothing is stored */
let stored =
    (prefix: string, name: string)
    : option((Segment.t, Sort.t, Refractors.RefractorList.t)) =>
  switch (load_slide_kind(prefix, name)) {
  | Some(CodePersist({editor: Some(e), _})) =>
    let root = e.editor.root;
    switch (ItemPersist.load(~store=item_store(prefix, name))) {
    | Some(seg) => Some((seg, root, read_probes(prefix, name, seg)))
    | None =>
      let z = PersistentZipper.unpersist(e.editor.zipper, ~root);
      Some((
        Zipper.unselect_and_zip(~erase_buffer=true, z),
        root,
        z.refractors.manuals,
      ));
    };
  | _ => None
  };

/* [open_slide] over a stored text */
let open_text =
    (~settings, ~prefix, ~name, persisted: CellEditor.Model.persistent) => {
  let root = persisted.editor.root;
  let z = PersistentZipper.unpersist(persisted.editor.zipper, ~root);
  open_slide(
    ~settings,
    ~prefix,
    ~name,
    ~root,
    ~probes=z.refractors.manuals,
    persisted,
    Zipper.unselect_and_zip(~erase_buffer=true, z),
  );
};

let load_scratchpad = (~settings, prefix: string, name: string): Scratchpad.t => {
  stage_side(prefix, name);
  Hashtbl.remove(world_roots, content_key(prefix, name));
  switch (load_slide_kind(prefix, name)) {
  | Some(CodePersist({editor: e, agent})) =>
    let persisted =
      switch (e) {
      | Some(e) => e
      | None => Init.default_documentation_slide_name(name)
      };
    /* per-item restore, else the whole-doc text */
    let (editor, world) =
      switch (ItemPersist.load(~store=item_store(prefix, name))) {
      | Some(seg) =>
        /* prime the dirty cache: the first autosave tick after a load
           should rewrite nothing */
        Hashtbl.replace(
          last_item_saves,
          content_key(prefix, name),
          ItemPersist.items_of(seg),
        );
        open_slide(
          ~settings,
          ~prefix,
          ~name,
          ~root=persisted.editor.root,
          ~probes=read_probes(prefix, name, seg),
          persisted,
          seg,
        );
      | None =>
        /* the text copy re-mints ids, so no stored item slice can be
           reused: drop them rather than leave them */
        let ns = items_ns(prefix, name);
        HazelDB.kv_remove_where(k => String.starts_with(~prefix=ns, k));
        open_text(~settings, ~prefix, ~name, persisted);
      };
    Scratchpad.mk_code(
      ~name,
      ~agent=
        Agent.Persistent.unpersist(
          Option.value(load_agent(prefix, name), ~default=agent),
        ),
      ~editor,
      ~world?,
      (),
    );
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
      let (editor, world) =
        open_text(
          ~settings,
          ~prefix,
          ~name,
          Init.default_documentation_slide_name(name),
        );
      Scratchpad.mk_code(
        ~name,
        ~editor,
        ~agent=?
          Option.map(Agent.Persistent.unpersist, load_agent(prefix, name)),
        ~world?,
        (),
      );
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
        ~f=
        (i, name) =>
          i == current
            ? load_scratchpad(~settings, prefix, name)
            : Scratchpad.dormant_code(name),
        names,
      ),
  };
};

/* Swap the placeholder at [current] for the real slide, if dormant. */
let hydrate_current = (~settings, prefix: string, model: Model.t): Model.t => {
  let sp = List.nth_exn(model.scratchpads, model.current);
  if (sp.dormant) {
    Model.map_current(
      _ => load_scratchpad(~settings, prefix, sp.name),
      model,
    );
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
      ~f=
      name =>
        switch (load_slide_kind(prefix, name)) {
        | Some(CodePersist({editor, agent})) =>
          Scratchpad.{
            name,
            kind:
              CodePersist({
                editor,
                agent:
                  Option.value(load_agent(prefix, name), ~default=agent),
              }),
          }
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
      List.map(~f=(sp: Scratchpad.persistent) => sp.name, scratchpads);
    /* an import replaces every slide: stale per-item saves would win
       over the imported text on load */
    let old_names =
      switch (load_meta(prefix)) {
      | Some(m) => m.names
      | None => []
      };
    List.iter(~f=forget_slide(prefix), old_names @ names);
    save_meta(
      prefix,
      {
        current,
        names,
      },
    );
    List.iter(
      ~f=
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
  | _ => print_endline("ScratchPersist.import_all: error")
  };
