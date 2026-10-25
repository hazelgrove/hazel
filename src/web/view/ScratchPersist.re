open Haz3lcore;
open Util;

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
let slide_folds: Hashtbl.t(string, OutlineTree.folds) = Hashtbl.create(8);

let folds = (prefix: string, name: string): OutlineTree.folds =>
  switch (Hashtbl.find_opt(slide_folds, content_key(prefix, name))) {
  | Some(fs) => fs
  | None => []
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type slide_meta = {
  current: int,
  names: list(string),
  /* The default slide names this browser has already been offered.
     It is what lets reconcile_names tell a slide the user has never
     seen from one they deleted on purpose. Empty means "not recorded
     yet" -- a meta written before reconciliation existed, or one an
     import wrote; the defaults list is never itself empty, so the two
     cases do not collide. */
  [@sexp.default []] [@yojson.default []]
  known_defaults: list(string),
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
  [@sexp.default []]
  expand: list(OutlineTree.path),
  probes: Refractors.RefractorList.t,
  /* the outline's mode, and the other mode's pins and parked (dev mode,
     UX's pins never set, for a side saved before the modes) */
  [@sexp.default false]
  ux: bool,
  [@sexp.option]
  away: option(list((OutlineTree.path, bool))),
  [@sexp.default false]
  away_parked: bool,
};

/* the folds as the side keeps them: the rows folded, the rows opened */
let folds_of_side = (side: side): OutlineTree.folds =>
  List.map(p => (p, true), side.collapse)
  @ List.map(p => (p, false), side.expand);

let with_folds = (folds: OutlineTree.folds, side: side): side => {
  let paths = f => List.filter_map(((p, f')) => f' == f ? Some(p) : None);
  {
    ...side,
    collapse: paths(true, folds),
    expand: paths(false, folds),
  };
};

let side_key = (prefix: string, name: string): string =>
  content_key(prefix, name) ++ ":side";

/* the keys :side replaced, read while it is missing */
let legacy_suffixes = [
  ":caret",
  ":pins",
  ":view",
  ":mode",
  ":collapse",
  ":probes",
];
[@deriving of_sexp]
type legacy_pin = {
  pin_path: OutlineTree.path,
  pin_run: bool,
};
[@deriving of_sexp]
type legacy_view = {
  vf_zoom: option(OutlineTree.path),
  vf_parked: bool,
};
[@deriving of_sexp]
type legacy_mode = {
  mf_ux: bool,
  mf_away: option(list(legacy_pin)),
  mf_away_parked: bool,
};

let read_side = (prefix: string, name: string): side =>
  switch (HazelDB.kv_get_sexp(side_key(prefix, name), side_of_sexp)) {
  | Some(side) => side
  | None =>
    let ck = content_key(prefix, name);
    let get = (suffix, of_sexp, default) =>
      HazelDB.kv_get_sexp(ck ++ suffix, of_sexp) |> Option.value(~default);
    let view = HazelDB.kv_get_sexp(ck ++ ":view", legacy_view_of_sexp);
    let mode = HazelDB.kv_get_sexp(ck ++ ":mode", legacy_mode_of_sexp);
    {
      caret:
        Option.bind(HazelDB.kv_get(ck ++ ":caret"), txt =>
          Scanf.sscanf_opt(txt, " %d %d", (row, col) => Point.mk(~row, ~col))
        ),
      pins:
        get(":pins", list_of_sexp(legacy_pin_of_sexp), [])
        |> List.map(p => (p.pin_path, p.pin_run)),
      zoom: Option.bind(view, v => v.vf_zoom),
      parked: Option.fold(~none=false, ~some=v => v.vf_parked, view),
      collapse: get(":collapse", list_of_sexp(OutlineTree.path_of_sexp), []),
      expand: [],
      probes: get(":probes", Refractors.RefractorList.t_of_sexp, []),
      ux: Option.fold(~none=false, ~some=m => m.mf_ux, mode),
      away:
        Option.bind(mode, m =>
          Option.map(List.map(p => (p.pin_path, p.pin_run)), m.mf_away)
        ),
      away_parked:
        Option.fold(~none=false, ~some=m => m.mf_away_parked, mode),
    };
  };

/* the last side record written per slide: the next write's base (what it
   doesn't update carries over) and its change gate */
let last_side: Hashtbl.t(string, (side, string)) = Hashtbl.create(8);

let write_side = (prefix: string, name: string, update: side => side): unit => {
  let ck = content_key(prefix, name);
  let (prev, prev_s) =
    switch (Hashtbl.find_opt(last_side, ck)) {
    | Some(last) => last
    | None => (read_side(prefix, name), "")
    };
  let side = update(prev);
  let s = Sexplib.Sexp.to_string(sexp_of_side(side));
  if (s != prev_s) {
    Hashtbl.replace(last_side, ck, (side, s));
    HazelDB.kv_batch(() => {
      HazelDB.kv_save(side_key(prefix, name), s);
      List.iter(
        suffix =>
          if (HazelDB.kv_get(ck ++ suffix) != None) {
            HazelDB.kv_remove(ck ++ suffix);
          },
        legacy_suffixes,
      );
    });
  };
};

/* the stored probes whose anchors are still in [seg] */
let read_probes =
    (prefix: string, name: string, seg: Segment.t): Refractors.RefractorList.t => {
  let present = Segment.ids(seg);
  List.filter(
    ((id, _)) => List.mem(id, present),
    read_side(prefix, name).probes,
  );
};

/* restores awaiting hydration, per slide; a load that finds nothing
   clears its entry, so no leftover survives a failed read */
let pending_caret: Hashtbl.t(string, Point.t) = Hashtbl.create(8);
let pending_pins: Hashtbl.t(string, side) = Hashtbl.create(8);

let stage_side = (prefix: string, name: string): unit => {
  let ck = content_key(prefix, name);
  let side = read_side(prefix, name);
  switch (side.caret) {
  | Some(p) => Hashtbl.replace(pending_caret, ck, p)
  | None => Hashtbl.remove(pending_caret, ck)
  };
  side.pins == [] && side.zoom == None && !side.ux && side.away == None
    ? Hashtbl.remove(pending_pins, ck)
    : Hashtbl.replace(pending_pins, ck, side);
  switch (folds_of_side(side)) {
  | [] => Hashtbl.remove(slide_folds, ck)
  | folds => Hashtbl.replace(slide_folds, ck, folds)
  };
};

/* the saved view against the loaded program's outline */
let resolve_pins =
    (saved: list((OutlineTree.path, bool)), outline: OutlineTree.t)
    : list(SlideView.pin) =>
  List.filter_map(
    ((path, run)) =>
      OutlineTree.resolve_path(path, outline)
      |> Option.map(id =>
           SlideView.{
             p_id: id,
             p_run: run,
           }
         ),
    saved,
  );
let resolve_view = (saved: side, outline: OutlineTree.t): SlideView.t => {
  mode: saved.ux ? UX : Dev,
  away_pins: Option.map(ps => resolve_pins(ps, outline), saved.away),
  away_parked: saved.away_parked,
  zoom:
    switch (
      Option.bind(saved.zoom, path => OutlineTree.resolve_path(path, outline))
    ) {
    | Some(m) =>
      OutlineTree.trail_of(m, outline) |> Option.value(~default=[])
    | None => []
    },
  pins: resolve_pins(saved.pins, outline),
  parked: saved.parked,
};

let write_folds = (prefix: string, name: string): unit =>
  write_side(prefix, name, with_folds(folds(prefix, name)));

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
let last_saved_agent: Hashtbl.t(string, Agent.Model.t) = Hashtbl.create(8);
let last_agent_save_ts: Hashtbl.t(string, float) = Hashtbl.create(8);

/* the same gate for the editor blob, so an idle autosave serializes
   nothing: the zipper's content, or a divided program's cells' (caret
   moves count, for the saved caret) */
type save_stamp =
  | Unstacked(Zipper.t)
  | Stacked(Divided.t);
let last_saved_content: Hashtbl.t(string, save_stamp) = Hashtbl.create(8);

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
let last_item_saves: Hashtbl.t(string, ItemPersist.saved) =
  Hashtbl.create(8);

let save_items = (prefix: string, name: string, z: Zipper.t): unit => {
  let ck = content_key(prefix, name);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let prev =
    Hashtbl.find_opt(last_item_saves, ck) |> Option.value(~default=[]);
  let saved = ItemPersist.save(~store=item_store(prefix, name), ~prev, seg);
  Hashtbl.replace(last_item_saves, ck, saved);
};

/* the keys a slide owns, exactly: its content key, its side keys, its item
   store. a name may itself contain `:`, so no prefix match */
let side_suffixes = [":agent", ":side", ...legacy_suffixes];
let slide_suffix = (ck: string, k: string): option(string) => {
  let n = String.length(ck);
  if (k == ck) {
    Some("");
  } else if (String.length(k) > n && String.sub(k, 0, n) == ck) {
    let rest = String.sub(k, n, String.length(k) - n);
    List.mem(rest, side_suffixes)
    || String.starts_with(~prefix=":items:", rest)
      ? Some(rest) : None;
  } else {
    None;
  };
};

/* the save gates under a slide's keys: once its keys move or go, the next
   autosave (or an undo bringing the slide back) must write everything */
let forget_gates = (ck: string): unit => {
  let drop = tbl =>
    Hashtbl.fold(
      (k, _, ks) => slide_suffix(ck, k) == None ? ks : [k, ...ks],
      tbl,
      [],
    )
    |> List.iter(Hashtbl.remove(tbl));
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
  HazelDB.kv_remove_where(k => slide_suffix(ck, k) != None);
  Hashtbl.remove(slide_folds, ck);
  Hashtbl.remove(pending_pins, ck);
  Hashtbl.remove(pending_caret, ck);
  forget_gates(ck);
};

let rename_slide = (prefix: string, old_name: string, new_name: string): unit =>
  if (old_name != new_name) {
    let (old_ck, new_ck) = (
      content_key(prefix, old_name),
      content_key(prefix, new_name),
    );
    HazelDB.kv_rekey(k =>
      Option.map(rest => new_ck ++ rest, slide_suffix(old_ck, k))
    );
    switch (Hashtbl.find_opt(slide_folds, old_ck)) {
    | Some(folds) => Hashtbl.replace(slide_folds, new_ck, folds)
    | None => Hashtbl.remove(slide_folds, new_ck)
    };
    Hashtbl.remove(slide_folds, old_ck);
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
  /* Carry the offered-defaults record forward: the model does not hold
     it, and dropping it here would let reconcile_names resurrect a
     slide the user deleted after their first visit. */
  let known_defaults =
    switch (load_meta(prefix)) {
    | Some(m) => m.known_defaults
    | None => []
    };
  save_meta(
    prefix,
    {
      current: model.current,
      names,
      known_defaults,
    },
  );
  let sp = List.nth(model.scratchpads, model.current);
  switch (sp.dormant, sp.kind) {
  | (true, _) => () /* never write a placeholder over the stored slide */
  | (false, Code({program, agent, view, _})) =>
    let stamp =
      switch (program) {
      | Divided(d) => Stacked(d)
      | Whole(editor) => Unstacked(editor.editor.editor.state.zipper)
      };
    let ck = content_key(prefix, sp.name);
    let content_unchanged =
      switch (Hashtbl.find_opt(last_saved_content, ck)) {
      | Some(prev) => stamp_equal(prev, stamp)
      | None => false
      };
    if (!content_unchanged) {
      Hashtbl.replace(last_saved_content, ck, stamp);
      /* text, without a live editor (syntax init and zipper sexps are
         slow); a divided program saves its document */
      let z =
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
        };
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
                    ~root=Program.root(program),
                  ),
                result: EvalResult.Model.persist(Program.result(program)),
              },
            ),
          agent: Agent.Persistent.persist(Agent.Utils.init()),
        }),
      );
    };
    let outline = Program.outline(program);
    let paths = (pins: list(SlideView.pin)) =>
      List.filter_map(
        (p: SlideView.pin) =>
          OutlineTree.label_path(p.p_id, outline)
          |> Option.map(path => (path, p.p_run)),
        pins,
      );
    /* the folds carry over: they're written as they change */
    write_side(prefix, sp.name, side =>
      {
        ...side,
        caret:
          switch (program) {
          /* text saves lose it, so it's restored after hydration; a
             divided program's is in a cell anyway */
          | Whole(editor) when !content_unchanged =>
            try(
              Some(
                Zipper.Caret.point(
                  Haz3lcore.CachedSyntax.measured(
                    editor.editor.editor.syntax,
                  ),
                  editor.editor.editor.state.zipper,
                ),
              )
            ) {
            | _ => side.caret
            }
          | _ => side.caret
          },
        probes: content_unchanged ? side.probes : Program.probes(program),
        /* the view can change without the text (a parked pin dropped) */
        pins: paths(view.pins),
        zoom:
          Option.bind(SlideView.zoom_root(view), m =>
            OutlineTree.label_path(m, outline)
          ),
        parked: view.parked,
        ux: view.mode == SlideView.UX,
        away: Option.map(paths, view.away_pins),
        away_parked: view.away_parked,
      }
    );
    /* the agent model changes on every streamed chunk, so a physical
       equality gate saved (and serialized, several MB) many times a
       second while the model spoke; gate on the fields that persist */
    let unchanged =
      switch (Hashtbl.find_opt(last_saved_agent, ck)) {
      | Some(prev) =>
        let prev: Agent.Model.t = prev;
        prev === agent
        || prev.chat_system === agent.chat_system
        && prev.prompting === agent.prompting
        && prev.active_timeline_node == agent.active_timeline_node
        && prev.awaiting_response == agent.awaiting_response;
      | None => false
      };
    /* while the agent works (tools landing every few hundred ms) one save
       per 10 s is enough; the final save comes when it goes idle */
    let busy =
      agent.awaiting_response != None || agent.pending_dispatch_send != None;
    let now = JsUtil.timestamp();
    let recently =
      switch (Hashtbl.find_opt(last_agent_save_ts, ck)) {
      | Some(t) => now -. t < 10000.
      | None => false
      };
    if (!unchanged && !(busy && recently)) {
      save_agent(prefix, sp.name, Agent.Persistent.persist(agent));
      Hashtbl.replace(last_saved_agent, ck, agent);
      Hashtbl.replace(last_agent_save_ts, ck, now);
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

let load_scratchpad = (~settings, prefix: string, name: string): Scratchpad.t => {
  stage_side(prefix, name);
  switch (load_slide_kind(prefix, name)) {
  | Some(CodePersist({editor: e, agent})) =>
    let persisted =
      switch (e) {
      | Some(e) => e
      | None => Init.default_documentation_slide_name(name)
      };
    Scratchpad.mk_code(
      ~name,
      ~agent=
        Agent.Persistent.unpersist(
          Option.value(load_agent(prefix, name), ~default=agent),
        ),
      /* per-item restore, else the whole-doc text */
      ~editor=
        switch (ItemPersist.load(~store=item_store(prefix, name))) {
        | Some(seg) =>
          let root = persisted.editor.root;
          let z =
            Zipper.unzip(~direction=Left, seg)
            |> Zipper.remold_regrout(Right, ~root)
            |> ZipperBase.update_refractors(_, r =>
                 Refractors.{
                   ...r,
                   manuals: read_probes(prefix, name, seg),
                 }
               );
          /* prime the dirty cache: the first autosave tick after a load
             should rewrite nothing */
          Hashtbl.replace(
            last_item_saves,
            content_key(prefix, name),
            ItemPersist.items_of(
              Zipper.unselect_and_zip(~erase_buffer=true, z),
            ),
          );
          CellEditor.Model.unpersist_with(~settings, ~zipper=z, persisted);
        | None =>
          /* the text copy re-mints ids, so no stored item slice can be
             reused: drop them rather than leave them */
          let ns = items_ns(prefix, name);
          HazelDB.kv_remove_where(k => String.starts_with(~prefix=ns, k));
          CellEditor.Model.unpersist(~settings, persisted);
        },
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
      Scratchpad.mk_code(
        ~name,
        ~editor=
          Init.default_documentation_slide_name(name)
          |> CellEditor.Model.unpersist(~settings),
        ~agent=?
          Option.map(Agent.Persistent.unpersist, load_agent(prefix, name)),
        (),
      )
    }
  };
};

let index_of = (x: string, xs: list(string)): option(int) => {
  let rec go = (i, xs) =>
    switch (xs) {
    | [] => None
    | [y, ...ys] => y == x ? Some(i) : go(i + 1, ys)
    };
  go(0, xs);
};

/* A slide added to Slides.re used to be invisible to anyone who had
   opened the mode before: the saved name list won outright, so it
   stayed frozen at whatever it was on that browser's first visit.

   Reconcile splices in the defaults this browser has not been offered
   yet, each landing in its default-order position rather than at the
   end -- so "Fumola / 0. Big picture" arrives before "1. Values", not
   after "Derivations". Saved order, renames and user-added slides are
   left where they are, [current] follows its slide by name, and a
   default the user deleted stays deleted, since it is in
   known_defaults. On a meta from before this existed, known_defaults
   is empty and we seed it from the defaults still present, which costs
   one resurrection of anything deleted before the upgrade. */
let reconcile_names =
    (~default_names: list(string), meta: slide_meta): slide_meta => {
  let known =
    switch (meta.known_defaults) {
    | [] => List.filter(n => List.mem(n, meta.names), default_names)
    | known => known
    };
  let pending = List.filter(n => !List.mem(n, known), default_names);
  if (pending == []) {
    {
      ...meta,
      known_defaults: default_names,
    };
  } else {
    let present = n => List.mem(n, meta.names);
    /* Anchor each new slide to the nearest default ahead of it that
       this browser actually has, and emit it just after. That keeps it
       beside the neighbour it is numbered against even if the saved
       list has been reordered; nothing ahead of it means it goes
       first. */
    let anchor = (p: string): option(string) => {
      let i = index_of(p, default_names) |> Option.value(~default=0);
      let (_, found) =
        List.fold_left(
          ((j, found), d) =>
            (j + 1, j < i && present(d) ? Some(d) : found),
          (0, None),
          default_names,
        );
      found;
    };
    let anchored = List.map(p => (anchor(p), p), pending);
    let anchored_to = a =>
      anchored |> List.filter(((x, _)) => x == a) |> List.map(snd);
    let names =
      anchored_to(None)
      @ List.concat_map(n => [n, ...anchored_to(Some(n))], meta.names);
    let current =
      switch (
        Option.bind(List.nth_opt(meta.names, meta.current), n =>
          index_of(n, names)
        )
      ) {
      | Some(i) => i
      | None => min(meta.current, max(List.length(names) - 1, 0))
      };
    {
      current,
      names,
      known_defaults: default_names,
    };
  };
};

let load_all =
    /* doc reconciles; scratch (a seed, not shipped content) does not */
    (
      ~reconcile: bool=false,
      prefix: string,
      ~settings,
      ~default_names: list(string),
      ~default_current: int,
    )
    : Model.t => {
  let (current, names) =
    switch (load_meta(prefix)) {
    | Some(meta) when reconcile =>
      let reconciled = reconcile_names(~default_names, meta);
      if (reconciled != meta) {
        save_meta(prefix, reconciled);
      };
      (reconciled.current, reconciled.names);
    | Some(meta) => (meta.current, meta.names)
    | None =>
      /* Record the shipped list as offered now, so a slide deleted
         later is not read back as one this browser never saw. */
      if (reconcile) {
        save_meta(
          prefix,
          {
            current: default_current,
            names: default_names,
            known_defaults: default_names,
          },
        );
      };
      (default_current, default_names);
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
  };
};

/* Swap the placeholder at [current] for the real slide, if dormant. */
let hydrate_current = (~settings, prefix: string, model: Model.t): Model.t => {
  let sp = List.nth(model.scratchpads, model.current);
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
      List.map((sp: Scratchpad.persistent) => sp.name, scratchpads);
    /* an import replaces every slide: stale per-item saves would win
       over the imported text on load */
    let old_names =
      switch (load_meta(prefix)) {
      | Some(m) => m.names
      | None => []
      };
    List.iter(forget_slide(prefix), old_names @ names);
    save_meta(
      prefix,
      {
        current,
        names,
        known_defaults: [],
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
  | _ => print_endline("ScratchPersist.import_all: error")
  };
