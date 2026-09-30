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
     <k>:caret, :pins, :view, :collapse, :probes → what the slide shows
     <k>:items:…     → per-item slices and their roster (ItemPersist) */

/* per-slide tables below key on this, never the bare name: a Scratch
   and a Documentation slide may share a name */
let content_key = (prefix: string, name: string): string =>
  prefix ++ ":" ++ name;

/* outline collapse per slide, as occurrence-qualified label paths;
   modeled because <details> DOM state bleeds across slides and resets
   when the vdom recreates elements */
let slide_collapse: Hashtbl.t(string, list(OutlineTree.path)) =
  Hashtbl.create(8);

let collapse_paths = (prefix: string, name: string): list(OutlineTree.path) =>
  switch (Hashtbl.find_opt(slide_collapse, content_key(prefix, name))) {
  | Some(ps) => ps
  | None => []
  };

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
let probes_key = (prefix: string, name: string): string =>
  prefix ++ ":" ++ name ++ ":probes";

/* manual probes live in zippers, not segments, so the per-item store
   can't carry them: they ride their own key */
let last_saved_probes: Hashtbl.t(string, string) = Hashtbl.create(8);

let write_probes =
    (prefix: string, name: string, probes: Refractors.RefractorList.t): unit => {
  let key = probes_key(prefix, name);
  let s = Sexplib.Sexp.to_string(Refractors.RefractorList.sexp_of_t(probes));
  if (Hashtbl.find_opt(last_saved_probes, key) != Some(s)) {
    Hashtbl.replace(last_saved_probes, key, s);
    HazelDB.kv_save(key, s);
  };
};

/* the stored probes whose anchors are still in [seg] */
let read_probes =
    (prefix: string, name: string, seg: Segment.t): Refractors.RefractorList.t =>
  switch (HazelDB.kv_get(probes_key(prefix, name))) {
  | None => []
  | Some(s) =>
    switch (Refractors.RefractorList.t_of_sexp(Sexplib.Sexp.of_string(s))) {
    | exception _ => []
    | probes =>
      let present = Segment.ids(seg);
      List.filter(((id, _)) => List.mem(id, present), probes);
    }
  };

/* restores awaiting hydration, per slide; a read_* that finds nothing
   clears its entry, so no leftover survives a failed read */
let pending_caret: Hashtbl.t(string, Point.t) = Hashtbl.create(8);
/* a slide's saved view, by outline label path */
type saved_view = {
  sv_pins: list((OutlineTree.path, bool)),
  sv_zoom: option(OutlineTree.path),
  sv_parked: bool,
};
let pending_pins: Hashtbl.t(string, saved_view) = Hashtbl.create(8);

/* pins and collapse store as sexps, as labels are arbitrary program text;
   the old line format is a read fallback (bare labels at occurrence 0) */
[@deriving sexp]
type pin_rec = {
  pin_path: OutlineTree.path,
  pin_run: bool,
};
[@deriving sexp]
type pins_file = list(pin_rec);
[@deriving sexp]
type collapse_file = list(OutlineTree.path);
/* zoom and parked ride their own key beside the pins */
[@deriving sexp]
type view_file = {
  vf_zoom: option(OutlineTree.path),
  vf_parked: bool,
};

let view_key = (prefix: string, name: string): string =>
  prefix ++ ":" ++ name ++ ":view";

let legacy_path = (p: string): OutlineTree.path =>
  String.split_on_char('/', p)
  |> List.map(l =>
       OutlineTree.{
         s_label: l,
         s_occ: 0,
       }
     );

let read_pins = (prefix: string, name: string): unit => {
  let decode = (txt: string): list((OutlineTree.path, bool)) =>
    switch (pins_file_of_sexp(Sexplib.Sexp.of_string(txt))) {
    | pins => List.map(p => (p.pin_path, p.pin_run), pins)
    | exception _ =>
      String.split_on_char('\n', txt)
      |> List.filter_map(line =>
           switch (String.split_on_char(' ', String.trim(line))) {
           | [flag, path] when path != "" =>
             Some((legacy_path(path), flag == "1"))
           | _ => None
           }
         )
    };
  let pins =
    switch (HazelDB.kv_get(pins_key(prefix, name))) {
    | Some(txt) => decode(txt)
    | None => []
    };
  let (zoom, parked) =
    switch (
      HazelDB.kv_get(view_key(prefix, name))
      |> Option.map(txt => view_file_of_sexp(Sexplib.Sexp.of_string(txt)))
    ) {
    | Some(vf) => (vf.vf_zoom, vf.vf_parked)
    | None
    | exception _ => (None, false)
    };
  let ck = content_key(prefix, name);
  pins == [] && zoom == None
    ? Hashtbl.remove(pending_pins, ck)
    : Hashtbl.replace(
        pending_pins,
        ck,
        {
          sv_pins: pins,
          sv_zoom: zoom,
          sv_parked: parked,
        },
      );
};

/* a deleted slide leaves nothing a later slide of the same name could
   pick up: stored keys, collapse, pending restores */
let forget_slide = (prefix: string, name: string): unit => {
  let ck = content_key(prefix, name);
  HazelDB.kv_remove_under(ck);
  Hashtbl.remove(slide_collapse, ck);
  Hashtbl.remove(pending_pins, ck);
  Hashtbl.remove(pending_caret, ck);
};

let rename_slide = (prefix: string, old_name: string, new_name: string): unit => {
  let (old_ck, new_ck) = (
    content_key(prefix, old_name),
    content_key(prefix, new_name),
  );
  HazelDB.kv_move_under(~from=old_ck, ~to_=new_ck);
  switch (Hashtbl.find_opt(slide_collapse, old_ck)) {
  | Some(paths) => Hashtbl.replace(slide_collapse, new_ck, paths)
  | None => Hashtbl.remove(slide_collapse, new_ck)
  };
  Hashtbl.remove(slide_collapse, old_ck);
};

/* the saved view against the loaded program's outline */
let resolve_view = (saved: saved_view, term: Language.Exp.t): SlideView.t => {
  zoom:
    switch (
      Option.bind(saved.sv_zoom, path => OutlineTree.resolve_path(path, term))
    ) {
    | Some(m) => OutlineTree.trail_of(m, term) |> Option.value(~default=[])
    | None => []
    },
  pins:
    List.filter_map(
      ((path, run)) =>
        OutlineTree.resolve_path(path, term)
        |> Option.map(id =>
             SlideView.{
               p_id: id,
               p_run: run,
             }
           ),
      saved.sv_pins,
    ),
  parked: saved.sv_parked,
};

let read_collapse = (prefix: string, name: string): unit => {
  let ck = content_key(prefix, name);
  let decode = (txt: string): list(OutlineTree.path) =>
    switch (collapse_file_of_sexp(Sexplib.Sexp.of_string(txt))) {
    | paths => paths
    | exception _ =>
      String.split_on_char('\n', txt)
      |> List.filter_map(line => {
           let line = String.trim(line);
           line == "" ? None : Some(legacy_path(line));
         })
    };
  switch (HazelDB.kv_get(collapse_key(prefix, name)) |> Option.map(decode)) {
  | Some([]) => Hashtbl.remove(slide_collapse, ck)
  | Some(paths) => Hashtbl.replace(slide_collapse, ck, paths)
  | None => Hashtbl.remove(slide_collapse, ck)
  };
};

let write_collapse = (prefix: string, name: string): unit =>
  HazelDB.kv_save(
    collapse_key(prefix, name),
    collapse_paths(prefix, name)
    |> sexp_of_collapse_file
    |> Sexplib.Sexp.to_string,
  );

let last_saved_view: Hashtbl.t(string, string) = Hashtbl.create(8);
let save_if_changed = (key: string, s: string): unit =>
  if (Hashtbl.find_opt(last_saved_view, key) != Some(s)) {
    Hashtbl.replace(last_saved_view, key, s);
    HazelDB.kv_save(key, s);
  };

let write_view =
    (prefix: string, name: string, view: SlideView.t, term: Language.Exp.t)
    : unit => {
  save_if_changed(
    pins_key(prefix, name),
    view.pins
    |> List.filter_map((p: SlideView.pin) =>
         OutlineTree.label_path(p.p_id, term)
         |> Option.map(pin_path =>
              {
                pin_path,
                pin_run: p.p_run,
              }
            )
       )
    |> sexp_of_pins_file
    |> Sexplib.Sexp.to_string,
  );
  save_if_changed(
    view_key(prefix, name),
    {
      vf_zoom:
        Option.bind(SlideView.zoom_root(view), m =>
          OutlineTree.label_path(m, term)
        ),
      vf_parked: view.parked,
    }
    |> sexp_of_view_file
    |> Sexplib.Sexp.to_string,
  );
};

let read_caret = (prefix: string, name: string): unit => {
  let ck = content_key(prefix, name);
  switch (
    Option.map(
      txt => String.split_on_char(' ', String.trim(txt)),
      HazelDB.kv_get(caret_key(prefix, name)),
    )
  ) {
  | Some([r, c]) =>
    switch (int_of_string_opt(r), int_of_string_opt(c)) {
    | (Some(row), Some(col)) =>
      Hashtbl.replace(
        pending_caret,
        ck,
        Point.{
          row,
          col,
        },
      )
    | _ => Hashtbl.remove(pending_caret, ck)
    }
  | _ => Hashtbl.remove(pending_caret, ck)
  };
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
let last_agent_save_ts: Hashtbl.t(string, float) = Hashtbl.create(8);

/* the same gate for the editor blob, so an idle autosave serializes
   nothing: identity of the zipper, or of a divided program's cells
   (caret moves count, for the caret side key) */
type save_stamp =
  | Unstacked(Zipper.t)
  | Stacked(Divided.t);
let last_saved_content: Hashtbl.t(string, save_stamp) = Hashtbl.create(8);

/* per-item persistence, the primary restore: item slices as sexps plus a
   roster, so autosave writes only touched items and reload restores the
   exact zipper unparsed; the text blob is the fallback for a bad roster */
let items_ns = (prefix: string, name: string): string =>
  prefix ++ ":" ++ name ++ ":items:";

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
  let content_key = prefix ++ ":" ++ name;
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let prev =
    Hashtbl.find_opt(last_item_saves, content_key)
    |> Option.value(~default=[]);
  let saved = ItemPersist.save(~store=item_store(prefix, name), ~prev, seg);
  Hashtbl.replace(last_item_saves, content_key, saved);
};
let stamp_equal = (a: save_stamp, b: save_stamp): bool =>
  switch (a, b) {
  | (Unstacked(x), Unstacked(y)) => x === y
  | (Stacked(x), Stacked(y)) => Divided.same_content(x, y)
  | _ => false
  };

/* a divided program saves its document as text, without a live editor
   (syntax init and zipper sexps are slow); the caret is in a cell anyway */
let persist_divided = (d: Divided.t): CellEditor.Model.persistent => {
  let z =
    Divided.document(d)
    |> Zipper.unzip
    |> ZipperBase.update_refractors(_, r =>
         Refractors.{
           ...r,
           manuals: Divided.probes(d),
         }
       );
  CellEditor.Model.{
    editor:
      Editor.Model.mk_persistent(
        PersistentZipper.of_text(PersistentZipper.to_string(z) ++ "\n"),
        ~root=Divided.root(d),
      ),
    result: EvalResult.Model.persist(Divided.result(d)),
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
  | (false, Code({program, agent, view})) =>
    let stamp =
      switch (program) {
      | Divided(d) => Stacked(d)
      | Whole(editor) => Unstacked(editor.editor.editor.state.zipper)
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
      /* whole programs save as text too (zipper sexps are slow), so the
         caret saves as a (row col) side key, restored after hydration */
      switch (program) {
      | Divided(_) => ()
      | Whole(editor) =>
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
      write_probes(prefix, sp.name, Program.probes(program));
      switch (program) {
      | Divided(d) =>
        save_items(prefix, sp.name, Divided.document(d) |> Zipper.unzip)
      | Whole(editor) =>
        save_items(prefix, sp.name, editor.editor.editor.state.zipper)
      };
      /* the slide blob carries the editor only; the conversation lives
         solely under the :agent key */
      save_slide_kind(
        prefix,
        sp.name,
        CodePersist({
          editor:
            Some(
              switch (program) {
              | Divided(d) => persist_divided(d)
              | Whole(editor) =>
                CellEditor.Model.{
                  editor:
                    Editor.Model.mk_persistent(
                      PersistentZipper.of_text(
                        PersistentZipper.to_string(
                          editor.editor.editor.state.zipper,
                        )
                        ++ "\n",
                      ),
                      /* the editor's own root: a Mod-rooted slide saved
                         as Exp reloads as an expression and wedges */
                      ~root=editor.editor.editor.root,
                    ),
                  result: EvalResult.Model.persist(editor.result),
                }
              },
            ),
          agent: Agent.Persistent.persist(Agent.Utils.init()),
        }),
      );
    };
    /* the view can change without the text (a parked pin dropped) */
    write_view(prefix, sp.name, view, Program.statics(program).term);
    let agent_key_str = prefix ++ ":" ++ sp.name;
    /* the agent model changes on every streamed chunk, so a physical
       equality gate saved (and serialized, several MB) many times a
       second while the model spoke; gate on the fields that persist */
    let unchanged =
      switch (Hashtbl.find_opt(last_saved_agent, agent_key_str)) {
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
      switch (Hashtbl.find_opt(last_agent_save_ts, agent_key_str)) {
      | Some(t) => now -. t < 10000.
      | None => false
      };
    if (!unchanged && !(busy && recently)) {
      save_agent(prefix, sp.name, Agent.Persistent.persist(agent));
      Hashtbl.replace(last_saved_agent, agent_key_str, agent);
      Hashtbl.replace(last_agent_save_ts, agent_key_str, now);
    };
  | (false, Drv(_)) =>
    switch (Scratchpad.persist(sp).kind) {
    | DrvPersist(_) as k => save_slide_kind(prefix, sp.name, k)
    | CodePersist(_) => ()
    }
  };
};

let load_scratchpad = (~settings, prefix: string, name: string): Scratchpad.t => {
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
          view: SlideView.init,
          program:
            Whole(
              {
                /* the slide table's root wins for documentation slides,
                   repairing blobs saved under the wrong one */
                let (persisted, root_repaired) =
                  switch (e) {
                  | Some(e) =>
                    switch (Init.documentation_slide_root(name)) {
                    | Some(root) when root != e.editor.root => (
                        CellEditor.Model.{
                          ...e,
                          editor: {
                            ...e.editor,
                            root,
                          },
                        },
                        true,
                      )
                    | _ => (e, false)
                    }
                  | None => (
                      Init.default_documentation_slide_name(name),
                      false,
                    )
                  };
                /* per-item restore, skipped after a root repair (the
                   items were stored under the old root) */
                switch (
                  root_repaired
                    ? None
                    : ItemPersist.load(~store=item_store(prefix, name))
                ) {
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
                  /* prime the dirty cache: the first autosave tick after
                     a load should rewrite nothing */
                  Hashtbl.replace(
                    last_item_saves,
                    prefix ++ ":" ++ name,
                    ItemPersist.items_of(
                      Zipper.unselect_and_zip(~erase_buffer=true, z),
                    ),
                  );
                  CellEditor.Model.unpersist_with(
                    ~settings,
                    ~zipper=z,
                    persisted,
                  );
                | None => CellEditor.Model.unpersist(~settings, persisted)
                };
              },
            ),
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
            program:
              Whole(
                Init.default_documentation_slide_name(name)
                |> CellEditor.Model.unpersist(~settings),
              ),
            view: SlideView.init,
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
  | _ => print_endline("ScratchPersist.import_all: error")
  };
