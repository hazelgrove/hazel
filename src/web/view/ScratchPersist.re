open Haz3lcore;
open Util;

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Focus = ScratchFocus;

/* Per-slide IndexedDB persistence. Each scratchpad's editor and agent
   data is stored as separate HazelDB KV keys, so autosave only writes
   the current slide.

   Key layout:
     <prefix>:_meta         → slide_meta (current_index, names)
     <prefix>:<name>        → CellEditor.Model.persistent
     <prefix>:<name>:agent  → Agent.Persistent.t */

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
    (prefix: string, name: string, pins: list((list(string), bool))): unit =>
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

/* === Per-item persistence (ItemPersist) ===
   The primary restore: top-level item slices as individual sexp
   values + an ordered roster under side keys, so autosave writes
   only the items an edit touched and reload restores the zipper
   EXACTLY (incomplete tiles included) with no text parse. The text
   blob below remains the write-through fallback and migration path:
   no/inconsistent roster falls back to the text load. */
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
        ~root=editor.editor.editor.root,
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
      switch (model.focus) {
      | Some(f) =>
        save_items(prefix, sp.name, Focus.splice_all(f) |> Zipper.unzip)
      | None => save_items(prefix, sp.name, editor.editor.editor.state.zipper)
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
                /* the editor's OWN root: persisting a Mod-rooted
                   slide as Exp made the reload re-parse it as an
                   expression (backpack full of `in`s, editor wedged) */
                ~root=editor.editor.editor.root,
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
          editor: {
            /* repair blobs persisted with the wrong root (and track
               canonical root changes): the slide table is
               authoritative for documentation slides */
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
              | None => (Init.default_documentation_slide_name(name), false)
              };
            /* per-item restore: exact zipper, no text parse. Skipped
               when the root was just repaired (stored items were
               normalized under the OLD root — reparse once instead)
               or on any roster inconsistency (text fallback). */
            switch (
              root_repaired
                ? None : ItemPersist.load(~store=item_store(prefix, name))
            ) {
            | Some(seg) =>
              let root = persisted.editor.root;
              let z =
                Zipper.unzip(~direction=Left, seg)
                |> Zipper.remold_regrout(Right, ~root);
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
  | _ => print_endline("ScratchPersist.import_all: error")
  };
