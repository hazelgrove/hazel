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
  /* Definition-focus mode (modular-editors phase 2): the current
     slide's master cell is PARKED and a cell holding just one
     definition takes its place, so every existing route (actions,
     statics, view, cursor) targets the focus cell unchanged. Unfocus
     splices the (possibly edited) definition back into the master.
     Transient — never persisted; persistence reads through
     effective_scratchpad. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus_t = {
    f_id: Haz3lcore.Id.t, /* focused definition's piece id in the master */
    f_parked: Scratchpad.t, /* the master slide, held aside */
    f_ctx: Language.Ctx.t /* frozen outer ctx at the definition */
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list(Scratchpad.t),
    focus: option(focus_t),
  };

  /* The monolithic export/import format (per-slide keys are the live
     storage; see Persist below). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list(Scratchpad.persistent));

  let scratchpad_names = (model: t): list(string) =>
    List.map((s: Scratchpad.t) => s.name, model.scratchpads);

  let get_derivation_info = (model: t) => {
    let current = List.nth(model.scratchpads, model.current);
    switch (current.kind) {
    | Code(_) => None
    | Drv(m) => DerivationExerciseMode.Model.get_derivation_info(m)
    };
  };
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

  let cell_of_seg = (seg: Segment.t): CellEditor.Model.t =>
    seg |> Zipper.unzip |> Editor.Model.mk(~root=Exp) |> CellEditor.Model.mk;

  /* the master slide with the live focus-cell content spliced back in
     (pure; used by unfocus AND by persistence while focused) */
  let spliced_master =
      (focus: Model.focus_t, current: Scratchpad.t): Scratchpad.t =>
    switch (focus.f_parked.kind, current.kind) {
    | (Code(parked), Code({editor: focus_cell, agent})) =>
      let master_seg = zip_of_cell(parked.editor);
      let new_seg =
        splice_def(focus.f_id, zip_of_cell(focus_cell), master_seg);
      {
        ...focus.f_parked,
        kind:
          Code({
            editor: cell_of_seg(new_seg),
            agent,
          }),
      };
    | _ => focus.f_parked
    };
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

  let delete_slide = (prefix: string, name: string): unit => {
    HazelDB.kv_delete(slide_key(prefix, name));
    HazelDB.kv_delete(agent_key(prefix, name));
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

  /* the scratchpad persistence should see: the master with any live
     focus-cell edits spliced in — never the bare focus cell */
  let effective_current = (model: Model.t): Scratchpad.t => {
    let sp = List.nth(model.scratchpads, model.current);
    switch (model.focus) {
    | Some(f) => Focus.spliced_master(f, sp)
    | None => sp
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
    let sp = effective_current(model);
    switch (sp.dormant, sp.kind) {
    | (true, _) => () /* never write a placeholder over the stored slide */
    | (false, Code({editor, agent})) =>
      switch (CellEditor.Model.persist(editor)) {
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
    | FocusDef(Haz3lcore.Id.t)
    | UnfocusDef
    | RefreshStatics
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
      /* if already focused, splice the current focus back first */
      let model =
        switch (model.focus) {
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
        | None => model
        };
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, agent}) =>
        let master_seg = Focus.zip_of_cell(editor);
        switch (Focus.find_def(fid, master_seg)) {
        | None => model |> Updated.return_quiet
        | Some(def_seg) =>
          /* freeze the ctx the DEFINITION BODY actually sees: the def
             term's own info (includes the self-binding for recursive
             lets); fall back to the ctx at the let, then builtins */
          let info_map = editor.editor.statics.info_map;
          let info_of = id => Haz3lcore.Id.Map.find_opt(id, info_map);
          let def_info =
            List.fold_left(
              (acc, p: Haz3lcore.Piece.t) =>
                acc == None ? info_of(Haz3lcore.Piece.id(p)) : acc,
              None,
              def_seg,
            );
          let f_ctx =
            switch (def_info, info_of(fid)) {
            | (Some(info), _)
            | (None, Some(info)) => Language.Info.ctx_of(info)
            | (None, None) =>
              Language.Builtins.ctx_init(
                Some(Language.Operators.default_mode),
              )
            };
          let focus_cell = Focus.cell_of_seg(def_seg);
          Model.{
            current: model.current,
            scratchpads:
              ListUtil.put_nth(
                model.current,
                Scratchpad.{
                  ...scratchpad,
                  kind:
                    Code({
                      editor: focus_cell,
                      agent,
                    }),
                },
                model.scratchpads,
              ),
            focus:
              Some({
                f_id: fid,
                f_parked: scratchpad,
                f_ctx,
              }),
          }
          |> Updated.return;
        };
      | Drv(_) => model |> Updated.return_quiet
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
        Model.{
          current: model.current,
          scratchpads:
            ListUtil.put_nth(model.current, restored, model.scratchpads),
          focus: None,
        }
        |> Updated.return;
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
      let model = commit_focus(model);
      WorkerClient.cancel();
      let* current = i |> Updated.return(~historic=false);
      Persist.hydrate_current(
        ~settings=settings.core,
        is_documentation ? "doc" : "scratch",
        {
          ...model,
          current,
        },
      );
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
      let worker_request = ref([]);
      let queue_worker =
        Some(
          (req_value: WorkerServer.Request.value) => {
            worker_request := worker_request^ @ [("", req_value)]
          },
        );
      let ctx = Option.map((f: Model.focus_t) => f.f_ctx, model.focus);
      let new_ed =
        CellEditor.Update.calculate(
          ~settings,
          ~autoprobe_mode,
          ~is_edited,
          ~statics_mode,
          ~ctx?,
          ~queue_worker,
          ~stitch=x => x,
          editor,
        );
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
      | (Drv(selection), Drv(m)) =>
        let+ a =
          DerivationExerciseMode.Selection.get_cursor_info(
            ~inject=a => inject(DrvAction(a)),
            ~selection,
            m,
          );
        Update.DrvAction(a);
      | (Cell(_), Drv(_))
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
    switch (current.kind) {
    | Code({editor, _}) =>
      (SlideContent.get_content(current.name) |> Option.to_list)
      @ [
        CellEditor.View.view(
          ~globals,
          ~signal=
            fun
            | MakeActive(selection) => signal(MakeActive(Cell(selection))),
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
