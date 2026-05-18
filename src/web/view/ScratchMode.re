open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

/* Dirty tracking for autosave: only re-persist slides that changed since
   last save. Eliminates expensive Zipper.zip + Base.equal_segment checks. */
let dirty_slides: ref(Sets.StringSet.t) = ref(Sets.StringSet.empty);
let persist_cache:
  ref(Maps.StringMap.t(option(CellEditor.Model.persistent))) =
  ref(Maps.StringMap.empty);

let mark_dirty = (name: string): unit =>
  dirty_slides := Sets.StringSet.add(name, dirty_slides^);

let reset_persist_state = (): unit => {
  dirty_slides := Sets.StringSet.empty;
  persist_cache := Maps.StringMap.empty;
};

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

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    name: string,
    kind,
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
      let current_segment = Zipper.zip(editor.editor.editor.state.zipper);
      let original = Init.find_documentation_slide(s.name);
      let original_segment =
        original
        |> Option.map((pce: CellEditor.Model.persistent) =>
             PersistentZipper.unpersist(
               pce.editor.zipper,
               ~root=pce.editor.root,
             )
           )
        |> Option.map(Zipper.zip);
      let editor_persist =
        if (Option.equal(
              Base.equal_segment,
              original_segment,
              Some(current_segment),
            )) {
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

  /* Used only for migration fallback from old monolithic format */
  let unpersist = (~settings, p: persistent): t => {
    switch (p.kind) {
    | CodePersist({editor, agent}) => {
        name: p.name,
        kind:
          Code({
            editor:
              OptUtil.get(
                () => Init.default_documentation_slide_name(p.name),
                editor,
              )
              |> CellEditor.Model.unpersist(~settings),
            agent: Agent.Persistent.unpersist(agent),
          }),
      }
    | DrvPersist(dp) => {
        name: p.name,
        kind:
          Drv(
            DerivationExerciseMode.Model.unpersist(
              ~settings,
              ~instructor_mode=false,
              dp,
              DerivationExercise.blank_spec(
                ~title=p.name,
                ~module_name=p.name,
              ),
            ),
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
  };

  let blank_code = (name: string): t =>
    mk_code(
      ~name,
      ~editor=CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp)),
      (),
    );

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
  };

  /* Backward-compat constructor (Code-only). Kept for call sites that
     pre-date the kind split. */
  let mk = mk_code;
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list(Scratchpad.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list(Scratchpad.persistent));

  let persist = (model: t): persistent => {
    let persisted_slides =
      List.map(
        (s: Scratchpad.t) => {
          switch (s.kind) {
          | Code({editor, agent}) =>
            let is_dirty = Sets.StringSet.mem(s.name, dirty_slides^);
            let has_cache = Maps.StringMap.mem(s.name, persist_cache^);
            let editor_persist =
              if (is_dirty || !has_cache) {
                let persisted = Some(CellEditor.Model.persist(editor));
                persist_cache :=
                  Maps.StringMap.add(s.name, persisted, persist_cache^);
                persisted;
              } else {
                Maps.StringMap.find(s.name, persist_cache^);
              };
            Scratchpad.{
              name: s.name,
              kind:
                CodePersist({
                  editor: editor_persist,
                  agent: Agent.Persistent.persist(agent),
                }),
            };
          | Drv(m) =>
            Scratchpad.{
              name: s.name,
              kind:
                DrvPersist(
                  DerivationExerciseMode.Model.persist(
                    m,
                    ~instructor_mode=false,
                  ),
                ),
            }
          }
        },
        model.scratchpads,
      );
    dirty_slides := Sets.StringSet.empty;
    (model.current, persisted_slides);
  };

  let unpersist = (~settings, (current, scratchpads): persistent): t => {
    /* Seed persist cache with loaded values so unchanged slides
       (stored as None) aren't needlessly re-persisted on first save */
    reset_persist_state();
    List.iter(
      (sp: Scratchpad.persistent) =>
        switch (sp.kind) {
        | CodePersist({editor, _}) =>
          persist_cache := Maps.StringMap.add(sp.name, editor, persist_cache^)
        | DrvPersist(_) => ()
        },
      scratchpads,
    );
    {
      current,
      scratchpads:
        List.map(sp => Scratchpad.unpersist(~settings, sp), scratchpads),
    };
  };

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
    let p = Scratchpad.persist(sp);
    switch (p.kind) {
    | CodePersist({editor, agent}) =>
      switch (editor) {
      | Some(e) =>
        save_slide_kind(
          prefix,
          sp.name,
          CodePersist({
            editor: Some(e),
            agent,
          }),
        )
      | None => ()
      };
      save_agent(prefix, sp.name, agent);
    | DrvPersist(_) as k => save_slide_kind(prefix, sp.name, k)
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
        List.map(name => load_scratchpad(~settings, prefix, name), names),
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
    let shared: PersistentZipper.t = {
      zipper: "invalid",
      backup_text: shared_text,
    };
    let shared: CellEditor.Model.persistent = {
      editor: {
        root: Exp,
        zipper: shared,
      },
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
    };
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
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

  let can_undo = (action: t) => {
    switch (action) {
    | CellAction(action) => CellEditor.Update.can_undo(action)
    | RefreshStatics => false
    | AgentAction(_) => true
    | DrvAction(action) => DerivationExerciseMode.Update.can_undo(action)
    | SwitchSlide(_) => false
    | ResetCurrent => true
    | InitImportScratchpad(_) => true
    | FinishImportScratchpad(_) => false
    | Export => false
    | Encode => false
    | AddSlide => true
    | AddDrvSlide => true
    | DeleteSlide => true
    | RenameSlide => true
    };
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
    | CellAction(a) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, agent}) =>
        mark_dirty(scratchpad.name);
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
        mark_dirty(scratchpad.name);
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
      WorkerClient.cancel();
      let* current = i |> Updated.return;
      {
        ...model,
        current,
      };
    | AddSlide =>
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
        let old_name = current.name;
        persist_cache := Maps.StringMap.remove(old_name, persist_cache^);
        dirty_slides := Sets.StringSet.remove(old_name, dirty_slides^);
        mark_dirty(new_name);
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
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to delete this slide? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        WorkerClient.cancel();
        let deleted_name = List.nth(model.scratchpads, model.current).name;
        persist_cache := Maps.StringMap.remove(deleted_name, persist_cache^);
        dirty_slides := Sets.StringSet.remove(deleted_name, dirty_slides^);
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
            : {
              scratchpads: new_sp,
              current: max(model.current - 1, 0),
            };
        Updated.return(m);
      } else {
        model |> return_quiet;
      };

    | ResetCurrent =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      mark_dirty(scratchpad.name);
      persist_cache := Maps.StringMap.remove(scratchpad.name, persist_cache^);
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
      // reset file input so same file can be re-imported if desired
      JsUtil.reset_file_input("import-scratchpad");
      switch (data) {
      | None => model |> return_quiet
      | Some(data) =>
        let scratchpad = List.nth(model.scratchpads, model.current);
        switch (scratchpad.kind) {
        | Code({agent, _}) =>
          mark_dirty(scratchpad.name);
          persist_cache :=
            Maps.StringMap.remove(scratchpad.name, persist_cache^);
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
      let new_ed =
        CellEditor.Update.calculate(
          ~settings,
          ~autoprobe_mode,
          ~is_edited,
          ~statics_mode,
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
