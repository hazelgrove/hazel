open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Focus = ScratchFocus;
module Restructure = ScratchRestructure;
module Persist = ScratchPersist;

/* per-slide pin/collapse side state lives with the persistence layer */
let slide_pins = ScratchPersist.slide_pins;
let slide_collapse = ScratchPersist.slide_collapse;
let collapse_paths = ScratchPersist.collapse_paths;

/* outline context-menu state (row id + screen position): transient
   UI, module-level like the other view caches — not model data */
let outline_menu: ref(option((Haz3lcore.Id.t, bool, float, float))) =
  ref(None);

/* the header symbol a headerless cell for [fid] should show, from
   the OUTLINE's view of the row (span kinds mis-read member-fn tails:
   a member terminates with `;`, so its fn-body tail extracts from an
   IStmt-shaped run — the row is still a ⇒) */
let outline_sym = (fid: Haz3lcore.Id.t, term: Language.Exp.t): option(string) =>
  switch (OutlineTree.kind_of(fid, term)) {
  | Some(OutlineTree.KTrail) => Some({js|⇒|js})
  | Some(OutlineTree.KTest)
  | Some(OutlineTree.KStmt) => Some({js|;|js})
  | _ => None
  };

/* PROJECTION (plan §9e / program-view-split step 3): a stack cell's
   statics come from its DefStatics ITEM — the same ids, analyzed with
   the program's real context (headers see the type the def gave their
   binder; module headers get real MPat info; warnings appear) —
   scoped to the ids the cell actually contains so id-keyed consumers
   (Arms, occurrence highlight) never see foreign ids. The private
   init_* wrappers remain only as the fallback when no item is found.
   [engine_warnings]: unused-binder warnings are computed by the
   ENGINE across items (an item alone can't see its downstream uses),
   so headers take them from the whole-program list. */
let project_cell_statics =
    (
      ~item: Haz3lcore.DefStatics.item,
      ~engine_warnings: list(Haz3lcore.Id.t),
      cell: CellEditor.Model.t,
    )
    : Haz3lcore.CachedStatics.t => {
  let term_data = cell.editor.editor.syntax.term_data;
  let in_cell = id => Haz3lcore.Id.Map.mem(id, term_data);
  Haz3lcore.CachedStatics.{
    term: item.d_node,
    elaborated: item.d_elab,
    info_map: Haz3lcore.Id.Map.filter((id, _) => in_cell(id), item.d_map),
    error_ids: List.filter(in_cell, item.d_error_ids),
    warning_ids: List.filter(in_cell, item.d_warning_ids @ engine_warnings),
    targets: Haz3lcore.Id.Map.empty, /* with_targets refreshes */
    probe_ids:
      Haz3lcore.CachedStatics.probe_ids_of_zipper(
        cell.editor.editor.state.zipper,
      ),
  };
};
let stacked_statics: ref(option(Haz3lcore.CachedStatics.t)) = ref(None);
/* incremental-parse cache for the stacked Force frame: the plain
   memoized term_of cost ~312ms/edit at 4k (ledger §14) — the go_incr
   path with a persistent cache replays the top frame exactly and
   re-parses only the edited item */
let stacked_incr_cache: ref(Haz3lcore.MakeTerm.Incr.cache) =
  ref(Haz3lcore.MakeTerm.Incr.mk_cache());

/* Structural operations on TOP-LEVEL definitions (outline context
   menu): insert / duplicate / move / delete. All act on the LIVE
   whole-program segment (spliced when a stack is open) and rebuild
   the master editor. Untouched items keep their piece ids, so open
   cells still find their definitions and probes stay pinned. */

/* the outline's ids in document order — the stack mirrors this order */
let outline_order = (term: Language.Exp.t): list(Haz3lcore.Id.t) => {
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

/* where a cell for [fid] goes (or sits) in the stack: entries keep
   PROGRAM order, not click order */
let stack_position =
    (~term, fid: Haz3lcore.Id.t, entries: list(Model.stack_entry)): int => {
  let rec index_of = (k, l: list(Model.stack_entry)) =>
    switch (l) {
    | [] => None
    | [e, ..._] when e.e_id == fid => Some(k)
    | [_, ...rest] => index_of(k + 1, rest)
    };
  switch (index_of(0, entries)) {
  | Some(j) => j
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
    let r = rank(fid);
    List.length(
      List.filter((e: Model.stack_entry) => rank(e.e_id) < r, entries),
    );
  };
};

let insert_entry =
    (~term, entry: Model.stack_entry, entries: list(Model.stack_entry))
    : list(Model.stack_entry) => {
  let pos = stack_position(~term, entry.e_id, entries);
  let rec ins = (k, es) =>
    k == 0
      ? [entry, ...es]
      : (
        switch (es) {
        | [] => [entry]
        | [e, ...rest] => [e, ...ins(k - 1, rest)]
        }
      );
  ins(pos, entries);
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
    | StackHeader(int, CellEditor.Update.t)
    | StackBody(int, CellEditor.Update.t)
    | FocusDef(Haz3lcore.Id.t) /* replace the stack with this one def */
    | FocusToggle(Haz3lcore.Id.t) /* add/remove a def in the stack */
    | FocusToggleRun(Haz3lcore.Id.t) /* one cell for a whole test run */
    | RestorePins /* deferred per-slide pin restore after slide load */
    | OutlineCollapse(OutlineTree.path) /* toggle a branch's collapse */
    | FocusEnsure(Haz3lcore.Id.t) /* add if absent (cross-cell jump) */
    | RestoreCaret(Point.t) /* deferred caret restore after slide load */
    | OutlineMenu(option((Haz3lcore.Id.t, bool, float, float)))
    | OutlineDefOp(OutlineSidebar.def_op, Haz3lcore.Id.t)
    | UnfocusDef
    | RefreshStatics
    | HydrateCurrent /* deferred slide hydration (SwitchSlide shows a
                        loading frame first) */
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
      /* replace the whole stack with this one definition */
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, _}) =>
        let master_seg =
          switch (model.focus) {
          /* stack already open: splice its entries home first */
          | Some(f) => Focus.splice_all(f)
          | None => Focus.zip_of_cell(editor)
          };
        let info_map = editor.editor.statics.info_map;
        switch (
          Focus.mk_entry(
            ~info_map,
            ~sym=?outline_sym(fid, editor.editor.statics.term),
            fid,
            master_seg,
          )
        ) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          {
            ...model,
            focus:
              Some(
                Model.{
                  f_entries: [entry],
                  f_master_seg: master_seg,
                },
              ),
          }
          |> Updated.return
        };
      | Drv(_) => model |> Updated.return_quiet
      };
    | FocusToggle(fid) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, _}) =>
        switch (model.focus) {
        | None =>
          /* no stack yet: same as single focus */
          let master_seg = Focus.zip_of_cell(editor);
          let info_map = editor.editor.statics.info_map;
          switch (
            Focus.mk_entry(
              ~info_map,
              ~sym=?outline_sym(fid, editor.editor.statics.term),
              fid,
              master_seg,
            )
          ) {
          | None => model |> Updated.return_quiet
          | Some(entry) =>
            {
              ...model,
              focus:
                Some(
                  Model.{
                    f_entries: [entry],
                    f_master_seg: master_seg,
                  },
                ),
            }
            |> Updated.return
          };
        | Some(f) =>
          if (List.exists(
                (e: Model.stack_entry) => e.e_id == fid,
                f.f_entries,
              )) {
            /* remove: splice that entry home; empty stack = unfocus */
            let closing =
              List.find(
                (e: Model.stack_entry) => e.e_id == fid,
                f.f_entries,
              );
            let master_seg = Focus.splice_entry(closing, f.f_master_seg);
            let rest =
              List.filter(
                (e: Model.stack_entry) => e.e_id != fid,
                f.f_entries,
              );
            switch (rest) {
            | [] =>
              let restored =
                Focus.spliced_master(
                  Model.{
                    f_entries: [],
                    f_master_seg: master_seg,
                  },
                  scratchpad,
                );
              {
                ...model,
                scratchpads:
                  ListUtil.put_nth(
                    model.current,
                    restored,
                    model.scratchpads,
                  ),
                focus: None,
              }
              |> Updated.return;
            | _ =>
              {
                ...model,
                focus:
                  Some(
                    Model.{
                      f_entries: rest,
                      f_master_seg: master_seg,
                    },
                  ),
              }
              |> Updated.return
            };
          } else if (List.exists(
                       (e: Model.stack_entry) =>
                         e.e_run && List.mem(fid, e.e_members),
                       f.f_entries,
                     )) {
            /* the id lives inside an OPEN run cell: its ⊖ closes the
               run (the row reads as pinned because the run covers it) */
            schedule_action(FocusToggleRun(fid));
            model |> Updated.return_quiet;
          } else {
            /* add to the stack, keeping program order. Pinning a
               PARENT (module/fn) first splices its pinned descendants
               home and unpins them — the parent's cell holds their
               content (andrew: parent-pin unpins children). */
            let info_map = editor.editor.statics.info_map;
            let term = editor.editor.statics.term;
            let desc = OutlineTree.descendant_ids(fid, term);
            let (closing, keeping) =
              List.partition(
                (e: Model.stack_entry) => List.mem(e.e_id, desc),
                f.f_entries,
              );
            let master_seg =
              List.fold_left(
                (seg, e) => Focus.splice_entry(e, seg),
                f.f_master_seg,
                closing,
              );
            switch (
              Focus.mk_entry(
                ~info_map,
                ~sym=?outline_sym(fid, editor.editor.statics.term),
                fid,
                master_seg,
              )
            ) {
            | None => model |> Updated.return_quiet
            | Some(entry) =>
              {
                ...model,
                focus:
                  Some(
                    Model.{
                      f_entries: insert_entry(~term, entry, keeping),
                      f_master_seg: master_seg,
                    },
                  ),
              }
              |> Updated.return
            };
          }
        }
      | Drv(_) => model |> Updated.return_quiet
      };
    | FocusToggleRun(fid) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Drv(_) => model |> Updated.return_quiet
      | Code({editor, _}) =>
        let info_map = editor.editor.statics.info_map;
        let unfocus_with = (master_seg: Haz3lcore.Segment.t) => {
          let restored =
            Focus.spliced_master(
              Model.{
                f_entries: [],
                f_master_seg: master_seg,
              },
              scratchpad,
            );
          {
            ...model,
            scratchpads:
              ListUtil.put_nth(model.current, restored, model.scratchpads),
            focus: None,
          }
          |> Updated.return;
        };
        switch (model.focus) {
        | None =>
          let master_seg = Focus.zip_of_cell(editor);
          switch (Focus.mk_run_entry(~info_map, fid, master_seg)) {
          | None => model |> Updated.return_quiet
          | Some(entry) =>
            {
              ...model,
              focus:
                Some(
                  Model.{
                    f_entries: [entry],
                    f_master_seg: master_seg,
                  },
                ),
            }
            |> Updated.return
          };
        | Some(f) =>
          let covering =
            List.find_opt(
              (e: Model.stack_entry) =>
                e.e_run && (e.e_id == fid || List.mem(fid, e.e_members)),
              f.f_entries,
            );
          switch (covering) {
          | Some(run) =>
            /* toggle OFF: splice the run cell home */
            let master_seg = Focus.splice_entry(run, f.f_master_seg);
            let rest =
              List.filter(
                (e: Model.stack_entry) => !(e === run),
                f.f_entries,
              );
            rest == []
              ? unfocus_with(master_seg)
              : {
                  ...model,
                  focus:
                    Some(
                      Model.{
                        f_entries: rest,
                        f_master_seg: master_seg,
                      },
                    ),
                }
                |> Updated.return;
          | None =>
            let members =
              switch (Focus.test_run(fid, f.f_master_seg)) {
              | Some((_, _, ms)) => ms
              | None => [fid]
              };
            let (member_entries, keeping) =
              List.partition(
                (e: Model.stack_entry) => List.mem(e.e_id, members),
                f.f_entries,
              );
            let master_seg =
              List.fold_left(
                (seg, e) => Focus.splice_entry(e, seg),
                f.f_master_seg,
                member_entries,
              );
            let all_open =
              members != []
              && List.length(member_entries) == List.length(members);
            if (all_open) {
              /* the container's ⊖ with every test open individually:
                 close them all */
              keeping == []
                ? unfocus_with(master_seg)
                : {
                    ...model,
                    focus:
                      Some(
                        Model.{
                          f_entries: keeping,
                          f_master_seg: master_seg,
                        },
                      ),
                  }
                  |> Updated.return;
            } else {
              switch (Focus.mk_run_entry(~info_map, fid, master_seg)) {
              | None => model |> Updated.return_quiet
              | Some(entry) =>
                {
                  ...model,
                  focus:
                    Some(
                      Model.{
                        f_entries:
                          insert_entry(
                            ~term=editor.editor.statics.term,
                            entry,
                            keeping,
                          ),
                        f_master_seg: master_seg,
                      },
                    ),
                }
                |> Updated.return
              };
            };
          };
        };
      };
    | RestorePins =>
      switch (Persist.pending_pins^) {
      | None => model |> Updated.return_quiet
      | Some((ck, _))
          when
            ck
            != Persist.content_key(
                 is_documentation ? "doc" : "scratch",
                 List.nth(model.scratchpads, model.current).name,
               ) =>
        /* read for a different slide/mode: never resolve against
           whatever document happens to be current now */
        Persist.pending_pins := None;
        model |> Updated.return_quiet;
      | Some((_, pins)) =>
        let scratchpad = List.nth(model.scratchpads, model.current);
        switch (scratchpad.kind) {
        | Code({editor, _})
            when
              List.exists(
                (n: OutlineTree.node) => n.o_label != "",
                OutlineTree.of_term(editor.editor.statics.term),
              ) =>
          Persist.pending_pins := None;
          let term = editor.editor.statics.term;
          List.iter(
            ((path, run)) =>
              switch (OutlineTree.resolve_path(path, term)) {
              | Some(id) =>
                schedule_action(run ? FocusToggleRun(id) : FocusToggle(id))
              | None => ()
              },
            pins,
          );
          model |> Updated.return_quiet;
        | _ => model |> Updated.return_quiet /* statics not ready: retry */
        };
      }
    | FocusEnsure(fid) =>
      /* cross-cell jump support: add [fid] to the stack iff absent
         (never removes; requires an open stack — the master handles
         its own jumps) */
      switch (model.focus) {
      | None => model |> Updated.return_quiet
      | Some(f) =>
        if (List.exists((e: Model.stack_entry) => e.e_id == fid, f.f_entries)) {
          model |> Updated.return_quiet;
        } else {
          let scratchpad = List.nth(model.scratchpads, model.current);
          switch (scratchpad.kind) {
          | Drv(_) => model |> Updated.return_quiet
          | Code({editor, _}) =>
            let info_map = editor.editor.statics.info_map;
            switch (
              Focus.mk_entry(
                ~info_map,
                ~sym=?outline_sym(fid, editor.editor.statics.term),
                fid,
                f.f_master_seg,
              )
            ) {
            | None => model |> Updated.return_quiet
            | Some(entry) =>
              {
                ...model,
                focus:
                  Some(
                    Model.{
                      ...f,
                      f_entries:
                        insert_entry(
                          ~term=editor.editor.statics.term,
                          entry,
                          f.f_entries,
                        ),
                    },
                  ),
              }
              |> Updated.return
            };
          };
        }
      }
    | RestoreCaret(p) =>
      /* clearing here (not at schedule time) makes delivery robust:
         the boot-time calculate runs with a no-op scheduler, so the
         ref keeps re-scheduling until a real action loop picks it up */
      Persist.pending_caret := None;
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Code({editor, agent}) =>
        let* new_ed =
          CellEditor.Update.update(
            ~settings,
            MainEditor(Perform(Move(Point(p, None)))),
            editor,
          );
        {
          ...model,
          scratchpads:
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
            ),
        };
      | Drv(_) => model |> Updated.return_quiet
      };
    | OutlineMenu(m) =>
      outline_menu := m;
      model |> Updated.return_quiet;
    | OutlineCollapse(path) =>
      let prefix = is_documentation ? "doc" : "scratch";
      let name = List.nth(model.scratchpads, model.current).name;
      let ck = Persist.content_key(prefix, name);
      let cur = collapse_paths(prefix, name);
      let next =
        List.mem(path, cur)
          ? List.filter(p => p != path, cur) : [path, ...cur];
      next == []
        ? Hashtbl.remove(slide_collapse, ck)
        : Hashtbl.replace(slide_collapse, ck, next);
      Persist.write_collapse(prefix, name);
      model |> Updated.return_quiet;
    | OutlineDefOp(op, fid) =>
      outline_menu := None;
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Drv(_) => model |> Updated.return_quiet
      | Code({editor, agent}) =>
        let live_seg =
          switch (model.focus) {
          | Some(f) => Focus.splice_all(f)
          | None => Focus.zip_of_cell(editor)
          };
        switch (Restructure.apply(op, fid, live_seg)) {
        | None => model |> Updated.return_quiet
        | Some((new_seg, focus_target)) =>
          /* Statics are seeded SYNCHRONOUSLY: the outline reads the
             master's statics.term, and while a stack is open the
             master's own calculate is skipped — a fresh empty statics
             would blank the outline. Probe-aware (union of master +
             open-cell zippers), so this single whole-program parse
             also serves as the stacked-statics frame (no second
             Force parse next frame). */
          let probe_union = (a, b) =>
            Haz3lcore.Id.Map.union((_, x, _) => Some(x), a, b);
          let entry_probes =
            switch (model.focus) {
            | None => Haz3lcore.Id.Map.empty
            | Some(f) =>
              List.fold_left(
                (acc, e: Model.stack_entry) =>
                  probe_union(
                    acc,
                    Haz3lcore.CachedStatics.probe_ids_of_zipper(
                      e.e_body.editor.editor.state.zipper,
                    ),
                  ),
                Haz3lcore.Id.Map.empty,
                f.f_entries,
              )
            };
          let probe_ids =
            probe_union(
              entry_probes,
              Haz3lcore.CachedStatics.probe_ids_of_zipper(
                editor.editor.editor.state.zipper,
              ),
            );
          let statics =
            settings.core.statics
              ? Haz3lcore.CachedStatics.init_compositional_term(
                  ~settings=settings.core,
                  ~probe_ids,
                  MakeTerm.Incr.term_of(new_seg),
                )
              : Haz3lcore.CachedStatics.empty;
          let stays_stacked =
            switch (model.focus) {
            | None => false
            | Some(f) =>
              (
                op == OutlineSidebar.Delete
                  ? List.filter(
                      (e: Model.stack_entry) => e.e_id != fid,
                      f.f_entries,
                    )
                  : f.f_entries
              )
              != []
            };
          let new_editor: CellEditor.Model.t =
            if (stays_stacked) {
              {
                /* master hidden while stacked: SKIP the whole-program
                   editor rebuild (cell_of_seg re-measures everything,
                   seconds on mega) — the zipper goes stale but every
                   consumer while stacked reads f_master_seg, and
                   unfocus rebuilds from it */

                editor: {
                  ...editor.editor,
                  statics,
                },
                result: editor.result,
              };
            } else {
              /* master (re)becomes visible — including when this op
                 deletes the LAST open cell: a stale zipper here would
                 resurrect the deleted def on the next calculate */
              let fresh = Focus.cell_of_seg(new_seg);
              {
                editor: {
                  ...fresh.editor,
                  statics,
                },
                result: editor.result,
              };
            };
          let new_sp = {
            ...scratchpad,
            kind:
              Code({
                editor: new_editor,
                agent,
              }),
          };
          /* a DELETEd definition's open cell closes with it; an empty
             stack unfocuses (the rebuilt master is already live) */
          let focus =
            switch (model.focus) {
            | None => None
            | Some(f) =>
              let entries =
                op == OutlineSidebar.Delete
                  ? List.filter(
                      (e: Model.stack_entry) => e.e_id != fid,
                      f.f_entries,
                    )
                  : f.f_entries;
              entries == []
                ? None
                : Some(
                    Model.{
                      f_entries: entries,
                      f_master_seg: new_seg,
                    },
                  );
            };
          /* the op may have landed INSIDE an open cell (a nested row
             of an open def): that cell's zipper is authoritative on
             the next splice and would silently ERASE the edit — and
             opening the created subdef as its own cell would overlap
             the parent. Rebuild containing cells from the post-op
             segment instead, and keep focus inside the parent. */
          let entry_contains = (e: Model.stack_entry, id: Haz3lcore.Id.t) =>
            e.e_id != id
            && (
              Focus.seg_contains_id(id, Focus.zip_of_cell(e.e_body))
              || Focus.seg_contains_id(id, Focus.zip_of_cell(e.e_header))
            );
          let op_inside_open =
            switch (focus) {
            | Some(f) =>
              List.exists(e => entry_contains(e, fid), f.f_entries)
            | None => false
            };
          let focus =
            switch (focus) {
            | None => None
            | Some(f) =>
              op_inside_open
                ? Some(
                    Model.{
                      ...f,
                      f_entries:
                        List.map(
                          (e: Model.stack_entry) =>
                            entry_contains(e, fid)
                              ? switch (
                                  Focus.mk_entry(
                                    ~info_map=statics.info_map,
                                    ~sym=?outline_sym(e.e_id, statics.term),
                                    e.e_id,
                                    new_seg,
                                  )
                                ) {
                                | Some(e') => e'
                                | None => e
                                }
                              : e,
                          f.f_entries,
                        ),
                    },
                  )
                : Some(f)
            };
          /* single-parse restructure: [statics] IS the stacked frame.
             Seed the slot and recapture the open cells' frozen ctxs
             from the fresh DefStatics items (a deleted/moved upstream
             def changes what downstream cells see) — no Force pass. */
          let focus =
            switch (focus) {
            | None =>
              stacked_statics := None;
              None;
            | Some(f) =>
              stacked_statics := Some(statics);
              let ds_items =
                switch (Haz3lcore.DefStatics.current()) {
                | Some(ds) => ds.items
                | None => []
                };
              let f_entries =
                List.map(
                  (e: Model.stack_entry) =>
                    switch (
                      List.find_opt(
                        (it: Haz3lcore.DefStatics.item) =>
                          it.d_id == e.e_id
                          || Haz3lcore.Id.Map.mem(e.e_id, it.d_map),
                        ds_items,
                      )
                    ) {
                    | Some(it) =>
                      switch (Focus.cell_content(e, new_seg)) {
                      | Some(content) =>
                        switch (
                          Focus.captured_ctx(
                            ~info_map=it.d_map,
                            e.e_id,
                            content,
                          )
                        ) {
                        | Some(ctx) => {
                            ...e,
                            e_ctx: ctx,
                          }
                        | None => e
                        }
                      | None => e
                      }
                    | None => e
                    },
                  f.f_entries,
                );
              Some(
                Model.{
                  ...f,
                  f_entries,
                },
              );
            };
          switch (focus_target) {
          | Some(_) when op_inside_open => () /* shown in the parent */
          | Some(id) =>
            schedule_action(
              focus == None ? FocusToggle(id) : FocusEnsure(id),
            )
          | None => ()
          };
          {
            ...model,
            scratchpads:
              ListUtil.put_nth(model.current, new_sp, model.scratchpads),
            focus,
          }
          |> Updated.return;
        };
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
        {
          ...model,
          scratchpads:
            ListUtil.put_nth(model.current, restored, model.scratchpads),
          focus: None,
        }
        |> Updated.return;
      }
    | StackHeader(i, a) =>
      switch (model.focus) {
      | None => model |> Updated.return_quiet
      | Some(f) =>
        switch (List.nth_opt(f.f_entries, i)) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          let* new_header =
            CellEditor.Update.update(~settings, a, entry.e_header);
          {
            ...model,
            focus:
              Some(
                Model.{
                  ...f,
                  f_entries:
                    ListUtil.put_nth(
                      i,
                      {
                        ...entry,
                        e_header: new_header,
                      },
                      f.f_entries,
                    ),
                },
              ),
          };
        }
      }
    | StackBody(i, a) =>
      switch (model.focus) {
      | None => model |> Updated.return_quiet
      | Some(f) =>
        switch (List.nth_opt(f.f_entries, i)) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          let* new_body =
            CellEditor.Update.update(~settings, a, entry.e_body);
          {
            ...model,
            focus:
              Some(
                Model.{
                  ...f,
                  f_entries:
                    ListUtil.put_nth(
                      i,
                      {
                        ...entry,
                        e_body: new_body,
                      },
                      f.f_entries,
                    ),
                },
              ),
          };
        }
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
      {
        let name = List.nth(model.scratchpads, model.current).name;
        let ck =
          Persist.content_key(is_documentation ? "doc" : "scratch", name);
        switch (model.focus) {
        | Some(f) =>
          Hashtbl.replace(
            slide_pins,
            ck,
            List.map(
              (e: Model.stack_entry) => (e.e_id, e.e_run),
              f.f_entries,
            ),
          )
        | None => Hashtbl.remove(slide_pins, ck)
        };
      };
      let model = commit_focus(model);
      WorkerClient.cancel();
      /* hydration (parse + first statics) can take seconds on large
         slides: paint a loading frame first, then hydrate. A plain
         schedule_action drains before the next render, so defer via a
         real timer. */
      ignore(
        Js_of_ocaml.Dom_html.window##setTimeout(
          Js_of_ocaml.Js.wrap_callback(() => schedule_action(HydrateCurrent)),
          30.,
        ),
      );
      {
        ...model,
        current: i,
      }
      |> Updated.return(~historic=false);
    | HydrateCurrent =>
      let model =
        Persist.hydrate_current(
          ~settings=settings.core,
          is_documentation ? "doc" : "scratch",
          model,
        );
      {
        /* restore this slide's pins (stale ids no-op in FocusToggle) */

        let name = List.nth(model.scratchpads, model.current).name;
        let ck =
          Persist.content_key(is_documentation ? "doc" : "scratch", name);
        switch (Hashtbl.find_opt(slide_pins, ck)) {
        | Some(ids) when model.focus == None =>
          List.iter(
            ((id, run)) =>
              schedule_action(run ? FocusToggleRun(id) : FocusToggle(id)),
            ids,
          )
        | _ => ()
        };
      };
      model |> Updated.return(~historic=false);
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

  /* per-entry calculate memo (see calc_entry): FIXPOINT check. An
     entry that comes in physically identical to the last calculate's
     OUTPUT is already calculated — update only replaces an entry's
     record when it's edited, so unchanged entries hit this on every
     recalculate (evaluator-streaming actions trigger them
     constantly). Reuse also preserves the entry's physical identity,
     which the stack view cache keys on. */
  let calc_entry_memo:
    Hashtbl.t(
      Haz3lcore.Id.t,
      (Language.CoreSettings.t, Language.Dynamics.Map.t, Model.stack_entry),
    ) =
    Hashtbl.create(8);

  let calculate =
      (
        ~settings,
        ~autoprobe_mode,
        ~schedule_action,
        ~is_edited,
        ~is_documentation: bool,
        model: Model.t,
      )
      : Model.t => {
    let statics_mode =
      CodeWithStatics.StaticsDebounce.consume(~is_edited, ~schedule_refresh=() =>
        schedule_action(RefreshStatics)
      );

    let scratchpad = List.nth(model.scratchpads, model.current);
    /* pending restore state applies only to the slide it was read
       for: the tag check keeps a hydration/mode-switch race from
       moving some OTHER current editor */
    let cur_ck =
      Persist.content_key(
        is_documentation ? "doc" : "scratch",
        scratchpad.name,
      );
    switch (scratchpad.kind) {
    | Code({editor, agent}) =>
      /* restore a loaded slide's saved caret: the Move runs as its own
         follow-up action, after this calculate builds measured */
      switch (Persist.pending_caret^) {
      | Some((ck, p)) when ck == cur_ck => schedule_action(RestoreCaret(p))
      | Some(_) => Persist.pending_caret := None /* stale: drop */
      | None => ()
      };
      switch (Persist.pending_pins^) {
      | Some((ck, _))
          when
            ck == cur_ck
            && model.focus == None
            && List.exists(
                 (n: OutlineTree.node) => n.o_label != "",
                 OutlineTree.of_term(editor.editor.statics.term),
               ) =>
        /* only once statics carries a NAMED outline: hydration's
           first frames run against placeholder/hole programs (whose
           outline is a lone unnamed ⇒ row), and resolving there
           would silently drop the pins */
        schedule_action(RestorePins)
      | _ => ()
      };
      let worker_request = ref([]);
      let queue_worker =
        Some(
          (req_value: WorkerServer.Request.value) => {
            worker_request := worker_request^ @ [("", req_value)]
          },
        );
      /* calculate every stack cell: bodies with their frozen ctx
         (statics off entirely for non-Exp roots, i.e. type bodies);
         headers molding-only. Memoized per entry: a keystroke in one
         cell must not re-run statics for the others (their zippers
         are unchanged), and reuse must preserve entry IDENTITY so the
         stack view cache can hit. Force-refresh frames recompute. */
      let statics_off = (cs: Language.CoreSettings.t) =>
        Language.CoreSettings.{
          ...cs,
          statics: false,
          dynamics: false,
        };
      /* TRACK B while a stack is open: on the debounced Force frame,
         re-run compositional statics on the SPLICED program so
         cross-cell effects propagate — a rename/retype in one cell
         errors its dependents, and the outline badges update. Only
         dirty items re-analyze; open cells whose item changed get
         their frozen ctx recaptured (a fresh entry record), which
         forces their own recalc below. */
      if (model.focus == None) {
        stacked_statics := None;
        stacked_incr_cache := Haz3lcore.MakeTerm.Incr.mk_cache();
      };
      let model =
        switch (model.focus) {
        | Some(f)
            when
              statics_mode == CodeWithStatics.StaticsForce
              || stacked_statics^ == None =>
          let prev_items =
            switch (Haz3lcore.DefStatics.current()) {
            | Some(p) => p.items
            | None => []
            };
          let spliced = Focus.splice_all(f);
          /* per-item incremental parse: unchanged items reuse their
             terms (parity test-gated); a one-cell edit re-parses one
             item instead of the whole program (~165ms at 2k) */
          let term =
            Haz3lcore.MakeTerm.Incr.go_incr(
              ~root=editor.editor.editor.root,
              ~cache=stacked_incr_cache^,
              spliced,
            ).
              term;
          /* probes live in ZIPPERS: union the master's with every open
             cell's, so a probe placed in a cell reaches the
             whole-program evaluation */
          let probe_union = (a, b) =>
            Id.Map.union((_, x, _) => Some(x), a, b);
          let probe_ids =
            List.fold_left(
              (acc, e: Model.stack_entry) =>
                probe_union(
                  probe_union(
                    acc,
                    Haz3lcore.CachedStatics.probe_ids_of_zipper(
                      e.e_body.editor.editor.state.zipper,
                    ),
                  ),
                  /* header probes too: projected statics make header
                     positions probeable */
                  Haz3lcore.CachedStatics.probe_ids_of_zipper(
                    e.e_header.editor.editor.state.zipper,
                  ),
                ),
              Haz3lcore.CachedStatics.probe_ids_of_zipper(
                editor.editor.editor.state.zipper,
              ),
              f.f_entries,
            );
          let clamped = Haz3lcore.DefStatics.clamp^;
          let ds =
            Haz3lcore.DefStatics.calc_auto(
              ~settings,
              ~propagate=!clamped,
              ~probe_ids,
              term,
            );
          /* W2 stacked-mode sync: ship the SPLICED program (this is
             the coherent segment/statics moment while stacked; the
             Main.after_display hook covers only the unstacked case) */
          switch (editor.editor.editor.root) {
          | Exp
          | Mod =>
            ShadowResidency.on_master_statics(
              ~key=ShadowResidency.master_key,
              ~root=editor.editor.editor.root,
              ~settings,
              spliced,
              ds,
            )
          | _ => ()
          };
          stacked_statics :=
            Some(
              Haz3lcore.CachedStatics.{
                term,
                elaborated:
                  clamped
                    /* worker-resident dynamics: sentinel keeps the
                       eval-request cadence (see CachedStatics) */
                    ? Haz3lcore.CachedStatics.dh_err(
                        "w2-resident:"
                        ++ string_of_int(Haz3lcore.DefStatics.semantic_gen^),
                      )
                    : (
                      switch (Haz3lcore.DefStatics.whole_elab(ds)) {
                      | Some(elab) => elab
                      | None =>
                        Haz3lcore.CachedStatics.dh_err(
                          "Compositional elaboration gap",
                        )
                      }
                    ),
                info_map: ds.merged,
                error_ids: Haz3lcore.DefStatics.all_error_ids(ds),
                warning_ids: Haz3lcore.DefStatics.all_warning_ids(ds),
                targets:
                  Haz3lcore.CachedStatics.compute_targets(
                    ~settings,
                    ~info_map=ds.merged,
                    ~probe_ids,
                  ),
                probe_ids,
              },
            );
          let fresh = it => !List.exists(p => p === it, prev_items);
          let f_entries =
            List.map(
              (e: Model.stack_entry) =>
                switch (
                  /* the entry may be a MODULE MEMBER: its containing
                     top-level item is the one whose map knows its id */
                  List.find_opt(
                    (it: Haz3lcore.DefStatics.item) =>
                      it.d_id == e.e_id || Id.Map.mem(e.e_id, it.d_map),
                    ds.items,
                  )
                ) {
                | Some(it) when fresh(it) =>
                  switch (Focus.cell_content(e, spliced)) {
                  | Some(def_seg) =>
                    switch (
                      Focus.captured_ctx(~info_map=it.d_map, e.e_id, def_seg)
                    ) {
                    | Some(ctx) => {
                        ...e,
                        e_ctx: ctx,
                      }
                    | None => e
                    }
                  | None => e
                  }
                | _ => e
                },
              f.f_entries,
            );
          {
            ...model,
            focus:
              Some(
                Model.{
                  ...f,
                  f_entries,
                },
              ),
          };
        | _ => model
        };
      /* While a stack is open the master's zipper cannot change (all
         edits route to stack cells; splices happen in update), so its
         EDITOR calculate is skipped — but its RESULT keeps evaluating
         the SPLICED program (stacked_statics): whole-program dynamics
         stays live while stacked. Requests only fire when the grafted
         elaboration actually changed (Calc-gated inside). */
      let new_ed =
        switch (model.focus, stacked_statics^) {
        | (Some(_), Some(synth)) =>
          let result =
            EvalResult.Update.calculate(
              ~settings={
                ...settings,
                assist: false,
              },
              ~queue_worker,
              /* the master's editor (and its pending highlight) isn't
                 rendered while stacked: skip the O(program) worklist */
              ~compute_pending=false,
              ~is_edited,
              synth,
              editor.result,
            );
          {
            ...editor,
            result,
          };
        | (Some(_), None) => editor
        | (None, _) =>
          CellEditor.Update.calculate(
            ~settings,
            ~autoprobe_mode,
            ~is_edited,
            ~statics_mode,
            ~compositional=true,
            ~queue_worker,
            ~stitch=x => x,
            editor,
          )
        };
      /* whole-program samples flow into every cell (probes with
         out-of-cell call sites); the memo gates on the dynamics map's
         identity so cells re-render when new samples land */
      let extra_dyn =
        switch (model.focus) {
        | Some(_) => EvalResult.Model.dynamics(new_ed.result)
        | None => Language.Dynamics.Map.empty
        };
      let calc_entry = (e: Model.stack_entry): Model.stack_entry => {
        let reuse =
          statics_mode != CodeWithStatics.StaticsForce
            ? switch (Hashtbl.find_opt(calc_entry_memo, e.e_id)) {
              | Some((s', d', prev))
                  when prev === e && s' === settings && d' === extra_dyn =>
                Some(prev)
              | _ => None
              }
            : None;
        switch (reuse) {
        | Some(prev) => prev
        | None =>
          let body_is_exp = e.e_body.editor.editor.root == Haz3lcore.Sort.Exp;
          let body_is_typ = e.e_body.editor.editor.root == Haz3lcore.Sort.Typ;
          /* PROJECTION: on Force frames (fresh DefStatics just ran on
             the spliced program earlier in this calculate), cells read
             their item's analysis instead of re-running a private one.
             Built only on Force — the statics gate inside only
             consults it then. */
          let (proj_header, proj_body) =
            statics_mode == CodeWithStatics.StaticsForce
              ? {
                switch (Haz3lcore.DefStatics.current()) {
                | Some(ds) =>
                  switch (
                    List.find_opt(
                      (it: Haz3lcore.DefStatics.item) =>
                        it.d_id == e.e_id
                        || Haz3lcore.Id.Map.mem(e.e_id, it.d_map),
                      ds.items,
                    )
                  ) {
                  | Some(it) =>
                    let warns = Haz3lcore.DefStatics.all_warning_ids(ds);
                    (
                      Some(
                        project_cell_statics(
                          ~item=it,
                          ~engine_warnings=warns,
                          e.e_header,
                        ),
                      ),
                      Some(
                        project_cell_statics(
                          ~item=it,
                          ~engine_warnings=warns,
                          e.e_body,
                        ),
                      ),
                    );
                  | None => (None, None)
                  }
                | None => (None, None)
                };
              }
              : (None, None);
          /* type bodies: STATICS on, dynamics off */
          let body_settings =
            body_is_exp
              ? settings
              : body_is_typ || proj_body != None
                  ? Language.CoreSettings.{
                      ...settings,
                      dynamics: false,
                    }
                  : statics_off(settings);
          let e' =
            Model.{
              ...e,
              e_header:
                /* headers: projected item statics when available
                   (real binder types, warnings, MPat info for module
                   headers); wrapped init_pat/init_tpat as fallback.
                   Module headers stay statics-off only when no
                   projection exists (the Pat wrapper misreads MPat). */
                CellEditor.Update.calculate(
                  ~settings=
                    e.e_mod && proj_header == None
                      ? statics_off(settings)
                      : Language.CoreSettings.{
                          ...settings,
                          dynamics: false,
                        },
                  ~is_edited,
                  ~statics_mode,
                  ~ctx=e.e_ctx,
                  ~projected=?proj_header,
                  ~queue_worker=None,
                  ~stitch=x => x,
                  e.e_header,
                ),
              e_body:
                CellEditor.Update.calculate(
                  ~settings=body_settings,
                  ~is_edited,
                  ~statics_mode,
                  ~ctx=e.e_ctx,
                  ~projected=?proj_body,
                  ~extra_dynamics=extra_dyn,
                  ~queue_worker=None,
                  ~stitch=x => x,
                  e.e_body,
                ),
            };
          Hashtbl.replace(
            calc_entry_memo,
            e.e_id,
            (settings, extra_dyn, e'),
          );
          e';
        };
      };
      let model = {
        ...model,
        focus:
          Option.map(
            (f: Model.focus_t) =>
              Model.{
                ...f,
                f_entries: List.map(calc_entry, f.f_entries),
              },
            model.focus,
          ),
      };
      /* W2 unstacked sync: MUST ship before the eval batch posts —
         a Resident eval references the worker's resident program, and
         postMessage order is the only thing keeping it current (the
         stacked path ships at its Force site above; shipping from
         after_display was one edit LATE and evals ran on the previous
         generation) */
      switch (model.focus) {
      | None =>
        let ed =
          switch (List.nth(model.scratchpads, model.current).kind) {
          | Code({editor, _}) => Some(editor.editor)
          | Drv(_) => None
          };
        switch (ed, Haz3lcore.DefStatics.current()) {
        | (Some(ed), Some(ds)) =>
          switch (ed.editor.root) {
          | Exp
          | Mod =>
            ShadowResidency.on_master_statics(
              ~key=ShadowResidency.master_key,
              ~root=ed.editor.root,
              ~settings,
              ed.editor.syntax.segment,
              ds,
            )
          | _ => ()
          }
        | _ => ()
        };
      | Some(_) => () /* shipped at the stacked Force site */
      };
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
    | StackH(int, CellEditor.Selection.t)
    | StackB(int, CellEditor.Selection.t)
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
      | (StackH(i, selection), Code(_)) =>
        switch (
          Option.bind(model.focus, (f: Model.focus_t) =>
            List.nth_opt(f.f_entries, i)
          )
        ) {
        | Some(entry) =>
          let+ a =
            CellEditor.Selection.get_cursor_info(
              ~inject=a => inject(StackHeader(i, a)),
              ~selection,
              entry.e_header,
            );
          Update.StackHeader(i, a);
        | None => empty
        }
      | (StackB(i, selection), Code(_)) =>
        switch (
          Option.bind(model.focus, (f: Model.focus_t) =>
            List.nth_opt(f.f_entries, i)
          )
        ) {
        | Some(entry) =>
          let+ a =
            CellEditor.Selection.get_cursor_info(
              ~inject=a => inject(StackBody(i, a)),
              ~selection,
              entry.e_body,
            );
          Update.StackBody(i, a);
        | None => empty
        }
      | (Drv(selection), Drv(m)) =>
        let+ a =
          DerivationExerciseMode.Selection.get_cursor_info(
            ~inject=a => inject(DrvAction(a)),
            ~selection,
            m,
          );
        Update.DrvAction(a);
      | (Cell(_), Drv(_))
      | (StackH(_), Drv(_))
      | (StackB(_), Drv(_))
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

  /* Cross-cell jump-to-definition: a stack cell's jump whose binder is
     OUTSIDE the cell becomes (ensure the binder's outline item is in
     the stack, select the pane holding the binder, then a follow-up
     caret jump there). None = local jump or not a jump — take the
     normal path. */
  /* resolve a MASTER-domain id to a cross-cell jump while a stack is
     open: (open the containing item, focus the right pane, move its
     caret). Serves goto-definition from any pane AND result-strip /
     test jumps (which used to move the hidden master's caret). */
  let cross_cell_target =
      (~target_id: Haz3lcore.Id.t, ~model: Model.t, ~f: Model.focus_t)
      : option((Update.t, t, Update.t)) => {
    Util.OptUtil.Syntax.(
      {
        let scratchpad = List.nth(model.scratchpads, model.current);
        switch (scratchpad.kind) {
        | Drv(_) => None
        | Code({editor, _}) =>
          let statics = editor.editor.statics;
          let* info = Id.Map.find_opt(target_id, statics.info_map);
          /* the nearest enclosing outline item is the def to focus */
          let rec outline_ids = (acc, ns: list(OutlineTree.node)) =>
            List.fold_left(
              (acc, n: OutlineTree.node) =>
                outline_ids(
                  switch (n.o_id) {
                  | Some(id) => [id, ...acc]
                  | None => acc
                  },
                  n.o_children,
                ),
              acc,
              ns,
            );
          let items = outline_ids([], OutlineTree.of_term(statics.term));
          let* fid =
            List.find_opt(
              id => List.mem(id, items),
              [target_id, ...Language.Info.ancestors_of(info)],
            );
          let j = stack_position(~term=statics.term, fid, f.f_entries);
          /* the target lives in the pattern (header cell) for def
             binders, in the body for everything else */
          let in_header =
            Focus.seg_contains_id(
              target_id,
              Option.value(Focus.find_pat(fid, f.f_master_seg), ~default=[]),
            );
          let caret: CellEditor.Update.t =
            MainEditor(Perform(Move(Goal(TileId(target_id)))));
          Some((
            Update.FocusEnsure(fid),
            in_header ? StackH(j, MainEditor) : StackB(j, MainEditor),
            in_header
              ? Update.StackHeader(j, caret) : Update.StackBody(j, caret),
          ));
        };
      }
    );
  };

  let stack_jump_override =
      (action: Update.t, model: Model.t): option((Update.t, t, Update.t)) => {
    Util.OptUtil.Syntax.(
      switch (action, model.focus) {
      | (
          StackBody(
            i,
            MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
          ) |
          StackHeader(
            i,
            MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
          ),
          Some(f),
        ) =>
        let from_header =
          switch (action) {
          | StackHeader(_) => true
          | _ => false
          };
        let* entry = List.nth_opt(f.f_entries, i);
        let cell = from_header ? entry.Model.e_header : entry.Model.e_body;
        let cell_map = cell.editor.statics.info_map;
        let* ci = Indicated.ci_of(cell.editor.editor.state.zipper, cell_map);
        let* binding_id = Language.Info.get_binding_site(ci);
        if (Id.Map.mem(binding_id, cell_map)) {
          None; /* binder is inside this cell: the cell's own jump works */
        } else {
          cross_cell_target(~target_id=binding_id, ~model, ~f);
        };
      | _ => None
      }
    );
  };

  /* the selection an outline add/ensure should land on: the body pane
     of [fid] at its (future) stack position. None for removals — the
     selection stays put. */
  let stack_add_selection = (action: Update.t, model: Model.t): option(t) => {
    let target = (fid, entries) => {
      let scratchpad = List.nth(model.scratchpads, model.current);
      switch (scratchpad.kind) {
      | Drv(_) => None
      | Code({editor, _}) =>
        Some(
          StackB(
            stack_position(~term=editor.editor.statics.term, fid, entries),
            MainEditor,
          ),
        )
      };
    };
    switch (action, model.focus) {
    | (FocusEnsure(fid), Some(f)) => target(fid, f.f_entries)
    | (FocusToggle(fid), Some(f)) =>
      List.exists((e: Model.stack_entry) => e.e_id == fid, f.f_entries)
        ? None : target(fid, f.f_entries)
    | (FocusToggle(_), None) => Some(StackB(0, MainEditor))
    | _ => None
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

  /* Stack-cell view cache: with N cells open, a keystroke in one cell
     must not rebuild the other N-1 cell views (measured 150-380ms per
     keystroke at 5 cells vs 10-70ms at 1 on Mega 1k). Reusing the
     physically-same nodes also short-circuits the vdom diff. Keyed on
     everything the cell view reads; models/settings by physical
     identity, small values structurally. Pruned to the live stack
     every render. */
  type stack_cache_key = {
    k_index: int,
    k_stack_len: int, /* escape closures bound-check against it */
    k_header_sel: option(CellEditor.Selection.t),
    k_body_sel: option(CellEditor.Selection.t),
    k_meta_down: bool,
    k_visible_rows: option(Globals.VisibleRows.t),
  };
  type cached_cell = {
    c_key: stack_cache_key,
    c_header: CellEditor.Model.t,
    c_body: CellEditor.Model.t,
    c_settings: Settings.t,
    c_font_metrics: FontMetrics.t,
    c_colors: option(ColorSteps.colorMap),
    c_nodes: list(Virtual_dom.Vdom.Node.t),
  };
  let stack_cache: ref(list((Haz3lcore.Id.t, cached_cell))) = ref([]);

  /* IMPORTANT: the view must read the cache through this helper, never
     bind `stack_cache^` locally. jsoo closures share one context object
     per scope — with the previous generation bound in the view scope,
     every handler closure of render N retained render N-1's vdom
     (whose handlers retained N-2's …): a linked list of generations,
     measured at ~11MB leaked per edit on mega-1k. */
  let stack_cache_lookup = (id: Haz3lcore.Id.t): option(cached_cell) =>
    List.assoc_opt(id, stack_cache^);

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
    if (current.dormant) {
      [
        /* SwitchSlide painted this frame before hydration: the next
           update parses + runs first statics, which blocks for a bit on
           large slides */
        /* same spinner as the app boot screen (index.html/loading.css) */
        Virtual_dom.Vdom.Node.div(
          ~attrs=[Virtual_dom.Vdom.Attr.classes(["slide-loading"])],
          [
            Virtual_dom.Vdom.Node.div(
              ~attrs=[Virtual_dom.Vdom.Attr.classes(["spinner"])],
              [
                Virtual_dom.Vdom.Node.div(
                  ~attrs=[Virtual_dom.Vdom.Attr.classes(["loader"])],
                  [],
                ),
                Virtual_dom.Vdom.Node.div(
                  ~attrs=[Virtual_dom.Vdom.Attr.classes(["nut-container"])],
                  [
                    Virtual_dom.Vdom.Node.create(
                      "img",
                      ~attrs=[
                        Virtual_dom.Vdom.Attr.classes(["spinner-nut"]),
                        Virtual_dom.Vdom.Attr.create(
                          "src",
                          "img/hazelnut.svg",
                        ),
                      ],
                      [],
                    ),
                  ],
                ),
              ],
            ),
            Virtual_dom.Vdom.Node.text("loading "),
            Virtual_dom.Vdom.Node.text(current.name),
            Virtual_dom.Vdom.Node.text({js|…|js}),
          ],
        ),
      ];
    } else {
      switch (current.kind) {
      | Code({editor, _}) =>
        /* the STACK: [header band, body cell] per entry, thin rules
           between; rendered INSTEAD of the master cell */
        let stack_views = (f: Model.focus_t) => {
          let rendered =
            List.mapi(
              (i, e: Model.stack_entry) => {
                let header_sel =
                  switch (selected) {
                  | Some(Selection.StackH(j, sel)) when j == i => Some(sel)
                  | _ => None
                  };
                let body_sel =
                  switch (selected) {
                  | Some(Selection.StackB(j, sel)) when j == i => Some(sel)
                  | _ => None
                  };
                let key = {
                  k_index: i,
                  k_stack_len: List.length(f.f_entries),
                  k_header_sel: header_sel,
                  k_body_sel: body_sel,
                  k_meta_down: globals.Globals.Model.meta_down,
                  k_visible_rows: globals.Globals.Model.visible_rows,
                };
                switch (stack_cache_lookup(e.e_id)) {
                | Some(c)
                    when
                      c.c_key == key
                      && c.c_header === e.e_header
                      && c.c_body === e.e_body
                      && c.c_settings === globals.Globals.Model.settings
                      && c.c_font_metrics
                      === globals.Globals.Model.font_metrics
                      && c.c_colors === globals.Globals.Model.color_highlights => (
                    e.e_id,
                    c,
                  )
                | _ =>
                  /* qualifier chip: the def's module path (stable while
                     the stack is open — the master term is frozen) */
                  let qualifier =
                    switch (
                      OutlineTree.path_of(e.e_id, editor.editor.statics.term)
                    ) {
                    | [] => []
                    | path => [
                        Virtual_dom.Vdom.Node.span(
                          ~attrs=[
                            Virtual_dom.Vdom.Attr.classes([
                              "focus-qualifier",
                            ]),
                          ],
                          [
                            Virtual_dom.Vdom.Node.text(
                              String.concat(".", path) ++ ".",
                            ),
                          ],
                        ),
                      ]
                    };
                  /* arrow keys at a pane's edge walk the stack:
                     ... body(i-1) <- header(i) <-> body(i) -> header(i+1) ... */
                  let headerless = idx =>
                    switch (List.nth_opt(f.f_entries, idx)) {
                    | Some(e) => e.Model.e_sym != None
                    | None => false
                    };
                  let pane_focus =
                      (idx, to_header, move: Haz3lcore.Action.move) =>
                    if (idx < 0 || idx >= List.length(f.f_entries)) {
                      Virtual_dom.Vdom.Effect.Ignore;
                    } else {
                      /* headerless entries have no header pane */
                      let to_header = to_header && !headerless(idx);
                      /* DOM focus must follow the selection to the new
                         pane (after render — the active-cell id moves
                         with the re-render) or the caret vanishes and
                         arrows scroll the page */
                      Haz3lcore.ProbePerform.FocusEffect.schedule_cell();
                      Virtual_dom.Vdom.Effect.Many([
                        signal(
                          MakeActive(
                            to_header
                              ? StackH(idx, MainEditor)
                              : StackB(idx, MainEditor),
                          ),
                        ),
                        inject(
                          to_header
                            ? StackHeader(
                                idx,
                                MainEditor(Perform(Move(move))),
                              )
                            : StackBody(
                                idx,
                                MainEditor(Perform(Move(move))),
                              ),
                        ),
                      ]);
                    };
                  let header_escape = (d: Util.Direction.t) =>
                    switch (d) {
                    | Left => pane_focus(i - 1, false, End)
                    | Right => pane_focus(i, false, Start)
                    };
                  let body_escape = (d: Util.Direction.t) =>
                    switch (d) {
                    | Left =>
                      headerless(i)
                        ? pane_focus(i - 1, false, End)
                        : pane_focus(i, true, End)
                    | Right => pane_focus(i + 1, true, Start)
                    };
                  /* vertical escape: Up/Down at a pane's row edge move
                     straight to the adjacent pane at the same goal
                     column (no end-of-line snap first). Header editors
                     sit one qualifier-chip width right of body content,
                     so columns shift by the qualifier's length when
                     crossing a header boundary. At the stack's ends the
                     plain vertical move is re-dispatched (restores the
                     line-start/end snap). */
                  let qual_cols = idx =>
                    switch (List.nth_opt(f.f_entries, idx)) {
                    | Some(e) =>
                      switch (
                        OutlineTree.path_of(
                          e.Model.e_id,
                          editor.editor.statics.term,
                        )
                      ) {
                      | [] => 0
                      | path => String.length(String.concat(".", path)) + 1
                      }
                    | None => 0
                    };
                  let body_last_row = idx =>
                    switch (List.nth_opt(f.f_entries, idx)) {
                    | Some(e) =>
                      max(
                        0,
                        e.Model.e_body.editor.editor.syntax.measured.total_rows
                        - 1,
                      )
                    | None => 0
                    };
                  let pane_point = (idx, to_header, row, col) =>
                    pane_focus(
                      idx,
                      to_header,
                      Point(
                        Util.Point.{
                          row,
                          col: max(0, col),
                        },
                        None,
                      ),
                    );
                  let same_pane = (to_header, v: Haz3lcore.Action.vertical) =>
                    inject(
                      to_header
                        ? StackHeader(
                            i,
                            MainEditor(Perform(Move(Vertical(v, ByChar)))),
                          )
                        : StackBody(
                            i,
                            MainEditor(Perform(Move(Vertical(v, ByChar)))),
                          ),
                    );
                  let header_escape_vertical =
                      (v: Haz3lcore.Action.vertical, col) =>
                    switch (v) {
                    | Down => pane_point(i, false, 0, col + qual_cols(i))
                    | Up =>
                      i == 0
                        ? same_pane(true, Up)
                        : pane_point(
                            i - 1,
                            false,
                            body_last_row(i - 1),
                            col + qual_cols(i),
                          )
                    };
                  let body_escape_vertical =
                      (v: Haz3lcore.Action.vertical, col) =>
                    switch (v) {
                    | Down =>
                      i + 1 >= List.length(f.f_entries)
                        ? same_pane(false, Down)
                        : headerless(i + 1)
                            ? pane_point(i + 1, false, 0, col)
                            : pane_point(
                                i + 1,
                                true,
                                0,
                                col - qual_cols(i + 1),
                              )
                    | Up =>
                      headerless(i)
                        ? i == 0
                            ? same_pane(false, Up)
                            : pane_point(
                                i - 1,
                                false,
                                body_last_row(i - 1),
                                col,
                              )
                        : pane_point(i, true, 0, col - qual_cols(i))
                    };
                  let header_pane =
                    switch (e.e_sym) {
                    | Some(sym) =>
                      /* headerless items (statements, trailing expr):
                         a static symbol chip instead of a header cell */
                      Virtual_dom.Vdom.Node.div(
                        ~attrs=[
                          Virtual_dom.Vdom.Attr.classes([
                            "focus-header",
                            "focus-header-sym",
                          ]),
                        ],
                        /* no qualifier chip: the symbol IS the label
                           (a run cell was rendering "tests tests") */
                        [
                          Virtual_dom.Vdom.Node.span(
                            ~attrs=[
                              Virtual_dom.Vdom.Attr.classes(["focus-sym"]),
                            ],
                            [Virtual_dom.Vdom.Node.text(sym)],
                          ),
                        ],
                      )
                    | None =>
                      Virtual_dom.Vdom.Node.div(
                        ~attrs=[
                          Virtual_dom.Vdom.Attr.classes(["focus-header"]),
                        ],
                        qualifier
                        @ [
                          CellEditor.View.view(
                            ~globals,
                            ~signal=
                              fun
                              | MakeActive(sel) =>
                                signal(MakeActive(StackH(i, sel))),
                            ~inject=a => inject(StackHeader(i, a)),
                            ~selected=header_sel,
                            ~result_kind=`NoResults,
                            ~locked=false,
                            ~lines=false,
                            ~escape=header_escape,
                            ~escape_vertical=Some(header_escape_vertical),
                            e.e_header,
                          ),
                        ],
                      )
                    };
                  let nodes = [
                    header_pane,
                    Virtual_dom.Vdom.Node.div(
                      ~attrs=[Virtual_dom.Vdom.Attr.classes(["focus-body"])],
                      [
                        CellEditor.View.view(
                          ~globals,
                          ~signal=
                            fun
                            | MakeActive(sel) =>
                              signal(MakeActive(StackB(i, sel))),
                          ~inject=a => inject(StackBody(i, a)),
                          ~selected=body_sel,
                          ~result_kind=`NoResults,
                          ~locked=false,
                          ~lines=true,
                          ~master_result=editor.result,
                          ~escape=body_escape,
                          ~escape_vertical=Some(body_escape_vertical),
                          e.e_body,
                        ),
                      ],
                    ),
                  ];
                  (
                    e.e_id,
                    {
                      c_key: key,
                      c_header: e.e_header,
                      c_body: e.e_body,
                      c_settings: globals.Globals.Model.settings,
                      c_font_metrics: globals.Globals.Model.font_metrics,
                      c_colors: globals.Globals.Model.color_highlights,
                      c_nodes: nodes,
                    },
                  );
                };
              },
              f.f_entries,
            );
          stack_cache := rendered;
          /* the whole program's RESULT stays live below the stack (the
             master keeps evaluating the spliced program) */
          let (result_footer, _overlays) =
            EvalResult.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive(a) => signal(MakeActive(Cell(Result(a))))
                | JumpTo(id) =>
                  /* the jump target lives in the HIDDEN master while a
                     stack is open: open the containing item instead */
                  switch (
                    Selection.cross_cell_target(~target_id=id, ~model, ~f)
                  ) {
                  | Some((ensure, sel, caret)) =>
                    Virtual_dom.Vdom.Effect.Many([
                      inject(ensure),
                      signal(MakeActive(sel)),
                      inject(caret),
                    ])
                  | None =>
                    Virtual_dom.Vdom.Effect.Many([
                      signal(MakeActive(Cell(MainEditor))),
                      inject(
                        CellAction(
                          MainEditor(Perform(Move(Goal(TileId(id))))),
                        ),
                      ),
                    ])
                  },
              ~inject=a => inject(CellAction(ResultAction(a))),
              ~selected=
                switch (selected) {
                | Some(Selection.Cell(Result(a))) => Some(a)
                | _ => None
                },
              ~locked=false,
              editor.result,
            );
          List.concat_map(((_, c)) => c.c_nodes, rendered)
          @ [
            Virtual_dom.Vdom.Node.div(
              ~attrs=[Virtual_dom.Vdom.Attr.classes(["stack-result"])],
              result_footer,
            ),
          ]
          @ [
            /* trailing slack: any entry (incl. the last) can align to
               the viewport top, and the user can scroll to position any
               def where they like */
            Virtual_dom.Vdom.Node.div(
              ~attrs=[Virtual_dom.Vdom.Attr.classes(["stack-slack"])],
              [],
            ),
          ];
        };
        switch (model.focus) {
        | Some(f) =>
          (SlideContent.get_content(current.name) |> Option.to_list)
          @ stack_views(f)
        | None =>
          (SlideContent.get_content(current.name) |> Option.to_list)
          @ [
            CellEditor.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive(selection) =>
                  signal(MakeActive(Cell(selection))),
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
        };
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
