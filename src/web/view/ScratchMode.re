open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Focus = ScratchFocus;
module Restructure = ScratchRestructure;
module Persist = ScratchPersist;

/* per-slide pin/collapse side state lives with the persistence layer */
let slide_collapse = ScratchPersist.slide_collapse;
let collapse_paths = ScratchPersist.collapse_paths;

/* outline context-menu state (row id + screen position): transient
   UI, module-level like the other view caches — not model data */
let outline_menu: ref(option((Haz3lcore.Id.t, bool, float, float))) =
  ref(None);

/* the outline's keyboard cursor: a row path, None for the header row */
let outline_cursor: ref(option(OutlineTree.path)) = ref(None);

/* a name being typed in the outline, and the row just created from a
   `type `/`module ` name (its keyword animates away) */
let outline_edit: ref(option(OutlineEdit.t)) = ref(None);
let outline_created: ref(option((Haz3lcore.Id.t, string))) = ref(None);

/* the header symbol a headerless cell for [fid] should show, from
   the OUTLINE's view of the row (span kinds mis-read member-fn tails:
   a member terminates with `;`, so its fn-body tail extracts from an
   IStmt-shaped run — the row is still a ⇒) */
let outline_sym = SlideView.sym_of;

/* A stack cell's statics come from its DefStatics ITEM — the same ids, analyzed with
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
    completion: None,
    probe_ids:
      Haz3lcore.CachedStatics.probe_ids_of_zipper(
        cell.editor.editor.state.zipper,
      ),
  };
};
/* incremental-parse cache for the stacked Force frame: go_incr with
   a persistent cache replays the top frame exactly and re-parses only
   the edited item */
let stacked_incr_cache: ref(Haz3lcore.MakeTerm.Incr.cache) =
  ref(Haz3lcore.MakeTerm.Incr.mk_cache());

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
    };
  };
};

/* the current slide's open cells (none when it is whole) */
let current_cells = (model: Model.t): list(ScratchCell.t) =>
  switch (Model.current_program(model)) {
  | Some(Divided(d)) => Divided.cells(d)
  | _ => []
  };
let nth_cell = (model: Model.t, i: int): option(ScratchCell.t) =>
  List.nth_opt(current_cells(model), i);

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
    | UnfocusDef /* drop the pins shown at this level */
    | ZoomIn(Haz3lcore.Id.t)
    | ZoomOut
    | ZoomTo(option(Haz3lcore.Id.t)) /* a breadcrumb: None = the program */
    | ShowWhole(bool) /* park (true) or unpark the pins at this level */
    | RealizeView /* open and close cells to match the view (after undo) */
    | OutlineCursor(option(OutlineTree.path))
    | OutlineEdit(option(OutlineEdit.t))
    | OutlineCommit(OutlineEdit.t, bool) /* true: then a new one */
    | OutlineFocused /* the outline took keyboard focus */
    | FocusOutline
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

  let current_code = (model: Model.t): option(Scratchpad.code) =>
    switch (List.nth(model.scratchpads, model.current).kind) {
    | Code(c) => Some(c)
    | Drv(_) => None
    };

  let with_code = (model: Model.t, code: Scratchpad.code): Model.t => {
    let sp = List.nth(model.scratchpads, model.current);
    {
      ...model,
      scratchpads:
        ListUtil.put_nth(
          model.current,
          {
            ...sp,
            kind: Code(code),
          },
          model.scratchpads,
        ),
    };
  };

  /* change what the current slide shows, then open and close cells to
     match; view changes are not undo steps */
  let update_view =
      (
        model: Model.t,
        f: (Language.Exp.t, Program.t, SlideView.t) => SlideView.t,
      )
      : Updated.t(Model.t) =>
    switch (current_code(model)) {
    | None => model |> Updated.return_quiet
    | Some({program, view, _} as code) =>
      let statics = Program.statics(program);
      let (view, program) =
        SlideView.realize(
          ~info_map=statics.info_map,
          ~term=statics.term,
          f(statics.term, program, view),
          program,
        );
      with_code(
        model,
        {
          ...code,
          program,
          view,
        },
      )
      |> Updated.return(~historic=false);
    };

  /* after an edit to the whole program (agent, outline menu): the same
     view again, pins to vanished items dropped */
  let resync = (code: Scratchpad.code, program: Program.t): Scratchpad.code => {
    let statics = Program.statics(program);
    let (view, program) =
      SlideView.realize(
        ~info_map=statics.info_map,
        ~term=statics.term,
        code.view,
        program,
      );
    {
      ...code,
      program,
      view,
    };
  };

  /* the program after an outline edit: statics seeded now (the outline
     reads them, and the edit's one parse doubles as the next statics
     frame), manual probes and the caret kept, cells re-cut */
  let with_segment =
      (~settings: Settings.t, code: Scratchpad.code, new_seg: Segment.t)
      : Scratchpad.code => {
    let program = code.program;
    let root = Program.root(program);
    let present = {
      let ids = Segment.ids(new_seg);
      List.fold_left((m, id) => Id.Map.add(id, (), m), Id.Map.empty, ids);
    };
    let manuals =
      List.filter(
        ((id, _)) => Id.Map.mem(id, present),
        Program.probes(program),
      );
    let statics =
      settings.core.statics
        ? Haz3lcore.CachedStatics.init_compositional_term(
            ~settings=settings.core,
            ~probe_ids=
              List.fold_left(
                (m, (id, _)) => Id.Map.add(id, (), m),
                Id.Map.empty,
                manuals,
              ),
            root == Haz3lcore.Sort.Mod
              ? MakeTerm.Incr.term_of_mod(new_seg)
              : MakeTerm.Incr.term_of(new_seg),
          )
        : Haz3lcore.CachedStatics.empty;
    let z =
      Zipper.unzip(~direction=Left, new_seg)
      |> ZipperBase.update_refractors(_, r =>
           Refractors.{
             ...r,
             manuals,
           }
         );
    let z =
      switch (program) {
      | Whole(e) =>
        switch (Divided.anchor_of(e.editor.editor.state.zipper)) {
        | Some((side, id)) =>
          Option.value(Move.jump_to_side_of_id(side, z, id), ~default=z)
        | None => z
        }
      | Divided(_) => z
      };
    let fresh = CellEditor.Model.mk(Editor.Model.mk(z, ~root));
    let editor: CellEditor.Model.t = {
      editor: {
        ...fresh.editor,
        statics,
      },
      result: Program.result(program),
    };
    let program =
      switch (program) {
      | Whole(_) => Program.Whole(editor)
      | Divided(d) =>
        switch (
          Divided.resplit(
            ~info_map=statics.info_map,
            ~term=statics.term,
            editor,
            d,
          )
        ) {
        | Joined(e) => Program.Whole(e)
        | Still(d) => Program.Divided(Divided.with_statics(statics, d))
        }
      };
    resync(
      {
        ...code,
        program,
      },
      program,
    );
  };

  let export_scratch_slide = (model: Model.t): unit => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    switch (scratchpad.kind) {
    | Code({program, _}) =>
      let persistent = CellEditor.Model.persist(Program.whole(program));
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
    | Code({program, _}) =>
      let c = Program.whole(program) |> CellEditor.Model.to_string;
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
        action: t,
        model: Model.t,
      ) => {
    switch (action) {
    | AgentAction(a) =>
      switch (current_code(model)) {
      | None => model |> return_quiet
      | Some({program, agent, _} as code) =>
        let schedule_agent = (a: Agent.Update.Action.t) =>
          schedule_action(AgentAction(a));
        /* the agent reads and edits the whole program: a divided one is
           joined for it, and re-divided with the same cells if it
           changed anything */
        let editor = Program.whole_memo(program);
        let (new_agent, updated_editor) =
          Agent.Update.update(a, agent, editor, settings, schedule_agent);
        let* new_ed = updated_editor;
        let code = {
          ...code,
          agent: new_agent,
        };
        switch (program) {
        | Whole(_) =>
          with_code(
            model,
            {
              ...code,
              program: Whole(new_ed),
            },
          )
        | Divided(_) when new_ed === editor => with_code(model, code)
        | Divided(d) =>
          with_code(
            model,
            resync(
              code,
              Program.of_close(
                Divided.resplit(
                  ~info_map=new_ed.editor.statics.info_map,
                  ~term=new_ed.editor.statics.term,
                  new_ed,
                  d,
                ),
              ),
            ),
          )
        };
      }
    | FocusDef(fid) =>
      /* show only [fid] at this level */
      update_view(model, (term, _, v) =>
        SlideView.pin(~term, fid, SlideView.discard(~term, v))
      )
    | FocusToggle(fid) =>
      update_view(model, (term, program, v) =>
        if (List.mem(
              SlideView.{
                p_id: fid,
                p_run: false,
              },
              v.pins,
            )) {
          SlideView.unpin(fid, v);
        } else {
          /* inside an open run cell, the ⊖ closes the run */
          switch (
            switch (program) {
            | Divided(d) =>
              List.find_opt(
                (e: ScratchCell.t) => e.e_run && List.mem(fid, e.e_members),
                Divided.cells(d),
              )
            | Whole(_) => None
            }
          ) {
          | Some(run) => SlideView.unpin(run.e_id, v)
          | None => SlideView.pin(~term, fid, v)
          };
        }
      )
    | FocusToggleRun(fid) =>
      /* the tests container: one cell for the run, or close it (or its
         members open one by one) */
      update_view(
        model,
        (term, program, v) => {
          let members =
            switch (Focus.test_run_deep(fid, Program.document(program))) {
            | Some((_, ms)) => ms
            | None => [fid]
            };
          switch (
            List.find_opt(
              (p: SlideView.pin) => p.p_run && List.mem(p.p_id, members),
              v.pins,
            )
          ) {
          | Some(run) => SlideView.unpin(run.p_id, v)
          | None =>
            let singles =
              List.filter(
                (p: SlideView.pin) => !p.p_run && List.mem(p.p_id, members),
                v.pins,
              );
            let v =
              List.fold_left(
                (v, p: SlideView.pin) => SlideView.unpin(p.p_id, v),
                v,
                singles,
              );
            List.length(singles) == List.length(members)
              ? v : SlideView.pin(~term, ~run=true, fid, v);
          };
        },
      )
    | RestorePins =>
      /* each slide's saved view waits under its own key, so another
         slide hydrating first can't drop it */
      let ck =
        Persist.content_key(
          is_documentation ? "doc" : "scratch",
          List.nth(model.scratchpads, model.current).name,
        );
      switch (
        Hashtbl.find_opt(Persist.pending_pins, ck),
        current_code(model),
      ) {
      | (Some(saved), Some({program: Whole(editor), _}))
          when
            List.exists(
              (n: OutlineTree.node) => n.o_label != "",
              OutlineTree.of_term(editor.editor.statics.term),
            ) =>
        Hashtbl.remove(Persist.pending_pins, ck);
        update_view(model, (term, _, _) =>
          Persist.resolve_view(saved, term)
        );
      | _ => model |> Updated.return_quiet /* statics not ready: retry */
      };
    | FocusEnsure(fid) =>
      /* cross-cell jumps: show [fid] unless an open cell already holds
         it (only while divided) */
      update_view(model, (term, program, v) =>
        switch (program) {
        | Divided(d) when Divided.owner(fid, d) == None =>
          SlideView.pin(~term, fid, v)
        | _ => v
        }
      )
    | RestoreCaret(p) =>
      /* clearing here (not at schedule time) makes delivery robust:
         the boot-time calculate runs with a no-op scheduler, so the
         ref keeps re-scheduling until a real action loop picks it up */
      Hashtbl.remove(
        Persist.pending_caret,
        Persist.content_key(
          is_documentation ? "doc" : "scratch",
          List.nth(model.scratchpads, model.current).name,
        ),
      );
      switch (current_code(model)) {
      | Some({program: Whole(editor), _} as code) =>
        let* new_ed =
          CellEditor.Update.update(
            ~settings,
            MainEditor(Perform(Move(Point(p, None)))),
            editor,
          );
        with_code(
          model,
          {
            ...code,
            program: Whole(new_ed),
          },
        );
      | _ => model |> Updated.return_quiet
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
      switch (current_code(model)) {
      | None => model |> Updated.return_quiet
      | Some({program, _} as code) =>
        let seg = Program.document(program);
        let mod_root = Program.root(program) == Haz3lcore.Sort.Mod;
        let term = Program.statics(program).term;
        let result =
          switch (op) {
          | MoveUp
          | MoveDown =>
            /* moves cross module edges (into expanded modules, out at a
               module's first or last member) */
            let prefix = is_documentation ? "doc" : "scratch";
            let collapsed =
              collapse_paths(
                prefix,
                List.nth(model.scratchpads, model.current).name,
              );
            let is_open = id =>
              switch (OutlineTree.label_path(id, term)) {
              | Some(path) => !List.mem(path, collapsed)
              | None => true
              };
            let owner =
              switch (Option.map(List.rev, OutlineTree.trail_of(fid, term))) {
              | Some([_, parent, ..._])
                  when
                    OutlineTree.kind_of(parent, term)
                    == Some(OutlineTree.KModule) =>
                Some(parent)
              | _ => None
              };
            Restructure.move(
              ~mod_root,
              ~is_open,
              ~owner,
              ~up=op == MoveUp,
              fid,
              seg,
            );
          | _ => Restructure.apply(~mod_root, op, fid, seg)
          };
        switch (result) {
        | None => model |> Updated.return_quiet
        | Some((new_seg, focus_target)) =>
          let code = with_segment(~settings, code, new_seg);
          switch (op, focus_target, code.program) {
          | (MoveUp | MoveDown, Some(id), _) =>
            /* the cursor follows the moved item */
            outline_cursor :=
              OutlineTree.label_path(id, Program.statics(code.program).term)
          | (_, Some(id), Whole(_)) => schedule_action(FocusToggle(id))
          | (_, Some(id), Divided(d)) when Divided.owner(id, d) == None =>
            schedule_action(FocusEnsure(id))
          | _ => ()
          };
          with_code(model, code) |> Updated.return;
        };
      };
    | UnfocusDef =>
      update_view(model, (term, _, v) => SlideView.discard(~term, v))
    | ZoomIn(fid) =>
      update_view(model, (term, _, v) => SlideView.zoom_in(~term, fid, v))
    | ZoomOut => update_view(model, (_, _, v) => SlideView.zoom_out(v))
    | ZoomTo(m) => update_view(model, (_, _, v) => SlideView.zoom_to(m, v))
    | ShowWhole(parked) =>
      update_view(model, (_, _, v) => SlideView.park(parked, v))
    | RealizeView =>
      /* after undo: the restored program's statics may be compacted
         away, so the view is realized against fresh ones */
      switch (current_code(model)) {
      | None => model |> Updated.return_quiet
      | Some({program, view, _} as code) =>
        let seg = Program.document(program);
        let term =
          Program.root(program) == Haz3lcore.Sort.Mod
            ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);
        let info_map =
          settings.core.statics
            ? Haz3lcore.CachedStatics.init_compositional_term(
                ~settings=settings.core,
                ~probe_ids=Program.probe_ids(program),
                term,
              ).
                info_map
            : Haz3lcore.Id.Map.empty;
        let (view, program) =
          SlideView.realize(~info_map, ~term, view, program);
        with_code(
          model,
          {
            ...code,
            program,
            view,
          },
        )
        |> Updated.return(~historic=false);
      }
    | OutlineCursor(c) =>
      outline_cursor := c;
      outline_created := None;
      model |> Updated.return_quiet;
    | OutlineEdit(e) =>
      outline_edit := e;
      outline_created := None;
      model |> Updated.return_quiet;
    | OutlineCommit(ed, then_new) =>
      /* nothing reaches the program until here: a rename is one
         refactoring, a new definition one insertion */
      let fresh_below = (id: Haz3lcore.Id.t): OutlineEdit.t => {
        ed_row: None,
        ed_anchor: Some(id),
        ed_text: "",
        ed_caret: 0,
        ed_error: None,
      };
      let refuse = why => {
        outline_edit :=
          Some({
            ...ed,
            ed_error: Some(why),
          });
        model |> Updated.return_quiet;
      };
      switch (current_code(model)) {
      | None => model |> Updated.return_quiet
      | Some({program, _} as code) =>
        let text = String.trim(ed.ed_text);
        let seg = Program.document(program);
        let root = Program.root(program);
        let term =
          root == Haz3lcore.Sort.Mod
            ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);
        switch (ed.ed_row, ed.ed_anchor) {
        | (Some(row), _) =>
          let old =
            Option.map(
              (n: OutlineTree.node) => n.o_label,
              OutlineTree.node_of(row, term),
            );
          if (old == Some(text)) {
            outline_edit := then_new ? Some(fresh_below(row)) : None;
            model |> Updated.return_quiet;
          } else if (!settings.core.statics) {
            refuse("renaming needs statics on");
          } else {
            let statics =
              Haz3lcore.CachedStatics.init_compositional_term(
                ~settings=settings.core,
                ~probe_ids=Program.probe_ids(program),
                term,
              );
            switch (
              OutlineRename.rename(
                ~info_map=statics.info_map,
                ~term,
                row,
                text,
                seg,
              )
            ) {
            | Error(why) => refuse(why)
            | Ok(new_seg) =>
              let code = with_segment(~settings, code, new_seg);
              let new_term = Program.statics(code.program).term;
              outline_cursor := OutlineTree.label_path(row, new_term);
              outline_edit := then_new ? Some(fresh_below(row)) : None;
              with_code(model, code) |> Updated.return;
            };
          };
        | (None, Some(anchor)) when text == "" =>
          outline_edit := None;
          outline_cursor := OutlineTree.label_path(anchor, term);
          model |> Updated.return_quiet;
        | (None, Some(anchor)) =>
          let (kind, prefix) = OutlineSidebar.new_kind(text);
          let name =
            String.trim(
              String.sub(
                text,
                String.length(prefix),
                String.length(text) - String.length(prefix),
              ),
            );
          let (rkind, op): (OutlineRename.kind, OutlineSidebar.def_op) =
            switch (kind) {
            | KType => (KType, NewTypeBelow)
            | KModule => (KModule, NewModuleBelow)
            | _ => (KValue, NewBelow)
            };
          switch (OutlineRename.check_name(rkind, name)) {
          | Some(why) => refuse(why)
          | None =>
            switch (
              Restructure.apply(
                ~name,
                ~mod_root=root == Haz3lcore.Sort.Mod,
                op,
                anchor,
                seg,
              )
            ) {
            | None => refuse("a definition can't go here")
            | Some((new_seg, created)) =>
              let code = with_segment(~settings, code, new_seg);
              let new_term = Program.statics(code.program).term;
              switch (created) {
              | Some(id) =>
                outline_cursor := OutlineTree.label_path(id, new_term);
                outline_edit := then_new ? Some(fresh_below(id)) : None;
                outline_created := prefix == "" ? None : Some((id, prefix));
              | None => outline_edit := None
              };
              with_code(model, code) |> Updated.return;
            }
          };
        | (None, None) =>
          outline_edit := None;
          model |> Updated.return_quiet;
        };
      };
    | OutlineFocused =>
      /* the outline starts at the row holding the caret */
      switch (current_code(model), OutlineFollow.mark^) {
      | (Some({program, _}), Some(id)) =>
        outline_cursor :=
          OutlineTree.label_path(id, Program.statics(program).term)
      | _ => ()
      };
      model |> Updated.return_quiet;
    | FocusOutline =>
      JsUtil.focus_outline();
      model |> Updated.return_quiet;
    | StackHeader(i, a) =>
      switch (current_code(model)) {
      | Some({program: Divided(d), _} as code) =>
        switch (List.nth_opt(Divided.cells(d), i)) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          let* new_header =
            CellEditor.Update.update(~settings, a, entry.e_header);
          let d =
            d
            |> Divided.update_cell(i, e =>
                 {
                   ...e,
                   e_header: new_header,
                 }
               )
            |> Divided.set_active(i, Header);
          with_code(
            model,
            {
              ...code,
              program: Divided(d),
            },
          );
        }
      | _ => model |> Updated.return_quiet
      }
    | StackBody(i, a) =>
      switch (current_code(model)) {
      | Some({program: Divided(d), _} as code) =>
        switch (List.nth_opt(Divided.cells(d), i)) {
        | None => model |> Updated.return_quiet
        | Some(entry) =>
          let* new_body =
            CellEditor.Update.update(~settings, a, entry.e_body);
          let d =
            d
            |> Divided.update_cell(i, e =>
                 {
                   ...e,
                   e_body: new_body,
                 }
               )
            |> Divided.set_active(i, Body);
          with_code(
            model,
            {
              ...code,
              program: Divided(d),
            },
          );
        }
      | _ => model |> Updated.return_quiet
      }
    | CellAction(a) =>
      switch (current_code(model)) {
      | Some({program: Whole(editor), _} as code) =>
        let* new_ed = CellEditor.Update.update(~settings, a, editor);
        with_code(
          model,
          {
            ...code,
            program: Whole(new_ed),
          },
        );
      | Some({program: Divided(d), _} as code) =>
        /* while divided only the whole-program result takes actions */
        switch (a) {
        | ResultAction(ra) =>
          let* result =
            EvalResult.Update.update(
              ~settings={
                ...settings,
                core: {
                  ...settings.core,
                  assist: false,
                },
              },
              ra,
              Divided.result(d),
            );
          with_code(
            model,
            {
              ...code,
              program: Divided(Divided.with_result(result, d)),
            },
          );
        | MainEditor(_) => model |> Updated.return_quiet
        }
      | None => model |> return_quiet
      }
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
      /* a divided slide stays divided: its cells are part of its program */
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
      model |> Updated.return(~historic=false);
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
        Persist.rename_slide(
          is_documentation ? "doc" : "scratch",
          current.name,
          new_name,
        );
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
        Persist.forget_slide(
          is_documentation ? "doc" : "scratch",
          List.nth(model.scratchpads, model.current).name,
        );
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
                },
              );
        Updated.return(m);
      } else {
        model |> return_quiet;
      };

    | ResetCurrent =>
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
                    program: Whole(data),
                    view: SlideView.init,
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
                    program: Whole(new_data),
                    view: SlideView.init,
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
      (Language.CoreSettings.t, Language.Dynamics.Map.t, ScratchCell.t),
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
    | Code({program, agent, view}) =>
      /* restore a loaded slide's saved caret: the Move runs as its own
         follow-up action, after this calculate builds measured */
      switch (Hashtbl.find_opt(Persist.pending_caret, cur_ck)) {
      | Some(p) => schedule_action(RestoreCaret(p))
      | None => ()
      };
      switch (Hashtbl.mem(Persist.pending_pins, cur_ck), program) {
      | (true, Whole(editor))
          when
            List.exists(
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
      let statics_off = (cs: Language.CoreSettings.t) =>
        Language.CoreSettings.{
          ...cs,
          statics: false,
          dynamics: false,
        };
      let program =
        switch (program) {
        | Whole(editor) =>
          stacked_incr_cache := Haz3lcore.MakeTerm.Incr.mk_cache();
          Program.Whole(
            CellEditor.Update.calculate(
              ~settings,
              ~autoprobe_mode,
              ~is_edited,
              ~statics_mode,
              ~compositional=true,
              ~queue_worker,
              ~stitch=x => x,
              editor,
            ),
          );
        | Divided(d) =>
          /* on statics frames, compositional statics of the assembled
             document: a rename in one cell errors its users in the
             others, and cells whose item changed recapture their ctx.
             Only dirty items re-analyze. */
          let d =
            if (statics_mode == StaticsMode.Force
                || !Divided.has_fresh_statics(d)) {
              let prev_items =
                switch (Haz3lcore.DefStatics.current()) {
                | Some(p) => p.items
                | None => []
                };
              let spliced = Divided.document(d);
              let term =
                Haz3lcore.MakeTerm.Incr.go_incr(
                  ~root=Divided.root(d),
                  ~cache=stacked_incr_cache^,
                  spliced,
                ).
                  term;
              let probe_ids = Program.probe_ids(Divided(d));
              let ds =
                Haz3lcore.DefStatics.calc_auto(~settings, ~probe_ids, term);
              let statics =
                Haz3lcore.CachedStatics.{
                  term,
                  elaborated:
                    switch (Haz3lcore.DefStatics.whole_elab(ds)) {
                    | Some(elab) => elab
                    | None =>
                      Haz3lcore.CachedStatics.dh_err(
                        "Compositional elaboration gap",
                      )
                    },
                  info_map: ds.merged,
                  error_ids: Haz3lcore.DefStatics.all_error_ids(ds),
                  warning_ids: Haz3lcore.DefStatics.all_warning_ids(ds),
                  targets:
                    Haz3lcore.CachedStatics.compute_targets(
                      ~settings,
                      ~info_map=ds.merged,
                      ~probe_ids,
                    ),
                  completion: None,
                  probe_ids,
                };
              let fresh = it => !List.exists(p => p === it, prev_items);
              d
              |> Divided.with_statics(statics)
              |> Divided.map_cells((e: ScratchCell.t) =>
                   switch (
                     /* the cell may be a MODULE MEMBER: its top-level
                        item is the one whose map knows its id */
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
                         Focus.captured_ctx(
                           ~info_map=it.d_map,
                           e.e_id,
                           def_seg,
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
                   | _ => e
                   }
                 );
            } else {
              d;
            };
          /* the whole program's result keeps evaluating the assembled
             document; requests fire only when its elaboration changed */
          let d =
            Divided.with_result(
              EvalResult.Update.calculate(
                ~settings={
                  ...settings,
                  assist: false,
                },
                ~queue_worker,
                ~compute_pending=false,
                ~is_edited,
                Divided.statics(d),
                Divided.result(d),
              ),
              d,
            );
          /* whole-program samples flow into every cell (probes with
             out-of-cell call sites); the memo gates on the dynamics
             map's identity so cells re-render when new samples land */
          let extra_dyn = EvalResult.Model.dynamics(Divided.result(d));
          let calc_entry = (e: ScratchCell.t): ScratchCell.t => {
            let reuse =
              statics_mode != StaticsMode.Force
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
              /* a zoomed module's members are a Mod-rooted body */
              let body_is_exp =
                e.e_body.editor.editor.root == Haz3lcore.Sort.Exp
                || e.e_body.editor.editor.root == Haz3lcore.Sort.Mod;
              let body_is_typ =
                e.e_body.editor.editor.root == Haz3lcore.Sort.Typ;
              /* PROJECTION: on statics frames cells read their item's
                 analysis instead of re-running a private one */
              let (proj_header, proj_body) =
                statics_mode == StaticsMode.Force
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
                ScratchCell.{
                  ...e,
                  e_header:
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
          Program.Divided(Divided.map_cells(calc_entry, d));
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
      with_code(
        model,
        {
          program,
          view,
          agent,
        },
      );
    | Drv(m) =>
      let new_m =
        DerivationExerciseMode.Update.calculate(
          ~settings,
          ~autoprobe_mode,
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
      | (Cell(selection), Code({program: Whole(editor), _})) =>
        let+ a =
          CellEditor.Selection.get_cursor_info(
            ~inject=a => inject(CellAction(a)),
            ~selection,
            editor,
          );
        Update.CellAction(a);
      | (StackH(i, selection), Code(_)) =>
        switch (nth_cell(model, i)) {
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
        switch (nth_cell(model, i)) {
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
      | (Cell(_), Code({program: Divided(_), _}))
      | (Cell(_), Drv(_))
      | (StackH(_), Drv(_))
      | (StackB(_), Drv(_))
      | (Drv(_), Code(_))
      | (TextBox, _) => empty
      };
    cursor
    |> Cursor.with_actions([
         ContextualAction.of_shortcut(
           ~action=inject(FocusOutline),
           FocusOutline,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(Export),
           ExportCurrentScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(Encode),
           EncodeCurrentScratchpadInUrl,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(AddSlide),
           AddNewCodeScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(AddDrvSlide),
           AddNewDerivationScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(RenameSlide),
           RenameCurrentScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(DeleteSlide),
           DeleteCurrentScratchpad,
         ),
       ]);
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    switch (scratchpad.kind) {
    | Code({program: Whole(editor), _}) =>
      CellEditor.Selection.jump_to_tile(tile, editor)
      |> Option.map(((x, y)) => (Update.CellAction(x), Cell(y)))
    | Code({program: Divided(d), _}) =>
      /* while divided, jump inside the open cell holding the tile */
      let rec find = (i, cells: list(ScratchCell.t)) =>
        switch (cells) {
        | [] => None
        | [e, ...rest] =>
          let in_cell = cell =>
            Focus.seg_contains_id(tile, Focus.zip_of_cell(cell));
          let caret: CellEditor.Update.t =
            MainEditor(Perform(Move(Goal(TileId(tile)))));
          if (in_cell(e.e_body)) {
            Some((Update.StackBody(i, caret), StackB(i, MainEditor)));
          } else if (in_cell(e.e_header)) {
            Some((Update.StackHeader(i, caret), StackH(i, MainEditor)));
          } else {
            find(i + 1, rest);
          };
        };
      find(0, Divided.cells(d));
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
     test jumps. */
  let cross_cell_target =
      (~target_id: Haz3lcore.Id.t, ~d: Divided.t)
      : option((Update.t, t, Update.t)) => {
    Util.OptUtil.Syntax.(
      {
        let statics = Divided.statics(d);
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
        let j = Divided.position(~term=statics.term, fid, d);
        /* the target lives in the pattern (header cell) for def
           binders, in the body for everything else */
        let in_header =
          Focus.seg_contains_id(
            target_id,
            Option.value(
              Focus.find_pat(fid, Divided.document(d)),
              ~default=[],
            ),
          );
        let caret: CellEditor.Update.t =
          MainEditor(Perform(Move(Goal(TileId(target_id)))));
        Some((
          Update.FocusEnsure(fid),
          in_header ? StackH(j, MainEditor) : StackB(j, MainEditor),
          in_header
            ? Update.StackHeader(j, caret) : Update.StackBody(j, caret),
        ));
      }
    );
  };

  /* a jump (problems, inspector, agent results) to a tile outside every
     open cell: open the item holding it, then move there */
  let closed_jump =
      (tile: Haz3lcore.Id.t, model: Model.t)
      : option((Update.t, t, Update.t)) =>
    switch (Model.current_program(model)) {
    | Some(Divided(d)) when Divided.owner(tile, d) == None =>
      cross_cell_target(~target_id=tile, ~d)
    | _ => None
    };

  let stack_jump_override =
      (action: Update.t, model: Model.t): option((Update.t, t, Update.t)) => {
    Util.OptUtil.Syntax.(
      switch (action, Model.current_program(model)) {
      | (
          StackBody(
            i,
            MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
          ) |
          StackHeader(
            i,
            MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
          ),
          Some(Divided(d)),
        ) =>
        let from_header =
          switch (action) {
          | StackHeader(_) => true
          | _ => false
          };
        let* entry = List.nth_opt(Divided.cells(d), i);
        let cell =
          from_header ? entry.ScratchCell.e_header : entry.ScratchCell.e_body;
        let cell_map = cell.editor.statics.info_map;
        let* ci = Indicated.ci_of(cell.editor.editor.state.zipper, cell_map);
        let* binding_id = Language.Info.get_binding_site(ci);
        if (Id.Map.mem(binding_id, cell_map)) {
          None; /* binder is inside this cell: the cell's own jump works */
        } else {
          cross_cell_target(~target_id=binding_id, ~d);
        };
      | _ => None
      }
    );
  };

  /* the selection an outline add/ensure should land on: the body pane
     of [fid] at its (future) stack position. None for removals — the
     selection stays put. */
  let stack_add_selection = (action: Update.t, model: Model.t): option(t) =>
    switch (action, Model.current_program(model)) {
    | (FocusEnsure(fid), Some(Divided(d))) =>
      Some(
        StackB(
          Divided.position(~term=Divided.statics(d).term, fid, d),
          MainEditor,
        ),
      )
    | (FocusToggle(fid), Some(Divided(d))) =>
      List.exists((e: ScratchCell.t) => e.e_id == fid, Divided.cells(d))
        ? None
        : Some(
            StackB(
              Divided.position(~term=Divided.statics(d).term, fid, d),
              MainEditor,
            ),
          )
    | (FocusToggle(_), Some(Whole(_))) => Some(StackB(0, MainEditor))
    | _ => None
    };

  /* keep the selection on the same pane across an update. Cells are
     addressed by index, which shifts as cells open and close, so remap
     by the cell's id; a closed cell falls back to the active one, and a
     whole program selects its editor */
  let follow = (~before: Model.t, selection: t, after: Model.t): t => {
    let index = (id, cells) => {
      let rec go = (k, l: list(ScratchCell.t)) =>
        switch (l) {
        | [] => None
        | [e, ...rest] => e.e_id == id ? Some(k) : go(k + 1, rest)
        };
      go(0, cells);
    };
    let pane = ((i, side): (int, Divided.side), s) =>
      side == Divided.Header ? StackH(i, s) : StackB(i, s);
    let active = (d: Divided.t) =>
      (
        switch (Divided.active(d)) {
        | Some((id, side)) =>
          index(id, Divided.cells(d)) |> Option.map(i => (i, side))
        | None => None
        }
      )
      |> Option.value(~default=(0, Divided.Body));
    let remap = (i, side, s, d) =>
      switch (List.nth_opt(current_cells(before), i)) {
      | Some(e) =>
        switch (index(e.e_id, Divided.cells(d))) {
        | Some(j) => pane((j, side), s)
        | None => pane(active(d), CellEditor.Selection.MainEditor)
        }
      | None => pane(active(d), CellEditor.Selection.MainEditor)
      };
    switch (selection, Model.current_program(after)) {
    | (StackH(_) | StackB(_), Some(Whole(_))) => Cell(MainEditor)
    | (StackH(i, s), Some(Divided(d))) => remap(i, Divided.Header, s, d)
    | (StackB(i, s), Some(Divided(d))) => remap(i, Divided.Body, s, d)
    | (Cell(MainEditor), Some(Divided(d))) => pane(active(d), MainEditor)
    | _ => selection
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
    k_zoom_cell: bool,
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
     (whose handlers retained N-2's …): a linked list of generations
     that leaks on every edit. */
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
      | Code({program, view, _}) =>
        /* the STACK: [header band, body cell] per entry, thin rules
           between; rendered INSTEAD of the master cell */
        let stack_views = (d: Divided.t) => {
          let cells = Divided.cells(d);
          let term = Divided.statics(d).term;
          /* the zoomed module as one cell: the breadcrumb names it */
          let zoom_cell = SlideView.showing_zoom_cell(~term, view);
          let rendered =
            List.mapi(
              (i, e: ScratchCell.t) => {
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
                  k_stack_len: List.length(cells),
                  k_header_sel: header_sel,
                  k_body_sel: body_sel,
                  k_meta_down: globals.Globals.Model.meta_down,
                  k_visible_rows: globals.Globals.Model.visible_rows,
                  k_zoom_cell: zoom_cell,
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
                    switch (OutlineTree.path_of(e.e_id, term)) {
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
                    switch (List.nth_opt(cells, idx)) {
                    | Some(e) => e.ScratchCell.e_sym != None
                    | None => false
                    };
                  let pane_focus =
                      (idx, to_header, move: Haz3lcore.Action.move) =>
                    if (idx < 0 || idx >= List.length(cells)) {
                      Virtual_dom.Vdom.Effect.Ignore;
                    } else {
                      /* headerless entries have no header pane */
                      let to_header = to_header && !headerless(idx);
                      /* DOM focus must follow the selection to the new
                         pane (after render — the active-cell id moves
                         with the re-render) or the caret vanishes and
                         arrows scroll the page */
                      Haz3lcore.FocusEffect.schedule_cell();
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
                    switch (List.nth_opt(cells, idx)) {
                    | Some(e) =>
                      switch (OutlineTree.path_of(e.ScratchCell.e_id, term)) {
                      | [] => 0
                      | path => String.length(String.concat(".", path)) + 1
                      }
                    | None => 0
                    };
                  let body_last_row = idx =>
                    switch (List.nth_opt(cells, idx)) {
                    | Some(e) =>
                      max(
                        0,
                        e.ScratchCell.e_body.editor.editor.syntax.measured.
                          total_rows
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
                      i + 1 >= List.length(cells)
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
                            ~cull=false,
                            e.e_header,
                          ),
                        ],
                      )
                    };
                  let nodes =
                    (zoom_cell ? [] : [header_pane])
                    @ [
                      Virtual_dom.Vdom.Node.div(
                        ~attrs=[
                          Virtual_dom.Vdom.Attr.classes(
                            ["focus-body"] @ (zoom_cell ? ["zoom-body"] : []),
                          ),
                        ],
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
                            ~master_result=Divided.result(d),
                            ~escape=body_escape,
                            ~escape_vertical=Some(body_escape_vertical),
                            /* culling measures ONE container (dev's
                               `.cull-scope` invariant): in a focus stack only
                               the first body cell opts in; the rest render
                               unculled rather than against another cell's rows */
                            ~cull={
                              i == 0;
                            },
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
              cells,
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
                  switch (Selection.cross_cell_target(~target_id=id, ~d)) {
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
              Divided.result(d),
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
        switch (program) {
        | Divided(d) =>
          (SlideContent.get_content(current.name) |> Option.to_list)
          @ stack_views(d)
        | Whole(editor) =>
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
      NutMenu.item_group("Reset", [reset_button, reparse, reset_hazel]);

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
        /* No arrows in these modes (~nav_buttons=false above): slides are
           reached through the breadcrumb dropdowns. */
        | Previous
        | Next => Virtual_dom.Vdom.Effect.Ignore
        | Add => inject(AddSlide)
        | Rename => inject(RenameSlide)
        | Delete => inject(DeleteSlide),
      ~indicator=
        EditorModeView.indicator_select(
          ~signal=i => inject(SwitchSlide(i)),
          model.current,
          List.map(
            (s: Scratchpad.t) => SlidePath.of_string(s.name),
            model.scratchpads,
          ),
        ),
      (),
    );
  };
};
