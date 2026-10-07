open Haz3lcore;
open Util;

/* One slide's program and what it shows: every program edit (cells,
   outline, agent) and every view change goes through here. */

module Scratchpad = ScratchModel.Scratchpad;
module Focus = ScratchFocus;
module Persist = ScratchPersist;

type code = Scratchpad.code;

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* the whole program's editor; while divided, its result */
    | CellAction(CellEditor.Update.t)
    | StackHeader(Haz3lcore.Id.t, CellEditor.Update.t)
    | StackBody(Haz3lcore.Id.t, CellEditor.Update.t)
    /* open or close an item's cell */
    | FocusToggle(Haz3lcore.Id.t)
    /* one cell for a whole test run */
    | FocusToggleRun(Haz3lcore.Id.t)
    /* open an item unless an open cell holds it (cross-cell jumps) */
    | FocusEnsure(Haz3lcore.Id.t)
    /* drop the pins shown at this level */
    | UnfocusDef
    | ZoomIn(Haz3lcore.Id.t)
    | ZoomOut
    /* a breadcrumb: None = the program */
    | ZoomTo(option(Haz3lcore.Id.t))
    /* park (true) or unpark the pins at this level */
    | ShowWhole(bool)
    /* open and close cells to match the view (after undo) */
    | RealizeView
    /* a loaded slide's saved view and caret, restored once ready */
    | RestorePins
    | RestoreCaret(Point.t)
    | AgentAction(Agent.Update.Action.t);
};

open Action;

/* a cell's statics: its DefStatics item (analyzed in the program's
   context) scoped to the cell's ids, so id-keyed consumers never see
   foreign ones; unused-binder warnings come from across items */
let project_cell_statics =
    (
      ~settings: Language.CoreSettings.t,
      ~item: Haz3lcore.DefStatics.item,
      ~engine_warnings: list(Haz3lcore.Id.t),
      cell: CellEditor.Model.t,
    )
    : Haz3lcore.CachedStatics.t => {
  let term_data = cell.editor.editor.syntax.term_data;
  let in_cell = id => Haz3lcore.Id.Map.mem(id, term_data);
  let info_map =
    Haz3lcore.Id.Map.filter((id, _) => in_cell(id), item.d_map);
  Haz3lcore.CachedStatics.{
    term: item.d_node,
    elaborated: item.d_elab,
    info_map,
    error_ids: List.filter(in_cell, item.d_error_ids),
    warning_ids: List.filter(in_cell, item.d_warning_ids @ engine_warnings),
    /* the cell's own probes: empty targets read as a probe change and
       swapped in a private analysis */
    targets:
      Haz3lcore.CachedStatics.compute_targets(
        ~settings,
        ~info_map,
        ~probe_ids=
          Haz3lcore.CachedStatics.probe_ids_of_zipper(
            cell.editor.editor.state.zipper,
          ),
      ),
    completion: None,
    pins:
      Haz3lcore.CachedStatics.probe_ids_of_zipper(
        cell.editor.editor.state.zipper,
      ),
  };
};
/* incremental-parse cache for a divided program's statics frames:
   only the edited item re-parses */
let stacked_incr_cache: ref(Haz3lcore.MakeTerm.Incr.cache) =
  ref(Haz3lcore.MakeTerm.Incr.mk_cache());

/* change what the slide shows, then open and close cells to match;
   view changes are not undo steps */
let view =
    (f: (Language.Exp.t, Program.t, SlideView.t) => SlideView.t, code: code)
    : Updated.t(code) => {
  let statics = Program.statics(code.program);
  let (view, program) =
    SlideView.realize(
      ~info_map=statics.info_map,
      ~term=statics.term,
      f(statics.term, code.program, code.view),
      code.program,
    );
  {
    ...code,
    program,
    view,
  }
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
   reads them), manual probes and the caret kept, cells re-cut */
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

/* the program as an item edit sees it: parsed from the current
   document, which the statics may lag */
let item_ctx =
    (~settings: Settings.t, ~collapsed, program: Program.t): ItemEdit.ctx => {
  let seg = Program.document(program);
  let mod_root = Program.root(program) == Haz3lcore.Sort.Mod;
  let term =
    mod_root ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);
  {
    mod_root,
    term,
    info_map:
      lazy(
        settings.core.statics
          ? Haz3lcore.CachedStatics.init_compositional_term(
              ~settings=settings.core,
              ~probe_ids=Program.probe_ids(program),
              term,
            ).
              info_map
          : Haz3lcore.Id.Map.empty
      ),
    is_open: id =>
      switch (OutlineTree.label_path(id, term)) {
      | Some(path) => !List.mem(path, collapsed)
      | None => true
      },
  };
};

let update =
    (
      ~settings: Settings.t,
      ~schedule_action: Action.t => unit,
      ~slide_key: string,
      action: Action.t,
      code: code,
    )
    : Updated.t(code) => {
  open Updated;
  let cell_update = (id, side: Divided.side, a) =>
    switch (code.program) {
    | Divided(d) =>
      switch (
        List.find_opt((e: ScratchCell.t) => e.e_id == id, Divided.cells(d))
      ) {
      | None => code |> return_quiet
      | Some(e) =>
        let* ed =
          CellEditor.Update.update(
            ~settings,
            a,
            side == Header ? e.e_header : e.e_body,
          );
        let d =
          d
          |> Divided.update_cell(id, e =>
               side == Header
                 ? {
                   ...e,
                   e_header: ed,
                 }
                 : {
                   ...e,
                   e_body: ed,
                 }
             )
          |> Divided.set_active(id, side);
        {
          ...code,
          program: Divided(d),
        };
      }
    | Whole(_) => code |> return_quiet
    };
  switch (action) {
  | AgentAction(a) =>
    /* the agent reads and edits the whole program: a divided one is joined
       for it, then re-divided with the same cells if it changed anything */
    let editor = Program.whole_memo(code.program);
    let (agent, updated_editor) =
      Agent.Update.update(a, code.agent, editor, settings, a =>
        schedule_action(AgentAction(a))
      );
    let* new_ed = updated_editor;
    let code = {
      ...code,
      agent,
    };
    switch (code.program) {
    | Whole(_) => {
        ...code,
        program: Whole(new_ed),
      }
    | Divided(_) when new_ed === editor => code
    | Divided(d) =>
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
      )
    };
  | FocusToggle(fid) =>
    view(
      (term, program, v) =>
        if (List.mem(
              SlideView.{
                p_id: fid,
                p_run: false,
              },
              v.pins,
            )) {
          SlideView.unpin(fid, v);
        } else {
          /* inside an open run cell, the toggle closes the run */
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
        },
      code,
    )
  | FocusToggleRun(fid) =>
    /* the tests container: one cell for the run, or close it (or its
       members open one by one) */
    view(
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
          Some(List.length(singles))
          == Focus.test_run_size_deep(fid, Program.document(program))
            ? v : SlideView.pin(~term, ~run=true, fid, v);
        };
      },
      code,
    )
  | FocusEnsure(fid) =>
    view(
      (term, program, v) =>
        switch (program) {
        | Divided(d) when Divided.owner(fid, d) == None =>
          SlideView.pin(~term, fid, v)
        | _ => v
        },
      code,
    )
  | UnfocusDef => view((term, _, v) => SlideView.discard(~term, v), code)
  | ZoomIn(fid) =>
    view((term, _, v) => SlideView.zoom_in(~term, fid, v), code)
  | ZoomOut => view((_, _, v) => SlideView.zoom_out(v), code)
  | ZoomTo(m) => view((_, _, v) => SlideView.zoom_to(m, v), code)
  | ShowWhole(parked) => view((_, _, v) => SlideView.park(parked, v), code)
  | RealizeView =>
    /* after undo: the restored program's statics may be compacted
       away, so the view is realized against fresh ones */
    let seg = Program.document(code.program);
    let term =
      Program.root(code.program) == Sort.Mod
        ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);
    let info_map =
      settings.core.statics
        ? CachedStatics.init_compositional_term(
            ~settings=settings.core,
            ~probe_ids=Program.probe_ids(code.program),
            term,
          ).
            info_map
        : Id.Map.empty;
    let (view, program) =
      SlideView.realize(~info_map, ~term, code.view, code.program);
    {
      ...code,
      program,
      view,
    }
    |> return(~historic=false);
  | RestorePins =>
    /* each slide's saved view waits under its own key, so another
       slide hydrating first can't drop it */
    switch (Hashtbl.find_opt(Persist.pending_pins, slide_key), code.program) {
    | (Some(saved), Whole(editor))
        when
          List.exists(
            (n: OutlineTree.node) => n.o_label != "",
            OutlineTree.of_term(editor.editor.statics.term),
          ) =>
      Hashtbl.remove(Persist.pending_pins, slide_key);
      view((term, _, _) => Persist.resolve_view(saved, term), code);
    | _ => code |> return_quiet /* statics not ready: retry */
    }
  | RestoreCaret(p) =>
    /* cleared here, not when scheduled: boot's calculate has a no-op
       scheduler, so the entry must survive until a real loop runs it */
    Hashtbl.remove(Persist.pending_caret, slide_key);
    switch (code.program) {
    | Whole(editor) =>
      let* editor =
        CellEditor.Update.update(
          ~settings,
          MainEditor(Perform(Move(Point(p, None)))),
          editor,
        );
      {
        ...code,
        program: Whole(editor),
      };
    | Divided(_) => code |> return_quiet
    };
  | StackHeader(id, a) => cell_update(id, Header, a)
  | StackBody(id, a) => cell_update(id, Body, a)
  | CellAction(a) =>
    switch (code.program) {
    | Whole(editor) =>
      let* editor = CellEditor.Update.update(~settings, a, editor);
      {
        ...code,
        program: Whole(editor),
      };
    | Divided(d) =>
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
        {
          ...code,
          program: Divided(Divided.with_result(result, d)),
        };
      | MainEditor(_) => code |> return_quiet
      }
    }
  };
};

/* per-cell calculate memo: a cell physically equal to the last output
   is already calculated (update replaces only edited cells), and reuse
   keeps the identity the view cache keys on */
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
      ~schedule_action: Action.t => unit,
      ~is_edited,
      ~statics_mode,
      ~slide_key: string,
      code: code,
    )
    : code => {
  let program = code.program;
  /* restore a loaded slide's saved caret: the Move runs as its own
     follow-up action, after this calculate builds measured */
  switch (Hashtbl.find_opt(Persist.pending_caret, slide_key)) {
  | Some(p) => schedule_action(RestoreCaret(p))
  | None => ()
  };
  switch (Hashtbl.mem(Persist.pending_pins, slide_key), program) {
  | (true, Whole(editor))
      when
        List.exists(
          (n: OutlineTree.node) => n.o_label != "",
          OutlineTree.of_term(editor.editor.statics.term),
        ) =>
    /* only once statics has a named outline: early hydration frames see
       a placeholder (a lone unnamed ⇒ row), where resolving drops pins */
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
      Hashtbl.reset(calc_entry_memo);
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
         document, re-analyzing only dirty items: a rename in one cell
         errors its users in others; changed items' cells recapture ctx */
      let (d, ds) =
        if (statics_mode == StaticsMode.Force || !Divided.has_fresh_statics(d)) {
          let spliced = Divided.document(d);
          let term =
            Haz3lcore.MakeTerm.Incr.go_incr(
              ~root=Divided.root(d),
              ~cache=stacked_incr_cache^,
              spliced,
            ).
              term;
          let prev_items =
            switch (Haz3lcore.DefStatics.cached(term)) {
            | Some(p) => p.items
            | None => []
            };
          let probe_ids = Program.probe_ids(Divided(d));
          let clamped = Haz3lcore.DefStatics.clamp^;
          let ds =
            Haz3lcore.DefStatics.calc_auto(
              ~settings,
              ~propagate=!clamped,
              ~probe_ids,
              term,
            );
          /* W2 divided-mode sync: ship the assembled document (this is
             the coherent segment/statics moment while divided; the
             whole-program case ships before the eval batch below) */
          switch (Divided.root(d)) {
          | Exp
          | Mod =>
            ShadowResidency.on_master_statics(
              ~key=ShadowResidency.master_key,
              ~root=Divided.root(d),
              ~settings,
              spliced,
              ds,
            )
          | _ => ()
          };
          let statics =
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
              completion: None,
              /* the cells' own pins (probe_ids adds the projectors') */
              pins: Program.probe_ids(Divided(d)),
            };
          let fresh = it => !List.exists(p => p === it, prev_items);
          (
            d
            |> Divided.with_statics(statics)
            |> Divided.map_cells((e: ScratchCell.t) =>
                 switch (
                   /* the cell may be a module member: its top-level
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
                 }
               ),
            Some(ds),
          );
        } else {
          (d, None);
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
      /* unused-binder warnings span items: once per frame, not per cell */
      let frame_warns =
        lazy(
          Option.map(Haz3lcore.DefStatics.all_warning_ids, ds)
          |> Option.value(~default=[])
        );
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
          let body_is_typ = e.e_body.editor.editor.root == Haz3lcore.Sort.Typ;
          /* on statics frames cells read their item's
             analysis instead of re-running a private one */
          let (proj_header, proj_body) =
            statics_mode == StaticsMode.Force
              ? {
                switch (ds) {
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
                    let warns = Lazy.force(frame_warns);
                    (
                      Some(
                        project_cell_statics(
                          ~settings,
                          ~item=it,
                          ~engine_warnings=warns,
                          e.e_header,
                        ),
                      ),
                      Some(
                        project_cell_statics(
                          ~settings,
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
          /* dynamics off: a cell shows the whole program's samples, and
             its own run would be on the main thread with no step limit */
          let body_settings =
            body_is_exp || body_is_typ || proj_body != None
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
      let d = Divided.map_cells(calc_entry, d);
      /* entries only for the cells still open */
      let open_ids =
        List.map((e: ScratchCell.t) => e.e_id, Divided.cells(d));
      Hashtbl.filter_map_inplace(
        (id, entry) => List.mem(id, open_ids) ? Some(entry) : None,
        calc_entry_memo,
      );
      Program.Divided(d);
    };
  /* W2 whole-program sync: MUST ship before the eval batch posts — a
     Resident eval references the worker's resident program, and
     postMessage order is the only thing keeping it current (the divided
     case ships at its statics site above) */
  switch (program) {
  | Whole(editor) =>
    switch (editor.editor.editor.root) {
    | Exp
    | Mod =>
      switch (Haz3lcore.DefStatics.cached(editor.editor.statics.term)) {
      | Some(ds) =>
        ShadowResidency.on_master_statics(
          ~key=ShadowResidency.master_key,
          ~root=editor.editor.editor.root,
          ~settings,
          editor.editor.editor.syntax.segment,
          ds,
        )
      | None => ()
      }
    | _ => ()
    }
  | Divided(_) => ()
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
  {
    ...code,
    program,
  };
};
