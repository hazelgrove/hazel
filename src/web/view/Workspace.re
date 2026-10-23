open Haz3lcore;
open Util;

/* One slide's program and what it shows: every program edit (cells,
   outline, agent) and every view change goes through here. */

module Scratchpad = ScratchModel.Scratchpad;
module Focus = ScratchFocus;
module Persist = ScratchPersist;

type code = Scratchpad.code;

/* whether [term]'s outline names anything: a hydrating slide's
   placeholder statics show only an unnamed ⇒ row */
let named_outline = (term: Language.Exp.t): bool =>
  List.exists(
    (n: OutlineTree.node) => n.o_label != "",
    OutlineTree.of_term(term),
  );

/* a loaded slide's saved view, once statics can resolve it: against the
   placeholder, resolving drops pins */
let saved_view = (~slide_key, code: code): option(Persist.side) =>
  switch (Hashtbl.find_opt(Persist.pending_pins, slide_key), code.program) {
  | (Some(saved), Whole(editor))
      when named_outline(editor.editor.statics.term) =>
    Some(saved)
  | _ => None
  };

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* the whole program's editor; while divided, its result */
    | CellAction(CellEditor.Update.t)
    /* an open cell's header or body */
    | Stack(Haz3lcore.Id.t, ScratchCell.side, CellEditor.Update.t)
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

/* whether [action] gives an item's cell the focus: an outline ensure
   while divided, or a toggle that opens one (it closes an item an open
   cell holds) */
let focuses_item = (action: Action.t, program: Program.t): bool =>
  switch (action, program) {
  | (FocusEnsure(_), Divided(_)) => true
  | (FocusToggle(fid), Divided(d)) => Divided.owner(fid, d) == None
  | (FocusToggle(_), Whole(_)) => true
  | _ => false
  };

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
        (),
      ),
    completion: None,
    pins:
      Haz3lcore.CachedStatics.probe_ids_of_zipper(
        cell.editor.editor.state.zipper,
      ),
  };
};

/* a cell's item: the top-level one whose map knows its id (a module
   member's is its module's) */
let item_of =
    (ds: Haz3lcore.DefStatics.t, e: ScratchCell.t)
    : option(Haz3lcore.DefStatics.item) =>
  List.find_opt(
    (it: Haz3lcore.DefStatics.item) =>
      it.d_id == e.e_id || Haz3lcore.Id.Map.mem(e.e_id, it.d_map),
    ds.items,
  );

/* [code] showing [v]: its cells opened and closed to match */
let realize = (~info_map, ~term, v: SlideView.t, code: code): code => {
  let (view, program) = SlideView.realize(~info_map, ~term, v, code.program);
  {
    ...code,
    program,
    view,
  };
};

/* change what the slide shows, then open and close cells to match;
   view changes are not undo steps */
let view =
    (f: (Language.Exp.t, Program.t, SlideView.t) => SlideView.t, code: code)
    : Updated.t(code) => {
  let statics = Program.statics(code.program);
  realize(
    ~info_map=statics.info_map,
    ~term=statics.term,
    f(statics.term, code.program, code.view),
    code,
  )
  |> Updated.return(~historic=false);
};

/* a view change from the outline: the keys go to the cell the stack
   focuses, which a close can move. The vdom gives a closed cell's element
   to the next cell, so the DOM focus follows only through FocusEffect,
   unless the outline holds the keys */
let outline_view = (f, code: code): Updated.t(code) => {
  Haz3lcore.FocusEffect.schedule_cell();
  view(f, code);
};

/* after an edit to the whole program (agent, outline menu): the same
   view again, pins to vanished items dropped */
let resync = (code: Scratchpad.code, program: Program.t): Scratchpad.code => {
  let statics = Program.statics(program);
  realize(
    ~info_map=statics.info_map,
    ~term=statics.term,
    code.view,
    {
      ...code,
      program,
    },
  );
};

/* [program]'s term as its statics read it, now: they may lag it, or be
   compacted away by undo. A whole editor's without an unparsed buffer,
   from its syntax's pieces when they're current, with its parse cache:
   the statics path's own holds the last program any editor analyzed */
let fresh_term = (program: Program.t): Language.Exp.t => {
  let root = Program.root(program);
  switch (program) {
  | Whole(e) =>
    let z = e.editor.editor.state.zipper;
    let syntax = e.editor.editor.syntax;
    MakeTerm.semantic_term(
      ~root,
      ~shared=syntax.t_cache,
      switch (CachedSyntax.semantic_source(z, syntax)) {
      | Some(seg) => seg
      | None => MakeTerm.semantic_source(z)
      },
    );
  | Divided(d) => MakeTerm.semantic_term(~root, Divided.document(d))
  };
};
let fresh_info_map =
    (~settings: Settings.t, program: Program.t, term: Language.Exp.t)
    : Language.Statics.Map.t =>
  settings.core.statics
    ? CachedStatics.init_compositional_term(
        ~settings=settings.core,
        ~probe_ids=Program.probe_ids(program),
        term,
      ).
        info_map
    : Id.Map.empty;

/* the program after an outline edit: statics seeded now (the outline
   reads them), manual probes and the caret kept, cells re-cut; or the
   caret beside [caret], in whichever editor holds it */
let with_segment =
    (
      ~settings: Settings.t,
      ~caret: option((Direction.t, Id.t))=?,
      code: Scratchpad.code,
      new_seg: Segment.t,
    )
    : Scratchpad.code => {
  let program = code.program;
  let root = Program.root(program);
  let probes = Program.probes(program);
  /* the program's parse and measure caches: they hit for the items the
     edit didn't change */
  let fresh =
    switch (program) {
    | Whole(e) =>
      Divided.editor_of(
        ~prev=e,
        ~root,
        ~probes,
        ~anchor=
          switch (caret) {
          | Some(_) => caret
          | None => Divided.anchor_of(e.editor.editor.state.zipper)
          },
        new_seg,
      )
    | Divided(d) =>
      Divided.shell_editor_of(~probes, ~anchor=caret, new_seg, d)
    };
  let statics =
    settings.core.statics
      ? Haz3lcore.CachedStatics.init_compositional_term(
          ~settings=settings.core,
          ~probe_ids=
            CachedStatics.probe_ids_of_zipper(
              fresh.editor.editor.state.zipper,
            ),
          MakeTerm.semantic_term(
            ~root,
            ~shared=fresh.editor.editor.syntax.t_cache,
            new_seg,
          ),
        )
      : Haz3lcore.CachedStatics.empty;
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
  let code =
    resync(
      {
        ...code,
        program,
      },
      program,
    );
  switch (caret, code.program) {
  | (Some(c), Divided(d)) => {
      ...code,
      program: Divided(Divided.place_caret(c, d)),
    }
  | _ => code
  };
};

/* the program as an item edit sees it: parsed from the current
   document, which the statics may lag */
let item_ctx =
    (~settings: Settings.t, ~folds, program: Program.t): ItemEdit.ctx => {
  let term = fresh_term(program);
  {
    mod_root: Program.root(program) == Haz3lcore.Sort.Mod,
    term,
    info_map: lazy(fresh_info_map(~settings, program, term)),
    is_open: id => OutlineTree.expanded(~folds, id, term),
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
  let cell_update = (id, side, a) =>
    switch (code.program) {
    | Divided(d) =>
      switch (Divided.cell(id, d)) {
      | None => code |> return_quiet
      | Some(e) =>
        let* ed =
          CellEditor.Update.update(~settings, a, ScratchCell.pane(side, e));
        /* the pane an action reaches takes the focus */
        let d =
          d
          |> Divided.update_cell(id, ScratchCell.with_pane(side, ed))
          |> Divided.set_active(id, side);
        let d =
          switch (ed.editor.editor.state.zipper.refractors.stepping) {
          | Some(st) => Divided.keep_stepping(st.span, d)
          | None => d
          };
        {
          ...code,
          program: Divided(d),
        };
      }
    | Whole(_) => code |> return_quiet
    };
  /* the cell now holding [fid] takes the focus, on its body */
  let focusing = (fid, updated: Updated.t(code)): Updated.t(code) =>
    if (focuses_item(action, code.program)) {
      let* after = updated;
      switch (after.program) {
      | Divided(d) =>
        switch (Divided.owner(fid, d)) {
        | Some(e) => {
            ...after,
            program: Divided(Divided.set_active(e.e_id, Body, d)),
          }
        | None => after
        }
      | Whole(_) => after
      };
    } else {
      updated;
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
    outline_view(
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
                (e: ScratchCell.t) =>
                  switch (e.e_kind) {
                  | Run(ids) => List.mem(fid, ids)
                  | Def(_)
                  | Members
                  | Headless(_) => false
                  },
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
    |> focusing(fid)
  | FocusToggleRun(fid) =>
    /* the tests container: one cell for the run, or close it (or its
       members open one by one) */
    outline_view(
      (term, program, v) => {
        let (members, size) =
          switch (
            Focus.test_run_deep(
              ~root=Program.root(program),
              fid,
              Program.document(program),
            )
          ) {
          | Some((_, ms, n)) => (ms, Some(n))
          | None => ([fid], None)
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
          Some(List.length(singles)) == size
            ? v : SlideView.pin(~term, ~run=true, fid, v);
        };
      },
      code,
    )
  | FocusEnsure(fid) =>
    outline_view(
      (term, program, v) =>
        switch (program) {
        | Divided(d) when Divided.owner(fid, d) == None =>
          SlideView.pin(~term, fid, v)
        | _ => v
        },
      code,
    )
    |> focusing(fid)
  | UnfocusDef =>
    outline_view((term, _, v) => SlideView.discard(~term, v), code)
  | ZoomIn(fid) =>
    outline_view((term, _, v) => SlideView.zoom_in(~term, fid, v), code)
  | ZoomOut => outline_view((_, _, v) => SlideView.zoom_out(v), code)
  | ZoomTo(m) => outline_view((_, _, v) => SlideView.zoom_to(m, v), code)
  | ShowWhole(parked) =>
    outline_view((_, _, v) => SlideView.park(parked, v), code)
  | RealizeView =>
    /* after undo: against the statics the restored program shows (the
       live program's, Page.carry_over), as view changes read them while
       typing; a snapshot nothing was carried into has none */
    let statics = Program.statics(code.program);
    let (term, info_map) =
      Id.Map.is_empty(statics.info_map)
        ? {
          let term = fresh_term(code.program);
          (term, fresh_info_map(~settings, code.program, term));
        }
        : (statics.term, statics.info_map);
    realize(~info_map, ~term, code.view, code) |> return(~historic=false);
  | RestorePins =>
    /* each slide's saved view waits under its own key, so another
       slide hydrating first can't drop it */
    switch (saved_view(~slide_key, code)) {
    | Some(saved) =>
      Hashtbl.remove(Persist.pending_pins, slide_key);
      view((term, _, _) => Persist.resolve_view(saved, term), code);
    | None => code |> return_quiet /* statics not ready: retry */
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
  | Stack(id, side, a) => cell_update(id, side, a)
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
    (
      Language.CoreSettings.t,
      Language.Dynamics.Map.t,
      (bool, int),
      ScratchCell.t,
    ),
  ) =
  Hashtbl.create(8);

let calculate =
    (
      ~settings,
      ~autoprobe_mode,
      /* the ⇓ toggle: the whole program's last expression, or the ⇒
         cell's, probed as an open drawer */
      ~tail_probe,
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
  if (Option.is_some(saved_view(~slide_key, code))) {
    schedule_action(RestorePins);
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
      Hashtbl.reset(calc_entry_memo);
      Program.Whole(
        CellEditor.Update.calculate(
          ~settings,
          ~autoprobe_mode,
          ~tail_probe,
          ~proofs=Theorems,
          ~is_edited,
          ~statics_mode,
          ~compositional=true,
          ~show_value=false,
          ~queue_worker,
          ~stitch=x => x,
          editor,
        ),
      );
    | Divided(d) =>
      /* on statics frames, compositional statics of the assembled
         document, re-analyzing only dirty items: a rename in one cell
         errors its users in others; changed items' cells recapture ctx */
      /* the ⇒ cell's tail probe anchors its expression's current root
         (its key, e_id, follows only on view changes): counted before
         the cell calculates, so this frame's run samples it */
      let is_tail_cell = (e: ScratchCell.t) =>
        tail_probe && e.e_kind == Headless({js|⇒|js});
      let tail_root = (e: ScratchCell.t) =>
        switch (Focus.zip_of_cell(e.e_body)) {
        | [] => None
        | seg =>
          try(Some(Segment.root_id(Segment.skel(seg), seg))) {
          | _ => None
          }
        };
      let probe_ids =
        List.fold_left(
          (acc, e: ScratchCell.t) =>
            switch (is_tail_cell(e) ? tail_root(e) : None) {
            | Some(id) => Id.Map.add(id, (), acc)
            | None => acc
            },
          Program.probe_ids(Divided(d)),
          Divided.cells(d),
        );
      let (d, ds) =
        if (statics_mode == StaticsMode.Force
            || !Divided.has_fresh_statics(d)
            || !Id.Map.equal((==), probe_ids, Divided.statics(d).pins)) {
          let spliced = Divided.document(d);
          /* read as a whole editor's statics read it: an open form in a
             cell takes none of the items after it */
          let term =
            Haz3lcore.MakeTerm.semantic_term(~root=Divided.root(d), spliced);
          let prev_items =
            switch (Haz3lcore.DefStatics.cached(term)) {
            | Some(p) => p.items
            | None => []
            };
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
          let statics = {
            ...
              Haz3lcore.CachedStatics.of_def_statics(
                ~settings,
                ~probe_ids,
                /* worker-resident dynamics: the sentinel keeps the
                   eval-request cadence (see CachedStatics) */
                ~elaborated=?
                  clamped
                    ? Some(Haz3lcore.CachedStatics.resident_sentinel()) : None,
                term,
                ds,
              ),
            /* the cells' own pins (probe_ids adds the projectors') */
            pins: probe_ids,
          };
          let fresh = it => !List.exists(p => p === it, prev_items);
          (
            d
            |> Divided.with_statics(statics)
            |> Divided.map_cells((e: ScratchCell.t) =>
                 switch (item_of(ds, e)) {
                 | Some(it) when fresh(it) =>
                   switch (
                     Focus.cell_content(~root=Divided.root(d), e, spliced)
                   ) {
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
      /* a stepping drawer's probe keeps its function values, in the
         program's run as in a whole editor's */
      let stepping = Divided.stepping(d);
      let d = {
        let st = Divided.statics(d);
        let full = id =>
          switch (stepping) {
          | Some(s) => s.span.probe_id == id
          | None => false
          };
        Id.Map.exists(
          (id, spec: Language.Sample.capture_spec) => spec.full != full(id),
          st.targets,
        )
          ? Divided.with_statics(
              {
                ...st,
                targets:
                  Id.Map.mapi(
                    (id, spec: Language.Sample.capture_spec) =>
                      {
                        ...spec,
                        full: full(id),
                      },
                    st.targets,
                  ),
              },
              d,
            )
          : d;
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
            ~show_value=false,
            ~is_edited,
            ~stepping=
              Option.map(
                (st: Haz3lcore.ProjectorBase.stepping) => st.span,
                stepping,
              ),
            Divided.statics(d),
            Divided.result(d),
          ),
          d,
        );
      /* the proofs' drawers take the rows their steppers need; a change
         re-lays out the cells (the memo keys on the layout) */
      let _: bool =
        Theorems.fit(
          ~settings,
          (Divided.result(d): EvalResult.Model.t).theorems,
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
      /* a theorem's cell holds its statement: its proof goes under it */
      let program_theorems =
        lazy(
          Haz3lcore.AutoProbePerform.theorems_of_exp(Divided.statics(d).term)
        );
      let calc_entry = (e: ScratchCell.t): ScratchCell.t => {
        /* the stepping drawer's rows follow the program's stepper */
        let fit = c =>
          fst(CellEditor.Update.fit_steps(~settings, Divided.result(d), c));
        let (h, b) = (fit(e.e_header), fit(e.e_body));
        /* identity kept when nothing moved: the memo below keys on it */
        let e =
          h === e.e_header && b === e.e_body
            ? e
            : {
              ...e,
              e_header: h,
              e_body: b,
            };
        let reuse =
          statics_mode != StaticsMode.Force
            ? switch (Hashtbl.find_opt(calc_entry_memo, e.e_id)) {
              | Some((s', d', t', prev))
                  when
                    prev === e
                    && s' === settings
                    && d' === extra_dyn
                    && t' == (is_tail_cell(e), DrawerFit.layout^) =>
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
          /* on statics frames (which always analyze, so [ds] is there)
             cells read their item's analysis instead of re-running a
             private one */
          let item =
            statics_mode == StaticsMode.Force
              ? Option.bind(ds, ds => item_of(ds, e)) : None;
          let project = (c: CellEditor.Model.t) =>
            Option.map(
              item =>
                project_cell_statics(
                  ~settings,
                  ~item,
                  ~engine_warnings=Lazy.force(frame_warns),
                  c,
                ),
              item,
            );
          let (proj_header, proj_body) = (
            project(e.e_header),
            project(e.e_body),
          );
          /* dynamics off: a cell shows the whole program's samples, and
             its own run would be on the main thread with no step limit */
          let body_settings =
            body_is_exp || body_is_typ || proj_body != None
              ? Language.CoreSettings.{
                  ...settings,
                  dynamics: false,
                }
              : statics_off(settings);
          /* a module binder: its header's private pat statics would misread
             it as a constructor, so it takes only its item's projected ones */
          let binds_module =
            switch (e.e_kind) {
            | Def({binds_module}) => binds_module
            | Members => true
            | Headless(_)
            | Run(_) => false
            };
          let e' =
            ScratchCell.{
              ...e,
              e_header:
                CellEditor.Update.calculate(
                  ~settings=
                    binds_module && proj_header == None
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
                  ~tail_probe=is_tail_cell(e),
                  ~proofs=
                    List.mem(e.e_id, Lazy.force(program_theorems))
                      ? ProofOf(e.e_id) : Theorems,
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
            (settings, extra_dyn, (is_tail_cell(e), DrawerFit.layout^), e'),
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
