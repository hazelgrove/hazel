open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Focus = ScratchFocus;
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

/* the key the current slide's saved state waits under */
let slide_key = (~is_documentation, model: Model.t): string =>
  Persist.content_key(
    is_documentation ? "doc" : "scratch",
    List.nth(model.scratchpads, model.current).name,
  );

/* the current slide's open cells (none when it is whole) */
let current_cells = (model: Model.t): list(ScratchCell.t) =>
  switch (Model.current_program(model)) {
  | Some(Divided(d)) => Divided.cells(d)
  | _ => []
  };
let cell_by_id = (model: Model.t, id: Haz3lcore.Id.t): option(ScratchCell.t) =>
  List.find_opt((e: ScratchCell.t) => e.e_id == id, current_cells(model));

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Workspace(Workspace.Action.t)
    | OutlineCollapse(OutlineTree.path) /* toggle a branch's collapse */
    | OutlineMenu(option((Haz3lcore.Id.t, bool, float, float)))
    | OutlineDefOp(OutlineSidebar.def_op, Haz3lcore.Id.t)
    | OutlineCursor(option(OutlineTree.path))
    | OutlineEdit(option(OutlineEdit.t))
    | OutlineCommit(OutlineEdit.t, bool) /* true: then a new one */
    | OutlineFocused /* the outline took keyboard focus */
    | FocusOutline
    | RefreshStatics
    | DrvAction(DerivationExerciseMode.Update.t)
    | Deck(SlideDeck.Action.t);

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

  let update =
      (
        ~schedule_action,
        ~settings: Settings.t,
        ~is_documentation: bool,
        action: t,
        model: Model.t,
      ) => {
    switch (action) {
    | Workspace(a) =>
      switch (current_code(model)) {
      | None => model |> return_quiet
      | Some(code) =>
        let* code =
          Workspace.update(
            ~settings,
            ~schedule_action=a => schedule_action(Workspace(a)),
            ~slide_key=slide_key(~is_documentation, model),
            a,
            code,
          );
        with_code(model, code);
      }
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
        let collapsed =
          collapse_paths(
            is_documentation ? "doc" : "scratch",
            List.nth(model.scratchpads, model.current).name,
          );
        switch (
          ItemEdit.edit(
            Workspace.item_ctx(~settings, ~collapsed, program),
            Op(op, fid),
            Program.document(program),
          )
        ) {
        | Error(_) => model |> Updated.return_quiet
        | Ok((new_seg, focus_target)) =>
          let code = Workspace.with_segment(~settings, code, new_seg);
          switch (op, focus_target, code.program) {
          | (MoveUp | MoveDown, Some(id), _) =>
            /* the cursor follows the moved item */
            outline_cursor :=
              OutlineTree.label_path(id, Program.statics(code.program).term)
          | (_, Some(id), Whole(_)) =>
            schedule_action(Workspace(FocusToggle(id)))
          | (_, Some(id), Divided(d)) when Divided.owner(id, d) == None =>
            schedule_action(Workspace(FocusEnsure(id)))
          | _ => ()
          };
          with_code(model, code) |> Updated.return;
        };
      };
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
        let ctx = Workspace.item_ctx(~settings, ~collapsed=[], program);
        let term = ctx.term;
        let edit = e => ItemEdit.edit(ctx, e, Program.document(program));
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
            switch (edit(Rename(row, text))) {
            | Error(why) => refuse(why)
            | Ok((new_seg, _)) =>
              let code = Workspace.with_segment(~settings, code, new_seg);
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
          switch (edit(Insert(anchor, text))) {
          | Error(why) => refuse(why)
          | Ok((new_seg, created)) =>
            let code = Workspace.with_segment(~settings, code, new_seg);
            let new_term = Program.statics(code.program).term;
            let prefix = snd(OutlineSidebar.new_kind(text));
            switch (created) {
            | Some(id) =>
              outline_cursor := OutlineTree.label_path(id, new_term);
              outline_edit := then_new ? Some(fresh_below(id)) : None;
              outline_created := prefix == "" ? None : Some((id, prefix));
            | None => outline_edit := None
            };
            with_code(model, code) |> Updated.return;
          }
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
    | Deck(a) =>
      SlideDeck.update(
        ~settings,
        ~schedule_action=a => schedule_action(Deck(a)),
        ~is_documentation,
        a,
        model,
      )
    };
  };

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
    switch (scratchpad.kind) {
    | Code(code) =>
      with_code(
        model,
        Workspace.calculate(
          ~settings,
          ~autoprobe_mode,
          ~schedule_action=a => schedule_action(Workspace(a)),
          ~is_edited,
          ~statics_mode,
          ~slide_key=slide_key(~is_documentation, model),
          code,
        ),
      )
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
    | StackH(Haz3lcore.Id.t, CellEditor.Selection.t)
    | StackB(Haz3lcore.Id.t, CellEditor.Selection.t)
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
            ~inject=a => inject(Workspace(CellAction(a))),
            ~selection,
            editor,
          );
        Update.Workspace(CellAction(a));
      | (StackH(i, selection), Code(_)) =>
        switch (cell_by_id(model, i)) {
        | Some(entry) =>
          let+ a =
            CellEditor.Selection.get_cursor_info(
              ~inject=a => inject(Workspace(StackHeader(i, a))),
              ~selection,
              entry.e_header,
            );
          Update.Workspace(StackHeader(i, a));
        | None => empty
        }
      | (StackB(i, selection), Code(_)) =>
        switch (cell_by_id(model, i)) {
        | Some(entry) =>
          let+ a =
            CellEditor.Selection.get_cursor_info(
              ~inject=a => inject(Workspace(StackBody(i, a))),
              ~selection,
              entry.e_body,
            );
          Update.Workspace(StackBody(i, a));
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
           ~action=inject(Deck(Export)),
           ExportCurrentScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(Deck(Encode)),
           EncodeCurrentScratchpadInUrl,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(Deck(AddSlide)),
           AddNewCodeScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(Deck(AddDrvSlide)),
           AddNewDerivationScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(Deck(RenameSlide)),
           RenameCurrentScratchpad,
         ),
         ContextualAction.of_shortcut(
           ~action=inject(Deck(DeleteSlide)),
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
      |> Option.map(((x, y)) =>
           (Update.Workspace(CellAction(x)), Cell(y))
         )
    | Code({program: Divided(d), _}) =>
      /* while divided, jump inside the open cell holding the tile */
      let caret: CellEditor.Update.t =
        MainEditor(Perform(Move(Goal(TileId(tile)))));
      let in_cell = cell =>
        Focus.seg_contains_id(tile, Focus.zip_of_cell(cell));
      Divided.cells(d)
      |> List.find_map((e: ScratchCell.t) =>
           if (in_cell(e.e_body)) {
             Some((
               Update.Workspace(StackBody(e.e_id, caret)),
               StackB(e.e_id, MainEditor),
             ));
           } else if (in_cell(e.e_header)) {
             Some((
               Update.Workspace(StackHeader(e.e_id, caret)),
               StackH(e.e_id, MainEditor),
             ));
           } else {
             None;
           }
         );
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
        /* the cell that holds [fid] once it's ensured */
        let j =
          switch (Divided.owner(fid, d)) {
          | Some(e) => e.e_id
          | None => fid
          };
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
          Update.Workspace(FocusEnsure(fid)),
          in_header ? StackH(j, MainEditor) : StackB(j, MainEditor),
          in_header
            ? Update.Workspace(StackHeader(j, caret))
            : Update.Workspace(StackBody(j, caret)),
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
          Workspace(
            StackBody(
              i,
              MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
            ),
          ) |
          Workspace(
            StackHeader(
              i,
              MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
            ),
          ),
          Some(Divided(d)),
        ) =>
        let from_header =
          switch (action) {
          | Workspace(StackHeader(_)) => true
          | _ => false
          };
        let* entry = cell_by_id(model, i);
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
     of the cell that will hold [fid]. None for removals: the selection
     stays put. */
  let stack_add_selection = (action: Update.t, model: Model.t): option(t) =>
    switch (action, Model.current_program(model)) {
    | (Workspace(FocusEnsure(fid)), Some(Divided(d))) =>
      Some(
        StackB(
          switch (Divided.owner(fid, d)) {
          | Some(e) => e.e_id
          | None => fid
          },
          MainEditor,
        ),
      )
    | (Workspace(FocusToggle(fid)), Some(Divided(d))) =>
      List.exists((e: ScratchCell.t) => e.e_id == fid, Divided.cells(d))
        ? None : Some(StackB(fid, MainEditor))
    | (Workspace(FocusToggle(fid)), Some(Whole(_))) =>
      Some(StackB(fid, MainEditor))
    | _ => None
    };

  /* after an update: a selected cell that closed falls back to the
     active one, and a whole program selects its editor */
  let follow = (selection: t, after: Model.t): t => {
    let pane = ((id, side): (Haz3lcore.Id.t, Divided.side), s) =>
      side == Divided.Header ? StackH(id, s) : StackB(id, s);
    let active = (d: Divided.t) =>
      switch (Divided.active(d), Divided.cells(d)) {
      | (Some(a), _) => a
      | (None, [e, ..._]) => (e.e_id, Divided.Body)
      | (None, []) => (Haz3lcore.Id.invalid, Divided.Body)
      };
    let open_ = (id, d) =>
      List.exists((e: ScratchCell.t) => e.e_id == id, Divided.cells(d));
    switch (selection, Model.current_program(after)) {
    | (StackH(_) | StackB(_), Some(Whole(_))) => Cell(MainEditor)
    | (StackH(id, _) | StackB(id, _), Some(Divided(d))) =>
      open_(id, d) ? selection : pane(active(d), MainEditor)
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
                  | Some(Selection.StackH(j, sel)) when j == e.e_id =>
                    Some(sel)
                  | _ => None
                  };
                let body_sel =
                  switch (selected) {
                  | Some(Selection.StackB(j, sel)) when j == e.e_id =>
                    Some(sel)
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
                      let id = List.nth(cells, idx).e_id;
                      Virtual_dom.Vdom.Effect.Many([
                        signal(
                          MakeActive(
                            to_header
                              ? StackH(id, MainEditor)
                              : StackB(id, MainEditor),
                          ),
                        ),
                        inject(
                          to_header
                            ? Workspace(
                                StackHeader(
                                  id,
                                  MainEditor(Perform(Move(move))),
                                ),
                              )
                            : Workspace(
                                StackBody(
                                  id,
                                  MainEditor(Perform(Move(move))),
                                ),
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
                        ? Workspace(
                            StackHeader(
                              e.e_id,
                              MainEditor(
                                Perform(Move(Vertical(v, ByChar))),
                              ),
                            ),
                          )
                        : Workspace(
                            StackBody(
                              e.e_id,
                              MainEditor(
                                Perform(Move(Vertical(v, ByChar))),
                              ),
                            ),
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
                                signal(MakeActive(StackH(e.e_id, sel))),
                            ~inject=
                              a =>
                                inject(Workspace(StackHeader(e.e_id, a))),
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
                                signal(MakeActive(StackB(e.e_id, sel))),
                            ~inject=
                              a => inject(Workspace(StackBody(e.e_id, a))),
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
                        Workspace(
                          CellAction(
                            MainEditor(Perform(Move(Goal(TileId(id))))),
                          ),
                        ),
                      ),
                    ])
                  },
              ~inject=a => inject(Workspace(CellAction(ResultAction(a)))),
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
              ~inject=a => inject(Workspace(CellAction(a))),
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
        _ => inject(Deck(Export)),
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
        _ => inject(Deck(Encode)),
        ~tooltip="Encode Scratchpad in URL",
      );

    let import_button =
      Widgets.file_select_button_named(
        "import-scratchpad",
        Icons.import,
        file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => inject(Deck(InitImportScratchpad(file)))
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
            inject(Deck(ResetCurrent));
          } else {
            Virtual_dom.Vdom.Effect.Ignore;
          };
        },
        ~tooltip="Reset Editor",
      );

    let reparse =
      Widgets.button_named(
        Icons.backpack,
        _ => inject(Workspace(CellAction(MainEditor(Perform(Reparse))))),
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
      inject(Deck(AddDrvSlide))
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
        | Add => inject(Deck(AddSlide))
        | Rename => inject(Deck(RenameSlide))
        | Delete => inject(Deck(DeleteSlide)),
      ~indicator=
        EditorModeView.indicator_select(
          ~signal=i => inject(Deck(SwitchSlide(i))),
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
