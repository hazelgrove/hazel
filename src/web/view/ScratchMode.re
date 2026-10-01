open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Focus = ScratchFocus;
module Persist = ScratchPersist;

let slide_key = (~is_documentation, model: Model.t): string =>
  Persist.content_key(
    is_documentation ? "doc" : "scratch",
    List.nth(model.scratchpads, model.current).name,
  );

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
    | Outline(OutlineControl.Action.t)
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
    | Outline(a) =>
      switch (current_code(model)) {
      | None => model |> return_quiet
      | Some(code) =>
        let* code =
          OutlineControl.update(
            ~settings,
            ~schedule_workspace=a => schedule_action(Workspace(a)),
            ~prefix=is_documentation ? "doc" : "scratch",
            ~name=List.nth(model.scratchpads, model.current).name,
            a,
            code,
          );
        with_code(model, code);
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
           ~action=inject(Outline(Focus)),
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

  /* a jump to a whole-program id across open cells: (open the item
     holding it, select the pane holding it, move that pane's caret) */
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
        /* def binders live in the header cell, all else in the body */
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

  /* where an outline open lands the selection: the body pane of the
     cell that will hold [fid]; None when a toggle closes it */
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
    /* an id already in an open cell (its own or an enclosing one) opens
       no cell, so the selection stays put */
    | (Workspace(FocusToggle(fid)), Some(Divided(d))) =>
      Option.is_none(Divided.owner(fid, d))
        ? Some(StackB(fid, MainEditor)) : None
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

  /* per-cell view cache, keyed on everything the cell view reads: a
     keystroke in one cell must not rebuild the others */
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

  /* read the cache only through this helper, never bind `stack_cache^` in
     the view: jsoo closures share one context per scope, so each render's
     handlers would retain the last one's vdom, leaking every generation */
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
        /* shown until hydration, which blocks on large slides; the same
           spinner as the boot screen (index.html/loading.css) */
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
                  /* arrow keys at a pane's edge walk the cells:
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
                      /* DOM focus must follow to the new pane after render,
                         or the caret vanishes and arrows scroll the page */
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
                  /* Up/Down at a pane's edge keep the goal column in the
                     adjacent pane, shifted by the qualifier chip's width
                     across a header; at the ends the plain move is
                     re-dispatched, keeping its line-start/end snap */
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
                      Virtual_dom.Vdom.Node.div(
                        ~attrs=[
                          Virtual_dom.Vdom.Attr.classes([
                            "focus-header",
                            "focus-header-sym",
                          ]),
                        ],
                        /* no qualifier chip: the symbol is the label */
                        [
                          Virtual_dom.Vdom.Node.span(
                            ~attrs=[
                              Virtual_dom.Vdom.Attr.classes(["focus-sym"]),
                            ],
                            [Virtual_dom.Vdom.Node.text(sym)]
                            @ (
                              sym == {js|⇒|js}
                                ? [
                                  Virtual_dom.Vdom.Node.span(
                                    ~attrs=[
                                      Virtual_dom.Vdom.Attr.classes([
                                        "focus-sym-word",
                                      ]),
                                    ],
                                    [Virtual_dom.Vdom.Node.text("result")],
                                  ),
                                ]
                                : []
                            ),
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
                            /* culling measures one `.cull-scope`: only
                               the first body opts in; the rest render
                               unculled, not against another's rows */
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
          /* the whole program's result, live below the cells */
          let (result_footer, _overlays) =
            EvalResult.View.view(
              ~globals,
              ~signal=
                fun
                | MakeActive(a) => signal(MakeActive(Cell(Result(a))))
                | JumpTo(id) =>
                  /* the target may be in no open cell: open its item */
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
            /* slack, so even the last cell can scroll to the viewport top */
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
