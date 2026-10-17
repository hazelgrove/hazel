open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Focus = ScratchFocus;
module Persist = ScratchPersist;

let slide_key = (~is_documentation, model: Model.t): string =>
  Persist.content_key(
    ScratchModel.prefix(~is_documentation),
    List.nth(model.scratchpads, model.current).name,
  );

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Workspace(Workspace.Action.t)
    | Outline(OutlineControl.Action.t)
    | RefreshStatics
    | DrvAction(DerivationExerciseMode.Update.t)
    | Deck(SlideDeck.Action.t);

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
      switch (Model.current_code(model)) {
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
        Model.with_current_kind(Code(code), model);
      }
    | Outline(a) =>
      switch (Model.current_code(model)) {
      | None => model |> return_quiet
      | Some(code) =>
        let* code =
          OutlineControl.update(
            ~settings,
            ~schedule_workspace=a => schedule_action(Workspace(a)),
            ~prefix=ScratchModel.prefix(~is_documentation),
            ~name=List.nth(model.scratchpads, model.current).name,
            a,
            code,
          );
        Model.with_current_kind(Code(code), model);
      }
    | DrvAction(a) =>
      switch (List.nth(model.scratchpads, model.current).kind) {
      | Drv(m) =>
        let* new_m =
          DerivationExerciseMode.Update.update(
            ~settings,
            ~schedule_action=a => schedule_action(DrvAction(a)),
            ~scratch_mode=true,
            a,
            m,
          );
        Model.with_current_kind(Drv(new_m), model);
      | Code(_) => model |> return_quiet
      }
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
        ~settings: Settings.t,
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

    let kind: Scratchpad.kind =
      switch (List.nth(model.scratchpads, model.current).kind) {
      | Code(code) =>
        Code(
          Workspace.calculate(
            ~settings=settings.core,
            ~autoprobe_mode=settings.autoprobe_mode,
            ~tail_probe=settings.tail_probe,
            ~schedule_action=a => schedule_action(Workspace(a)),
            ~is_edited,
            ~statics_mode,
            ~slide_key=slide_key(~is_documentation, model),
            code,
          ),
        )
      | Drv(m) =>
        Drv(
          DerivationExerciseMode.Update.calculate(
            ~settings=settings.core,
            ~autoprobe_mode=settings.autoprobe_mode,
            ~is_edited,
            ~schedule_action=a => schedule_action(DrvAction(a)),
            m,
          ),
        )
      };
    Model.with_current_kind(kind, model);
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(CellEditor.Selection.t)
    /* the open cells: the one Divided.active names has the focus */
    | Stack(CellEditor.Selection.t)
    /* the program level's library document */
    | Library(CellEditor.Selection.t)
    | Drv(DerivationExerciseMode.Selection.t)
    | TextBox;

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection, model: Model.t)
      : cursor(Update.t) => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    let cursor =
      switch (selection, scratchpad.kind) {
      /* while the library is shown, the keys are its */
      | (
          Cell(selection) | Stack(selection) | Library(selection),
          Code({lib: Some(editor), _}),
        ) =>
        let+ a =
          CellEditor.Selection.get_cursor_info(
            ~inject=a => inject(Workspace(LibraryAction(a))),
            ~selection,
            editor,
          );
        Update.Workspace(LibraryAction(a));
      | (Library(_), _) => empty
      | (Cell(selection), Code({program: Whole(editor), _})) =>
        let+ a =
          CellEditor.Selection.get_cursor_info(
            ~inject=a => inject(Workspace(CellAction(a))),
            ~selection,
            editor,
          );
        Update.Workspace(CellAction(a));
      | (Stack(selection), Code({program: Divided(d), _})) =>
        switch (Divided.focused(d)) {
        | Some((e, side)) =>
          let+ a =
            CellEditor.Selection.get_cursor_info(
              ~inject=a => inject(Workspace(Stack(e.e_id, side, a))),
              ~selection,
              ScratchCell.pane(side, e),
            );
          Update.Workspace(Stack(e.e_id, side, a));
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
      | (Stack(_), Code({program: Whole(_), _}))
      | (Stack(_), Drv(_))
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

  /* the open cell holding [tile], and the caret's move there */
  let jump_in_divided = (tile, d: Divided.t): option((Update.t, t)) => {
    let find = tile => {
      let caret: CellEditor.Update.t =
        MainEditor(Perform(Move(Goal(TileId(tile)))));
      let in_cell = cell =>
        Focus.seg_contains_id(tile, Focus.zip_of_cell(cell));
      Divided.cells(d)
      |> List.find_map((e: ScratchCell.t) =>
           List.find_map(
             side =>
               in_cell(ScratchCell.pane(side, e))
                 ? Some((
                     Update.Workspace(Stack(e.e_id, side, caret)),
                     Stack(MainEditor),
                   ))
                 : None,
             [ScratchCell.Body, Header],
           )
         );
    };
    switch (find(tile)) {
    | Some(_) as found => found
    /* a run member's row is its `;`, outside the run's cell: go to its
       `test` */
    | None =>
      Option.bind(Focus.test_before_semi(tile, Divided.document(d)), find)
    };
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    switch (scratchpad.kind) {
    | Code({lib: Some(editor), _}) =>
      CellEditor.Selection.jump_to_tile(tile, editor)
      |> Option.map(((x, y)) =>
           (Update.Workspace(LibraryAction(x)), Library(y))
         )
    | Code({program: Whole(editor), _}) =>
      CellEditor.Selection.jump_to_tile(tile, editor)
      |> Option.map(((x, y)) =>
           (Update.Workspace(CellAction(x)), Cell(y))
         )
    | Code({program: Divided(d), _}) => jump_in_divided(tile, d)
    | Drv(m) =>
      DerivationExerciseMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.DrvAction(x), Drv(y)))
    };
  };

  /* a jump to a whole-program id across open cells: (open the item
     holding it, select the stack, focus the pane holding it and move its
     caret there) */
  let cross_cell_target =
      (~target_id: Haz3lcore.Id.t, ~d: Divided.t)
      : option((Update.t, t, Update.t)) => {
    Util.OptUtil.Syntax.(
      {
        let statics = Divided.statics(d);
        /* the nearest enclosing outline item is the def to focus */
        let* fid =
          OutlineTree.enclosing_row(
            ~info_map=statics.info_map,
            target_id,
            statics.term,
          );
        /* the cell that holds [fid] once it's ensured */
        let j =
          switch (Divided.owner(fid, d)) {
          | Some(e) => e.e_id
          | None => fid
          };
        /* def binders live in the header cell, all else in the body;
           statements, tails and test runs have no header */
        let doc = Divided.document(d);
        let headless =
          Focus.headless_content_deep(fid, doc) != None
          || Focus.test_run_deep(fid, doc) != None;
        let side: ScratchCell.side =
          !headless
          && Focus.seg_contains_id(
               target_id,
               Option.value(Focus.find_pat(fid, doc), ~default=[]),
             )
            ? Header : Body;
        let caret: CellEditor.Update.t =
          MainEditor(Perform(Move(Goal(TileId(target_id)))));
        Some((
          Update.Workspace(FocusEnsure(fid)),
          Stack(MainEditor),
          Update.Workspace(Stack(j, side, caret)),
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
            Stack(
              i,
              side,
              MainEditor(Perform(Move(Goal(BindingSiteOfIndicatedVar)))),
            ),
          ),
          Some(Divided(d)),
        ) =>
        let* entry = Divided.cell(i, d);
        let cell = ScratchCell.pane(side, entry);
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

  /* an outline open selects the stack, where Workspace focuses the cell
     it opens; None when a toggle closes one */
  let stack_add_selection = (action: Update.t, model: Model.t): option(t) =>
    switch (action, Model.current_program(model)) {
    | (Workspace(a), Some(p)) when Workspace.focuses_item(a, p) =>
      Some(Stack(MainEditor))
    | _ => None
    };

  /* after an update, the selection follows the program between one
     editor and open cells */
  let follow = (selection: t, after: Model.t): t =>
    switch (selection, Model.current_program(after)) {
    | (Stack(_), Some(Whole(_))) => Cell(MainEditor)
    | (Cell(MainEditor), Some(Divided(_))) => Stack(MainEditor)
    | _ => selection
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
  open Virtual_dom.Vdom;
  open Node;

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
    /* the ⇒ header names the program */
    k_program: string,
  };
  type cached_cell = {
    c_key: stack_cache_key,
    c_header: CellEditor.Model.t,
    c_body: CellEditor.Model.t,
    c_settings: Settings.t,
    c_font_metrics: FontMetrics.t,
    c_colors: option(ColorSteps.colorMap),
    /* the whole program's test results, drawn as the cells' markers */
    c_tests: option(Language.TestResults.t),
    /* the program's result, for a cell whose drawers show its probe
       stepper or proofs */
    c_drawers: option(EvalResult.Model.t),
    c_nodes: list(Node.t),
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
        div(
          ~attrs=[Attr.classes(["slide-loading"])],
          [
            div(
              ~attrs=[Attr.classes(["spinner"])],
              [
                div(~attrs=[Attr.classes(["loader"])], []),
                div(
                  ~attrs=[Attr.classes(["nut-container"])],
                  [
                    create(
                      "img",
                      ~attrs=[
                        Attr.classes(["spinner-nut"]),
                        Attr.create("src", "img/hazelnut.svg"),
                      ],
                      [],
                    ),
                  ],
                ),
              ],
            ),
            text("loading "),
            text(current.name),
            text({js|…|js}),
          ],
        ),
      ];
    } else {
      switch (current.kind) {
      /* the program level: the library's document in the slide's place,
         its probes drawing the slide's samples (its run is the library's) */
      | Code({lib: Some(editor), program, _}) => [
          CellEditor.View.view(
            ~globals,
            ~master=(
              Program.result(program),
              a => inject(Workspace(CellAction(ResultAction(a)))),
            ),
            ~signal=
              fun
              | MakeActive(selection) =>
                signal(MakeActive(Library(selection))),
            ~inject=a => inject(Workspace(LibraryAction(a))),
            ~selected=
              switch (selected) {
              | Some(Selection.Cell(s) | Stack(s) | Library(s)) => Some(s)
              | _ => None
              },
            ~locked=false,
            ~lines=true,
            ~result_kind=`NoResults,
            editor,
          ),
        ]
      | Code({program, view, _}) =>
        let stack_views = (d: Divided.t) => {
          let cells = Divided.cells(d);
          let term = Divided.statics(d).term;
          /* the zoomed module as one cell: the breadcrumb names it */
          let zoom_cell = SlideView.showing_zoom_cell(~term, view);
          let tests = EvalResult.Model.test_results(Divided.result(d));
          let focus =
            switch (selected, Divided.focused(d)) {
            | (Some(Selection.Stack(sel)), Some((e, side))) =>
              Some((e.e_id, side, sel))
            | _ => None
            };
          let rendered =
            List.mapi(
              (i, e: ScratchCell.t) => {
                let sel = (side: ScratchCell.side) =>
                  switch (focus) {
                  | Some((j, s, sel)) when j == e.e_id && s == side =>
                    Some(sel)
                  | _ => None
                  };
                let key = {
                  k_index: i,
                  k_stack_len: List.length(cells),
                  k_header_sel: sel(Header),
                  k_body_sel: sel(Body),
                  k_meta_down: globals.Globals.Model.meta_down,
                  /* only the first body culls (below) */
                  k_visible_rows:
                    i == 0 ? globals.Globals.Model.visible_rows : None,
                  k_zoom_cell: zoom_cell,
                  k_program: current.name,
                };
                let draws = (c: CellEditor.Model.t) =>
                  c.editor.editor.state.zipper.refractors.stepping != None
                  || !
                       Haz3lcore.Id.Map.is_empty(
                         c.editor.editor.state.zipper.refractors.proofs,
                       );
                let drawers =
                  draws(e.e_body) || draws(e.e_header)
                    ? Some(Divided.result(d)) : None;
                switch (stack_cache_lookup(e.e_id)) {
                | Some(c)
                    when
                      c.c_key == key
                      && Option.equal((===), c.c_drawers, drawers)
                      && c.c_header === e.e_header
                      && c.c_body === e.e_body
                      && c.c_settings === globals.Globals.Model.settings
                      && c.c_font_metrics
                      === globals.Globals.Model.font_metrics
                      && c.c_colors === globals.Globals.Model.color_highlights
                      && Option.equal((===), c.c_tests, tests) => (
                    e.e_id,
                    c,
                  )
                | _ =>
                  let qualifier =
                    switch (OutlineTree.path_of(e.e_id, term)) {
                    | [] => []
                    | path => [
                        span(
                          ~attrs=[Attr.classes(["focus-qualifier"])],
                          [text(String.concat(".", path) ++ ".")],
                        ),
                      ]
                    };
                  /* arrow keys at a pane's edge walk the cells:
                     ... body(i-1) <- header(i) <-> body(i) -> header(i+1) ... */
                  /* no header pane: symbol cells, and every cell while a
                     zoomed module shows as its members */
                  let headerless = idx =>
                    switch (List.nth_opt(cells, idx)) {
                    | Some(e) => zoom_cell || ScratchCell.symbol(e) != None
                    | None => false
                    };
                  let pane_focus =
                      (
                        idx,
                        side: ScratchCell.side,
                        move: Haz3lcore.Action.move,
                      ) =>
                    if (idx < 0 || idx >= List.length(cells)) {
                      Effect.Ignore;
                    } else {
                      /* headerless entries have no header pane */
                      let side = headerless(idx) ? ScratchCell.Body : side;
                      /* DOM focus must follow to the new pane after render,
                         or the caret vanishes and arrows scroll the page */
                      Haz3lcore.FocusEffect.schedule_cell();
                      let id = List.nth(cells, idx).e_id;
                      Effect.Many([
                        signal(MakeActive(Stack(MainEditor))),
                        inject(
                          Workspace(
                            Stack(
                              id,
                              side,
                              MainEditor(Perform(Move(move))),
                            ),
                          ),
                        ),
                      ]);
                    };
                  let header_escape = (d: Util.Direction.t) =>
                    switch (d) {
                    | Left => pane_focus(i - 1, Body, End)
                    | Right => pane_focus(i, Body, Start)
                    };
                  let body_escape = (d: Util.Direction.t) =>
                    switch (d) {
                    | Left =>
                      headerless(i)
                        ? pane_focus(i - 1, Body, End)
                        : pane_focus(i, Header, End)
                    | Right => pane_focus(i + 1, Header, Start)
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
                  let pane_point = (idx, side, row, col) =>
                    pane_focus(
                      idx,
                      side,
                      Point(
                        Util.Point.{
                          row,
                          col: max(0, col),
                        },
                        None,
                      ),
                    );
                  let same_pane = (side, v: Haz3lcore.Action.vertical) =>
                    inject(
                      Workspace(
                        Stack(
                          e.e_id,
                          side,
                          MainEditor(Perform(Move(Vertical(v, ByChar)))),
                        ),
                      ),
                    );
                  let header_escape_vertical =
                      (v: Haz3lcore.Action.vertical, col) =>
                    switch (v) {
                    | Down => pane_point(i, Body, 0, col + qual_cols(i))
                    | Up =>
                      i == 0
                        ? same_pane(Header, Up)
                        : pane_point(
                            i - 1,
                            Body,
                            body_last_row(i - 1),
                            col + qual_cols(i),
                          )
                    };
                  let body_escape_vertical =
                      (v: Haz3lcore.Action.vertical, col) =>
                    switch (v) {
                    | Down =>
                      i + 1 >= List.length(cells)
                        ? same_pane(Body, Down)
                        : headerless(i + 1)
                            ? pane_point(i + 1, Body, 0, col)
                            : pane_point(
                                i + 1,
                                Header,
                                0,
                                col - qual_cols(i + 1),
                              )
                    | Up =>
                      headerless(i)
                        ? i == 0
                            ? same_pane(Body, Up)
                            : pane_point(
                                i - 1,
                                Body,
                                body_last_row(i - 1),
                                col,
                              )
                        : pane_point(i, Header, 0, col - qual_cols(i))
                    };
                  let pane_view =
                      (
                        side: ScratchCell.side,
                        ~result_kind,
                        ~lines,
                        ~master=?,
                        ~escape,
                        ~escape_vertical,
                        ~cull,
                        (),
                      ) =>
                    CellEditor.View.view(
                      ~globals,
                      /* the pane's own action (a click's caret move)
                         gives it the focus */
                      ~signal=
                        fun
                        | MakeActive(s) => signal(MakeActive(Stack(s))),
                      ~inject=
                        a => inject(Workspace(Stack(e.e_id, side, a))),
                      ~selected=sel(side),
                      ~result_kind,
                      ~locked=false,
                      ~lines,
                      ~master?,
                      ~escape,
                      ~escape_vertical=Some(escape_vertical),
                      ~cull,
                      ScratchCell.pane(side, e),
                    );
                  let header_pane =
                    switch (ScratchCell.symbol(e)) {
                    | Some(sym) =>
                      div(
                        ~attrs=[
                          Attr.classes(["focus-header", "focus-header-sym"]),
                        ],
                        /* no qualifier chip: the symbol is the label; the
                           trailing ⇒ names what it's the result of, the
                           program (not editable) */
                        [
                          span(
                            ~attrs=[Attr.classes(["focus-sym"])],
                            (
                              sym == {js|⇒|js}
                                ? [
                                  span(
                                    ~attrs=[
                                      Attr.classes(["focus-sym-name"]),
                                    ],
                                    [text(current.name)],
                                  ),
                                ]
                                : []
                            )
                            @ [
                              span(
                                ~attrs=[Attr.classes(["focus-sym-mark"])],
                                [text(sym)],
                              ),
                            ]
                            @ (
                              sym == {js|⇒|js}
                                ? [
                                  span(
                                    ~attrs=[
                                      Attr.classes(["focus-sym-word"]),
                                    ],
                                    [text("result")],
                                  ),
                                ]
                                : []
                            ),
                          ),
                        ],
                      )
                    | None =>
                      div(
                        ~attrs=[Attr.classes(["focus-header"])],
                        qualifier
                        @ [
                          pane_view(
                            Header,
                            ~result_kind=`NoResults,
                            ~lines=false,
                            ~escape=header_escape,
                            ~escape_vertical=header_escape_vertical,
                            ~cull=false,
                            (),
                          ),
                        ],
                      )
                    };
                  let nodes =
                    (zoom_cell ? [] : [header_pane])
                    @ [
                      div(
                        ~attrs=[
                          Attr.classes(
                            ["focus-body"] @ (zoom_cell ? ["zoom-body"] : []),
                          ),
                        ],
                        [
                          pane_view(
                            Body,
                            /* its tests ran in the whole program */
                            ~result_kind=`TestSigilsOnly,
                            ~lines=true,
                            ~master=(
                              Divided.result(d),
                              a =>
                                inject(
                                  Workspace(CellAction(ResultAction(a))),
                                ),
                            ),
                            ~escape=body_escape,
                            ~escape_vertical=body_escape_vertical,
                            /* culling measures one `.cull-scope`: only
                               the first body opts in; the rest render
                               unculled, not against another's rows */
                            ~cull=i == 0,
                            (),
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
                      c_tests: tests,
                      c_drawers: drawers,
                      c_nodes: nodes,
                    },
                  );
                };
              },
              cells,
            );
          stack_cache := rendered;
          /* the whole program's status is the inspector's; below the
             cells, its elaboration while evaluation is off. Nothing here
             takes input */
          let (result_footer, _overlays) =
            EvalResult.View.view(
              ~globals,
              ~signal=_ => Effect.Ignore,
              ~inject=_ => Effect.Ignore,
              ~selected=None,
              ~locked=false,
              ~result_kind=`StatusLine,
              Divided.result(d),
            );
          List.concat_map(((_, c)) => c.c_nodes, rendered)
          @ (
            List.is_empty(result_footer)
              ? []
              : [
                div(~attrs=[Attr.classes(["stack-result"])], result_footer),
              ]
          )
          @ [
            /* slack, so even the last cell can scroll to the viewport top */
            div(~attrs=[Attr.classes(["stack-slack"])], []),
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
              ~result_kind=`StatusLine,
              editor,
            ),
          ]
          /* room past the end, so the last line can scroll to the top;
             with ⇓ on, the program's drawer runs on into it. The same
             height either way, so toggling ⇓ leaves the view in place */
          @ [
            div(
              ~attrs=[
                Attr.classes(
                  ["tail-slack"]
                  @ (globals.settings.tail_probe ? ["drawer"] : []),
                ),
              ],
              [],
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
          | None => Effect.Ignore
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
            Effect.Ignore;
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
          Effect.Ignore;
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
        | Next => Effect.Ignore
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
