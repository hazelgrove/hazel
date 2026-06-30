open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Haz3lcore;
open Language;

let errc = "error";
let warnc = "warning";
let okc = "ok";
let div_err = div(~attrs=[clss(["status", errc])]);
let div_ok = div(~attrs=[clss(["status", okc])]);
let div_warn = div(~attrs=[clss(["status", warnc])]);
let code_box_container = x =>
  div(~attrs=[clss(["code-box-container"])], [x]);
/* When true, prefixes type displays with ":" (e.g. ": Int").
   Appropriate in the cursor inspector but not in the error sidebar. */
let colon_prefix = show_type_colon => show_type_colon ? [text(":")] : [];

let code = (code: string): Node.t =>
  div(~attrs=[clss(["code"])], [text(code)]);

let label_view = (label: string): Node.t =>
  div(
    ~attrs=[clss(["code"])],
    [text(Haz3lcore.Token.quote_label_when_necessary(label))],
  );

let cls_view = (ci: Info.t): Node.t => {
  let cls = ci |> Info.cls_of;
  let cls_text =
    switch (Info.projector_kind_of(ci)) {
    | Some(kind) => "Projector (" ++ ProjectorKind.show(kind) ++ ")"
    | None =>
      switch (cls) {
      | Typ(EmptyHole)
      | Exp(EmptyHole)
      | Pat(EmptyHole) =>
        Info.is_label(ci) ? "Label Hole" : Info.cls_text_of(ci)
      | _ => Info.cls_text_of(ci)
      }
    };

  div(~attrs=[clss(["syntax-class"])], [text(cls_text)]);
};

let ctx_toggle = (~globals: Globals.t): Node.t =>
  div(
    ~attrs=[
      Attr.on_click(_ => globals.inject_global(Set(ContextInspector))),
      clss(
        ["gamma"] @ (globals.settings.context_inspector ? ["visible"] : []),
      ),
    ],
    [Icons.gamma],
    //[text("Γ")],
  );

let term_view = (~globals: Globals.t, ci) => {
  /* Drv(_) sorts have verbose type-level names like "DrvJdmt"/"DrvProp"
     via Sort.to_string (needed for pretty-printing `DrvQuoteTy`). For the
     inspector header we prefer the terse form ("Jdmt", "Prop", ...),
     keeping the ALFA prefix for object-language sorts. */
  let sort_text =
    Info.is_label(ci)
      ? "Label"
      : (
        switch (Info.sort_of(ci)) {
        | Drv(s) => DrvSort.to_string_short(s)
        | s => Sort.to_string(s)
        }
      );
  let sort_class = Info.is_label(ci) ? "Label" : ci |> Info.class_of;
  div(
    ~attrs=[
      clss(
        ["ci-header", sort_class]
        @ (
          Info.is_error(ci)
            ? [errc]
            : Info.is_warning(ci) && globals.settings.core.display_warnings
                ? [warnc] : [okc]
        ),
      ),
    ],
    [
      ctx_toggle(~globals),
      div(~attrs=[clss(["term-tag"])], [text(sort_text)]),
      div(~attrs=[clss(["divider"])], [text("/")]),
      cls_view(ci),
    ],
  );
};

let elements_noun: Cls.t => string =
  fun
  | Exp(Match | If) => "Branches"
  | Exp(ListLit)
  | Pat(ListLit) => "Elements"
  | Exp(ListConcat)
  | Exp(BinOp(Poly(_))) => "Operands"
  | _ => "Sub-expressions";

let code_view_settings: Haz3lcore.ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: false,
  show_unknown_as_hole: true,
};

module TypeTarget = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Synthesizing
    | Analyzing;

  let label =
    fun
    | Synthesizing => "syn"
    | Analyzing => "ana";
};

let fold_editor_permits = (action: CodeEditable.Update.t): bool =>
  switch (action) {
  | Perform(Move(_) | Select(_) | Unselect(_) | Copy | Project(_)) => true
  | Perform(
      Destruct(_) | Insert(_) | Put_down | Paste(_) | Reparse | Cut | Buffer(_) |
      Structural(_) |
      Probe(_) |
      PrettyPrint |
      Dump |
      Introduce |
      ToggleLineComment,
    )
  | ContextMenu(_)
  | DebugConsole(_)
  | TAB => false
  };

let type_menu_showing: ContextMenu.parts = {
  jump_to_binding: false,
  select_term: true,
  introduce: false,
  refractors: false,
  projectors: true,
};

let type_editor_of_type = (typ: Typ.t): (Id.t, CodeEditable.Model.t) => {
  let typ = Typ.replace_temp(typ);
  (
    Typ.rep_id(typ),
    ExpToSegment.typ_to_segment(~settings=code_view_settings, typ)
    |> Zipper.unzip
    |> Editor.Model.mk(~root=Sort.Typ)
    |> CodeWithStatics.Model.mk,
  );
};

let explain_type_tooltip = "Explain this type by folding parts of it into a slicing query.";

let typ_for_target = (target: TypeTarget.t, ci: Info.t): option(Typ.t) =>
  switch (ci, target) {
  | (InfoExp({elab_syn_ty, _}), Synthesizing)
  | (InfoPat({elab_syn_ty, _}), Synthesizing) => Some(elab_syn_ty)
  | (InfoExp({ana, _}), Analyzing)
  | (InfoPat({ana, _}), Analyzing) =>
    Some(Statics.ana_skip_explicit_nonlabel(ana))
  | _ => None
  };

module Model = {
  module OptionalId = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | NoId
      | SomeId(Id.t);
  };

  module EditorSlot = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | NoEditor
      | SomeEditor(CodeEditable.Model.t);
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type row = {
    active: bool,
    cursor_id: OptionalId.t,
    typ_id: OptionalId.t,
    editor: EditorSlot.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type menu_state =
    | NoMenu
    | Menu(TypeTarget.t, float, float);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    syn: row,
    ana: row,
    menu: menu_state,
  };

  let empty_row = {
    active: false,
    cursor_id: OptionalId.NoId,
    typ_id: OptionalId.NoId,
    editor: EditorSlot.NoEditor,
  };

  let init = {
    syn: empty_row,
    ana: empty_row,
    menu: NoMenu,
  };

  let row = (target: TypeTarget.t, model: t): row =>
    switch (target) {
    | Synthesizing => model.syn
    | Analyzing => model.ana
    };

  let put_row = (target: TypeTarget.t, row: row, model: t): t =>
    switch (target) {
    | Synthesizing => {
        ...model,
        syn: row,
      }
    | Analyzing => {
        ...model,
        ana: row,
      }
    };

  let refresh_row = (target: TypeTarget.t, ci: Info.t, row: row): row =>
    switch (typ_for_target(target, ci)) {
    | None => empty_row
    | Some(typ) =>
      let cursor_id = Info.id_of(ci);
      let source_typ_id = Typ.rep_id(typ);
      switch (row.cursor_id, row.typ_id, row.editor) {
      | (
          OptionalId.SomeId(old_cursor),
          OptionalId.SomeId(old_typ),
          EditorSlot.SomeEditor(_),
        )
          when
            Id.equal(old_cursor, cursor_id)
            && (
              Id.equal(source_typ_id, Id.invalid)
              || Id.equal(old_typ, source_typ_id)
            ) => row
      | _ =>
        let (typ_id, editor) = type_editor_of_type(typ);
        {
          ...row,
          cursor_id: OptionalId.SomeId(cursor_id),
          typ_id: OptionalId.SomeId(typ_id),
          editor: EditorSlot.SomeEditor(editor),
        };
      };
    };

  let refresh_for_info = (ci: Info.t, model: t): t =>
    switch (ci) {
    | InfoExp(_)
    | InfoPat(_) => {
        ...model,
        syn: refresh_row(Synthesizing, ci, model.syn),
        ana: refresh_row(Analyzing, ci, model.ana),
      }
    | _ => init
    };

  let has_active = model => model.syn.active || model.ana.active;
};

let type_slicing_focus_of_row =
    (target: TypeTarget.t, row: Model.row): option((string, Ctx.t, Typ.t)) =>
  switch (row.active, row.editor) {
  | (true, Model.EditorSlot.SomeEditor(editor)) =>
    switch (
      Indicated.ci_of(editor.editor.state.zipper, editor.statics.info_map)
    ) {
    | Some(InfoTyp({user_term, ctx, _})) =>
      Some((
        "Type Slicing ("
        ++ (
          switch (target) {
          | Synthesizing => "Synthesis"
          | Analyzing => "Analysis"
          }
        )
        ++ ")",
        ctx,
        user_term,
      ))
    | _ => None
    }
  | _ => None
  };

let type_slicing_focuses = (model: Model.t): list((string, Ctx.t, Typ.t)) =>
  [
    type_slicing_focus_of_row(Synthesizing, model.syn),
    type_slicing_focus_of_row(Analyzing, model.ana),
  ]
  |> List.filter_map(focus => focus);

module TypeSlicing = {
  let id_set_of_list = (ids: list(Id.t)): Id.Set.t =>
    List.fold_left((acc, id) => Id.Set.add(id, acc), Id.Set.empty, ids);

  let typ_of_editor = (editor: CodeEditable.Model.t): option(Typ.t) =>
    switch (
      editor.editor.state.zipper
      |> Zipper.unselect_and_zip
      |> MakeTerm.for_projection
    ) {
    | Some(Typ(typ)) => Some(typ)
    | _ => None
    };

  let query_of_typ = (typ: Typ.t): Typ.t => {
    let f_typ = (continue, {term, _} as typ: Typ.t) =>
      switch (term) {
      | Projector({kind, _}, _) when kind == ProjectorCore.Kind.Fold => Statics.Slice.gap
      | _ => continue(typ)
      };
    Typ.map_term(~f_typ, typ);
  };

  let query_of_row = (row: Model.row): option(Typ.t) =>
    switch (row.editor) {
    | Model.EditorSlot.NoEditor => None
    | Model.EditorSlot.SomeEditor(editor) =>
      Option.map(query_of_typ, typ_of_editor(editor))
    };

  let protected_ids = (ci: Info.t): Id.Set.t =>
    id_set_of_list([Info.id_of(ci), ...Info.ancestors_of(ci)]);

  let slice_for_target =
      (~root_exp: Exp.t, ~ci: Info.t, target: TypeTarget.t, row: Model.row)
      : option(Id.Set.t) =>
    if (!row.active) {
      None;
    } else {
      switch (query_of_row(row)) {
      | None => Some(Id.Set.empty)
      | Some(query) =>
        let direction =
          switch (target) {
          | Synthesizing => `Syn
          | Analyzing => `Ana
          };
        try(
          Some(
            Id.Set.diff(
              Statics.slice(
                ~ctx=Info.ctx_of(ci),
                ~focus=Some(Info.id_of(ci)),
                ~direction,
                root_exp,
                query,
              ).
                omitted,
              protected_ids(ci),
            ),
          )
        ) {
        | _ => Some(Id.Set.empty)
        };
      };
    };

  let omitted_ids = (~root_exp: Exp.t, ~ci: Info.t, model: Model.t) => {
    let syn = slice_for_target(~root_exp, ~ci, Synthesizing, model.syn);
    let ana = slice_for_target(~root_exp, ~ci, Analyzing, model.ana);
    switch (syn, ana) {
    | (Some(syn), Some(ana)) => Id.Set.inter(syn, ana)
    | (Some(ids), None)
    | (None, Some(ids)) => ids
    | (None, None) => Id.Set.empty
    };
  };

  let row_info =
      (
        ~info_map: Statics.Map.t,
        ~fallback_ci: option(Info.t),
        row: Model.row,
      )
      : option(Info.t) =>
    switch (row.cursor_id) {
    | Model.OptionalId.SomeId(id) =>
      switch (Statics.Map.lookup(id, info_map)) {
      | Some(_) as ci => ci
      | None => fallback_ci
      }
    | Model.OptionalId.NoId => fallback_ci
    };

  let slice_for_model_row =
      (
        ~root_exp: Exp.t,
        ~info_map: Statics.Map.t,
        ~fallback_ci: option(Info.t),
        target: TypeTarget.t,
        row: Model.row,
      )
      : option(Id.Set.t) =>
    switch (row_info(~info_map, ~fallback_ci, row)) {
    | Some(ci) => slice_for_target(~root_exp, ~ci, target, row)
    | None => None
    };

  let omitted_ids_for_model =
      (
        ~root_exp: Exp.t,
        ~info_map: Statics.Map.t,
        ~fallback_ci: option(Info.t),
        model: Model.t,
      )
      : Id.Set.t => {
    let syn =
      slice_for_model_row(
        ~root_exp,
        ~info_map,
        ~fallback_ci,
        Synthesizing,
        model.syn,
      );
    let ana =
      slice_for_model_row(
        ~root_exp,
        ~info_map,
        ~fallback_ci,
        Analyzing,
        model.ana,
      );
    switch (syn, ana) {
    | (Some(syn), Some(ana)) => Id.Set.inter(syn, ana)
    | (Some(ids), None)
    | (None, Some(ids)) => ids
    | (None, None) => Id.Set.empty
    };
  };
};

module ProgramFolds = {
  type result = {
    model: CodeEditable.Model.t,
    changed: bool,
  };

  let with_zipper = (zipper: Zipper.t, model: CodeEditable.Model.t) => {
    let editor = Editor.Model.mk(zipper, ~root=model.editor.root);
    {
      ...model,
      editor: {
        ...editor,
        syntax: CachedSyntax.mark_old(editor.syntax),
      },
      context_menu: None,
    };
  };

  let remove_all = (model: CodeEditable.Model.t): result => {
    let changed = ref(false);
    let remove_piece = (piece: Piece.t): Segment.t =>
      switch (piece) {
      | Piece.Projector({kind, syntax, _})
          when kind == ProjectorCore.Kind.Fold =>
        changed := true;
        Piece.unparenthesize(syntax);
      | _ => [piece]
      };
    {
      model:
        model
        |> with_zipper(
             ZipperBase.MapPiece.go(remove_piece, model.editor.state.zipper),
             _,
           ),
      changed: changed^,
    };
  };

  let apply_folds = (~omitted: Id.Set.t, model: CodeEditable.Model.t): result =>
    if (Id.Set.is_empty(omitted)) {
      {
        model,
        changed: false,
      };
    } else {
      let generated = ref([]);
      let add_piece = (piece: Piece.t): Segment.t =>
        if (Id.Set.mem(Piece.id(piece), omitted)) {
          let seg = [piece];
          switch (MakeTerm.for_projection(seg)) {
          | None => [piece]
          | Some(any) =>
            switch (
              ProjectorInit.init(
                ProjectorCore.Kind.Fold,
                Segment.parenthesize(seg),
                any,
              )
            ) {
            | Some(Piece.Projector(pr) as projected) =>
              generated := [pr.id, ...generated^];
              [projected];
            | Some(projected) => [projected]
            | None => [piece]
            }
          };
        } else {
          [piece];
        };
      let model =
        model
        |> with_zipper(
             ZipperBase.MapPiece.go(add_piece, model.editor.state.zipper),
             _,
           );
      {
        model,
        changed: generated^ != [],
      };
    };

  let root_exp = (model: CodeEditable.Model.t): option(Exp.t) =>
    switch (
      model.editor.state.zipper
      |> Zipper.unselect_and_zip
      |> MakeTerm.for_projection
    ) {
    | Some(Exp(exp)) => Some(exp)
    | _ => None
    };

  let apply_type_slice =
      (
        ~info_map: Statics.Map.t,
        ~fallback_ci: option(Info.t),
        ~cursor_inspector: Model.t,
        model: CodeEditable.Model.t,
      )
      : result =>
    if (!Model.has_active(cursor_inspector)) {
      {
        model,
        changed: false,
      };
    } else {
      switch (root_exp(model)) {
      | Some(root_exp) =>
        let ids =
          TypeSlicing.omitted_ids_for_model(
            ~root_exp,
            ~info_map,
            ~fallback_ci,
            cursor_inspector,
          );
        apply_folds(~omitted=ids, model);
      | None => {
          model,
          changed: false,
        }
      };
    };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Toggle(TypeTarget.t)
    | TypeEditor(TypeTarget.t, CodeEditable.Update.t)
    | OpenMenu(TypeTarget.t, float, float)
    | CloseMenu;

  let can_undo = (_: t) => false;

  let type_editor_caret_selector = "#cursor-inspector .type-summary-editor.active .caret";

  let animate_type_editor_caret =
      (~settings: Settings.t, action: CodeEditable.Update.t) =>
    switch (action) {
    | Perform(action)
        when settings.core.flip_animations && Action.should_animate(action) =>
      Animation.request([Animation.Actions.move(type_editor_caret_selector)])
    | _ => ()
    };

  let calculate_type_editor =
      (~settings: Settings.t, model: CodeEditable.Model.t)
      : CodeEditable.Model.t => {
    ...model,
    editor:
      Editor.Update.calculate(
        ~settings=settings.core,
        ~autoprobe_mode=false,
        ~is_edited=true,
        model.statics,
        model.dynamics,
        model.editor,
      ),
  };

  let move_editor_to_root = (~settings: Settings.t, row: Model.row): Model.row =>
    switch (row.typ_id, row.editor) {
    | (Model.OptionalId.SomeId(id), Model.EditorSlot.SomeEditor(editor)) =>
      switch (CodeEditable.Selection.jump_to_tile(id, editor)) {
      | Some(action) =>
        animate_type_editor_caret(~settings, action);
        let updated = CodeEditable.Update.update(~settings, action, editor);
        {
          ...row,
          editor:
            Model.EditorSlot.SomeEditor(
              calculate_type_editor(~settings, updated.model),
            ),
        };
      | None => row
      }
    | _ => row
    };

  let update =
      (
        ~settings: Settings.t,
        ~cursor_info: option(Info.t),
        action: t,
        model: Model.t,
      )
      : Updated.t(Model.t) =>
    switch (cursor_info) {
    | None => model |> Updated.return_quiet
    | Some(ci) =>
      let model = Model.refresh_for_info(ci, model);
      switch (action) {
      | Toggle(target) =>
        let row = Model.row(target, model);
        let row = {
          ...row,
          active: !row.active,
        };
        (row.active ? move_editor_to_root(~settings, row) : row)
        |> Model.put_row(target, _, model)
        |> Updated.return_quiet;
      | TypeEditor(target, editor_action) =>
        let row = Model.row(target, model);
        switch (row.active, row.editor) {
        | (true, Model.EditorSlot.SomeEditor(editor))
            when fold_editor_permits(editor_action) =>
          animate_type_editor_caret(~settings, editor_action);
          let updated =
            CodeEditable.Update.update(~settings, editor_action, editor);
          {
            ...row,
            editor:
              Model.EditorSlot.SomeEditor(
                calculate_type_editor(~settings, updated.model),
              ),
          }
          |> Model.put_row(target, _, model)
          |> Updated.return_quiet;
        | _ => model |> Updated.return_quiet
        };
      | OpenMenu(target, x, y) =>
        {
          ...model,
          menu: Model.Menu(target, x, y),
        }
        |> Updated.return_quiet
      | CloseMenu =>
        {
          ...model,
          menu: Model.NoMenu,
        }
        |> Updated.return_quiet
      };
    };
};

let view_any = (~globals, any: Any.t) =>
  any
  |> CodeViewable.view_any(~globals, ~settings=code_view_settings)
  |> code_box_container;

let view_type = (~globals, typ: Typ.t) =>
  typ
  |> CodeViewable.view_typ(~globals, ~settings=code_view_settings)
  |> code_box_container;

/* Inline type in a message: static, or a fold editor when toggled. */
let type_slot =
    (
      ~globals,
      ~model: Model.t,
      ~inject: Update.t => Ui_effect.t(unit),
      target: TypeTarget.t,
      typ: Typ.t,
    )
    : Node.t => {
  let row_model = Model.row(target, model);
  let toggle =
    div(
      ~attrs=[
        clss(["explain-toggle"] @ (row_model.active ? ["active"] : [])),
        Attr.title(explain_type_tooltip),
        Attr.on_pointerdown(_ =>
          Effect.Many([
            Effect.Stop_propagation,
            inject(Update.Toggle(target)),
          ])
        ),
      ],
      [Icons.explain_this],
    );
  let body =
    switch (row_model.active, row_model.editor) {
    | (true, Model.EditorSlot.SomeEditor(editor)) =>
      let edit_mode =
        EditMode.Editable({
          inject: action =>
            fold_editor_permits(action)
              ? inject(Update.TypeEditor(target, action)) : Ui_effect.Ignore,
          escape: _ => Ui_effect.Ignore,
          take_focus: _ => Ui_effect.Ignore,
          focus: Some(),
        });
      div(
        ~attrs=[
          clss(["type-summary-editor", "active"]),
          Attr.on_pointerdown(evt => {
            let button: int = Js_of_ocaml.Js.Unsafe.get(evt, "button");
            if (button == 2) {
              let x: float = Js_of_ocaml.Js.Unsafe.get(evt, "clientX");
              let y: float = Js_of_ocaml.Js.Unsafe.get(evt, "clientY");
              Effect.Many([
                Effect.Prevent_default,
                inject(Update.OpenMenu(target, x, y)),
              ]);
            } else {
              Effect.Ignore;
            };
          }),
        ],
        [
          CodeEditable.View.view(
            ~globals,
            ~signal=_ => Ui_effect.Ignore,
            ~edit_mode,
            ~dynamics=editor.dynamics,
            editor,
          ),
        ],
      );
    | _ => view_type(~globals, typ)
    };
  let menu =
    switch (model.menu, row_model.editor) {
    | (Model.Menu(mt, x, y), Model.EditorSlot.SomeEditor(editor))
        when mt == target && row_model.active =>
      let menu_inject = (action: Action.t) =>
        Effect.Many([
          inject(Update.TypeEditor(target, Perform(action))),
          inject(Update.CloseMenu),
        ]);
      let items =
        ContextMenu.get_sections(
          ~showing=type_menu_showing,
          ~info_map=editor.statics.info_map,
          editor.editor.state.zipper,
        )
        |> List.map(
             List.map(
               ContextMenu.menu_item_view(
                 ~inject=menu_inject,
                 ~is_selected=false,
               ),
             ),
           )
        |> ListUtil.join([ContextMenu.divider])
        |> List.concat;
      [
        div(
          ~attrs=[
            clss(["context-menu-backdrop"]),
            Attr.on_pointerdown(_ => inject(Update.CloseMenu)),
          ],
          [],
        ),
        div(
          ~attrs=[
            clss(["context-menu", "open-up-right", "type-slice-menu"]),
            Attr.create(
              "style",
              Printf.sprintf("left: %fpx; top: %fpx;", x, y),
            ),
          ],
          [ContextMenu.context_menu(items)],
        ),
      ];
    | _ => []
    };
  div(~attrs=[clss(["type-slot"])], [body, toggle] @ menu);
};

let core_mark_err_view =
    (
      ~globals,
      ~show_type_colon=true,
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      ~ctx: Ctx.t,
      ~ana: Typ.t,
      cls: Cls.t,
      m: Mark.t,
    ) => {
  let view_type = view_type(~globals);
  let view_any = view_any(~globals);
  let ana = Statics.ana_skip_explicit_nonlabel(ana);
  let expectation_view = (~ana: Typ.t, ~syn: Typ.t) =>
    switch (syn.term, ana.term) {
    | (Label(syn_l), Label(an_label)) => [
        code(syn_l),
        text("but expected label"),
        code(an_label),
      ]
    | _ =>
      colon_prefix(show_type_colon)
      @ [
        view_type(syn) |> code_box_container,
        text("inconsistent with expected type"),
        view_type(ana) |> code_box_container,
      ]
      @ (
        switch (lifted_ty) {
        | None => []
        | Some(lifted) => [text(" lifted to"), view_type(lifted)]
        }
      )
      @ (
        switch (introduced_labels) {
        | [] => []
        | [a] => [text("after automatically added label "), code(a)]
        | _ => [
            text("after automatically added labels "),
            ...ListUtil.join(text(","), List.map(code, introduced_labels)),
          ]
        }
      )
    };
  (
    switch (m) {
    | BadToken(token) =>
      switch (Haz3lcore.Token.bad_token_cls(token)) {
      | BadInt => [text("Integer is too large or too small")]
      | Other => [text(Printf.sprintf("\"%s\" isn't a valid token", token))]
      }
    | BadLabel(label) => [text("Malformed Label: "), view_any(label)]
    | FreeConstructor(name) => [code(name), text("not found")]

    | InvalidLabel(name, expected_labels) =>
      switch (expected_labels) {
      | [] => [
          text("Invalid label: "),
          label_view(name),
          text(". No labels were expected."),
        ]
      | _ => [
          text("Invalid label: "),
          label_view(name),
          text(" is not part of the expected labels: "),
          ...List.map(code, expected_labels),
        ]
      }
    | UnexpectedLabelSort(name) => [
        text("Label "),
        label_view(name),
        text(" is here, but another sort is expected."),
      ]
    | IsMulti => [text("Broken expression")]

    | TupleLabelError({malformed_labels, duplicate_labels, invalid_labels, _}) =>
      (
        List.is_empty(malformed_labels)
          ? []
          : [
            text("Malformed labels: "),
            ...List.map(view_any, malformed_labels),
          ]
      )
      @ (
        List.is_empty(duplicate_labels)
          ? []
          : [
            text("Duplicate labels: "),
            ...List.map(code, duplicate_labels),
          ]
      )
      @ (
        List.is_empty(invalid_labels)
          ? []
          : [text("Invalid labels: "), ...List.map(code, invalid_labels)]
      )
    | DuplicateVar(name, _) => [text("Duplicate Variable:"), code(name)]
    | DuplicateLabel(name, _) => [
        text("Duplicate Label:"),
        label_view(name),
      ]
    | CompareFun(ty) => [text("values cannot be compared:"), view_type(ty)]
    | ExpectationMismatch({ana, syn}) => expectation_view(~ana, ~syn)
    | NoMeet(PolyEq, tys)
    | NoMeet(_, tys) when ana.term == Unknown(SynSwitch) => [
        text(elements_noun(cls) ++ " have inconsistent types:"),
        ...ListUtil.join(
             text(","),
             List.map(view_type, Typ.of_source(tys)),
           ),
      ]
    | NoMeet(wrap, _) =>
      let syn: Typ.t = SynTy.meet_of(wrap, Unknown(Internal) |> Typ.temp);
      switch (Typ.meet(ctx, ana, syn)) {
      | Some(_) => [text("Type error")]
      | None =>
        switch (ana.term, syn.term) {
        | (Label(_), _) => [text("Malformed Label: "), view_any(Typ(syn))]
        | _ => expectation_view(~ana, ~syn)
        }
      };
    | ExplicitNonlabel => [text("Type error")]
    | Free(_)
    | InexhaustiveMatch(_)
    | IsDeferral(_)
    | IsBadPartialAp(_)
    | BuiltinError(_)
    | InvalidUseMode(_)
    | IsLivelitName(_)
    | BadTrivAp(_)
    | DotOperatorRequiresTuple
    | TupleExtensionRequiresTuples
    | LabelNotFound(_)
    | BadOperator(_)
    | BadLivelitModel(_)
    | BadTheorem(_)
    | Redundant
    | ExpectedConstructor
    | TypFreeTypeVariable(_)
    | TypKindMismatch(_)
    | TypParamApplyNonArrowKind(_)
    | TypParamApplyArityMismatch(_)
    | TypAbsApplyArityMismatch(_)
    | TypDuplicateConstructor(_)
    | TypDuplicateLabels(_, _)
    | TypWantTypeFoundAp
    | TypWantLabel
    | TypWantProduct(_)
    | TypWantConstructorFoundType(_)
    | TypWantConstructorFoundAp
    | TypParseFailure
    | TPatShadowsType(_)
    | TPatNotAVar(_)
    | TPatParamNotAtAliasHead(_) => [text("Type error")]
    }
  )
  @ (
    switch (inferred_label) {
    | None => []
    | Some(l) => [text(" for label "), label_view(l)]
    }
  );
};

let common_warn_view = (warning: Warning.t) => {
  switch (warning) {
  | WarningPat(UnusedVar(name)) => [
      text("Warning: Variable"),
      code(name),
      text("is unused."),
    ]
  | None => []
  };
};
let common_ok_view =
    (
      ~globals,
      ~show_type_colon=true,
      ~reordered: bool,
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      ~label_sort: bool,
      ~syn_view: option(Typ.t => Node.t)=?,
      ~ana_view: option(Typ.t => Node.t)=?,
      cls: Cls.t,
      ok: Message.ok_common,
    ) => {
  let view_type = view_type(~globals);
  let syn_view = Option.value(syn_view, ~default=view_type);
  let ana_view = Option.value(ana_view, ~default=view_type);
  (
    switch (cls, ok) {
    | (Pat(EmptyHole), _) when label_sort => []
    | (Exp(EmptyHole), _) when label_sort => []
    | (Pat(ExplicitNonlabel), _) when label_sort => [
        text("Explicitly unlabeled entry"),
      ]
    | (Exp(ExplicitNonlabel), _) when label_sort => [
        text("Explicitly unlabeled entry"),
      ]
    | (Exp(MultiHole) | Pat(MultiHole), _) => [
        text("Expecting operator or delimiter"),
      ]
    | (Exp(EmptyHole), Syn(_)) => [text("Fillable by any expression")]
    | (Pat(EmptyHole), Syn(_)) => [text("Fillable by any pattern")]
    | (Exp(EmptyHole), Ana(Consistent({ana, _}))) => [
        text("Fillable by any expression of type"),
        ana_view(ana),
      ]
    | (Pat(EmptyHole), Ana(Consistent({ana, _}))) => [
        text("Fillable by any pattern of type"),
        ana_view(ana),
      ]
    | (_, Syn(syn)) =>
      switch (syn.term) {
      | Label(l) => [label_view(l)]
      | _ => colon_prefix(show_type_colon) @ [syn_view(syn)]
      }
    | (Pat(Var) | Pat(Wild) | Pat(ApFunc), Ana(Consistent({ana, _}))) =>
      /* Pat(ApFunc) is only produced by the `let f(args) = ...` function
         sugar (see FunctionSugar.re), where it denotes the function binder
         as a whole. Render it the same way as a plain variable binder. */
      colon_prefix(show_type_colon) @ [ana_view(ana)]
    | (_, Ana(Consistent({ana, syn, _})))
        when Equality.semantic.typ(ana, syn) =>
      switch (syn.term) {
      | Label(l) => [label_view(l), text(" is a valid label")]
      | _ =>
        colon_prefix(show_type_colon)
        @ [syn_view(syn)]
        @ [text("equals expected type")]
        @ (
          switch (lifted_ty) {
          | None => []
          | Some(lifted) => [text(" lifted to"), view_type(lifted)]
          }
        )
        @ (
          switch (introduced_labels) {
          | [] => []
          | [a] => [text("by automatically adding label "), label_view(a)]
          | _ => [
              text("by automatically adding labels "),
              ...ListUtil.join(
                   text(","),
                   List.map(label_view, introduced_labels),
                 ),
            ]
          }
        )
        @ (
          switch (reordered) {
          | false => []
          | true => [text(" after reordering by labels ")]
          }
        )
      }
    | (_, Ana(Consistent({ana, syn, _}))) =>
      (
        switch (syn.term) {
        | Label(l) => [code(l), text(" is a valid label")]
        | _ =>
          colon_prefix(show_type_colon)
          @ [syn_view(syn), text("consistent with expected type")]
        }
      )
      @ [ana_view(ana)]
      @ (
        switch (lifted_ty) {
        | None => []
        | Some(lifted) => [text(" lifted to"), view_type(lifted)]
        }
      )
      @ (
        switch (introduced_labels) {
        | [] => []
        | [a] => [text("by automatically adding label "), label_view(a)]
        | _ => [
            text("by automatically adding labels "),
            ...ListUtil.join(
                 text(","),
                 List.map(label_view, introduced_labels),
               ),
          ]
        }
      )
      @ (
        switch (reordered) {
        | false => []
        | true => [text(" after reordering by labels ")]
        }
      )
    | (_, Ana(InternallyInconsistent({ana, nomeet: tys}))) =>
      [
        text(elements_noun(cls) ++ " have inconsistent types:"),
        ...ListUtil.join(text(","), List.map(view_type, tys)),
      ]
      @ [text("but consistent with expected"), ana_view(ana)]
    }
  )
  @ (
    switch (inferred_label) {
    | None => []
    | Some(l) => [text(" for label "), label_view(l)]
    }
  );
};

let underdetermined_typ_view =
    (~globals, underdetermined: Message.underdetermined_typ) => {
  let view_type = view_type(~globals);
  switch (underdetermined) {
  | ProdExtensionUnderdetermined(tys) => [
      text("Cannot determine type of product extension with argument types:"),
      ...ListUtil.join(text(","), List.map(view_type, tys)),
    ]
  | ProdProjectionMissingLabel(label, labels) => [
      text("Cannot project label "),
      label_view(label),
      text(". Valid labels are: "),
      ...List.map(code, labels),
    ]
  | ProdProjectionBadArgs({product, label}) =>
    let product_error =
      switch (product) {
      | Some(ty) => [
          text("type"),
          view_type(ty),
          text("is not a tuple type"),
        ]
      | None => []
      };
    let label_error =
      switch (label) {
      | Some(ty) => [
          text("label"),
          view_type(ty),
          text("is not a valid label: "),
        ]
      | None => []
      };

    [text("Cannot determine projected type because ")]
    @ (
      ListUtil.join(
        [text(" and ")],
        [product_error, label_error] |> List.filter(x => x != []),
      )
      |> List.concat
    );
  };
};

let typ_ok_view = (~globals, cls: Cls.t, ok: Message.ok_typ) => {
  let view_type = view_type(~globals);
  switch (ok) {
  | EmptyLabel
  | Default => []
  | Type(_) when cls == Typ(EmptyHole) => [text("Fillable by any type")]
  | Type(ty) =>
    [view_type(ty)]
    @ (
      switch (cls) {
      | Typ(Label) => []
      | _ => [text("is a type")]
      }
    )

  | TypeAlias(name, ty_lookup) => [
      view_type(Var(name) |> Typ.fresh),
      text("is equal to"),
      view_type(ty_lookup),
    ]
  | WHNormalizedTo({unnormalized, whnormalized}) => [
      view_type(unnormalized),
      text("is equal to"),
      view_type(whnormalized),
    ]
  | Kind(kind) => [text("has kind "), code(TypKind.to_string(kind))]
  | Variant(name, sum_ty) => [
      view_type(Var(name) |> Typ.fresh),
      text("is a sum type constuctor of type"),
      view_type(sum_ty),
    ]
  | TypeUnderdetermined(underdetermined) =>
    underdetermined_typ_view(~globals, underdetermined)
  };
};

let typ_mark_err_view = (~globals, m: Mark.t) => {
  let view_type = view_type(~globals);
  switch (m) {
  | TypFreeTypeVariable(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("not found"),
    ]
  | BadToken(token) => [code(token), text("not a type or type operator")]
  | TypKindMismatch({expected, actual}) => [
      text("Expected kind "),
      code(TypKind.to_string(expected)),
      text(", found "),
      code(TypKind.to_string(actual)),
    ]
  | TypParamApplyNonArrowKind(kind) => [
      text("Cannot apply a type of kind "),
      code(TypKind.to_string(kind)),
    ]
  | TypParamApplyArityMismatch({callee, expected, actual, _}) => [
      code(Typ.pretty_print(callee)),
      text(" expects "),
      code(string_of_int(expected)),
      text(" argument" ++ (expected == 1 ? "" : "s") ++ ", got "),
      code(string_of_int(actual)),
    ]
  | TypAbsApplyArityMismatch({expected, actual}) => [
      text("Type abstraction expects "),
      code(string_of_int(expected)),
      text(" type argument" ++ (expected == 1 ? "" : "s") ++ ", got "),
      code(string_of_int(actual)),
    ]
  | TypWantConstructorFoundAp
  | TypWantConstructorFoundType(_) => [text("Expected a constructor")]
  | TypWantTypeFoundAp => [text("Must be part of a sum type")]
  | TypWantLabel => [text("Expect a valid label")]
  | InvalidLabel(name, expected_labels) =>
    switch (expected_labels) {
    | [] => [
        text("Member "),
        label_view(name),
        text(" not found — no members available"),
      ]
    | _ => [
        text("Member "),
        label_view(name),
        text(" not found. Available: "),
        text(String.concat(", ", expected_labels)),
      ]
    }
  | TypDuplicateLabels(labels, _) => [
      text("Duplicate labels within tuple: "),
      ...List.map(label_view, labels),
    ]
  | DuplicateLabel(name, _) => [
      text("Duplicate Label: "),
      label_view(name),
    ]
  | TypDuplicateConstructor(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("already used in this sum"),
    ]
  | TypParseFailure => [text("Parse failure")]
  | TypWantProduct(ty) => [
      text("Expected a tuple type, found type"),
      view_type(ty),
    ]
  | _ => [text("Type error")]
  };
};

let rec automatic_inserted_labels_exp =
        (info: option(Info.exp)): list(string) =>
  switch (Option.bind(info, i => i.label_inference)) {
  | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
  | Some(SingletonLabelInference({label, pre_labeled_info})) =>
    [label] @ automatic_inserted_labels_exp(Some(pre_labeled_info))
  | _ => []
  };

let rec automatic_inserted_labels_pat =
        (info: option(Info.pat)): list(string) =>
  switch (Option.bind(info, i => i.label_inference)) {
  | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
  | Some(SingletonLabelInference({label, pre_labeled_info})) =>
    [label] @ automatic_inserted_labels_pat(Some(pre_labeled_info))
  | _ => []
  };

let exp_mark_err_view =
    (~globals, ~show_type_colon=true, cls: Cls.t, m: Mark.t, info: Info.exp) => {
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      [label] @ automatic_inserted_labels_exp(Some(pre_labeled_info))
    | _ => []
    };
  let lifted_ty =
    switch (info.label_inference) {
    | Some(SingletonLabelInference(_)) => Some(info.ty)
    | _ => None
    };
  let inferred_label = info.inferred_label;
  let view_type = view_type(~globals);
  let view_any = view_any(~globals);
  let ctx = info.ctx;
  let ana = info.ana;
  let common_from_core = () =>
    div_err(
      core_mark_err_view(
        ~globals,
        ~show_type_colon,
        ~introduced_labels,
        ~lifted_ty,
        ~inferred_label,
        ~ctx,
        ~ana,
        cls,
        m,
      ),
    );
  switch (m) {
  | Free(name) => div_err([code(name), text("not found")])
  | InexhaustiveMatch(_, inner_marks, example) =>
    let cls_str = Cls.show(cls);
    let additional =
      switch (
        Mark.highest(inner_marks),
        Statics.ana_skip_explicit_nonlabel(ana).term,
      ) {
      | (Some(NoMeet(PolyEq, tys)), _) => Some(Typ.of_source(tys))
      | (Some(NoMeet(_, tys)), Unknown(SynSwitch)) =>
        Some(Typ.of_source(tys))
      | _ => None
      };
    switch (additional) {
    | None =>
      div_err([
        text(
          cls_str ++ " is inexhaustive. An example of a missing pattern is ",
        ),
        view_any(example),
      ])
    | Some(tys) =>
      let cls_str = String.uncapitalize_ascii(cls_str);
      div_err([
        div_err([
          text(elements_noun(cls) ++ " have inconsistent types:"),
          ...ListUtil.join(text(","), List.map(view_type, tys)),
        ])
        |> code_box_container,
        text(
          "; "
          ++ cls_str
          ++ " is inexhaustive. An example of a missing pattern is ",
        ),
        view_any(example),
      ]);
    };
  | IsDeferral(InAp) =>
    div_err([
      text("(internal) deferral in application is not an error mark"),
    ])
  | IsDeferral(_) =>
    div_err([text("Deferral must appear as a function argument")])
  | IsBadPartialAp(NoDeferredArgs) =>
    div_err([text("Expected at least one non-deferred argument")])
  | IsBadPartialAp(ArityMismatch({expected, actual})) =>
    div_err([
      text(
        "Arity mismatch: expected "
        ++ string_of_int(expected)
        ++ " argument"
        ++ (expected == 1 ? "" : "s")
        ++ ", got "
        ++ string_of_int(actual)
        ++ " arguments",
      ),
    ])
  | BuiltinError(e) =>
    switch (e) {
    | MissingLabels(labels) =>
      div_err([
        text("Labels not present in tuple: "),
        ...List.map(label_view, labels),
      ])
    | ToLvsMissingLabelsOnTuple(_) =>
      div_err([
        text(
          "All entries in the argument must have labels, but some were not provided",
        ),
      ])
    | ProjectLabelsMissingLabels(labels) =>
      div_err([
        text("Projected tuple does not have the following labels: "),
        ...List.map(label_view, labels),
      ])
    | ArgumentMustBeTuple => div_err([text("Argument must be a tuple")])
    | AtLeast2Arguments =>
      div_err([text("Must have 2 or more direct arguments")])
    | Exactly2Arguments =>
      div_err([text("Must have exactly 2 direct arguments")])
    | ArgumentMustBeListOfTuples =>
      div_err([text("First argument must be a list of labeled tuples")])
    | PivotLabelIsNotString(ty) =>
      div_err([
        text("Pivot column must be a string, but got: "),
        view_type(ty),
      ])
    }
  | InvalidUseMode({bad_typ, _}) =>
    div_err([
      text("Cannot use type "),
      view_type(bad_typ) |> code_box_container,
      text(" for number operators and literals."),
    ])
  | BadTrivAp(ty) =>
    div_err([
      text("Function argument type"),
      view_type(ty),
      text("inconsistent with"),
      view_type(Prod([]) |> Typ.fresh),
    ])
  | TupleExtensionRequiresTuples =>
    div_err([text("Tuple extension requires tuple")])
  | DotOperatorRequiresTuple =>
    div_err([text("Requires tuple for first argument")])
  | IsLivelitName({name, _}) =>
    switch (Ctx.lookup_livelit(ctx, name)) {
    | None =>
      div_err([
        text("Livelit with name"),
        code(name),
        text("not found, and also, it's a livelit"),
      ])
    | Some(_) =>
      div_err([text("(internal) livelit should not surface as error")])
    }
  | BadOperator(msg) => div_err([text("Invalid operator: "), text(msg)])
  | LabelNotFound(name, labels) =>
    div_err([
      text("Label "),
      label_view(name),
      text(" not found in tuple's labels: "),
      ...List.map(label_view, labels),
    ])
  | BadLivelitModel(_) => div_err([text("Bad internal livelit model")])
  | BadTheorem(typ) =>
    div_err([
      text("Theorem pattern is not of the form p : t, got "),
      view_type(typ),
    ])
  | TypFreeTypeVariable(_)
  | TypKindMismatch(_)
  | TypParamApplyNonArrowKind(_)
  | TypParamApplyArityMismatch(_)
  | TypDuplicateConstructor(_)
  | TypDuplicateLabels(_, _)
  | TypWantTypeFoundAp
  | TypWantLabel
  | TypWantProduct(_)
  | TypWantConstructorFoundType(_)
  | TypWantConstructorFoundAp
  | TypParseFailure
  | TPatShadowsType(_)
  | TPatNotAVar(_)
  | TPatParamNotAtAliasHead(_) =>
    div_err([text("(internal) typ/tpat mark on expression")])
  | TypAbsApplyArityMismatch({expected, actual}) =>
    div_err([
      text("Type abstraction expects "),
      code(string_of_int(expected)),
      text(" type argument" ++ (expected == 1 ? "" : "s") ++ ", got "),
      code(string_of_int(actual)),
    ])
  | Redundant
  | ExpectedConstructor =>
    div_err([text("(internal) pattern-only mark on expression")])
  | FreeConstructor(_)
  | BadToken(_)
  | BadLabel(_)
  | ExplicitNonlabel
  | UnexpectedLabelSort(_)
  | InvalidLabel(_, _)
  | TupleLabelError(_)
  | IsMulti
  | DuplicateLabel(_, _)
  | DuplicateVar(_, _)
  | ExpectationMismatch(_)
  | NoMeet(_)
  | CompareFun(_) => common_from_core()
  };
};

let exp_view =
    (
      ~globals,
      ~show_type_colon=true,
      ~slicing: option((Model.t, Update.t => Ui_effect.t(unit)))=?,
      cls: Cls.t,
      message: Message.t,
      info: Info.exp,
    ) => {
  let (syn_view, ana_view): (
    option(Typ.t => Node.t),
    option(Typ.t => Node.t),
  ) =
    switch (slicing) {
    | None => (None, None)
    | Some((model, inject)) => (
        Some(type_slot(~globals, ~model, ~inject, Synthesizing)),
        Some(type_slot(~globals, ~model, ~inject, Analyzing)),
      )
    };
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      [label] @ automatic_inserted_labels_exp(Some(pre_labeled_info))
    | _ => []
    };
  let reordered =
    switch (info.label_inference) {
    | Some(MultiLabelInference({reordered, _})) => reordered
    | _ => false
    };
  let lifted_ty =
    switch (info.label_inference) {
    | Some(SingletonLabelInference(_)) => Some(info.ty)
    | _ => None
    };
  let inferred_label = info.inferred_label;
  let marks = info.marks;
  switch (marks != []) {
  | false =>
    switch (message) {
    | Exp(Default) =>
      div_ok(
        common_ok_view(
          ~globals,
          ~show_type_colon,
          ~lifted_ty,
          ~reordered,
          ~introduced_labels,
          ~inferred_label,
          ~label_sort=info.label_sort,
          ~syn_view?,
          ~ana_view?,
          cls,
          Message.Syn(info.elab_syn_ty),
        ),
      )
    | Exp(AnaDeferralConsistent(ana)) =>
      let render = Option.value(ana_view, ~default=view_type(~globals));
      div_ok([text("Expecting type"), render(ana)]);
    | Exp(Common(ok)) =>
      div_ok(
        common_ok_view(
          ~globals,
          ~show_type_colon,
          ~lifted_ty,
          ~reordered,
          ~introduced_labels,
          ~inferred_label,
          ~label_sort=info.label_sort,
          ~syn_view?,
          ~ana_view?,
          cls,
          ok,
        ),
      )
    | Pat(_)
    | TypOk(_)
    | TPatOk(_) =>
      failwith("CursorInspector.exp_view: expected Message.Exp(...)")
    }
  | true =>
    switch (Mark.highest(marks)) {
    | Some(m) => exp_mark_err_view(~globals, ~show_type_colon, cls, m, info)
    | None =>
      div_err([
        text("(internal) expression marks indicate error but no syn mark"),
      ])
    }
  };
};

let pat_marks_err_view =
    (
      ~globals,
      ~show_type_colon=true,
      cls: Cls.t,
      marks: list(Mark.t),
      info: Info.pat,
    ) => {
  let ctx = info.ctx;
  let ana = info.ana;
  let lifted_ty =
    switch (info.label_inference) {
    | Some(SingletonLabelInference(_)) => Some(info.ty)
    | _ => None
    };
  let inferred_label = info.inferred_label;
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      [label] @ automatic_inserted_labels_pat(Some(pre_labeled_info))
    | _ => []
    };

  switch (marks) {
  | [Redundant, ...tl] =>
    let additional = Mark.highest(tl);
    switch (additional) {
    | None => div_err([text("Pattern is redundant")])
    | Some(m) =>
      div_err([
        div_err(
          core_mark_err_view(
            ~globals,
            ~show_type_colon,
            ~inferred_label,
            ~introduced_labels,
            ~lifted_ty,
            ~ctx,
            ~ana,
            cls,
            m,
          ),
        )
        |> code_box_container,
        text("; pattern is redundant"),
      ])
    };
  | [ExpectedConstructor, ..._] => div_err([text("Expected a constructor")])
  | _ =>
    switch (Mark.highest(marks)) {
    | None => div_err([text("(internal) pattern error but no pat syn mark")])
    | Some(m) =>
      div_err(
        core_mark_err_view(
          ~globals,
          ~show_type_colon,
          ~inferred_label,
          ~introduced_labels,
          ~lifted_ty,
          ~ctx,
          ~ana,
          cls,
          m,
        ),
      )
    }
  };
};

let pat_view =
    (
      ~globals,
      ~show_type_colon=true,
      ~slicing: option((Model.t, Update.t => Ui_effect.t(unit)))=?,
      cls: Cls.t,
      message: Message.t,
      info: Info.pat,
    ) => {
  let (syn_view, ana_view): (
    option(Typ.t => Node.t),
    option(Typ.t => Node.t),
  ) =
    switch (slicing) {
    | None => (None, None)
    | Some((model, inject)) => (
        Some(type_slot(~globals, ~model, ~inject, Synthesizing)),
        Some(type_slot(~globals, ~model, ~inject, Analyzing)),
      )
    };
  let lifted_ty =
    switch (info.label_inference) {
    | Some(SingletonLabelInference(_)) => Some(info.ty)
    | _ => None
    };
  let inferred_label = info.inferred_label;
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      [label] @ automatic_inserted_labels_pat(Some(pre_labeled_info))
    | _ => []
    };

  let marks = info.marks;
  marks != []
    ? pat_marks_err_view(~globals, ~show_type_colon, cls, marks, info)
    : {
      let ok =
        switch (message) {
        | Pat(Default) => Message.Syn(info.elab_syn_ty)
        | Pat(Common(ok)) => ok
        | Exp(_)
        | TypOk(_)
        | TPatOk(_) =>
          failwith("CursorInspector.pat_view: expected Message.Pat(...)")
        };
      let ok_view =
        common_ok_view(
          ~globals,
          ~show_type_colon,
          ~lifted_ty,
          ~reordered=
            switch (info.label_inference) {
            | Some(MultiLabelInference({reordered, _})) => reordered
            | _ => false
            },
          ~introduced_labels,
          ~inferred_label,
          ~label_sort=info.label_sort,
          ~syn_view?,
          ~ana_view?,
          cls,
          ok,
        );
      switch (info.warnings) {
      | [Pat(UnusedVar(name))] =>
        if (globals.settings.core.display_warnings) {
          div_warn(common_warn_view(WarningPat(UnusedVar(name))));
        } else {
          div_ok(ok_view);
        }
      | _ => div_ok(ok_view)
      };
    };
};

let typ_view =
    (
      ~globals,
      cls: Cls.t,
      ~marks: list(Mark.t),
      ~message: option(Message.t),
    )
    : Node.t =>
  switch (marks) {
  | [] =>
    switch (message) {
    | Some(TypOk(o)) => div_ok(typ_ok_view(~globals, cls, o))
    | Some(Pat(_) | Exp(_) | TPatOk(_)) =>
      div_err([text("(internal) expected TypOk")])
    | None => div_err([text("(internal) missing type ok payload")])
    }
  | ms =>
    switch (Mark.highest(ms)) {
    | Some(m) => div_err(typ_mark_err_view(~globals, m))
    | None => div_err([text("(internal) missing type mark")])
    }
  };

let tpat_view =
    (~globals, _: Cls.t, ~marks: list(Mark.t), ~message: option(Message.t))
    : Node.t => {
  let view_type = view_type(~globals);
  let kind_view = kind => [
    text("has kind "),
    code(TypKind.to_string(kind)),
  ];
  switch (marks) {
  | [] =>
    switch (message) {
    | Some(TPatOk(Message.Empty)) =>
      div_ok([text("Fillable with a new alias")])
    | Some(TPatOk(Message.Default)) => div_ok([])
    | Some(TPatOk(TypeAlias({name, kind}))) =>
      div_ok([code(name), ...kind_view(kind)])
    | Some(TPatOk(TypeParameter({name, kind}))) =>
      div_ok([
        code(name),
        text("is a type parameter and "),
        ...kind_view(kind),
      ])
    | Some(Pat(_) | Exp(_) | TypOk(_)) =>
      div_err([text("(internal) expected TPatOk")])
    | None => div_err([text("(internal) missing tpat ok payload")])
    }
  | ms =>
    switch (Mark.highest(ms)) {
    | None => div_err([text("(internal) missing type pattern mark")])
    | Some(m) =>
      switch (m) {
      | TPatNotAVar(NotCapitalized) =>
        div_err([text("Must begin with a capital letter")])
      | TPatNotAVar(Other) => div_err([text("Expected an alias")])
      | TPatShadowsType(name, BaseTyp) =>
        div_err([
          text("Can't shadow base type"),
          view_type(Var(name) |> Typ.fresh),
        ])
      | TPatShadowsType(name, TyAlias) =>
        div_err([
          text("Can't shadow existing alias"),
          view_type(Var(name) |> Typ.fresh),
        ])
      | TPatShadowsType(name, TyVar) =>
        div_err([
          text("Can't shadow existing type variable"),
          view_type(Var(name) |> Typ.fresh),
        ])
      | TPatParamNotAtAliasHead(_) =>
        div_err([
          text("This form is only allowed as the head of a type alias"),
        ])
      | _ => div_err([text("Type pattern error")])
      }
    }
  };
};

let secondary_view = (cls: Cls.t) => div_ok([text(cls |> Cls.show)]);

let view_of_info = (~globals, ~model, ~inject, ci): list(Node.t) => {
  let model = Model.refresh_for_info(ci, model);
  let wrapper = status_view => [term_view(~globals, ci), status_view];
  switch (ci) {
  | Secondary(_) => wrapper(div([]))
  | InfoSliceScratch(_) => wrapper(div([]))
  | InfoMod({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
  | InfoSig({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
  | InfoMPat({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
  | InfoExp({cls, message, _} as ie) =>
    wrapper(exp_view(~globals, ~slicing=(model, inject), cls, message, ie))
  | InfoPat({cls, message, _} as ip) =>
    wrapper(pat_view(~globals, ~slicing=(model, inject), cls, message, ip))
  | InfoTyp({cls, marks, message, _}) =>
    wrapper(typ_view(~globals, cls, ~marks, ~message))
  | InfoTPat({cls, marks, message, _}) =>
    wrapper(tpat_view(~globals, cls, ~marks, ~message))
  | InfoDrv(ci) => wrapper(DrvCursorInspector.drv_view(~globals, ci))
  };
};

let inspector_view = (~globals: Globals.t, ~model, ~inject, ci): Node.t =>
  div(
    ~attrs=[
      Attr.id("cursor-inspector"),
      clss([
        Info.is_error(ci)
          ? errc
          : Info.is_warning(ci) && globals.settings.core.display_warnings
              ? warnc : okc,
      ]),
    ],
    view_of_info(~globals, ~model, ~inject, ci),
  );

let view =
    (
      ~globals: Globals.t,
      ~model: Model.t,
      ~inject: Update.t => Ui_effect.t(unit),
      cursor: Cursor.cursor(Editors.Update.t),
    ) => {
  let bar_view = div(~attrs=[Attr.id("bottom-bar")]);
  let err_view = err =>
    bar_view([
      div(
        ~attrs=[Attr.id("cursor-inspector"), clss(["no-info"])],
        [div(~attrs=[clss(["icon"])], [Icons.magnify]), text(err)],
      ),
    ]);
  switch (cursor.info) {
  | _ when !globals.settings.core.statics => div_empty
  | None => err_view("Whitespace or Comment")
  | Some(ci) => bar_view([inspector_view(~globals, ~model, ~inject, ci)])
  };
};
