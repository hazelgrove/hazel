open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Language;

/* Debug sidebar panel: dumps the statics Info metadata for the term under the
   cursor. Intended for developers — wired up behind `show_debug_panel`.

   Has two display modes, toggled at the top of the panel:
   - Rendered (default): types and terms are pretty-printed via CodeViewable.
   - Raw: every field is dumped via its derived `show`. */

let code_settings: Haz3lcore.ExpToSegment.Settings.t = {
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
  project_tables: false,
};

/* Same as code_settings but lets the pretty-printer break across lines for
   terms/types that naturally span multiple lines (let/case bodies, large
   records, ...). Used for the standalone term and type fields; context rows
   keep code_settings so each `name : type` row stays on one line. */
let code_settings_ml: Haz3lcore.ExpToSegment.Settings.t = {
  ...code_settings,
  inline: false,
};

/* Pretty-print a type/term to the same text the rendered view displays, by
   printing the segment the code view is built from. Used so the copy button
   matches what's on screen in rendered mode (raw mode copies `show`). */
let typ_to_text = (~settings, typ: Typ.t): string =>
  Haz3lcore.Printer.of_segment(
    Haz3lcore.ExpToSegment.typ_to_segment(~settings, typ),
  );

let any_to_text = (~settings, any: Any.t): string =>
  Haz3lcore.Printer.of_segment(
    Haz3lcore.ExpToSegment.any_to_segment(~settings, any),
  );

/* Copies the raw `show` text to the clipboard. The payload is a thunk so the
   (potentially expensive) `show` only runs on click, not when the field is
   built. Revealed on field hover via CSS so it doesn't clutter the list. */
let copy_button = (payload: unit => string): Node.t =>
  span(
    ~attrs=[
      clss(["debug-copy-button"]),
      Attr.title("Copy raw value to clipboard"),
      Attr.on_click(_ => {
        Util.JsUtil.write_clipboard(payload());
        Virtual_dom.Vdom.Effect.Ignore;
      }),
    ],
    [text({|⧉|})],
  );

/* Label row: optional collapse chevron, the field name, and (when `copy` is
   given) a copy button carrying the raw `show` text. */
let label_row =
    (
      ~chevron: list(Node.t)=[],
      ~copy: option(unit => string)=None,
      label: string,
    )
    : Node.t =>
  div(
    ~attrs=[clss(["debug-field-label"])],
    chevron
    @ [span(~attrs=[clss(["debug-field-name"])], [text(label)])]
    @ (
      switch (copy) {
      | None => []
      | Some(f) => [copy_button(f)]
      }
    ),
  );

/* Collapsible section header; clicking toggles visibility of `fields`. The
   fields are a thunk so a collapsed section builds none of them — and since
   the panel only ever renders the single section under the cursor, that is
   the dominant cost. Collapse state is keyed by the section title. */
let section =
    (~globals: Globals.t, title: string, fields: unit => list(Node.t))
    : list(Node.t) => {
  let collapsed =
    SidebarModel.Settings.is_debug_collapsed(title, globals.settings.sidebar);
  let title_node =
    div(
      ~attrs=[
        clss(["debug-section-title"]),
        Attr.on_click(_ =>
          globals.inject_global(Set(Sidebar(ToggleDebugCollapsed(title))))
        ),
      ],
      [
        span(
          ~attrs=[clss(["debug-section-chevron"])],
          [text(collapsed ? {|▸|} : {|▾|})],
        ),
        text(title),
      ],
    );
  [title_node] @ (collapsed ? [] : fields());
};

let field_node =
    (~copy: option(unit => string)=None, label: string, body: Node.t): Node.t =>
  div(
    ~attrs=[clss(["debug-field"])],
    [
      label_row(~copy, label),
      div(~attrs=[clss(["debug-field-value"])], [body]),
    ],
  );

let field_str = (label: string, body: string): Node.t =>
  div(
    ~attrs=[clss(["debug-field"])],
    [
      label_row(~copy=Some(() => body), label),
      pre(~attrs=[clss(["debug-field-value", "raw"])], [text(body)]),
    ],
  );

/* A collapsible field for heavy values (contexts, term dumps). The body is a
   thunk so a collapsed field renders nothing below its label, and `copy` is a
   thunk so the raw `show` only runs on click. Collapse state is keyed by the
   field label. */
let field_collapsible =
    (
      ~globals: Globals.t,
      ~copy: unit => string,
      ~body: unit => Node.t,
      label: string,
    )
    : Node.t => {
  let collapsed =
    SidebarModel.Settings.is_debug_collapsed(label, globals.settings.sidebar);
  let chevron =
    span(
      ~attrs=[
        clss(["debug-field-chevron"]),
        Attr.on_click(_ =>
          globals.inject_global(Set(Sidebar(ToggleDebugCollapsed(label))))
        ),
      ],
      [text(collapsed ? {|▸|} : {|▾|})],
    );
  div(
    ~attrs=[clss(["debug-field"])],
    [label_row(~chevron=[chevron], ~copy=Some(copy), label)]
    @ (collapsed ? [] : [body()]),
  );
};

let field_typ = (~globals, ~raw, label: string, typ: Typ.t): Node.t =>
  raw
    ? field_str(label, Typ.show(typ))
    : field_node(
        ~copy=Some(() => typ_to_text(~settings=code_settings_ml, typ)),
        label,
        CodeViewable.view_typ(~globals, ~settings=code_settings_ml, typ),
      );

let field_any = (~globals, ~raw, label: string, any: Any.t): Node.t =>
  field_collapsible(
    ~globals,
    ~copy=
      () =>
        raw ? Any.show(any) : any_to_text(~settings=code_settings_ml, any),
    ~body=
      () =>
        raw
          ? pre(
              ~attrs=[clss(["debug-field-value", "raw"])],
              [text(Any.show(any))],
            )
          : div(
              ~attrs=[clss(["debug-field-value"])],
              [
                CodeViewable.view_any(
                  ~globals,
                  ~settings=code_settings_ml,
                  any,
                ),
              ],
            ),
    label,
  );

let id_str = (id: Id.t): string => Id.to_string(id);

let typ_node = (~globals, typ: Typ.t): Node.t =>
  CodeViewable.view_typ(~globals, ~settings=code_settings, typ);

let ctx_row = (label: Node.t, body: option(Node.t)): Node.t =>
  div(
    ~attrs=[clss(["debug-ctx-row"])],
    [div(~attrs=[clss(["debug-ctx-name"])], [label])]
    @ (
      switch (body) {
      | None => []
      | Some(b) => [div(~attrs=[clss(["debug-ctx-body"])], [b])]
      }
    ),
  );

let ctx_entry_node = (~globals, entry: Ctx.entry): Node.t =>
  switch (entry) {
  | VarEntry({name, typ, _}) =>
    ctx_row(text(name ++ " :"), Some(typ_node(~globals, typ)))
  | ConstructorEntry({name, typ, _}) =>
    ctx_row(text("ctor " ++ name ++ " :"), Some(typ_node(~globals, typ)))
  | TVarEntry({name, kind: Singleton(ty), _}) =>
    ctx_row(text("type " ++ name ++ " ="), Some(typ_node(~globals, ty)))
  | TVarEntry({name, kind: Abstract, _}) =>
    ctx_row(text("type " ++ name), None)
  | LivelitEntry(_) => ctx_row(text("livelit"), None)
  };

let ctx_view_rendered = (~globals, ctx: Ctx.t): Node.t =>
  switch (ctx.entries) {
  | [] => div(~attrs=[clss(["debug-ctx-empty"])], [text("(empty)")])
  | entries =>
    div(
      ~attrs=[clss(["debug-ctx"])],
      List.map(ctx_entry_node(~globals), entries),
    )
  };

let co_ctx_entry_node =
    (~globals, name: Var.t, entries: list(CoCtx.entry)): Node.t => {
  let uses =
    List.map(
      (e: CoCtx.entry) =>
        div(
          ~attrs=[clss(["debug-coctx-use"])],
          [text(": "), typ_node(~globals, e.expected_ty)],
        ),
      entries,
    );
  div(
    ~attrs=[clss(["debug-coctx-var"])],
    [div(~attrs=[clss(["debug-ctx-name"])], [text(name)])] @ uses,
  );
};

let co_ctx_view_rendered = (~globals, co_ctx: CoCtx.t): Node.t =>
  switch (co_ctx) {
  | [] => div(~attrs=[clss(["debug-ctx-empty"])], [text("(empty)")])
  | _ =>
    div(
      ~attrs=[clss(["debug-coctx"])],
      List.map(
        ((name, entries)) => co_ctx_entry_node(~globals, name, entries),
        co_ctx,
      ),
    )
  };

/* Text analogs of the rendered ctx/co_ctx layouts, so the copy button matches
   what's displayed in rendered mode (raw mode copies `show`). */
let ctx_entry_text = (entry: Ctx.entry): string =>
  switch (entry) {
  | VarEntry({name, typ, _}) =>
    name ++ " : " ++ typ_to_text(~settings=code_settings, typ)
  | ConstructorEntry({name, typ, _}) =>
    "ctor " ++ name ++ " : " ++ typ_to_text(~settings=code_settings, typ)
  | TVarEntry({name, kind: Singleton(ty), _}) =>
    "type " ++ name ++ " = " ++ typ_to_text(~settings=code_settings, ty)
  | TVarEntry({name, kind: Abstract, _}) => "type " ++ name
  | LivelitEntry(_) => "livelit"
  };

let ctx_to_text = (ctx: Ctx.t): string =>
  switch (ctx.entries) {
  | [] => "(empty)"
  | entries => String.concat("\n", List.map(ctx_entry_text, entries))
  };

let co_ctx_to_text = (co_ctx: CoCtx.t): string =>
  switch (co_ctx) {
  | [] => "(empty)"
  | _ =>
    String.concat(
      "\n",
      List.map(
        ((name, entries)) =>
          name
          ++ String.concat(
               "",
               List.map(
                 (e: CoCtx.entry) =>
                   "\n  : "
                   ++ typ_to_text(~settings=code_settings, e.expected_ty),
                 entries,
               ),
             ),
        co_ctx,
      ),
    )
  };

let field_ctx = (~globals, ~raw, label: string, ctx: Ctx.t): Node.t =>
  field_collapsible(
    ~globals,
    ~copy=() => raw ? Ctx.show(ctx) : ctx_to_text(ctx),
    ~body=
      () =>
        raw
          ? pre(
              ~attrs=[clss(["debug-field-value", "raw"])],
              [text(Ctx.show(ctx))],
            )
          : div(
              ~attrs=[clss(["debug-field-value"])],
              [ctx_view_rendered(~globals, ctx)],
            ),
    label,
  );

let field_co_ctx = (~globals, ~raw, label: string, co_ctx: CoCtx.t): Node.t =>
  field_collapsible(
    ~globals,
    ~copy=() => raw ? CoCtx.show(co_ctx) : co_ctx_to_text(co_ctx),
    ~body=
      () =>
        raw
          ? pre(
              ~attrs=[clss(["debug-field-value", "raw"])],
              [text(CoCtx.show(co_ctx))],
            )
          : div(
              ~attrs=[clss(["debug-field-value"])],
              [co_ctx_view_rendered(~globals, co_ctx)],
            ),
    label,
  );

let ancestors_str = (ancestors: Info.ancestors): string =>
  switch (ancestors) {
  | [] => "[]"
  | _ => "[" ++ String.concat(", ", List.map(id_str, ancestors)) ++ "]"
  };

let marks_str = (marks: list(Mark.t)): string =>
  switch (marks) {
  | [] => "[]"
  | _ =>
    "[\n  " ++ String.concat(",\n  ", List.map(Mark.show, marks)) ++ "\n]"
  };

let warnings_str = (warnings: list(Warning.list_item)): string =>
  switch (warnings) {
  | [] => "[]"
  | _ =>
    "[\n  "
    ++ String.concat(",\n  ", List.map(Warning.show_list_item, warnings))
    ++ "\n]"
  };

let label_inference_str = (li: option(Info.label_inference(_))): string =>
  switch (li) {
  | None => "None"
  | Some(SingletonLabelInference({label, _})) =>
    "SingletonLabelInference(" ++ label ++ ")"
  | Some(MultiLabelInference({reordered, introduced_labels})) =>
    Printf.sprintf(
      "MultiLabelInference(reordered=%b, introduced=[%s])",
      reordered,
      String.concat(", ", introduced_labels),
    )
  };

let exp_view = (~globals, ~raw, info: Info.exp): list(Node.t) =>
  section(~globals, "InfoExp", () =>
    [
      field_str("id", id_str(Exp.rep_id(info.user_term))),
      field_str("cls", Cls.show(info.cls)),
      field_str("ancestors", ancestors_str(info.ancestors)),
      field_any(~globals, ~raw, "user_term", Exp(info.user_term)),
      field_any(~globals, ~raw, "elab_term", Exp(info.elab_term)),
      field_typ(~globals, ~raw, "ana", info.ana),
      field_typ(~globals, ~raw, "elab_syn_ty", info.elab_syn_ty),
      field_typ(~globals, ~raw, "ty (post-fix)", info.ty),
      field_str("marks", marks_str(info.marks)),
      field_str("warnings", warnings_str(info.warnings)),
      field_ctx(~globals, ~raw, "ctx", info.ctx),
      field_co_ctx(~globals, ~raw, "co_ctx", info.co_ctx),
      field_str(
        "label_inference",
        label_inference_str(info.label_inference),
      ),
      field_str(
        "inferred_label",
        Option.value(info.inferred_label, ~default="None"),
      ),
      field_str("label_sort", string_of_bool(info.label_sort)),
      field_str(
        "dot_labels",
        "[" ++ String.concat(", ", info.dot_labels) ++ "]",
      ),
    ]
  );

let pat_view = (~globals, ~raw, info: Info.pat): list(Node.t) =>
  section(~globals, "InfoPat", () =>
    [
      field_str("id", id_str(Pat.rep_id(info.user_term))),
      field_str("cls", Cls.show(info.cls)),
      field_str("ancestors", ancestors_str(info.ancestors)),
      field_any(~globals, ~raw, "user_term", Pat(info.user_term)),
      field_any(~globals, ~raw, "elab_term", Pat(info.elab_term)),
      field_typ(~globals, ~raw, "ana", info.ana),
      field_typ(~globals, ~raw, "elab_syn_ty", info.elab_syn_ty),
      field_typ(~globals, ~raw, "ty (post-fix)", info.ty),
      field_str("marks", marks_str(info.marks)),
      field_str("warnings", warnings_str(info.warnings)),
      field_ctx(~globals, ~raw, "ctx", info.ctx),
      field_co_ctx(~globals, ~raw, "co_ctx", info.co_ctx),
      field_str("constraint_", Coverage.Constraint.show(info.constraint_)),
      field_str(
        "label_inference",
        label_inference_str(info.label_inference),
      ),
      field_str(
        "inferred_label",
        Option.value(info.inferred_label, ~default="None"),
      ),
      field_str("label_sort", string_of_bool(info.label_sort)),
    ]
  );

let typ_view = (~globals, ~raw, info: Info.typ): list(Node.t) =>
  section(~globals, "InfoTyp", () =>
    [
      field_str("id", id_str(Typ.rep_id(info.user_term))),
      field_str("cls", Cls.show(info.cls)),
      field_str("ancestors", ancestors_str(info.ancestors)),
      field_typ(~globals, ~raw, "user_term", info.user_term),
      field_str("expects", TypExpectation.show(info.expects)),
      field_str("marks", marks_str(info.marks)),
      field_str("warnings", warnings_str(info.warnings)),
      field_ctx(~globals, ~raw, "ctx", info.ctx),
    ]
  );

let tpat_view = (~globals, ~raw, info: Info.tpat): list(Node.t) =>
  section(~globals, "InfoTPat", () =>
    [
      field_str("id", id_str(TPat.rep_id(info.user_term))),
      field_str("cls", Cls.show(info.cls)),
      field_str("ancestors", ancestors_str(info.ancestors)),
      field_any(~globals, ~raw, "user_term", TPat(info.user_term)),
      field_str("marks", marks_str(info.marks)),
      field_str("warnings", warnings_str(info.warnings)),
      field_ctx(~globals, ~raw, "ctx", info.ctx),
    ]
  );

let secondary_view = (~globals, s: Info.secondary): list(Node.t) =>
  section(~globals, "Secondary", () =>
    [
      field_str("id", id_str(s.id)),
      field_str("cls", Cls.show(s.cls)),
      field_str("sort", Sort.show(s.sort)),
    ]
  );

let mod_view = (~globals, ~raw, m: Info.mod_): list(Node.t) =>
  section(~globals, "InfoMod", () =>
    [
      field_str("id", id_str(m.id)),
      field_str("cls", Cls.show(m.cls)),
      field_str("sort", Sort.show(m.sort)),
      field_str("ancestors", ancestors_str(m.ancestors)),
      field_ctx(~globals, ~raw, "ctx", m.ctx),
    ]
  );

let sig_view = (~globals, ~raw, s: Info.sig_): list(Node.t) =>
  section(~globals, "InfoSig", () =>
    [
      field_str("id", id_str(s.id)),
      field_str("cls", Cls.show(s.cls)),
      field_str("sort", Sort.show(s.sort)),
      field_str("ancestors", ancestors_str(s.ancestors)),
      field_ctx(~globals, ~raw, "ctx", s.ctx),
    ]
  );

let mpat_view = (~globals, ~raw, m: Info.mpat): list(Node.t) =>
  section(~globals, "InfoMPat", () =>
    [
      field_str("id", id_str(m.id)),
      field_str("cls", Cls.show(m.cls)),
      field_str("sort", Sort.show(m.sort)),
      field_str("ancestors", ancestors_str(m.ancestors)),
      field_ctx(~globals, ~raw, "ctx", m.ctx),
    ]
  );

let drv_view = (~globals, _: DrvInfo.t): list(Node.t) =>
  section(~globals, "InfoDrv", () => [field_str("(see DrvInfo)", "—")]);

let info_view = (~globals, ~raw, ci: Info.t): list(Node.t) =>
  switch (ci) {
  | InfoExp(i) => exp_view(~globals, ~raw, i)
  | InfoPat(i) => pat_view(~globals, ~raw, i)
  | InfoTyp(i) => typ_view(~globals, ~raw, i)
  | InfoTPat(i) => tpat_view(~globals, ~raw, i)
  | InfoMod(m) => mod_view(~globals, ~raw, m)
  | InfoSig(s) => sig_view(~globals, ~raw, s)
  | InfoMPat(m) => mpat_view(~globals, ~raw, m)
  | Secondary(s) => secondary_view(~globals, s)
  | InfoDrv(d) => drv_view(~globals, d)
  };

let toggle_bar = (~globals: Globals.t): Node.t => {
  let raw = globals.settings.sidebar.debug_show_raw;
  let on_click = _ => globals.inject_global(Set(Sidebar(ToggleDebugRaw)));
  div(
    ~attrs=[clss(["debug-toggle-bar"])],
    [
      div(
        ~attrs=[clss(["debug-toggle-label"])],
        [text(raw ? "Showing raw internal structure" : "Showing rendered")],
      ),
      div(
        ~attrs=[
          clss(["debug-toggle-button"]),
          Attr.on_click(on_click),
          Attr.title("Toggle between rendered code and raw `show` output"),
        ],
        [text(raw ? "Show rendered" : "Show raw")],
      ),
    ],
  );
};

let view = (~globals: Globals.t, ~cursor: Cursor.cursor(_)): Node.t => {
  let raw = globals.settings.sidebar.debug_show_raw;
  div(
    ~attrs=[Attr.id("debug-sidebar"), clss(["panel"])],
    [
      div(
        ~attrs=[clss(["panel-title-bar"])],
        [text("Debug — Cursor Info")],
      ),
      toggle_bar(~globals),
      div(
        ~attrs=[clss(["panel-body"])],
        switch (cursor.info) {
        | None => [text("No info at cursor.")]
        | Some(ci) => info_view(~globals, ~raw, ci)
        },
      ),
    ],
  );
};
