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

let section_title = (title: string): Node.t =>
  div(~attrs=[clss(["debug-section-title"])], [text(title)]);

let field_node = (label: string, body: Node.t): Node.t =>
  div(
    ~attrs=[clss(["debug-field"])],
    [
      div(~attrs=[clss(["debug-field-label"])], [text(label)]),
      div(~attrs=[clss(["debug-field-value"])], [body]),
    ],
  );

let field_str = (label: string, body: string): Node.t =>
  div(
    ~attrs=[clss(["debug-field"])],
    [
      div(~attrs=[clss(["debug-field-label"])], [text(label)]),
      pre(~attrs=[clss(["debug-field-value", "raw"])], [text(body)]),
    ],
  );

let field_typ = (~globals, ~raw, label: string, typ: Typ.t): Node.t =>
  raw
    ? field_str(label, Typ.show(typ))
    : field_node(
        label,
        CodeViewable.view_typ(~globals, ~settings=code_settings, typ),
      );

let field_any = (~globals, ~raw, label: string, any: Any.t): Node.t =>
  raw
    ? field_str(label, Any.show(any))
    : field_node(
        label,
        CodeViewable.view_any(~globals, ~settings=code_settings, any),
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

let field_ctx = (~globals, ~raw, label: string, ctx: Ctx.t): Node.t =>
  raw
    ? field_str(label, Ctx.show(ctx))
    : field_node(label, ctx_view_rendered(~globals, ctx));

let field_co_ctx = (~globals, ~raw, label: string, co_ctx: CoCtx.t): Node.t =>
  raw
    ? field_str(label, CoCtx.show(co_ctx))
    : field_node(label, co_ctx_view_rendered(~globals, co_ctx));

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

let exp_view = (~globals, ~raw, info: Info.exp): list(Node.t) => [
  section_title("InfoExp"),
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
  field_str("label_inference", label_inference_str(info.label_inference)),
  field_str(
    "inferred_label",
    Option.value(info.inferred_label, ~default="None"),
  ),
  field_str("label_sort", string_of_bool(info.label_sort)),
  field_str(
    "dot_labels",
    "[" ++ String.concat(", ", info.dot_labels) ++ "]",
  ),
];

let pat_view = (~globals, ~raw, info: Info.pat): list(Node.t) => [
  section_title("InfoPat"),
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
  field_str("label_inference", label_inference_str(info.label_inference)),
  field_str(
    "inferred_label",
    Option.value(info.inferred_label, ~default="None"),
  ),
  field_str("label_sort", string_of_bool(info.label_sort)),
];

let typ_view = (~globals, ~raw, info: Info.typ): list(Node.t) => [
  section_title("InfoTyp"),
  field_str("id", id_str(Typ.rep_id(info.user_term))),
  field_str("cls", Cls.show(info.cls)),
  field_str("ancestors", ancestors_str(info.ancestors)),
  field_typ(~globals, ~raw, "user_term", info.user_term),
  field_str("expects", TypExpectation.show(info.expects)),
  field_str("marks", marks_str(info.marks)),
  field_str("warnings", warnings_str(info.warnings)),
  field_ctx(~globals, ~raw, "ctx", info.ctx),
];

let tpat_view = (~globals, ~raw, info: Info.tpat): list(Node.t) => [
  section_title("InfoTPat"),
  field_str("id", id_str(TPat.rep_id(info.user_term))),
  field_str("cls", Cls.show(info.cls)),
  field_str("ancestors", ancestors_str(info.ancestors)),
  field_any(~globals, ~raw, "user_term", TPat(info.user_term)),
  field_str("marks", marks_str(info.marks)),
  field_str("warnings", warnings_str(info.warnings)),
  field_ctx(~globals, ~raw, "ctx", info.ctx),
];

let secondary_view = (s: Info.secondary): list(Node.t) => [
  section_title("Secondary"),
  field_str("id", id_str(s.id)),
  field_str("cls", Cls.show(s.cls)),
  field_str("sort", Sort.show(s.sort)),
];

let mod_view = (~globals, ~raw, m: Info.mod_): list(Node.t) => [
  section_title("InfoMod"),
  field_str("id", id_str(m.id)),
  field_str("cls", Cls.show(m.cls)),
  field_str("sort", Sort.show(m.sort)),
  field_str("ancestors", ancestors_str(m.ancestors)),
  field_ctx(~globals, ~raw, "ctx", m.ctx),
];

let sig_view = (~globals, ~raw, s: Info.sig_): list(Node.t) => [
  section_title("InfoSig"),
  field_str("id", id_str(s.id)),
  field_str("cls", Cls.show(s.cls)),
  field_str("sort", Sort.show(s.sort)),
  field_str("ancestors", ancestors_str(s.ancestors)),
  field_ctx(~globals, ~raw, "ctx", s.ctx),
];

let mpat_view = (~globals, ~raw, m: Info.mpat): list(Node.t) => [
  section_title("InfoMPat"),
  field_str("id", id_str(m.id)),
  field_str("cls", Cls.show(m.cls)),
  field_str("sort", Sort.show(m.sort)),
  field_str("ancestors", ancestors_str(m.ancestors)),
  field_ctx(~globals, ~raw, "ctx", m.ctx),
];

let drv_view = (_: DrvInfo.t): list(Node.t) => [
  section_title("InfoDrv"),
  field_str("(see DrvInfo)", "—"),
];

let info_view = (~globals, ~raw, ci: Info.t): list(Node.t) =>
  switch (ci) {
  | InfoExp(i) => exp_view(~globals, ~raw, i)
  | InfoPat(i) => pat_view(~globals, ~raw, i)
  | InfoTyp(i) => typ_view(~globals, ~raw, i)
  | InfoTPat(i) => tpat_view(~globals, ~raw, i)
  | InfoMod(m) => mod_view(~globals, ~raw, m)
  | InfoSig(s) => sig_view(~globals, ~raw, s)
  | InfoMPat(m) => mpat_view(~globals, ~raw, m)
  | Secondary(s) => secondary_view(s)
  | InfoDrv(d) => drv_view(d)
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
