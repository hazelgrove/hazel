open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attr=clss([errc]));
let div_ok = div(~attr=clss([okc]));

let cls_str = (ci: Info.t): string =>
  switch (ci) {
  | InfoExp({cls, _}) => Term.UExp.show_cls(cls)
  | InfoPat({cls, _}) => Term.UPat.show_cls(cls)
  | InfoTyp({cls, _}) => Term.UTyp.show_cls(cls)
  | InfoTPat({cls, _}) => Term.UTPat.show_cls(cls)
  };

let lang_doc_toggle = (~inject, ~show_lang_doc) => {
  let tooltip = "Toggle language documentation";
  let toggle_landocs = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Update.UpdateLangDocMessages(LangDocMessages.ToggleShow)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attr=clss(["lang-doc-button"]),
    [Widgets.toggle(~tooltip, "i", show_lang_doc, toggle_landocs)],
  );
};

let term_tag =
    (~inject, ~settings: Model.settings, ~show_lang_doc, is_err, sort) =>
  div(
    ~attr=clss(["term-tag", "term-tag-" ++ sort] @ (is_err ? [errc] : [])),
    [
      div(
        ~attr=
          clss(["gamma"] @ (settings.context_inspector ? ["visible"] : [])),
        [text("Γ")],
      ),
      text(sort),
      lang_doc_toggle(~inject, ~show_lang_doc),
    ],
  );

let common_err_view = (err: Info.error_common) =>
  switch (err) {
  | FreeVar => [text("Variable is not bound")]
  | BadToken(token) => [
      text(Printf.sprintf("\"%s\" isn't a valid token", token)),
    ]
  | NoFun(typ) => [
      Type.view(typ),
      text("is not consistent with arrow type"),
    ]
  | FreeTag => [text("Constructor is not defined")]
  | SynInconsistentBranches(tys) => [
      text("Expecting branches to have consistent types but got:"),
      ...ListUtil.join(text(","), List.map(Type.view, tys)),
    ]
  | TypeInconsistent({ana, syn}) => [
      text("Expecting"),
      Type.view(ana),
      text("but got"),
      Type.view(syn),
    ]
  };

let common_ok_view = (ok: Info.ok_common) => {
  switch (ok) {
  | SynConsistent(ty_syn) => [text("has type"), Type.view(ty_syn)]
  | AnaConsistent({ana, syn, _}) when ana == syn => [
      text("has expected & actual type"),
      Type.view(ana),
    ]
  | AnaConsistent({ana, syn: Unknown(_), _}) => [
      text("satisfies expected type"),
      Type.view(ana),
    ]
  | AnaConsistent({ana, syn, _}) => [
      text("has type"),
      Type.view(syn),
      text("which is consistent with"),
      Type.view(ana),
    ]
  | AnaInternalInconsistent({ana, nojoin}) =>
    [
      text("is consistent with"),
      Type.view(ana),
      text("but is internally inconsistent:"),
    ]
    @ ListUtil.join(text(","), List.map(Type.view, nojoin))
  };
};

let info_common_view = (mode, self, ctx) => {
  let status_common = Info.status_common(ctx, mode, self);
  switch (status_common) {
  | InHole(error) => div_err(common_err_view(error))
  | NotInHole(ok) => div_ok(common_ok_view(ok))
  };
};

let typ_ok_view = (ok: Info.ok_typ, ctx: Ctx.t, ty: Typ.t) =>
  switch (ok) {
  | Variant(name, sum_ty) => [
      Type.view(Var(name)),
      text("is a sum type constuctor of type"),
      Type.view(sum_ty),
    ]
  | VariantIncomplete(sum_ty) => [
      text("An incomplete sum type constuctor of type"),
      Type.view(sum_ty),
    ]
  | Type => [Type.view(ty), text("is a type")]
  | TypeAlias(name) => [
      Type.view(Var(name)),
      text("is a type alias for"),
      Type.view(Typ.normalize_shallow(ctx, ty)),
    ]
  };

let typ_err_view = (ok: Info.error_typ) =>
  switch (ok) {
  | FreeTypeVar(name) => [
      text("Type variable"),
      Type.view(Var(name)),
      text("is not bound"),
    ]
  | BadToken(token) => [
      text(Printf.sprintf("\"%s\" isn't a valid type token", token)),
    ]
  | WantTagFoundAp => [text("Expected a constructor, found application")]
  | WantTagFoundType(ty) => [
      text("Expected a constructor, found type "),
      Type.view(ty),
    ]
  | WantTypeFoundAp => [text("Constructor application must be in sum")]
  | DuplicateTag(name) => [
      text("Constructor"),
      Type.view(Var(name)),
      text("already used in this sum"),
    ]
  };

let info_typ_view = ({ctx, mode, term, ty, _}: Info.info_typ) =>
  switch (Info.status_typ(ctx, mode, term)) {
  | NotInHole(ok) => div_ok(typ_ok_view(ok, ctx, ty))
  | InHole(err) => div_err(typ_err_view(err))
  };

let info_tpat_view = ({term, _}: Info.info_tpat) =>
  switch (Info.status_tpat(term)) {
  | NotInHole(Empty) => div_ok([text("Enter a new type alias")])
  | NotInHole(Var(name)) =>
    div_ok([Type.alias_view(name), text("is a new type alias")])
  | InHole(NotAVar) => div_err([text("Not a valid type name")])
  };

let view_of_info =
    (~inject, ~settings, ~show_lang_doc: bool, ci: Info.t): Node.t => {
  let is_err = Info.is_error(ci);
  let wrapper = (sort, status_view) =>
    div(
      ~attr=clss(["info", sort]),
      [
        term_tag(~inject, ~settings, ~show_lang_doc, is_err, sort),
        status_view,
      ],
    );
  switch (ci) {
  | InfoExp({mode, self, ctx, _}) =>
    wrapper("exp", info_common_view(mode, self, ctx))
  | InfoPat({mode, self, ctx, _}) =>
    wrapper("pat", info_common_view(mode, self, ctx))
  | InfoTyp(info) => wrapper("typ", info_typ_view(info))
  | InfoTPat(info) => wrapper("tpat", info_tpat_view(info))
  };
};

let cls_view = (ci: Info.t): Node.t =>
  div(~attr=clss(["syntax-class"]), [text(cls_str(ci))]);

let id_view = (id: Id.t): Node.t =>
  div(~attr=clss(["id"]), [text(string_of_int(id + 1))]);

let cls_and_id_view = (id: int, ci: Info.t): Node.t =>
  div(
    ~attr=Attr.many([clss(["id-and-class"])]),
    [cls_view(ci), id_view(id)],
  );

let inspector_view = (~inject, ~settings, ~show_lang_doc, id, ci): Node.t =>
  div(
    ~attr=
      Attr.many([
        clss(["cursor-inspector"] @ [Info.is_error(ci) ? errc : okc]),
        Attr.on_click(_ => inject(Update.Set(ContextInspector))),
      ]),
    [
      view_of_info(~inject, ~settings, ~show_lang_doc, ci),
      CtxInspector.inspector_view(~inject, ~settings, id, ci),
    ],
  );

let view =
    (
      ~inject,
      ~settings: Model.settings,
      ~show_lang_doc: bool,
      zipper: Zipper.t,
      info_map: Statics.map,
    ) => {
  let bar_view = div_c("bottom-bar");
  let err_view' = err =>
    div(
      ~attr=clss(["cursor-inspector", "no-info"]),
      [div(~attr=clss(["icon"]), [Icons.magnify]), text(err)],
    );
  let err_view = err => bar_view([err_view'(err)]);
  switch (zipper.backpack, Indicated.index(zipper)) {
  | ([_, ..._], _) => err_view("No information while backpack in use")
  | (_, None) => err_view("No cursor in program")
  | (_, Some(id)) =>
    switch (Id.Map.find_opt(id, info_map)) {
    | None => err_view("Whitespace or Comment")
    | Some(ci) =>
      bar_view([
        inspector_view(~inject, ~settings, ~show_lang_doc, id, ci),
        cls_and_id_view(id, ci),
      ])
    }
  };
};
