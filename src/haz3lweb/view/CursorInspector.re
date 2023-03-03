open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let sort_cls = (ci: Statics.Info.t): string =>
  ci |> Statics.Info.sort_of |> Sort.to_string;

let status_cls: Statics.Info.status_cls => string =
  fun
  | Ok => "ok"
  | Error => "error"
  | Warn => "warn";

let cls_cls = (ci: Statics.Info.t): string =>
  switch (ci) {
  | InfoExp({cls, _}) => Term.UExp.show_cls(cls)
  | InfoPat({cls, _}) => Term.UPat.show_cls(cls)
  | InfoTyp({cls, _}) => Term.UTyp.show_cls(cls)
  | InfoTPat({cls, _}) => Term.UTPat.show_cls(cls)
  };

let status_view = status => div(~attr=clss([status_cls(status)]));
let view_err = status_view(Error);
let view_warn = status_view(Warn);
let view_ok = status_view(Ok);

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

let term_tag = (~inject, ~settings: Model.settings, ~show_lang_doc, ci) =>
  div(
    ~attr=
      clss(["term-tag", sort_cls(ci), status_cls(Info.status_cls(ci))]),
    [
      div(
        ~attr=
          clss(["gamma"] @ (settings.context_inspector ? ["visible"] : [])),
        [text("Γ")],
      ),
      text(sort_cls(ci)),
      lang_doc_toggle(~inject, ~show_lang_doc),
    ],
  );

let common_err_view = (err: Info.error_common) =>
  switch (err) {
  | BadToken(token) => [
      text("is"),
      Type.view(Unknown(ErrorHole)),
      text("given《》"),
      text(Printf.sprintf("as \"%s\" isn't a valid token", token)),
    ]
  | FreeTag => [
      text("is"),
      Type.view(Unknown(ErrorHole)),
      text("given《》"),
      text("as constructor is not defined"),
    ]
  | TypeInconsistent({ana, syn}) => [
      text("is"),
      Type.view(Unknown(ErrorHole)),
      text("expected"),
      Type.view(ana),
      text("given"),
      Type.view(syn),
    ]
  };

let common_ok_view = (ok: Info.ok_pat) => {
  switch (ok) {
  | AnaConsistent({ana, syn, _}) when ana == syn => [
      text("is"),
      Type.view(ana),
    ]
  | AnaConsistent({ana, syn, join}) when ana == join => [
      text("is"),
      Type.view(ana),
      text("given"),
      Type.view(syn),
    ]
  | AnaConsistent({ana, syn, join}) when syn == join => [
      text("is"),
      Type.view(syn),
      text("expected"),
      Type.view(ana),
    ]
  | AnaConsistent({ana, syn, join}) => [
      text("is"),
      Type.view(join),
      text("expected"),
      Type.view(ana),
      text("given"),
      Type.view(syn),
    ]
  | AnaInternalInconsistent({ana, nojoin}) =>
    [
      text("is"),
      Type.view(Unknown(ErrorHole)),
      text("expected"),
      Type.view(ana),
      text("given《"),
    ]
    @ ListUtil.join(text(","), List.map(Type.view, nojoin))
    @ [text("》")]
  };
};

let typ_ok_view = (ok: Info.ok_typ) =>
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
  | Type(ty) => [Type.view(ty), text("is a type")]
  | TypeAlias(name, ty_lookup) => [
      Type.view(Var(name)),
      text("is a type alias for"),
      Type.view(ty_lookup),
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

let exp_view: Info.status_exp => t =
  fun
  | InHole(FreeVariable) =>
    view_err([
      text("is"),
      Type.view(Unknown(ErrorHole)),
      text("given《》"),
      text("as variable is not bound"),
    ])
  | InHole(Common(error)) => view_err(common_err_view(error))
  | NotInHole(ok) => view_ok(common_ok_view(ok));

let pat_view: Info.status_pat => t =
  fun
  | Warning(ok, SoloRefutable) =>
    view_warn(common_ok_view(ok) @ [text("but doesn't cover all cases")])
  | Warning(ok, Shadowing(_id)) =>
    view_warn(common_ok_view(ok) @ [text("but shadows previous binding ")])
  | InHole(Common(error)) => view_err(common_err_view(error))
  | NotInHole(ok) => view_ok(common_ok_view(ok));

let typ_view: Info.status_typ => t =
  fun
  | NotInHole(ok) => view_ok(typ_ok_view(ok))
  | InHole(err) => view_err(typ_err_view(err));

let tpat_view: Info.status_tpat => t =
  fun
  | NotInHole(Empty) => view_ok([text("Enter a new type alias")])
  | NotInHole(Var(name)) =>
    view_ok([Type.alias_view(name), text("is a new type alias")])
  | InHole(NotAVar) => view_err([text("Not a valid type name")]);

let view_of_info_sort: Statics.Info.t => Node.t =
  fun
  | InfoExp({status, _}) => exp_view(status)
  | InfoPat({status, _}) => pat_view(status)
  | InfoTyp({status, _}) => typ_view(status)
  | InfoTPat({status, _}) => tpat_view(status);

let view_of_info =
    (~inject, ~settings, ~show_lang_doc: bool, ci: Statics.Info.t): Node.t =>
  div(
    ~attr=clss(["info", sort_cls(ci)]),
    [
      term_tag(~inject, ~settings, ~show_lang_doc, ci),
      view_of_info_sort(ci),
    ],
  );

let cls_and_id_view = (id: int, ci: Statics.Info.t): Node.t =>
  div(
    ~attr=Attr.many([clss(["id-and-class"])]),
    [
      div(~attr=clss(["syntax-class"]), [text(cls_cls(ci))]),
      div(~attr=clss(["id"]), [text(string_of_int(id + 1))]),
    ],
  );

let inspector_view = (~inject, ~settings, ~show_lang_doc, id, ci): Node.t =>
  div(
    ~attr=
      Attr.many([
        clss(["cursor-inspector", status_cls(Info.status_cls(ci))]),
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
      info_map: Statics.Map.t,
    ) => {
  let bar_view = div_c("bottom-bar");
  let err_view = err =>
    bar_view([
      div(
        ~attr=clss(["cursor-inspector", "no-info"]),
        [div(~attr=clss(["icon"]), [Icons.magnify]), text(err)],
      ),
    ]);
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
