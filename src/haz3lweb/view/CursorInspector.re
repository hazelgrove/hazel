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

let term_tag = (~inject, ~settings: ModelSettings.t, ~show_lang_doc, ci) =>
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

module I = {
  let unk_err = Typ.Unknown(ErrorHole);
  let (t, v, pp) = (text, Type.view, Printf.sprintf);
  let intersperse = (str, tys) => ListUtil.join(t(str), List.map(v, tys));
};

let common_err_view: Info.error_common => list(t) =
  e => {
    let is_wanted =
      I.[t("is"), v(unk_err), t("; wanted"), v(Info.ana_common(e))];
    switch (e) {
    | FreeToken({name, _}) =>
      is_wanted @ I.[t(pp("but got《》as \'%s\' is invalid", name))]
    | FreeTag({name, _}) =>
      is_wanted @ I.[t(pp("but got《》as \'%s\' is undefined", name))]
    | TypeInconsistent({syn, _}) =>
      is_wanted @ I.[t("but got conflicting《"), v(syn), t("》")]
    | TypeDiscordant({nojoin, _}) =>
      is_wanted
      @ I.[t("but got incompatible《"), ...I.intersperse(",", nojoin)]
      @ I.[t("》")]
    };
  };

let common_ok_view: Info.ok_pat => list(t) =
  fun
  | Consistent({ana, syn, join}) =>
    switch () {
    | () when ana == syn => I.[t("is"), v(join)]
    | () when ana == join => I.[t("is"), v(join), t("; got"), v(syn)]
    | () when syn == join => I.[t("is"), v(join), t("; wanted"), v(ana)]
    | () =>
      I.[t("is"), v(join), t("; wanted"), v(ana), t("got"), v(syn)]
    };

let exp_view: Info.status_exp => t =
  fun
  | InHole(FreeVariable({ana, name})) =>
    view_err(
      I.[t("is"), v(unk_err), t("; wanted"), v(ana)]
      @ I.[t(pp("got《》as \'%s\' is unbound", name))],
    )
  | InHole(Common(err)) => view_err(common_err_view(err))
  | NotInHole(ok) => view_ok(common_ok_view(ok));

let pat_view: Info.status_pat => t =
  fun
  | Warning(ok, SoloRefutable) =>
    view_warn(common_ok_view(ok) @ I.[t("; note doesn't cover all cases")])
  | Warning(ok, Shadowing(_id)) =>
    view_warn(common_ok_view(ok) @ I.[t("; note name already used")])
  | InHole(Common(error)) => view_err(common_err_view(error))
  | NotInHole(ok) => view_ok(common_ok_view(ok));

let typ_ok_view = (ok: Info.ok_typ) =>
  switch (ok) {
  | Variant(name, sum_ty) =>
    I.[v(Var(name)), text("is a sum type constuctor of type"), v(sum_ty)]
  | VariantIncomplete(sum_ty) =>
    I.[t("An incomplete sum type constuctor of type"), v(sum_ty)]
  | Type(ty) => I.[v(ty), t("is a type")]
  | TypeAlias(name, ty_lookup) =>
    I.[v(Var(name)), t("is a type alias for"), v(ty_lookup)]
  };

let typ_err_view = (ok: Info.error_typ) =>
  switch (ok) {
  | FreeTypeVar(name) =>
    I.[t("Type variable"), v(Var(name)), t("is unbound")]
  | FreeTypeToken(name) =>
    I.[t(pp("\"%s\" isn't a valid type token", name))]
  | WantTagFoundAp => [text("Expected a constructor, found application")]
  | WantTagFoundType(ty) =>
    I.[t("Expected a constructor, found type "), v(ty)]
  | WantTypeFoundAp => [text("Constructor application must be in sum")]
  | DuplicateTag(name) =>
    I.[t("Constructor"), v(Var(name)), t("already used in this sum")]
  };

let typ_view: Info.status_typ => t =
  fun
  | NotInHole(ok) => view_ok(typ_ok_view(ok))
  | InHole(err) => view_err(typ_err_view(err));

let tpat_view: Info.status_tpat => t =
  fun
  | NotInHole(Empty) => view_ok([text("Enter a new type alias")])
  | NotInHole(Var(name)) =>
    view_ok([Type.alias_view(name), text("is a new type alias")])
  | InHole(NotAVar) => view_err([text("Not a valid type name")])
  | InHole(ShadowsBaseType(name)) =>
    view_err([text("Can't shadow base type"), Type.view(Var(name))]);

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
      ~settings: ModelSettings.t,
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
