open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attr=clss([errc]));
let div_ok = div(~attr=clss([okc]));

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

let cls_view = (ci: Info.t) =>
  div(
    ~attr=clss(["syntax-class"]),
    [text(ci |> Info.cls_of |> Term.Cls.show)],
  );

let term_view = (~inject, ~settings: ModelSettings.t, ~show_lang_doc, id, ci) => {
  let sort = ci |> Info.sort_of |> Sort.show;
  let context_menu =
    div(
      ~attr=
        Attr.many([
          Attr.on_click(_ => inject(Update.Set(ContextInspector))),
          clss(["gamma"] @ (settings.context_inspector ? ["visible"] : [])),
        ]),
      [text("Γ")],
    );
  div(
    ~attr=
      clss(
        ["ci-header", "ci-header-" ++ sort]
        @ (Info.is_error(ci) ? [errc] : []),
      ),
    [
      context_menu,
      CtxInspector.inspector_view(~inject, ~settings, id, ci),
      div(~attr=clss(["term-tag"]), [text(sort)]),
      lang_doc_toggle(~inject, ~show_lang_doc),
      cls_view(ci),
    ],
  );
};

let no_type_error = (err: Info.error_no_type) =>
  switch (err) {
  | BadToken(token) =>
    switch (Form.bad_token_cls(token)) {
    | BadInt => "Integer is too large or too small"
    | Other => Printf.sprintf("\"%s\" isn't a valid token", token)
    }
  | FreeConstructor(name) => "'" ++ name ++ "' not found"
  };

let elements_noun: Term.Cls.t => string =
  fun
  | Exp(Match | If) => "Branches"
  | Exp(ListLit)
  | Pat(ListLit) => "Elements"
  | _ => failwith("elements_noun: Cls doesn't have elements");

let common_err_view = (cls: Term.Cls.t, err: Info.error_common) =>
  switch (err) {
  | NoType(err) => [text(no_type_error(err))]
  | Inconsistent(WithArrow(typ)) => [
      Type.view(typ),
      text("is inconsistent with arrow type"),
    ]
  | Inconsistent(Internal(tys)) => [
      text(elements_noun(cls) ++ " have inconsistent types:"),
      ...ListUtil.join(text(","), List.map(Type.view, tys)),
    ]
  | Inconsistent(Expectation({ana, syn})) => [
      text(":"),
      Type.view(syn),
      text("inconsistent with expected type"),
      Type.view(ana),
    ]
  };

let common_ok_view = (cls: Term.Cls.t, ok: Info.ok_pat) => {
  switch (ok) {
  | Syn(_) when cls == Exp(EmptyHole) => [
      text("Fillable by any expression"),
    ]
  | Syn(_) when cls == Pat(EmptyHole) => [text("Fillable by any pattern")]
  | Ana(Consistent({ana, _}))
      when cls == Exp(EmptyHole) || cls == Pat(EmptyHole) => [
      text("Expecting type"),
      Type.view(ana),
    ]
  | Syn(syn) => [text(":"), Type.view(syn)]
  | Ana(Consistent({ana, syn, _})) when ana == syn => [
      text(":"),
      Type.view(syn),
      text("exactly satisfies expected type"),
    ]
  | Ana(Consistent({ana, syn: Unknown(_) as syn, _})) => [
      text(":"),
      Type.view(syn),
      text("trivially satisfies expected type"),
      Type.view(ana),
    ]
  | Ana(Consistent({ana: Unknown(_) as ana, syn, _})) => [
      text(":"),
      Type.view(syn),
      text("satisfies trivial expected type"),
      Type.view(ana),
    ]
  | Ana(Consistent({ana, syn, _})) => [
      text(":"),
      Type.view(syn),
      text("satisfies expected type"),
      Type.view(ana),
    ]
  | Ana(InternallyInconsistent({ana, nojoin: tys})) =>
    [
      text(elements_noun(cls) ++ " have inconsistent types:"),
      ...ListUtil.join(text(","), List.map(Type.view, tys)),
    ]
    @ [text("but these satisfy expected type"), Type.view(ana)]
  };
};

let typ_ok_view = (cls: Term.Cls.t, ok: Info.ok_typ) =>
  switch (ok) {
  | Type(_) when cls == Typ(EmptyHole) => [text("Fillable by any type")]
  | Type(ty) => [Type.view(ty)]
  | TypeAlias(name, ty_lookup) => [
      Type.view(Var(name)),
      text("is a type alias for"),
      Type.view(ty_lookup),
    ]
  | Variant(name, _sum_ty) => [Type.view(Var(name))]
  | VariantIncomplete(_sum_ty) => [text("is incomplete")]
  };

let typ_err_view = (ok: Info.error_typ) =>
  switch (ok) {
  | FreeTypeVariable(name) => [Type.view(Var(name)), text("not found")]
  | BadToken(token) => [
      text("'" ++ token ++ "' isn't a type or type operator"),
    ]
  | WantConstructorFoundAp
  | WantConstructorFoundType(_) => [text("Expected a constructor")]
  | WantTypeFoundAp => [text("Must be part of a sum type")]
  | DuplicateConstructor(name) => [
      Type.view(Var(name)),
      text("already used in this sum"),
    ]
  };

let exp_view: (cls: Term.Cls.t, status: Info.status_exp) =>
  switch (status) {
  | InHole(FreeVariable(name)) =>
    div_err([text("'" ++ name ++ "' not found")])
  | InHole(UnusedDeferral) =>
    div_err([text("Deferral must appear as a function argument")])
  | InHole(ErroneousPartialAp(Meaningless)) =>
    div_err([text("Expected at least one non-deferred argument")])
  | InHole(ErroneousPartialAp(ArityMismatch({expected, actual}))) =>
    div_err([
      text(
        "Arity mismatched partial application: expected "
        ++ string_of_int(expected)
        ++ " argument"
        ++ (expected == 1 ? "" : "s")
        ++ ", got "
        ++ string_of_int(actual),
      ),
    ])
  | InHole(Common(error)) => div_err(common_err_view(cls, error))
  | NotInHole(AnaDeferralConsistent(ana)) =>
    div_ok([text("satisfies expected type"), Type.view(ana)])
  | NotInHole(Common(ok)) => div_ok(common_ok_view(cls, ok))
  };

let pat_view = (cls: Term.Cls.t, status: Info.status_pat) =>
  switch (status) {
  | InHole(ExpectedConstructor) => div_err([text("Expected a constructor")])
  | InHole(Common(error)) => div_err(common_err_view(cls, error))
  | NotInHole(ok) => div_ok(common_ok_view(cls, ok))
  };

let typ_view = (cls: Term.Cls.t, status: Info.status_typ) =>
  switch (status) {
  | NotInHole(ok) => div_ok(typ_ok_view(cls, ok))
  | InHole(err) => div_err(typ_err_view(err))
  };

let tpat_view = (_: Term.Cls.t, status: Info.status_tpat) =>
  switch (status) {
  | NotInHole(Empty) => div_ok([text("Fillable with a new type alias")])
  | NotInHole(Var(name)) => div_ok([Type.alias_view(name)])
  | InHole(NotAVar(NotCapitalized)) =>
    div_err([text("Must begin with a capital letter")])
  | InHole(NotAVar(_)) => div_err([text("Expected a type alias")])
  | InHole(ShadowsType(name)) when Form.is_base_typ(name) =>
    div_err([text("Can't shadow base type"), Type.view(Var(name))])
  | InHole(ShadowsType(name)) =>
    div_err([text("Can't shadow existing alias"), Type.view(Var(name))])
  };

let view_of_info =
    (~inject, ~settings, ~show_lang_doc: bool, id, ci: Statics.Info.t): Node.t => {
  let wrapper = status_view =>
    div(
      ~attr=clss(["info"]),
      [term_view(~inject, ~settings, ~show_lang_doc, id, ci), status_view],
    );
  switch (ci) {
  | InfoExp({cls, status, _}) => wrapper(exp_view(cls, status))
  | InfoPat({cls, status, _}) => wrapper(pat_view(cls, status))
  | InfoTyp({cls, status, _}) => wrapper(typ_view(cls, status))
  | InfoTPat({cls, status, _}) => wrapper(tpat_view(cls, status))
  };
};

let inspector_view = (~inject, ~settings, ~show_lang_doc, id, ci): Node.t =>
  div(
    ~attr=clss(["cursor-inspector"] @ [Info.is_error(ci) ? errc : okc]),
    [view_of_info(~inject, ~settings, ~show_lang_doc, id, ci)],
  );

let view =
    (
      ~inject,
      ~settings: ModelSettings.t,
      ~show_lang_doc: bool,
      zipper: Zipper.t,
      info_map: Statics.Map.t,
    ) => {
  let bar_view = div(~attr=Attr.id("bottom-bar"));
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
        div(~attr=clss(["id"]), [text(string_of_int(id + 1))]),
      ])
    }
  };
};
