open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let errorc = "error";
let happyc = "happy";
let div_error = div(~attr=clss([errorc]));
let div_happy = div(~attr=clss([happyc]));

let cls_str = (ci: Info.t): string =>
  switch (ci) {
  | Invalid(msg) => TermBase.show_parse_flag(msg)
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

let term_tag = (~inject, ~show_lang_doc, is_err, sort) =>
  div(
    ~attr=
      clss(["term-tag", "term-tag-" ++ sort] @ (is_err ? [errorc] : [])),
    [
      div(~attr=clss(["gamma"]), [text("Γ")]),
      text(sort),
      lang_doc_toggle(~inject, ~show_lang_doc),
    ],
  );

let error_view = (err: Info.error_common) =>
  switch (err) {
  | FreeVar => div_error([text("Variable is not bound")])
  | NoFun(typ) =>
    div_error([text("Not consistent with arrow type:"), Type.view(typ)])
  | FreeTag => div_error([text("Constructor is not defined")])
  | SynInconsistentBranches(tys) =>
    div_error([
      text("Expecting branches to have consistent types but got:"),
      ...ListUtil.join(text(","), List.map(Type.view, tys)),
    ])
  | TypeInconsistent({ana, syn}) =>
    div_error([
      text("Expecting"),
      Type.view(ana),
      text("but got"),
      Type.view(syn),
    ])
  };

let happy_view = (suc: Info.happy_common) => {
  switch (suc) {
  | SynConsistent(ty_syn) =>
    div_happy([text("has type"), Type.view(ty_syn)])
  | AnaConsistent({ana, syn, _}) when ana == syn =>
    div_happy([text("has expected & actual type"), Type.view(ana)])
  | AnaConsistent({ana, syn: Unknown(_), _}) =>
    div_happy([text("satisfies expected type"), Type.view(ana)])
  | AnaConsistent({ana, syn, _}) =>
    div_happy([
      text("has type"),
      Type.view(syn),
      text("which is consistent with"),
      Type.view(ana),
    ])
  | AnaInternalInconsistent({ana, nojoin}) =>
    div_happy(
      [
        text("is consistent with"),
        Type.view(ana),
        text("but is internally inconsistent:"),
      ]
      @ ListUtil.join(text(","), List.map(Type.view, nojoin)),
    )
  };
};

let info_common_view = (mode, self, ctx) => {
  let status_common = Info.status_common(ctx, mode, self);
  switch (status_common) {
  | InHole(error) => error_view(error)
  | NotInHole(happy) => happy_view(happy)
  };
};

let info_typ_view = ({ctx, mode, self, _}: Info.info_typ) =>
  switch (Info.status_typ(ctx, mode, self)) {
  | NotInHole(Variant) =>
    div_happy([text("Sum type constuctor definition")])
  | NotInHole(Type(ty)) =>
    div_happy([ty |> Typ.normalize_shallow(ctx) |> Type.view])
  | InHole(FreeTypeVar) => div_error([text("Type Variable is not bound")])
  | InHole(TagExpected) =>
    div_error([text("Expected a constructor, found a type")])
  | InHole(ApOutsideSum) =>
    div_error([text("Constructor application must be in sum")])
  | InHole(DuplicateTag) => div_error([text("Duplicate constructor")])
  };

let info_tpat_view = ({term, _}: Info.info_tpat) =>
  switch (Info.status_tpat(term)) {
  | NotInHole(AVar) => div_happy([text("New type alias")])
  | InHole(NotAVar) => div_error([text("Not a valid type name")])
  };

let view_of_info = (~inject, ~show_lang_doc: bool, ci: Info.t): Node.t => {
  let is_err = Info.is_error(ci);
  let wrapper = (str, status_view) =>
    div(
      ~attr=clss(["info", str]),
      [term_tag(~inject, ~show_lang_doc, is_err, str), status_view],
    );
  switch (ci) {
  | InfoExp({mode, self, ctx, _})
  | InfoPat({mode, self, ctx, _}) =>
    wrapper("pat", info_common_view(mode, self, ctx))
  | InfoTyp(info) => wrapper("typ", info_typ_view(info))
  | InfoTPat(info) => wrapper("tpat", info_tpat_view(info))
  | Invalid(msg) =>
    div(
      ~attr=clss(["info", "unknown"]),
      [text("🚫 " ++ TermBase.show_parse_flag(msg))],
    )
  };
};

let cls_view = (ci: Info.t): Node.t =>
  div(~attr=clss(["syntax-class"]), [text(cls_str(ci))]);

let id_view = (id): Node.t =>
  div(~attr=clss(["id"]), [text(string_of_int(id + 1))]);

let extra_view = (visible: bool, id: int, ci: Info.t): Node.t =>
  div(
    ~attr=Attr.many([clss(["extra"] @ (visible ? ["visible"] : []))]),
    [id_view(id), cls_view(ci)],
  );

let inspector_view =
    (
      ~inject,
      ~settings: Model.settings,
      ~show_lang_doc: bool,
      id: int,
      ci: Info.t,
    )
    : Node.t =>
  div(
    ~attr=
      Attr.many([
        clss(["cursor-inspector"] @ [Info.is_error(ci) ? errorc : happyc]),
        Attr.on_click(_ => inject(Update.Set(ContextInspector))),
      ]),
    [
      extra_view(settings.context_inspector, id, ci),
      view_of_info(~inject, ~show_lang_doc, ci),
      CtxInspector.inspector_view(~inject, ~settings, id, ci),
    ],
  );

let view =
    (
      ~inject,
      ~settings,
      ~show_lang_doc: bool,
      zipper: Zipper.t,
      info_map: Statics.map,
    ) => {
  let backpack = zipper.backpack;
  if (List.length(backpack) > 0) {
    div([]);
  } else {
    let index = Indicated.index(zipper);

    switch (index) {
    | Some(index) =>
      switch (Id.Map.find_opt(index, info_map)) {
      | Some(ci) =>
        inspector_view(~inject, ~settings, ~show_lang_doc, index, ci)
      | None =>
        div(
          ~attr=clss(["cursor-inspector"]),
          [div(~attr=clss(["icon"]), [Icons.magnify]), text("")],
        )
      }
    | None =>
      div(
        ~attr=clss(["cursor-inspector"]),
        [
          div(~attr=clss(["icon"]), [Icons.magnify]),
          text("No Indicated Index"),
        ],
      )
    };
  };
};
