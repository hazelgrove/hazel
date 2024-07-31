open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attrs=[clss([errc])]);
let div_ok = div(~attrs=[clss([okc])]);

let code_err = (code: string): Node.t =>
  div(~attrs=[clss(["code"])], [text(code)]);

let explain_this_toggle = (~globals: Globals.t): Node.t => {
  let tooltip = "Toggle language documentation";
  let toggle_explain_this = _ =>
    Virtual_dom.Vdom.Effect.Many([
      globals.inject_global(Set(ExplainThis(ToggleShow))),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attrs=[clss(["explain-this-button"])],
    [
      Widgets.toggle(
        ~tooltip,
        "?",
        globals.settings.explainThis.show,
        toggle_explain_this,
      ),
    ],
  );
};

let cls_view = (ci: Info.t): Node.t =>
  div(
    ~attrs=[clss(["syntax-class"])],
    [text(ci |> Info.cls_of |> Cls.show)],
  );

let ctx_toggle = (~globals: Globals.t): Node.t =>
  div(
    ~attrs=[
      Attr.on_click(_ => globals.inject_global(Set(ContextInspector))),
      clss(
        ["gamma"] @ (globals.settings.context_inspector ? ["visible"] : []),
      ),
    ],
    [text("Γ")],
  );

let term_view = (~globals: Globals.t, ci) => {
  let sort = ci |> Info.sort_of |> Sort.show;
  div(
    ~attrs=[
      clss(["ci-header", sort] @ (Info.is_error(ci) ? [errc] : [])),
    ],
    [
      ctx_toggle(~globals),
      CtxInspector.view(~globals, ci),
      div(~attrs=[clss(["term-tag"])], [text(sort)]),
      explain_this_toggle(~globals),
      cls_view(ci),
    ],
  );
};

let elements_noun: Cls.t => string =
  fun
  | Exp(Match | If) => "Branches"
  | Exp(ListLit)
  | Pat(ListLit) => "Elements"
  | Exp(ListConcat) => "Operands"
  | _ => failwith("elements_noun: Cls doesn't have elements");

let common_err_view = (~globals, cls: Cls.t, err: Info.error_common) => {
  let view_type = CodeViewable.view_typ(~globals, ~inline=true);
  switch (err) {
  | NoType(BadToken(token)) =>
    switch (Form.bad_token_cls(token)) {
    | BadInt => [text("Integer is too large or too small")]
    | Other => [text(Printf.sprintf("\"%s\" isn't a valid token", token))]
    }
  | NoType(BadTrivAp(ty)) => [
      text("Function argument type"),
      view_type(ty),
      text("inconsistent with"),
      view_type(Prod([]) |> Typ.fresh),
    ]
  | NoType(FreeConstructor(name)) => [code_err(name), text("not found")]
  | Inconsistent(WithArrow(typ)) => [
      text(":"),
      view_type(typ),
      text("inconsistent with arrow type"),
    ]
  | Inconsistent(Expectation({ana, syn})) => [
      text(":"),
      view_type(syn),
      text("inconsistent with expected type"),
      view_type(ana),
    ]
  | Inconsistent(Internal(tys)) => [
      text(elements_noun(cls) ++ " have inconsistent types:"),
      ...ListUtil.join(text(","), List.map(view_type, tys)),
    ]
  };
};

let common_ok_view = (~globals, cls: Cls.t, ok: Info.ok_pat) => {
  let view_type = CodeViewable.view_typ(~globals, ~inline=true);
  switch (cls, ok) {
  | (Exp(MultiHole) | Pat(MultiHole), _) => [
      text("Expecting operator or delimiter"),
    ]
  | (Exp(EmptyHole), Syn(_)) => [text("Fillable by any expression")]
  | (Pat(EmptyHole), Syn(_)) => [text("Fillable by any pattern")]
  | (Exp(EmptyHole), Ana(Consistent({ana, _}))) => [
      text("Fillable by any expression of type"),
      view_type(ana),
    ]
  | (Pat(EmptyHole), Ana(Consistent({ana, _}))) => [
      text("Fillable by any pattern of type"),
      view_type(ana),
    ]
  | (_, Syn(syn)) => [text(":"), view_type(syn)]
  | (Pat(Var) | Pat(Wild), Ana(Consistent({ana, _}))) => [
      text(":"),
      view_type(ana),
    ]
  | (_, Ana(Consistent({ana, syn, _}))) when ana == syn => [
      text(":"),
      view_type(syn),
      text("equals expected type"),
    ]
  | (_, Ana(Consistent({ana, syn, _}))) => [
      text(":"),
      view_type(syn),
      text("consistent with expected type"),
      view_type(ana),
    ]
  | (_, Ana(InternallyInconsistent({ana, nojoin: tys}))) =>
    [
      text(elements_noun(cls) ++ " have inconsistent types:"),
      ...ListUtil.join(text(","), List.map(view_type, tys)),
    ]
    @ [text("but consistent with expected"), view_type(ana)]
  };
};

let typ_ok_view = (~globals, cls: Cls.t, ok: Info.ok_typ) => {
  let view_type = CodeViewable.view_typ(~globals, ~inline=true);
  switch (ok) {
  | Type(_) when cls == Typ(EmptyHole) => [text("Fillable by any type")]
  | Type(ty) => [view_type(ty), text("is a type")]
  | TypeAlias(name, ty_lookup) => [
      view_type(Var(name) |> Typ.fresh),
      text("is an alias for"),
      view_type(ty_lookup),
    ]
  | Variant(name, sum_ty) => [
      view_type(Var(name) |> Typ.fresh),
      text("is a sum type constuctor of type"),
      view_type(sum_ty),
    ]
  | VariantIncomplete(sum_ty) => [
      text("An incomplete sum type constuctor of type"),
      view_type(sum_ty),
    ]
  };
};

let typ_err_view = (~globals, ok: Info.error_typ) => {
  let view_type = CodeViewable.view_typ(~globals, ~inline=true);
  switch (ok) {
  | FreeTypeVariable(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("not found"),
    ]
  | BadToken(token) => [
      code_err(token),
      text("not a type or type operator"),
    ]
  | WantConstructorFoundAp
  | WantConstructorFoundType(_) => [text("Expected a constructor")]
  | WantTypeFoundAp => [text("Must be part of a sum type")]
  | DuplicateConstructor(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("already used in this sum"),
    ]
  };
};

let rec exp_view = (~globals, cls: Cls.t, status: Info.status_exp) => {
  let view_type = CodeViewable.view_typ(~globals, ~inline=true);
  switch (status) {
  | InHole(FreeVariable(name)) =>
    div_err([code_err(name), text("not found")])
  | InHole(InexhaustiveMatch(additional_err)) =>
    let cls_str = Cls.show(cls);
    switch (additional_err) {
    | None => div_err([text(cls_str ++ " is inexhaustive")])
    | Some(err) =>
      let cls_str = String.uncapitalize_ascii(cls_str);
      div_err([
        exp_view(~globals, cls, InHole(Common(err))),
        text("; " ++ cls_str ++ " is inexhaustive"),
      ]);
    };
  | InHole(UnusedDeferral) =>
    div_err([text("Deferral must appear as a function argument")])
  | InHole(BadPartialAp(NoDeferredArgs)) =>
    div_err([text("Expected at least one non-deferred argument")])
  | InHole(BadPartialAp(ArityMismatch({expected, actual}))) =>
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
  | InHole(Common(error)) => div_err(common_err_view(~globals, cls, error))
  | NotInHole(AnaDeferralConsistent(ana)) =>
    div_ok([text("Expecting type"), view_type(ana)])
  | NotInHole(Common(ok)) => div_ok(common_ok_view(~globals, cls, ok))
  };
};

let rec pat_view = (~globals, cls: Cls.t, status: Info.status_pat) =>
  switch (status) {
  | InHole(ExpectedConstructor) => div_err([text("Expected a constructor")])
  | InHole(Redundant(additional_err)) =>
    switch (additional_err) {
    | None => div_err([text("Pattern is redundant")])
    | Some(err) =>
      div_err([
        pat_view(~globals, cls, InHole(err)),
        text("; pattern is redundant"),
      ])
    }
  | InHole(Common(error)) => div_err(common_err_view(~globals, cls, error))
  | NotInHole(ok) => div_ok(common_ok_view(~globals, cls, ok))
  };

let typ_view = (~globals, cls: Cls.t, status: Info.status_typ) =>
  switch (status) {
  | NotInHole(ok) => div_ok(typ_ok_view(~globals, cls, ok))
  | InHole(err) => div_err(typ_err_view(~globals, err))
  };

let tpat_view = (~globals, _: Cls.t, status: Info.status_tpat) => {
  let view_type = CodeViewable.view_typ(~globals, ~inline=true);
  switch (status) {
  | NotInHole(Empty) => div_ok([text("Fillable with a new alias")])
  | NotInHole(Var(name)) => div_ok([CtxInspector.alias_view(name)])
  | InHole(NotAVar(NotCapitalized)) =>
    div_err([text("Must begin with a capital letter")])
  | InHole(NotAVar(_)) => div_err([text("Expected an alias")])
  | InHole(ShadowsType(name, BaseTyp)) =>
    div_err([
      text("Can't shadow base type"),
      view_type(Var(name) |> Typ.fresh),
    ])
  | InHole(ShadowsType(name, TyAlias)) =>
    div_err([
      text("Can't shadow existing alias"),
      view_type(Var(name) |> Typ.fresh),
    ])
  | InHole(ShadowsType(name, TyVar)) =>
    div_err([
      text("Can't shadow existing type variable"),
      view_type(Var(name) |> Typ.fresh),
    ])
  };
};

let secondary_view = (cls: Cls.t) => div_ok([text(cls |> Cls.show)]);

let view_of_info = (~globals, ci): Node.t => {
  let wrapper = status_view =>
    div(~attrs=[clss(["info"])], [term_view(~globals, ci), status_view]);
  switch (ci) {
  | Secondary(_) => wrapper(div([]))
  | InfoExp({cls, status, _}) => wrapper(exp_view(~globals, cls, status))
  | InfoPat({cls, status, _}) => wrapper(pat_view(~globals, cls, status))
  | InfoTyp({cls, status, _}) => wrapper(typ_view(~globals, cls, status))
  | InfoTPat({cls, status, _}) => wrapper(tpat_view(~globals, cls, status))
  };
};

let inspector_view = (~globals, ci): Node.t =>
  div(
    ~attrs=[
      clss(["cursor-inspector"] @ [Info.is_error(ci) ? errc : okc]),
    ],
    [view_of_info(~globals, ci)],
  );

let view = (~globals: Globals.t, cursor_info: option(Info.t)) => {
  let bar_view = div(~attrs=[Attr.id("bottom-bar")]);
  let err_view = err =>
    bar_view([
      div(
        ~attrs=[clss(["cursor-inspector", "no-info"])],
        [div(~attrs=[clss(["icon"])], [Icons.magnify]), text(err)],
      ),
    ]);
  switch (cursor_info) {
  | _ when !globals.settings.core.statics => div_empty
  | None => err_view("Whitespace or Comment")
  | Some(ci) =>
    bar_view([
      inspector_view(~globals, ci),
      div(
        ~attrs=[clss(["id"])],
        [text(String.sub(Id.to_string(Info.id_of(ci)), 0, 4))],
      ),
    ])
  };
};
