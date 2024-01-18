open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attr=clss([errc]));
let div_ok = div(~attr=clss([okc]));

let infoc = "info"; //TODO: ????

let code_err = (code: string): Node.t =>
  div(~attr=clss(["code"]), [text(code)]);

let lang_doc_toggle = (~inject, ~show_lang_doc: bool): Node.t => {
  let tooltip = "Toggle language documentation";
  let toggle_landocs = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Update.UpdateLangDocMessages(ToggleShow)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attr=clss(["lang-doc-button"]),
    [Widgets.toggle(~tooltip, "?", show_lang_doc, toggle_landocs)],
  );
};

let cls_view = (ci: Info.t): Node.t =>
  div(
    ~attr=clss(["syntax-class"]),
    [text(ci |> Info.cls_of |> Term.Cls.show)],
  );

let ctx_toggle = (~inject, context_inspector: bool): Node.t =>
  div(
    ~attr=
      Attr.many([
        Attr.on_click(_ => inject(Update.Set(ContextInspector))),
        clss(["gamma"] @ (context_inspector ? ["visible"] : [])),
      ]),
    [text("Γ")],
  );

let term_view = (~inject, ~settings: Settings.t, ~show_lang_doc, ci) => {
  let sort = ci |> Info.sort_of |> Sort.show;
  div(
    ~attr=clss(["ci-header", sort] @ (Info.is_error(ci) ? [errc] : [])),
    [
      ctx_toggle(~inject, settings.context_inspector),
      CtxInspector.view(~inject, ~settings, ci),
      div(~attr=clss(["term-tag"]), [text(sort)]),
      lang_doc_toggle(~inject, ~show_lang_doc),
      cls_view(ci),
    ],
  );
};

module State = {
  type t = {
    considering_suggestion: ref(bool),
    suggestion_pasted: ref(bool),
    last_inspector: ref(Node.t),
  };

  let init = () => {
    considering_suggestion: ref(false),
    suggestion_pasted: ref(false),
    last_inspector: ref(div([])),
  };

  let curr_state = init();

  let get_considering_suggestion = () => curr_state.considering_suggestion^;
  let set_considering_suggestion = v => curr_state.considering_suggestion := v;

  let get_suggestion_pasted = () => curr_state.suggestion_pasted^;
  let set_suggestion_pasted = v => curr_state.suggestion_pasted := v;

  let get_last_inspector = () => curr_state.last_inspector^;
  let set_last_inspector = v => curr_state.last_inspector := v;
};

let view_of_global_inference_info =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~global_inference_info: Haz3lcore.InferenceResult.global_inference_info,
      id: Id.t,
    ) => {
  let font_metrics = Some(font_metrics);
  let no_hole_marks = typ_filling =>
    typ_filling
    |> StringUtil.to_list
    |> List.filter(s => s != "?" && s != "!")
    |> String.concat("");
  let suggestion_button_of_typ = (~id: option(Id.t)=None, typ) => {
    div(
      ~attr=clss(["typ-view-conflict"]),
      [
        Widgets.hoverable_button(
          [Type.view(~font_metrics, typ)],
          _mouse_event => {
            State.set_considering_suggestion(false);
            inject(Update.SetMeta(Mouseup));
          },
          _mouse_event => {
            State.set_considering_suggestion(true);
            if (!State.get_suggestion_pasted()) {
              State.set_suggestion_pasted(true);
              switch (id) {
              | Some(id) =>
                Ui_effect.bind(
                  inject(Update.PerformAction(Jump(TileId(id), Right))),
                  ~f=_res =>
                  inject(
                    Update.Paste(
                      " : "
                      ++ no_hole_marks(
                           Haz3lcore.Typ.typ_to_string(typ, false),
                         ),
                    ),
                  )
                )
              | None =>
                inject(
                  Update.Paste(
                    no_hole_marks(Haz3lcore.Typ.typ_to_string(typ, false)),
                  ),
                )
              };
            } else {
              inject(Update.SetMeta(Mouseup));
            };
          },
          _mouse_event =>
            if (State.get_considering_suggestion()) {
              State.set_suggestion_pasted(false);
              State.set_considering_suggestion(false);
              switch (id) {
              | Some(_) =>
                Ui_effect.bind(inject(Update.Undo), ~f=_res =>
                  inject(Update.Undo)
                )
              | None => inject(Update.Undo)
              };
            } else {
              inject(Update.SetMeta(Mouseup));
            },
        ),
      ],
    );
  };
  switch (InferenceView.get_cursor_inspect_result(~global_inference_info, id)) {
  | SolvedTypeHole(solution) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [text("consistent constraints"), Type.view(~font_metrics, solution)],
    )

  | SolvedExpHole(id, solution) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [
        text("consistent constraints"),
        suggestion_button_of_typ(~id=Some(id), solution),
      ],
    )
  | UnsolvedTypeHole(occurs, [typ_with_nested_conflict]) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [
        text(
          occurs ? "inferred type refers to self" : "conflicting constraints",
        ),
        suggestion_button_of_typ(typ_with_nested_conflict),
      ],
    )
  | UnsolvedExpHole(occurs, id, [typ_with_nested_conflict]) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [
        text(
          occurs ? "inferred type refers to self" : "conflicting constraints",
        ),
        suggestion_button_of_typ(~id=Some(id), typ_with_nested_conflict),
      ],
    )
  | UnsolvedTypeHole(occurs, conflicting_typs) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [
        text(
          occurs
            ? "inferred type may refer to self and contains conflicting constraints"
            : "conflicting constraints",
        ),
        ...List.map(suggestion_button_of_typ, conflicting_typs),
      ],
    )
  | UnsolvedExpHole(occurs, id, conflicting_typs) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [
        text(
          occurs
            ? "inferred type may refer to self and contains conflicting constraints"
            : "conflicting constraints",
        ),
        ...List.map(
             suggestion_button_of_typ(~id=Some(id)),
             conflicting_typs,
           ),
      ],
    )
  | NoSuggestion => div([])
  };
};

let elements_noun: Term.Cls.t => string =
  fun
  | Exp(Match | If) => "Branches"
  | Exp(ListLit)
  | Pat(ListLit) => "Elements"
  | Exp(ListConcat) => "Operands"
  | _ => failwith("elements_noun: Cls doesn't have elements");

let common_err_view = (~font_metrics, cls: Term.Cls.t, err: Info.error_common) =>
  switch (err) {
  | NoType(BadToken(token)) =>
    switch (Form.bad_token_cls(token)) {
    | BadInt => [text("Integer is too large or too small")]
    | Other => [text(Printf.sprintf("\"%s\" isn't a valid token", token))]
    }
  | NoType(BadTrivAp(ty)) => [
      text("Function argument type"),
      Type.view(~font_metrics, ty),
      text("inconsistent with"),
      Type.view(~font_metrics, Prod([])),
    ]
  | NoType(FreeConstructor(name)) => [code_err(name), text("not found")]
  | Inconsistent(WithArrow(typ)) => [
      text(":"),
      Type.view(~font_metrics, typ),
      text("inconsistent with arrow type"),
    ]
  | Inconsistent(Expectation({ana, syn})) => [
      text(":"),
      Type.view(~font_metrics, syn),
      text("inconsistent with expected type"),
      Type.view(~font_metrics, ana),
    ]
  | Inconsistent(Internal(tys)) => [
      text(elements_noun(cls) ++ " have inconsistent types:"),
      ...ListUtil.join(text(","), List.map(Type.view(~font_metrics), tys)),
    ]
  };

let common_ok_view =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info,
      ~id,
      cls: Term.Cls.t,
      ok: Info.ok_pat,
    ) => {
  switch (
    Haz3lcore.InferenceResult.get_suggestion_text_for_id(
      id,
      global_inference_info,
    )
  ) {
  | (NoSuggestion(SuggestionsDisabled), _)
  | (NoSuggestion(NotSuggestableHoleId), _)
  | (NoSuggestion(OnlyHoleSolutions), _) =>
    let font_metrics = Some(font_metrics);
    switch (cls, ok) {
    | (Exp(MultiHole) | Pat(MultiHole), _) => [
        text("Expecting operator or delimiter"),
      ]
    | (Exp(EmptyHole), Syn(_)) => [text("Fillable by any expression")]
    | (Pat(EmptyHole), Syn(_)) => [text("Fillable by any pattern")]
    | (Exp(EmptyHole), Ana(Consistent({ana, _}))) => [
        text("Fillable by any expression of type"),
        Type.view(~font_metrics, ana),
      ]
    | (Pat(EmptyHole), Ana(Consistent({ana, _}))) => [
        text("Fillable by any pattern of type"),
        Type.view(~font_metrics, ana),
      ]
    | (_, Syn(syn)) => [text(":"), Type.view(~font_metrics, syn)]
    | (Pat(Var) | Pat(Wild), Ana(Consistent({ana, _}))) => [
        text(":"),
        Type.view(~font_metrics, ana),
      ]
    | (_, Ana(Consistent({ana, syn, _}))) when ana == syn => [
        text(":"),
        Type.view(~font_metrics, syn),
        text("equals expected type"),
      ]
    | (_, Ana(Consistent({ana, syn, _}))) => [
        text(":"),
        Type.view(~font_metrics, syn),
        text("consistent with expected type"),
        Type.view(~font_metrics, ana),
      ]
    | (_, Ana(InternallyInconsistent({ana, nojoin: tys}))) =>
      [
        text(elements_noun(cls) ++ " have inconsistent types:"),
        ...ListUtil.join(
             text(","),
             List.map(Type.view(~font_metrics), tys),
           ),
      ]
      @ [
        text("but consistent with expected"),
        Type.view(~font_metrics, ana),
      ]
    };
  | _ => [
      view_of_global_inference_info(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        id,
      ),
    ]
  };
};

let typ_ok_view =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info,
      ~id,
      _cls: Term.Cls.t,
      ok: Info.ok_typ,
    ) =>
  switch (ok) {
  | Type(ty) =>
    switch (
      Haz3lcore.InferenceResult.get_suggestion_text_for_id(
        id,
        global_inference_info,
      )
    ) {
    | (NoSuggestion(SuggestionsDisabled), _)
    | (NoSuggestion(NotSuggestableHoleId), _)
    | (NoSuggestion(OnlyHoleSolutions), _) => [
        Type.view(~font_metrics=Some(font_metrics), ty),
      ]
    | _ => [
        view_of_global_inference_info(
          ~inject,
          ~font_metrics,
          ~global_inference_info,
          id,
        ),
      ]
    }
  //TODO(andrew): restore this message?
  //| Type(_) when cls == Typ(EmptyHole) => [text("Fillable by any type")]
  //| Type(ty) => [Type.view(ty)]
  //TODO(andrew): how do these interact with THI?
  | TypeAlias(name, ty_lookup) => [
      Type.view(~font_metrics=Some(font_metrics), Var(name)),
      text("is an alias for"),
      Type.view(~font_metrics=Some(font_metrics), ty_lookup),
    ]
  | Variant(name, _sum_ty) => [
      Type.view(~font_metrics=Some(font_metrics), Var(name)),
    ]
  | VariantIncomplete(_sum_ty) => [text("is incomplete")]
  };

let typ_err_view = (ok: Info.error_typ, ~font_metrics) =>
  switch (ok) {
  | FreeTypeVariable(name) => [
      Type.view(~font_metrics, Var(name)),
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
      Type.view(~font_metrics, Var(name)),
      text("already used in this sum"),
    ]
  };

let exp_view =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info,
      ~id,
      cls: Term.Cls.t,
      status: Info.status_exp,
    ) =>
  switch (status) {
  | InHole(FreeVariable(name)) =>
    div_err([code_err(name), text("not found")])
  | InHole(Common(error)) =>
    div_err(common_err_view(~font_metrics=Some(font_metrics), cls, error))
  | NotInHole(ok) =>
    div_ok(
      common_ok_view(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        ~id,
        cls,
        ok,
      ),
    )
  };

let pat_view =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info,
      ~id,
      cls: Term.Cls.t,
      status: Info.status_pat,
    ) =>
  switch (status) {
  | InHole(ExpectedConstructor) => div_err([text("Expected a constructor")])
  | InHole(Common(error)) =>
    div_err(common_err_view(~font_metrics=Some(font_metrics), cls, error))
  | NotInHole(ok) =>
    div_ok(
      common_ok_view(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        ~id,
        cls,
        ok,
      ),
    )
  };

let typ_view =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info,
      ~id,
      cls: Term.Cls.t,
      status: Info.status_typ,
    ) =>
  switch (status) {
  | NotInHole(ok) =>
    div_ok(
      typ_ok_view(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        ~id,
        cls,
        ok,
      ),
    )
  | InHole(err) =>
    div_err(typ_err_view(~font_metrics=Some(font_metrics), err))
  };

let tpat_view = (_: Term.Cls.t, status: Info.status_tpat, ~font_metrics) =>
  switch (status) {
  | NotInHole(Empty) => div_ok([text("Fillable with a new alias")])
  | NotInHole(Var(name)) => div_ok([Type.alias_view(name)])
  | InHole(NotAVar(NotCapitalized)) =>
    div_err([text("Must begin with a capital letter")])
  | InHole(NotAVar(_)) => div_err([text("Expected an alias")])
  | InHole(ShadowsType(name)) when Form.is_base_typ(name) =>
    div_err([
      text("Can't shadow base type"),
      Type.view(~font_metrics, Var(name)),
    ])
  | InHole(ShadowsType(name)) =>
    div_err([
      text("Can't shadow existing alias"),
      Type.view(~font_metrics, Var(name)),
    ])
  };

let view_of_info =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info,
      ~settings,
      ~show_lang_doc: bool,
      ~id,
      ci: Statics.Info.t,
    )
    : Node.t => {
  let wrapper = status_view =>
    div(
      ~attr=clss(["info"]),
      [term_view(~inject, ~settings, ~show_lang_doc, ci), status_view],
    );
  switch (ci) {
  | InfoExp({cls, status, _}) =>
    wrapper(
      exp_view(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        ~id,
        cls,
        status,
      ),
    )
  | InfoPat({cls, status, _}) =>
    wrapper(
      pat_view(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        ~id,
        cls,
        status,
      ),
    )
  | InfoTyp({cls, status, _}) =>
    wrapper(
      typ_view(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        ~id,
        cls,
        status,
      ),
    )
  | InfoTPat({cls, status, _}) =>
    wrapper(tpat_view(cls, status, ~font_metrics=Some(font_metrics)))
  };
};

let inspector_view =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info,
      ~settings,
      ~show_lang_doc,
      ~id,
      ci,
    )
    : Node.t =>
  div(
    ~attr=clss(["cursor-inspector"] @ [Info.is_error(ci) ? errc : okc]),
    [
      view_of_info(
        ~inject,
        ~font_metrics,
        ~global_inference_info,
        ~settings,
        ~show_lang_doc,
        ~id,
        ci,
      ),
    ],
  );

let view =
    (
      ~inject,
      ~settings: Settings.t,
      ~font_metrics,
      ~show_lang_doc: bool,
      zipper: Zipper.t,
      info_map: Statics.Map.t,
      global_inference_info: Haz3lcore.InferenceResult.global_inference_info,
    ) => {
  let curr_view =
    if (State.get_considering_suggestion()) {
      State.get_last_inspector();
    } else {
      let bar_view = div(~attr=Attr.id("bottom-bar"));
      let err_view = err =>
        bar_view([
          div(
            ~attr=clss(["cursor-inspector", "no-info"]),
            [div(~attr=clss(["icon"]), [Icons.magnify]), text(err)],
          ),
        ]);
      switch (zipper.backpack, Indicated.index(zipper)) {
      | _ when !settings.core.statics => div_empty
      | _ when Id.Map.is_empty(info_map) =>
        err_view("No Static information available")
      | (_, None) => err_view("No cursor in program")
      | (_, Some(id)) =>
        switch (Id.Map.find_opt(id, info_map)) {
        | None => err_view("Whitespace or Comment")
        | Some(ci) =>
          bar_view([
            inspector_view(
              ~inject,
              ~font_metrics,
              ~global_inference_info,
              ~settings,
              ~show_lang_doc,
              ~id,
              ci,
            ),
            div(
              ~attr=clss(["id"]),
              [text(String.sub(Id.to_string(id), 0, 4))],
            ),
          ])
        }
      };
    };
  State.set_last_inspector(curr_view);
  curr_view;
};
