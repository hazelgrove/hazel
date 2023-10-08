open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attr=clss([errc]));
let div_ok = div(~attr=clss([okc]));

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

let term_view = (~inject, ~settings: ModelSettings.t, ~show_lang_doc, ci) => {
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

<<<<<<< HEAD
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
      id: int,
    ) => {
  let font_metrics = Some(font_metrics);
  // switch (InferenceView.get_cursor_inspect_result(~global_inference_info, id)) {
  // | Some((true, solution)) =>
  //   div(
  //     ~attr=clss([infoc, "typ"]),
  //     [
  //       text("consistent constraints"),
  //       Type.view(~font_metrics, List.nth(solution, 0)),
  //     ],
  //   )
  // | Some((false, [typ_with_nested_conflict])) =>
  //   div(
  //     ~attr=clss([infoc, "typ"]),
  //     [Type.view(~font_metrics, typ_with_nested_conflict)],
  //   )
  // | Some((false, conflicting_typs)) =>
  //   div(
  //     ~attr=clss([infoc, "typ"]),
  //     [
  //       text("conflicting constraints"),
  //       ...List.map(
  //            typ =>
  //              div(
  //                ~attr=clss(["typ-view-conflict"]),
  //                [
  //                  Widgets.hoverable_button(
  //                    [Type.view(~font_metrics, typ)],
  //                    _mouse_event => {
  //                      State.set_considering_suggestion(false);
  //                      inject(Update.Mouseup);
  //                    },
  //                    _mouse_event => {
  //                      State.set_considering_suggestion(true);
  //                      if (!State.get_suggestion_pasted()) {
  //                        State.set_suggestion_pasted(true);
  //                        inject(
  //                          Update.Paste(Haz3lcore.Typ.typ_to_string(typ)),
  //                        );
  //                      } else {
  //                        inject(Update.Mouseup);
  //                      };
  //                    },
  //                    _mouse_event =>
  //                      if (State.get_considering_suggestion()) {
  //                        State.set_suggestion_pasted(false);
  //                        State.set_considering_suggestion(false);
  //                        inject(Update.Undo);
  //                      } else {
  //                        inject(Update.Mouseup);
  //                      },
  //                  ),
  //                ],
  //              ),
  //            conflicting_typs,
  //          ),
  //     ],
  //   )
  // | None => div([])
  // };
  if (global_inference_info.enabled) {
    let status = Haz3lcore.Infer.get_status(global_inference_info.ctx, id);
    switch (status) {
    | Solved(ty) => div([Type.view(~font_metrics, ty)])
    | Unsolved(conflicting_typs) =>
      div(
        ~attr=clss([infoc, "typ"]),
        [
          text("conflicting constraints"),
          ...List.map(
               typ =>
                 div(
                   ~attr=clss(["typ-view-conflict"]),
                   [
                     Widgets.hoverable_button(
                       [Type.view(~font_metrics, typ)],
                       _mouse_event => {
                         State.set_considering_suggestion(false);
                         inject(Update.Mouseup);
                       },
                       _mouse_event => {
                         State.set_considering_suggestion(true);
                         if (!State.get_suggestion_pasted()) {
                           State.set_suggestion_pasted(true);
                           inject(
                             Update.Paste(Haz3lcore.Typ.typ_to_string(typ)),
                           );
                         } else {
                           inject(Update.Mouseup);
                         };
                       },
                       _mouse_event =>
                         if (State.get_considering_suggestion()) {
                           State.set_suggestion_pasted(false);
                           State.set_considering_suggestion(false);
                           inject(Update.Undo);
                         } else {
                           inject(Update.Mouseup);
                         },
                     ),
                   ],
                 ),
               conflicting_typs,
             ),
        ],
      )
    };
  } else {
    div([]);
  };
};

let view_of_info =
    (
      ~inject,
      ~font_metrics,
      ~show_lang_doc: bool,
      ~global_inference_info,
      id: int,
      ci: Haz3lcore.Statics.t,
    )
    : Node.t => {
  let is_err = Haz3lcore.Statics.is_error(ci);
  switch (ci) {
  | Invalid(msg) =>
    div(
      ~attr=clss([infoc, "unknown"]),
      [text("🚫 " ++ Haz3lcore.TermBase.show_parse_flag(msg))],
    )
  | InfoExp({mode, self, _}) =>
    let error_status = Haz3lcore.Statics.error_status(mode, self);
    div(
      ~attr=clss([infoc, "exp"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "exp"),
        status_view(error_status),
      ],
    );
  | InfoPat({mode, self, _}) =>
    let error_status = Haz3lcore.Statics.error_status(mode, self);
    div(
      ~attr=clss([infoc, "pat"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "pat"),
        status_view(error_status),
      ],
    );
  | InfoTyp({self: Free(free_error), _}) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "typ"),
        error_view(Free(free_error)),
      ],
    )
  | InfoTyp({self: Just(ty), _}) =>
    switch (
      Haz3lcore.InferenceResult.get_suggestion_text_for_id(
        id,
        global_inference_info,
      )
    ) {
    | NoSuggestion(SuggestionsDisabled)
    | NoSuggestion(NonTypeHoleId)
    | NoSuggestion(OnlyHoleSolutions) =>
      div(
        ~attr=clss([infoc, "typ"]),
        [
          term_tag(~inject, ~show_lang_doc, is_err, "typ"),
          text("is"),
          Type.view(ty),
        ],
      )
    | _ =>
      div(
        ~attr=clss([infoc, "typ"]),
        [
          term_tag(~inject, ~show_lang_doc, is_err, "typ"),
          view_of_global_inference_info(
            ~inject,
            ~font_metrics,
            ~global_inference_info,
            id,
          ),
        ],
      )
    }
  | InfoTyp({self: _, _}) =>
    failwith("CursorInspector: Impossible type error")
  | InfoRul(_) =>
    div(
      ~attr=clss([infoc, "rul"]),
      [term_tag(~inject, ~show_lang_doc, is_err, "rul"), text("Rule")],
    )
=======
let elements_noun: Term.Cls.t => string =
  fun
  | Exp(Match | If) => "Branches"
  | Exp(ListLit)
  | Pat(ListLit) => "Elements"
  | Exp(ListConcat) => "Operands"
  | _ => failwith("elements_noun: Cls doesn't have elements");

let common_err_view = (cls: Term.Cls.t, err: Info.error_common) =>
  switch (err) {
  | NoType(BadToken(token)) =>
    switch (Form.bad_token_cls(token)) {
    | BadInt => [text("Integer is too large or too small")]
    | Other => [text(Printf.sprintf("\"%s\" isn't a valid token", token))]
    }
  | NoType(FreeConstructor(name)) => [code_err(name), text("not found")]
  | Inconsistent(WithArrow(typ)) => [
      text(":"),
      Type.view(typ),
      text("inconsistent with arrow type"),
    ]
  | Inconsistent(Expectation({ana, syn})) => [
      text(":"),
      Type.view(syn),
      text("inconsistent with expected type"),
      Type.view(ana),
    ]
  | Inconsistent(Internal(tys)) => [
      text(elements_noun(cls) ++ " have inconsistent types:"),
      ...ListUtil.join(text(","), List.map(Type.view, tys)),
    ]
  };

let common_ok_view = (cls: Term.Cls.t, ok: Info.ok_pat) => {
  switch (cls, ok) {
  | (Exp(MultiHole) | Pat(MultiHole), _) => [
      text("Expecting operator or delimiter"),
    ]
  | (Exp(EmptyHole), Syn(_)) => [text("Fillable by any expression")]
  | (Pat(EmptyHole), Syn(_)) => [text("Fillable by any pattern")]
  | (Exp(EmptyHole), Ana(Consistent({ana, _}))) => [
      text("Fillable by any expression of type"),
      Type.view(ana),
    ]
  | (Pat(EmptyHole), Ana(Consistent({ana, _}))) => [
      text("Fillable by any pattern of type"),
      Type.view(ana),
    ]
  | (_, Syn(syn)) => [text(":"), Type.view(syn)]
  | (Pat(Var) | Pat(Wild), Ana(Consistent({ana, _}))) => [
      text(":"),
      Type.view(ana),
    ]
  | (_, Ana(Consistent({ana, syn, _}))) when ana == syn => [
      text(":"),
      Type.view(syn),
      text("equals expected type"),
    ]
  | (_, Ana(Consistent({ana, syn, _}))) => [
      text(":"),
      Type.view(syn),
      text("consistent with expected type"),
      Type.view(ana),
    ]
  | (_, Ana(InternallyInconsistent({ana, nojoin: tys}))) =>
    [
      text(elements_noun(cls) ++ " have inconsistent types:"),
      ...ListUtil.join(text(","), List.map(Type.view, tys)),
    ]
    @ [text("but consistent with expected"), Type.view(ana)]
  };
};

let typ_ok_view = (cls: Term.Cls.t, ok: Info.ok_typ) =>
  switch (ok) {
  | Type(_) when cls == Typ(EmptyHole) => [text("Fillable by any type")]
  | Type(ty) => [Type.view(ty)]
  | TypeAlias(name, ty_lookup) => [
      Type.view(Var(name)),
      text("is an alias for"),
      Type.view(ty_lookup),
    ]
  | Variant(name, _sum_ty) => [Type.view(Var(name))]
  | VariantIncomplete(_sum_ty) => [text("is incomplete")]
  };

let typ_err_view = (ok: Info.error_typ) =>
  switch (ok) {
  | FreeTypeVariable(name) => [Type.view(Var(name)), text("not found")]
  | BadToken(token) => [
      code_err(token),
      text("not a type or type operator"),
    ]
  | WantConstructorFoundAp
  | WantConstructorFoundType(_) => [text("Expected a constructor")]
  | WantTypeFoundAp => [text("Must be part of a sum type")]
  | DuplicateConstructor(name) => [
      Type.view(Var(name)),
      text("already used in this sum"),
    ]
  };

let exp_view = (cls: Term.Cls.t, status: Info.status_exp) =>
  switch (status) {
  | InHole(FreeVariable(name)) =>
    div_err([code_err(name), text("not found")])
  | InHole(Common(error)) => div_err(common_err_view(cls, error))
  | NotInHole(ok) => div_ok(common_ok_view(cls, ok))
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
  | NotInHole(Empty) => div_ok([text("Fillable with a new alias")])
  | NotInHole(Var(name)) => div_ok([Type.alias_view(name)])
  | InHole(NotAVar(NotCapitalized)) =>
    div_err([text("Must begin with a capital letter")])
  | InHole(NotAVar(_)) => div_err([text("Expected an alias")])
  | InHole(ShadowsType(name)) when Form.is_base_typ(name) =>
    div_err([text("Can't shadow base type"), Type.view(Var(name))])
  | InHole(ShadowsType(name)) =>
    div_err([text("Can't shadow existing alias"), Type.view(Var(name))])
  };

let view_of_info =
    (~inject, ~settings, ~show_lang_doc: bool, ci: Statics.Info.t): Node.t => {
  let wrapper = status_view =>
    div(
      ~attr=clss(["info"]),
      [term_view(~inject, ~settings, ~show_lang_doc, ci), status_view],
    );
  switch (ci) {
  | InfoExp({cls, status, _}) => wrapper(exp_view(cls, status))
  | InfoPat({cls, status, _}) => wrapper(pat_view(cls, status))
  | InfoTyp({cls, status, _}) => wrapper(typ_view(cls, status))
  | InfoTPat({cls, status, _}) => wrapper(tpat_view(cls, status))
>>>>>>> dev
  };
};

let inspector_view = (~inject, ~settings, ~show_lang_doc, ci): Node.t =>
  div(
<<<<<<< HEAD
    ~attr=Attr.many([clss(["extra"] @ (visible ? ["visible"] : []))]),
    [id_view(id), cls_view(ci)],
  );

let toggle_context_and_print_ci = (~inject: Update.t => 'a, ci, _) => {
  print_endline(Haz3lcore.Statics.show(ci));
  switch (ci) {
  | InfoPat({mode, self, _})
  | InfoExp({mode, self, _}) =>
    Haz3lcore.Statics.error_status(mode, self)
    |> Haz3lcore.Statics.show_error_status
    |> print_endline
  | _ => ()
  };
  inject(Set(ContextInspector));
};

let inspector_view =
    (
      ~inject,
      ~font_metrics,
      ~global_inference_info: Haz3lcore.InferenceResult.global_inference_info,
      ~settings: ModelSettings.t,
      ~show_lang_doc: bool,
      id: int,
      ci: Haz3lcore.Statics.t,
    )
    : Node.t =>
  div(
    ~attr=
      Attr.many([
        clss(
          ["cursor-inspector"]
          @ [Haz3lcore.Statics.is_error(ci) ? errorc : happyc],
        ),
        Attr.on_click(toggle_context_and_print_ci(~inject, ci)),
      ]),
    [
      extra_view(settings.context_inspector, id, ci),
      view_of_info(
        ~inject,
        ~font_metrics,
        ~show_lang_doc,
        ~global_inference_info,
        id,
        ci,
      ),
      CtxInspector.inspector_view(~inject, ~settings, id, ci),
    ],
=======
    ~attr=clss(["cursor-inspector"] @ [Info.is_error(ci) ? errc : okc]),
    [view_of_info(~inject, ~settings, ~show_lang_doc, ci)],
>>>>>>> dev
  );

let view =
    (
      ~inject,
<<<<<<< HEAD
      ~settings,
      ~font_metrics,
      ~show_lang_doc: bool,
      zipper: Haz3lcore.Zipper.t,
      info_map: Haz3lcore.Statics.map,
      global_inference_info: Haz3lcore.InferenceResult.global_inference_info,
    ) => {
  let backpack = zipper.backpack;
  let curr_view =
    if (State.get_considering_suggestion()) {
      State.get_last_inspector();
    } else if (List.length(backpack) > 0) {
      div([]);
    } else {
      let index = Haz3lcore.Indicated.index(zipper);

      switch (index) {
      | Some(index) =>
        switch (Haz3lcore.Id.Map.find_opt(index, info_map)) {
        | Some(ci) =>
          inspector_view(
            ~inject,
            ~font_metrics,
            ~global_inference_info,
            ~settings,
            ~show_lang_doc,
            index,
            ci,
          )
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
  State.set_last_inspector(curr_view);
  curr_view;
=======
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
        inspector_view(~inject, ~settings, ~show_lang_doc, ci),
        div(
          ~attr=clss(["id"]),
          [text(String.sub(Id.to_string(id), 0, 4))],
        ),
      ])
    }
  };
>>>>>>> dev
};
