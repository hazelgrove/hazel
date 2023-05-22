open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;

let cls_str = (ci: Haz3lcore.Statics.t): string =>
  switch (ci) {
  | Invalid(msg) => Haz3lcore.TermBase.show_parse_flag(msg)
  | InfoExp({cls, _}) => Haz3lcore.Term.UExp.show_cls(cls)
  | InfoPat({cls, _}) => Haz3lcore.Term.UPat.show_cls(cls)
  | InfoTyp({cls, _}) => Haz3lcore.Term.UTyp.show_cls(cls)
  | InfoRul({cls, _}) => Haz3lcore.Term.URul.show_cls(cls)
  };

let errorc = "error";
let happyc = "happy";
let infoc = "info";

let error_view = (err: Haz3lcore.Statics.error) =>
  switch (err) {
  | Multi =>
    div(~attr=clss([errorc, "err-multi"]), [text("⑂ Multi Hole")])
  | Free(Variable) =>
    div(
      ~attr=clss([errorc, "err-free-variable"]),
      [text("Variable is not bound")],
    )
  | NoFun(typ) =>
    div(
      ~attr=clss([errorc, "err-not-function"]),
      [text("Not a function: "), Type.view(typ)],
    )
  | Free(TypeVariable) =>
    div(
      ~attr=clss([errorc, "err-free-variable"]),
      [text("Type Variable is not bound")],
    )
  | Free(Tag) =>
    div(
      ~attr=clss([errorc, "err-free-variable"]),
      [text("Constructor is not defined")],
    )
  | SynInconsistentBranches(tys) =>
    div(
      ~attr=clss([errorc, "err-inconsistent-branches"]),
      [text("Expecting branches to have consistent types but got:")]
      @ ListUtil.join(text(","), List.map(Type.view, tys)),
    )
  | TypeInconsistent(ty_syn, ty_ana) =>
    div(
      ~attr=clss([errorc, "err-type-inconsistent"]),
      [
        text("Expecting"),
        Type.view(ty_ana),
        text("but found"),
        Type.view(ty_syn),
      ],
    )
  };

let happy_view = (suc: Haz3lcore.Statics.happy) => {
  switch (suc) {
  | SynConsistent(ty_syn) =>
    div(
      ~attr=clss([happyc, "syn-consistent"]),
      [text("has type"), Type.view(ty_syn)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) when ty_ana == ty_syn =>
    div(
      ~attr=clss([happyc, "ana-consistent-equal"]),
      [text("has expected type"), Type.view(ty_ana)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) =>
    div(
      ~attr=clss([happyc, "ana-consistent"]),
      switch (ty_syn) {
      // A hack for EECS 490 A1
      | Haz3lcore.Typ.Unknown(_) => [
          text("has expected type"),
          Type.view(ty_ana),
        ]
      | _ => [
          text("has type"),
          Type.view(ty_syn),
          text("which is consistent with"),
          Type.view(ty_ana),
        ]
      },
    )
  | AnaInternalInconsistent(ty_ana, _)
  | AnaExternalInconsistent(ty_ana, _) =>
    div(
      ~attr=clss([happyc, "ana-consistent-external"]),
      [
        div(
          ~attr=clss(["typ-view", "atom"]),
          [text("⇐"), div(~attr=clss(["typ-mod"]), [text("☆")])],
        ),
        Type.view(ty_ana),
      ],
    )
  };
};

let status_view = (err: Haz3lcore.Statics.error_status) => {
  switch (err) {
  | InHole(error) => error_view(error)
  | NotInHole(happy) => happy_view(happy)
  };
};

let term_tag = (~inject, ~show_lang_doc, is_err, sort) => {
  let lang_doc =
    div(
      ~attr=clss(["lang-doc-button"]),
      [
        Widgets.toggle(
          ~tooltip="Toggle language documentation", "i", show_lang_doc, _ =>
          Effect.Many([
            inject(Update.UpdateLangDocMessages(LangDocMessages.ToggleShow)),
            Effect.Stop_propagation,
          ])
        ),
      ],
    );

  div(
    ~attr=
      Attr.many([
        clss(["term-tag", "term-tag-" ++ sort] @ (is_err ? [errorc] : [])),
      ]),
    [div(~attr=clss(["gamma"]), [text("Γ")]), text(sort), lang_doc],
  );
};

module State = {
  type t = {
    considering_suggestion: ref(bool),
    last_inspector: ref(Node.t),
  };

  let init = () => {
    considering_suggestion: ref(false),
    last_inspector: ref(div([])),
  };

  let curr_state = init();

  let get_considering_suggestion = () => curr_state.considering_suggestion^;
  let set_considering_suggestion = v => curr_state.considering_suggestion := v;

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
  switch (InferenceView.get_cursor_inspect_result(~global_inference_info, id)) {
  | Some((true, solution)) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [Type.view(~font_metrics, List.nth(solution, 0))],
    )
  | Some((false, [typ_with_nested_conflict])) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [Type.view(~font_metrics, typ_with_nested_conflict)],
    )
  | Some((false, conflicting_typs)) =>
    div(
      ~attr=clss([infoc, "typ"]),
      List.map(
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
                  inject(Update.Paste(Haz3lcore.Typ.typ_to_string(typ)));
                },
                _mouse_event =>
                  if (State.get_considering_suggestion()) {
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
    )
  | None => div([])
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
  };
};

let cls_view = (ci: Haz3lcore.Statics.t): Node.t =>
  div(~attr=clss(["syntax-class"]), [text(cls_str(ci))]);

let id_view = (id): Node.t =>
  div(~attr=clss(["id"]), [text(string_of_int(id + 1))]);

let extra_view = (visible: bool, id: int, ci: Haz3lcore.Statics.t): Node.t =>
  div(
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
  );

let view =
    (
      ~inject,
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
};
