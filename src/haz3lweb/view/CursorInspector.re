open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;

let cls_str = (ci: Haz3lcore.Info.t): string =>
  switch (ci) {
  | Invalid(msg) => Haz3lcore.TermBase.show_parse_flag(msg)
  | InfoExp({cls, _}) => Haz3lcore.Term.UExp.show_cls(cls)
  | InfoPat({cls, _}) => Haz3lcore.Term.UPat.show_cls(cls)
  | InfoTyp({cls, _}) => Haz3lcore.Term.UTyp.show_cls(cls)
  | InfoRul({cls, _}) => Haz3lcore.Term.URul.show_cls(cls)
  | InfoTPat({cls, _}) => Haz3lcore.Term.UTPat.show_cls(cls)
  };

let errorc = "error";
let happyc = "happy";
let infoc = "info";

let error_view = (err: Haz3lcore.Info.error) =>
  switch (err) {
  | Self(Multi) =>
    div(~attr=clss([errorc, "err-multi"]), [text("⑂ Multi Hole")])
  | Self(Free) =>
    div(
      ~attr=clss([errorc, "err-free-variable"]),
      [text("Variable is not bound")],
    )
  | Self(NoFun(typ)) =>
    div(
      ~attr=clss([errorc, "err-not-function"]),
      [text("Not consistent with arrow type:"), Type.view(typ)],
    )
  | Self(FreeTag) =>
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

let happy_view = (suc: Haz3lcore.Info.happy) => {
  switch (suc) {
  | SynConsistent(ty_syn) =>
    div(
      ~attr=clss([happyc, "syn-consistent"]),
      [text("has type"), Type.view(ty_syn)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) when ty_ana == ty_syn =>
    div(
      ~attr=clss([happyc, "ana-consistent-equal"]),
      [text("has expected & actual type"), Type.view(ty_ana)],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) =>
    div(
      ~attr=clss([happyc, "ana-consistent"]),
      switch (ty_syn) {
      // A hack for EECS 490 A1
      | Haz3lcore.Typ.Unknown(_) => [
          text("satisfies expected type"),
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
  | AnaInternalInconsistent(ty_ana, tys) =>
    div(
      ~attr=clss([happyc, "ana-inconsistent-internal"]),
      [
        div(~attr=clss(["typ-view", "atom"]), [text("is consistent with")]),
        Type.view(ty_ana),
        div(
          ~attr=clss(["typ-view", "atom"]),
          [text("but is internally inconsistent: ")]
          @ ListUtil.join(text(","), List.map(Type.view, tys)),
        ),
      ],
    )
  };
};

let status_view = (err: Haz3lcore.Info.error_status) => {
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

let view_of_info =
    (~inject, ~show_lang_doc: bool, ci: Haz3lcore.Info.t): Node.t => {
  let is_err = Haz3lcore.Info.is_error(ci);
  switch (ci) {
  | Invalid(msg) =>
    div(
      ~attr=clss([infoc, "unknown"]),
      [text("🚫 " ++ Haz3lcore.TermBase.show_parse_flag(msg))],
    )
  | InfoExp({mode, self, ctx, _}) =>
    let error_status = Haz3lcore.Info.error_status(ctx, mode, self);
    div(
      ~attr=clss([infoc, "exp"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "exp"),
        status_view(error_status),
      ],
    );
  | InfoPat({mode, self, ctx, _}) =>
    let error_status = Haz3lcore.Info.error_status(ctx, mode, self);
    div(
      ~attr=clss([infoc, "pat"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "pat"),
        status_view(error_status),
      ],
    );
  | InfoTyp({status, ctx, mode, cls, _}) =>
    div(
      ~attr=clss([infoc, "typ"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "typ"),
        switch (status) {
        | Ok(_) when cls == Var && mode != TypeExpected =>
          div(
            ~attr=clss([happyc]),
            [text("Sum type constuctor definition")],
          )
        | Ok(_) when cls == Ap && mode != TypeExpected =>
          div(
            ~attr=clss([happyc]),
            [text("Sum type constuctor definition")],
          )
        | Ok(ty) => ty |> Haz3lcore.Kind.normalize_shallow(ctx) |> Type.view
        | FreeTypeVar =>
          div(~attr=clss([errorc]), [text("Type Variable is not bound")])
        | TagExpected(typ) =>
          div(
            ~attr=clss([errorc]),
            [text("Expected a constructor, found"), Type.view(typ)],
          )
        | ApOutsideSum =>
          div(
            ~attr=clss([errorc]),
            [text("Constructor application must be in sum")],
          )
        | DuplicateTag =>
          div(~attr=clss([errorc]), [text("Duplicate constructor")])
        },
      ],
    )
  | InfoRul(_) =>
    div(
      ~attr=clss([infoc, "rul"]),
      [term_tag(~inject, ~show_lang_doc, is_err, "rul"), text("Rule")],
    )
  | InfoTPat({status, _}) =>
    div(
      ~attr=clss([infoc, "tpat"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "tpat"),
        switch (status) {
        | Ok => div(~attr=clss([happyc]), [text("New type alias")])
        | NotAName =>
          div(~attr=clss([errorc]), [text("Not a valid type name")])
        },
      ],
    )
  };
};

let cls_view = (ci: Haz3lcore.Info.t): Node.t =>
  div(~attr=clss(["syntax-class"]), [text(cls_str(ci))]);

let id_view = (id): Node.t =>
  div(~attr=clss(["id"]), [text(string_of_int(id + 1))]);

let extra_view = (visible: bool, id: int, ci: Haz3lcore.Info.t): Node.t =>
  div(
    ~attr=Attr.many([clss(["extra"] @ (visible ? ["visible"] : []))]),
    [id_view(id), cls_view(ci)],
  );

let toggle_context_and_print_ci = (~inject: Update.t => 'a, ci, _) => {
  print_endline(Haz3lcore.Info.show(ci));
  switch (ci) {
  | InfoPat({mode, self, ctx, _})
  | InfoExp({mode, self, ctx, _}) =>
    Haz3lcore.Info.error_status(ctx, mode, self)
    |> Haz3lcore.Info.show_error_status
    |> print_endline
  | _ => ()
  };
  inject(Set(ContextInspector));
};

let inspector_view =
    (
      ~inject,
      ~settings: Model.settings,
      ~show_lang_doc: bool,
      id: int,
      ci: Haz3lcore.Info.t,
    )
    : Node.t =>
  div(
    ~attr=
      Attr.many([
        clss(
          ["cursor-inspector"]
          @ [Haz3lcore.Info.is_error(ci) ? errorc : happyc],
        ),
        Attr.on_click(toggle_context_and_print_ci(~inject, ci)),
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
      zipper: Haz3lcore.Zipper.t,
      info_map: Haz3lcore.Statics.map,
    ) => {
  let backpack = zipper.backpack;
  if (List.length(backpack) > 0) {
    div([]);
  } else {
    let index = Haz3lcore.Indicated.index(zipper);

    switch (index) {
    | Some(index) =>
      switch (Haz3lcore.Id.Map.find_opt(index, info_map)) {
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
