open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;
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

let happy_view = (~inject, suc: Haz3lcore.Statics.happy) => {
  let toggle_cursor_inspector_event = toggle =>
    Virtual_dom.Vdom.Effect.(
      Many([
        Prevent_default,
        Stop_propagation,
        inject(UpdateAction.UpdateStrategyGuide(toggle)),
      ])
    );
  let fill_icon =
    Node.div(
      ~attr=
        Attr.many([
          Attr.classes(["clickable-help"]),
          Attr.create("title", "Click to toggle strategy guide"),
          Attr.on_click(_ =>
            toggle_cursor_inspector_event(
              StrategyGuideModel.Toggle_strategy_guide,
            )
          ),
        ]),
      //fix unicode
      [Node.text("💡")],
    );
  switch (suc) {
  | SynConsistent(ty_syn) =>
    div(
      ~attr=clss([happyc, "syn-consistent"]),
      [text("has type"), Type.view(ty_syn), fill_icon],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) when ty_ana == ty_syn =>
    div(
      ~attr=clss([happyc, "ana-consistent-equal"]),
      [text("has expected type"), Type.view(ty_ana), fill_icon],
    )
  | AnaConsistent(ty_ana, ty_syn, _ty_join) =>
    div(
      ~attr=clss([happyc, "ana-consistent"]),
      switch (ty_syn) {
      // A hack for EECS 490 A1
      | Haz3lcore.Typ.Unknown(_) => [
          text("has expected type"),
          Type.view(ty_ana),
          fill_icon,
        ]
      | _ => [
          text("has type"),
          Type.view(ty_syn),
          text("which is consistent with"),
          Type.view(ty_ana),
          fill_icon,
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
        fill_icon,
      ],
    )
  };
};

let status_view = (~inject, err: Haz3lcore.Statics.error_status) => {
  switch (err) {
  | InHole(error) => error_view(error)
  | NotInHole(happy) => happy_view(~inject, happy)
  };
};

let term_tag = (~inject, ~show_lang_doc, is_err, sort) => {
  let lang_doc =
    div(
      ~attr=clss(["lang-doc-button"]),
      [
        Widgets.toggle(
          ~tooltip="Toggle language documentation", "i", show_lang_doc, _ =>
          Virtual_dom.Vdom.Effect.Many([
            inject(Update.UpdateLangDocMessages(LangDocMessages.ToggleShow)),
            Virtual_dom.Vdom.Effect.Stop_propagation,
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
    (~inject, ~show_lang_doc: bool, ci: Haz3lcore.Statics.t): Node.t => {
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
        status_view(~inject, error_status),
      ],
    );
  | InfoPat({mode, self, _}) =>
    let error_status = Haz3lcore.Statics.error_status(mode, self);
    div(
      ~attr=clss([infoc, "pat"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "pat"),
        status_view(~inject, error_status),
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
    div(
      ~attr=clss([infoc, "typ"]),
      [
        term_tag(~inject, ~show_lang_doc, is_err, "typ"),
        text("is"),
        Type.view(ty),
      ],
    )
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
      ~settings: Model.settings,
      ~show_lang_doc: bool,
      id: int,
      ci: Haz3lcore.Statics.t,
      info_map: Haz3lcore.Statics.map,
    )
    : Node.t => {
  let strategymodel: StrategyGuideModel.t = settings.strategy_guide;
  // let (strategy_guide) =
  //   switch (ci, cursor_info.parent_info) {
  //   | (ExpOperand(_, EmptyHole(_)), _) => Some(StrategyGuide.exp_hole_view(~inject, strategymodel, ci))
  //   | (Rule(_), _)
  //   | (ExpOperand(_, Case(_)), _)
  //   | (_, AfterBranchClause) =>
  //     switch (StrategyGuide.rules_view(cursor_info)) {
  //     | None => (None)
  //     | Some(sg_rules) => (Some(sg_rules))
  //     }
  //   | (Line(_, EmptyLine), _) => (
  //       Some(StrategyGuide.lines_view(true)),
  //     )
  //   | (Line(_), _) => (Some(StrategyGuide.lines_view(false)))
  //   | _ => (None)
  //   };

  let strategy_guide =
    switch (ci) {
    | InfoExp({term, _}) =>
      switch (term.term) {
      | EmptyHole =>
        Some(
          StrategyGuide.exp_hole_view(~inject, strategymodel, ci, info_map),
        )
      | Match(_) =>
        switch (StrategyGuide.rules_view(ci)) {
        | None => None
        | Some(sg_rules) => Some(sg_rules)
        }
      | _ => None
      }
    | InfoRul(_) =>
      switch (StrategyGuide.rules_view(cursor_info)) {
      | None => None
      | Some(sg_rules) => Some(sg_rules)
      }
    | _ => None
    };
  // switch (ci) {
  // | Some(InfoExp({term, _})) => {
  //     switch (term) {
  //     | EmptyHole
  //     }
  //   }
  // | _ => None
  // }
  // let strategy_guide =
  //   switch (ci, cursor_info.parent_info) {
  //   | (ExpOperand(_, EmptyHole(_)), _) =>
  //     Some(StrategyGuide.exp_hole_view(~inject, strategymodel, ci))
  //   | (Rule(_), _)
  //   | (ExpOperand(_, Case(_)), _)
  //   | (_, AfterBranchClause) =>
  // switch (StrategyGuide.rules_view(cursor_info)) {
  // | None => None
  // | Some(sg_rules) => Some(sg_rules)
  // }
  //   | (Line(_, EmptyLine), _) => Some(StrategyGuide.lines_view(true))
  //   | (Line(_), _) => Some(StrategyGuide.lines_view(false))
  //   | _ => None
  //   };
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
      view_of_info(~inject, ~show_lang_doc, ci),
      CtxInspector.inspector_view(~inject, ~settings, id, ci),
      strategymodel.strategy_guide ? strategy_guide : div([Node.text("")]),
    ],
  );
};

let view =
    (
      ~inject,
      ~settings: Model.settings,
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
        inspector_view(
          ~inject,
          ~settings,
          ~show_lang_doc,
          index,
          ci,
          info_map,
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
};
