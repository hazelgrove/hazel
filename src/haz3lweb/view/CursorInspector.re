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

type usage_info = (Haz3lcore.Token.t, list(Haz3lcore.Id.t));
let binding_uses =
    (
      ci: Haz3lcore.Statics.t,
      _zipper: Haz3lcore.Zipper.t,
      info_map: Haz3lcore.Statics.map,
    )
    : Node.t => {
  switch (ci) {
  | InfoPat({term, body_ids, _}) =>
    let bindings =
      List.sort_uniq(compare, Haz3lcore.Term.UPat.get_all_bindings(term));
    let exps_to_search: list(Haz3lcore.Statics.info_exp) =
      List.fold_left(
        (exps, id) => {
          switch (Haz3lcore.Id.Map.find_opt(id, info_map)) {
          | Some(InfoExp(info_body)) => [info_body, ...exps]
          | _ => exps
          }
        },
        [],
        body_ids,
      );
    let empty_bindings_map: list(usage_info) =
      List.map(var => (var, []), bindings);
    let bindings_map =
      List.fold_left(
        (binding_map: list(usage_info), info_exp: Haz3lcore.Statics.info_exp) => {
          let updated_map: list(usage_info) =
            List.map(
              (usage_pair: usage_info) => {
                let (var, ids) = usage_pair;
                let new_ids: list(Haz3lcore.Id.t) =
                  List.map(
                    (item: Haz3lcore.Ctx.co_item) => {item.id},
                    List.flatten(
                      Haz3lcore.VarMap.lookup_all(info_exp.free, var),
                    ),
                  );
                print_string("co_ctx | ");
                let _ =
                  List.map(
                    coctx_pair => {
                      let (var, co_items) = coctx_pair;
                      print_string(var);
                      print_string(":[");
                      let _ =
                        List.map(
                          (co_it: Haz3lcore.Ctx.co_item) => {
                            print_int(co_it.id);
                            print_string(",");
                          },
                          co_items,
                        );
                      print_string("]; ");
                    },
                    info_exp.free,
                  );
                let all_ids: list(Haz3lcore.Id.t) =
                  List.sort_uniq(compare, new_ids @ ids);
                let pair: (Haz3lcore.Token.t, list(Haz3lcore.Id.t)) = (
                  var,
                  all_ids,
                );
                pair;
              },
              binding_map,
            );
          updated_map;
        },
        empty_bindings_map,
        exps_to_search,
      );
    let _ =
      List.map(
        (usage_pair: usage_info) => {
          let (var, ids) = usage_pair;
          print_string(var);
          print_string(": ");
          let _ =
            List.map(
              id => {
                print_int(id);
                print_string(", ");
              },
              ids,
            );
          print_newline();
        },
        bindings_map,
      );
    div([]);
  // extract the parent index
  /* let parent_exp_gen = */
  /*   List.find_opt( */
  /*     gen => { */
  /*       let anc: Haz3lcore.Ancestor.t = fst(gen); */
  /*       switch (Haz3lcore.Ancestor.sort(anc)) { */
  /*       | Exp => true */
  /*       | Any => true */
  /*       | _ => false */
  /*       }; */
  /*     }, */
  /*     zipper.relatives.ancestors, */
  /*   ); */
  /* switch (parent_exp_gen) { */
  /* | Some((anc, _)) => */
  /*   switch (Haz3lcore.Id.Map.find_opt(anc.id, info_map)) { */
  /*   | Some(InfoExp(info)) => */
  /*     print_int(anc.id); */
  /*     let _ = */
  /*       List.map((_, co_entry) => print_endline(co_entry), info.free); */
  /*     (); */
  /*   | None => print_string("nothing in info_map") */
  /*   | _ => () */
  /*   }; */
  /*   print_newline(); */
  /*   div(List.map(x => text(string_of_int(x)), [anc.id])); */
  /* | _ => div([]) */
  /* }; */
  // iterate over children of the parent exp
  // co-context of binding expression will have the uids of variable uses
  // in the info_map, store the binding uses
  // make it a list of co-contexts for mutual recursion to work
  // test recursive and mutually recursive bindings
  // get mold.out (sort) of Exp
  // lookup in info_map to find items of cls Var
  // for each var, check if the token matches
  // if it does, determine if the id of this is in the Ctx
  // then how do we get a id from that??
  | _ => div([])
  };
};

let inspector_view =
    (
      ~inject,
      ~settings: Model.settings,
      ~show_lang_doc: bool,
      id: int,
      ci: Haz3lcore.Statics.t,
      zipper: Haz3lcore.Zipper.t,
      info_map: Haz3lcore.Statics.map,
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
      view_of_info(~inject, ~show_lang_doc, ci),
      binding_uses(ci, zipper, info_map),
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
        inspector_view(
          ~inject,
          ~settings,
          ~show_lang_doc,
          index,
          ci,
          zipper,
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
