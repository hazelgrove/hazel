open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util.Web;

type state = (Id.t, Editor.t);

let view =
    (
      ~inject,
      ~ctx_init: Ctx.t,
      ~model as
        {
          editors,
          settings,
          langDocMessages,
          meta: {
            results,
            ui_state: {font_metrics, show_backpack_targets, mousedown, _},
            _,
          },
        }: Model.t,
    ) => {
  let editor = Editors.get_editor(editors);
  let zipper = editor.state.zipper;
  let (term, _) = MakeTerm.from_zip_for_sem(zipper);
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let result =
    ModelResult.get_simple(
      ModelResults.lookup(results, ScratchSlide.scratch_key),
    );
  let color_highlighting: option(ColorSteps.colorMap) =
    if (langDocMessages.highlight && langDocMessages.show) {
      Some(LangDoc.get_color_map(~settings, ~doc=langDocMessages, zipper));
    } else {
      None;
    };

  let code_id = "code-container";
  let editor_view =
    Cell.editor_with_result_view(
      ~inject,
      ~font_metrics,
      ~show_backpack_targets,
      ~clss=["single"],
      ~selected=true,
      ~mousedown,
      ~code_id,
      ~settings,
      ~color_highlighting,
      ~info_map,
      ~term,
      ~result,
      editor,
    );
  let bottom_bar =
    CursorInspector.view(
      ~inject,
      ~settings,
      ~show_lang_doc=langDocMessages.show,
      zipper,
      info_map,
    );
  let sidebar =
    langDocMessages.show && settings.core.statics
      ? LangDoc.view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~doc=langDocMessages,
          Indicated.index(zipper),
          info_map,
        )
      : div_empty;
  [
    div(
      ~attr=Attr.id("main"),
      [div(~attr=clss(["editor", "single"]), [editor_view])],
    ),
    sidebar,
    bottom_bar,
  ];
};

let download_slide_state = state => {
  let json_data = ScratchSlide.export(state);
  JsUtil.download_json("hazel-scratchpad", json_data);
};
let breadcrumb_bar = (~inject, ~model as {editors, _}: Model.t) => {
  let editor = Editors.get_editor(editors);
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  let empty = [
    select([
      option(
        ~attr=Attr.many([Attr.create("selected", "selected")]),
        [text(".")],
      ),
    ]),
  ];
  switch (zipper.backpack, Indicated.index(zipper)) {
  | ([_, ..._], _) => empty
  | (_, None) => empty
  | (_, Some(id)) =>
    switch (Id.Map.find_opt(id, info_map)) {
    | None => empty
    | Some(ci) =>
      let view_silbings = (name, ids, _) => [
        div(
          ~attr=
            Attr.many([
              clss(["breadcrumb_bar_silbing"]),
              Attr.on_click(_ =>
                inject(
                  UpdateAction.PerformAction(Jump(TileId(List.hd(ids)))),
                )
              ),
            ]),
          [text(name)],
        ),
      ];
      let rec tag_term = (term: TermBase.UExp.t, level: int) => {
        switch (term.term) {
        | Let({term: Var(name), _}, def_body, body) =>
          let l1 = tag_term(body, level);
          let l2 = tag_term(def_body, level + 1);
          List.cons((level, (name, term.ids)), l1) @ l2;
        | Module(
            {term: TypeAnn({term: Constructor(name), _}, _), _},
            t1,
            t2,
          )
        | Module({term: Constructor(name), _}, t1, t2) =>
          let l1 = tag_term(t2, level);
          let l2 = tag_term(t1, level + 1);
          List.cons((level, (name, term.ids)), l1) @ l2;
        | Module(_) => tag_term(term, level + 1)
        | UnOp(_, t1)
        | Fun(_, t1)
        | TyAlias(_, _, t1)
        | Test(t1)
        | Parens(t1)
        | Ap(_, t1) => tag_term(t1, level)
        | Let(_, t1, t2)
        | Seq(t1, t2)
        | Cons(t1, t2)
        | ListConcat(t1, t2)
        | Dot(t1, t2)
        | Pipeline(t1, t2)
        | BinOp(_, t1, t2) =>
          let l1 = tag_term(t1, level);
          let l2 = tag_term(t2, level);
          l1 @ l2;
        | If(t1, t2, t3) =>
          tag_term(t1, level) @ tag_term(t2, level) @ tag_term(t3, level)
        | ListLit(t)
        | Tuple(t) => List.concat(List.map(t1 => tag_term(t1, level), t))
        | Match(t1, t) =>
          let l1 = tag_term(t1, level);
          let l2 =
            List.concat(List.map(t1 => tag_term(snd(t1), level), t));
          l1 @ l2;
        | Constructor(_)
        | Var(_)
        | Int(_)
        | String(_)
        | Bool(_)
        | EmptyHole
        | Triv
        | Invalid(_)
        | Float(_)
        | MultiHole(_) => []
        };
      };
      let combineList = (lst: list((int, (string, list(Uuidm.t))))) => {
        let combine = (acc, (key, id)) =>
          switch (List.assoc_opt(key, acc)) {
          | Some(ids) =>
            List.remove_assoc(key, acc) @ [(key, [id, ...ids])]
          | None => [(key, [id]), ...acc]
          };
        let result = List.fold_left(combine, [], lst);
        List.map(((key, ids)) => (key, List.rev(ids)), result);
      };
      let tagged = tag_term(term, 1);
      let lst = combineList(tagged);
      let ancestors = Info.ancestors_of(ci);
      let rec filter_ancestors = (ancestors_lst: Info.ancestors, level: int) =>
        if (List.length(ancestors_lst) == 0) {
          [];
        } else {
          switch (Id.Map.find_opt(List.hd(ancestors_lst), info_map)) {
          | Some(Info.InfoExp({term, ancestors, _}))
              when List.length(ancestors) >= 1 =>
            switch (term.term) {
            | Let({term: Var(name), _}, _, _)
            | Module({term: Constructor(name), _}, _, _) =>
              let lv = List.find_opt(a => fst(snd(a)) == name, tagged);
              switch (lv) {
              | Some(lv) =>
                if (fst(lv) < level) {
                  filter_ancestors(ancestors_lst, level - 1);
                } else if (fst(lv) == level) {
                  [
                    List.hd(ancestors_lst),
                    ...filter_ancestors(List.tl(ancestors_lst), level - 1),
                  ];
                } else {
                  filter_ancestors(List.tl(ancestors_lst), level);
                }
              | None => filter_ancestors(List.tl(ancestors_lst), level)
              };
            | _ => filter_ancestors(List.tl(ancestors_lst), level)
            }
          | _ => filter_ancestors(List.tl(ancestors_lst), level)
          };
        };
      let ancestors = filter_ancestors(ancestors, List.length(lst));
      let rec breadcrumb_funs = (level, res) =>
        if (level == 0) {
          res;
        } else if (List.find_opt(a => fst(a) == level, lst) == None) {
          breadcrumb_funs(level - 1, res);
        } else {
          let siblings_div =
            List.map(
              t =>
                if (List.exists(a => a == List.hd(snd(t)), ancestors)) {
                  option(
                    ~attr=Attr.many([Attr.create("selected", "selected")]),
                    view_silbings(fst(t), snd(t), level),
                  );
                } else {
                  option(
                    ~attr=Attr.many([]),
                    view_silbings(fst(t), snd(t), level),
                  );
                },
              List.rev(snd(List.find(a => fst(a) == level, lst))),
            );
          if (siblings_div == []) {
            breadcrumb_funs(level - 1, res);
          } else {
            let toggle_div = {
              [
                select(
                  ~attr=
                    Attr.on_change((_, name) => {
                      let all_siblings =
                        snd(List.find(a => fst(a) == level, lst));
                      let selected_siblings =
                        List.hd(
                          List.filter(a => fst(a) == name, all_siblings),
                        );
                      inject(
                        UpdateAction.PerformAction(
                          Jump(TileId(List.hd(snd(selected_siblings)))),
                        ),
                      );
                    }),
                  siblings_div,
                ),
              ];
            };
            let ret = toggle_div;
            let ret =
              if (res == []) {
                ret;
              } else if (siblings_div == []) {
                ret;
              } else {
                ret @ [text("->")] @ res;
              };
            breadcrumb_funs(level - 1, ret);
          };
        };
      let res = breadcrumb_funs(List.length(ancestors) + 1, []);
      if (res == []) {
        empty;
      } else {
        res;
      };
    }
  };
};
let toolbar_buttons = (~inject, state: ScratchSlide.state) => {
  let export_button =
    Widgets.button(
      Icons.export,
      _ => {
        download_slide_state(state);
        Virtual_dom.Vdom.Effect.Ignore;
      },
      ~tooltip="Export Scratchpad",
    );
  let import_button =
    Widgets.file_select_button(
      "import-scratchpad",
      Icons.import,
      file => {
        switch (file) {
        | None => Virtual_dom.Vdom.Effect.Ignore
        | Some(file) => inject(UpdateAction.InitImportScratchpad(file))
        }
      },
      ~tooltip="Import Scratchpad",
    );

  let reset_button =
    Widgets.button(
      Icons.trash,
      _ => {
        let confirmed =
          JsUtil.confirm(
            "Are you SURE you want to reset this scratchpad? You will lose any existing code.",
          );
        if (confirmed) {
          inject(ResetCurrentEditor);
        } else {
          Virtual_dom.Vdom.Effect.Ignore;
        };
      },
      ~tooltip="Reset Scratchpad",
    );
  [export_button, import_button] @ [reset_button];
};
