open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

/* If you are adding docs here for new syntax, see PipelineExp.re
 * which documents the simplest way to add a new form. */

let feedback_view = (message, up_active, up_action, down_active, down_action) => {
  div(
    ~attrs=[clss(["feedback"])],
    [
      div(~attrs=[clss(["message"])], [text(message)]),
      div(
        ~attrs=[
          clss(["option"] @ (up_active ? ["active"] : [])),
          Attr.on_click(up_action),
        ],
        [text("👍")],
      ),
      div(
        ~attrs=[
          clss(["option"] @ (down_active ? ["active"] : [])),
          Attr.on_click(down_action),
        ],
        [text("👎")],
      ),
    ],
  );
};

let explanation_feedback_view = (~inject, group_id, form_id, model) => {
  let (up_active, down_active) =
    switch (
      ExplainThisModel.get_explanation_feedback(group_id, form_id, model)
    ) {
    | Some(ThumbsUp) => (true, false)
    | Some(ThumbsDown) => (false, true)
    | None => (false, false)
    };
  feedback_view(
    "This explanation is helpful",
    up_active,
    _ =>
      inject(
        UpdateAction.UpdateExplainThisModel(
          ToggleExplanationFeedback(group_id, form_id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        UpdateAction.UpdateExplainThisModel(
          ToggleExplanationFeedback(group_id, form_id, ThumbsDown),
        ),
      ),
  );
};

let example_feedback_view = (~inject, group_id, form_id, example_id, model) => {
  let (up_active, down_active) =
    switch (
      ExplainThisModel.get_example_feedback(
        group_id,
        form_id,
        example_id,
        model,
      )
    ) {
    | Some(ThumbsUp) => (true, false)
    | Some(ThumbsDown) => (false, true)
    | None => (false, false)
    };
  feedback_view(
    "This example is helpful",
    up_active,
    _ =>
      inject(
        UpdateAction.UpdateExplainThisModel(
          ToggleExampleFeedback(group_id, form_id, example_id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        UpdateAction.UpdateExplainThisModel(
          ToggleExampleFeedback(group_id, form_id, example_id, ThumbsDown),
        ),
      ),
  );
};

let code_node = text =>
  Node.span(~attrs=[clss(["code"])], [Node.text(text)]);

let highlight =
    (~inject, msg: list(Node.t), id: Id.t, mapping: ColorSteps.t)
    : (Node.t, ColorSteps.t) => {
  let (c, mapping) = ColorSteps.get_color(id, mapping);
  let classes = clss(["highlight-" ++ c, "clickable"]);
  let attrs =
    switch (inject) {
    | Some(inject) => [
        classes,
        Attr.on_mouseenter(_ =>
          inject(UpdateAction.Set(ExplainThis(SetHighlight(Hover(id)))))
        ),
        Attr.on_mouseleave(_ =>
          inject(UpdateAction.Set(ExplainThis(SetHighlight(UnsetHover))))
        ),
        Attr.on_click(_ =>
          inject(UpdateAction.PerformAction(Select(Term(Id(id, Left)))))
        ),
      ]
    | None => [classes]
    };
  (Node.span(~attrs, msg), mapping);
};

/*
 Markdown like thing:
 highlighty thing : [thing to highlight](id)
 bulleted list: - list item
                - list item
 code: `code`
 italics: *word*
 */
let mk_translation = (~inject, text: string): (list(Node.t), ColorSteps.t) => {
  let omd = Omd.of_string(text);
  //print_markdown(omd);
  let rec translate =
          (doc: Omd.t, mapping: ColorSteps.t): (list(Node.t), ColorSteps.t) =>
    List.fold_left(
      ((msg, mapping), elem) => {
        switch (elem) {
        | Omd.Paragraph(d) => translate(d, mapping)
        | Text(t) => (List.append(msg, [Node.text(t)]), mapping)
        | Ul(items) =>
          let (bullets, mapping) =
            List.fold_left(
              ((nodes, mapping), d) => {
                let (n, mapping) = translate(d, mapping);
                (List.append(nodes, [Node.li(n)]), mapping);
              },
              ([], mapping),
              items,
            );
          (List.append(msg, [Node.ul(bullets)]), mapping); /* TODO Hannah - Should this be an ordered list instead of an unordered list? */
        | Code(_name, t) => (List.append(msg, [code_node(t)]), mapping)
        | Url(id, d, _title) =>
          let (d, mapping) = translate(d, mapping);
          let id =
            switch (Id.of_string(id)) {
            | Some(id) => id
            | None => Id.invalid
            };
          let (inner_msg, mapping) = highlight(~inject, d, id, mapping);
          (List.append(msg, [inner_msg]), mapping);
        | Emph(d) =>
          let (d, mapping) = translate(d, mapping);
          (
            List.append(
              msg,
              [
                Node.span(
                  ~attrs=[
                    Attr.style(
                      Css_gen.create(~field="font-style", ~value="italic"),
                    ),
                  ],
                  d,
                ),
              ],
            ),
            mapping,
          );
        | _ => (msg, mapping)
        }
      },
      ([], mapping),
      doc,
    );
  translate(omd, ColorSteps.empty);
};

let mk_explanation =
    (
      ~inject,
      ~settings: Settings.t,
      group_id,
      form_id,
      text: string,
      model: ExplainThisModel.t,
    )
    : (Node.t, ColorSteps.t) => {
  let (msg, color_map) = mk_translation(~inject=Some(inject), text);
  let feedback =
    settings.explainThis.show_feedback
      ? [explanation_feedback_view(~inject, group_id, form_id, model)] : [];
  (
    div([div(~attrs=[clss(["explanation-contents"])], msg)] @ feedback),
    color_map,
  );
};

let expander_deco =
    (
      ~docs: ExplainThisModel.t,
      ~settings: Settings.t,
      ~inject,
      ~ui_state as {font_metrics, _}: Model.ui_state,
      ~options: list((ExplainThisForm.form_id, Segment.t)),
      ~group: ExplainThisForm.group,
      ~doc: ExplainThisForm.form,
    ) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = Measured.of_segment(doc.syntactic_form);
      let show_backpack_targets = false;
      let (_term, terms) = MakeTerm.go(doc.syntactic_form);
      let term_ranges = TermRanges.mk(doc.syntactic_form);
      let tiles = TileMap.mk(doc.syntactic_form);
      let error_ids = [];
    });
  switch (doc.expandable_id, List.length(options)) {
  | (None, _)
  | (_, 0 | 1) => div([])
  | (Some((expandable, _)), _) =>
    Deco.term_decoration(
      ~id=expandable,
      ((origin, _, path)) => {
        let specificity_pos =
          Printf.sprintf(
            "position: absolute; top: %fpx;",
            font_metrics.row_height,
          );

        let specificity_style =
          Attr.create(
            "style",
            specificity_pos
            ++ (docs.specificity_open ? "transform: scaleY(1);" : ""),
          );

        let get_clss = segment =>
          switch (List.nth(segment, 0)) {
          | Base.Tile({mold, _}) => [
              "ci-header-" ++ Sort.to_string(mold.out) // TODO the brown on brown isn't the greatest... but okay
            ]
          | _ => []
          };

        let specificity_menu =
          Node.div(
            ~attrs=[
              clss(["specificity-options-menu", "expandable"]),
              specificity_style,
            ],
            List.map(
              ((id: ExplainThisForm.form_id, segment: Segment.t)): Node.t => {
                let map = Measured.of_segment(segment);
                let code_view =
                  Code.simple_view(
                    ~font_metrics,
                    ~unselected=segment,
                    ~map,
                    ~settings,
                  );
                let classes =
                  id == doc.id
                    ? ["selected"] @ get_clss(segment) : get_clss(segment);
                let update_group_selection = _ =>
                  inject(
                    UpdateAction.UpdateExplainThisModel(
                      ExplainThisUpdate.UpdateGroupSelection(group.id, id),
                    ),
                  );
                Node.div(
                  ~attrs=[
                    clss(classes),
                    Attr.on_click(update_group_selection),
                  ],
                  [code_view],
                );
              },
              options,
            ),
          );

        let expand_arrow_style = Attr.create("style", specificity_pos);
        let expand_arrow =
          Node.div(~attrs=[clss(["arrow"]), expand_arrow_style], []);

        let expandable_deco =
          DecUtil.code_svg(
            ~font_metrics,
            ~origin,
            ~base_cls=["expandable"],
            ~abs_pos=false,
            path,
          );

        Node.div(
          ~attrs=[
            clss(["expandable-target"]),
            DecUtil.abs_position(~font_metrics, origin),
            Attr.on_click(_ => {
              inject(
                UpdateAction.UpdateExplainThisModel(
                  ExplainThisUpdate.SpecificityOpen(!docs.specificity_open),
                ),
              )
            }),
          ],
          [expandable_deco, specificity_menu]
          @ (docs.specificity_open ? [] : [expand_arrow]),
        );
      },
    )
  };
};

let example_view =
    (
      ~inject,
      ~ui_state,
      ~settings: Settings.t,
      ~group_id,
      ~form_id,
      ~examples: list(ExplainThisForm.example),
      ~model: ExplainThisModel.t,
    ) => {
  examples == []
    ? []
    : [
      div(
        ~attrs=[Attr.id("examples")],
        List.mapi(
          (idx, {term, message, sub_id, _}: ExplainThisForm.example) => {
            let feedback =
              settings.explainThis.show_feedback
                ? [
                  example_feedback_view(
                    ~inject,
                    group_id,
                    form_id,
                    sub_id,
                    model,
                  ),
                ]
                : [];
            div(
              ~attrs=[clss(["example"])],
              [
                Cell.locked(
                  ~segment=term,
                  ~target_id="example" ++ string_of_int(idx),
                  ~ui_state,
                  ~settings,
                  ~inject,
                ),
                div(
                  ~attrs=[clss(["explanation"])],
                  [text(message)] @ feedback,
                ),
              ],
            );
          },
          examples,
        ),
      ),
    ];
};

let rec bypass_parens_and_annot_pat = (pat: TermBase.UPat.t) => {
  switch (pat.term) {
  | Parens(p)
  | TypeAnn(p, _) => bypass_parens_and_annot_pat(p)
  | _ => pat
  };
};

let rec bypass_parens_pat = (pat: TermBase.UPat.t) => {
  switch (pat.term) {
  | Parens(p) => bypass_parens_pat(p)
  | _ => pat
  };
};

let rec bypass_parens_exp = (exp: TermBase.UExp.t) => {
  switch (exp.term) {
  | Parens(e) => bypass_parens_exp(e)
  | _ => exp
  };
};

let rec bypass_parens_typ = (typ: TermBase.UTyp.t) => {
  switch (typ.term) {
  | Parens(t) => bypass_parens_typ(t)
  | _ => typ
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type message_mode =
  | MessageContent(
      UpdateAction.t => Virtual_dom.Vdom.Effect.t(unit),
      Model.ui_state,
      Settings.t,
    )
  | Colorings;

let get_doc =
    (
      ~docs: ExplainThisModel.t,
      info: option(Statics.Info.t),
      mode: message_mode,
    )
    : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
  let simple = msg => ([], ([text(msg)], (Id.Map.empty, 0)), []);
  let default = simple("No docs available");
  let get_specificity_level = group_id =>
    fst(ExplainThisModel.get_form_and_options(group_id, docs)).id;
  let get_message =
      (
        ~colorings=[],
        ~format: option(string => string)=None,
        ~explanation: option(string)=?,
        group: ExplainThisForm.group,
      )
      : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
    let (doc, options) = ExplainThisModel.get_form_and_options(group, docs);

    // https://stackoverflow.com/questions/31998408/ocaml-converting-strings-to-a-unit-string-format
    let explanation_msg =
      switch (explanation, format) {
      | (Some(msg), _) => msg
      | (_, Some(f)) => f(doc.explanation)
      | (_, None) => doc.explanation
      };
    switch (mode) {
    | MessageContent(inject, ui_state, settings) =>
      let (explanation, color_map) =
        mk_explanation(
          ~settings,
          ~inject,
          group.id,
          doc.id,
          explanation_msg,
          docs,
        );
      let sort =
        switch (info) {
        | None => Sort.Any
        | Some(ci) => Info.sort_of(ci)
        };
      let highlights =
        colorings
        |> List.map(((syntactic_form_id: Id.t, code_id: Id.t)) => {
             let (color, _) = ColorSteps.get_color(code_id, color_map);
             (syntactic_form_id, color);
           })
        |> List.to_seq
        |> Id.Map.of_seq
        |> Option.some;
      let expander_deco =
        expander_deco(
          ~docs,
          ~settings,
          ~inject,
          ~ui_state,
          ~options,
          ~group,
          ~doc,
        );
      let syntactic_form_view =
        Cell.locked_no_statics(
          ~target_id="explainThisSyntacticForm",
          ~inject,
          ~ui_state,
          ~segment=doc.syntactic_form,
          ~highlights,
          ~settings,
          ~sort,
          ~expander_deco,
        );
      let example_view =
        example_view(
          ~inject,
          ~ui_state,
          ~settings,
          ~group_id=group.id,
          ~form_id=doc.id,
          ~examples=doc.examples,
          ~model=docs,
        );
      (syntactic_form_view, ([explanation], color_map), example_view);
    | Colorings =>
      let (_, color_map) = mk_translation(~inject=None, explanation_msg);
      ([], ([], color_map), []);
    };
  };

  /* Use this when adding new entries */
  let message_single = (e: ExplainThisForm.Simple.t) => {
    let (explanation, colorings, group) = ExplainThisForm.Simple.to_group(e);
    get_message(~colorings, ~format=None, ~explanation, group);
  };

  switch (info) {
  | Some(InfoExp({term, _})) =>
    let rec get_message_exp =
            (term)
            : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) =>
      switch (term) {
      | TermBase.UExp.Invalid(_) => simple("Not a valid expression")
      | EmptyHole => get_message(HoleExp.empty_hole_exps)
      | MultiHole(_children) => get_message(HoleExp.multi_hole_exps)
      | TyAlias(ty_pat, ty_def, _body) =>
        let tpat_id = List.nth(ty_pat.ids, 0);
        let def_id = List.nth(ty_def.ids, 0);
        get_message(
          ~colorings=
            TyAliasExp.tyalias_base_exp_coloring_ids(~tpat_id, ~def_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s"),
                  Id.to_string(def_id),
                  Id.to_string(tpat_id),
                ),
            ),
          TyAliasExp.tyalias_exps,
        );
      | Triv => get_message(TerminalExp.triv_exps)
      | Deferral(_) => get_message(TerminalExp.deferral_exps)
      | Bool(b) => get_message(TerminalExp.bool_exps(b))
      | Int(i) => get_message(TerminalExp.int_exps(i))
      | Float(f) => get_message(TerminalExp.float_exps(f))
      | String(s) => get_message(TerminalExp.string_exps(s))
      | ListLit(terms) =>
        get_message(
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s"),
                  string_of_int(List.length(terms)),
                ),
            ),
          ListExp.listlits,
        )
      | TypFun(tpat, body) =>
        let basic = group_id => {
          let tpat_id = List.nth(tpat.ids, 0);
          let body_id = List.nth(body.ids, 0);
          get_message(
            ~colorings=
              FunctionExp.function_exp_coloring_ids(
                ~pat_id=tpat_id,
                ~body_id,
              ),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s"),
                    Id.to_string(tpat_id),
                    Id.to_string(body_id),
                  ),
              ),
            group_id,
          );
        };
        /* TODO: More could be done here probably for different patterns. */
        basic(TypFunctionExp.type_functions_basic);
      | Fun(pat, body) =>
        let basic = group_id => {
          let pat_id = List.nth(pat.ids, 0);
          let body_id = List.nth(body.ids, 0);
          get_message(
            ~colorings=
              FunctionExp.function_exp_coloring_ids(~pat_id, ~body_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s"),
                    Id.to_string(pat_id),
                    Id.to_string(body_id),
                  ),
              ),
            group_id,
          );
        };
        let pat = bypass_parens_and_annot_pat(pat);
        let pat_id = List.nth(pat.ids, 0);
        let body_id = List.nth(body.ids, 0);
        switch (pat.term) {
        | EmptyHole =>
          if (FunctionExp.function_empty_hole_exp.id
              == get_specificity_level(FunctionExp.functions_empty_hole)) {
            get_message(
              ~colorings=
                FunctionExp.function_empty_hole_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                      Id.to_string(pat_id),
                    ),
                ),
              FunctionExp.functions_empty_hole,
            );
          } else {
            basic(FunctionExp.functions_empty_hole);
          }
        | MultiHole(_) =>
          if (FunctionExp.function_multi_hole_exp.id
              == get_specificity_level(FunctionExp.functions_multi_hole)) {
            get_message(
              ~colorings=
                FunctionExp.function_multi_hole_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                      Id.to_string(pat_id),
                    ),
                ),
              FunctionExp.functions_multi_hole,
            );
          } else {
            basic(FunctionExp.functions_multi_hole);
          }
        | Wild =>
          if (FunctionExp.function_wild_exp.id
              == get_specificity_level(FunctionExp.functions_wild)) {
            get_message(
              ~colorings=FunctionExp.function_wild_exp_coloring_ids(~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s"),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_wild,
            );
          } else {
            basic(FunctionExp.functions_wild);
          }
        | Int(i) =>
          if (FunctionExp.function_intlit_exp.id
              == get_specificity_level(FunctionExp.functions_int)) {
            get_message(
              ~colorings=
                FunctionExp.function_intlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s"),
                      Id.to_string(pat_id),
                      string_of_int(i),
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_int,
            );
          } else {
            basic(FunctionExp.functions_int);
          }
        | Float(f) =>
          if (FunctionExp.function_floatlit_exp.id
              == get_specificity_level(FunctionExp.functions_float)) {
            get_message(
              ~colorings=
                FunctionExp.function_floatlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%f%s%s"),
                      Id.to_string(pat_id),
                      f,
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_float,
            );
          } else {
            basic(FunctionExp.functions_float);
          }
        | Bool(b) =>
          if (FunctionExp.function_boollit_exp.id
              == get_specificity_level(FunctionExp.functions_bool)) {
            get_message(
              ~colorings=
                FunctionExp.function_boollit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%b%s%s"),
                      Id.to_string(pat_id),
                      b,
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_bool,
            );
          } else {
            basic(FunctionExp.functions_bool);
          }
        | String(s) =>
          if (FunctionExp.function_strlit_exp.id
              == get_specificity_level(FunctionExp.functions_str)) {
            get_message(
              ~colorings=
                FunctionExp.function_strlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s"),
                      Id.to_string(pat_id),
                      s,
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_str,
            );
          } else {
            basic(FunctionExp.functions_str);
          }
        | Triv =>
          if (FunctionExp.function_triv_exp.id
              == get_specificity_level(FunctionExp.functions_triv)) {
            get_message(
              ~colorings=
                FunctionExp.function_triv_exp_coloring_ids(~pat_id, ~body_id),
              // HANNAH TODO - think could move this format thing like into the colorings
              // functions
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(pat_id),
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_triv,
            );
          } else {
            basic(FunctionExp.functions_triv);
          }
        | ListLit(elements) =>
          if (List.length(elements) == 0) {
            if (FunctionExp.function_listnil_exp.id
                == get_specificity_level(FunctionExp.functions_listnil)) {
              get_message(
                ~colorings=
                  FunctionExp.function_listnil_exp_coloring_ids(
                    ~pat_id,
                    ~body_id,
                  ),
                ~format=
                  Some(
                    msg =>
                      Printf.sprintf(
                        Scanf.format_from_string(msg, "%s%s%s"),
                        Id.to_string(pat_id),
                        Id.to_string(pat_id),
                        Id.to_string(body_id),
                      ),
                  ),
                FunctionExp.functions_listnil,
              );
            } else {
              basic(FunctionExp.functions_listnil);
            };
          } else if (FunctionExp.function_listlit_exp.id
                     == get_specificity_level(FunctionExp.functions_listlit)) {
            get_message(
              ~colorings=
                FunctionExp.function_listlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s"),
                      Id.to_string(pat_id),
                      string_of_int(List.length(elements)),
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_listlit,
            );
          } else {
            basic(FunctionExp.functions_listlit);
          }
        | Cons(hd, tl) =>
          if (FunctionExp.function_cons_exp.id
              == get_specificity_level(FunctionExp.functions_cons)) {
            let hd_id = List.nth(hd.ids, 0);
            let tl_id = List.nth(tl.ids, 0);
            get_message(
              ~colorings=
                FunctionExp.function_cons_exp_coloring_ids(
                  ~hd_id,
                  ~tl_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(hd_id),
                      Id.to_string(tl_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_cons,
            );
          } else {
            basic(FunctionExp.functions_cons);
          }
        | Var(var) =>
          if (FunctionExp.function_var_exp.id
              == get_specificity_level(FunctionExp.functions_var)) {
            get_message(
              ~colorings=
                FunctionExp.function_var_exp_coloring_ids(~pat_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(pat_id),
                      var,
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_var,
            );
          } else {
            basic(FunctionExp.functions_var);
          }
        | Tuple(elements) =>
          let pat_id = List.nth(pat.ids, 0);
          let body_id = List.nth(body.ids, 0);
          let basic_tuple = group_id => {
            get_message(
              ~colorings=
                FunctionExp.function_tuple_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s"),
                      Id.to_string(pat_id),
                      string_of_int(List.length(elements)),
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              group_id,
            );
          };

          switch (List.length(elements)) {
          | 2 =>
            let doc_id = get_specificity_level(FunctionExp.functions_tuple2);
            if (FunctionExp.function_tuple2_exp.id == doc_id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              get_message(
                ~colorings=
                  FunctionExp.function_tuple2_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~body_id,
                  ),
                ~format=
                  Some(
                    msg =>
                      Printf.sprintf(
                        Scanf.format_from_string(msg, "%s%s%s"),
                        Id.to_string(pat1_id),
                        Id.to_string(pat2_id),
                        Id.to_string(body_id),
                      ),
                  ),
                FunctionExp.functions_tuple2,
              );
            } else if (FunctionExp.function_tuple_exp.id == doc_id) {
              basic_tuple(FunctionExp.functions_tuple2);
            } else {
              basic(FunctionExp.functions_tuple2);
            };
          | 3 =>
            let doc_id = get_specificity_level(FunctionExp.functions_tuple3);
            if (FunctionExp.function_tuple3_exp.id == doc_id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              let pat3_id = List.nth(List.nth(elements, 2).ids, 0);
              get_message(
                ~colorings=
                  FunctionExp.function_tuple3_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~pat3_id,
                    ~body_id,
                  ),
                ~format=
                  Some(
                    msg =>
                      Printf.sprintf(
                        Scanf.format_from_string(msg, "%s%s%s%s"),
                        Id.to_string(pat1_id),
                        Id.to_string(pat2_id),
                        Id.to_string(pat3_id),
                        Id.to_string(body_id),
                      ),
                  ),
                FunctionExp.functions_tuple3,
              );
            } else if (FunctionExp.function_tuple_exp.id == doc_id) {
              basic_tuple(FunctionExp.functions_tuple3);
            } else {
              basic(FunctionExp.functions_tuple3);
            };
          | _ =>
            if (FunctionExp.function_tuple_exp.id
                == get_specificity_level(FunctionExp.functions_tuple)) {
              basic_tuple(FunctionExp.functions_tuple);
            } else {
              basic(FunctionExp.functions_tuple);
            }
          };
        | Ap(con, arg) =>
          if (FunctionExp.function_ap_exp.id
              == get_specificity_level(FunctionExp.functions_ap)) {
            let con_id = List.nth(con.ids, 0);
            let arg_id = List.nth(arg.ids, 0);
            get_message(
              ~colorings=
                FunctionExp.function_ap_exp_coloring_ids(
                  ~con_id,
                  ~arg_id,
                  ~body_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(con_id),
                      Id.to_string(arg_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_ap,
            );
          } else {
            basic(FunctionExp.functions_ap);
          }
        | Constructor(v) =>
          if (FunctionExp.function_ctr_exp.id
              == get_specificity_level(FunctionExp.functions_ctr)) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              ~colorings=
                FunctionExp.function_ctr_exp_coloring_ids(~pat_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s"),
                      Id.to_string(pat_id),
                      v,
                      Id.to_string(pat_id),
                      Id.to_string(body_id),
                    ),
                ),
              FunctionExp.functions_ctr,
            );
          } else {
            basic(FunctionExp.functions_ctr);
          }
        | Invalid(_) => default // Shouldn't get hit
        | Parens(_) => default // Shouldn't get hit?
        | TypeAnn(_) => default // Shouldn't get hit?
        };
      | Tuple(terms) =>
        let basic = group_id =>
          get_message(
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s"),
                    string_of_int(List.length(terms)),
                  ),
              ),
            group_id,
          );
        switch (List.length(terms)) {
        | 2 =>
          if (TupleExp.tuple_exp_size2.id
              == get_specificity_level(TupleExp.tuples2)) {
            let exp1_id = List.nth(List.nth(terms, 0).ids, 0);
            let exp2_id = List.nth(List.nth(terms, 1).ids, 0);
            get_message(
              ~colorings=
                TupleExp.tuple_exp_size2_coloring_ids(~exp1_id, ~exp2_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s"),
                      Id.to_string(exp1_id),
                      Id.to_string(exp2_id),
                    ),
                ),
              TupleExp.tuples2,
            );
          } else {
            basic(TupleExp.tuples2);
          }
        | 3 =>
          if (TupleExp.tuple_exp_size3.id
              == get_specificity_level(TupleExp.tuples3)) {
            let exp1_id = List.nth(List.nth(terms, 0).ids, 0);
            let exp2_id = List.nth(List.nth(terms, 1).ids, 0);
            let exp3_id = List.nth(List.nth(terms, 2).ids, 0);
            get_message(
              ~colorings=
                TupleExp.tuple_exp_size3_coloring_ids(
                  ~exp1_id,
                  ~exp2_id,
                  ~exp3_id,
                ),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(exp1_id),
                      Id.to_string(exp2_id),
                      Id.to_string(exp3_id),
                    ),
                ),
              TupleExp.tuples3,
            );
          } else {
            basic(TupleExp.tuples3);
          }
        | _ => basic(TupleExp.tuples)
        };
      | Var(n) => get_message(TerminalExp.var_exps(n))
      | Let(pat, def, body) =>
        let pat = bypass_parens_and_annot_pat(pat);
        let pat_id = List.nth(pat.ids, 0);
        let def_id = List.nth(def.ids, 0);
        let body_id = List.nth(body.ids, 0);
        let basic = group_id => {
          get_message(
            ~colorings=LetExp.let_base_exp_coloring_ids(~pat_id, ~def_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s"),
                    Id.to_string(def_id),
                    Id.to_string(pat_id),
                  ),
              ),
            group_id,
          );
        };
        switch (pat.term) {
        | EmptyHole =>
          if (LetExp.let_empty_hole_exp.id
              == get_specificity_level(LetExp.lets_emptyhole)) {
            get_message(
              ~colorings=
                LetExp.let_empty_hole_exp_coloring_ids(~pat_id, ~def_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(pat_id),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                    ),
                ),
              LetExp.lets_emptyhole,
            );
          } else {
            basic(LetExp.lets_emptyhole);
          }
        | MultiHole(_) =>
          if (LetExp.let_multi_hole_exp.id
              == get_specificity_level(LetExp.lets_mutlihole)) {
            get_message(
              ~colorings=
                LetExp.let_multi_hole_exp_coloring_ids(~pat_id, ~def_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(pat_id),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                    ),
                ),
              LetExp.lets_mutlihole,
            );
          } else {
            basic(LetExp.lets_mutlihole);
          }
        | Wild =>
          if (LetExp.let_wild_exp.id
              == get_specificity_level(LetExp.lets_wild)) {
            get_message(
              ~colorings=LetExp.let_wild_exp_coloring_ids(~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(def_id),
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_wild,
            );
          } else {
            basic(LetExp.lets_wild);
          }
        | Int(i) =>
          if (LetExp.let_int_exp.id == get_specificity_level(LetExp.lets_int)) {
            get_message(
              ~colorings=
                LetExp.let_int_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      string_of_int(i),
                      Id.to_string(def_id),
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_int,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and some other places when switching between forms and specificity levels... maybe a Safari issue... */
            basic(
              LetExp.lets_int,
            );
          }
        | Float(f) =>
          if (LetExp.let_float_exp.id
              == get_specificity_level(LetExp.lets_float)) {
            // TODO Make sure everywhere printing the float literal print it prettier
            get_message(
              ~colorings=
                LetExp.let_float_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%f%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      f,
                      Id.to_string(def_id),
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_float,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              LetExp.lets_float,
            );
          }
        | Bool(b) =>
          if (LetExp.let_bool_exp.id
              == get_specificity_level(LetExp.lets_bool)) {
            get_message(
              ~colorings=
                LetExp.let_bool_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%b%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      b,
                      Id.to_string(def_id),
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_bool,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              LetExp.lets_bool,
            );
          }
        | String(s) =>
          if (LetExp.let_str_exp.id == get_specificity_level(LetExp.lets_str)) {
            get_message(
              ~colorings=
                LetExp.let_str_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      s,
                      Id.to_string(def_id),
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_str,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              LetExp.lets_str,
            );
          }
        | Triv =>
          if (LetExp.let_triv_exp.id
              == get_specificity_level(LetExp.lets_triv)) {
            get_message(
              ~colorings=
                LetExp.let_triv_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      Id.to_string(def_id),
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_triv,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and other places when switching syntactic specificities... seems like might be Safari issue... */
            basic(
              LetExp.lets_triv,
            );
          }
        | ListLit(elements) =>
          if (List.length(elements) == 0) {
            if (LetExp.let_listnil_exp.id
                == get_specificity_level(LetExp.lets_listnil)) {
              get_message(
                ~colorings=
                  LetExp.let_listnil_exp_coloring_ids(
                    ~pat_id,
                    ~def_id,
                    ~body_id,
                  ),
                ~format=
                  Some(
                    msg =>
                      Printf.sprintf(
                        Scanf.format_from_string(msg, "%s%s%s%s"),
                        Id.to_string(def_id),
                        Id.to_string(pat_id),
                        Id.to_string(def_id),
                        Id.to_string(body_id),
                      ),
                  ),
                LetExp.lets_listnil,
              );
            } else {
              basic(LetExp.lets_listnil);
            };
          } else if (LetExp.let_listlit_exp.id
                     == get_specificity_level(LetExp.lets_listlit)) {
            get_message(
              ~colorings=
                LetExp.let_listlit_exp_coloring_ids(~pat_id, ~def_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      string_of_int(List.length(elements)),
                    ),
                ),
              LetExp.lets_listlit,
            );
          } else {
            basic(LetExp.lets_listlit);
          }
        | Cons(hd, tl) =>
          if (LetExp.let_cons_exp.id
              == get_specificity_level(LetExp.lets_cons)) {
            let hd_id = List.nth(hd.ids, 0);
            let tl_id = List.nth(tl.ids, 0);
            get_message(
              ~colorings=
                LetExp.let_cons_exp_coloring_ids(~hd_id, ~tl_id, ~def_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(hd_id),
                      Id.to_string(tl_id),
                    ),
                ),
              LetExp.lets_cons,
            );
          } else {
            basic(LetExp.lets_cons);
          }
        | Var(var) =>
          if (LetExp.let_var_exp.id == get_specificity_level(LetExp.lets_var)) {
            get_message(
              ~colorings=
                LetExp.let_var_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      var,
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_var,
            );
          } else {
            basic(LetExp.lets_var);
          }
        | Tuple(elements) =>
          let basic_tuple = group_id => {
            get_message(
              ~colorings=LetExp.let_tuple_exp_coloring_ids(~pat_id, ~def_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      string_of_int(List.length(elements)),
                    ),
                ),
              group_id,
            );
          };

          switch (List.length(elements)) {
          | 2 =>
            let doc_id = get_specificity_level(LetExp.lets_tuple2);
            if (LetExp.let_tuple2_exp.id == doc_id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              get_message(
                ~colorings=
                  LetExp.let_tuple2_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~def_id,
                  ),
                ~format=
                  Some(
                    msg =>
                      Printf.sprintf(
                        Scanf.format_from_string(msg, "%s%s%s"),
                        Id.to_string(def_id),
                        Id.to_string(pat1_id),
                        Id.to_string(pat2_id),
                      ),
                  ),
                LetExp.lets_tuple2,
              );
            } else if (LetExp.let_tuple_exp.id == doc_id) {
              basic_tuple(LetExp.lets_tuple2);
            } else {
              basic(LetExp.lets_tuple2);
            };
          | 3 =>
            let doc_id = get_specificity_level(LetExp.lets_tuple3);
            // TODO Syntactic form can go off page - so can examples - but can scroll, just can't see bottom scroll bar
            if (LetExp.let_tuple3_exp.id == doc_id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              let pat3_id = List.nth(List.nth(elements, 2).ids, 0);
              get_message(
                ~colorings=
                  LetExp.let_tuple3_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~pat3_id,
                    ~def_id,
                  ),
                ~format=
                  Some(
                    msg =>
                      Printf.sprintf(
                        Scanf.format_from_string(msg, "%s%s%s%s"),
                        Id.to_string(def_id),
                        Id.to_string(pat1_id),
                        Id.to_string(pat2_id),
                        Id.to_string(pat3_id),
                      ),
                  ),
                LetExp.lets_tuple3,
              );
            } else if (LetExp.let_tuple_exp.id == doc_id) {
              basic_tuple(LetExp.lets_tuple3);
            } else {
              basic(LetExp.lets_tuple3);
            };
          | _ =>
            if (LetExp.let_tuple_exp.id
                == get_specificity_level(LetExp.lets_tuple)) {
              basic_tuple(LetExp.lets_tuple);
            } else {
              basic(LetExp.lets_tuple);
            }
          };
        | Ap(con, arg) =>
          if (LetExp.let_ap_exp.id == get_specificity_level(LetExp.lets_ap)) {
            let con_id = List.nth(con.ids, 0);
            let arg_id = List.nth(arg.ids, 0);
            get_message(
              ~colorings=
                LetExp.let_ap_exp_coloring_ids(~con_id, ~arg_id, ~def_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(con_id),
                      Id.to_string(arg_id),
                    ),
                ),
              LetExp.lets_ap,
            );
          } else {
            basic(LetExp.lets_ap);
          }
        | Constructor(v) =>
          if (LetExp.let_ctr_exp.id == get_specificity_level(LetExp.lets_ctr)) {
            get_message(
              ~colorings=
                LetExp.let_ctr_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%s%s%s%s%s"),
                      Id.to_string(def_id),
                      Id.to_string(pat_id),
                      v,
                      Id.to_string(def_id),
                      Id.to_string(body_id),
                    ),
                ),
              LetExp.lets_ctr,
            );
          } else {
            basic(LetExp.lets_ctr);
          }
        | Invalid(_) => default // Shouldn't get hit
        | Parens(_) => default // Shouldn't get hit?
        | TypeAnn(_) => default // Shouldn't get hit?
        };
      | Pipeline(arg, fn) =>
        message_single(
          PipelineExp.single(
            ~arg_id=Term.UExp.rep_id(arg),
            ~fn_id=Term.UExp.rep_id(fn),
          ),
        )
      | TypAp(f, typ) =>
        let f_id = List.nth(f.ids, 0);
        let typ_id = List.nth(typ.ids, 0);
        let basic = (group, format, coloring_ids) => {
          get_message(
            ~colorings=coloring_ids(~f_id, ~typ_id),
            ~format=Some(format),
            group,
          );
        };
        basic(
          TypAppExp.typfunaps,
          msg =>
            Printf.sprintf(
              Scanf.format_from_string(msg, "%s%s"),
              Id.to_string(f_id),
              Id.to_string(typ_id),
            ),
          TypAppExp.typfunapp_exp_coloring_ids,
        );

      | Ap(x, arg) =>
        let x_id = List.nth(x.ids, 0);
        let arg_id = List.nth(arg.ids, 0);
        let basic = (group, format, coloring_ids) => {
          get_message(
            ~colorings=coloring_ids(~x_id, ~arg_id),
            ~format=Some(format),
            group,
          );
        };
        switch (x.term) {
        | Constructor(v) =>
          basic(
            AppExp.conaps,
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s%s%s"),
                v,
                Id.to_string(x_id),
                Id.to_string(arg_id),
              ),
            AppExp.conapp_exp_coloring_ids,
          )
        | _ =>
          basic(
            AppExp.funaps,
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s%s"),
                Id.to_string(x_id),
                Id.to_string(arg_id),
              ),
            AppExp.funapp_exp_coloring_ids,
          )
        };
      | DeferredAp(x, args) =>
        let x_id = List.nth(x.ids, 0);
        let supplied_id = Id.mk();
        let deferred_id = {
          let deferral = List.find(Term.UExp.is_deferral, args);
          List.nth(deferral.ids, 0);
        };
        switch (mode) {
        | MessageContent(_) =>
          get_message(
            ~colorings=
              AppExp.deferred_funapp_exp_coloring_ids(~x_id, ~deferred_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s%s"),
                    Id.to_string(x_id),
                    Id.to_string(supplied_id),
                    Id.to_string(deferred_id),
                  ),
              ),
            AppExp.deferredaps,
          )
        | Colorings =>
          let color_fn = List.nth(ColorSteps.child_colors, 0);
          let color_supplied = List.nth(ColorSteps.child_colors, 1);
          let color_deferred = List.nth(ColorSteps.child_colors, 2);
          let add = (mapping, arg: Term.UExp.t) => {
            let arg_id = List.nth(arg.ids, 0);
            Haz3lcore.Id.Map.add(
              arg_id,
              Term.UExp.is_deferral(arg) ? color_deferred : color_supplied,
              mapping,
            );
          };
          let mapping = Haz3lcore.Id.Map.singleton(x_id, color_fn);
          let mapping = List.fold_left(add, mapping, args);
          let color_map = (mapping, List.length(args) + 1);
          ([], ([], color_map), []);
        };
      | If(cond, then_, else_) =>
        let cond_id = List.nth(cond.ids, 0);
        let then_id = List.nth(then_.ids, 0);
        let else_id = List.nth(else_.ids, 0);
        get_message(
          ~colorings=IfExp.if_exp_coloring_ids(~cond_id, ~then_id, ~else_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s%s"),
                  Id.to_string(cond_id),
                  Id.to_string(then_id),
                  Id.to_string(else_id),
                ),
            ),
          IfExp.ifs,
        );
      | Seq(left, right) =>
        let exp1_id = List.nth(left.ids, 0);
        let exp2_id = List.nth(right.ids, 0);
        get_message(
          ~colorings=SeqExp.seq_exp_coloring_ids(~exp1_id, ~exp2_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s"),
                  Id.to_string(exp1_id),
                  Id.to_string(exp2_id),
                ),
            ),
          SeqExp.seqs,
        );
      | Filter((Step, One), pat, body) =>
        message_single(
          FilterExp.filter_pause(
            ~p_id=Term.UExp.rep_id(pat),
            ~body_id=Term.UExp.rep_id(body),
          ),
        )
      | Filter((Step, All), pat, body) =>
        message_single(
          FilterExp.filter_debug(
            ~p_id=Term.UExp.rep_id(pat),
            ~body_id=Term.UExp.rep_id(body),
          ),
        )
      | Filter((Eval, All), pat, body) =>
        message_single(
          FilterExp.filter_eval(
            ~p_id=Term.UExp.rep_id(pat),
            ~body_id=Term.UExp.rep_id(body),
          ),
        )
      | Filter((Eval, One), pat, body) =>
        message_single(
          FilterExp.filter_hide(
            ~p_id=Term.UExp.rep_id(pat),
            ~body_id=Term.UExp.rep_id(body),
          ),
        )
      | Test(body) =>
        let body_id = List.nth(body.ids, 0);
        get_message(
          ~colorings=TestExp.test_exp_coloring_ids(~body_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s"),
                  Id.to_string(body_id),
                ),
            ),
          TestExp.tests,
        );
      | Parens(term) => get_message_exp(term.term) // No Special message?
      | Cons(hd, tl) =>
        let hd_id = List.nth(hd.ids, 0);
        let tl_id = List.nth(tl.ids, 0);
        get_message(
          ~colorings=ListExp.cons_exp_coloring_ids(~hd_id, ~tl_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s"),
                  Id.to_string(hd_id),
                  Id.to_string(tl_id),
                ),
            ),
          ListExp.listcons,
        );
      | ListConcat(xs, ys) =>
        let xs_id = List.nth(xs.ids, 0);
        let ys_id = List.nth(ys.ids, 0);
        get_message(
          ~colorings=ListExp.concat_exp_coloring_ids(~xs_id, ~ys_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s"),
                  Id.to_string(xs_id),
                  Id.to_string(ys_id),
                ),
            ),
          ListExp.listconcats,
        );
      | UnOp(op, exp) =>
        switch (op) {
        | Bool(Not) =>
          let exp_id = List.nth(exp.ids, 0);
          get_message(
            ~colorings=OpExp.bool_unary_not_exp_coloring_ids(~exp_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s"),
                    Id.to_string(exp_id),
                  ),
              ),
            OpExp.bool_un_not,
          );
        | Int(Minus) =>
          let exp_id = List.nth(exp.ids, 0);
          get_message(
            ~colorings=OpExp.int_unary_minus_exp_coloring_ids(~exp_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s"),
                    Id.to_string(exp_id),
                  ),
              ),
            OpExp.int_un_minus,
          );
        | Meta(Unquote) =>
          message_single(FilterExp.unquote(~sel_id=Term.UExp.rep_id(exp)))
        }
      | BinOp(op, left, right) =>
        open OpExp;
        let (group, coloring_ids) =
          switch (op) {
          | Int(Plus) => (int_plus, int_plus_exp_coloring_ids)
          | Int(Minus) => (int_minus, int_minus_exp_coloring_ids)
          | Int(Times) => (int_times, int_times_exp_coloring_ids)
          | Int(Power) => (int_power, int_power_exp_coloring_ids)
          | Int(Divide) => (int_divide, int_divide_exp_coloring_ids)
          | Int(LessThan) => (int_less_than, int_lt_exp_coloring_ids)
          | Int(LessThanOrEqual) => (
              int_less_than_equal,
              int_lte_exp_coloring_ids,
            )
          | Int(GreaterThan) => (int_greater_than, int_gt_exp_coloring_ids)
          | Int(GreaterThanOrEqual) => (
              int_greater_than_equal,
              int_gte_exp_coloring_ids,
            )
          | Int(Equals) => (int_equal, int_eq_exp_coloring_ids)
          | Int(NotEquals) => (int_not_equal, int_neq_exp_coloring_ids)
          | Float(Plus) => (float_plus, float_plus_exp_coloring_ids)
          | Float(Minus) => (float_minus, float_minus_exp_coloring_ids)
          | Float(Times) => (float_times, float_times_exp_coloring_ids)
          | Float(Power) => (float_power, float_power_exp_coloring_ids)
          | Float(Divide) => (float_divide, float_divide_exp_coloring_ids)
          | Float(LessThan) => (float_less_than, float_lt_exp_coloring_ids)
          | Float(LessThanOrEqual) => (
              float_less_than_equal,
              float_lte_exp_coloring_ids,
            )
          | Float(GreaterThan) => (
              float_greater_than,
              float_gt_exp_coloring_ids,
            )
          | Float(GreaterThanOrEqual) => (
              float_greater_than_equal,
              float_gte_exp_coloring_ids,
            )
          | Float(Equals) => (float_equal, float_eq_exp_coloring_ids)
          | Float(NotEquals) => (float_not_equal, float_neq_exp_coloring_ids)
          | Bool(And) => (bool_and, bool_and_exp_coloring_ids)
          | Bool(Or) => (bool_or, bool_or_exp_coloring_ids)
          | String(Equals) => (string_equal, str_eq_exp_coloring_ids)
          | String(Concat) => (string_concat, str_concat_exp_coloring_ids)
          };
        let left_id = List.nth(left.ids, 0);
        let right_id = List.nth(right.ids, 0);
        get_message(
          ~colorings=coloring_ids(~left_id, ~right_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s"),
                  Id.to_string(left_id),
                  Id.to_string(right_id),
                ),
            ),
          group,
        );
      | Match(scrut, _rules) =>
        let scrut_id = List.nth(scrut.ids, 0);
        get_message(
          ~colorings=CaseExp.case_exp_coloring_ids(~scrut_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s"),
                  Id.to_string(scrut_id),
                ),
            ),
          CaseExp.case,
        );
      | Constructor(v) =>
        get_message(
          ~format=
            Some(
              msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), v),
            ),
          TerminalExp.ctr(v),
        )
      };
    get_message_exp(term.term);
  | Some(InfoPat({term, _})) =>
    switch (bypass_parens_pat(term).term) {
    | EmptyHole => get_message(HolePat.empty_hole)
    | MultiHole(_) => get_message(HolePat.multi_hole)
    | Wild => get_message(TerminalPat.wild)
    | Int(i) =>
      get_message(
        ~format=
          Some(
            msg =>
              Printf.sprintf(Scanf.format_from_string(msg, "%i%i"), i, i),
          ),
        TerminalPat.intlit(i),
      )
    | Float(f) =>
      get_message(
        ~format=
          Some(
            msg =>
              Printf.sprintf(Scanf.format_from_string(msg, "%f%f"), f, f),
          ),
        TerminalPat.floatlit(f),
      )
    | Bool(b) =>
      get_message(
        ~format=
          Some(
            msg =>
              Printf.sprintf(Scanf.format_from_string(msg, "%b%b"), b, b),
          ),
        TerminalPat.boollit(b),
      )
    | String(s) =>
      get_message(
        ~format=
          Some(
            msg =>
              Printf.sprintf(Scanf.format_from_string(msg, "%s%s"), s, s),
          ),
        TerminalPat.strlit(s),
      )
    | Triv => get_message(TerminalPat.triv)
    | ListLit(elements) =>
      if (List.length(elements) == 0) {
        get_message(ListPat.listnil);
      } else {
        get_message(
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s"),
                  string_of_int(List.length(elements)),
                ),
            ),
          ListPat.listlit,
        );
      }
    | Cons(hd, tl) =>
      let hd_id = List.nth(hd.ids, 0);
      let tl_id = List.nth(tl.ids, 0);
      let basic = doc =>
        get_message(
          ~colorings=ListPat.cons_base_pat_coloring_ids(~hd_id, ~tl_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s"),
                  Id.to_string(hd_id),
                  Id.to_string(tl_id),
                ),
            ),
          doc,
        );
      switch (tl.term) {
      | TermBase.UPat.Cons(hd2, tl2) =>
        if (ListPat.cons2_pat.id == get_specificity_level(ListPat.cons2)) {
          let hd2_id = List.nth(hd2.ids, 0);
          let tl2_id = List.nth(tl2.ids, 0);
          get_message(
            ~colorings=
              ListPat.cons2_pat_coloring_ids(
                ~fst_id=hd_id,
                ~snd_id=hd2_id,
                ~tl_id=tl2_id,
              ),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s%s"),
                    Id.to_string(hd_id),
                    Id.to_string(hd2_id),
                    Id.to_string(tl2_id),
                  ),
              ),
            ListPat.cons2,
          );
        } else {
          basic(ListPat.cons2);
        }
      | _ => basic(ListPat.cons)
      };
    | Var(v) =>
      get_message(
        ~format=
          Some(
            msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), v),
          ),
        TerminalPat.var(v),
      )
    | Tuple(elements) =>
      let basic = group =>
        get_message(
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s"),
                  string_of_int(List.length(elements)),
                ),
            ),
          group,
        );
      switch (List.length(elements)) {
      | 2 =>
        if (TuplePat.tuple_pat_size2.id
            == get_specificity_level(TuplePat.tuple2)) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          get_message(
            ~colorings=
              TuplePat.tuple_pat_size2_coloring_ids(~elem1_id, ~elem2_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s"),
                    Id.to_string(elem1_id),
                    Id.to_string(elem2_id),
                  ),
              ),
            TuplePat.tuple2,
          );
        } else {
          basic(TuplePat.tuple2);
        }
      | 3 =>
        if (TuplePat.tuple_pat_size3.id
            == get_specificity_level(TuplePat.tuple3)) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          let elem3_id = List.nth(List.nth(elements, 2).ids, 0);
          get_message(
            ~colorings=
              TuplePat.tuple_pat_size3_coloring_ids(
                ~elem1_id,
                ~elem2_id,
                ~elem3_id,
              ),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s%s"),
                    Id.to_string(elem1_id),
                    Id.to_string(elem2_id),
                    Id.to_string(elem3_id),
                  ),
              ),
            TuplePat.tuple3,
          );
        } else {
          basic(TuplePat.tuple3);
        }
      | _ => basic(TuplePat.tuple)
      };
    | Ap(con, arg) =>
      let con_id = List.nth(con.ids, 0);
      let arg_id = List.nth(arg.ids, 0);
      get_message(
        ~colorings=AppPat.ap_pat_coloring_ids(~con_id, ~arg_id),
        ~format=
          Some(
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s%s"),
                Id.to_string(con_id),
                Id.to_string(arg_id),
              ),
          ),
        AppPat.ap,
      );
    | Constructor(con) =>
      get_message(
        ~format=
          Some(
            msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), con),
          ),
        TerminalPat.ctr(con),
      )
    | TypeAnn(pat, typ) =>
      let pat_id = List.nth(pat.ids, 0);
      let typ_id = List.nth(typ.ids, 0);
      get_message(
        ~colorings=TypAnnPat.typann_pat_coloring_ids(~pat_id, ~typ_id),
        ~format=
          Some(
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s%s"),
                Id.to_string(pat_id),
                Id.to_string(typ_id),
              ),
          ),
        TypAnnPat.typann,
      );
    | Invalid(_) => simple("Not a valid pattern")
    | Parens(_) =>
      // Shouldn't be hit?
      default
    }
  | Some(InfoTyp({term, cls, _})) =>
    switch (bypass_parens_typ(term).term) {
    | EmptyHole => get_message(HoleTyp.empty_hole)
    | MultiHole(_) => get_message(HoleTyp.multi_hole)
    | Int => get_message(TerminalTyp.int)
    | Float => get_message(TerminalTyp.float)
    | Bool => get_message(TerminalTyp.bool)
    | String => get_message(TerminalTyp.str)
    | List(elem) =>
      let elem_id = List.nth(elem.ids, 0);
      get_message(
        ~colorings=ListTyp.list_typ_coloring_ids(~elem_id),
        ~format=
          Some(
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s"),
                Id.to_string(elem_id),
              ),
          ),
        ListTyp.list,
      );
    | Forall(tpat, typ) =>
      let tpat_id = List.nth(tpat.ids, 0);
      let tbody_id = List.nth(typ.ids, 0);
      get_message(
        ~colorings=ForallTyp.forall_typ_coloring_ids(~tpat_id, ~tbody_id),
        ~format=
          Some(
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s%s"),
                Id.to_string(tpat_id),
                Id.to_string(tbody_id),
              ),
          ),
        ForallTyp.forall,
      );
    | Rec(tpat, typ) =>
      let tpat_id = List.nth(tpat.ids, 0);
      let tbody_id = List.nth(typ.ids, 0);
      get_message(
        ~colorings=RecTyp.rec_typ_coloring_ids(~tpat_id, ~tbody_id),
        ~format=
          Some(
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s%s"),
                Id.to_string(tpat_id),
                Id.to_string(tbody_id),
              ),
          ),
        RecTyp.rec_,
      );
    | Arrow(arg, result) =>
      let arg_id = List.nth(arg.ids, 0);
      let result_id = List.nth(result.ids, 0);
      let basic = doc =>
        get_message(
          ~colorings=ArrowTyp.arrow_typ_coloring_ids(~arg_id, ~result_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s%s"),
                  Id.to_string(arg_id),
                  Id.to_string(result_id),
                ),
            ),
          doc,
        );
      switch (result.term) {
      | TermBase.UTyp.Arrow(arg2, result2) =>
        if (ArrowTyp.arrow3_typ.id == get_specificity_level(ArrowTyp.arrow3)) {
          let arg2_id = List.nth(arg2.ids, 0);
          let result2_id = List.nth(result2.ids, 0);
          get_message(
            ~colorings=
              ArrowTyp.arrow3_typ_coloring_ids(
                ~arg1_id=arg_id,
                ~arg2_id,
                ~result_id=result2_id,
              ),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s%s"),
                    Id.to_string(arg_id),
                    Id.to_string(arg2_id),
                    Id.to_string(result2_id),
                  ),
              ),
            ArrowTyp.arrow3,
          );
        } else {
          basic(ArrowTyp.arrow3);
        }
      | _ => basic(ArrowTyp.arrow)
      };
    | Tuple(elements) =>
      let basic = group =>
        get_message(
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%s"),
                  string_of_int(List.length(elements)),
                ),
            ),
          group,
        );
      switch (List.length(elements)) {
      | 0 =>
        if (TupleTyp.tuple0_typ.id == get_specificity_level(TupleTyp.tuple0)) {
          get_message(
            ~colorings=[],
            ~format=Some(msg => msg),
            TupleTyp.tuple0,
          );
        } else {
          basic(TupleTyp.tuple2);
        }
      | 2 =>
        if (TupleTyp.tuple2_typ.id == get_specificity_level(TupleTyp.tuple2)) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          get_message(
            ~colorings=TupleTyp.tuple2_typ_coloring_ids(~elem1_id, ~elem2_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s"),
                    Id.to_string(elem1_id),
                    Id.to_string(elem2_id),
                  ),
              ),
            TupleTyp.tuple2,
          );
        } else {
          basic(TupleTyp.tuple2);
        }
      | 3 =>
        if (TupleTyp.tuple3_typ.id == get_specificity_level(TupleTyp.tuple3)) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          let elem3_id = List.nth(List.nth(elements, 2).ids, 0);
          get_message(
            ~colorings=
              TupleTyp.tuple3_typ_coloring_ids(
                ~elem1_id,
                ~elem2_id,
                ~elem3_id,
              ),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%s%s%s"),
                    Id.to_string(elem1_id),
                    Id.to_string(elem2_id),
                    Id.to_string(elem3_id),
                  ),
              ),
            TupleTyp.tuple3,
          );
        } else {
          basic(TupleTyp.tuple3);
        }
      | _ => basic(TupleTyp.tuple)
      };
    | Constructor(c) =>
      get_message(SumTyp.sum_typ_nullary_constructor_defs(c))
    | Var(c) when cls == Typ(Constructor) =>
      get_message(SumTyp.sum_typ_nullary_constructor_defs(c))
    | Var(v) =>
      get_message(
        ~format=
          Some(
            msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), v),
          ),
        TerminalTyp.var(v),
      )
    | Sum(_) => get_message(SumTyp.labelled_sum_typs)
    | Ap({term: Constructor(c), _}, _) =>
      get_message(SumTyp.sum_typ_unary_constructor_defs(c))
    | Invalid(_) => simple("Not a type or type operator")
    | Ap(_)
    | Parens(_) => default // Shouldn't be hit?
    }
  | Some(InfoTPat(info)) =>
    switch (info.term.term) {
    | Invalid(_) => simple("Type names must begin with a capital letter")
    | EmptyHole => get_message(HoleTPat.empty_hole_tpats)
    | MultiHole(_) => get_message(HoleTPat.multi_hole_tpats)
    | Var(v) =>
      get_message(
        ~format=
          Some(
            msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), v),
          ),
        VarTPat.var_typ_pats(v),
      )
    }
  | Some(Secondary(s)) =>
    switch (s.cls) {
    | Secondary(Whitespace) => simple("A semantic void, pervading but inert")
    | Secondary(Comment) =>
      simple("Comments are ignored by systems but treasured by readers")
    | _ => failwith("ExplainThis: Secondary Impossible")
    }
  | None => default
  };
};

let section = (~section_clss: string, ~title: string, contents: list(Node.t)) =>
  div(
    ~attrs=[clss(["section", section_clss])],
    [div(~attrs=[clss(["section-title"])], [text(title)])] @ contents,
  );

let get_color_map =
    (~settings: Settings.t, ~explainThisModel: ExplainThisModel.t, info) =>
  switch (settings.explainThis.highlight) {
  | All when settings.explainThis.show =>
    let (_, (_, (color_map, _)), _) =
      get_doc(~docs=explainThisModel, info, Colorings);
    Some(color_map);
  | One(id) when settings.explainThis.show =>
    let (_, (_, (color_map, _)), _) =
      get_doc(~docs=explainThisModel, info, Colorings);
    Some(Id.Map.filter((id', _) => id == id', color_map));
  | _ => None
  };

let view =
    (
      ~inject,
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~explainThisModel: ExplainThisModel.t,
      info: option(Info.t),
    ) => {
  let (syn_form, (explanation, _), example) =
    get_doc(
      ~docs=explainThisModel,
      info,
      MessageContent(inject, ui_state, settings),
    );
  div(
    ~attrs=[Attr.id("side-bar")],
    [
      div(
        ~attrs=[clss(["explain-this"])],
        [
          div(
            ~attrs=[clss(["top-bar"])],
            [
              Widgets.toggle(
                ~tooltip="Toggle highlighting",
                "🔆",
                settings.explainThis.highlight == All,
                _ =>
                inject(UpdateAction.Set(ExplainThis(SetHighlight(Toggle))))
              ),
              div(
                ~attrs=[
                  clss(["close"]),
                  Attr.on_click(_ =>
                    inject(UpdateAction.Set(ExplainThis(ToggleShow)))
                  ),
                ],
                [text("x")],
              ),
            ],
          ),
        ]
        @ [
          section(
            ~section_clss="syntactic-form",
            ~title=
              switch (info) {
              | None => "Whitespace or Comment"
              | Some(info) => Info.cls_of(info) |> Term.Cls.show
              },
            syn_form @ explanation,
          ),
        ]
        @ (
          example == []
            ? []
            : [section(~section_clss="examples", ~title="Examples", example)]
        ),
      ),
    ],
  );
};
