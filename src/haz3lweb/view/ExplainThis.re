open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;
open Widgets;

let feedback_view = (message, up_active, up_action, down_active, down_action) => {
  div(
    ~attr=clss(["feedback"]),
    [
      div(~attr=clss(["message"]), [text(message)]),
      div(
        ~attr=
          Attr.many([
            clss(["option"] @ (up_active ? ["active"] : [])),
            Attr.on_click(up_action),
          ]),
        [text("👍")],
      ),
      div(
        ~attr=
          Attr.many([
            clss(["option"] @ (down_active ? ["active"] : [])),
            Attr.on_click(down_action),
          ]),
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
        Update.UpdateExplainThisModel(
          ToggleExplanationFeedback(group_id, form_id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        Update.UpdateExplainThisModel(
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
        Update.UpdateExplainThisModel(
          ToggleExampleFeedback(group_id, form_id, example_id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        Update.UpdateExplainThisModel(
          ToggleExampleFeedback(group_id, form_id, example_id, ThumbsDown),
        ),
      ),
  );
};

let code_node = text => Node.span(~attr=clss(["code"]), [Node.text(text)]);

let highlight =
    (~inject, msg: list(Node.t), id: Haz3lcore.Id.t, mapping: ColorSteps.t)
    : (Node.t, ColorSteps.t) => {
  let (c, mapping) = ColorSteps.get_color(id, mapping);
  let classes = clss(["highlight-" ++ c, "clickable"]);
  let attr =
    switch (inject) {
    | Some(inject) =>
      Attr.many([
        classes,
        Attr.on_click(_ =>
          inject(UpdateAction.PerformAction(Jump(TileId(id))))
        ),
      ])
    | None => classes
    };
  (Node.span(~attr, msg), mapping);
};

/*
 Markdown like thing:
 highlighty thing : [thing to highlight](id)
 bulleted list: - list item
                - list item
 code: `code`
 italics: *word*
 */
let mk_translation =
    (~inject, text: string, show_highlight: bool)
    : (list(Node.t), ColorSteps.t) => {
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
          let id = int_of_string(id);
          let (inner_msg, mapping) =
            if (show_highlight) {
              highlight(~inject, d, id, mapping);
            } else {
              switch (inject) {
              | Some(inject) => (
                  Node.span(
                    ~attr=
                      Attr.many([
                        clss(["clickable"]),
                        Attr.on_click(_ =>
                          inject(
                            UpdateAction.PerformAction(Jump(TileId(id))),
                          )
                        ),
                      ]),
                    d,
                  ),
                  mapping,
                )
              | None => (Node.span(d), mapping)
              };
            };
          (List.append(msg, [inner_msg]), mapping);
        | Emph(d) =>
          let (d, mapping) = translate(d, mapping);
          (
            List.append(
              msg,
              [
                Node.span(
                  ~attr=
                    Attr.style(
                      Css_gen.create(~field="font-style", ~value="italic"),
                    ),
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
      group_id,
      form_id,
      text: string,
      show_highlight: bool,
      model: ExplainThisModel.t,
    )
    : (Node.t, ColorSteps.t) => {
  let (msg, color_map) =
    mk_translation(~inject=Some(inject), text, show_highlight);
  (
    div([
      div(~attr=clss(["explanation-contents"]), msg),
      explanation_feedback_view(~inject, group_id, form_id, model),
    ]),
    color_map,
  );
};

let deco =
    (
      ~doc: ExplainThisModel.t,
      ~settings,
      ~colorings,
      ~expandable: option((Id.t, Segment.t)),
      ~unselected,
      ~map,
      ~inject,
      ~font_metrics,
      ~options: list((ExplainThisForm.form_id, Segment.t)),
      ~group_id,
      ~form_id,
    ) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = false;
      let (term, terms) = MakeTerm.go(unselected);
      let info_map = Statics.mk_map(term);
      let term_ranges = TermRanges.mk(unselected);
      let tiles = TileMap.mk(unselected);
    });

  let term_lang_doc =
    switch (expandable, List.length(options)) {
    | (None, _)
    | (_, 0 | 1) => []
    | (Some((expandable, _)), _) => [
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
                ++ (doc.specificity_open ? "transform: scaleY(1);" : ""),
              );

            let get_clss = segment =>
              switch (List.nth(segment, 0)) {
              | Base.Tile({mold, _}) =>
                switch (mold.out) {
                | Pat => ["term-tag-pat"]
                | Exp => ["term-tag-exp"] // TODO the brown on brown isn't the greatest... but okay
                | Typ => ["term-tag-typ"]
                | Any
                | Nul
                | Rul => []
                }
              | _ => []
              };

            let specificity_menu =
              Node.div(
                ~attr=
                  Attr.many([
                    clss(["specificity-options-menu", "expandable"]),
                    specificity_style,
                  ]),
                List.map(
                  ((id: ExplainThisForm.form_id, segment: Segment.t)): Node.t => {
                    let map = Measured.of_segment(segment);
                    let code_view =
                      Code.simple_view(~unselected=segment, ~map, ~settings);
                    let classes =
                      id == form_id
                        ? ["selected"] @ get_clss(segment)
                        : get_clss(segment);
                    let update_group_selection = _ =>
                      inject(
                        Update.UpdateExplainThisModel(
                          ExplainThisUpdate.UpdateGroupSelection(
                            group_id,
                            id,
                          ),
                        ),
                      );
                    Node.div(
                      ~attr=
                        Attr.many([
                          clss(classes),
                          Attr.on_click(update_group_selection),
                        ]),
                      [code_view],
                    );
                  },
                  options,
                ),
              );

            let expand_arrow_style = Attr.create("style", specificity_pos);
            let expand_arrow =
              Node.div(
                ~attr=Attr.many([clss(["arrow"]), expand_arrow_style]),
                [],
              );

            let expandable_deco =
              DecUtil.code_svg(
                ~font_metrics,
                ~origin,
                ~base_cls=["expandable"],
                ~abs_pos=false,
                path,
              );

            Node.div(
              ~attr=
                Attr.many([
                  clss(["expandable-target"]),
                  DecUtil.abs_position(~font_metrics, origin),
                  Attr.on_click(_ => {
                    inject(
                      Update.UpdateExplainThisModel(
                        ExplainThisUpdate.SpecificityOpen(
                          !doc.specificity_open,
                        ),
                      ),
                    )
                  }),
                ]),
              [expandable_deco, specificity_menu]
              @ (doc.specificity_open ? [] : [expand_arrow]),
            );
          },
        ),
      ]
    };

  let color_highlight =
    if (doc.highlight) {
      Deco.color_highlights(colorings);
    } else {
      [];
    };
  color_highlight @ term_lang_doc;
};

let syntactic_form_view =
    (
      ~doc,
      ~colorings,
      ~expandable,
      ~inject,
      ~font_metrics,
      ~unselected,
      ~settings,
      ~id,
      ~options: list((ExplainThisForm.form_id, Segment.t)),
      ~group_id,
      ~form_id,
    ) => {
  let map = Measured.of_segment(unselected);
  let code_view = Code.simple_view(~unselected, ~map, ~settings);
  let deco_view =
    deco(
      ~doc,
      ~settings,
      ~colorings,
      ~expandable,
      ~unselected,
      ~map,
      ~inject,
      ~font_metrics,
      ~options,
      ~group_id,
      ~form_id,
    );
  div(
    ~attr=Attr.many([Attr.id(id), Attr.class_("code-container")]),
    [code_view] @ deco_view,
  );
};

let example_view =
    (
      ~inject,
      ~font_metrics,
      ~settings,
      ~group_id,
      ~form_id,
      ~examples: list(ExplainThisForm.example),
      ~model: ExplainThisModel.t,
    ) => {
  div(
    ~attr=Attr.id("examples"),
    List.length(examples) == 0
      ? [text("No examples available")]
      : List.map(
          ({term, message, _} as example: ExplainThisForm.example) => {
            let map_code = Measured.of_segment(term);
            let code_view =
              Code.simple_view(~unselected=term, ~map=map_code, ~settings);
            let (uhexp, _) = MakeTerm.go(term);
            let info_map = Statics.mk_map(uhexp);
            let result_view =
              switch (Interface.evaluation_result(info_map, uhexp)) {
              | None => []
              | Some(dhexp) => [
                  DHCode.view_tylr(
                    ~settings=Settings.Evaluation.init,
                    ~selected_hole_instance=None,
                    ~font_metrics,
                    ~width=80,
                    dhexp,
                  ),
                ]
              };
            let code_container = view =>
              div(~attr=clss(["code-container"]), view);
            div(
              ~attr=clss(["example"]),
              [
                code_container([code_view]),
                div(
                  ~attr=clss(["ex-result"]),
                  [text("Result: "), code_container(result_view)],
                ),
                div(
                  ~attr=clss(["explanation"]),
                  [text("Explanation: "), text(message)],
                ),
                example_feedback_view(
                  ~inject,
                  group_id,
                  form_id,
                  example.sub_id,
                  model,
                ),
              ],
            );
          },
          examples,
        ),
  );
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
      Update.t => Virtual_dom.Vdom.Effect.t(unit),
      FontMetrics.t,
      ModelSettings.t,
    )
  | Colorings;

let get_doc =
    (~docs: ExplainThisModel.t, info: option(Statics.t), mode: message_mode)
    : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
  let default = (
    [text("No syntactic form available")],
    ([text("No explanation available")], ColorSteps.empty),
    [text("No examples available")],
  );
  let get_specificity_level = group_id =>
    fst(ExplainThisModel.get_form_and_options(group_id, docs)).id;
  let get_message =
      (
        ~colorings=[],
        ~format: option(string => string)=None,
        group: ExplainThisForm.group,
      )
      : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
    let (doc, options) = ExplainThisModel.get_form_and_options(group, docs);

    // https://stackoverflow.com/questions/31998408/ocaml-converting-strings-to-a-unit-string-format
    let explanation_msg =
      switch (format) {
      | Some(f) => f(doc.explanation)
      | None => doc.explanation
      };
    switch (mode) {
    | MessageContent(inject, font_metrics, settings) =>
      let (explanation, color_map) =
        mk_explanation(
          ~inject,
          group.id,
          doc.id,
          explanation_msg,
          docs.highlight,
          docs,
        );
      let syntactic_form_view =
        syntactic_form_view(
          ~doc=docs,
          ~colorings=
            List.map(
              ((syntactic_form_id: int, code_id: int)) => {
                let (color, _) = ColorSteps.get_color(code_id, color_map);
                (syntactic_form_id, color);
              },
              colorings,
            ),
          ~expandable=doc.expandable_id,
          ~inject,
          ~font_metrics,
          ~unselected=doc.syntactic_form,
          ~settings,
          ~id="syntactic-form-code",
          ~options,
          ~group_id=group.id,
          ~form_id=doc.id,
        );
      let example_view =
        example_view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~group_id=group.id,
          ~form_id=doc.id,
          ~examples=doc.examples,
          ~model=docs,
        );
      ([syntactic_form_view], ([explanation], color_map), [example_view]);
    | Colorings =>
      let (_, color_map) =
        mk_translation(~inject=None, explanation_msg, docs.highlight);
      ([], ([], color_map), []);
    };
  };

  switch (info) {
  | Some(InfoExp({term, _})) =>
    let rec get_message_exp =
            (term)
            : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) =>
      switch (term) {
      | TermBase.UExp.Invalid(_) => default
      | EmptyHole => get_message(HoleExp.empty_hole_exps)

      | MultiHole(_children) => get_message(HoleExp.multi_hole_exps)
      | Triv => get_message(TerminalExp.triv_exps)
      | Bool(_bool_lit) => get_message(TerminalExp.bool_exps)
      | Int(_int_lit) => get_message(TerminalExp.int_exps)
      | Float(_float_lit) => get_message(TerminalExp.float_exps)
      | String(_str_lit) => get_message(TerminalExp.string_exps)
      | ListLit(terms) =>
        get_message(
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%i"),
                  List.length(terms),
                ),
            ),
          ListExp.listlits,
        )
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
                    Scanf.format_from_string(msg, "%i%i"),
                    pat_id,
                    body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      pat_id,
                      body_id,
                      pat_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      pat_id,
                      body_id,
                      pat_id,
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
                      Scanf.format_from_string(msg, "%i"),
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i%i"),
                      pat_id,
                      i,
                      pat_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%f%i%i"),
                      pat_id,
                      f,
                      pat_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%b%i%i"),
                      pat_id,
                      b,
                      pat_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%s%i%i"),
                      pat_id,
                      s,
                      pat_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      pat_id,
                      pat_id,
                      body_id,
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
                        Scanf.format_from_string(msg, "%i%i%i"),
                        pat_id,
                        pat_id,
                        body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i%i"),
                      pat_id,
                      List.length(elements),
                      pat_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      hd_id,
                      tl_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%s%i"),
                      pat_id,
                      var,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i%i"),
                      pat_id,
                      List.length(elements),
                      pat_id,
                      body_id,
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
                        Scanf.format_from_string(msg, "%i%i%i"),
                        pat1_id,
                        pat2_id,
                        body_id,
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
                        Scanf.format_from_string(msg, "%i%i%i%i"),
                        pat1_id,
                        pat2_id,
                        pat3_id,
                        body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      con_id,
                      arg_id,
                      body_id,
                    ),
                ),
              FunctionExp.functions_ap,
            );
          } else {
            basic(FunctionExp.functions_ap);
          }
        | Tag(v) =>
          if (FunctionExp.function_tag_exp.id
              == get_specificity_level(FunctionExp.functions_tag)) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              ~colorings=
                FunctionExp.function_tag_exp_coloring_ids(~pat_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%i%s%i%i"),
                      pat_id,
                      v,
                      pat_id,
                      body_id,
                    ),
                ),
              FunctionExp.functions_tag,
            );
          } else {
            basic(FunctionExp.functions_tag);
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
                    Scanf.format_from_string(msg, "%i"),
                    List.length(terms),
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
                      Scanf.format_from_string(msg, "%i%i"),
                      exp1_id,
                      exp2_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      exp1_id,
                      exp2_id,
                      exp3_id,
                    ),
                ),
              TupleExp.tuples3,
            );
          } else {
            basic(TupleExp.tuples3);
          }
        | _ => basic(TupleExp.tuples)
        };
      | Var(_var) => get_message(TerminalExp.var_exps)
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
                    Scanf.format_from_string(msg, "%i%i"),
                    def_id,
                    pat_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      pat_id,
                      def_id,
                      pat_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      pat_id,
                      def_id,
                      pat_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      def_id,
                      def_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i%i%i"),
                      def_id,
                      pat_id,
                      i,
                      def_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%f%i%i"),
                      def_id,
                      pat_id,
                      f,
                      def_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%b%i%i"),
                      def_id,
                      pat_id,
                      b,
                      def_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%s%i%i"),
                      def_id,
                      pat_id,
                      s,
                      def_id,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i%i"),
                      def_id,
                      pat_id,
                      def_id,
                      body_id,
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
                        Scanf.format_from_string(msg, "%i%i%i%i"),
                        def_id,
                        pat_id,
                        def_id,
                        body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      def_id,
                      pat_id,
                      List.length(elements),
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      def_id,
                      hd_id,
                      tl_id,
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
                      Scanf.format_from_string(msg, "%i%i%s%i"),
                      def_id,
                      pat_id,
                      var,
                      body_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      def_id,
                      pat_id,
                      List.length(elements),
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
                        Scanf.format_from_string(msg, "%i%i%i"),
                        def_id,
                        pat1_id,
                        pat2_id,
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
                        Scanf.format_from_string(msg, "%i%i%i%i"),
                        def_id,
                        pat1_id,
                        pat2_id,
                        pat3_id,
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
                      Scanf.format_from_string(msg, "%i%i%i"),
                      def_id,
                      con_id,
                      arg_id,
                    ),
                ),
              LetExp.lets_ap,
            );
          } else {
            basic(LetExp.lets_ap);
          }
        | Tag(v) =>
          if (LetExp.let_tag_exp.id == get_specificity_level(LetExp.lets_tag)) {
            get_message(
              ~colorings=
                LetExp.let_tag_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              ~format=
                Some(
                  msg =>
                    Printf.sprintf(
                      Scanf.format_from_string(msg, "%i%i%s%i%i"),
                      def_id,
                      pat_id,
                      v,
                      def_id,
                      body_id,
                    ),
                ),
              LetExp.lets_tag,
            );
          } else {
            basic(LetExp.lets_tag);
          }
        | Invalid(_) => default // Shouldn't get hit
        | Parens(_) => default // Shouldn't get hit?
        | TypeAnn(_) => default // Shouldn't get hit?
        };
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
        | Tag(v) =>
          basic(
            AppExp.conaps,
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%s%i%i"),
                v,
                x_id,
                arg_id,
              ),
            AppExp.conapp_exp_coloring_ids,
          )
        | _ =>
          basic(
            AppExp.funaps,
            msg =>
              Printf.sprintf(
                Scanf.format_from_string(msg, "%i%i"),
                x_id,
                arg_id,
              ),
            AppExp.funapp_exp_coloring_ids,
          )
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
                  Scanf.format_from_string(msg, "%i%i%i"),
                  cond_id,
                  then_id,
                  else_id,
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
                  Scanf.format_from_string(msg, "%i%i"),
                  exp1_id,
                  exp2_id,
                ),
            ),
          SeqExp.seqs,
        );
      | Test(body) =>
        let body_id = List.nth(body.ids, 0);
        get_message(
          ~colorings=TestExp.test_exp_coloring_ids(~body_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(Scanf.format_from_string(msg, "%i"), body_id),
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
                  Scanf.format_from_string(msg, "%i%i"),
                  hd_id,
                  tl_id,
                ),
            ),
          ListExp.listcons,
        );
      | UnOp(op, exp) =>
        switch (op) {
        | Int(Minus) =>
          let exp_id = List.nth(exp.ids, 0);
          get_message(
            ~colorings=OpExp.int_unary_minus_exp_coloring_ids(~exp_id),
            ~format=
              Some(
                msg =>
                  Printf.sprintf(
                    Scanf.format_from_string(msg, "%i"),
                    exp_id,
                  ),
              ),
            OpExp.int_un_minus,
          );
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
          | Bool(And) => (bool_and, bool_and_exp_coloring_ids)
          | Bool(Or) => (bool_or, bool_or_exp_coloring_ids)
          | String(Equals) => (string_equal, str_eq_exp_coloring_ids)
          };
        let left_id = List.nth(left.ids, 0);
        let right_id = List.nth(right.ids, 0);
        get_message(
          ~colorings=coloring_ids(~left_id, ~right_id),
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%i%i"),
                  left_id,
                  right_id,
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
                  Scanf.format_from_string(msg, "%i"),
                  scrut_id,
                ),
            ),
          CaseExp.case,
        );
      | Tag(v) =>
        get_message(
          ~format=
            Some(
              msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), v),
            ),
          TerminalExp.tag,
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
        TerminalPat.intlit,
      )
    | Float(f) =>
      get_message(
        ~format=
          Some(
            msg =>
              Printf.sprintf(Scanf.format_from_string(msg, "%f%f"), f, f),
          ),
        TerminalPat.floatlit,
      )
    | Bool(b) =>
      get_message(
        ~format=
          Some(
            msg =>
              Printf.sprintf(Scanf.format_from_string(msg, "%b%b"), b, b),
          ),
        TerminalPat.boollit,
      )
    | String(s) =>
      get_message(
        ~format=
          Some(
            msg =>
              Printf.sprintf(Scanf.format_from_string(msg, "%s%s"), s, s),
          ),
        TerminalPat.strlit,
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
                  Scanf.format_from_string(msg, "%i"),
                  List.length(elements),
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
                  Scanf.format_from_string(msg, "%i%i"),
                  hd_id,
                  tl_id,
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
                    Scanf.format_from_string(msg, "%i%i%i"),
                    hd_id,
                    hd2_id,
                    tl2_id,
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
        TerminalPat.var,
      )
    | Tuple(elements) =>
      let basic = group =>
        get_message(
          ~format=
            Some(
              msg =>
                Printf.sprintf(
                  Scanf.format_from_string(msg, "%i"),
                  List.length(elements),
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
                    Scanf.format_from_string(msg, "%i%i"),
                    elem1_id,
                    elem2_id,
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
                    Scanf.format_from_string(msg, "%i%i%i"),
                    elem1_id,
                    elem2_id,
                    elem3_id,
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
                Scanf.format_from_string(msg, "%i%i"),
                con_id,
                arg_id,
              ),
          ),
        AppPat.ap,
      );
    | Tag(con) =>
      get_message(
        ~format=
          Some(
            msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), con),
          ),
        TerminalPat.tag,
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
                Scanf.format_from_string(msg, "%i%i"),
                pat_id,
                typ_id,
              ),
          ),
        TypAnnPat.typann,
      );
    | Invalid(_) // Shouldn't be hit
    | Parens(_) =>
      // Shouldn't be hit?
      default
    }
  | Some(InfoTyp({term, _})) =>
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
              Printf.sprintf(Scanf.format_from_string(msg, "%i"), elem_id),
          ),
        ListTyp.list,
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
                  Scanf.format_from_string(msg, "%i%i"),
                  arg_id,
                  result_id,
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
                    Scanf.format_from_string(msg, "%i%i%i"),
                    arg_id,
                    arg2_id,
                    result2_id,
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
                  Scanf.format_from_string(msg, "%i"),
                  List.length(elements),
                ),
            ),
          group,
        );
      switch (List.length(elements)) {
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
                    Scanf.format_from_string(msg, "%i%i"),
                    elem1_id,
                    elem2_id,
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
                    Scanf.format_from_string(msg, "%i%i%i"),
                    elem1_id,
                    elem2_id,
                    elem3_id,
                  ),
              ),
            TupleTyp.tuple3,
          );
        } else {
          basic(TupleTyp.tuple3);
        }
      | _ => basic(TupleTyp.tuple)
      };
    | Var(v) =>
      get_message(
        ~format=
          Some(
            msg => Printf.sprintf(Scanf.format_from_string(msg, "%s"), v),
          ),
        TerminalTyp.var,
      )
    | Invalid(_) // Shouldn't be hit
    | Parens(_) => default // Shouldn't be hit?
    }
  | Some(InfoRul(_)) // Can't have cursor on just a rule atm
  | None
  | Some(Invalid(_)) => default
  };
};

let section = (~section_clss: string, ~title: string, contents: list(Node.t)) =>
  div(
    ~attr=clss(["section", section_clss]),
    [div(~attr=clss(["section-title"]), [text(title)])] @ contents,
  );

let get_color_map =
    (~doc: ExplainThisModel.t, index': option(int), info_map: Statics.map) => {
  let info: option(Statics.t) =
    switch (index') {
    | Some(index) =>
      switch (Id.Map.find_opt(index, info_map)) {
      | Some(ci) => Some(ci)
      | None => None
      }
    | None => None
    };
  let (_, (_, (color_map, _)), _) = get_doc(~docs=doc, info, Colorings);
  color_map;
};

let view =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~settings: ModelSettings.t,
      ~doc: ExplainThisModel.t,
      index': option(int),
      info_map: Statics.map,
    ) => {
  let info: option(Statics.t) =
    switch (index') {
    | Some(index) =>
      switch (Id.Map.find_opt(index, info_map)) {
      | Some(ci) => Some(ci)
      | None => None
      }
    | None => None
    };
  let (syn_form, (explanation, _), example) =
    get_doc(~docs=doc, info, MessageContent(inject, font_metrics, settings));
  div(
    ~attr=clss(["lang-doc"]),
    [
      div(
        ~attr=clss(["content"]),
        [
          div(
            ~attr=clss(["top-bar"]),
            [
              toggle(~tooltip="Toggle highlighting", "🔆", doc.highlight, _ =>
                inject(
                  Update.UpdateExplainThisModel(
                    ExplainThisUpdate.ToggleHighlight,
                  ),
                )
              ),
              div(
                ~attr=
                  Attr.many([
                    clss(["close"]),
                    Attr.on_click(_ =>
                      inject(
                        Update.UpdateExplainThisModel(
                          ExplainThisUpdate.ToggleShow,
                        ),
                      )
                    ),
                  ]),
                [text("X")],
              ),
            ],
          ),
          section(
            ~section_clss="syntactic-form",
            ~title="Syntactic Form",
            syn_form,
          ),
          section(
            ~section_clss="explanation",
            ~title="Explanation",
            explanation,
          ),
          section(~section_clss="examples", ~title="Examples", example),
        ],
      ),
    ],
  );
};
