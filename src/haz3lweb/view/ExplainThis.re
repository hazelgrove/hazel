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

let explanation_feedback_view =
    (~inject, id, explanation: ExplainThisForm.explanation) => {
  let (up_active, down_active) =
    switch (explanation.feedback) {
    | ThumbsUp => (true, false)
    | ThumbsDown => (false, true)
    | Unselected => (false, false)
    };
  feedback_view(
    "This explanation is helpful",
    up_active,
    _ =>
      inject(
        Update.UpdateExplainThisMessages(
          ToggleExplanationFeedback(id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        Update.UpdateExplainThisMessages(
          ToggleExplanationFeedback(id, ThumbsDown),
        ),
      ),
  );
};

let example_feedback_view = (~inject, id, example: ExplainThisForm.example) => {
  let (up_active, down_active) =
    switch (example.feedback) {
    | ThumbsUp => (true, false)
    | ThumbsDown => (false, true)
    | Unselected => (false, false)
    };
  feedback_view(
    "This example is helpful",
    up_active,
    _ =>
      inject(
        Update.UpdateExplainThisMessages(
          ToggleExampleFeedback(id, example.sub_id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        Update.UpdateExplainThisMessages(
          ToggleExampleFeedback(id, example.sub_id, ThumbsDown),
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
    (~inject, id, explanation, text: string, show_highlight: bool)
    : (Node.t, ColorSteps.t) => {
  let (msg, color_map) =
    mk_translation(~inject=Some(inject), text, show_highlight);
  (
    div([
      div(~attr=clss(["explanation-contents"]), msg),
      explanation_feedback_view(~inject, id, explanation),
    ]),
    color_map,
  );
};

let deco =
    (
      ~doc: ExplainThisMessages.t,
      ~settings,
      ~colorings,
      ~expandable: option(Id.t),
      ~unselected,
      ~map,
      ~inject,
      ~font_metrics,
      ~options,
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
    | (Some(expandable), _) => [
        Deco.term_decoration(
          ~id=expandable,
          ((origin, path)) => {
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
                List.mapi(
                  (index, (id, segment)) => {
                    let map = Measured.of_segment(segment);
                    let code_view =
                      Code.simple_view(~unselected=segment, ~map, ~settings);
                    let classes = get_clss(segment);
                    id == form_id
                      ? Node.div(
                          ~attr=
                            Attr.many([
                              clss(["selected"] @ classes),
                              Attr.on_click(_ =>
                                inject(
                                  Update.UpdateExplainThisMessages(
                                    ExplainThisMessages.UpdateGroupSelection(
                                      group_id,
                                      index,
                                    ),
                                  ),
                                )
                              ),
                            ]),
                          [code_view],
                        )
                      : Node.div(
                          ~attr=
                            Attr.many([
                              clss(classes),
                              Attr.on_click(_ =>
                                inject(
                                  Update.UpdateExplainThisMessages(
                                    ExplainThisMessages.UpdateGroupSelection(
                                      group_id,
                                      index,
                                    ),
                                  ),
                                )
                              ),
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
                      Update.UpdateExplainThisMessages(
                        ExplainThisMessages.SpecificityOpen(
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
      ~options,
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
      ~id,
      ~examples: list(ExplainThisForm.example),
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
                example_feedback_view(~inject, id, example),
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
    (
      ~docs: ExplainThisMessages.t,
      info: option(Statics.t),
      mode: message_mode,
    )
    : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
  let default = (
    [text("No syntactic form available")],
    ([text("No explanation available")], ColorSteps.empty),
    [text("No examples available")],
  );
  let get_message =
      (
        doc: ExplainThisForm.form,
        options,
        group_id,
        explanation_msg,
        colorings,
      )
      : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
    switch (mode) {
    | MessageContent(inject, font_metrics, settings) =>
      let (explanation, color_map) =
        mk_explanation(
          ~inject,
          doc.id,
          doc.explanation,
          explanation_msg,
          docs.highlight,
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
          ~group_id,
          ~form_id=doc.id,
        );
      let example_view =
        example_view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~id=doc.id,
          ~examples=doc.examples,
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
      | EmptyHole =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            HoleExp.empty_hole_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          HoleExp.empty_hole_exp_group,
          doc.explanation.message,
          [],
        );
      | MultiHole(_children) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            HoleExp.multi_hole_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          HoleExp.multi_hole_exp_group,
          doc.explanation.message,
          [],
        );
      | Triv =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TerminalExp.triv_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          TerminalExp.triv_exp_group,
          doc.explanation.message,
          [],
        );
      | Bool(_bool_lit) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TerminalExp.bool_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          TerminalExp.bool_exp_group,
          doc.explanation.message,
          [],
        );
      | Int(_int_lit) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TerminalExp.int_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          TerminalExp.int_exp_group,
          doc.explanation.message,
          [],
        );
      | Float(_float_lit) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TerminalExp.float_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          TerminalExp.float_exp_group,
          doc.explanation.message,
          [],
        );
      | String(_str_lit) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TerminalExp.string_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          TerminalExp.string_exp_group,
          doc.explanation.message,
          [],
        );
      | ListLit(terms) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ListExp.list_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          ListExp.list_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            List.length(terms),
          ),
          [],
        );
      | Fun(pat, body) =>
        let basic = (doc: ExplainThisForm.form, group_id, options) => {
          let pat_id = List.nth(pat.ids, 0);
          let body_id = List.nth(body.ids, 0);
          get_message(
            doc,
            options,
            group_id,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              pat_id,
              body_id,
            ),
            FunctionExp.function_exp_coloring_ids(~pat_id, ~body_id),
          );
        };
        let pat = bypass_parens_and_annot_pat(pat);
        switch (pat.term) {
        | EmptyHole =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_empty_hole_group,
              docs,
            );
          if (FunctionExp.function_empty_hole_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_empty_hole_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id,
                body_id,
                pat_id,
              ), // https://stackoverflow.com/questions/31998408/ocaml-converting-strings-to-a-unit-string-format
              FunctionExp.function_empty_hole_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, FunctionExp.function_empty_hole_group, options);
          };
        | MultiHole(_) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_multi_hole_group,
              docs,
            );
          if (FunctionExp.function_multi_hole_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_multi_hole_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id,
                body_id,
                pat_id,
              ),
              FunctionExp.function_multi_hole_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, FunctionExp.function_multi_hole_group, options);
          };
        | Wild =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_wild_group,
              docs,
            );
          if (FunctionExp.function_wild_exp.id == doc.id) {
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_wild_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i"),
                body_id,
              ),
              FunctionExp.function_wild_exp_coloring_ids(~body_id),
            );
          } else {
            basic(doc, FunctionExp.function_wild_group, options);
          };
        | Int(i) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_int_group,
              docs,
            );
          if (FunctionExp.function_intlit_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_int_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i%i"),
                pat_id,
                i,
                pat_id,
                body_id,
              ),
              FunctionExp.function_intlit_exp_coloring_ids(~pat_id, ~body_id),
            );
          } else {
            basic(doc, FunctionExp.function_int_group, options);
          };
        | Float(f) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_float_group,
              docs,
            );
          if (FunctionExp.function_floatlit_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_float_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%f%i%i"),
                pat_id,
                f,
                pat_id,
                body_id,
              ),
              FunctionExp.function_floatlit_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, FunctionExp.function_float_group, options);
          };
        | Bool(b) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_bool_group,
              docs,
            );
          if (FunctionExp.function_boollit_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_bool_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%b%i%i"),
                pat_id,
                b,
                pat_id,
                body_id,
              ),
              FunctionExp.function_boollit_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, FunctionExp.function_bool_group, options);
          };
        | String(s) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_str_group,
              docs,
            );
          if (FunctionExp.function_strlit_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_str_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%s%i%i"),
                pat_id,
                s,
                pat_id,
                body_id,
              ),
              FunctionExp.function_strlit_exp_coloring_ids(~pat_id, ~body_id),
            );
          } else {
            basic(doc, FunctionExp.function_str_group, options);
          };
        | Triv =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_triv_group,
              docs,
            );
          if (FunctionExp.function_triv_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_triv_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id,
                pat_id,
                body_id,
              ),
              FunctionExp.function_triv_exp_coloring_ids(~pat_id, ~body_id),
            );
          } else {
            basic(doc, FunctionExp.function_triv_group, options);
          };
        | ListLit(elements) =>
          if (List.length(elements) == 0) {
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                FunctionExp.function_listnil_group,
                docs,
              );
            if (FunctionExp.function_listnil_exp.id == doc.id) {
              let pat_id = List.nth(pat.ids, 0);
              let body_id = List.nth(body.ids, 0);
              get_message(
                doc,
                options,
                FunctionExp.function_listnil_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  pat_id,
                  pat_id,
                  body_id,
                ),
                FunctionExp.function_listnil_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              );
            } else {
              basic(doc, FunctionExp.function_listnil_group, options);
            };
          } else {
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                FunctionExp.function_listlit_group,
                docs,
              );
            if (FunctionExp.function_listlit_exp.id == doc.id) {
              let pat_id = List.nth(pat.ids, 0);
              let body_id = List.nth(body.ids, 0);
              get_message(
                doc,
                options,
                FunctionExp.function_listlit_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  pat_id,
                  List.length(elements),
                  pat_id,
                  body_id,
                ),
                FunctionExp.function_listlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              );
            } else {
              basic(doc, FunctionExp.function_listlit_group, options);
            };
          }
        | Cons(hd, tl) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_cons_group,
              docs,
            );
          if (FunctionExp.function_cons_exp.id == doc.id) {
            let hd_id = List.nth(hd.ids, 0);
            let tl_id = List.nth(tl.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_cons_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                hd_id,
                tl_id,
                body_id,
              ),
              FunctionExp.function_cons_exp_coloring_ids(
                ~hd_id,
                ~tl_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, FunctionExp.function_cons_group, options);
          };
        | Var(var) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_var_group,
              docs,
            );
          if (FunctionExp.function_var_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_var_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%s%i"),
                pat_id,
                var,
                body_id,
              ),
              FunctionExp.function_var_exp_coloring_ids(~pat_id, ~body_id),
            );
          } else {
            basic(doc, FunctionExp.function_var_group, options);
          };
        | Tuple(elements) =>
          let pat_id = List.nth(pat.ids, 0);
          let body_id = List.nth(body.ids, 0);
          let basic_tuple = (doc: ExplainThisForm.form, group_id, options) => {
            get_message(
              doc,
              options,
              group_id,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i%i"),
                pat_id,
                List.length(elements),
                pat_id,
                body_id,
              ),
              FunctionExp.function_tuple_exp_coloring_ids(~pat_id, ~body_id),
            );
          };

          switch (List.length(elements)) {
          | 2 =>
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                FunctionExp.function_tuple_2_group,
                docs,
              );
            if (FunctionExp.function_tuple2_exp.id == doc.id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              get_message(
                doc,
                options,
                FunctionExp.function_tuple_2_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  pat1_id,
                  pat2_id,
                  body_id,
                ),
                FunctionExp.function_tuple2_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~body_id,
                ),
              );
            } else if (FunctionExp.function_tuple_exp.id == doc.id) {
              basic_tuple(doc, FunctionExp.function_tuple_2_group, options);
            } else {
              basic(doc, FunctionExp.function_tuple_2_group, options);
            };
          | 3 =>
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                FunctionExp.function_tuple_3_group,
                docs,
              );
            if (FunctionExp.function_tuple3_exp.id == doc.id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              let pat3_id = List.nth(List.nth(elements, 2).ids, 0);
              get_message(
                doc,
                options,
                FunctionExp.function_tuple_3_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  pat1_id,
                  pat2_id,
                  pat3_id,
                  body_id,
                ),
                FunctionExp.function_tuple3_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~pat3_id,
                  ~body_id,
                ),
              );
            } else if (FunctionExp.function_tuple_exp.id == doc.id) {
              basic_tuple(doc, FunctionExp.function_tuple_3_group, options);
            } else {
              basic(doc, FunctionExp.function_tuple_3_group, options);
            };
          | _ =>
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                FunctionExp.function_tuple_group,
                docs,
              );
            if (FunctionExp.function_tuple_exp.id == doc.id) {
              basic_tuple(doc, FunctionExp.function_tuple_group, options);
            } else {
              basic(doc, FunctionExp.function_tuple_group, options);
            };
          };
        | Ap(con, arg) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_ap_group,
              docs,
            );
          if (FunctionExp.function_ap_exp.id == doc.id) {
            let con_id = List.nth(con.ids, 0);
            let arg_id = List.nth(arg.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_ap_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                con_id,
                arg_id,
                body_id,
              ),
              FunctionExp.function_ap_exp_coloring_ids(
                ~con_id,
                ~arg_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, FunctionExp.function_ap_group, options);
          };
        | Tag(v) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              FunctionExp.function_tag_group,
              docs,
            );
          if (FunctionExp.function_tag_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              FunctionExp.function_tag_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%s%i%i"),
                pat_id,
                v,
                pat_id,
                body_id,
              ),
              FunctionExp.function_tag_exp_coloring_ids(~pat_id, ~body_id),
            );
          } else {
            basic(doc, FunctionExp.function_tag_group, options);
          };
        | Invalid(_) => default // Shouldn't get hit
        | Parens(_) => default // Shouldn't get hit?
        | TypeAnn(_) => default // Shouldn't get hit?
        };
      | Tuple(terms) =>
        let basic = (doc, group_id, options) =>
          get_message(
            doc,
            options,
            group_id,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i"),
              List.length(terms),
            ),
            [],
          );
        switch (List.length(terms)) {
        | 2 =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              TupleExp.tuple_exp_2_group,
              docs,
            );
          if (TupleExp.tuple_exp_size2.id == doc.id) {
            let exp1_id = List.nth(List.nth(terms, 0).ids, 0);
            let exp2_id = List.nth(List.nth(terms, 1).ids, 0);
            get_message(
              doc,
              options,
              TupleExp.tuple_exp_2_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i"),
                exp1_id,
                exp2_id,
              ),
              TupleExp.tuple_exp_size2_coloring_ids(~exp1_id, ~exp2_id),
            );
          } else {
            basic(doc, TupleExp.tuple_exp_2_group, options);
          };
        | 3 =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              TupleExp.tuple_exp_3_group,
              docs,
            );
          if (TupleExp.tuple_exp_size3.id == doc.id) {
            let exp1_id = List.nth(List.nth(terms, 0).ids, 0);
            let exp2_id = List.nth(List.nth(terms, 1).ids, 0);
            let exp3_id = List.nth(List.nth(terms, 2).ids, 0);
            get_message(
              doc,
              options,
              TupleExp.tuple_exp_3_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                exp1_id,
                exp2_id,
                exp3_id,
              ),
              TupleExp.tuple_exp_size3_coloring_ids(
                ~exp1_id,
                ~exp2_id,
                ~exp3_id,
              ),
            );
          } else {
            basic(doc, TupleExp.tuple_exp_3_group, options);
          };
        | _ =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              TupleExp.tuple_exp_group,
              docs,
            );
          basic(doc, TupleExp.tuple_exp_group, options);
        };
      | Var(_var) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TerminalExp.var_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          TerminalExp.var_exp_group,
          doc.explanation.message,
          [],
        );
      | Let(pat, def, body) =>
        let basic = (doc: ExplainThisForm.form, group_id, options) => {
          let pat_id = List.nth(pat.ids, 0);
          let def_id = List.nth(def.ids, 0);
          get_message(
            doc,
            options,
            group_id,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              def_id,
              pat_id,
            ),
            LetExp.let_base_exp_coloring_ids(~pat_id, ~def_id),
          );
        };
        let pat = bypass_parens_and_annot_pat(pat);
        switch (pat.term) {
        | EmptyHole =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_empty_hole_exp_group,
              docs,
            );
          if (LetExp.let_empty_hole_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_empty_hole_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id,
                def_id,
                pat_id,
              ),
              LetExp.let_empty_hole_exp_coloring_ids(~pat_id, ~def_id),
            );
          } else {
            basic(doc, LetExp.let_empty_hole_exp_group, options);
          };
        | MultiHole(_) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_multi_hole_exp_group,
              docs,
            );
          if (LetExp.let_multi_hole_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_multi_hole_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id,
                def_id,
                pat_id,
              ),
              LetExp.let_multi_hole_exp_coloring_ids(~pat_id, ~def_id),
            );
          } else {
            basic(doc, LetExp.let_multi_hole_exp_group, options);
          };
        | Wild =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_wild_exp_group,
              docs,
            );
          if (LetExp.let_wild_exp.id == doc.id) {
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_wild_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id,
                def_id,
                body_id,
              ),
              LetExp.let_wild_exp_coloring_ids(~def_id, ~body_id),
            );
          } else {
            basic(doc, LetExp.let_wild_exp_group, options);
          };
        | Int(i) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_int_exp_group,
              docs,
            );
          if (LetExp.let_int_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_int_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%i%i%i",
                ),
                def_id,
                pat_id,
                i,
                def_id,
                body_id,
              ),
              LetExp.let_int_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and some other places when switching between forms and specificity levels... maybe a Safari issue... */
            basic(
              doc,
              LetExp.let_int_exp_group,
              options,
            );
          };
        | Float(f) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_float_exp_group,
              docs,
            );
          if (LetExp.let_float_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            // TODO Make sure everywhere printing the float literal print it prettier
            get_message(
              doc,
              options,
              LetExp.let_float_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%f%i%i",
                ),
                def_id,
                pat_id,
                f,
                def_id,
                body_id,
              ),
              LetExp.let_float_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              doc,
              LetExp.let_float_exp_group,
              options,
            );
          };
        | Bool(b) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_bool_exp_group,
              docs,
            );
          if (LetExp.let_bool_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_bool_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%b%i%i",
                ),
                def_id,
                pat_id,
                b,
                def_id,
                body_id,
              ),
              LetExp.let_bool_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              doc,
              LetExp.let_bool_exp_group,
              options,
            );
          };
        | String(s) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_str_exp_group,
              docs,
            );
          if (LetExp.let_str_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_str_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%s%i%i",
                ),
                def_id,
                pat_id,
                s,
                def_id,
                body_id,
              ),
              LetExp.let_str_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              doc,
              LetExp.let_str_exp_group,
              options,
            );
          };
        | Triv =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_triv_exp_group,
              docs,
            );
          if (LetExp.let_triv_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_triv_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i%i"),
                def_id,
                pat_id,
                def_id,
                body_id,
              ),
              LetExp.let_triv_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and other places when switching syntactic specificities... seems like might be Safari issue... */
            basic(
              doc,
              LetExp.let_triv_exp_group,
              options,
            );
          };
        | ListLit(elements) =>
          if (List.length(elements) == 0) {
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                LetExp.let_listnil_exp_group,
                docs,
              );
            if (LetExp.let_listnil_exp.id == doc.id) {
              let pat_id = List.nth(pat.ids, 0);
              let def_id = List.nth(def.ids, 0);
              let body_id = List.nth(body.ids, 0);
              get_message(
                doc,
                options,
                LetExp.let_listnil_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  def_id,
                  pat_id,
                  def_id,
                  body_id,
                ),
                LetExp.let_listnil_exp_coloring_ids(
                  ~pat_id,
                  ~def_id,
                  ~body_id,
                ),
              );
            } else {
              basic(doc, LetExp.let_listnil_exp_group, options);
            };
          } else {
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                LetExp.let_listlit_exp_group,
                docs,
              );
            if (LetExp.let_listlit_exp.id == doc.id) {
              let pat_id = List.nth(pat.ids, 0);
              let def_id = List.nth(def.ids, 0);
              get_message(
                doc,
                options,
                LetExp.let_listlit_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  def_id,
                  pat_id,
                  List.length(elements),
                ),
                LetExp.let_listlit_exp_coloring_ids(~pat_id, ~def_id),
              );
            } else {
              basic(doc, LetExp.let_listlit_exp_group, options);
            };
          }
        | Cons(hd, tl) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_cons_exp_group,
              docs,
            );
          if (LetExp.let_cons_exp.id == doc.id) {
            let hd_id = List.nth(hd.ids, 0);
            let tl_id = List.nth(tl.ids, 0);
            let def_id = List.nth(def.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_cons_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id,
                hd_id,
                tl_id,
              ),
              LetExp.let_cons_exp_coloring_ids(~hd_id, ~tl_id, ~def_id),
            );
          } else {
            basic(doc, LetExp.let_cons_exp_group, options);
          };
        | Var(var) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_var_exp_group,
              docs,
            );
          if (LetExp.let_var_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_var_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%s%i"),
                def_id,
                pat_id,
                var,
                body_id,
              ),
              LetExp.let_var_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
            );
          } else {
            basic(doc, LetExp.let_var_exp_group, options);
          };
        | Tuple(elements) =>
          let pat_id = List.nth(pat.ids, 0);
          let def_id = List.nth(def.ids, 0);
          let basic_tuple = (doc: ExplainThisForm.form, group_id, options) => {
            get_message(
              doc,
              options,
              group_id,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id,
                pat_id,
                List.length(elements),
              ),
              LetExp.let_tuple_exp_coloring_ids(~pat_id, ~def_id),
            );
          };

          switch (List.length(elements)) {
          | 2 =>
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                LetExp.let_tuple2_exp_group,
                docs,
              );
            if (LetExp.let_tuple2_exp.id == doc.id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              get_message(
                doc,
                options,
                LetExp.let_tuple2_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  def_id,
                  pat1_id,
                  pat2_id,
                ),
                LetExp.let_tuple2_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~def_id,
                ),
              );
            } else if (LetExp.let_tuple_exp.id == doc.id) {
              basic_tuple(doc, LetExp.let_tuple2_exp_group, options);
            } else {
              basic(doc, LetExp.let_tuple2_exp_group, options);
            };
          | 3 =>
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                LetExp.let_tuple3_exp_group,
                docs,
              );
            // TODO Syntactic form can go off page - so can examples - but can scroll, just can't see bottom scroll bar
            if (LetExp.let_tuple3_exp.id == doc.id) {
              let pat1_id = List.nth(List.nth(elements, 0).ids, 0);
              let pat2_id = List.nth(List.nth(elements, 1).ids, 0);
              let pat3_id = List.nth(List.nth(elements, 2).ids, 0);
              get_message(
                doc,
                options,
                LetExp.let_tuple3_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  def_id,
                  pat1_id,
                  pat2_id,
                  pat3_id,
                ),
                LetExp.let_tuple3_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~pat3_id,
                  ~def_id,
                ),
              );
            } else if (LetExp.let_tuple_exp.id == doc.id) {
              basic_tuple(doc, LetExp.let_tuple3_exp_group, options);
            } else {
              basic(doc, LetExp.let_tuple3_exp_group, options);
            };
          | _ =>
            let (doc, options) =
              ExplainThisMessages.get_form_and_options(
                LetExp.let_tuple_base_exp_group,
                docs,
              );
            if (LetExp.let_tuple_exp.id == doc.id) {
              basic_tuple(doc, LetExp.let_tuple_base_exp_group, options);
            } else {
              basic(doc, LetExp.let_tuple_base_exp_group, options);
            };
          };
        | Ap(con, arg) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_ap_exp_group,
              docs,
            );
          if (LetExp.let_ap_exp.id == doc.id) {
            let con_id = List.nth(con.ids, 0);
            let arg_id = List.nth(arg.ids, 0);
            let def_id = List.nth(def.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_ap_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id,
                con_id,
                arg_id,
              ),
              LetExp.let_ap_exp_coloring_ids(~con_id, ~arg_id, ~def_id),
            );
          } else {
            basic(doc, LetExp.let_ap_exp_group, options);
          };
        | Tag(v) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              LetExp.let_tag_exp_group,
              docs,
            );
          if (LetExp.let_tag_exp.id == doc.id) {
            let pat_id = List.nth(pat.ids, 0);
            let def_id = List.nth(def.ids, 0);
            let body_id = List.nth(body.ids, 0);
            get_message(
              doc,
              options,
              LetExp.let_tag_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%s%i%i",
                ),
                def_id,
                pat_id,
                v,
                def_id,
                body_id,
              ),
              LetExp.let_tag_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
            );
          } else {
            basic(doc, LetExp.let_tag_exp_group, options);
          };
        | Invalid(_) => default // Shouldn't get hit
        | Parens(_) => default // Shouldn't get hit?
        | TypeAnn(_) => default // Shouldn't get hit?
        };
      | Ap(x, arg) =>
        let x_id = List.nth(x.ids, 0);
        let arg_id = List.nth(arg.ids, 0);
        let basic =
            (doc: ExplainThisForm.form, group, options, msg, coloring_ids) => {
          get_message(
            doc,
            options,
            group,
            msg,
            coloring_ids(~x_id, ~arg_id),
          );
        };
        switch (x.term) {
        | Tag(v) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              AppExp.conapp_exp_group,
              docs,
            );
          basic(
            doc,
            AppExp.conapp_exp_group,
            options,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%s%i%i"),
              v,
              x_id,
              arg_id,
            ),
            AppExp.conapp_exp_coloring_ids,
          );
        | _ =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              AppExp.funapp_exp_group,
              docs,
            );
          basic(
            doc,
            AppExp.funapp_exp_group,
            options,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              x_id,
              arg_id,
            ),
            AppExp.funapp_exp_coloring_ids,
          );
        };
      | If(cond, then_, else_) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(IfExp.if_exp_group, docs);
        let cond_id = List.nth(cond.ids, 0);
        let then_id = List.nth(then_.ids, 0);
        let else_id = List.nth(else_.ids, 0);
        get_message(
          doc,
          options,
          IfExp.if_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
            cond_id,
            then_id,
            else_id,
          ),
          IfExp.if_exp_coloring_ids(~cond_id, ~then_id, ~else_id),
        );
      | Seq(left, right) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            SeqExp.seq_exp_group,
            docs,
          );
        let exp1_id = List.nth(left.ids, 0);
        let exp2_id = List.nth(right.ids, 0);
        get_message(
          doc,
          options,
          SeqExp.seq_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            exp1_id,
            exp2_id,
          ),
          SeqExp.seq_exp_coloring_ids(~exp1_id, ~exp2_id),
        );
      | Test(body) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(TestExp.test_group, docs);
        let body_id = List.nth(body.ids, 0);
        get_message(
          doc,
          options,
          TestExp.test_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            body_id,
          ),
          TestExp.test_exp_coloring_ids(~body_id),
        );
      | Parens(term) => get_message_exp(term.term) // No Special message?
      | Cons(hd, tl) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ListExp.cons_exp_group,
            docs,
          );
        let hd_id = List.nth(hd.ids, 0);
        let tl_id = List.nth(tl.ids, 0);
        get_message(
          doc,
          options,
          ListExp.cons_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            hd_id,
            tl_id,
          ),
          ListExp.cons_exp_coloring_ids(~hd_id, ~tl_id),
        );
      | UnOp(op, exp) =>
        switch (op) {
        | Int(Minus) =>
          let (doc, options) =
            ExplainThisMessages.get_form_and_options(
              OpExp.int_unary_minus_group,
              docs,
            );
          let exp_id = List.nth(exp.ids, 0);
          get_message(
            doc,
            options,
            OpExp.int_unary_minus_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i"),
              exp_id,
            ),
            OpExp.int_unary_minus_exp_coloring_ids(~exp_id),
          );
        }
      | BinOp(op, left, right) =>
        let (group, coloring_ids) =
          switch (op) {
          | Int(Plus) => (
              OpExp.int_plus_group,
              OpExp.int_plus_exp_coloring_ids,
            )
          | Int(Minus) => (
              OpExp.int_minus_group,
              OpExp.int_minus_exp_coloring_ids,
            )
          | Int(Times) => (
              OpExp.int_times_group,
              OpExp.int_times_exp_coloring_ids,
            )
          | Int(Power) => (
              OpExp.int_power_group,
              OpExp.int_power_exp_coloring_ids,
            )
          | Int(Divide) => (
              OpExp.int_divide_group,
              OpExp.int_divide_exp_coloring_ids,
            )
          | Int(LessThan) => (
              OpExp.int_lt_group,
              OpExp.int_lt_exp_coloring_ids,
            )
          | Int(LessThanOrEqual) => (
              OpExp.int_lte_group,
              OpExp.int_lte_exp_coloring_ids,
            )
          | Int(GreaterThan) => (
              OpExp.int_gt_group,
              OpExp.int_gt_exp_coloring_ids,
            )
          | Int(GreaterThanOrEqual) => (
              OpExp.int_gte_group,
              OpExp.int_gte_exp_coloring_ids,
            )
          | Int(Equals) => (
              OpExp.int_eq_group,
              OpExp.int_eq_exp_coloring_ids,
            )
          | Float(Plus) => (
              OpExp.float_plus_group,
              OpExp.float_plus_exp_coloring_ids,
            )
          | Float(Minus) => (
              OpExp.float_minus_group,
              OpExp.float_minus_exp_coloring_ids,
            )
          | Float(Times) => (
              OpExp.float_times_group,
              OpExp.float_times_exp_coloring_ids,
            )
          | Float(Power) => (
              OpExp.float_power_group,
              OpExp.float_power_exp_coloring_ids,
            )
          | Float(Divide) => (
              OpExp.float_divide_group,
              OpExp.float_divide_exp_coloring_ids,
            )
          | Float(LessThan) => (
              OpExp.float_lt_group,
              OpExp.float_lt_exp_coloring_ids,
            )
          | Float(LessThanOrEqual) => (
              OpExp.float_lte_group,
              OpExp.float_lte_exp_coloring_ids,
            )
          | Float(GreaterThan) => (
              OpExp.float_gt_group,
              OpExp.float_gt_exp_coloring_ids,
            )
          | Float(GreaterThanOrEqual) => (
              OpExp.float_gte_group,
              OpExp.float_gte_exp_coloring_ids,
            )
          | Float(Equals) => (
              OpExp.float_eq_group,
              OpExp.float_eq_exp_coloring_ids,
            )
          | Bool(And) => (
              OpExp.bool_and_group,
              OpExp.bool_and_exp_coloring_ids,
            )
          | Bool(Or) => (OpExp.bool_or_group, OpExp.bool_or_exp_coloring_ids)
          | String(Equals) => (
              OpExp.str_eq_group,
              OpExp.str_eq_exp_coloring_ids,
            )
          };
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(group, docs);
        let left_id = List.nth(left.ids, 0);
        let right_id = List.nth(right.ids, 0);
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            left_id,
            right_id,
          ),
          coloring_ids(~left_id, ~right_id),
        );
      | Match(scrut, _rules) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            CaseExp.case_exp_group,
            docs,
          );
        let scrut_id = List.nth(scrut.ids, 0);
        get_message(
          doc,
          options,
          CaseExp.case_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            scrut_id,
          ),
          CaseExp.case_exp_coloring_ids(~scrut_id),
        );
      | Tag(v) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TerminalExp.tag_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          TerminalExp.tag_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%s"),
            v,
          ),
          [],
        );
      };
    get_message_exp(term.term);
  | Some(InfoPat({term, _})) =>
    switch (bypass_parens_pat(term).term) {
    | EmptyHole =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          HolePat.empty_hole_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        HolePat.empty_hole_pat_group,
        doc.explanation.message,
        [],
      );
    | MultiHole(_) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          HolePat.multi_hole_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        HolePat.multi_hole_pat_group,
        doc.explanation.message,
        [],
      );
    | Wild =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.wild_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.wild_pat_group,
        doc.explanation.message,
        [],
      );
    | Int(i) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.intlit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.intlit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i%i"),
          i,
          i,
        ),
        [],
      );
    | Float(f) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.floatlit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.floatlit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%f%f"),
          f,
          f,
        ),
        [],
      );
    | Bool(b) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.boollit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.boollit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%b%b"),
          b,
          b,
        ),
        [],
      );
    | String(s) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.strlit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.strlit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%s%s"),
          s,
          s,
        ),
        [],
      );
    | Triv =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.triv_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.triv_pat_group,
        doc.explanation.message,
        [],
      );
    | ListLit(elements) =>
      if (List.length(elements) == 0) {
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ListPat.listnil_pat_group,
            docs,
          );
        get_message(
          doc,
          options,
          ListPat.listnil_pat_group,
          doc.explanation.message,
          [],
        );
      } else {
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ListPat.listlit_pat_group,
            docs,
          );
        get_message(
          doc,
          options,
          ListPat.listlit_pat_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            List.length(elements),
          ),
          [],
        );
      }
    | Cons(hd, tl) =>
      let hd_id = List.nth(hd.ids, 0);
      let tl_id = List.nth(tl.ids, 0);
      let basic = (doc, group, options) =>
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            hd_id,
            tl_id,
          ),
          ListPat.cons_base_pat_coloring_ids(~hd_id, ~tl_id),
        );
      switch (tl.term) {
      | TermBase.UPat.Cons(hd2, tl2) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ListPat.cons2_pat_group,
            docs,
          );
        if (ListPat.cons2_pat.id == doc.id) {
          let hd2_id = List.nth(hd2.ids, 0);
          let tl2_id = List.nth(tl2.ids, 0);
          get_message(
            doc,
            options,
            ListPat.cons2_pat_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              hd_id,
              hd2_id,
              tl2_id,
            ),
            ListPat.cons2_pat_coloring_ids(
              ~fst_id=hd_id,
              ~snd_id=hd2_id,
              ~tl_id=tl2_id,
            ),
          );
        } else {
          basic(doc, ListPat.cons2_pat_group, options);
        };
      | _ =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ListPat.cons_pat_group,
            docs,
          );
        basic(doc, ListPat.cons_pat_group, options);
      };
    | Var(v) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.var_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.var_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%s"),
          v,
        ),
        [],
      );
    | Tuple(elements) =>
      let basic = (doc, group, options) =>
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            List.length(elements),
          ),
          [],
        );
      switch (List.length(elements)) {
      | 2 =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TuplePat.tuple_pat_2_group,
            docs,
          );
        if (TuplePat.tuple_pat_size2.id == doc.id) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          get_message(
            doc,
            options,
            TuplePat.tuple_pat_2_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              elem1_id,
              elem2_id,
            ),
            TuplePat.tuple_pat_size2_coloring_ids(~elem1_id, ~elem2_id),
          );
        } else {
          basic(doc, TuplePat.tuple_pat_2_group, options);
        };
      | 3 =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TuplePat.tuple_pat_3_group,
            docs,
          );
        if (TuplePat.tuple_pat_size3.id == doc.id) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          let elem3_id = List.nth(List.nth(elements, 2).ids, 0);
          get_message(
            doc,
            options,
            TuplePat.tuple_pat_3_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              elem1_id,
              elem2_id,
              elem3_id,
            ),
            TuplePat.tuple_pat_size3_coloring_ids(
              ~elem1_id,
              ~elem2_id,
              ~elem3_id,
            ),
          );
        } else {
          basic(doc, TuplePat.tuple_pat_3_group, options);
        };
      | _ =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TuplePat.tuple_pat_group,
            docs,
          );
        basic(doc, TuplePat.tuple_pat_group, options);
      };
    | Ap(con, arg) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(AppPat.ap_pat_group, docs);
      let con_id = List.nth(con.ids, 0);
      let arg_id = List.nth(arg.ids, 0);
      get_message(
        doc,
        options,
        AppPat.ap_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i%i"),
          con_id,
          arg_id,
        ),
        AppPat.ap_pat_coloring_ids(~con_id, ~arg_id),
      );
    | Tag(con) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalPat.tag_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalPat.tag_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%s"),
          con,
        ),
        [],
      );
    | TypeAnn(pat, typ) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TypAnnPat.typann_pat_group,
          docs,
        );
      let pat_id = List.nth(pat.ids, 0);
      let typ_id = List.nth(typ.ids, 0);
      get_message(
        doc,
        options,
        TypAnnPat.typann_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i%i"),
          pat_id,
          typ_id,
        ),
        TypAnnPat.typann_pat_coloring_ids(~pat_id, ~typ_id),
      );
    | Invalid(_) // Shouldn't be hit
    | Parens(_) =>
      // Shouldn't be hit?
      default
    }
  | Some(InfoTyp({term, _})) =>
    switch (bypass_parens_typ(term).term) {
    | EmptyHole =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          HoleTyp.empty_hole_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        HoleTyp.empty_hole_typ_group,
        doc.explanation.message,
        [],
      );
    | MultiHole(_) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          HoleTyp.multi_hole_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        HoleTyp.multi_hole_typ_group,
        doc.explanation.message,
        [],
      );
    | Int =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalTyp.int_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalTyp.int_typ_group,
        doc.explanation.message,
        [],
      );
    | Float =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalTyp.float_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalTyp.float_typ_group,
        doc.explanation.message,
        [],
      );
    | Bool =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalTyp.bool_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalTyp.bool_typ_group,
        doc.explanation.message,
        [],
      );
    | String =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalTyp.str_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalTyp.str_typ_group,
        doc.explanation.message,
        [],
      );
    | List(elem) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          ListTyp.list_typ_group,
          docs,
        );
      let elem_id = List.nth(elem.ids, 0);
      get_message(
        doc,
        options,
        ListTyp.list_typ_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i"),
          elem_id,
        ),
        ListTyp.list_typ_coloring_ids(~elem_id),
      );
    | Arrow(arg, result) =>
      let arg_id = List.nth(arg.ids, 0);
      let result_id = List.nth(result.ids, 0);
      let basic = (doc, group, options) =>
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            arg_id,
            result_id,
          ),
          ArrowTyp.arrow_typ_coloring_ids(~arg_id, ~result_id),
        );
      switch (result.term) {
      | TermBase.UTyp.Arrow(arg2, result2) =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ArrowTyp.arrow3_typ_group,
            docs,
          );
        if (ArrowTyp.arrow3_typ.id == doc.id) {
          let arg2_id = List.nth(arg2.ids, 0);
          let result2_id = List.nth(result2.ids, 0);
          get_message(
            doc,
            options,
            ArrowTyp.arrow3_typ_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              arg_id,
              arg2_id,
              result2_id,
            ),
            ArrowTyp.arrow3_typ_coloring_ids(
              ~arg1_id=arg_id,
              ~arg2_id,
              ~result_id=result2_id,
            ),
          );
        } else {
          basic(doc, ArrowTyp.arrow3_typ_group, options);
        };
      | _ =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            ArrowTyp.arrow_typ_group,
            docs,
          );
        basic(doc, ArrowTyp.arrow_typ_group, options);
      };
    | Tuple(elements) =>
      let basic = (doc, group, options) =>
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            List.length(elements),
          ),
          [],
        );
      switch (List.length(elements)) {
      | 2 =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TupleTyp.tuple2_typ_group,
            docs,
          );
        if (TupleTyp.tuple2_typ.id == doc.id) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          get_message(
            doc,
            options,
            TupleTyp.tuple2_typ_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              elem1_id,
              elem2_id,
            ),
            TupleTyp.tuple2_typ_coloring_ids(~elem1_id, ~elem2_id),
          );
        } else {
          basic(doc, TupleTyp.tuple2_typ_group, options);
        };
      | 3 =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TupleTyp.tuple3_typ_group,
            docs,
          );
        if (TupleTyp.tuple3_typ.id == doc.id) {
          let elem1_id = List.nth(List.nth(elements, 0).ids, 0);
          let elem2_id = List.nth(List.nth(elements, 1).ids, 0);
          let elem3_id = List.nth(List.nth(elements, 2).ids, 0);
          get_message(
            doc,
            options,
            TupleTyp.tuple3_typ_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              elem1_id,
              elem2_id,
              elem3_id,
            ),
            TupleTyp.tuple3_typ_coloring_ids(~elem1_id, ~elem2_id, ~elem3_id),
          );
        } else {
          basic(doc, TupleTyp.tuple3_typ_group, options);
        };
      | _ =>
        let (doc, options) =
          ExplainThisMessages.get_form_and_options(
            TupleTyp.tuple_typ_group,
            docs,
          );
        basic(doc, TupleTyp.tuple_typ_group, options);
      };
    | Var(v) =>
      let (doc, options) =
        ExplainThisMessages.get_form_and_options(
          TerminalTyp.var_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        TerminalTyp.var_typ_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%s"),
          v,
        ),
        [],
      );
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
    (~doc: ExplainThisMessages.t, index': option(int), info_map: Statics.map) => {
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
      ~doc: ExplainThisMessages.t,
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
                  Update.UpdateExplainThisMessages(
                    ExplainThisMessages.ToggleHighlight,
                  ),
                )
              ),
              div(
                ~attr=
                  Attr.many([
                    clss(["close"]),
                    Attr.on_click(_ =>
                      inject(
                        Update.UpdateExplainThisMessages(
                          ExplainThisMessages.ToggleShow,
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
