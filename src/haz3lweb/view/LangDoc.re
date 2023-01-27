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
    (~inject, id, explanation: LangDocMessages.explanation) => {
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
        Update.UpdateLangDocMessages(
          ToggleExplanationFeedback(id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        Update.UpdateLangDocMessages(
          ToggleExplanationFeedback(id, ThumbsDown),
        ),
      ),
  );
};

let example_feedback_view = (~inject, id, example: LangDocMessages.example) => {
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
        Update.UpdateLangDocMessages(
          ToggleExampleFeedback(id, example.sub_id, ThumbsUp),
        ),
      ),
    down_active,
    _ =>
      inject(
        Update.UpdateLangDocMessages(
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
          let id = Id.mk(Default, int_of_string(id));
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
      ~doc: LangDocMessages.t,
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
                                  Update.UpdateLangDocMessages(
                                    LangDocMessages.UpdateGroupSelection(
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
                                  Update.UpdateLangDocMessages(
                                    LangDocMessages.UpdateGroupSelection(
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
                      Update.UpdateLangDocMessages(
                        LangDocMessages.SpecificityOpen(
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
      ~examples: list(LangDocMessages.example),
    ) => {
  div(
    ~attr=Attr.id("examples"),
    List.length(examples) == 0
      ? [text("No examples available")]
      : List.map(
          ({term, message, _} as example: LangDocMessages.example) => {
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
      Model.settings,
    )
  | Colorings;

let get_doc =
    (~docs: LangDocMessages.t, info: option(Statics.t), mode: message_mode)
    : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
  let default = (
    [text("No syntactic form available")],
    ([text("No explanation available")], ColorSteps.empty),
    [text("No examples available")],
  );
  let get_message =
      (
        doc: LangDocMessages.form,
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
              ((syntactic_form_id: Id.t, code_id: Id.t)) => {
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
            (term: TermBase.UExp.term)
            : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) =>
      switch (term) {
      | Closure(_) => failwith("get_doc UExp.Closure")
      | Hole(_, EmptyHole) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.empty_hole_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.empty_hole_exp_group,
          doc.explanation.message,
          [],
        );
      | Hole(_, MultiHole(_children)) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.multi_hole_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.multi_hole_exp_group,
          doc.explanation.message,
          [],
        );
      | Hole(_) => failwith("get_doc UExp.Hole")
      | Triv =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.triv_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.triv_exp_group,
          doc.explanation.message,
          [],
        );
      | Bool(_bool_lit) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.bool_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.bool_exp_group,
          doc.explanation.message,
          [],
        );
      | Int(_int_lit) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.int_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.int_exp_group,
          doc.explanation.message,
          [],
        );
      | Float(_float_lit) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.float_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.float_exp_group,
          doc.explanation.message,
          [],
        );
      | String(_str_lit) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.string_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.string_exp_group,
          doc.explanation.message,
          [],
        );
      | ListLit(terms, None) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.list_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.list_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            List.length(terms),
          ),
          [],
        );
      | ListLit(_, Some(_)) => failwith("get_doc UExp.ListLit(_, Some(_))")
      | FixF(_) => failwith("get_doc UExp.FixF")
      | Fun(pat, None, body, None) =>
        let basic = (doc: LangDocMessages.form, group_id, options) => {
          let pat_id = CH.Ids.rep_id(pat.ids);
          let body_id = CH.Ids.rep_id(body.ids);
          get_message(
            doc,
            options,
            group_id,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              pat_id.id,
              body_id.id,
            ),
            LangDocMessages.function_exp_coloring_ids(~pat_id, ~body_id),
          );
        };
        let pat = bypass_parens_and_annot_pat(pat);
        switch (pat.term) {
        | Hole(_, EmptyHole) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_empty_hole_group,
              docs,
            );
          if (LangDocMessages.function_empty_hole_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_empty_hole_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id.id,
                body_id.id,
                pat_id.id,
              ), // https://stackoverflow.com/questions/31998408/ocaml-converting-strings-to-a-unit-string-format
              LangDocMessages.function_empty_hole_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_empty_hole_group, options);
          };
        | Hole(_, MultiHole(_)) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_multi_hole_group,
              docs,
            );
          if (LangDocMessages.function_multi_hole_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_multi_hole_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id.id,
                body_id.id,
                pat_id.id,
              ),
              LangDocMessages.function_multi_hole_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_multi_hole_group, options);
          };
        | Hole(_) => failwith("get_doc on UPat.Hole(_)")
        | Wild =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_wild_group,
              docs,
            );
          if (LangDocMessages.function_wild_exp.id == doc.id) {
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_wild_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i"),
                body_id.id,
              ),
              LangDocMessages.function_wild_exp_coloring_ids(~body_id),
            );
          } else {
            basic(doc, LangDocMessages.function_wild_group, options);
          };
        | Int(i) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_int_group,
              docs,
            );
          if (LangDocMessages.function_intlit_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_int_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i%i"),
                pat_id.id,
                i,
                pat_id.id,
                body_id.id,
              ),
              LangDocMessages.function_intlit_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_int_group, options);
          };
        | Float(f) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_float_group,
              docs,
            );
          if (LangDocMessages.function_floatlit_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_float_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%f%i%i"),
                pat_id.id,
                f,
                pat_id.id,
                body_id.id,
              ),
              LangDocMessages.function_floatlit_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_float_group, options);
          };
        | Bool(b) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_bool_group,
              docs,
            );
          if (LangDocMessages.function_boollit_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_bool_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%b%i%i"),
                pat_id.id,
                b,
                pat_id.id,
                body_id.id,
              ),
              LangDocMessages.function_boollit_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_bool_group, options);
          };
        | String(s) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_str_group,
              docs,
            );
          if (LangDocMessages.function_strlit_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_str_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%s%i%i"),
                pat_id.id,
                s,
                pat_id.id,
                body_id.id,
              ),
              LangDocMessages.function_strlit_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_str_group, options);
          };
        | Triv =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_triv_group,
              docs,
            );
          if (LangDocMessages.function_triv_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_triv_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id.id,
                pat_id.id,
                body_id.id,
              ),
              LangDocMessages.function_triv_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_triv_group, options);
          };
        | ListLit(elements, _) =>
          if (List.length(elements) == 0) {
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.function_listnil_group,
                docs,
              );
            if (LangDocMessages.function_listnil_exp.id == doc.id) {
              let pat_id = CH.Ids.rep_id(pat.ids);
              let body_id = CH.Ids.rep_id(body.ids);
              get_message(
                doc,
                options,
                LangDocMessages.function_listnil_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  pat_id.id,
                  pat_id.id,
                  body_id.id,
                ),
                LangDocMessages.function_listnil_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              );
            } else {
              basic(doc, LangDocMessages.function_listnil_group, options);
            };
          } else {
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.function_listlit_group,
                docs,
              );
            if (LangDocMessages.function_listlit_exp.id == doc.id) {
              let pat_id = CH.Ids.rep_id(pat.ids);
              let body_id = CH.Ids.rep_id(body.ids);
              get_message(
                doc,
                options,
                LangDocMessages.function_listlit_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  pat_id.id,
                  List.length(elements),
                  pat_id.id,
                  body_id.id,
                ),
                LangDocMessages.function_listlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              );
            } else {
              basic(doc, LangDocMessages.function_listlit_group, options);
            };
          }
        | Cons(hd, tl) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_cons_group,
              docs,
            );
          if (LangDocMessages.function_cons_exp.id == doc.id) {
            let hd_id = CH.Ids.rep_id(hd.ids);
            let tl_id = CH.Ids.rep_id(tl.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_cons_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                hd_id.id,
                tl_id.id,
                body_id.id,
              ),
              LangDocMessages.function_cons_exp_coloring_ids(
                ~hd_id,
                ~tl_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_cons_group, options);
          };
        | Inj(_) => failwith("get_doc UPat.Inj")
        | Var(var) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_var_group,
              docs,
            );
          if (LangDocMessages.function_var_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_var_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%s%i"),
                pat_id.id,
                var,
                body_id.id,
              ),
              LangDocMessages.function_var_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_var_group, options);
          };
        | Tuple(elements) =>
          let pat_id = CH.Ids.rep_id(pat.ids);
          let body_id = CH.Ids.rep_id(body.ids);
          let basic_tuple = (doc: LangDocMessages.form, group_id, options) => {
            get_message(
              doc,
              options,
              group_id,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i%i"),
                pat_id.id,
                List.length(elements),
                pat_id.id,
                body_id.id,
              ),
              LangDocMessages.function_tuple_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          };

          switch (List.length(elements)) {
          | 2 =>
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.function_tuple_2_group,
                docs,
              );
            if (LangDocMessages.function_tuple2_exp.id == doc.id) {
              let pat1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
              let pat2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
              get_message(
                doc,
                options,
                LangDocMessages.function_tuple_2_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  pat1_id.id,
                  pat2_id.id,
                  body_id.id,
                ),
                LangDocMessages.function_tuple2_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~body_id,
                ),
              );
            } else if (LangDocMessages.function_tuple_exp.id == doc.id) {
              basic_tuple(
                doc,
                LangDocMessages.function_tuple_2_group,
                options,
              );
            } else {
              basic(doc, LangDocMessages.function_tuple_2_group, options);
            };
          | 3 =>
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.function_tuple_3_group,
                docs,
              );
            if (LangDocMessages.function_tuple3_exp.id == doc.id) {
              let pat1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
              let pat2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
              let pat3_id = CH.Ids.rep_id(List.nth(elements, 2).ids);
              get_message(
                doc,
                options,
                LangDocMessages.function_tuple_3_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  pat1_id.id,
                  pat2_id.id,
                  pat3_id.id,
                  body_id.id,
                ),
                LangDocMessages.function_tuple3_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~pat3_id,
                  ~body_id,
                ),
              );
            } else if (LangDocMessages.function_tuple_exp.id == doc.id) {
              basic_tuple(
                doc,
                LangDocMessages.function_tuple_3_group,
                options,
              );
            } else {
              basic(doc, LangDocMessages.function_tuple_3_group, options);
            };
          | _ =>
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.function_tuple_group,
                docs,
              );
            if (LangDocMessages.function_tuple_exp.id == doc.id) {
              basic_tuple(doc, LangDocMessages.function_tuple_group, options);
            } else {
              basic(doc, LangDocMessages.function_tuple_group, options);
            };
          };
        | Ap(con, arg) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_ap_group,
              docs,
            );
          if (LangDocMessages.function_ap_exp.id == doc.id) {
            let con_id = CH.Ids.rep_id(con.ids);
            let arg_id = CH.Ids.rep_id(arg.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_ap_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                con_id.id,
                arg_id.id,
                body_id.id,
              ),
              LangDocMessages.function_ap_exp_coloring_ids(
                ~con_id,
                ~arg_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_ap_group, options);
          };
        | Tag(v) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.function_tag_group,
              docs,
            );
          if (LangDocMessages.function_tag_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.function_tag_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%s%i%i"),
                pat_id.id,
                v,
                pat_id.id,
                body_id.id,
              ),
              LangDocMessages.function_tag_exp_coloring_ids(
                ~pat_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.function_tag_group, options);
          };
        | Parens(_) => default // Shouldn't get hit?
        | TypeAnn(_) => default // Shouldn't get hit?
        };
      | Fun(_) => failwith("get_doc UExp.Fun with non-empty type or name")
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
            LangDocMessages.get_form_and_options(
              LangDocMessages.tuple_exp_2_group,
              docs,
            );
          if (LangDocMessages.tuple_exp_size2.id == doc.id) {
            let exp1_id = CH.Ids.rep_id(List.nth(terms, 0).ids);
            let exp2_id = CH.Ids.rep_id(List.nth(terms, 1).ids);
            get_message(
              doc,
              options,
              LangDocMessages.tuple_exp_2_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i"),
                exp1_id.id,
                exp2_id.id,
              ),
              LangDocMessages.tuple_exp_size2_coloring_ids(
                ~exp1_id,
                ~exp2_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.tuple_exp_2_group, options);
          };
        | 3 =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.tuple_exp_3_group,
              docs,
            );
          if (LangDocMessages.tuple_exp_size3.id == doc.id) {
            let exp1_id = CH.Ids.rep_id(List.nth(terms, 0).ids);
            let exp2_id = CH.Ids.rep_id(List.nth(terms, 1).ids);
            let exp3_id = CH.Ids.rep_id(List.nth(terms, 2).ids);
            get_message(
              doc,
              options,
              LangDocMessages.tuple_exp_3_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                exp1_id.id,
                exp2_id.id,
                exp3_id.id,
              ),
              LangDocMessages.tuple_exp_size3_coloring_ids(
                ~exp1_id,
                ~exp2_id,
                ~exp3_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.tuple_exp_3_group, options);
          };
        | _ =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.tuple_exp_group,
              docs,
            );
          basic(doc, LangDocMessages.tuple_exp_group, options);
        };
      | Var(_var) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.var_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.var_exp_group,
          doc.explanation.message,
          [],
        );
      | Let(pat, def, body) =>
        let basic = (doc: LangDocMessages.form, group_id, options) => {
          let pat_id = CH.Ids.rep_id(pat.ids);
          let def_id = CH.Ids.rep_id(def.ids);
          get_message(
            doc,
            options,
            group_id,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              def_id.id,
              pat_id.id,
            ),
            LangDocMessages.let_base_exp_coloring_ids(~pat_id, ~def_id),
          );
        };
        let pat = bypass_parens_and_annot_pat(pat);
        switch (pat.term) {
        | Hole(_, EmptyHole) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_empty_hole_exp_group,
              docs,
            );
          if (LangDocMessages.let_empty_hole_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_empty_hole_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id.id,
                def_id.id,
                pat_id.id,
              ),
              LangDocMessages.let_empty_hole_exp_coloring_ids(
                ~pat_id,
                ~def_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.let_empty_hole_exp_group, options);
          };
        | Hole(_, MultiHole(_)) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_multi_hole_exp_group,
              docs,
            );
          if (LangDocMessages.let_multi_hole_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_multi_hole_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                pat_id.id,
                def_id.id,
                pat_id.id,
              ),
              LangDocMessages.let_multi_hole_exp_coloring_ids(
                ~pat_id,
                ~def_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.let_multi_hole_exp_group, options);
          };
        | Hole(_) => failwith("get_doc on UPat.Hole(_)")
        | Wild =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_wild_exp_group,
              docs,
            );
          if (LangDocMessages.let_wild_exp.id == doc.id) {
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_wild_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id.id,
                def_id.id,
                body_id.id,
              ),
              LangDocMessages.let_wild_exp_coloring_ids(~def_id, ~body_id),
            );
          } else {
            basic(doc, LangDocMessages.let_wild_exp_group, options);
          };
        | Int(i) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_int_exp_group,
              docs,
            );
          if (LangDocMessages.let_int_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_int_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%i%i%i",
                ),
                def_id.id,
                pat_id.id,
                i,
                def_id.id,
                body_id.id,
              ),
              LangDocMessages.let_int_exp_coloring_ids(
                ~pat_id,
                ~def_id,
                ~body_id,
              ),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and some other places when switching between forms and specificity levels... maybe a Safari issue... */
            basic(
              doc,
              LangDocMessages.let_int_exp_group,
              options,
            );
          };
        | Float(f) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_float_exp_group,
              docs,
            );
          if (LangDocMessages.let_float_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            // TODO Make sure everywhere printing the float literal print it prettier
            get_message(
              doc,
              options,
              LangDocMessages.let_float_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%f%i%i",
                ),
                def_id.id,
                pat_id.id,
                f,
                def_id.id,
                body_id.id,
              ),
              LangDocMessages.let_float_exp_coloring_ids(
                ~pat_id,
                ~def_id,
                ~body_id,
              ),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              doc,
              LangDocMessages.let_float_exp_group,
              options,
            );
          };
        | Bool(b) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_bool_exp_group,
              docs,
            );
          if (LangDocMessages.let_bool_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_bool_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%b%i%i",
                ),
                def_id.id,
                pat_id.id,
                b,
                def_id.id,
                body_id.id,
              ),
              LangDocMessages.let_bool_exp_coloring_ids(
                ~pat_id,
                ~def_id,
                ~body_id,
              ),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              doc,
              LangDocMessages.let_bool_exp_group,
              options,
            );
          };
        | String(s) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_str_exp_group,
              docs,
            );
          if (LangDocMessages.let_str_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_str_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%s%i%i",
                ),
                def_id.id,
                pat_id.id,
                s,
                def_id.id,
                body_id.id,
              ),
              LangDocMessages.let_str_exp_coloring_ids(
                ~pat_id,
                ~def_id,
                ~body_id,
              ),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              doc,
              LangDocMessages.let_str_exp_group,
              options,
            );
          };
        | Triv =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_triv_exp_group,
              docs,
            );
          if (LangDocMessages.let_triv_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_triv_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i%i"),
                def_id.id,
                pat_id.id,
                def_id.id,
                body_id.id,
              ),
              LangDocMessages.let_triv_exp_coloring_ids(
                ~pat_id,
                ~def_id,
                ~body_id,
              ),
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and other places when switching syntactic specificities... seems like might be Safari issue... */
            basic(
              doc,
              LangDocMessages.let_triv_exp_group,
              options,
            );
          };
        | ListLit(elements, None) =>
          if (List.length(elements) == 0) {
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.let_listnil_exp_group,
                docs,
              );
            if (LangDocMessages.let_listnil_exp.id == doc.id) {
              let pat_id = CH.Ids.rep_id(pat.ids);
              let def_id = CH.Ids.rep_id(def.ids);
              let body_id = CH.Ids.rep_id(body.ids);
              get_message(
                doc,
                options,
                LangDocMessages.let_listnil_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  def_id.id,
                  pat_id.id,
                  def_id.id,
                  body_id.id,
                ),
                LangDocMessages.let_listnil_exp_coloring_ids(
                  ~pat_id,
                  ~def_id,
                  ~body_id,
                ),
              );
            } else {
              basic(doc, LangDocMessages.let_listnil_exp_group, options);
            };
          } else {
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.let_listlit_exp_group,
                docs,
              );
            if (LangDocMessages.let_listlit_exp.id == doc.id) {
              let pat_id = CH.Ids.rep_id(pat.ids);
              let def_id = CH.Ids.rep_id(def.ids);
              get_message(
                doc,
                options,
                LangDocMessages.let_listlit_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  def_id.id,
                  pat_id.id,
                  List.length(elements),
                ),
                LangDocMessages.let_listlit_exp_coloring_ids(
                  ~pat_id,
                  ~def_id,
                ),
              );
            } else {
              basic(doc, LangDocMessages.let_listlit_exp_group, options);
            };
          }
        | ListLit(_, Some(_)) => failwith("get_doc on ListLit(_, Some(_))")
        | Cons(hd, tl) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_cons_exp_group,
              docs,
            );
          if (LangDocMessages.let_cons_exp.id == doc.id) {
            let hd_id = CH.Ids.rep_id(hd.ids);
            let tl_id = CH.Ids.rep_id(tl.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_cons_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id.id,
                hd_id.id,
                tl_id.id,
              ),
              LangDocMessages.let_cons_exp_coloring_ids(
                ~hd_id,
                ~tl_id,
                ~def_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.let_cons_exp_group, options);
          };
        | Inj(_) => failwith("get_doc UPat.Inj")
        | Var(var) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_var_exp_group,
              docs,
            );
          if (LangDocMessages.let_var_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_var_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%s%i"),
                def_id.id,
                pat_id.id,
                var,
                body_id.id,
              ),
              LangDocMessages.let_var_exp_coloring_ids(
                ~pat_id,
                ~def_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.let_var_exp_group, options);
          };
        | Tuple(elements) =>
          let pat_id = CH.Ids.rep_id(pat.ids);
          let def_id = CH.Ids.rep_id(def.ids);
          let basic_tuple = (doc: LangDocMessages.form, group_id, options) => {
            get_message(
              doc,
              options,
              group_id,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id.id,
                pat_id.id,
                List.length(elements),
              ),
              LangDocMessages.let_tuple_exp_coloring_ids(~pat_id, ~def_id),
            );
          };

          switch (List.length(elements)) {
          | 2 =>
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.let_tuple2_exp_group,
                docs,
              );
            if (LangDocMessages.let_tuple2_exp.id == doc.id) {
              let pat1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
              let pat2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
              get_message(
                doc,
                options,
                LangDocMessages.let_tuple2_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                  def_id.id,
                  pat1_id.id,
                  pat2_id.id,
                ),
                LangDocMessages.let_tuple2_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~def_id,
                ),
              );
            } else if (LangDocMessages.let_tuple_exp.id == doc.id) {
              basic_tuple(doc, LangDocMessages.let_tuple2_exp_group, options);
            } else {
              basic(doc, LangDocMessages.let_tuple2_exp_group, options);
            };
          | 3 =>
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.let_tuple3_exp_group,
                docs,
              );
            // TODO Syntactic form can go off page - so can examples - but can scroll, just can't see bottom scroll bar
            if (LangDocMessages.let_tuple3_exp.id == doc.id) {
              let pat1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
              let pat2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
              let pat3_id = CH.Ids.rep_id(List.nth(elements, 2).ids);
              get_message(
                doc,
                options,
                LangDocMessages.let_tuple3_exp_group,
                Printf.sprintf(
                  Scanf.format_from_string(
                    doc.explanation.message,
                    "%i%i%i%i",
                  ),
                  def_id.id,
                  pat1_id.id,
                  pat2_id.id,
                  pat3_id.id,
                ),
                LangDocMessages.let_tuple3_exp_coloring_ids(
                  ~pat1_id,
                  ~pat2_id,
                  ~pat3_id,
                  ~def_id,
                ),
              );
            } else if (LangDocMessages.let_tuple_exp.id == doc.id) {
              basic_tuple(doc, LangDocMessages.let_tuple3_exp_group, options);
            } else {
              basic(doc, LangDocMessages.let_tuple3_exp_group, options);
            };
          | _ =>
            let (doc, options) =
              LangDocMessages.get_form_and_options(
                LangDocMessages.let_tuple_base_exp_group,
                docs,
              );
            if (LangDocMessages.let_tuple_exp.id == doc.id) {
              basic_tuple(
                doc,
                LangDocMessages.let_tuple_base_exp_group,
                options,
              );
            } else {
              basic(doc, LangDocMessages.let_tuple_base_exp_group, options);
            };
          };
        | Ap(con, arg) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_ap_exp_group,
              docs,
            );
          if (LangDocMessages.let_ap_exp.id == doc.id) {
            let con_id = CH.Ids.rep_id(con.ids);
            let arg_id = CH.Ids.rep_id(arg.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_ap_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
                def_id.id,
                con_id.id,
                arg_id.id,
              ),
              LangDocMessages.let_ap_exp_coloring_ids(
                ~con_id,
                ~arg_id,
                ~def_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.let_ap_exp_group, options);
          };
        | Tag(v) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.let_tag_exp_group,
              docs,
            );
          if (LangDocMessages.let_tag_exp.id == doc.id) {
            let pat_id = CH.Ids.rep_id(pat.ids);
            let def_id = CH.Ids.rep_id(def.ids);
            let body_id = CH.Ids.rep_id(body.ids);
            get_message(
              doc,
              options,
              LangDocMessages.let_tag_exp_group,
              Printf.sprintf(
                Scanf.format_from_string(
                  doc.explanation.message,
                  "%i%i%s%i%i",
                ),
                def_id.id,
                pat_id.id,
                v,
                def_id.id,
                body_id.id,
              ),
              LangDocMessages.let_tag_exp_coloring_ids(
                ~pat_id,
                ~def_id,
                ~body_id,
              ),
            );
          } else {
            basic(doc, LangDocMessages.let_tag_exp_group, options);
          };
        | Parens(_) => default // Shouldn't get hit?
        | TypeAnn(_) => default // Shouldn't get hit?
        };
      | ApBuiltin(_) => failwith("get_doc on ApBuiltin(_)")
      | Ap(x, arg) =>
        let x_id = CH.Ids.rep_id(x.ids);
        let arg_id = CH.Ids.rep_id(arg.ids);
        let basic =
            (doc: LangDocMessages.form, group, options, msg, coloring_ids) => {
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
            LangDocMessages.get_form_and_options(
              LangDocMessages.conapp_exp_group,
              docs,
            );
          basic(
            doc,
            LangDocMessages.conapp_exp_group,
            options,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%s%i%i"),
              v,
              x_id.id,
              arg_id.id,
            ),
            LangDocMessages.conapp_exp_coloring_ids,
          );
        | _ =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.funapp_exp_group,
              docs,
            );
          basic(
            doc,
            LangDocMessages.funapp_exp_group,
            options,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              x_id.id,
              arg_id.id,
            ),
            LangDocMessages.funapp_exp_coloring_ids,
          );
        };
      | If(cond, then_, else_) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.if_exp_group,
            docs,
          );
        let cond_id = CH.Ids.rep_id(cond.ids);
        let then_id = CH.Ids.rep_id(then_.ids);
        let else_id = CH.Ids.rep_id(else_.ids);
        get_message(
          doc,
          options,
          LangDocMessages.if_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
            cond_id.id,
            then_id.id,
            else_id.id,
          ),
          LangDocMessages.if_exp_coloring_ids(~cond_id, ~then_id, ~else_id),
        );
      | Seq(left, right) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.seq_exp_group,
            docs,
          );
        let exp1_id = CH.Ids.rep_id(left.ids);
        let exp2_id = CH.Ids.rep_id(right.ids);
        get_message(
          doc,
          options,
          LangDocMessages.seq_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            exp1_id.id,
            exp2_id.id,
          ),
          LangDocMessages.seq_exp_coloring_ids(~exp1_id, ~exp2_id),
        );
      | Test(body, None) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.test_group,
            docs,
          );
        let body_id = CH.Ids.rep_id(body.ids);
        get_message(
          doc,
          options,
          LangDocMessages.test_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            body_id.id,
          ),
          LangDocMessages.test_exp_coloring_ids(~body_id),
        );
      | Test(_, Some(_)) => failwith("get_doc on Test(_, Some(_))")
      | Parens(term) => get_message_exp(term.term) // No Special message?
      | Cons(hd, tl) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.cons_exp_group,
            docs,
          );
        let hd_id = CH.Ids.rep_id(hd.ids);
        let tl_id = CH.Ids.rep_id(tl.ids);
        get_message(
          doc,
          options,
          LangDocMessages.cons_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            hd_id.id,
            tl_id.id,
          ),
          LangDocMessages.cons_exp_coloring_ids(~hd_id, ~tl_id),
        );
      | Prj(_) => failwith("get_doc UExp.Prj")
      | Inj(_) => failwith("get_doc UExp.Inj")
      | UnOp(op, exp) =>
        switch (op) {
        | Int(Minus) =>
          let (doc, options) =
            LangDocMessages.get_form_and_options(
              LangDocMessages.int_unary_minus_group,
              docs,
            );
          let exp_id = CH.Ids.rep_id(exp.ids);
          get_message(
            doc,
            options,
            LangDocMessages.int_unary_minus_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i"),
              exp_id.id,
            ),
            LangDocMessages.int_unary_minus_exp_coloring_ids(~exp_id),
          );
        }
      | BinOp(op, left, right) =>
        let (group, coloring_ids) =
          switch (op) {
          | Int(Plus) => (
              LangDocMessages.int_plus_group,
              LangDocMessages.int_plus_exp_coloring_ids,
            )
          | Int(Minus) => (
              LangDocMessages.int_minus_group,
              LangDocMessages.int_minus_exp_coloring_ids,
            )
          | Int(Times) => (
              LangDocMessages.int_times_group,
              LangDocMessages.int_times_exp_coloring_ids,
            )
          | Int(Power) => (
              LangDocMessages.int_power_group,
              LangDocMessages.int_power_exp_coloring_ids,
            )
          | Int(Divide) => (
              LangDocMessages.int_divide_group,
              LangDocMessages.int_divide_exp_coloring_ids,
            )
          | Int(LessThan) => (
              LangDocMessages.int_lt_group,
              LangDocMessages.int_lt_exp_coloring_ids,
            )
          | Int(LessThanOrEqual) => (
              LangDocMessages.int_lte_group,
              LangDocMessages.int_lte_exp_coloring_ids,
            )
          | Int(GreaterThan) => (
              LangDocMessages.int_gt_group,
              LangDocMessages.int_gt_exp_coloring_ids,
            )
          | Int(GreaterThanOrEqual) => (
              LangDocMessages.int_gte_group,
              LangDocMessages.int_gte_exp_coloring_ids,
            )
          | Int(Equals) => (
              LangDocMessages.int_eq_group,
              LangDocMessages.int_eq_exp_coloring_ids,
            )
          | Float(Plus) => (
              LangDocMessages.float_plus_group,
              LangDocMessages.float_plus_exp_coloring_ids,
            )
          | Float(Minus) => (
              LangDocMessages.float_minus_group,
              LangDocMessages.float_minus_exp_coloring_ids,
            )
          | Float(Times) => (
              LangDocMessages.float_times_group,
              LangDocMessages.float_times_exp_coloring_ids,
            )
          | Float(Power) => (
              LangDocMessages.float_power_group,
              LangDocMessages.float_power_exp_coloring_ids,
            )
          | Float(Divide) => (
              LangDocMessages.float_divide_group,
              LangDocMessages.float_divide_exp_coloring_ids,
            )
          | Float(LessThan) => (
              LangDocMessages.float_lt_group,
              LangDocMessages.float_lt_exp_coloring_ids,
            )
          | Float(LessThanOrEqual) => (
              LangDocMessages.float_lte_group,
              LangDocMessages.float_lte_exp_coloring_ids,
            )
          | Float(GreaterThan) => (
              LangDocMessages.float_gt_group,
              LangDocMessages.float_gt_exp_coloring_ids,
            )
          | Float(GreaterThanOrEqual) => (
              LangDocMessages.float_gte_group,
              LangDocMessages.float_gte_exp_coloring_ids,
            )
          | Float(Equals) => (
              LangDocMessages.float_eq_group,
              LangDocMessages.float_eq_exp_coloring_ids,
            )
          | Bool(And) => (
              LangDocMessages.bool_and_group,
              LangDocMessages.bool_and_exp_coloring_ids,
            )
          | Bool(Or) => (
              LangDocMessages.bool_or_group,
              LangDocMessages.bool_or_exp_coloring_ids,
            )
          | String(Equals) => (
              LangDocMessages.str_eq_group,
              LangDocMessages.str_eq_exp_coloring_ids,
            )
          };
        let (doc, options) =
          LangDocMessages.get_form_and_options(group, docs);
        let left_id = CH.Ids.rep_id(left.ids);
        let right_id = CH.Ids.rep_id(right.ids);
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            left_id.id,
            right_id.id,
          ),
          coloring_ids(~left_id, ~right_id),
        );
      | Match(scrut, _rules, _) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.case_exp_group,
            docs,
          );
        let scrut_id = CH.Ids.rep_id(scrut.ids);
        get_message(
          doc,
          options,
          LangDocMessages.case_exp_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            scrut_id.id,
          ),
          LangDocMessages.case_exp_coloring_ids(~scrut_id),
        );
      | Cast(_) => failwith("get_doc UExp.Cast")
      | Tag(v) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.tag_exp_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.tag_exp_group,
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
    | Hole(_, EmptyHole) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.empty_hole_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.empty_hole_pat_group,
        doc.explanation.message,
        [],
      );
    | Hole(_, MultiHole(_)) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.multi_hole_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.multi_hole_pat_group,
        doc.explanation.message,
        [],
      );
    | Hole(_) => failwith("get_doc on UPat.Hole(_)")
    | Wild =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.wild_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.wild_pat_group,
        doc.explanation.message,
        [],
      );
    | Int(i) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.intlit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.intlit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i%i"),
          i,
          i,
        ),
        [],
      );
    | Float(f) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.floatlit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.floatlit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%f%f"),
          f,
          f,
        ),
        [],
      );
    | Bool(b) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.boollit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.boollit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%b%b"),
          b,
          b,
        ),
        [],
      );
    | String(s) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.strlit_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.strlit_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%s%s"),
          s,
          s,
        ),
        [],
      );
    | Triv =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.triv_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.triv_pat_group,
        doc.explanation.message,
        [],
      );
    | ListLit(elements, _) =>
      if (List.length(elements) == 0) {
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.listnil_pat_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.function_listnil_group,
          doc.explanation.message,
          [],
        );
      } else {
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.listlit_pat_group,
            docs,
          );
        get_message(
          doc,
          options,
          LangDocMessages.listlit_pat_group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i"),
            List.length(elements),
          ),
          [],
        );
      }
    | Cons(hd, tl) =>
      let hd_id = CH.Ids.rep_id(hd.ids);
      let tl_id = CH.Ids.rep_id(tl.ids);
      let basic = (doc, group, options) =>
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            hd_id.id,
            tl_id.id,
          ),
          LangDocMessages.cons_base_pat_coloring_ids(~hd_id, ~tl_id),
        );
      switch (tl.term) {
      | TermBase.UPat.Cons(hd2, tl2) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.cons2_pat_group,
            docs,
          );
        if (LangDocMessages.cons2_pat.id == doc.id) {
          let hd2_id = CH.Ids.rep_id(hd2.ids);
          let tl2_id = CH.Ids.rep_id(tl2.ids);
          get_message(
            doc,
            options,
            LangDocMessages.cons2_pat_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              hd_id.id,
              hd2_id.id,
              tl2_id.id,
            ),
            LangDocMessages.cons2_pat_coloring_ids(
              ~fst_id=hd_id,
              ~snd_id=hd2_id,
              ~tl_id=tl2_id,
            ),
          );
        } else {
          basic(doc, LangDocMessages.cons2_pat_group, options);
        };
      | _ =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.cons_pat_group,
            docs,
          );
        basic(doc, LangDocMessages.cons_pat_group, options);
      };
    | Inj(_) => failwith("get_doc UPat.Inj")
    | Var(v) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.var_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.var_pat_group,
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
          LangDocMessages.get_form_and_options(
            LangDocMessages.tuple_pat_2_group,
            docs,
          );
        if (LangDocMessages.tuple_pat_size2.id == doc.id) {
          let elem1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
          let elem2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
          get_message(
            doc,
            options,
            LangDocMessages.tuple_pat_2_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              elem1_id.id,
              elem2_id.id,
            ),
            LangDocMessages.tuple_pat_size2_coloring_ids(
              ~elem1_id,
              ~elem2_id,
            ),
          );
        } else {
          basic(doc, LangDocMessages.tuple_pat_2_group, options);
        };
      | 3 =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.tuple_pat_3_group,
            docs,
          );
        if (LangDocMessages.tuple_pat_size3.id == doc.id) {
          let elem1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
          let elem2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
          let elem3_id = CH.Ids.rep_id(List.nth(elements, 2).ids);
          get_message(
            doc,
            options,
            LangDocMessages.tuple_pat_3_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              elem1_id.id,
              elem2_id.id,
              elem3_id.id,
            ),
            LangDocMessages.tuple_pat_size3_coloring_ids(
              ~elem1_id,
              ~elem2_id,
              ~elem3_id,
            ),
          );
        } else {
          basic(doc, LangDocMessages.tuple_pat_3_group, options);
        };
      | _ =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.tuple_pat_group,
            docs,
          );
        basic(doc, LangDocMessages.tuple_pat_group, options);
      };
    | Ap(con, arg) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.ap_pat_group,
          docs,
        );
      let con_id = CH.Ids.rep_id(con.ids);
      let arg_id = CH.Ids.rep_id(arg.ids);
      get_message(
        doc,
        options,
        LangDocMessages.ap_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i%i"),
          con_id.id,
          arg_id.id,
        ),
        LangDocMessages.ap_pat_coloring_ids(~con_id, ~arg_id),
      );
    | Tag(con) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.tag_pat_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.tag_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%s"),
          con,
        ),
        [],
      );
    | TypeAnn(pat, typ) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.typann_pat_group,
          docs,
        );
      let pat_id = CH.Ids.rep_id(pat.ids);
      let typ_id = CH.Ids.rep_id(typ.ids);
      get_message(
        doc,
        options,
        LangDocMessages.typann_pat_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i%i"),
          pat_id.id,
          typ_id.id,
        ),
        LangDocMessages.typann_pat_coloring_ids(~pat_id, ~typ_id),
      );
    | Parens(_) =>
      // Shouldn't be hit?
      default
    }
  | Some(InfoTyp({term, _})) =>
    switch (bypass_parens_typ(term).term) {
    | EmptyHole =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.empty_hole_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.empty_hole_typ_group,
        doc.explanation.message,
        [],
      );
    | MultiHole(_) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.multi_hole_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.multi_hole_typ_group,
        doc.explanation.message,
        [],
      );
    | Int =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.int_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.int_typ_group,
        doc.explanation.message,
        [],
      );
    | Float =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.float_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.float_typ_group,
        doc.explanation.message,
        [],
      );
    | Bool =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.bool_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.bool_typ_group,
        doc.explanation.message,
        [],
      );
    | String =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.str_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.str_typ_group,
        doc.explanation.message,
        [],
      );
    | List(elem) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.list_typ_group,
          docs,
        );
      let elem_id = CH.Ids.rep_id(elem.ids);
      get_message(
        doc,
        options,
        LangDocMessages.list_typ_group,
        Printf.sprintf(
          Scanf.format_from_string(doc.explanation.message, "%i"),
          elem_id.id,
        ),
        LangDocMessages.list_typ_coloring_ids(~elem_id),
      );
    | Arrow(arg, result) =>
      let arg_id = CH.Ids.rep_id(arg.ids);
      let result_id = CH.Ids.rep_id(result.ids);
      let basic = (doc, group, options) =>
        get_message(
          doc,
          options,
          group,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            arg_id.id,
            result_id.id,
          ),
          LangDocMessages.arrow_typ_coloring_ids(~arg_id, ~result_id),
        );
      switch (result.term) {
      | TermBase.UTyp.Arrow(arg2, result2) =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.arrow3_typ_group,
            docs,
          );
        if (LangDocMessages.arrow3_typ.id == doc.id) {
          let arg2_id = CH.Ids.rep_id(arg2.ids);
          let result2_id = CH.Ids.rep_id(result2.ids);
          get_message(
            doc,
            options,
            LangDocMessages.arrow3_typ_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              arg_id.id,
              arg2_id.id,
              result2_id.id,
            ),
            LangDocMessages.arrow3_typ_coloring_ids(
              ~arg1_id=arg_id,
              ~arg2_id,
              ~result_id=result2_id,
            ),
          );
        } else {
          basic(doc, LangDocMessages.arrow3_typ_group, options);
        };
      | _ =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.arrow_typ_group,
            docs,
          );
        basic(doc, LangDocMessages.arrow_typ_group, options);
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
          LangDocMessages.get_form_and_options(
            LangDocMessages.tuple2_typ_group,
            docs,
          );
        if (LangDocMessages.tuple2_typ.id == doc.id) {
          let elem1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
          let elem2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
          get_message(
            doc,
            options,
            LangDocMessages.tuple2_typ_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i"),
              elem1_id.id,
              elem2_id.id,
            ),
            LangDocMessages.tuple2_typ_coloring_ids(~elem1_id, ~elem2_id),
          );
        } else {
          basic(doc, LangDocMessages.tuple2_typ_group, options);
        };
      | 3 =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.tuple3_typ_group,
            docs,
          );
        if (LangDocMessages.tuple3_typ.id == doc.id) {
          let elem1_id = CH.Ids.rep_id(List.nth(elements, 0).ids);
          let elem2_id = CH.Ids.rep_id(List.nth(elements, 1).ids);
          let elem3_id = CH.Ids.rep_id(List.nth(elements, 2).ids);
          get_message(
            doc,
            options,
            LangDocMessages.tuple3_typ_group,
            Printf.sprintf(
              Scanf.format_from_string(doc.explanation.message, "%i%i%i"),
              elem1_id.id,
              elem2_id.id,
              elem3_id.id,
            ),
            LangDocMessages.tuple3_typ_coloring_ids(
              ~elem1_id,
              ~elem2_id,
              ~elem3_id,
            ),
          );
        } else {
          basic(doc, LangDocMessages.tuple3_typ_group, options);
        };
      | _ =>
        let (doc, options) =
          LangDocMessages.get_form_and_options(
            LangDocMessages.tuple_typ_group,
            docs,
          );
        basic(doc, LangDocMessages.tuple_typ_group, options);
      };
    | Var(v) =>
      let (doc, options) =
        LangDocMessages.get_form_and_options(
          LangDocMessages.var_typ_group,
          docs,
        );
      get_message(
        doc,
        options,
        LangDocMessages.var_typ_group,
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
    (~doc: LangDocMessages.t, index': option(Id.t), info_map: Statics.map) => {
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
      ~settings: Model.settings,
      ~doc: LangDocMessages.t,
      index': option(Id.t),
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
                  Update.UpdateLangDocMessages(
                    LangDocMessages.ToggleHighlight,
                  ),
                )
              ),
              div(
                ~attr=
                  Attr.many([
                    clss(["close"]),
                    Attr.on_click(_ =>
                      inject(
                        Update.UpdateLangDocMessages(
                          LangDocMessages.ToggleShow,
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
