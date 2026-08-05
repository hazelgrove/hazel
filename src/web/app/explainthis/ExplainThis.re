open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;
open Language;

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

let explanation_feedback_view =
    (~inject: ExplainThisUpdate.update => 'a, group_id, form_id, model) => {
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
    _ => inject(ToggleExplanationFeedback(group_id, form_id, ThumbsUp)),
    down_active,
    _ => inject(ToggleExplanationFeedback(group_id, form_id, ThumbsDown)),
  );
};

let example_feedback_view =
    (
      ~inject: ExplainThisUpdate.update => 'a,
      group_id,
      form_id,
      example_id,
      model,
    ) => {
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
      inject(ToggleExampleFeedback(group_id, form_id, example_id, ThumbsUp)),
    down_active,
    _ =>
      inject(
        ToggleExampleFeedback(group_id, form_id, example_id, ThumbsDown),
      ),
  );
};

let code_node = text =>
  Node.span(~attrs=[clss(["code"])], [Node.text(text)]);

let highlight =
    (
      ~globals: Globals.t,
      ~inject as _: ExplainThisUpdate.update => 'a,
      msg: list(Node.t),
      id: Id.t,
      mapping: ColorSteps.t,
    )
    : (Node.t, ColorSteps.t) => {
  let (c, mapping) = ColorSteps.get_color(id, mapping);
  let classes = clss(["highlight-" ++ c, "clickable"]);
  let attrs = [
    classes,
    Attr.on_mouseenter(_ =>
      globals.inject_global(Set(ExplainThis(SetHighlight(Hover(id)))))
    ),
    Attr.on_mouseleave(_ =>
      globals.inject_global(Set(ExplainThis(SetHighlight(UnsetHover))))
    ),
    Attr.on_click(_ => globals.inject_global(JumpToTile(id))),
  ];
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
let mk_translation =
    (~globals, ~inject, text: string): (list(Node.t), ColorSteps.t) => {
  let omd = Omd.of_string(text);
  //print_markdown(omd);

  let rec translate_inline =
          (inline: Omd.inline(_), msg, mapping: ColorSteps.t, ~inject)
          : (list(Node.t), ColorSteps.t) => {
    switch (inline) {
    | Omd.Concat(_, items) =>
      let (nodes, mapping) =
        List.fold_left(
          ((msg, mapping), item) => {
            let (translated_item, mapping) =
              translate_inline(item, [], mapping, ~inject);
            (List.concat([msg, translated_item]), mapping);
          },
          ([], mapping),
          items,
        );
      (List.append(msg, nodes), mapping);
    | Omd.Text(_, d) => (List.append(msg, [Node.text(d)]), mapping)
    | Omd.Code(_, d) => (List.append(msg, [code_node(d)]), mapping)
    | Omd.Link(_, {label, destination, _}) =>
      let (d, mapping) = translate_inline(label, [], mapping, ~inject);
      let id =
        switch (Id.of_string(destination)) {
        | Some(id) => id
        | None => Id.invalid
        };
      let (inner_msg, mapping) =
        highlight(~globals, ~inject, d, id, mapping);
      (List.append(msg, [inner_msg]), mapping);
    | Omd.Emph(_, d) =>
      let (d, mapping) = translate_inline(d, [], mapping, ~inject);
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
    | Omd.Soft_break(_) => (List.append(msg, [Node.br()]), mapping)
    | _ => (msg, mapping)
    };
  };

  let rec translate_block =
          (doc: Omd.doc, mapping: ColorSteps.t)
          : (list(Node.t), ColorSteps.t) => {
    List.fold_left(
      ((msg, mapping), elem) => {
        switch (elem) {
        | Omd.Paragraph(_, d) =>
          /* Each markdown paragraph renders as its own <p> so that blank
             lines in the source produce visible paragraph breaks.
             (Soft line breaks inside a paragraph are already handled by
             [translate_inline] as <br>.) */
          let (p_nodes, mapping) = translate_inline(d, [], mapping, ~inject);
          (List.append(msg, [Node.p(p_nodes)]), mapping);
        | Omd.List(_, _, _, items) =>
          let (bullets, mapping) =
            List.fold_left(
              ((nodes, mapping), d) => {
                let (n, mapping) = translate_block(d, mapping);
                (List.append(nodes, [Node.li(n)]), mapping);
              },
              ([], mapping),
              items,
            );
          (List.append(msg, [Node.ul(bullets)]), mapping); /* TODO Hannah - Should this be an ordered list instead of an unordered list? */
        | _ => (msg, mapping)
        }
      },
      ([], mapping),
      doc,
    );
  };

  translate_block(omd, ColorSteps.empty);
};

let mk_explanation =
    (
      ~globals,
      ~inject,
      group_id,
      form_id,
      text: string,
      model: ExplainThisModel.t,
    )
    : (Node.t, ColorSteps.t) => {
  let (msg, color_map) = mk_translation(~globals, ~inject, text);
  let feedback =
    globals.settings.explainThis.show_feedback
      ? [explanation_feedback_view(~inject, group_id, form_id, model)] : [];
  (
    div([div(~attrs=[clss(["explanation-contents"])], msg)] @ feedback),
    color_map,
  );
};

let expander_deco =
    (
      ~globals as {font_metrics, _} as globals: Globals.t,
      ~docs: ExplainThisModel.t,
      ~inject,
      ~options: list((ExplainThisForm.form_id, Segment.t)),
      ~group: ExplainThisForm.group,
      ~doc: ExplainThisForm.form,
      editor: Editor.Model.t,
    ) => {
  switch (doc.expandable_id, List.length(options)) {
  | (None, _)
  | (_, 0 | 1) => div([])
  | (Some((id, _)), _) =>
    let origin =
      switch (
        TermData.extreme_measures(
          id,
          editor.syntax.term_data,
          editor.syntax.measured,
        )
      ) {
      | Some((origin, _)) => origin
      | None => {
          row: 0,
          col: 0,
        }
      };
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
            let code_view = CodeViewable.view_segment(~globals, segment);
            let classes =
              id == doc.id
                ? ["selected"] @ get_clss(segment) : get_clss(segment);
            let update_group_selection = _ =>
              inject(ExplainThisUpdate.UpdateGroupSelection(group.id, id));
            Node.div(
              ~attrs=[clss(classes), Attr.on_click(update_group_selection)],
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
      div_c(
        "color-highlights",
        Highlight.color(
          ~syntax=editor.syntax,
          ~font_metrics,
          ["expandable"],
          id,
        ),
      );

    let expander =
      div(
        ~attrs=[
          clss(["expandable-target"]),
          DecUtil.abs_position(~font_metrics, origin),
        ],
        [specificity_menu] @ (docs.specificity_open ? [] : [expand_arrow]),
      );

    Node.div(
      ~attrs=[
        clss(["expandable-target"]),
        Attr.on_click(_ =>
          inject(ExplainThisUpdate.SpecificityOpen(!docs.specificity_open))
        ),
      ],
      [expandable_deco, expander],
    );
  };
};

let example_view =
    (
      ~globals: Globals.t,
      ~inject,
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
          (_, {term, message, sub_id, _}: ExplainThisForm.example) => {
            let feedback =
              globals.settings.explainThis.show_feedback
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
                CellEditor.View.view(
                  ~globals,
                  ~signal=_ => Ui_effect.Ignore,
                  ~inject=_ => Ui_effect.Ignore,
                  ~selected=None,
                  ~caption=None,
                  ~locked=true,
                  {
                    term
                    |> Zipper.unzip
                    |> Editor.Model.mk(~root=Exp)
                    |> CellEditor.Model.mk
                    |> CellEditor.Update.calculate(
                         ~settings=globals.settings.core,
                         ~is_edited=true,
                         ~stitch=x => x,
                         ~queue_worker=None,
                       );
                  },
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

let rec bypass_parens_and_annot_pat = (pat: Pat.t) => {
  switch (pat.term) {
  | Parens(p)
  | Asc(p, _) => bypass_parens_and_annot_pat(p)
  | _ => pat
  };
};

let rec bypass_parens_pat = (pat: Pat.t) => {
  switch (pat.term) {
  | Parens(p) => bypass_parens_pat(p)
  | _ => pat
  };
};

let rec bypass_parens_exp = (exp: Exp.t) => {
  switch (exp.term) {
  | Parens(e) => bypass_parens_exp(e)
  | _ => exp
  };
};

let rec bypass_parens_typ = (typ: Typ.t) => {
  switch (typ.term) {
  | Parens(t) => bypass_parens_typ(t)
  | _ => typ
  };
};

type message_mode =
  | MessageContent(
      ExplainThisUpdate.update => Virtual_dom.Vdom.Effect.t(unit),
      Globals.t,
    )
  | Colorings;

type info_deduction = option(DrvGrading.VerifiedTree.info);

let get_doc_deduction =
    (
      ~globals: Globals.t,
      ~docs: ExplainThisModel.t,
      info_deduction: info_deduction,
      mode: message_mode,
    )
    : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
  let get_message =
      (~explanation: option(string)=?, group: ExplainThisForm.group)
      // Examples can be leaved blank.
      : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
    let (doc, _) = ExplainThisModel.get_form_and_options(group, docs);

    let explanation_msg =
      switch (explanation) {
      | Some(msg) => msg
      | None => doc.explanation
      };
    switch (mode) {
    | MessageContent(inject, globals) =>
      let (explanation_title, (explanation, color_map)) =
        if (globals.settings.core.dynamics) {
          (
            DrvExplainThis.mk_explanation_title(),
            mk_explanation(
              ~globals,
              ~inject,
              group.id,
              doc.id,
              explanation_msg,
              docs,
            ),
          );
        } else {
          (none, (none, ColorSteps.empty));
        };
      let rule_example_view =
        DrvExplainThis.rule_example_view(
          ~info=info_deduction,
          ~color_map,
          ~globals,
        );
      (
        [rule_example_view],
        ([explanation_title, explanation], color_map),
        [],
      );
    | Colorings =>
      let (_, color_map) =
        mk_translation(~globals, ~inject=_ => (), explanation_msg);
      ([], ([], color_map), []);
    };
  };

  let fake_get_message = msg =>
    get_message(~explanation=msg, DrvExplainThis.premise_mismatch);

  switch (info_deduction) {
  | None => fake_get_message("Deduction Not Available")
  | Some({res: Correct, _}) => fake_get_message("✅ Correct")
  | Some({res: Pending(p), _}) =>
    fake_get_message(DrvGrading.ExternalError.show(p))
  | Some({res: PartialCorrect(specced), _}) =>
    fake_get_message(
      if (globals.settings.explainThis.highlight == All) {
        Printf.sprintf(
          "❓ Correct until stop at a hole %s)",
          RuleVerify.show_linked(specced),
        );
      } else {
        "❓ Correct until stop at a hole";
      },
    )
  | Some({res: Incorrect(failure), _}) =>
    fake_get_message(
      (
        switch (failure) {
        | Mismatch(expected, actual) =>
          Printf.sprintf(
            "Expected %d premises, but found %d",
            expected,
            actual,
          )
        | FailMatch((spec, _) as specced) =>
          Printf.sprintf(
            "Could not match %s against expected form %s",
            RuleVerify.show_linked(specced),
            spec |> Drv.Any.cls_of |> Drv.Any.show_cls,
          )
        | NotEqual(specced1, specced2) =>
          Printf.sprintf(
            "Matched terms %s and %s that should be equal were different",
            RuleVerify.show_linked(specced1),
            RuleVerify.show_linked(specced2),
          )
        | FailUnbox(specced, cls) =>
          Printf.sprintf(
            "Could not extract a %s from %s",
            cls |> Drv.Any.show_cls,
            RuleVerify.show_linked(specced),
          )
        | FailTest(map, test) =>
          Printf.sprintf(
            "Matched terms failed the test (hidden premise): %s",
            test
            |> ExpToSegment.drv_formula_to_pretty(_, DrvSort.Jdmt)
            |> List.map(
                 Base.map_piece(~f_piece=(cont, piece) => {
                   switch (piece) {
                   | Tile(
                       {
                         children: [],
                         mold:
                           {
                             nibs: ({shape: Convex, _}, {shape: Convex, _}),
                             _,
                           },
                         _,
                       } as t,
                     ) =>
                     let label = t.label |> List.hd;
                     let (_, syntax) = RuleVerify.Map.find(label, map);
                     Tile({
                       ...t,
                       label: [
                         Printf.sprintf(
                           "[*%s*](%s)",
                           label,
                           syntax |> Drv.Any.rep_id |> Id.to_string,
                         ),
                       ],
                     });
                   | _ => cont(piece)
                   }
                 }),
               )
            |> Segment.to_string(
                 ~projector_to_segment=Triggers.projector_to_invoke,
                 ~refractor_seg_to_seg=Triggers.refractor_seg_to_seg,
               ),
          )
        }
      )
      |> Printf.sprintf("❌ %s"),
    )
  };
};

let get_color_map_deduction =
    (
      ~globals: Globals.t,
      ~explainThisModel: ExplainThisModel.t,
      info_deduction: info_deduction,
    ) =>
  switch (globals.settings.explainThis.highlight) {
  | All when globals.settings.explainThis.show =>
    let (_, (_, (color_map, _)), _) =
      get_doc_deduction(
        ~globals,
        ~docs=explainThisModel,
        info_deduction,
        Colorings,
      );
    Some(color_map);
  | One(id) when globals.settings.explainThis.show =>
    let (_, (_, (color_map, _)), _) =
      get_doc_deduction(
        ~globals,
        ~docs=explainThisModel,
        info_deduction,
        Colorings,
      );
    Some(Id.Map.filter((id', _) => id == id', color_map));
  | _ => None
  };

let get_doc =
    (
      ~globals: Globals.t,
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
        ~explanation: option(string)=?,
        group: ExplainThisForm.group,
      )
      : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
    let (doc, options) = ExplainThisModel.get_form_and_options(group, docs);

    /* Each form's explanation is already filled in by its data module, where
       the format literal and its arguments sit together. `~explanation`
       overrides it for the few callers that describe a different term. */
    let explanation_msg =
      switch (explanation) {
      | Some(msg) => msg
      | None => doc.explanation
      };
    switch (mode) {
    | MessageContent(inject, globals) =>
      let (explanation, color_map) =
        mk_explanation(
          ~globals,
          ~inject,
          group.id,
          doc.id,
          explanation_msg,
          docs,
        );
      let root =
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
      let editor = Editor.Model.mk(doc.syntactic_form |> Zipper.unzip, ~root);
      let expander_deco =
        expander_deco(
          ~globals,
          ~docs,
          ~inject,
          ~options,
          ~group,
          ~doc,
          editor,
        );
      let highlight_deco = [
        Highlight.colors(
          ~font_metrics=globals.font_metrics,
          ~syntax=editor.syntax,
          highlights,
        ),
      ];
      let syntactic_form_view =
        CodeWithStatics.View.view(
          ~globals,
          ~overlays=highlight_deco @ [expander_deco],
          {
            editor,
            statics: CachedStatics.empty,
            dynamics: Dynamics.Map.empty,
            context_menu: None,
          },
        );
      let example_view =
        example_view(
          ~globals,
          ~inject,
          ~group_id=group.id,
          ~form_id=doc.id,
          ~examples=doc.examples,
          ~model=docs,
        );
      ([syntactic_form_view], ([explanation], color_map), example_view);
    | Colorings =>
      let (_, color_map) =
        mk_translation(~globals, ~inject=_ => (), explanation_msg);
      ([], ([], color_map), []);
    };
  };

  /* Use this when adding new entries */
  let message_single = (e: ExplainThisForm.Simple.t) => {
    let (explanation, colorings, group) = ExplainThisForm.Simple.to_group(e);
    get_message(~colorings, ~explanation, group);
  };

  switch (info) {
  | Some(InfoMod({cls, _})) =>
    switch (cls) {
    | Mod(ModLet) => message_single(ModLetDecl.single)
    | Mod(ModType) => message_single(ModTypeDecl.single)
    | Mod(ModuleMod) => message_single(ModuleKeywordDecl.single)
    | _ => simple("Module item")
    }
  | Some(InfoSig({cls, _})) =>
    switch (cls) {
    | Sig(SigLet) => message_single(SigLetDecl.single)
    | Sig(SigType) => message_single(SigTypeDecl.single)
    | _ => simple("Signature item")
    }
  | Some(InfoMPat(_)) => simple("Module name")
  | Some(InfoExp({cls: Mod(ModLet), _})) =>
    message_single(ModLetDecl.single)
  | Some(InfoExp({cls: Mod(ModType), _})) =>
    message_single(ModTypeDecl.single)
  | Some(InfoExp({cls: Mod(ModuleMod), _})) =>
    message_single(ModuleKeywordDecl.single)
  | Some(InfoExp({cls: Mod(_), _})) => simple("Module item")
  | Some(InfoExp({user_term: term, _})) =>
    let rec get_message_exp =
            (term)
            : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) =>
      switch ((term: Exp.term)) {
      | DrvQuote(_) => (
          [],
          mk_translation(
            ~globals,
            ~inject=_ => (),
            "A derivation-mode quotation embeds a derivation-mode term into a regular expression. There are 5 forms of quotation:\n1) `of_jdmt`\n2) `of_ctx`\n3) `of_prop`\n4) `of_alfa_exp`\n5) `of_alfa_typ`",
          ),
          [],
        )
      | Invalid(_) => simple("Not a valid expression")
      | DynamicErrorHole(_)
      | Closure(_) => simple("Internal expression")
      | Asc(e, t) =>
        let exp_id = List.nth(IdTagged.ids(e), 0);
        let typ_id = List.nth(IdTagged.ids(t), 0);
        get_message(
          ~colorings=AscExp.ascription_coloring_ids(~exp_id, ~typ_id),
          AscExp.ascriptions(~exp_id, ~typ_id),
        );
      | Use(t, e) =>
        message_single(
          UseExp.single(~typ_id=Typ.rep_id(t), ~body_id=Exp.rep_id(e)),
        )
      | BuiltinFun(_) => simple("Internal expression")
      | LivelitName(n) => get_message(TerminalExp.livelit_name_exps(n))
      | EmptyHole => get_message(HoleExp.empty_hole_exps)
      | MultiHole(_children) => get_message(HoleExp.multi_hole_exps)
      | TyAlias(ty_pat, ty_def, _body) =>
        let tpat_id = List.nth(IdTagged.ids(ty_pat), 0);
        let def_id = List.nth(IdTagged.ids(ty_def), 0);
        get_message(
          ~colorings=
            TyAliasExp.tyalias_base_exp_coloring_ids(~tpat_id, ~def_id),
          TyAliasExp.tyalias_exps(~tpat_id, ~def_id),
        );
      | Undefined => get_message(UndefinedExp.undefined_exps)
      | Deferral(_) => get_message(TerminalExp.deferral_exps)
      | ExplicitNonlabel => simple("Explicitly unlabeled entry")
      | Atom(Bool(b)) => get_message(TerminalExp.bool_exps(b))
      | Atom(Int(i)) => get_message(TerminalExp.int_exps(i))
      | Atom(SInt(i)) => get_message(TerminalExp.sint_exps(i))
      | Atom(Float(f)) => get_message(TerminalExp.float_exps(f))
      | Atom(String(s)) => get_message(TerminalExp.string_exps(s))
      | Atom(Nat(i)) => get_message(TerminalExp.nat_exps(i))
      | ListLit(terms) =>
        get_message(ListExp.listlits(~n=List.length(terms)))
      | TypFun(tpat, body, _) =>
        let tpat_id = List.nth(IdTagged.ids(tpat), 0);
        let body_id = List.nth(IdTagged.ids(body), 0);
        /* TODO: More could be done here probably for different patterns. */
        get_message(
          ~colorings=
            FunctionExp.function_exp_coloring_ids(~pat_id=tpat_id, ~body_id),
          TypFunctionExp.type_functions_basic(~tpat_id, ~body_id),
        );
      | Fun(pat, body, _, _) =>
        /* The generic fallback form describes the *unbypassed* pattern, while
           the specific forms below describe the pattern with parens and
           annotations stripped. These ids differ for e.g. `fun (x) -> x`. */
        let unbypassed_pat_id = List.nth(IdTagged.ids(pat), 0);
        let basic = group => {
          let body_id = List.nth(IdTagged.ids(body), 0);
          get_message(
            ~colorings=
              FunctionExp.function_exp_coloring_ids(
                ~pat_id=unbypassed_pat_id,
                ~body_id,
              ),
            ~explanation=
              FunctionExp.function_exp_explanation(
                ~pat_id=unbypassed_pat_id,
                ~body_id,
              ),
            group,
          );
        };
        let pat = bypass_parens_and_annot_pat(pat);
        let pat_id = List.nth(IdTagged.ids(pat), 0);
        let body_id = List.nth(IdTagged.ids(body), 0);
        switch (pat.term) {
        | EmptyHole =>
          let group = FunctionExp.functions_empty_hole(~pat_id, ~body_id);
          if (FunctionExp.function_empty_hole_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_empty_hole_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | MultiHole(_) =>
          let group = FunctionExp.functions_multi_hole(~pat_id, ~body_id);
          if (FunctionExp.function_multi_hole_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_multi_hole_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Wild =>
          let group = FunctionExp.functions_wild(~pat_id, ~body_id);
          if (FunctionExp.function_wild_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=FunctionExp.function_wild_exp_coloring_ids(~body_id),
              group,
            );
          } else {
            basic(group);
          };
        | Atom(SInt(i)) =>
          let group = FunctionExp.functions_sint(~pat_id, ~body_id, ~i);
          if (FunctionExp.function_sintlit_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_sintlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Atom(Int(i) | Nat(i)) =>
          let group = FunctionExp.functions_int(~pat_id, ~body_id, ~i);
          if (FunctionExp.function_intlit_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_intlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Atom(Float(f)) =>
          let group = FunctionExp.functions_float(~pat_id, ~body_id, ~f);
          if (FunctionExp.function_floatlit_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_floatlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Atom(Bool(b)) =>
          let group = FunctionExp.functions_bool(~pat_id, ~body_id, ~b);
          if (FunctionExp.function_boollit_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_boollit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Atom(String(s)) =>
          let group = FunctionExp.functions_str(~pat_id, ~body_id, ~s);
          if (FunctionExp.function_strlit_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_strlit_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Tuple([]) =>
          let group = FunctionExp.functions_triv(~pat_id, ~body_id);
          if (FunctionExp.function_triv_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_triv_exp_coloring_ids(~pat_id, ~body_id),
              group,
            );
          } else {
            basic(group);
          };
        | ListLit(elements) =>
          if (List.length(elements) == 0) {
            let group = FunctionExp.functions_listnil(~pat_id, ~body_id);
            if (FunctionExp.function_listnil_exp_id
                == get_specificity_level(group)) {
              get_message(
                ~colorings=
                  FunctionExp.function_listnil_exp_coloring_ids(
                    ~pat_id,
                    ~body_id,
                  ),
                group,
              );
            } else {
              basic(group);
            };
          } else {
            let group =
              FunctionExp.functions_listlit(
                ~pat_id,
                ~body_id,
                ~n=List.length(elements),
              );
            if (FunctionExp.function_listlit_exp_id
                == get_specificity_level(group)) {
              get_message(
                ~colorings=
                  FunctionExp.function_listlit_exp_coloring_ids(
                    ~pat_id,
                    ~body_id,
                  ),
                group,
              );
            } else {
              basic(group);
            };
          }
        | Cons(hd, tl) =>
          let hd_id = List.nth(IdTagged.ids(hd), 0);
          let tl_id = List.nth(IdTagged.ids(tl), 0);
          let group =
            FunctionExp.functions_cons(~hd_id, ~tl_id, ~pat_id, ~body_id);
          if (FunctionExp.function_cons_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_cons_exp_coloring_ids(
                  ~hd_id,
                  ~tl_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Var(var) =>
          let group = FunctionExp.functions_var(~pat_id, ~body_id, ~name=var);
          if (FunctionExp.function_var_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_var_exp_coloring_ids(~pat_id, ~body_id),
              group,
            );
          } else {
            basic(group);
          };
        | Tuple([{term: TupLabel(l, p), _}]) =>
          let group =
            FunctionExp.functions_tuplabel(
              ~label_id=Pat.rep_id(l),
              ~label_pat_id=Pat.rep_id(p),
              ~pat_id,
              ~body_id,
            );
          if (FunctionExp.function_labeled_exp_id
              == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_labeled_exp_coloring_ids(
                  ~label_id=Pat.rep_id(l),
                  ~pat_id=Pat.rep_id(p),
                  ~body_id=Exp.rep_id(body),
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Tuple(elements) =>
          let n = List.length(elements);
          let basic_tuple = group =>
            get_message(
              ~colorings=
                FunctionExp.function_tuple_exp_coloring_ids(
                  ~pat_id,
                  ~body_id,
                ),
              group,
            );

          switch (n) {
          | 2 =>
            let pat1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
            let pat2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
            let group =
              FunctionExp.functions_tuple2(
                ~pat1_id,
                ~pat2_id,
                ~pat_id,
                ~body_id,
                ~n,
              );
            let doc_id = get_specificity_level(group);
            if (FunctionExp.function_tuple2_exp_id == doc_id) {
              get_message(
                ~colorings=
                  FunctionExp.function_tuple2_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~body_id,
                  ),
                group,
              );
            } else if (FunctionExp.function_tuple_exp_id == doc_id) {
              basic_tuple(group);
            } else {
              basic(group);
            };
          | 3 =>
            let pat1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
            let pat2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
            let pat3_id = List.nth(IdTagged.ids(List.nth(elements, 2)), 0);
            let group =
              FunctionExp.functions_tuple3(
                ~pat1_id,
                ~pat2_id,
                ~pat3_id,
                ~pat_id,
                ~body_id,
                ~n,
              );
            let doc_id = get_specificity_level(group);
            if (FunctionExp.function_tuple3_exp_id == doc_id) {
              get_message(
                ~colorings=
                  FunctionExp.function_tuple3_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~pat3_id,
                    ~body_id,
                  ),
                group,
              );
            } else if (FunctionExp.function_tuple_exp_id == doc_id) {
              basic_tuple(group);
            } else {
              basic(group);
            };
          | _ =>
            let group = FunctionExp.functions_tuple(~pat_id, ~body_id, ~n);
            if (FunctionExp.function_tuple_exp_id
                == get_specificity_level(group)) {
              basic_tuple(group);
            } else {
              basic(group);
            };
          };
        | Ap(con, arg) =>
          let con_id = List.nth(IdTagged.ids(con), 0);
          let arg_id = List.nth(IdTagged.ids(arg), 0);
          let group =
            FunctionExp.functions_ap(~con_id, ~arg_id, ~pat_id, ~body_id);
          if (FunctionExp.function_ap_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_ap_exp_coloring_ids(
                  ~con_id,
                  ~arg_id,
                  ~body_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | Constructor(v, _) =>
          let group = FunctionExp.functions_ctr(~pat_id, ~body_id, ~name=v);
          if (FunctionExp.function_ctr_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                FunctionExp.function_ctr_exp_coloring_ids(~pat_id, ~body_id),
              group,
            );
          } else {
            basic(group);
          };
        | TupLabel(_)
        | Invalid(_)
        | Parens(_)
        | Label(_)
        | ExplicitNonlabel
        | Projector(_)
        | Asc(_) => default // Shouldn't get hit?
        };
      | Label(name) => get_message(LabelTerm.labels(name))
      | TupLabel(l, e) =>
        get_message(
          ~colorings=
            TupLabelExp.labeled_exps_coloring_ids(
              ~label_id=Exp.rep_id(l),
              ~exp_id=Exp.rep_id(e),
            ),
          TupLabelExp.labeled_exps(
            ~label_id=Exp.rep_id(l),
            ~exp_id=Exp.rep_id(e),
          ),
        )
      | Dot(tup, lab) =>
        get_message(
          ~colorings=
            DotExp.dot_coloring_ids(
              ~tup_id=Exp.rep_id(tup),
              ~lab_id=Exp.rep_id(lab),
            ),
          DotExp.dot_exp(~lab_id=Exp.rep_id(lab), ~tup_id=Exp.rep_id(tup)),
        )
      | Tuple(terms) =>
        let n = List.length(terms);
        let basic = group =>
          get_message(
            ~explanation=TupleExp.tuple_exp_explanation(~n),
            group,
          );
        switch (n) {
        | 2 =>
          let exp1_id = List.nth(IdTagged.ids(List.nth(terms, 0)), 0);
          let exp2_id = List.nth(IdTagged.ids(List.nth(terms, 1)), 0);
          let group = TupleExp.tuples2(~exp1_id, ~exp2_id, ~n);
          if (TupleExp.tuple_exp_size2_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                TupleExp.tuple_exp_size2_coloring_ids(~exp1_id, ~exp2_id),
              group,
            );
          } else {
            basic(group);
          };
        | 3 =>
          let exp1_id = List.nth(IdTagged.ids(List.nth(terms, 0)), 0);
          let exp2_id = List.nth(IdTagged.ids(List.nth(terms, 1)), 0);
          let exp3_id = List.nth(IdTagged.ids(List.nth(terms, 2)), 0);
          let group = TupleExp.tuples3(~exp1_id, ~exp2_id, ~exp3_id, ~n);
          if (TupleExp.tuple_exp_size3_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                TupleExp.tuple_exp_size3_coloring_ids(
                  ~exp1_id,
                  ~exp2_id,
                  ~exp3_id,
                ),
              group,
            );
          } else {
            basic(group);
          };
        | _ => basic(TupleExp.tuples(~n))
        };
      | Var(n) => get_message(TerminalExp.var_exps(n))
      | Let(pat, def, body) =>
        let pat = bypass_parens_and_annot_pat(pat);
        let pat_id = List.nth(IdTagged.ids(pat), 0);
        let def_id = List.nth(IdTagged.ids(def), 0);
        let body_id = List.nth(IdTagged.ids(body), 0);
        let basic = group =>
          get_message(
            ~colorings=LetExp.let_base_exp_coloring_ids(~pat_id, ~def_id),
            ~explanation=LetExp.let_base_exp_explanation(~def_id, ~pat_id),
            group,
          );
        switch (pat.term) {
        | EmptyHole =>
          let group = LetExp.lets_emptyhole(~def_id, ~pat_id);
          if (LetExp.let_empty_hole_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_empty_hole_exp_coloring_ids(~pat_id, ~def_id),
              group,
            );
          } else {
            basic(group);
          };
        | MultiHole(_) =>
          let group = LetExp.lets_mutlihole(~def_id, ~pat_id);
          if (LetExp.let_multi_hole_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_multi_hole_exp_coloring_ids(~pat_id, ~def_id),
              group,
            );
          } else {
            basic(group);
          };
        | Wild =>
          let group = LetExp.lets_wild(~def_id, ~pat_id, ~body_id);
          if (LetExp.let_wild_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=LetExp.let_wild_exp_coloring_ids(~def_id, ~body_id),
              group,
            );
          } else {
            basic(group);
          };
        | Atom(Int(i) | Nat(i)) =>
          let group = LetExp.lets_int(~def_id, ~pat_id, ~i, ~body_id);
          if (LetExp.let_int_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_int_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and some other places when switching between forms and specificity levels... maybe a Safari issue... */
            basic(
              group,
            );
          };
        | Atom(SInt(i)) =>
          let group = LetExp.lets_sint(~def_id, ~pat_id, ~i, ~body_id);
          if (LetExp.let_sint_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_sint_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              group,
            );
          };
        | Atom(Float(f)) =>
          let group = LetExp.lets_float(~def_id, ~pat_id, ~f, ~body_id);
          // TODO Make sure everywhere printing the float literal print it prettier
          if (LetExp.let_float_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_float_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              group,
            );
          };
        | Atom(Bool(b)) =>
          let group = LetExp.lets_bool(~def_id, ~pat_id, ~b, ~body_id);
          if (LetExp.let_bool_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_bool_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              group,
            );
          };
        | Atom(String(s)) =>
          let group = LetExp.lets_str(~def_id, ~pat_id, ~s, ~body_id);
          if (LetExp.let_str_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_str_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here... */
            basic(
              group,
            );
          };
        | Tuple([]) =>
          let group = LetExp.lets_triv(~def_id, ~pat_id, ~body_id);
          if (LetExp.let_triv_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_triv_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            /* TODO The coloring for the syntactic form is sometimes wrong here and other places when switching syntactic specificities... seems like might be Safari issue... */
            basic(
              group,
            );
          };
        | ListLit(elements) =>
          if (List.length(elements) == 0) {
            let group = LetExp.lets_listnil(~def_id, ~pat_id, ~body_id);
            if (LetExp.let_listnil_exp_id == get_specificity_level(group)) {
              get_message(
                ~colorings=
                  LetExp.let_listnil_exp_coloring_ids(
                    ~pat_id,
                    ~def_id,
                    ~body_id,
                  ),
                group,
              );
            } else {
              basic(group);
            };
          } else {
            let group =
              LetExp.lets_listlit(
                ~def_id,
                ~pat_id,
                ~n=List.length(elements),
              );
            if (LetExp.let_listlit_exp_id == get_specificity_level(group)) {
              get_message(
                ~colorings=
                  LetExp.let_listlit_exp_coloring_ids(~pat_id, ~def_id),
                group,
              );
            } else {
              basic(group);
            };
          }
        | Cons(hd, tl) =>
          let hd_id = List.nth(IdTagged.ids(hd), 0);
          let tl_id = List.nth(IdTagged.ids(tl), 0);
          let group = LetExp.lets_cons(~def_id, ~hd_id, ~tl_id, ~pat_id);
          if (LetExp.let_cons_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_cons_exp_coloring_ids(~hd_id, ~tl_id, ~def_id),
              group,
            );
          } else {
            basic(group);
          };
        | Var(var) =>
          let group = LetExp.lets_var(~def_id, ~pat_id, ~name=var, ~body_id);
          if (LetExp.let_var_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_var_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            basic(group);
          };
        | Tuple(elements) =>
          let n = List.length(elements);
          let basic_tuple = group =>
            get_message(
              ~colorings=LetExp.let_tuple_exp_coloring_ids(~pat_id, ~def_id),
              ~explanation=
                LetExp.let_tuple_exp_explanation(~def_id, ~pat_id, ~n),
              group,
            );

          switch (n) {
          | 2 =>
            let pat1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
            let pat2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
            let group =
              LetExp.lets_tuple2(~def_id, ~pat1_id, ~pat2_id, ~pat_id, ~n);
            let doc_id = get_specificity_level(group);
            if (LetExp.let_tuple2_exp_id == doc_id) {
              get_message(
                ~colorings=
                  LetExp.let_tuple2_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~def_id,
                  ),
                group,
              );
            } else if (LetExp.let_tuple_exp_id == doc_id) {
              basic_tuple(group);
            } else {
              basic(group);
            };
          | 3 =>
            let pat1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
            let pat2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
            let pat3_id = List.nth(IdTagged.ids(List.nth(elements, 2)), 0);
            let group =
              LetExp.lets_tuple3(
                ~def_id,
                ~pat1_id,
                ~pat2_id,
                ~pat3_id,
                ~pat_id,
                ~n,
              );
            let doc_id = get_specificity_level(group);
            // TODO Syntactic form can go off page - so can examples - but can scroll, just can't see bottom scroll bar
            if (LetExp.let_tuple3_exp_id == doc_id) {
              get_message(
                ~colorings=
                  LetExp.let_tuple3_exp_coloring_ids(
                    ~pat1_id,
                    ~pat2_id,
                    ~pat3_id,
                    ~def_id,
                  ),
                group,
              );
            } else if (LetExp.let_tuple_exp_id == doc_id) {
              basic_tuple(group);
            } else {
              basic(group);
            };
          | _ =>
            let group = LetExp.lets_tuple(~def_id, ~pat_id, ~n);
            if (LetExp.let_tuple_exp_id == get_specificity_level(group)) {
              basic_tuple(group);
            } else {
              basic(group);
            };
          };
        | Ap(x, arg) =>
          let x_id = List.nth(IdTagged.ids(x), 0);
          let arg_id = List.nth(IdTagged.ids(arg), 0);
          let (lets_ap, let_ap_exp_coloring_ids, let_ap_exp_id) =
            switch (x.term) {
            | Constructor(_, _) => (
                LetExp.lets_conap(~def_id, ~x_id, ~arg_id, ~pat_id),
                LetExp.let_conap_exp_coloring_ids,
                LetExp.let_conap_exp_id,
              )
            | _ => (
                LetExp.lets_funap(~def_id, ~x_id, ~arg_id, ~pat_id),
                LetExp.let_funap_exp_coloring_ids,
                LetExp.let_funap_exp_id,
              )
            };
          if (let_ap_exp_id == get_specificity_level(lets_ap)) {
            get_message(
              ~colorings=let_ap_exp_coloring_ids(~x_id, ~arg_id, ~def_id),
              lets_ap,
            );
          } else {
            basic(lets_ap);
          };
        | Constructor(v, _) =>
          let group = LetExp.lets_ctr(~def_id, ~pat_id, ~name=v, ~body_id);
          if (LetExp.let_ctr_exp_id == get_specificity_level(group)) {
            get_message(
              ~colorings=
                LetExp.let_ctr_exp_coloring_ids(~pat_id, ~def_id, ~body_id),
              group,
            );
          } else {
            basic(group);
          };
        | TupLabel(_)
        | ExplicitNonlabel
        | Label(_)
        | Invalid(_) => default // Shouldn't get hit
        | Parens(_)
        | Projector(_)
        | Asc(_) => default // Shouldn't get hit?
        };
      | Theorem(pat, thm, body) =>
        let pat_id = List.nth(IdTagged.ids(pat), 0);
        let thm_id = List.nth(IdTagged.ids(thm), 0);
        let body_id = List.nth(IdTagged.ids(body), 0);
        get_message(
          ~colorings=
            TheoremExp.test_exp_coloring_ids(~body_id, ~pat_id, ~thm_id),
          TheoremExp.tests(~pat_id, ~thm_id),
        );
      | ProofObject(exp) =>
        let typ_id = List.nth(IdTagged.ids(exp), 0);
        get_message(
          ~colorings=ProofObjectExp.proof_of_exp_coloring_ids(~typ_id),
          ProofObjectExp.proof_of_exps(~typ_id),
        );
      | Forall(pat, typ) =>
        let pat_id = List.nth(IdTagged.ids(pat), 0);
        let body_id = List.nth(IdTagged.ids(typ), 0);
        get_message(
          ~colorings=ForallExp.forall_exp_coloring_ids(~pat_id, ~body_id),
          ForallExp.forall(~pat_id, ~body_id),
        );
      | FixF(pat, body, _) =>
        message_single(
          FixFExp.single(
            ~pat_id=Pat.rep_id(pat),
            ~body_id=Exp.rep_id(body),
          ),
        )
      | Ap(Reverse, arg, fn) =>
        message_single(
          PipelineExp.single(
            ~arg_id=Exp.rep_id(arg),
            ~fn_id=Exp.rep_id(fn),
          ),
        )
      | TypAp(f, typ) =>
        let f_id = List.nth(IdTagged.ids(f), 0);
        let typ_id = List.nth(IdTagged.ids(typ), 0);
        get_message(
          ~colorings=TypAppExp.typfunapp_exp_coloring_ids(~f_id, ~typ_id),
          TypAppExp.typfunaps(~f_id, ~typ_id),
        );

      | Ap(Forward, x, arg) =>
        let x_id = List.nth(IdTagged.ids(x), 0);
        let arg_id = List.nth(IdTagged.ids(arg), 0);
        let basic = (group, coloring_ids) =>
          get_message(~colorings=coloring_ids(~x_id, ~arg_id), group);
        switch (x.term) {
        | Constructor(v, _) =>
          basic(
            AppExp.conaps(~name=v, ~x_id, ~arg_id),
            AppExp.conapp_exp_coloring_ids,
          )
        | LivelitName(_) =>
          basic(
            AppExp.livelitaps(~x_id, ~arg_id),
            AppExp.livelitapp_exp_coloring_ids,
          )
        | _ =>
          basic(
            AppExp.funaps(~x_id, ~arg_id),
            AppExp.funapp_exp_coloring_ids,
          )
        };
      | DeferredAp(x, args) =>
        let x_id = List.nth(IdTagged.ids(x), 0);
        let supplied_id = Id.mk();
        let deferred_id = {
          let deferral = List.find(Exp.is_deferral, args);
          List.nth(IdTagged.ids(deferral), 0);
        };
        switch (mode) {
        | MessageContent(_) =>
          get_message(
            ~colorings=
              AppExp.deferred_funapp_exp_coloring_ids(~x_id, ~deferred_id),
            AppExp.deferredaps(~x_id, ~supplied_id, ~deferred_id),
          )
        | Colorings =>
          let color_fn = List.nth(ColorSteps.child_colors, 0);
          let color_supplied = List.nth(ColorSteps.child_colors, 1);
          let color_deferred = List.nth(ColorSteps.child_colors, 2);
          let add = (mapping, arg: Exp.t) => {
            let arg_id = List.nth(IdTagged.ids(arg), 0);
            Haz3lcore.Id.Map.add(
              arg_id,
              Exp.is_deferral(arg) ? color_deferred : color_supplied,
              mapping,
            );
          };
          let mapping = Haz3lcore.Id.Map.singleton(x_id, color_fn);
          let mapping = List.fold_left(add, mapping, args);
          let color_map = (mapping, List.length(args) + 1);
          ([], ([], color_map), []);
        };
      | If(cond, then_, else_) =>
        let cond_id = List.nth(IdTagged.ids(cond), 0);
        let then_id = List.nth(IdTagged.ids(then_), 0);
        let else_id = List.nth(IdTagged.ids(else_), 0);
        get_message(
          ~colorings=IfExp.if_exp_coloring_ids(~cond_id, ~then_id, ~else_id),
          IfExp.ifs(~cond_id, ~then_id, ~else_id),
        );
      | Seq(left, right) =>
        let exp1_id = List.nth(IdTagged.ids(left), 0);
        let exp2_id = List.nth(IdTagged.ids(right), 0);
        get_message(
          ~colorings=SeqExp.seq_exp_coloring_ids(~exp1_id, ~exp2_id),
          SeqExp.seqs(~exp1_id, ~exp2_id),
        );
      | Filter(Filter({act: (Step, One), pat}), body) =>
        message_single(
          FilterExp.filter_pause(
            ~p_id=Exp.rep_id(pat),
            ~body_id=Exp.rep_id(body),
          ),
        )
      | Filter(Filter({act: (Step, All), pat}), body) =>
        message_single(
          FilterExp.filter_debug(
            ~p_id=Exp.rep_id(pat),
            ~body_id=Exp.rep_id(body),
          ),
        )
      | Filter(Filter({act: (Eval, All), pat}), body) =>
        message_single(
          FilterExp.filter_eval(
            ~p_id=Exp.rep_id(pat),
            ~body_id=Exp.rep_id(body),
          ),
        )
      | Filter(Filter({act: (Eval, One), pat}), body) =>
        message_single(
          FilterExp.filter_hide(
            ~p_id=Exp.rep_id(pat),
            ~body_id=Exp.rep_id(body),
          ),
        )
      | Filter(_) => simple("Internal expression")
      | Test(body) =>
        let body_id = List.nth(IdTagged.ids(body), 0);
        get_message(
          ~colorings=TestExp.test_exp_coloring_ids(~body_id),
          TestExp.tests(~body_id),
        );
      | Parens(term) => get_message_exp(term.term) // No Special message?
      | HintedTest(body, hint) =>
        let hint_id = List.nth(IdTagged.ids(hint), 0);
        let body_id = List.nth(IdTagged.ids(body), 0);
        get_message(
          ~colorings=
            HintedTestExp.hinted_test_exp_coloring_ids(~body_id, ~hint_id),
          HintedTestExp.tests(~hint_id, ~body_id),
        );
      | Cons(hd, tl) =>
        let hd_id = List.nth(IdTagged.ids(hd), 0);
        let tl_id = List.nth(IdTagged.ids(tl), 0);
        get_message(
          ~colorings=ListExp.cons_exp_coloring_ids(~hd_id, ~tl_id),
          ListExp.listcons(~hd_id, ~tl_id),
        );
      | TupleExtension(x, y) =>
        let x_id = List.nth(IdTagged.ids(x), 0);
        let y_id = List.nth(IdTagged.ids(y), 0);
        get_message(
          ~colorings=TupleExp.tuple_extension_exp_coloring_ids(~x_id, ~y_id),
          TupleExp.tuple_extensions(~x_id, ~y_id),
        );
      | ListConcat(xs, ys) =>
        let xs_id = List.nth(IdTagged.ids(xs), 0);
        let ys_id = List.nth(IdTagged.ids(ys), 0);
        get_message(
          ~colorings=ListExp.concat_exp_coloring_ids(~xs_id, ~ys_id),
          ListExp.listconcats(~xs_id, ~ys_id),
        );
      | UnOp(op, exp) =>
        switch (op) {
        | Bool(Not) =>
          let exp_id = List.nth(IdTagged.ids(exp), 0);
          get_message(
            ~colorings=OpExp.bool_unary_not_exp_coloring_ids(~exp_id),
            OpExp.bool_un_not(~exp_id),
          );
        | Float(Minus) // TODO[Matt]: finish
        | SInt(Minus)
        | Nat(Minus)
        | Int(Minus) =>
          let exp_id = List.nth(IdTagged.ids(exp), 0);
          get_message(
            ~colorings=OpExp.int_unary_minus_exp_coloring_ids(~exp_id),
            OpExp.int_un_minus(~exp_id),
          );
        }
      | BinOp(op, left, right) =>
        open OpExp;
        let (group, coloring_ids) =
          switch (op) {
          | Nat(Plus)
          | SInt(Plus)
          | Int(Plus) => (int_plus, int_plus_exp_coloring_ids)
          | Nat(Minus)
          | SInt(Minus)
          | Int(Minus) => (int_minus, int_minus_exp_coloring_ids)
          | Nat(Times)
          | SInt(Times)
          | Int(Times) => (int_times, int_times_exp_coloring_ids)
          | Nat(Power)
          | SInt(Power)
          | Int(Power) => (int_power, int_power_exp_coloring_ids)
          | Nat(Divide)
          | SInt(Divide)
          | Int(Divide) => (int_divide, int_divide_exp_coloring_ids)
          | Nat(LessThan)
          | SInt(LessThan)
          | Int(LessThan) => (int_less_than, int_lt_exp_coloring_ids)
          | Nat(LessThanOrEqual)
          | SInt(LessThanOrEqual)
          | Int(LessThanOrEqual) => (
              int_less_than_equal,
              int_lte_exp_coloring_ids,
            )
          | Nat(GreaterThan)
          | SInt(GreaterThan)
          | Int(GreaterThan) => (int_greater_than, int_gt_exp_coloring_ids)
          | Nat(GreaterThanOrEqual)
          | SInt(GreaterThanOrEqual)
          | Int(GreaterThanOrEqual) => (
              int_greater_than_equal,
              int_gte_exp_coloring_ids,
            )
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
          | String(Concat) => (string_concat, str_concat_exp_coloring_ids)
          | Poly(Equals) => (poly_equal, poly_eq_exp_coloring_ids)
          | Poly(NotEquals) => (poly_not_equal, poly_neq_exp_coloring_ids)
          };
        let left_id = List.nth(IdTagged.ids(left), 0);
        let right_id = List.nth(IdTagged.ids(right), 0);
        get_message(
          ~colorings=coloring_ids(~left_id, ~right_id),
          group(~left_id, ~right_id),
        );
      | Match(scrut, _rules) =>
        let scrut_id = List.nth(IdTagged.ids(scrut), 0);
        get_message(
          ~colorings=CaseExp.case_exp_coloring_ids(~scrut_id),
          CaseExp.case(~scrut_id),
        );
      | Constructor(v, _) => get_message(TerminalExp.ctr(v))
      | Module(_) => message_single(ModuleExp.single)
      | ModuleExp(_) => message_single(ModuleKeywordExp.single)
      | Projector(_, e) => get_message_exp(e.term)
      };
    get_message_exp(term.term);
  | Some(InfoPat({user_term: term, _})) =>
    switch (bypass_parens_pat(term).term) {
    | EmptyHole => get_message(HolePat.empty_hole)
    | MultiHole(_) => get_message(HolePat.multi_hole)
    | Wild => get_message(TerminalPat.wild)
    | Atom(Int(i) | Nat(i)) => get_message(TerminalPat.intlit(i))
    | Atom(SInt(i)) => get_message(TerminalPat.sintlit(i))
    | Atom(Float(f)) => get_message(TerminalPat.floatlit(f))
    | Atom(Bool(b)) => get_message(TerminalPat.boollit(b))
    | Atom(String(s)) => get_message(TerminalPat.strlit(s))
    | Tuple([]) => get_message(TerminalPat.triv)
    | ListLit(elements) =>
      if (List.length(elements) == 0) {
        get_message(ListPat.listnil);
      } else {
        get_message(ListPat.listlit(~n=List.length(elements)));
      }
    | Cons(hd, tl) =>
      let hd_id = List.nth(IdTagged.ids(hd), 0);
      let tl_id = List.nth(IdTagged.ids(tl), 0);
      let basic = doc =>
        get_message(
          ~colorings=ListPat.cons_base_pat_coloring_ids(~hd_id, ~tl_id),
          ~explanation=ListPat.cons_base_pat_explanation(~hd_id, ~tl_id),
          doc,
        );
      switch (tl.term) {
      | Cons(hd2, tl2) =>
        let hd2_id = List.nth(IdTagged.ids(hd2), 0);
        let tl2_id = List.nth(IdTagged.ids(tl2), 0);
        let group =
          ListPat.cons2(~fst_id=hd_id, ~snd_id=hd2_id, ~tl_id=tl2_id, ~hd_id);
        if (ListPat.cons2_pat_id == get_specificity_level(group)) {
          get_message(
            ~colorings=
              ListPat.cons2_pat_coloring_ids(
                ~fst_id=hd_id,
                ~snd_id=hd2_id,
                ~tl_id=tl2_id,
              ),
            group,
          );
        } else {
          basic(group);
        };
      | _ => basic(ListPat.cons(~hd_id, ~tl_id))
      };
    | Var(v) => get_message(TerminalPat.var(v))
    | ExplicitNonlabel => simple("Explicitly unlabeled entry")
    | Label(name) => get_message(LabelTerm.labels(name))
    | TupLabel(l, p) =>
      get_message(
        ~colorings=
          TupLabelPat.labeled_exps_coloring_ids(
            ~label_id=Pat.rep_id(l),
            ~pat_id=Pat.rep_id(p),
          ),
        TupLabelPat.labeled_pats(
          ~label_id=Pat.rep_id(l),
          ~pat_id=Pat.rep_id(p),
        ),
      )
    | Tuple(elements) =>
      let n = List.length(elements);
      let basic = group =>
        get_message(~explanation=TuplePat.tuple_pat_explanation(~n), group);
      switch (n) {
      | 2 =>
        let elem1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
        let elem2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
        let group = TuplePat.tuple2(~elem1_id, ~elem2_id, ~n);
        if (TuplePat.tuple_pat_size2_id == get_specificity_level(group)) {
          get_message(
            ~colorings=
              TuplePat.tuple_pat_size2_coloring_ids(~elem1_id, ~elem2_id),
            group,
          );
        } else {
          basic(group);
        };
      | 3 =>
        let elem1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
        let elem2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
        let elem3_id = List.nth(IdTagged.ids(List.nth(elements, 2)), 0);
        let group = TuplePat.tuple3(~elem1_id, ~elem2_id, ~elem3_id, ~n);
        if (TuplePat.tuple_pat_size3_id == get_specificity_level(group)) {
          get_message(
            ~colorings=
              TuplePat.tuple_pat_size3_coloring_ids(
                ~elem1_id,
                ~elem2_id,
                ~elem3_id,
              ),
            group,
          );
        } else {
          basic(group);
        };
      | _ => basic(TuplePat.tuple(~n))
      };
    | Ap(x, arg) =>
      let x_id = List.nth(IdTagged.ids(x), 0);
      let arg_id = List.nth(IdTagged.ids(arg), 0);
      let basic = (group, coloring_ids) =>
        get_message(~colorings=coloring_ids(~x_id, ~arg_id), group);

      switch (x.term) {
      | Constructor(_, _) =>
        basic(AppPat.conaps(~x_id, ~arg_id), AppPat.conapp_pat_coloring_ids)
      | _ =>
        basic(AppPat.funaps(~x_id, ~arg_id), AppPat.funapp_pat_coloring_ids)
      };
    | Constructor(con, _) => get_message(TerminalPat.ctr(con))
    | Asc(pat, typ) =>
      let pat_id = List.nth(IdTagged.ids(pat), 0);
      let typ_id = List.nth(IdTagged.ids(typ), 0);
      get_message(
        ~colorings=TypAnnPat.typann_pat_coloring_ids(~pat_id, ~typ_id),
        TypAnnPat.typann(~pat_id, ~typ_id),
      );
    | Invalid(_) => simple("Not a valid pattern")
    | Parens(_)
    | Projector(_) =>
      // Shouldn't be hit?
      default
    }
  | Some(InfoTyp({user_term: term, _} as typ_info)) =>
    let typ = bypass_parens_typ(term);
    switch (typ.term) {
    | _ when Typ.is_void(typ) => get_message(TerminalTyp.void)
    | Unknown(SynSwitch)
    | Unknown(Internal)
    | Unknown(Hole(EmptyHole)) => get_message(HoleTyp.empty_hole)
    | Unknown(Hole(MultiHole(_))) => get_message(HoleTyp.multi_hole)
    | Atom(Int) => get_message(TerminalTyp.int)
    | Atom(SInt) => get_message(TerminalTyp.sint)
    | Atom(Float) => get_message(TerminalTyp.float)
    | Atom(Bool) => get_message(TerminalTyp.bool)
    | Atom(String) => get_message(TerminalTyp.str)
    | Atom(Nat) => get_message(TerminalTyp.nat)
    | List(elem) =>
      let elem_id = List.nth(IdTagged.ids(elem), 0);
      get_message(
        ~colorings=ListTyp.list_typ_coloring_ids(~elem_id),
        ListTyp.list(~elem_id),
      );
    | Poly(tpat, typ) =>
      let tpat_id = List.nth(IdTagged.ids(tpat), 0);
      let tbody_id = List.nth(IdTagged.ids(typ), 0);
      get_message(
        ~colorings=PolyTyp.poly_typ_coloring_ids(~tpat_id, ~tbody_id),
        PolyTyp.poly(~tpat_id, ~tbody_id),
      );
    | Rec(tpat, typ) =>
      let tpat_id = List.nth(IdTagged.ids(tpat), 0);
      let tbody_id = List.nth(IdTagged.ids(typ), 0);
      get_message(
        ~colorings=RecTyp.rec_typ_coloring_ids(~tpat_id, ~tbody_id),
        RecTyp.rec_(~tpat_id, ~tbody_id),
      );
    | ProofOf(exp) =>
      let body_id = List.nth(IdTagged.ids(exp), 0);
      get_message(
        ~colorings=ProofOfTyp.proof_of_typ_coloring_ids(~body_id),
        ProofOfTyp.proof_of(~body_id),
      );
    | Arrow(arg, result) =>
      let arg_id = List.nth(IdTagged.ids(arg), 0);
      let result_id = List.nth(IdTagged.ids(result), 0);
      let basic = doc =>
        get_message(
          ~colorings=ArrowTyp.arrow_typ_coloring_ids(~arg_id, ~result_id),
          ~explanation=ArrowTyp.arrow_typ_explanation(~arg_id, ~result_id),
          doc,
        );
      switch (result.term) {
      | Arrow(arg2, result2) =>
        let arg2_id = List.nth(IdTagged.ids(arg2), 0);
        let result2_id = List.nth(IdTagged.ids(result2), 0);
        let group =
          ArrowTyp.arrow3(
            ~arg1_id=arg_id,
            ~arg2_id,
            ~result_id=result2_id,
            ~arg_id,
            ~arrow_result_id=result_id,
          );
        if (ArrowTyp.arrow3_typ_id == get_specificity_level(group)) {
          get_message(
            ~colorings=
              ArrowTyp.arrow3_typ_coloring_ids(
                ~arg1_id=arg_id,
                ~arg2_id,
                ~result_id=result2_id,
              ),
            group,
          );
        } else {
          basic(group);
        };
      | _ => basic(ArrowTyp.arrow(~arg_id, ~result_id))
      };
    | Label(name) => get_message(LabelTerm.labels(name))
    | TupLabel(l, t) =>
      get_message(
        ~colorings=
          TupLabelTyp.labeled_exps_coloring_ids(
            ~label_id=Typ.rep_id(l),
            ~typ_id=Typ.rep_id(t),
          ),
        TupLabelTyp.labeled_typs(
          ~label_id=Typ.rep_id(l),
          ~typ_id=Typ.rep_id(t),
        ),
      )
    | Prod(elements) =>
      let n = List.length(elements);
      let basic = group =>
        get_message(~explanation=TupleTyp.tuple_typ_explanation(~n), group);
      switch (n) {
      | 0 =>
        if (TupleTyp.tuple0_typ.id == get_specificity_level(TupleTyp.tuple0)) {
          get_message(~colorings=[], TupleTyp.tuple0);
        } else {
          /* Unreachable: `tuple0` has a single form, so the selected form is
             always `tuple0_typ`. */
          basic(
            TupleTyp.tuple(~n),
          );
        }
      | 2 =>
        let elem1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
        let elem2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
        let group = TupleTyp.tuple2(~elem1_id, ~elem2_id, ~n);
        if (TupleTyp.tuple2_typ_id == get_specificity_level(group)) {
          get_message(
            ~colorings=TupleTyp.tuple2_typ_coloring_ids(~elem1_id, ~elem2_id),
            group,
          );
        } else {
          basic(group);
        };
      | 3 =>
        let elem1_id = List.nth(IdTagged.ids(List.nth(elements, 0)), 0);
        let elem2_id = List.nth(IdTagged.ids(List.nth(elements, 1)), 0);
        let elem3_id = List.nth(IdTagged.ids(List.nth(elements, 2)), 0);
        let group = TupleTyp.tuple3(~elem1_id, ~elem2_id, ~elem3_id, ~n);
        if (TupleTyp.tuple3_typ_id == get_specificity_level(group)) {
          get_message(
            ~colorings=
              TupleTyp.tuple3_typ_coloring_ids(
                ~elem1_id,
                ~elem2_id,
                ~elem3_id,
              ),
            group,
          );
        } else {
          basic(group);
        };
      | _ => basic(TupleTyp.tuple(~n))
      };
    | Var(c) when Info.typ_is_constructor_expected(typ_info) =>
      get_message(SumTyp.sum_typ_nullary_constructor_defs(c))
    | Var(v) => get_message(TerminalTyp.var(v))
    | Sum(_) => get_message(SumTyp.labelled_sum_typs)
    | Unknown(Hole(Invalid(_))) => simple("Not a type or type operator")
    | ProdProjection(_) => get_message(DotTyp.dot)
    | ExplicitNonlabel
    | ProdExtension(_)
    | Parens(_)
    | Sig(_) => message_single(SigTyp.single)
    | Projector(_) => default
    | DrvQuoteTy(Jdmt) =>
      simple(
        "`DrvJdmt` is the type of derivation-mode judgements. Quote a judgement with `of_jdmt` to embed it as an expression.",
      )
    | DrvQuoteTy(Ctx) =>
      simple(
        "`DrvCtx` is the type of derivation-mode typing contexts, mapping ALFA variables to ALFA types. Quote a context with `of_ctx`.",
      )
    | DrvQuoteTy(Prop) =>
      simple(
        "`DrvProp` is the type of derivation-mode propositions (e.g., equalities between ALFA terms or types). Quote a proposition with `of_prop`.",
      )
    | DrvQuoteTy(Exp) =>
      simple(
        "`ALFAExp` is the type of ALFA expressions: terms in the object language of the derivation. Quote an ALFA expression with `of_alfa_exp`.",
      )
    | DrvQuoteTy(Pat) =>
      simple(
        "`DrvPat` is the type of ALFA patterns, used in binding positions within ALFA expressions.",
      )
    | DrvQuoteTy(Typ) =>
      simple(
        "`ALFATyp` is the type of ALFA types: the types of the object language of the derivation. Quote an ALFA type with `of_alfa_typ`.",
      )
    | DrvQuoteTy(TPat) =>
      simple(
        "`DrvTPat` is the type of ALFA type patterns, used in binding positions within ALFA type abstractions.",
      )
    };
  | Some(InfoTPat(info)) =>
    switch (info.user_term.term) {
    | Invalid(_) => simple("Type names must begin with a capital letter")
    | EmptyHole => get_message(HoleTPat.empty_hole_tpats)
    | MultiHole(_) => get_message(HoleTPat.multi_hole_tpats)
    | Var(v) => get_message(VarTPat.var_typ_pats(v))
    }
  | Some(InfoDrv({term, _})) =>
    let (syntax, msg) =
      switch (term) {
      | Exp(exp) => DrvDoc.exp_form(exp)
      | Typ(typ) => DrvDoc.typ_form(typ)
      | Pat(pat) => DrvDoc.pat_form(pat)
      | TPat(tpat) => DrvDoc.tpat_form(tpat)
      };
    (
      [syntax |> CodeViewable.view_segment(~globals)],
      (
        [
          div(
            ~attrs=[clss(["explanation-contents"])],
            msg |> mk_translation(~globals, ~inject=_ => ()) |> fst,
          ),
        ],
        (Id.Map.empty, 0),
      ),
      [],
    );
  | Some(Secondary(s)) =>
    switch (s.cls) {
    | Secondary(Whitespace) => simple("A semantic void, pervading but inert")
    | Secondary(Comment) =>
      simple("Comments are ignored by systems but treasured by readers")
    | _ => simple("No documentation available")
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
    (~globals: Globals.t, ~explainThisModel: ExplainThisModel.t, info) =>
  switch (globals.settings.explainThis.highlight) {
  | All when globals.settings.sidebar.show =>
    let (_, (_, (color_map, _)), _) =
      get_doc(~globals, ~docs=explainThisModel, info, Colorings);
    Some(color_map);
  | One(id) when globals.settings.sidebar.show =>
    let (_, (_, (color_map, _)), _) =
      get_doc(~globals, ~docs=explainThisModel, info, Colorings);
    Some(Id.Map.filter((id', _) => id == id', color_map));
  | _ => None
  };

type info = {
  cursor: option(Statics.Info.t),
  deduction: info_deduction,
};

let view =
    (
      ~globals: Globals.t,
      ~inject,
      ~explainThisModel: ExplainThisModel.t,
      info: info,
    ) => {
  // This gets the info from the infomap before singleton autolabelling
  let info_cursor = Option.map(Info.pre_labeled_info, info.cursor);
  let (syn_form, (explanation, _), example) =
    get_doc(
      ~globals,
      ~docs=explainThisModel,
      info_cursor,
      MessageContent(inject, globals),
    );
  let (syn_form_Drv, (explanation_Drv, _), _) =
    get_doc_deduction(
      ~globals,
      ~docs=explainThisModel,
      info.deduction,
      MessageContent(inject, globals),
    );
  div(
    ~attrs=[Attr.id("explain-this")],
    [
      div(
        ~attrs=[clss(["header"])],
        [
          Widgets.toggle(
            ~tooltip="Toggle highlighting",
            "🔆",
            globals.settings.explainThis.highlight == All,
            _ =>
            globals.inject_global(Set(ExplainThis(SetHighlight(Toggle))))
          ),
        ],
      ),
    ]
    @ (
      switch (info.deduction) {
      | Some({rule, _}) => [
          section(
            ~section_clss="syntactic-form",
            ~title=
              switch (rule) {
              | Some({rule, _}) => Rule.show(rule)
              | None => "Unknown Rule"
              },
            syn_form_Drv @ explanation_Drv,
          ),
          div(~attrs=[clss(["hline"])], []),
        ]
      | None => []
      }
    )
    @ [
      section(
        ~section_clss="syntactic-form",
        ~title=
          switch (info_cursor) {
          | None => "Whitespace or Comment"
          | Some(info) => Info.cls_of(info) |> Cls.show
          },
        syn_form @ explanation,
      ),
    ]
    @ (
      example == []
        ? []
        : [section(~section_clss="examples", ~title="Examples", example)]
    ),
  );
};
