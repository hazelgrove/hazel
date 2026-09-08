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

/* Representative id of the nth element of a tuple/list child sequence. */
let nth_rep_id = (elements, n) => IdTagged.rep_id(List.nth(elements, n));

let rec bypass_parens_typ = (typ: Typ.t) => {
  switch (typ.term) {
  | Parens(t) => bypass_parens_typ(t)
  | _ => typ
  };
};

/* The doc decision for one term: which group the cursor dispatched to, which of
   its forms is selected, that form's filled explanation, and the
   syntactic-form-piece -> user-term-id pairs used to highlight.

   Deciding and rendering are separate because there are three consumers and
   only one of them wants Vdom: the sidebar renders it, the code editor harvests
   a color map from it, and the characterization test reads it directly. */
type doc = {
  group: ExplainThisForm.group,
  /* The selected form. `group.forms` is every form the group offers, ordered
     most-specific-first, so a test can sweep specificity levels instead of only
     ever seeing the most specific. */
  form: ExplainThisForm.form,
  options: list((ExplainThisForm.form_id, Segment.t)),
  explanation: string,
  colorings: list((Id.t, Id.t)),
  /* DeferredAp alone hand-builds its color map instead of deriving it from the
     explanation's links; every other branch leaves this None. The view ignores
     it — only the code highlighter reads it. */
  color_map: option(ColorSteps.t),
};

type decision =
  | NoDoc
  /* Prose with no syntactic form behind it, shown verbatim. */
  | Prose(string)
  /* Prose run through the markdown translator, so code spans and lists render.
     Distinct from `Prose`, whose ~30 one-off messages are shown verbatim;
     routing those through markdown would change what they display. */
  | Markdown(string)
  /* Derivation terms document themselves: DrvDoc supplies both the abstract
     syntax to show and the markdown describing it. */
  | DrvSyntax(Segment.t, string)
  | Doc(doc);

/* The deduction sidebar picks a message rather than a form; its one group is a
   stub whose explanation is always overridden. */
type decision_deduction = {
  group: ExplainThisForm.group,
  form: ExplainThisForm.form_id,
  explanation: string,
};

type info_deduction = option(DrvGrading.VerifiedTree.info);

let decide_deduction =
    (
      ~globals: Globals.t,
      ~docs: ExplainThisModel.t,
      info_deduction: info_deduction,
    )
    : decision_deduction => {
  let group = DrvExplainThis.premise_mismatch;
  /* group_id and form_id are one type, and this path's group is a one-form
     stub, so the group's own id is the right stand-in if there is no form. */
  let form =
    switch (fst(ExplainThisModel.get_form_and_options(group, docs))) {
    | Some(form) => form.id
    | None => group.id
    };
  let explanation =
    switch (info_deduction) {
    | None => "Deduction Not Available"
    | Some({res: Correct, _}) => "✅ Correct"
    | Some({res: Pending(p), _}) => DrvGrading.ExternalError.show(p)
    | Some({res: PartialCorrect(specced), _}) =>
      if (globals.settings.explainThis.highlight == All) {
        Printf.sprintf(
          "❓ Correct until stop at a hole %s)",
          RuleVerify.show_linked(specced),
        );
      } else {
        "❓ Correct until stop at a hole";
      }
    | Some({res: Incorrect(failure), _}) =>
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
      |> Printf.sprintf("❌ %s")
    };
  {
    group,
    form,
    explanation,
  };
};

let view_deduction =
    (
      ~globals: Globals.t,
      ~inject,
      ~docs: ExplainThisModel.t,
      ~info: info_deduction,
      d: decision_deduction,
    )
    : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) => {
  let (explanation_title, (explanation, color_map)) =
    if (globals.settings.core.dynamics) {
      (
        DrvExplainThis.mk_explanation_title(),
        mk_explanation(
          ~globals,
          ~inject,
          d.group.id,
          d.form,
          d.explanation,
          docs,
        ),
      );
    } else {
      (none, (none, ColorSteps.empty));
    };
  let rule_example_view =
    DrvExplainThis.rule_example_view(~info, ~color_map, ~globals);
  ([rule_example_view], ([explanation_title, explanation], color_map), []);
};

let color_map_deduction = (~globals: Globals.t, d: decision_deduction) =>
  snd(mk_translation(~globals, ~inject=_ => (), d.explanation));

/* Both color-map entry points ask the same question of a different doc source:
   run it for its color map, then narrow to one id if the highlight setting says
   so. `harvest` is passed as a thunk so nothing is computed when highlighting is
   off or the sidebar is collapsed. */
let narrow_color_map =
    (~globals: Globals.t, harvest: unit => ColorSteps.colorMap) =>
  switch (globals.settings.explainThis.highlight) {
  | All when globals.settings.sidebar.show => Some(harvest())
  | One(id) when globals.settings.sidebar.show =>
    Some(Id.Map.filter((id', _) => id == id', harvest()))
  | _ => None
  };

/* Keys on `sidebar.show` via `narrow_color_map`, the same setting `get_color_map`
   uses, so collapsing the sidebar stops highlighting for derivation terms and
   ordinary ones alike. Not `explainThis.show`: nothing toggles it and it defaults
   to true, so keying on it would leave derivation highlighting on. */
let get_color_map_deduction =
    (
      ~globals: Globals.t,
      ~explainThisModel: ExplainThisModel.t,
      info_deduction: info_deduction,
    ) =>
  narrow_color_map(~globals, () =>
    fst(
      color_map_deduction(
        ~globals,
        decide_deduction(~globals, ~docs=explainThisModel, info_deduction),
      ),
    )
  );

let decide =
    (~docs: ExplainThisModel.t, info: option(Statics.Info.t)): decision => {
  let get_specificity_level = group_id =>
    Option.map(
      (form: ExplainThisForm.form) => form.id,
      fst(ExplainThisModel.get_form_and_options(group_id, docs)),
    );
  let get_message =
      (
        ~colorings=[],
        ~explanation: option(string)=?,
        group: ExplainThisForm.group,
      )
      : decision => {
    let (selected, options) =
      ExplainThisModel.get_form_and_options(group, docs);
    switch (selected) {
    /* Unreachable: no group constructor produces a group with no forms. */
    | None => NoDoc
    | Some(doc) =>
      /* Each form's explanation is already filled in by its data module, where
         the format literal and its arguments sit together. `~explanation`
         overrides it for the few callers that describe a different term. */
      let explanation_msg =
        switch (explanation) {
        | Some(msg) => msg
        | None => doc.explanation
        };
      /* A form carries its own colorings; `~colorings` overrides them for the
         least specific form of a group, which is shared across a family of
         groups and so is not built for this particular call site. */
      let colorings = colorings == [] ? doc.colorings : colorings;
      Doc({
        group,
        form: doc,
        options,
        explanation: explanation_msg,
        colorings,
        color_map: None,
      });
    };
  };

  /* Use this when adding new entries */
  let message_single = (e: ExplainThisForm.Simple.t) =>
    get_message(ExplainThisForm.Simple.to_group(e));

  /* Is the user looking at the group's most specific form, or has it been
     expanded down to a more general one? `forms` is ordered most-specific-first,
     so no call site has to name the specific form's id to ask. */
  let at_specific_level = (group: ExplainThisForm.group) =>
    switch (group.forms) {
    | [specific, ..._] => get_specificity_level(group) == Some(specific.id)
    | [] => true
    };

  /* Every form but the least specific one is built for this exact term, so it
     supplies its own colorings and explanation. The least specific form is
     shared across a whole family of groups — one `function_exp` backs all 19
     FunctionExp groups — so it is built with ids that suit the family rather
     than this call site, and `fallback` re-describes it. */
  let leveled = (~fallback, group: ExplainThisForm.group) => {
    let selected = get_specificity_level(group);
    switch (List.rev(group.forms)) {
    | [least, _, ..._] when selected == Some(least.id) => fallback(group)
    | _ => get_message(group)
    };
  };

  switch (info) {
  | Some(InfoMod({cls, _})) =>
    switch (cls) {
    | Mod(ModLet) => message_single(ModLetDecl.single)
    | Mod(ModType) => message_single(ModTypeDecl.single)
    | Mod(ModuleMod) => message_single(ModuleKeywordDecl.single)
    | _ => Prose("Module item")
    }
  | Some(InfoSig({cls, _})) =>
    switch (cls) {
    | Sig(SigLet) => message_single(SigLetDecl.single)
    | Sig(SigType) => message_single(SigTypeDecl.single)
    | _ => Prose("Signature item")
    }
  | Some(InfoMPat(_)) => Prose("Module name")
  | Some(InfoExp({cls: Mod(ModLet), _})) =>
    message_single(ModLetDecl.single)
  | Some(InfoExp({cls: Mod(ModType), _})) =>
    message_single(ModTypeDecl.single)
  | Some(InfoExp({cls: Mod(ModuleMod), _})) =>
    message_single(ModuleKeywordDecl.single)
  | Some(InfoExp({cls: Mod(_), _})) => Prose("Module item")
  | Some(InfoExp({user_term: term, _})) =>
    let rec get_message_exp = (term): decision =>
      switch ((term: Exp.term)) {
      | DrvQuote(_) =>
        Markdown(
          "A derivation-mode quotation embeds a derivation-mode term into a regular expression. There are 5 forms of quotation:\n1) `of_jdmt`\n2) `of_ctx`\n3) `of_prop`\n4) `of_alfa_exp`\n5) `of_alfa_typ`",
        )
      | Invalid(_) => Prose("Not a valid expression")
      | DynamicErrorHole(_)
      | Closure(_) => Prose("Internal expression")
      | Asc(e, t) =>
        let exp_id = IdTagged.rep_id(e);
        let typ_id = IdTagged.rep_id(t);
        get_message(AscExp.ascriptions(~exp_id, ~typ_id));
      | Use(t, e) =>
        message_single(
          UseExp.single(~typ_id=Typ.rep_id(t), ~body_id=Exp.rep_id(e)),
        )
      | BuiltinFun(_) => Prose("Internal expression")
      | LivelitName(n) => get_message(TerminalExp.livelit_name_exps(n))
      | FumolaPeek({reads, _}) =>
        Prose(
          "A reference to the Fumola cell read by `"
          ++ reads
          ++ "`, carrying the value it held. It is a value, so a program can use it as that value while still showing which cell it came from.",
        )
      | EmptyHole => get_message(HoleExp.empty_hole_exps)
      | MultiHole(_children) => get_message(HoleExp.multi_hole_exps)
      | TyAlias(ty_pat, ty_def, _body) =>
        let tpat_id = IdTagged.rep_id(ty_pat);
        let def_id = IdTagged.rep_id(ty_def);
        get_message(TyAliasExp.tyalias_exps(~tpat_id, ~def_id));
      | Undefined => get_message(UndefinedExp.undefined_exps)
      | Deferral(_) => get_message(TerminalExp.deferral_exps)
      | ExplicitNonlabel => Prose("Explicitly unlabeled entry")
      | Atom(Bool(b)) => get_message(TerminalExp.bool_exps(b))
      | Atom(Int(i)) => get_message(TerminalExp.int_exps(i))
      | Atom(SInt(i)) => get_message(TerminalExp.sint_exps(i))
      | Atom(Float(f)) => get_message(TerminalExp.float_exps(f))
      | Atom(String(s)) => get_message(TerminalExp.string_exps(s))
      | Atom(Nat(i)) => get_message(TerminalExp.nat_exps(i))
      | ListLit(terms) =>
        get_message(ListExp.listlits(~n=List.length(terms)))
      | TypFun(tpat, body, _) =>
        let tpat_id = IdTagged.rep_id(tpat);
        let body_id = IdTagged.rep_id(body);
        /* TODO: More could be done here probably for different patterns. */
        get_message(TypFunctionExp.type_functions_basic(~tpat_id, ~body_id));
      | Fun(pat, body, _, _) =>
        /* The generic fallback form describes the *unbypassed* pattern, while
           the specific forms below describe the pattern with parens and
           annotations stripped. These ids differ for e.g. `fun (x) -> x`. */
        let unbypassed_pat_id = IdTagged.rep_id(pat);
        let basic = group => {
          let body_id = IdTagged.rep_id(body);
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
        let pat_id = IdTagged.rep_id(pat);
        let body_id = IdTagged.rep_id(body);
        switch (pat.term) {
        | EmptyHole =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_empty_hole(~pat_id, ~body_id),
          )
        | MultiHole(_) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_multi_hole(~pat_id, ~body_id),
          )
        | Wild =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_wild(~pat_id, ~body_id),
          )
        | Atom(SInt(i)) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_sint(~pat_id, ~body_id, ~i),
          )
        | Atom(Int(i) | Nat(i)) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_int(~pat_id, ~body_id, ~i),
          )
        | Atom(Float(f)) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_float(~pat_id, ~body_id, ~f),
          )
        | Atom(Bool(b)) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_bool(~pat_id, ~body_id, ~b),
          )
        | Atom(String(s)) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_str(~pat_id, ~body_id, ~s),
          )
        | Tuple([]) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_triv(~pat_id, ~body_id),
          )
        | ListLit(elements) =>
          List.length(elements) == 0
            ? leveled(
                ~fallback=basic,
                FunctionExp.functions_listnil(~pat_id, ~body_id),
              )
            : leveled(
                ~fallback=basic,
                FunctionExp.functions_listlit(
                  ~pat_id,
                  ~body_id,
                  ~n=List.length(elements),
                ),
              )
        | Cons(hd, tl) =>
          let hd_id = IdTagged.rep_id(hd);
          let tl_id = IdTagged.rep_id(tl);
          leveled(
            ~fallback=basic,
            FunctionExp.functions_cons(~hd_id, ~tl_id, ~pat_id, ~body_id),
          );
        | Var(var) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_var(~pat_id, ~body_id, ~name=var),
          )
        | Tuple([{term: TupLabel(l, p), _}]) =>
          let group =
            FunctionExp.functions_tuplabel(
              ~label_id=Pat.rep_id(l),
              ~label_pat_id=Pat.rep_id(p),
              ~pat_id,
              ~body_id,
            );
          leveled(~fallback=basic, group);
        | Tuple(elements) =>
          let n = List.length(elements);
          switch (n) {
          | 2 =>
            let pat1_id = nth_rep_id(elements, 0);
            let pat2_id = nth_rep_id(elements, 1);
            let group =
              FunctionExp.functions_tuple2(
                ~pat1_id,
                ~pat2_id,
                ~pat_id,
                ~body_id,
                ~n,
              );
            leveled(~fallback=basic, group);
          | 3 =>
            let pat1_id = nth_rep_id(elements, 0);
            let pat2_id = nth_rep_id(elements, 1);
            let pat3_id = nth_rep_id(elements, 2);
            let group =
              FunctionExp.functions_tuple3(
                ~pat1_id,
                ~pat2_id,
                ~pat3_id,
                ~pat_id,
                ~body_id,
                ~n,
              );
            leveled(~fallback=basic, group);
          | _ =>
            leveled(
              ~fallback=basic,
              FunctionExp.functions_tuple(~pat_id, ~body_id, ~n),
            )
          };
        | Ap(con, arg) =>
          let con_id = IdTagged.rep_id(con);
          let arg_id = IdTagged.rep_id(arg);
          leveled(
            ~fallback=basic,
            FunctionExp.functions_ap(~con_id, ~arg_id, ~pat_id, ~body_id),
          );
        | Constructor(v, _) =>
          leveled(
            ~fallback=basic,
            FunctionExp.functions_ctr(~pat_id, ~body_id, ~name=v),
          )
        | TupLabel(_)
        | Invalid(_)
        | Parens(_)
        | Label(_)
        | ExplicitNonlabel
        | Projector(_)
        | Asc(_) => NoDoc // Shouldn't get hit?
        };
      | Label(name) => get_message(LabelTerm.labels(name))
      | TupLabel(l, e) =>
        get_message(
          TupLabelExp.labeled_exps(
            ~label_id=Exp.rep_id(l),
            ~exp_id=Exp.rep_id(e),
          ),
        )
      | Dot(tup, lab) =>
        get_message(
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
          let exp1_id = nth_rep_id(terms, 0);
          let exp2_id = nth_rep_id(terms, 1);
          leveled(~fallback=basic, TupleExp.tuples2(~exp1_id, ~exp2_id, ~n));
        | 3 =>
          let exp1_id = nth_rep_id(terms, 0);
          let exp2_id = nth_rep_id(terms, 1);
          let exp3_id = nth_rep_id(terms, 2);
          leveled(
            ~fallback=basic,
            TupleExp.tuples3(~exp1_id, ~exp2_id, ~exp3_id, ~n),
          );
        | _ => basic(TupleExp.tuples(~n))
        };
      | Var(n) => get_message(TerminalExp.var_exps(n))
      | Let(pat, def, body) =>
        let pat = bypass_parens_and_annot_pat(pat);
        let pat_id = IdTagged.rep_id(pat);
        let def_id = IdTagged.rep_id(def);
        let body_id = IdTagged.rep_id(body);
        let basic = group =>
          get_message(
            ~colorings=LetExp.let_base_exp_coloring_ids(~pat_id, ~def_id),
            ~explanation=LetExp.let_base_exp_explanation(~def_id, ~pat_id),
            group,
          );
        switch (pat.term) {
        /* TODO The coloring for the syntactic form is sometimes wrong when
           switching between forms and specificity levels... maybe a Safari
           issue... */
        | EmptyHole =>
          leveled(~fallback=basic, LetExp.lets_emptyhole(~def_id, ~pat_id))
        | MultiHole(_) =>
          leveled(~fallback=basic, LetExp.lets_multihole(~def_id, ~pat_id))
        | Wild =>
          leveled(
            ~fallback=basic,
            LetExp.lets_wild(~def_id, ~pat_id, ~body_id),
          )
        | Atom(Int(i) | Nat(i)) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_int(~def_id, ~pat_id, ~i, ~body_id),
          )
        | Atom(SInt(i)) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_sint(~def_id, ~pat_id, ~i, ~body_id),
          )
        // TODO Make sure everywhere printing the float literal print it prettier
        | Atom(Float(f)) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_float(~def_id, ~pat_id, ~f, ~body_id),
          )
        | Atom(Bool(b)) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_bool(~def_id, ~pat_id, ~b, ~body_id),
          )
        | Atom(String(s)) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_str(~def_id, ~pat_id, ~s, ~body_id),
          )
        | Tuple([]) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_triv(~def_id, ~pat_id, ~body_id),
          )
        | ListLit(elements) =>
          List.length(elements) == 0
            ? leveled(
                ~fallback=basic,
                LetExp.lets_listnil(~def_id, ~pat_id, ~body_id),
              )
            : leveled(
                ~fallback=basic,
                LetExp.lets_listlit(
                  ~def_id,
                  ~pat_id,
                  ~n=List.length(elements),
                ),
              )
        | Cons(hd, tl) =>
          let hd_id = IdTagged.rep_id(hd);
          let tl_id = IdTagged.rep_id(tl);
          leveled(
            ~fallback=basic,
            LetExp.lets_cons(~def_id, ~hd_id, ~tl_id, ~pat_id),
          );
        | Var(var) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_var(~def_id, ~pat_id, ~name=var, ~body_id),
          )
        | Tuple(elements) =>
          let n = List.length(elements);
          /* The middle level describes the tuple generically, so unlike the
             other fallbacks it substitutes its own explanation. */
          let tuple_level = group =>
            get_message(
              ~colorings=LetExp.let_tuple_exp_coloring_ids(~pat_id, ~def_id),
              ~explanation=
                LetExp.let_tuple_exp_explanation(~def_id, ~pat_id, ~n),
              group,
            );

          switch (n) {
          | 2 =>
            let pat1_id = nth_rep_id(elements, 0);
            let pat2_id = nth_rep_id(elements, 1);
            leveled(
              ~fallback=basic,
              LetExp.lets_tuple2(~def_id, ~pat1_id, ~pat2_id, ~pat_id, ~n),
            );
          | 3 =>
            let pat1_id = nth_rep_id(elements, 0);
            let pat2_id = nth_rep_id(elements, 1);
            let pat3_id = nth_rep_id(elements, 2);
            // TODO Syntactic form can go off page - so can examples - but can scroll, just can't see bottom scroll bar
            leveled(
              ~fallback=basic,
              LetExp.lets_tuple3(
                ~def_id,
                ~pat1_id,
                ~pat2_id,
                ~pat3_id,
                ~pat_id,
                ~n,
              ),
            );
          | _ =>
            let group = LetExp.lets_tuple(~def_id, ~pat_id, ~n);
            at_specific_level(group) ? tuple_level(group) : basic(group);
          };
        | Ap(x, arg) =>
          let x_id = IdTagged.rep_id(x);
          let arg_id = IdTagged.rep_id(arg);
          let lets_ap =
            switch (x.term) {
            | Constructor(_, _) =>
              LetExp.lets_conap(~def_id, ~x_id, ~arg_id, ~pat_id)
            | _ => LetExp.lets_funap(~def_id, ~x_id, ~arg_id, ~pat_id)
            };
          leveled(~fallback=basic, lets_ap);
        | Constructor(v, _) =>
          leveled(
            ~fallback=basic,
            LetExp.lets_ctr(~def_id, ~pat_id, ~name=v, ~body_id),
          )
        | TupLabel(_)
        | ExplicitNonlabel
        | Label(_)
        | Invalid(_) => NoDoc // Shouldn't get hit
        | Parens(_)
        | Projector(_)
        | Asc(_) => NoDoc // Shouldn't get hit?
        };
      | Theorem(pat, thm, body) =>
        let pat_id = IdTagged.rep_id(pat);
        let thm_id = IdTagged.rep_id(thm);
        let body_id = IdTagged.rep_id(body);
        get_message(TheoremExp.tests(~pat_id, ~thm_id, ~body_id));
      | ProofObject(exp) =>
        let typ_id = IdTagged.rep_id(exp);
        get_message(ProofObjectExp.proof_of_exps(~typ_id));
      | Forall(pat, typ) =>
        let pat_id = IdTagged.rep_id(pat);
        let body_id = IdTagged.rep_id(typ);
        get_message(ForallExp.forall(~pat_id, ~body_id));
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
        let f_id = IdTagged.rep_id(f);
        let typ_id = IdTagged.rep_id(typ);
        get_message(TypAppExp.typfunaps(~f_id, ~typ_id));

      | Ap(Forward, x, arg) =>
        let x_id = IdTagged.rep_id(x);
        let arg_id = IdTagged.rep_id(arg);
        let basic = group => get_message(group);
        switch (x.term) {
        | Constructor(v, _) => basic(AppExp.conaps(~name=v, ~x_id, ~arg_id))
        | LivelitName(_) => basic(AppExp.livelitaps(~x_id, ~arg_id))
        | _ => basic(AppExp.funaps(~x_id, ~arg_id))
        };
      | DeferredAp(x, args) =>
        let x_id = IdTagged.rep_id(x);
        let supplied_id = Id.mk();
        let deferred_id = {
          let deferral = List.find(Exp.is_deferral, args);
          IdTagged.rep_id(deferral);
        };
        /* Unlike every other branch, this one hand-builds its color map rather
           than deriving it from the explanation's links, so it rides along on
           the decision. Only the code highlighter reads it; the sidebar renders
           this doc exactly like any other. */
        let color_map = {
          let color_fn = List.nth(ColorSteps.child_colors, 0);
          let color_supplied = List.nth(ColorSteps.child_colors, 1);
          let color_deferred = List.nth(ColorSteps.child_colors, 2);
          let add = (mapping, arg: Exp.t) => {
            let arg_id = IdTagged.rep_id(arg);
            Haz3lcore.Id.Map.add(
              arg_id,
              Exp.is_deferral(arg) ? color_deferred : color_supplied,
              mapping,
            );
          };
          let mapping = Haz3lcore.Id.Map.singleton(x_id, color_fn);
          let mapping = List.fold_left(add, mapping, args);
          (mapping, List.length(args) + 1);
        };
        switch (
          get_message(AppExp.deferredaps(~x_id, ~supplied_id, ~deferred_id))
        ) {
        | Doc(doc) =>
          Doc({
            ...doc,
            color_map: Some(color_map),
          })
        | other => other
        };
      | If(cond, then_, else_) =>
        let cond_id = IdTagged.rep_id(cond);
        let then_id = IdTagged.rep_id(then_);
        let else_id = IdTagged.rep_id(else_);
        get_message(IfExp.ifs(~cond_id, ~then_id, ~else_id));
      | Seq(left, right) =>
        let exp1_id = IdTagged.rep_id(left);
        let exp2_id = IdTagged.rep_id(right);
        get_message(SeqExp.seqs(~exp1_id, ~exp2_id));
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
      | Filter(_) => Prose("Internal expression")
      | Test(body) =>
        let body_id = IdTagged.rep_id(body);
        get_message(TestExp.tests(~body_id));
      | Parens(term) => get_message_exp(term.term) // No Special message?
      | HintedTest(body, hint) =>
        let hint_id = IdTagged.rep_id(hint);
        let body_id = IdTagged.rep_id(body);
        get_message(HintedTestExp.tests(~hint_id, ~body_id));
      | Cons(hd, tl) =>
        let hd_id = IdTagged.rep_id(hd);
        let tl_id = IdTagged.rep_id(tl);
        get_message(ListExp.listcons(~hd_id, ~tl_id));
      | TupleExtension(x, y) =>
        let x_id = IdTagged.rep_id(x);
        let y_id = IdTagged.rep_id(y);
        get_message(TupleExp.tuple_extensions(~x_id, ~y_id));
      | ListConcat(xs, ys) =>
        let xs_id = IdTagged.rep_id(xs);
        let ys_id = IdTagged.rep_id(ys);
        get_message(ListExp.listconcats(~xs_id, ~ys_id));
      | UnOp(op, exp) =>
        switch (op) {
        | Bool(Not) =>
          let exp_id = IdTagged.rep_id(exp);
          get_message(OpExp.bool_un_not(~exp_id));
        | Float(Minus) // TODO[Matt]: finish
        | SInt(Minus)
        | Nat(Minus)
        | Int(Minus) =>
          let exp_id = IdTagged.rep_id(exp);
          get_message(OpExp.int_un_minus(~exp_id));
        }
      | BinOp(op, left, right) =>
        open OpExp;
        let group =
          switch (op) {
          | Nat(Plus)
          | SInt(Plus)
          | Int(Plus) => int_plus
          | Nat(Minus)
          | SInt(Minus)
          | Int(Minus) => int_minus
          | Nat(Times)
          | SInt(Times)
          | Int(Times) => int_times
          | Nat(Power)
          | SInt(Power)
          | Int(Power) => int_power
          | Nat(Divide)
          | SInt(Divide)
          | Int(Divide) => int_divide
          | Nat(LessThan)
          | SInt(LessThan)
          | Int(LessThan) => int_less_than
          | Nat(LessThanOrEqual)
          | SInt(LessThanOrEqual)
          | Int(LessThanOrEqual) => int_less_than_equal
          | Nat(GreaterThan)
          | SInt(GreaterThan)
          | Int(GreaterThan) => int_greater_than
          | Nat(GreaterThanOrEqual)
          | SInt(GreaterThanOrEqual)
          | Int(GreaterThanOrEqual) => int_greater_than_equal
          | Float(Plus) => float_plus
          | Float(Minus) => float_minus
          | Float(Times) => float_times
          | Float(Power) => float_power
          | Float(Divide) => float_divide
          | Float(LessThan) => float_less_than
          | Float(LessThanOrEqual) => float_less_than_equal
          | Float(GreaterThan) => float_greater_than
          | Float(GreaterThanOrEqual) => float_greater_than_equal
          | Float(Equals) => float_equal
          | Float(NotEquals) => float_not_equal
          | Bool(And) => bool_and
          | Bool(Or) => bool_or
          | String(Concat) => string_concat
          | Poly(Equals) => poly_equal
          | Poly(NotEquals) => poly_not_equal
          };
        let left_id = IdTagged.rep_id(left);
        let right_id = IdTagged.rep_id(right);
        get_message(group(~left_id, ~right_id));
      | Match(scrut, _rules) =>
        let scrut_id = IdTagged.rep_id(scrut);
        get_message(CaseExp.case(~scrut_id));
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
      let hd_id = IdTagged.rep_id(hd);
      let tl_id = IdTagged.rep_id(tl);
      let basic = doc =>
        get_message(
          ~colorings=ListPat.cons_base_pat_coloring_ids(~hd_id, ~tl_id),
          ~explanation=ListPat.cons_base_pat_explanation(~hd_id, ~tl_id),
          doc,
        );
      switch (tl.term) {
      | Cons(hd2, tl2) =>
        let hd2_id = IdTagged.rep_id(hd2);
        let tl2_id = IdTagged.rep_id(tl2);
        leveled(
          ~fallback=basic,
          ListPat.cons2(~fst_id=hd_id, ~snd_id=hd2_id, ~tl_id=tl2_id, ~hd_id),
        );
      | _ => basic(ListPat.cons(~hd_id, ~tl_id))
      };
    | Var(v) => get_message(TerminalPat.var(v))
    | ExplicitNonlabel => Prose("Explicitly unlabeled entry")
    | Label(name) => get_message(LabelTerm.labels(name))
    | TupLabel(l, p) =>
      get_message(
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
        let elem1_id = nth_rep_id(elements, 0);
        let elem2_id = nth_rep_id(elements, 1);
        leveled(~fallback=basic, TuplePat.tuple2(~elem1_id, ~elem2_id, ~n));
      | 3 =>
        let elem1_id = nth_rep_id(elements, 0);
        let elem2_id = nth_rep_id(elements, 1);
        let elem3_id = nth_rep_id(elements, 2);
        leveled(
          ~fallback=basic,
          TuplePat.tuple3(~elem1_id, ~elem2_id, ~elem3_id, ~n),
        );
      | _ => basic(TuplePat.tuple(~n))
      };
    | Ap(x, arg) =>
      let x_id = IdTagged.rep_id(x);
      let arg_id = IdTagged.rep_id(arg);
      switch (x.term) {
      | Constructor(_, _) => get_message(AppPat.conaps(~x_id, ~arg_id))
      | _ => get_message(AppPat.funaps(~x_id, ~arg_id))
      };
    | Constructor(con, _) => get_message(TerminalPat.ctr(con))
    | Asc(pat, typ) =>
      let pat_id = IdTagged.rep_id(pat);
      let typ_id = IdTagged.rep_id(typ);
      get_message(TypAnnPat.typann(~pat_id, ~typ_id));
    | Invalid(_) => Prose("Not a valid pattern")
    | Parens(_)
    | Projector(_) =>
      // Shouldn't be hit?
      NoDoc
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
      let elem_id = IdTagged.rep_id(elem);
      get_message(ListTyp.list(~elem_id));
    | Poly(tpat, typ) =>
      let tpat_id = IdTagged.rep_id(tpat);
      let tbody_id = IdTagged.rep_id(typ);
      get_message(PolyTyp.poly(~tpat_id, ~tbody_id));
    | Rec(tpat, typ) =>
      let tpat_id = IdTagged.rep_id(tpat);
      let tbody_id = IdTagged.rep_id(typ);
      get_message(RecTyp.rec_(~tpat_id, ~tbody_id));
    | ProofOf(exp) =>
      let body_id = IdTagged.rep_id(exp);
      get_message(ProofOfTyp.proof_of(~body_id));
    | Arrow(arg, result) =>
      let arg_id = IdTagged.rep_id(arg);
      let result_id = IdTagged.rep_id(result);
      let basic = doc =>
        get_message(
          ~explanation=ArrowTyp.arrow_typ_explanation(~arg_id, ~result_id),
          doc,
        );
      switch (result.term) {
      | Arrow(arg2, result2) =>
        let arg2_id = IdTagged.rep_id(arg2);
        let result2_id = IdTagged.rep_id(result2);
        leveled(
          ~fallback=basic,
          ArrowTyp.arrow3(
            ~arg1_id=arg_id,
            ~arg2_id,
            ~result_id=result2_id,
            ~arg_id,
            ~arrow_result_id=result_id,
          ),
        );
      | _ => basic(ArrowTyp.arrow(~arg_id, ~result_id))
      };
    | Label(name) => get_message(LabelTerm.labels(name))
    | TupLabel(l, t) =>
      get_message(
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
      /* `tuple0` has a single form, so the selected form is always
         `tuple0_typ` — there is no other level to fall back to. */
      | 0 => get_message(TupleTyp.tuple0)
      | 2 =>
        let elem1_id = nth_rep_id(elements, 0);
        let elem2_id = nth_rep_id(elements, 1);
        leveled(~fallback=basic, TupleTyp.tuple2(~elem1_id, ~elem2_id, ~n));
      | 3 =>
        let elem1_id = nth_rep_id(elements, 0);
        let elem2_id = nth_rep_id(elements, 1);
        let elem3_id = nth_rep_id(elements, 2);
        leveled(
          ~fallback=basic,
          TupleTyp.tuple3(~elem1_id, ~elem2_id, ~elem3_id, ~n),
        );
      | _ => basic(TupleTyp.tuple(~n))
      };
    | Var(c) when Info.typ_is_constructor_expected(typ_info) =>
      get_message(SumTyp.sum_typ_nullary_constructor_defs(c))
    | Var(v) => get_message(TerminalTyp.var(v))
    | Sum(_) => get_message(SumTyp.labelled_sum_typs)
    | Unknown(Hole(Invalid(_))) => Prose("Not a type or type operator")
    | ProdProjection(_) => get_message(DotTyp.dot)
    | ExplicitNonlabel
    | ProdExtension(_)
    | Parens(_)
    | Sig(_) => message_single(SigTyp.single)
    | Projector(_) => NoDoc
    | DrvQuoteTy(Jdmt) =>
      Prose(
        "`DrvJdmt` is the type of derivation-mode judgements. Quote a judgement with `of_jdmt` to embed it as an expression.",
      )
    | DrvQuoteTy(Ctx) =>
      Prose(
        "`DrvCtx` is the type of derivation-mode typing contexts, mapping ALFA variables to ALFA types. Quote a context with `of_ctx`.",
      )
    | DrvQuoteTy(Prop) =>
      Prose(
        "`DrvProp` is the type of derivation-mode propositions (e.g., equalities between ALFA terms or types). Quote a proposition with `of_prop`.",
      )
    | DrvQuoteTy(Exp) =>
      Prose(
        "`ALFAExp` is the type of ALFA expressions: terms in the object language of the derivation. Quote an ALFA expression with `of_alfa_exp`.",
      )
    | DrvQuoteTy(Pat) =>
      Prose(
        "`DrvPat` is the type of ALFA patterns, used in binding positions within ALFA expressions.",
      )
    | DrvQuoteTy(Typ) =>
      Prose(
        "`ALFATyp` is the type of ALFA types: the types of the object language of the derivation. Quote an ALFA type with `of_alfa_typ`.",
      )
    | DrvQuoteTy(TPat) =>
      Prose(
        "`DrvTPat` is the type of ALFA type patterns, used in binding positions within ALFA type abstractions.",
      )
    };
  | Some(InfoTPat(info)) =>
    switch (info.user_term.term) {
    | Invalid(_) => Prose("Type names must begin with a capital letter")
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
    DrvSyntax(syntax, msg);
  | Some(Secondary(s)) =>
    switch (s.cls) {
    | Secondary(Whitespace) => Prose("A semantic void, pervading but inert")
    | Secondary(Comment) =>
      Prose("Comments are ignored by systems but treasured by readers")
    | _ => Prose("No documentation available")
    }
  | None => NoDoc
  };
};

let view_doc =
    (
      ~globals: Globals.t,
      ~inject,
      ~docs: ExplainThisModel.t,
      ~info: option(Statics.Info.t),
      decision: decision,
    )
    : (list(Node.t), (list(Node.t), ColorSteps.t), list(Node.t)) =>
  switch (decision) {
  | NoDoc => ([], ([text("No docs available")], ColorSteps.empty), [])
  | Prose(msg) => ([], ([text(msg)], ColorSteps.empty), [])
  | Markdown(msg) => ([], mk_translation(~globals, ~inject=_ => (), msg), [])
  | DrvSyntax(syntax, msg) => (
      [syntax |> CodeViewable.view_segment(~globals)],
      (
        [
          div(
            ~attrs=[clss(["explanation-contents"])],
            msg |> mk_translation(~globals, ~inject=_ => ()) |> fst,
          ),
        ],
        ColorSteps.empty,
      ),
      [],
    )
  | Doc({group, form, options, explanation, colorings, _}) =>
    let (explanation, color_map) =
      mk_explanation(~globals, ~inject, group.id, form.id, explanation, docs);
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
    let editor = Editor.Model.mk(form.syntactic_form |> Zipper.unzip, ~root);
    let expander_deco =
      expander_deco(
        ~globals,
        ~docs,
        ~inject,
        ~options,
        ~group,
        ~doc=form,
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
        ~form_id=form.id,
        ~examples=form.examples,
        ~model=docs,
      );
    ([syntactic_form_view], ([explanation], color_map), example_view);
  };

/* The code editor highlights user terms with the colors the sidebar's
   explanation assigns them, so it needs the color map without any of the view.
   `Prose`/`NoDoc`/`DrvSyntax` carry no links, hence no colors. */
let color_map_of = (~globals: Globals.t, decision: decision): ColorSteps.t =>
  switch (decision) {
  | NoDoc
  | Prose(_)
  | DrvSyntax(_) => ColorSteps.empty
  | Markdown(msg) => snd(mk_translation(~globals, ~inject=_ => (), msg))
  | Doc({color_map: Some(color_map), _}) => color_map
  | Doc({explanation, _}) =>
    snd(mk_translation(~globals, ~inject=_ => (), explanation))
  };

let section = (~section_clss: string, ~title: string, contents: list(Node.t)) =>
  div(
    ~attrs=[clss(["section", section_clss])],
    [div(~attrs=[clss(["section-title"])], [text(title)])] @ contents,
  );

let get_color_map =
    (~globals: Globals.t, ~explainThisModel: ExplainThisModel.t, info) =>
  narrow_color_map(~globals, () =>
    fst(color_map_of(~globals, decide(~docs=explainThisModel, info)))
  );

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
    view_doc(
      ~globals,
      ~inject,
      ~docs=explainThisModel,
      ~info=info_cursor,
      decide(~docs=explainThisModel, info_cursor),
    );
  let (syn_form_Drv, (explanation_Drv, _), _) =
    view_deduction(
      ~globals,
      ~inject,
      ~docs=explainThisModel,
      ~info=info.deduction,
      decide_deduction(~globals, ~docs=explainThisModel, info.deduction),
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
          | Some(info) => Info.cls_label(info)
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
