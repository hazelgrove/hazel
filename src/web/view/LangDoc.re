open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Core;

/* TODO copied from Page */
let toggle = (label, active, action) =>
  div(
    [
      clss(["toggle-switch"] @ (active ? ["active"] : [])),
      Attr.on_click(action),
    ],
    [div([clss(["toggle-knob"])], [text(label)])],
  );

let feedback_view = (message, up_active, up_action, down_active, down_action) => {
  div(
    [clss(["feedback"])],
    [
      div([clss(["message"])], [text(message)]),
      div(
        [
          clss(["option"] @ (up_active ? ["active"] : [])),
          Attr.on_click(up_action),
        ],
        [text("👍")],
      ),
      div(
        [
          clss(["option"] @ (down_active ? ["active"] : [])),
          Attr.on_click(down_action),
        ],
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

/* TODO - Hannah - this is used (or something pretty similar) other places and should probably be refactored to somewhere
   centeralized like AssistantView_common - or maybe the different uses are different enough... */
let code_node = text =>
  Node.span([Attr.classes(["code"])], [Node.text(text)]);

let highlight =
    (msg: list(Node.t), id: Core.Id.t, mapping: ColorSteps.t)
    : (Node.t, ColorSteps.t) => {
  let (c, mapping) = ColorSteps.get_color(id, mapping);
  /*print_endline(
      "Color chosen at highlight: ("
      ++ Sexp.to_string(CursorPath.sexp_of_steps(steps))
      ++ ", "
      ++ c
      ++ ")",
    );*/
  (Node.span([Attr.classes(["highlight-" ++ c])], msg), mapping);
};

let _max_elems = 7;
let int_to_word_number = (n: int): string => {
  switch (n) {
  | 1 => "first"
  | 2 => "second"
  | 3 => "third"
  | 4 => "fourth"
  | 5 => "fifth"
  | 6 => "sixth"
  | 7 => "seventh"
  | _ => ""
  };
};
let comma_separated_list = (items: list(string)): string => {
  /*let _ = List.map(item => print_endline(item), items);*/
  let length = List.length(items);
  let items =
    List.mapi(
      (index, item) => {
        let separator =
          if (index == length - 1) {
            length > 2 ? ", and" : " and";
          } else if (index == 0) {
            "";
          } else {
            ",";
          };
        separator ++ " " ++ item;
      },
      items,
    );
  List.fold_left((acc, item) => acc ++ item, "", items);
};

let print_markdown = doc => {
  print_endline("-----------------BEGIN PRINTING------------------");
  let rec print_markdown' = doc => {
    let _ =
      List.mapi(
        (index, element) => {
          print_endline(string_of_int(index));
          switch (element) {
          | Omd.Paragraph(d) =>
            print_endline("------Paragraph---------");
            print_markdown'(d);
          | Ul(_items) => print_endline("Ul")
          | Ulp(_items) => print_endline("Ul  PPPPPP")
          | Text(_) => print_endline("Text")
          | Url(_, d, _) =>
            print_endline("URL");
            print_markdown'(d);
          | Code(_) => print_endline("Code")
          | _ => print_endline("Something else")
          };
        },
        doc,
      );
    ();
  };
  print_markdown'(doc);
  print_endline("---------------------END PRINTING-----------------");
};

/*
 Markdown like thing:
 highlighty thing : [thing to highlight](id)
 bulleted list: - list item
                - list item
 code: `code`
 italics: *word*
 */
let mk_explanation =
    (~inject, id, explanation, text: string, show_highlight: bool)
    : (Node.t, ColorSteps.t) => {
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
          //print_endline("IN THE LIST THINGY");
          let (bullets, mapping) =
            List.fold_left(
              ((nodes, mapping), d) => {
                let (n, mapping) = translate(d, mapping);
                (List.append(nodes, [Node.li([], n)]), mapping);
              },
              ([], mapping),
              items,
            );
          (List.append(msg, [Node.ul([], bullets)]), mapping); /* TODO Hannah - Should this be an ordered list instead of an unordered list? */
        | Code(_name, t) => (List.append(msg, [code_node(t)]), mapping)
        | Url(id, d, _title) =>
          let (d, mapping) = translate(d, mapping);
          let (inner_msg, mapping) =
            if (show_highlight) {
              let id = int_of_string(id);
              highlight(d, id, mapping);
            } else {
              (Node.span([], d), mapping);
            };
          (List.append(msg, [inner_msg]), mapping);
        | Emph(d) =>
          let (d, mapping) = translate(d, mapping);
          (
            List.append(
              msg,
              [
                Node.span(
                  [
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
        | _ =>
          print_endline("OTHER");
          (msg, mapping);
        }
      },
      ([], mapping),
      doc,
    );
  let (msg, color_map) = translate(omd, ColorSteps.empty);
  (
    div(
      [],
      [
        div([clss(["explanation-contents"])], msg),
        explanation_feedback_view(~inject, id, explanation),
      ],
    ),
    color_map,
  );
};

let deco =
    (
      ~doc: LangDocMessages.t,
      ~settings,
      ~colorings,
      ~expandable,
      ~zipper,
      ~map,
      ~inject,
      ~font_metrics,
      ~options,
      ~form_id,
    ) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = false;
    });

  let term_lang_doc =
    Deco.term_decoration(
      ~ids=[expandable],
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

        let specificity_menu =
          Node.div(
            [
              clss(["specificity-options-menu", "expandable"]),
              specificity_style,
            ],
            List.map(
              ((id, segment)) => {
                let map = Measured.of_segment(segment);
                let code_view =
                  Code.simple_view(~unselected=segment, ~map, ~settings);
                Node.div(
                  id == form_id ? [clss(["selected"])] : [],
                  [code_view],
                );
              },
              options,
            ),
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
          [
            clss(["expandable-target"]),
            DecUtil.abs_position(~font_metrics, origin),
            Attr.on_click(_ => {
              inject(
                Update.UpdateLangDocMessages(
                  LangDocMessages.SpecificityOpen(!doc.specificity_open),
                ),
              )
            }),
          ],
          [expandable_deco, specificity_menu],
        );
      },
      zipper,
    );

  let color_highlight =
    if (doc.highlight) {
      List.map(
        ((id, color)) =>
          Deco.term_highlight(
            ~ids=[id],
            ~clss=["highlight-code-" ++ color],
            zipper,
          ),
        colorings,
      )
      |> List.flatten;
    } else {
      [];
    };
  let _ = inject;
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
      ~form_id,
      zipper,
    ) => {
  let map = Measured.of_segment(unselected);
  let code_view = Code.simple_view(~unselected, ~map, ~settings);
  let deco_view =
    deco(
      ~doc,
      ~settings,
      ~colorings,
      ~expandable,
      ~zipper,
      ~map,
      ~inject,
      ~font_metrics,
      ~options,
      ~form_id,
    );
  div(
    [Attr.id(id), Attr.class_("code-container")],
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
    [Attr.id("examples")],
    List.map(
      ({term, message, _} as example: LangDocMessages.example) => {
        let map_code = Measured.of_segment(term);
        let code_view =
          Code.simple_view(~unselected=term, ~map=map_code, ~settings);
        let uhexp = Core.MakeTerm.go(term);
        let (_, _, info_map) = Core.Statics.mk_map(uhexp);
        let result_view =
          switch (Interface.evaulation_result(info_map, uhexp)) {
          | None => []
          | Some(dhexp) => [SimpleMode.res_view(~font_metrics, dhexp)]
          };
        let code_container = view => div([clss(["something"])], view);
        div(
          [clss(["example"])],
          [
            code_container([code_view]),
            div(
              [clss(["ex-result"])],
              [text("Result: "), code_container(result_view)],
            ),
            div(
              [clss(["explanation"])],
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

let get_doc =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      ~docs: LangDocMessages.t,
      info: option(Core.Statics.t),
    ) => {
  let default = (
    text("No syntactic form available"),
    (text("No explanation available"), ColorSteps.empty),
    text("No examples available"),
  );
  switch (info) {
  | Some(InfoExp({term, _})) =>
    switch (term.term) {
    | Invalid(_) => default
    | EmptyHole => default
    | MultiHole(_ids, _uexps) => default
    | Triv => default
    | Bool(_bool_lit) => default
    | Int(_int_lit) => default
    | Float(_float_lit) => default
    | ListLit(_ids, _uexps) => default
    | Fun(_pat, _body) => default
    | Tuple(_ids, _uexps) => default
    | Var(_var) => default
    | Let(_pat, _def, _body) => default
    | Ap(_fun, _arg) => default
    | If(_cond, _then, _else) => default
    | Seq(_seq1, _seq2) => default
    | Test(_uexp) => default
    | Parens(_uexp) => default
    | Cons(hd, tl) =>
      let doc = LangDocMessages.get_form("cons_exp", docs.forms);
      let zipper: Zipper.t = {
        selection: {
          focus: Left,
          content: doc.syntactic_form,
        },
        backpack: [],
        relatives: {
          siblings: ([], []),
          ancestors: [],
        },
        caret: Outer,
      };

      let (explanation, color_map) =
        mk_explanation(
          ~inject,
          doc.id,
          doc.explanation,
          Printf.sprintf(
            Scanf.format_from_string(doc.explanation.message, "%i%i"),
            hd.id,
            tl.id,
          ),
          docs.highlight,
        );
      let (left_color, _) = ColorSteps.get_color(hd.id, color_map);
      let (right_color, _) = ColorSteps.get_color(tl.id, color_map);
      let syntactic_form_view =
        syntactic_form_view(
          ~doc=docs,
          ~colorings=[
            (Piece.id(List.nth(doc.syntactic_form, 0)), left_color),
            (Piece.id(List.nth(doc.syntactic_form, 2)), right_color),
          ],
          ~expandable=doc.expandable_id,
          ~inject,
          ~font_metrics,
          ~unselected=doc.syntactic_form,
          ~settings,
          ~id="syntactic-form-code",
          ~options=doc.options,
          ~form_id=doc.id,
          zipper,
        );
      let example_view =
        example_view(
          ~inject,
          ~font_metrics,
          ~settings,
          ~id=doc.id,
          ~examples=doc.examples,
        );
      (syntactic_form_view, (explanation, color_map), example_view);
    | UnOp(_op, _uexp) => default
    | BinOp(_op, _left, _right) => default
    | Match(_rule_ids, _scrut, _rule_exps) => default
    }
  | Some(InfoPat(_))
  | Some(InfoTyp(_))
  | Some(InfoRul(_))
  | None
  | Some(Invalid(_)) => default
  };
};

let section = (~section_clss: string, ~title: string, contents: Node.t) =>
  div(
    [clss(["section", section_clss])],
    [div([clss(["section-title"])], [text(title)]), contents],
  );

let get_color_map =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      ~doc: LangDocMessages.t,
      index': option(int),
      info_map: Core.Statics.map,
    ) => {
  let info: option(Core.Statics.t) =
    switch (index') {
    | Some(index) =>
      switch (Core.Id.Map.find_opt(index, info_map)) {
      | Some(ci) => Some(ci)
      | None => None
      }
    | None => None
    };
  let (_, (_, color_map), _) =
    get_doc(~inject, ~font_metrics, ~settings, ~docs=doc, info);
  color_map;
};

let view =
    (
      ~inject,
      ~font_metrics,
      ~settings: Model.settings,
      ~doc: LangDocMessages.t,
      index': option(int),
      info_map: Core.Statics.map,
    ) => {
  let info: option(Core.Statics.t) =
    switch (index') {
    | Some(index) =>
      switch (Core.Id.Map.find_opt(index, info_map)) {
      | Some(ci) => Some(ci)
      | None => None
      }
    | None => None
    };
  let (syn_form, (explanation, _), example) =
    get_doc(~inject, ~font_metrics, ~settings, ~docs=doc, info);
  div(
    [clss(["lang-doc"])],
    [
      div(
        [Attr.on_click(_ => Event.Stop_propagation)],
        [
          toggle("🔆", doc.highlight, _ =>
            inject(
              Update.UpdateLangDocMessages(LangDocMessages.ToggleHighlight),
            )
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
